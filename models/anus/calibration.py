#!/usr/bin/env python3
# coding: utf-8

# ## High-Dimensional sample-efficient Bayesian Optimization with SAASBO
# 
# This tutorial shows how to use the Sparse Axis-Aligned Subspace Bayesian Optimization (SAASBO) 
# method for high-dimensional Bayesian optimization [1]. SAASBO places strong priors on the 
# inverse lengthscales to avoid overfitting in high-dimensional spaces. Specifically, SAASBO 
# uses a hierarchical sparsity prior consisting of a global shrinkage parameter 
# $\tau \sim \mathcal{HC}(\beta)$ and inverse lengthscales $\rho_d \sim \mathcal{HC}(\tau)$ 
# for $d=1, \ldots, D$, where $\mathcal{HC}$ is the half-Cauchy distribution. 
# While half-Cauchy priors favor values near zero they also have heavy tails, which allows the 
# inverse lengthscales of the most important parameters to escape zero. To perform inference in the 
# SAAS model we use Hamiltonian Monte Carlo (HMC) as we found that to outperform MAP inference.
# 
# We find that SAASBO performs well on problems with hundreds of dimensions. As we rely on HMC 
# and in particular the No-U-Turn-Sampler (NUTS) for inference, the overhead of SAASBO scales 
# cubically with the number of datapoints. Depending on the problem, using more than a few hundred
# evaluations may not be feasible as SAASBO is designed for problems with a limited evaluation budget.
# 
# In general, we recommend using [Ax](https://ax.dev) for a simple BO setup like this one. See [here](https://ax.dev/tutorials/saasbo.html) for a SAASBO tutorial in Ax, which uses the Noisy Expected Improvement acquisition function. To customize the acquisition function used with SAASBO in Ax, see the [custom acquisition tutorial](./custom_acquisition), where adding `\"surrogate\": Surrogate(SaasFullyBayesianSingleTaskGP),` to the `model_kwargs` of `BOTORCH_MODULAR` step is sufficient to enable the SAAS model.
# 
# [1]: [D. Eriksson, M. Jankowiak. High-Dimensional Bayesian Optimization with Sparse Axis-Aligned Subspaces. Proceedings of the Thirty-Seventh Conference on Uncertainty in Artificial Intelligence, 2021.](https://proceedings.mlr.press/v161/eriksson21a.html)

# In[1]:

import os

import botorch
import gpytorch
import torch
import json
import math
import matplotlib.pyplot as plt
import numpy as np
import rpy2.robjects as ro
from timeit import default_timer as timer
from gpytorch.constraints import Interval
from gpytorch.kernels import MaternKernel, ScaleKernel
from gpytorch.likelihoods import GaussianLikelihood
from gpytorch.mlls import ExactMarginalLogLikelihood
from torch.quasirandom import SobolEngine
from dataclasses import dataclass

from timeit import default_timer as timer
from botorch import fit_fully_bayesian_model_nuts
from botorch.acquisition import qExpectedImprovement
from botorch.models.fully_bayesian import SaasFullyBayesianSingleTaskGP
from botorch.models.transforms import Standardize
from botorch.acquisition.analytic import LogExpectedImprovement
from botorch.exceptions import ModelFittingError
from botorch.fit import fit_gpytorch_mll
from botorch.generation import MaxPosteriorSampling
from botorch.models import SingleTaskGP
from botorch.optim import optimize_acqf

PROJECT_PATH = os.path.abspath(os.path.join(__file__, os.path.pardir, os.path.pardir))

r = ro.r
r.source('./calibration_error.R')
initial_guess = list(ro.globalenv['get.initial.guess']())
target_inc_hsil = list(ro.globalenv['get.initial.guess']())
calibration_error = ro.globalenv['calibration.error']

dim = len(initial_guess)
N_INIT = 10
N_ITERATIONS = 200
NUM_RESTARTS = 10
RAW_SAMPLES = 512
N_CANDIDATES = min(5000, max(2000, 200 * dim))

SEARCH_SCOPE = 1
bu = torch.minimum(torch.tensor(initial_guess) * (1+SEARCH_SCOPE), torch.tensor(1))
bl = torch.tensor(initial_guess) * (1-SEARCH_SCOPE)

def normalize(X):
    return (2*X - (bu+bl)) / (bu-bl)

def unnormalize(X_norm):
    return (X_norm * (bu-bl) + (bu+bl)) / 2

def get_target_func():
    fixed_params = []
    errors = []

    def calibration_error_wrapper(X):
        X2 = unnormalize(torch.tensor(X))
        X2 = ro.vectors.FloatVector(X2)
        error = calibration_error(X2)
        return list(error)[0]

    def model_evaluation_error(x):
      augmented_x = np.array(fixed_params + list(x)) # Parameters to be optimized prefixed by fixed parameters
      error = calibration_error_wrapper(augmented_x)
      #print('Error: {}'.format(error))
      return -error

    return model_evaluation_error


params = [('x{}'.format(i), '') for i, _ in enumerate(initial_guess)]
target_func = get_target_func()

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
print(f"Running on {device}")
dtype = torch.double
max_cholesky_size = float("inf")  # Always use Cholesky


# ## Maintain the BAxUS state
# BAxUS needs to maintain a state, which includes the length of the trust region, success and failure counters, success and failure tolerance, etc.
# In contrast to TuRBO, the failure tolerance depends on the target dimensionality.
#
# In this tutorial we store the state in a dataclass and update the state of TuRBO after each batch evaluation.
#
# **Note**: These settings assume that the domain has been scaled to $[-1, 1]^d$

# In[4]:


@dataclass
class BaxusState:
    dim: int
    eval_budget: int
    new_bins_on_split: int = 3
    d_init: int = float("nan")  # Note: post-initialized
    target_dim: int = float("nan")  # Note: post-initialized
    n_splits: int = float("nan")  # Note: post-initialized
    length: float = 0.8
    length_init: float = 0.8
    length_min: float = 0.5**7
    length_max: float = 1.6
    failure_counter: int = 0
    success_counter: int = 0
    success_tolerance: int = 3
    best_value: float = -float("inf")
    restart_triggered: bool = False

    def __post_init__(self):
        n_splits = round(math.log(self.dim, self.new_bins_on_split + 1))
        self.d_init = 1 + np.argmin(
            np.abs(
                (1 + np.arange(self.new_bins_on_split))
                * (1 + self.new_bins_on_split) ** n_splits
                - self.dim
            )
        )
        self.target_dim = self.d_init
        self.n_splits = n_splits

    @property
    def split_budget(self) -> int:
        return round(
            -1
            * (self.new_bins_on_split * self.eval_budget * self.target_dim)
            / (self.d_init * (1 - (self.new_bins_on_split + 1) ** (self.n_splits + 1)))
        )

    @property
    def failure_tolerance(self) -> int:
        if self.target_dim == self.dim:
            return self.target_dim
        k = math.floor(math.log(self.length_min / self.length_init, 0.5))
        split_budget = self.split_budget
        return min(self.target_dim, max(1, math.floor(split_budget / k)))


def update_state(state, Y_next):
    if max(Y_next) > state.best_value + 1e-3 * math.fabs(state.best_value):
        state.success_counter += 1
        state.failure_counter = 0
    else:
        state.success_counter = 0
        state.failure_counter += 1

    if state.success_counter == state.success_tolerance:  # Expand trust region
        state.length = min(2.0 * state.length, state.length_max)
        state.success_counter = 0
    elif state.failure_counter == state.failure_tolerance:  # Shrink trust region
        state.length /= 2.0
        state.failure_counter = 0

    state.best_value = max(state.best_value, max(Y_next).item())
    if state.length < state.length_min:
        state.restart_triggered = True
    return state


# ## Create a BAxUS embedding
#
# We now show how to create the BAxUS embedding. The essential idea is to assign input dimensions to target dimensions and to assign a sign $\in \pm 1$ to each input dimension, similar to the HeSBO embedding.
# We create the embedding matrix that is used to project points from the target to the input space. The matrix is sparse, each column has precisely one non-zero entry that is either 1 or -1.

# In[5]:


def embedding_matrix(input_dim: int, target_dim: int) -> torch.Tensor:
    if (
        target_dim >= input_dim
    ):  # return identity matrix if target size greater than input size
        return torch.eye(input_dim, device=device, dtype=dtype)

    input_dims_perm = (
        torch.randperm(input_dim, device=device) + 1
    )  # add 1 to indices for padding column in matrix

    bins = torch.tensor_split(
        input_dims_perm, target_dim
    )  # split dims into almost equally-sized bins
    bins = torch.nn.utils.rnn.pad_sequence(
        bins, batch_first=True
    )  # zero pad bins, the index 0 will be cut off later

    mtrx = torch.zeros(
        (target_dim, input_dim + 1), dtype=dtype, device=device
    )  # add one extra column for padding
    mtrx = mtrx.scatter_(
        1,
        bins,
        2 * torch.randint(2, (target_dim, input_dim), dtype=dtype, device=device) - 1,
    )  # fill mask with random +/- 1 at indices

    return mtrx[:, 1:]  # cut off index zero as this corresponds to zero padding


embedding_matrix(10, 3)  # example for an embedding matrix


# ## Function to increase the embedding
#
# Next, we write a helper function to increase the embedding and to bring observations to the increased target space.

# In[6]:


def increase_embedding_and_observations(
    S: torch.Tensor, X: torch.Tensor, n_new_bins: int
) -> torch.Tensor:
    assert X.size(1) == S.size(0), "Observations don't lie in row space of S"

    S_update = S.clone()
    X_update = X.clone()

    for row_idx in range(len(S)):
        row = S[row_idx]
        idxs_non_zero = torch.nonzero(row)
        idxs_non_zero = idxs_non_zero[torch.randperm(len(idxs_non_zero))].reshape(-1)

        if len(idxs_non_zero) <= 1:
            continue

        non_zero_elements = row[idxs_non_zero].reshape(-1)

        n_row_bins = min(
            n_new_bins, len(idxs_non_zero)
        )  # number of new bins is always less or equal than the contributing input dims in the row minus one

        new_bins = torch.tensor_split(idxs_non_zero, n_row_bins)[
            1:
        ]  # the dims in the first bin won't be moved
        elements_to_move = torch.tensor_split(non_zero_elements, n_row_bins)[1:]

        new_bins_padded = torch.nn.utils.rnn.pad_sequence(
            new_bins, batch_first=True
        )  # pad the tuples of bins with zeros to apply _scatter
        els_to_move_padded = torch.nn.utils.rnn.pad_sequence(
            elements_to_move, batch_first=True
        )

        S_stack = torch.zeros(
            (n_row_bins - 1, len(row) + 1), device=device, dtype=dtype
        )  # submatrix to stack on S_update

        S_stack = S_stack.scatter_(
            1, new_bins_padded + 1, els_to_move_padded
        )  # fill with old values (add 1 to indices for padding column)

        S_update[
            row_idx, torch.hstack(new_bins)
        ] = 0  # set values that were move to zero in current row

        X_update = torch.hstack(
            (X_update, X[:, row_idx].reshape(-1, 1).repeat(1, len(new_bins)))
        )  # repeat observations for row at the end of X (column-wise)
        S_update = torch.vstack(
            (S_update, S_stack[:, 1:])
        )  # stack onto S_update except for padding column

    return S_update, X_update


# In[7]:


#S = embedding_matrix(10, 2)
#X = torch.randint(100, (7, 2))
#print(f"S before increase\n{S}")
#print(f"X before increase\n{X}")

#S, X = increase_embedding_and_observations(S, X, 3)
#print(f"S after increase\n{S}")
#print(f"X after increase\n{X}")


# ## Take a look at the state

# In[8]:


#state = BaxusState(dim=dim, eval_budget=N_ITERATIONS)
#print(state)


# ## Generate initial points
# This generates an initial set of Sobol points that we use to start of the BO loop.

# In[9]:


def get_initial_points(dim, n_pts, seed=0):
    sobol = SobolEngine(dimension=dim, scramble=True, seed=seed)
    X_init = (
        2 * sobol.draw(n=n_pts).to(dtype=dtype, device=device) - 1
    )  # points have to be in [-1, 1]^d
    return X_init


# ## Generate new batch
# Given the current `state` and a probabilistic (GP) `model` built from observations `X` and `Y`, we generate a new batch of points.
#
# This method works on the domain $[-1, +1]^d$, so make sure to not pass in observations from the true domain.  `unnormalize` is called before the true function is evaluated which will first map the points back to the original domain.
#
# We support either TS and qEI which can be specified via the `acqf` argument.

# In[10]:


def create_candidate(
    state,
    model,  # GP model
    X,  # Evaluated points on the domain [-1, 1]^d
    Y,  # Function values
    n_candidates=None,  # Number of candidates for Thompson sampling
    num_restarts=NUM_RESTARTS,
    raw_samples=RAW_SAMPLES,
    acqf="ei",  # "ei" or "ts"
):
    assert acqf in ("ts", "ei")
    assert X.min() >= -1.0 and X.max() <= 1.0 and torch.all(torch.isfinite(Y))
    if n_candidates is None:
        n_candidates = min(5000, max(2000, 200 * X.shape[-1]))

    # Scale the TR to be proportional to the lengthscales
    x_center = X[Y.argmax(), :].clone()
    weights = model.covar_module.base_kernel.lengthscale.detach().view(-1)
    weights = weights / weights.mean()
    weights = weights / torch.prod(weights.pow(1.0 / len(weights)))
    tr_lb = torch.clamp(x_center - weights * state.length, -1.0, 1.0)
    tr_ub = torch.clamp(x_center + weights * state.length, -1.0, 1.0)

    if acqf == "ts":
        dim = X.shape[-1]
        sobol = SobolEngine(dim, scramble=True)
        pert = sobol.draw(n_candidates).to(dtype=dtype, device=device)
        pert = tr_lb + (tr_ub - tr_lb) * pert

        # Create a perturbation mask
        prob_perturb = min(20.0 / dim, 1.0)
        mask = torch.rand(n_candidates, dim, dtype=dtype, device=device) <= prob_perturb
        ind = torch.where(mask.sum(dim=1) == 0)[0]
        mask[ind, torch.randint(0, dim, size=(len(ind),), device=device)] = 1

        # Create candidate points from the perturbations and the mask
        X_cand = x_center.expand(n_candidates, dim).clone()
        X_cand[mask] = pert[mask]

        # Sample on the candidate points
        thompson_sampling = MaxPosteriorSampling(model=model, replacement=False)
        with torch.no_grad():  # We don't need gradients when using TS
            X_next = thompson_sampling(X_cand, num_samples=1)

    elif acqf == "ei":
        ei = LogExpectedImprovement(model, train_Y.max())
        X_next, acq_value = optimize_acqf(
            ei,
            bounds=torch.stack([tr_lb, tr_ub]),
            q=1,
            num_restarts=num_restarts,
            raw_samples=raw_samples,
        )

    return X_next


# ## Optimization loop
# This simple loop runs one instance of BAxUS with Thompson sampling until convergence.
#
# BAxUS works on a fixed evaluation budget and shrinks the trust region until the minimal trust region size is reached (`state["restart_triggered"]` is set to `True`).
# Then, BAxUS increases the target space and carries over the observations to the updated space.
#

# In[11]:


calib_start_time = timer()

state = BaxusState(dim=dim, eval_budget=N_ITERATIONS - N_INIT)
S = embedding_matrix(input_dim=state.dim, target_dim=state.d_init)

X_baxus_target = get_initial_points(state.d_init, N_INIT)
X_baxus_input = X_baxus_target @ S
Y_baxus = torch.tensor(
    [target_func(x) for x in X_baxus_input], dtype=dtype, device=device
).unsqueeze(-1)


# Disable input scaling checks as we normalize to [-1, 1]
with botorch.settings.validate_input_scaling(False):
    for _ in range(N_ITERATIONS - N_INIT):  # Run until evaluation budget depleted

        start = timer()
        # Fit a GP model
        train_Y = (Y_baxus - Y_baxus.mean()) / Y_baxus.std()
        likelihood = GaussianLikelihood(noise_constraint=Interval(1e-8, 1e-3))
        model = SingleTaskGP(
            X_baxus_target, train_Y, likelihood=likelihood
        )
        mll = ExactMarginalLogLikelihood(model.likelihood, model)

        # Do the fitting and acquisition function optimization inside the Cholesky context
        with gpytorch.settings.max_cholesky_size(max_cholesky_size):
            # Fit the model
            try:
                fit_gpytorch_mll(mll)
            except ModelFittingError:
                # Right after increasing the target dimensionality, the covariance matrix becomes indefinite
                # In this case, the Cholesky decomposition might fail due to numerical instabilities
                # In this case, we revert to Adam-based optimization
                optimizer = torch.optim.Adam([{"params": model.parameters()}], lr=0.1)

                for _ in range(100):
                    optimizer.zero_grad()
                    output = model(X_baxus_target)
                    loss = -mll(output, train_Y.flatten())
                    loss.backward()
                    optimizer.step()

            # Create a batch
            X_next_target = create_candidate(
                state=state,
                model=model,
                X=X_baxus_target,
                Y=train_Y,
                n_candidates=N_CANDIDATES,
                num_restarts=NUM_RESTARTS,
                raw_samples=RAW_SAMPLES,
                acqf="ei",
            )

        X_next_input = X_next_target @ S

        Y_next = torch.tensor(
            [target_func(x) for x in X_next_input], dtype=dtype, device=device
        ).unsqueeze(-1)

        # Update state
        state = update_state(state=state, Y_next=Y_next)

        # Append data
        X_baxus_input = torch.cat((X_baxus_input, X_next_input), dim=0)
        X_baxus_target = torch.cat((X_baxus_target, X_next_target), dim=0)
        Y_baxus = torch.cat((Y_baxus, Y_next), dim=0)

        end = timer()
        # Print current status
        print(
            f"iteration {len(X_baxus_input)}, d={len(X_baxus_target.T)})  ({(end-start):.3f} s) Best value: {-state.best_value:.3}, TR length: {state.length:.3}"
        )

        if state.restart_triggered:
            state.restart_triggered = False
            print("increasing target space")
            S, X_baxus_target = increase_embedding_and_observations(
                S, X_baxus_target, state.new_bins_on_split
            )
            print(f"new dimensionality: {len(S)}")
            state.target_dim = len(S)
            state.length = state.length_init
            state.failure_counter = 0
            state.success_counter = 0


calib_end_time = timer()
print(f"Total time: {(calib_end_time - calib_start_time):.3f}s")
