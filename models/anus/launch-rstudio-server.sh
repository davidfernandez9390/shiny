#!/bin/bash

eval "$(conda shell.bash hook)"
conda activate anus

PASSWORD=1234
PORT=$(python -c 'import socket; s=socket.socket(); s.bind(("", 0)); print(s.getsockname()[1]); s.close()')

singularity pull docker://rocker/rstudio:4.2 2> /dev/null

mkdir -p rstudio-server-data/run 2> /dev/null
mkdir -p rstudio-server-data/var-lib-rstudio-server 2> /dev/null
touch rstudio-server-data/database.conf

echo ""
echo "***********************************************************"
echo "Running RStudio Server on http://127.0.0.1:"$PORT
echo "Username: "$(whoami)
echo "***********************************************************"
echo ""

PASSWORD=$PASSWORD singularity exec \
   --bind rstudio-server-data/run:/run,rstudio-server-data/var-lib-rstudio-server:/var/lib/rstudio-server,rstudio-server-data/database.conf:/etc/rstudio/database.conf \
   rstudio_4.2.sif \
   /usr/lib/rstudio-server/bin/rserver --auth-none=0 --auth-pam-helper-path=pam-helper \
   --auth-timeout-minutes=0 --auth-stay-signed-in-days=30 --server-user=$(whoami) --www-port=$PORT

