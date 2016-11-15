#/bin/bash

sudo yum update
sudo yum install R

R -e "install.packages(sp)"
R -e "install.packages(magrittr)"
R -e "install.packages(stringr)"
