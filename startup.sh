#/bin/bash

sudo yum update
sudo yum install R

R -e "install.packages('sp', repos = 'https://cloud.r-project.org/')"
R -e "install.packages('magrittr', repos = 'https://cloud.r-project.org/')"
R -e "install.packages('stringr', repos = 'https://cloud.r-project.org/')"
