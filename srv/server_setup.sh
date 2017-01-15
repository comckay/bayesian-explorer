#!/bin/bash
# Add new R CRAN
sudo echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list

# add key for r-base download
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

# Add ssl reference for install
sudo echo "deb http://security.ubuntu.com/ubuntu lucid-security main" >> /etc/apt/sources.list

# Update and upgrade system
sudo apt-get -y update
sudo apt-get -y dist-upgrade

declare -a ubuntu_packages=('r-base'
                            'r-cran-xml'
                            'libcurl4-gnutls-dev'
                            'libxml2-dev'
                            'openssl'
                            'gdebi-core'
                            'jq'
                            'libpq-dev'
                            'libmariadb-client-lgpl-dev'
                            'libcairo2-dev'
                            'r-cran-slam'
                            )
for package_name in ${ubuntu_packages[@]}; do
  sudo apt-get install -y $package_name
done

# get and install shiny-server
wget -O ~/shiny-server-1.5.1.834-amd64.deb https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.1.834-amd64.deb
sudo gdebi --non-interactive ~/shiny-server-1.5.1.834-amd64.deb

# get the bradford repo
git clone https://github.com/comckay/bayesian-explorer ~/

# Install packages
declare -a packages=('shiny'
                     'magrittr'
                     'ggplot2'
                     'RCurl'
                     'openssl'
                     'XML'
                     'plotly'
                     'shinydashboard'
                     )

for package_name in "${packages[@]}"; do
  sudo su - -c "R -e \"install.packages('$package_name', repos='https://cran.rstudio.com/', dependencies = TRUE)\""
done

# restart the server
sudo systemctl restart shiny-server

sudo mkdir /srv/shiny-server/bayesian-explorer
sudo mkdir /srv/shiny-server/bayesian-explorer

# move the dashboard to the shiny-server directory
sudo cp -r ~/bayesian-explorer/* /srv/shiny-server/bayesian-explorer

#switch to port 80
sudo sed -i '/listen 3838;/c\  listen 80;' /etc/shiny-server/shiny-server.conf

sudo systemctl restart shiny-server

# hack per https://github.com/ropensci/plotly/issues/494 to get shiny + plotly to play nicely
sudo chown -R shiny:shiny /srv/shiny-server
