!#/bin/bash
cd ~/bayesian-explorer/
git pull
cd ..
sudo cp -r bayesian-explorer/* /srv/shiny-server/bayesian-explorer/
# restart the server
sudo systemctl restart shiny-server
