# based on: https://github.com/firsttris/docker-images/tree/master/tvh/master
FROM ubuntu
MAINTAINER Maximilian Huber <maximilian.huber@tngtech.com>

RUN apt-get update \
 && apt-get install -y wget

# Install Sundtek DVB-Driver
RUN wget http://www.sundtek.de/media/sundtek_netinst.sh \
 && chmod 777 sundtek_netinst.sh \
 && ./sundtek_netinst.sh -easyvdr

# ADD entrypoint.sh /entrypoint.sh
# RUN chmod +x /entrypoint.sh
