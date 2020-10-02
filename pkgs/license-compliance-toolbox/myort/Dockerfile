FROM ort:latest
ENV JAVA_OPTS "-Xms2048M -Xmx16g -XX:MaxPermSize=4096m -XX:MaxMetaspaceSize=4g"
RUN set -x \
 && ssh-keygen -b 2048 -t rsa -f ~/.ssh/id_rsa -q -N "" \
 && npm install --global rollup \
 && apt-get update \
 && apt-get install -y --no-install-recommends gocryptfs fuse \
 && rm -rf /var/lib/apt/lists/* \
 && mkdir /workdir \
 && chmod 777 /workdir /etc/passwd

COPY entrypoint.sh ort-password.sh /opt/

ENTRYPOINT ["/bin/bash", "/opt/entrypoint.sh"]
