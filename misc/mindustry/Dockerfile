FROM openjdk:17-alpine

EXPOSE 6567

ENV RELEASE=v135

RUN set -x \
 && wget -nc \
       -O "/opt/server-release.jar" \
       "https://github.com/Anuken/Mindustry/releases/download/$RELEASE/server-release.jar" \
 && echo "exec java -Djava.net.preferIPv4Stack=true -jar /opt/server-release.jar host" > /opt/entrypoint.sh \
 && chmod +x "/opt/entrypoint.sh"

ENTRYPOINT "/opt/entrypoint.sh"
