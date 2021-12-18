FROM debian:jessie
MAINTAINER Maximilian Huber <maximilian.huber@tngtech.com>
ENV DEBIAN_FRONTEND noninteractive

ENV _update="apt-get update"
ENV _install="apt-get install -y --no-install-recommends"
ENV _cleanup="eval apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*"
ENV _purge="apt-get purge -y --auto-remove"

RUN GOSU_VERSION='1.7' set -x \
 && $_update && $_install ca-certificates wget && $_cleanup \
 && wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$(dpkg --print-architecture)" \
 && chmod +x /usr/local/bin/gosu \
 && wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$(dpkg --print-architecture).asc" \
 && export GNUPGHOME="$(mktemp -d)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc && $_purge ca-certificates

RUN set -x \
  && $_update && $_install texlive-full latexmk zathura xpdf && $_cleanup
RUN echo "\$pdf_previewer = \"start zathura\";" > ~/.latexmkrc

RUN mkdir -p /workdir
WORKDIR /workdir

COPY docker-entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["bash"]
