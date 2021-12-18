FROM ubuntu:xenial
ENV DEBIAN_FRONTEND noninteractive

ENV _update="apt update"
ENV _install="apt install -y --no-install-recommends"
ENV _cleanup="eval apt clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*"
ENV _purge="apt purge -y --auto-remove"

RUN set -x \
 && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF \
 && echo "deb http://download.mono-project.com/repo/debian wheezy main" > /etc/apt/sources.list.d/mono-xamarin.list \
 && $_update && $_install mono-devel wget unzip \
 && $_cleanup

RUN set -x \
 && mkdir /chummer5a \
 && mkdir /chars \
 && mkdir /mount
WORKDIR /chummer5a
VOLUME /chars

RUN set -x \
 && wget --no-check-certificate https://github.com/chummer5a/chummer5a/releases/download/Nightly-v5.183.15/Chummer.Nightly.zip \
 && unzip Chummer.Nightly.zip

ENV DISPLAY :0

CMD mono Chummer5.exe || /bin/bash
