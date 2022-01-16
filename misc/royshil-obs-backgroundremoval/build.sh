#!/usr/bin/env bash
set -euo pipefail

cat <<'EOF' | docker build -t obs-backgroundremoval-builder -
FROM ubuntu:latest
ENV DEBIAN_FRONTEND noninteractive
RUN set -x \
    && apt-get update \
    && apt install -y \
        libobs-dev \
        libopencv-dev \
        language-pack-en \
        wget \
        git \
        build-essential \
        cmake \
    && rm -rf /var/lib/apt/lists/*
RUN set -x \
    && wget https://github.com/microsoft/onnxruntime/releases/download/v1.7.0/onnxruntime-linux-x64-1.7.0.tgz \
    && tar xzvf onnxruntime-linux-x64-1.7.0.tgz --strip-components=1 -C /usr/local/ --wildcards "*/include/*" "*/lib*/"
RUN set -x \
    && git clone https://github.com/royshil/obs-backgroundremoval /in \
    && mkdir /in/build && cd /in/build \
    && cmake -DCMAKE_INSTALL_PREFIX:PATH=/out .. && cmake --build .
WORKDIR /in/build
CMD ["cmake", "--install", "."]
EOF

mkdir -p ./out
docker run -it -v "$(readlink -f ./out)":/out obs-backgroundremoval-builder
