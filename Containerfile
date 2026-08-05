FROM docker.io/library/debian:bookworm-slim

RUN apt-get update \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      bash build-essential ca-certificates curl git jq less nodejs npm \
      openssh-client python3 python3-pip ripgrep \
 && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /home/agent/.cache /home/agent/.config /home/agent/.local/state \
 && chmod -R 0777 /home/agent

ENV HOME=/home/agent \
    XDG_CONFIG_HOME=/home/agent/.config \
    XDG_CACHE_HOME=/home/agent/.cache \
    XDG_STATE_HOME=/home/agent/.local/state

WORKDIR /workspace
CMD ["/bin/bash"]
