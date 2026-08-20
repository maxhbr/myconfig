# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Enable the llama-swap native `/metrics` scrape job
# (modules/myconfig.observability/client.llama-swap-metrics.nix) for
# the gfx1151 llama-swap instance running inside the
# `llama-cpp-33657` container. That container uses
# `privateNetwork = false`, so it shares the host's network
# namespace and the endpoint is reachable at `127.0.0.1:<port>` from
# the host's own vmagent — the same port `myconfig.deployedServices`
# publishes externally as `gfx1151` (see
# `../shared.deployedServices.nix`), reachable at
# `https://gfx1151.thing.wg0.maxhbr.local/metrics` via Caddy.
{ config, ... }:
{
  config = {
    myconfig.observability.client.llamaSwapMetrics = {
      enable = true;
      scrapePort = config.containers.llama-cpp-33657.config.myconfig.ai.llama-cpp.servicePort;
    };
  };
}
