# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# OWASP Dependency-Track (https://dependencytrack.org) on host.nuc, deployed
# as two podman containers:
#
#   dtrack-apiserver : dependencytrack/apiserver — REST API + analysis engine
#   dtrack-frontend  : dependencytrack/frontend — Vue SPA served by nginx
#
# Both images are pinned to 4.14.2 (the version `latest` pointed to at the
# time of writing; upstream also has a 5.x line that needs a DB migration).
# Bump `version` below to upgrade, and check the upstream release notes first.
#
# Architecture / networking (verified against the published images):
#   * apiserver listens on :8080 and runs as UID/GID 1000. Its data
#     (embedded H2 DB, encryption keys, logs) lives under /data
#     (HOME=/data, alpine.data.directory defaults to ~/.dependency-track,
#     i.e. /data/.dependency-track), persisted via a bind mount to
#     /var/lib/dependency-track/apiserver.
#   * frontend (nginx) listens on :8080 and serves the SPA. At startup its
#     `30-oidc-configuration.sh` entrypoint writes the `API_BASE_URL` env
#     var into /static/config.json. The SPA reads that file and calls the API
#     *directly from the browser*, so `API_BASE_URL` must be a
#     browser-reachable URL — it is fronted by Caddy at
#     https://dtrack-api.nuc.wg0.maxhbr.local (registered in
#     shared.deployedServices.nix).
#   * Because SPA (dtrack.nuc.*) and API (dtrack-api.nuc.*) are on different
#     subdomains, the browser makes cross-origin requests, so CORS is enabled
#     on the apiserver. The SPA authenticates with a Bearer token stored in
#     sessionStorage (NOT cookies — verified in the SPA bundle), so
#     `Access-Control-Allow-Credentials` can be false and
#     `Access-Control-Allow-Origin` can be `*`. Alpine's default
#     `cors.allow.headers` already permits `Authorization` and `X-Api-Key`.
#
# Database:
#   The apiserver uses the embedded H2 database (default). This is fine for
#   small / personal portfolios. For larger deployments, add a PostgreSQL
#   container and set the `ALPINE_DATABASE_*` env vars (see
#   WEB-INF/classes/application.properties inside the apiserver image for the
#   full list). Consider adding /var/lib/dependency-track to the restic
#   backup in backup-hdd.nix if this becomes important data.
{
  config,
  pkgs,
  lib,
  ...
}:
let
  version = "4.14.2";
  # The apiserver image runs as UID/GID 1000 (User: "1000"). UID 1000 on this
  # host is `mhuber`; the bind-mounted data dir is chowned to 1000:1000 so the
  # container process can write to /data.
  dataDir = "/var/lib/dependency-track/apiserver";
  # Browser-reachable API URL (fronted by Caddy, registered in
  # shared.deployedServices.nix as the `dtrack-api` service).
  apiBaseUrl = "https://dtrack-api.nuc.wg0.maxhbr.local";
in
{
  virtualisation.oci-containers.containers.dtrack-apiserver = {
    image = "dependencytrack/apiserver:${version}";
    hostname = "dtrack-apiserver";
    environment = {
      TZ = "Europe/Berlin";
      # CORS: the SPA is served from dtrack.nuc.* while the API is on
      # dtrack-api.nuc.*, so the browser makes cross-origin requests.
      # DT uses Bearer tokens (sessionStorage), not cookies, so
      # credentials=false + origin=* is valid CORS and sufficient.
      ALPINE_CORS_ENABLED = "true";
      ALPINE_CORS_ALLOW_ORIGIN = "*";
      ALPINE_CORS_ALLOW_CREDENTIALS = "false";
    };
    volumes = [ "${dataDir}:/data" ];
    # host:container — apiserver listens on 8080 inside the container.
    ports = [ "127.0.0.1:8082:8080" ];
    extraOptions = [
      # Protect the host: the Java heap auto-sizes to 80% of the container
      # memory limit (JAVA_OPTIONS=...MaxRAMPercentage=80.0). DT recommends
      # >=4GB; 3GB is a reasonable home-lab default. Increase for large
      # portfolios.
      "--memory=3g"
    ];
    # Pinned version tag → only pull when absent locally (avoids a registry
    # round-trip — and a hard failure if the registry is down — on every
    # restart). Bumping `version` pulls the new tag on next start.
    pull = "missing";
  };

  virtualisation.oci-containers.containers.dtrack-frontend = {
    image = "dependencytrack/frontend:${version}";
    hostname = "dtrack-frontend";
    environment = {
      TZ = "Europe/Berlin";
      # Must be the browser-reachable API URL, NOT an internal container
      # hostname: the SPA calls it directly from the user's browser.
      API_BASE_URL = apiBaseUrl;
    };
    ports = [ "127.0.0.1:8081:8080" ];
    dependsOn = [ "dtrack-apiserver" ];
    pull = "missing";
  };

  # Create the apiserver data directory with ownership matching the
  # container's UID/GID 1000 so it can write the H2 DB and keys.
  system.activationScripts.dependency-track.text = ''
    install -d -m 750 -o 1000 -g 1000 ${dataDir}
  '';
}
