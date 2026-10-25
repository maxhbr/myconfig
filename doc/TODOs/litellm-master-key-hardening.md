# Optionally give thing's LiteLLM a real master key

## What is in place

`modules/myconfig.ai.llmops/services.litellm.nix` sets
`settings.general_settings.dangerously_permit_weak_or_unset_master_key =
true` for every litellm instance in this deployment. LiteLLM (the
pinned version and newer) refuses to start when no master key is set;
the flag opts out of that boot check. This is deliberate: all clients
— the f13/p14 forwarding proxies (`modules/myconfig.ai.llmops/
litellm.proxy.nix`, loopback-only), the local agents, open-webui,
headroom, the vmagent `/metrics` scrape — call the proxy without any
API key.

The flag is the wrong posture for anything publicly reachable. In this
deployment it only matters for `thing`, whose litellm is exposed on
wg0 (home network). If an instance ever becomes reachable outside a
private network, the flag must go and the steps below apply.

Introduced in the commit that adds `dangerously_permit_weak_or_unset_
master_key` to `services.litellm.nix` (search `git log -S
dangerously_permit_weak_or_unset_master_key`).

## What to do (only if thing's exposure ever grows beyond the home
network)

1. Provision an agenix secret `litellm-master-key` in `../priv/`
   (content: `LITELLM_MASTER_KEY=sk-<hex>`), declare the stub
   (`myconfig.secrets."litellm-master-key".dest =
   "/run/agenix/litellm-master-key"`) in the public repo, and wire
   `services.litellm.environmentFile` to it on thing.
2. Set the upstream key on every client that calls thing's litellm:
   the f13/p14 proxy `myconfig.ai.llmops.litellm.proxy.apiKey`
   (per-entry `os.environ/…` + its own `environmentFile`, since the
   value is a boot-time secret), open-webui, headroom
   (`ai.llmops.container.headroom.targetApiUrl` caller), and the local
   agents' litellm API-key settings on thing.
3. Drop `dangerously_permit_weak_or_unset_master_key` for the hardened
   instance(s) and verify: `systemctl restart litellm` starts, and one
   authenticated + one unauthenticated request behave as expected
   (401 without the key).
