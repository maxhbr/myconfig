{pkgs, config, lib, ...}:
{
  config = {
    services.haproxy = {
      enable = true;
      config = ''
defaults
   mode    http
   option  httpchk

frontend frontend
   bind *:6443 ssl crt /etc/tls/nginx.pem
   redirect scheme https if !{ ssl_fc }
   mode http

   acl url_prometheus path_beg /prometheus
   # acl url_prometheus hdr(host)     -m beg prometheus.
   use_backend prometheus_backend if url_prometheus

   default_backend grafana_backend

backend grafana_backend
   option forwardfor
   http-request set-header X-Forwarded-Port %[dst_port]
   http-request add-header X-Forwarded-Proto https if { ssl_fc }
   option httpchk HEAD / HTTP/1.1\r\nHost:localhost
   server grafana01 127.0.0.1:2342 check inter 2000

backend prometheus_backend
   server prometheus01 127.0.0.1:9090 check inter 2000

# listen stats *:1936
#     stats enable
#     stats uri /
#     stats hide-version
#     stats auth someuser:password
'';
    };
    networking.firewall.allowedTCPPorts = [ 6443 ];
  };
}
