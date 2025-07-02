{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.services.swaync.enable {
    services.swaync = {
      style = ''
        .notification-row {
          outline: none;
        }

        .notification-row:focus,
        .notification-row:hover {
          background: @noti-bg-focus;
        }

        .notification {
          border-radius: 12px;
          margin: 6px 12px;
          box-shadow: 0 0 0 1px rgba(0, 0, 0, 0.3), 0 1px 3px 1px rgba(0, 0, 0, 0.7),
            0 2px 6px 2px rgba(0, 0, 0, 0.3);
          padding: 0;
        }
      '';
      settings = {
        positionX = "right";
        positionY = "bottom";
        layer = "overlay";
        control-center-layer = "top";
        layer-shell = true;
        cssPriority = "application";
        control-center-margin-top = 0;
        control-center-margin-bottom = 0;
        control-center-margin-right = 0;
        control-center-margin-left = 0;
        notification-2fa-action = true;
        notification-inline-replies = false;
        notification-icon-size = 32;
        notification-body-image-height = 100;
        notification-body-image-width = 200;
      };
      # backgroundColor = "#285577BB";
      # defaultTimeout = 5000;
    };
  };
}

