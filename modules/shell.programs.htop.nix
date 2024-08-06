# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: {
  config = {
    environment.systemPackages = with pkgs; [ htop ];
    home-manager.sharedModules = [
      ({ config, ... }: {
        xdg.configFile."htop/htoprc".force = true;
        programs.htop = {
          enable = true;
          settings = {
            color_scheme = 6;
            cpu_count_from_one = 0;
            delay = 15;
            fields = with config.lib.htop.fields; [
              PID
              USER
              PRIORITY
              NICE
              M_SIZE
              M_RESIDENT
              M_SHARE
              STATE
              PERCENT_CPU
              PERCENT_MEM
              IO_RATE
              TIME
              COMM
            ];
            sort_direction = 0;
            show_thread_names = 0;
            show_program_path = 0;
            highlight_base_name = 1;
            highlight_megabytes = 1;
            highlight_threads = 1;
            tree_view = 0;
            detailed_cpu_time = 1;

            left_meters = [ "AllCPUs2" ];
            left_meter_modes = with config.lib.htop.modes; [ Bar ];
            right_meters = [
              "Uptime"
              "Battery"
              "Hostname"
              "CPU"
              "Memory"
              "Swap"
              "LoadAverage"
              "Tasks"
              "Systemd"
              "Zram"
            ];
            right_meter_modes = with config.lib.htop.modes; [
              Text
              Text
              Text
              Bar
              Bar
              Bar
              Text
              Text
              Text
              Text
            ];
          };
        };
      })
    ];
  };
}
