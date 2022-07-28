{ config, pkgs, lib, myconfig, ... }: {
  config = {
    home-manager.sharedModules = [{
      programs.taskwarrior = {
        enable = true;
        config = {
            report.active.columns = [ "id" "start" "entry.age" "priority" "project" "due" "description" ];
            report.active.labels  = [ "ID" "Started" "Age" "Priority" "Project" "Due" "Description" ];
        };
        colorTheme = "light-256";
        dataLocation = "./task";
      };
      home.packages = with pkgs; [taskwarrior-tui];
    }];
  };
}
