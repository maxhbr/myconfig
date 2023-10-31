{ config, pkgs, lib, myconfig, ... }: {
  config = {
    home-manager.sharedModules = [{
      programs.taskwarrior = {
        enable = true;
        config = {
          report.active = {
            columns = [ "id" "start" "entry.age" "priority" "project" "due" "description" ];
            labels = [ "ID" "Started" "Age" "Priority" "Project" "Due" "Description" ];
          };
          tasksh.autoclear=1;
        };
        colorTheme = "light-256";
        dataLocation = "~/task";
      };
      home.packages = with pkgs; [ taskwarrior-tui tasksh ];
      programs.fish = {
        shellAbbrs = {
          taskP = "task project:Priv";
          taskW = "task project:Work";
        };
      };
    }];
  };
}
