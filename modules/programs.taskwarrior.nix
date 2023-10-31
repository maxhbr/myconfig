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

          uda.priority.label="Priority";
          uda.priority.type="string";
          uda.priority.values="H,M,,L";

          urgency.uda.priority.H.coefficient=6.0;
          urgency.uda.priority.M.coefficient=3.9;
          urgency.uda.priority.L.coefficient=0.9;
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
