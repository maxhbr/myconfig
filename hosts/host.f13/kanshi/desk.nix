# desk layout on f13, captured with `niri msg --json outputs`
{ ... }:
{
  services.kanshi.settings = [
    {
      profile.name = "desk";
      profile.outputs = [
        {
          criteria = "BOE NE135A1M-NY1 Unknown";
          mode = "2880x1920@120Hz";
          position = "0,0";
          scale = 1.3;
        }
        {
          criteria = "Dell Inc. DELL U3824DW FNGQZR3";
          mode = "3840x1600@60Hz";
          position = "2216,0";
          scale = 1.0;
        }
      ];
      # the U3824DW is silent; keep the Radeon HDMI audio card off so it
      # never grabs the default sink
      profile.exec = [ "kanshi-audio-card alsa_card.pci-0000_c1_00.1 off" ];
    }
  ];
}
