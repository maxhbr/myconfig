# desk2 layout on f13, captured with `niri msg --json outputs`
{ ... }:
{
  services.kanshi.settings = [
    {
      profile.name = "desk2";
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
        {
          criteria = "PNP(HJW) HDMI TO USB 0x00000001";
          mode = "1920x1080@60Hz";
          position = "3176,1600";
          scale = 1.0;
        }
      ];
      # like `desk`: the U3824DW is silent; keep the Radeon HDMI audio
      # card and the internal speakers off — audio goes through the Scarlett
      profile.exec = [
        "l alsa_card.pci-0000_c1_00.1 off"
        "l alsa_card.pci-0000_c1_00.6 off"
      ];
    }
  ];
}
