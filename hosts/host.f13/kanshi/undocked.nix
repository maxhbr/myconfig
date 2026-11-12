# undocked layout on f13, captured with `niri msg --json outputs`
{ ... }:
{
  services.kanshi.settings = [
    {
      profile.name = "undocked";
      profile.outputs = [
        {
          criteria = "BOE NE135A1M-NY1 Unknown";
          mode = "2880x1920@120Hz";
          position = "0,0";
          scale = 1.3;
        }
      ];
      # no external audio device while undocked: keep the Radeon HDMI
      # audio card off, but bring the internal speakers back (they are the
      # only output here)
      profile.exec = [
        "kanshi-audio-card alsa_card.pci-0000_c1_00.1 off"
        "kanshi-audio-card alsa_card.pci-0000_c1_00.6 auto"
      ];
    }
  ];
}
