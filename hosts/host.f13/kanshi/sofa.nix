# sofa layout on f13, captured with `niri msg --json outputs`
{ ... }:
{
  services.kanshi.settings = [
    {
      profile.name = "sofa";
      profile.outputs = [
        {
          criteria = "BOE NE135A1M-NY1 Unknown";
          mode = "2880x1920@120Hz";
          position = "0,1080";
          scale = 1.3;
        }
        {
          criteria = "HardCom Elektronik & Datateknik HDMI SPLITTER 0x01010101";
          mode = "1920x1080@60Hz";
          position = "170,0";
          scale = 1.0;
        }
      ];
    }
  ];
}
