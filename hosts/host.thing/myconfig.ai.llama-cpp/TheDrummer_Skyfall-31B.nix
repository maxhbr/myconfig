let
  modelsPullDir = "/home/mhuber/models";
in
{
  rtxModels = [
    {
      name = "TheDrummer_Skyfall-31B-v4.2-Q6_K";
      path = "/models/bartowski-TheDrummer_Skyfall-31B-v4.2-GGUF/TheDrummer_Skyfall-31B-v4.2-Q6_K.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "bartowski/TheDrummer_Skyfall-31B-v4.2-GGUF/TheDrummer_Skyfall-31B-v4.2-Q6_K.gguf" ];
      };
      aliases = [ ];
      ttl = 900;
    }
  ];

  amdModels = [ ];
}
