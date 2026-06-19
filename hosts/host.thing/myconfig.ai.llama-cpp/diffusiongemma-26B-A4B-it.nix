let
  modelsPullDir = "/home/mhuber/models";
in
{
  rtxModels = [
    {
      name = "diffusiongemma-26B-A4B-it-Q6_K";
      path = "/models/unsloth-diffusiongemma-26B-A4B-it-GGUF/diffusiongemma-26B-A4B-it-Q6_K.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/diffusiongemma-26B-A4B-it-GGUF/diffusiongemma-26B-A4B-it-Q6_K.gguf" ];
      };
      ctxSize = 262144;
      params = [
        "--batch-size"
        "2048"
        "--ubatch-size"
        "512"
        "--threads"
        "1"
        "--jinja"
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
        "--top-k"
        "64"
      ];
      aliases = [
        "diffusiongemma-26B"
      ];
      ttl = 300;
    }
  ];

  amdModels = [ ];
}
