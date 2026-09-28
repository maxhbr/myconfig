{ modelsPullDir }:
{
  amdModels = [
    {
      name = "Ornith-1.0-35B-Q8_0";
      path = "/models/bartowski-deepreinforce-ai_Ornith-1.0-35B-GGUF/deepreinforce-ai_Ornith-1.0-35B-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "bartowski/deepreinforce-ai_Ornith-1.0-35B-GGUF/deepreinforce-ai_Ornith-1.0-35B-Q8_0.gguf"
        ];
      };
      ttl = 1800;
    }
  ];
}
