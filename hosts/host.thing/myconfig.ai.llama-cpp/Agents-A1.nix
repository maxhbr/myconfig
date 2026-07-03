{ modelsPullDir }:
{
  amdModels = [
    {
      name = "InternScience-Agents-A1-Q8_0";
      path = "/models/InternScience-Agents-A1-Q8_0-GGUF/Agents-A1-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "InternScience/Agents-A1-Q8_0-GGUF/Agents-A1-Q8_0.gguf" ];
      };
      ttl = 1500;
    }
  ];
}
