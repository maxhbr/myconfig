{ modelsPullDir }:
{
  amdModels = [
    {
      name = "Qwen-AgentWorld-35B-A3B-Q8_0";
      path = "/models/unsloth-Qwen-AgentWorld-35B-A3B-GGUF/Qwen-AgentWorld-35B-A3B-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen-AgentWorld-35B-A3B-GGUF/Qwen-AgentWorld-35B-A3B-Q8_0.gguf" ];
      };
      ttl = 1500;
    }
  ];
}
