let
  modelsPullDir = "/home/mhuber/models";
in
{
  amdModels = [
    {
      name = "DeepSeek-V4-Flash-IQ2XXS-w2Q2K-AProjQ8-SExpQ8-OutQ8-chat-v2-imatrix";
      path = "/models/ds4/gguf/DeepSeek-V4-Flash-IQ2XXS-w2Q2K-AProjQ8-SExpQ8-OutQ8-chat-v2-imatrix.gguf";
      aliases = [
        "DeepSeek-V4-Flash"
      ];
      ttl = 1800;
    }
  ];
}
