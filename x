        #!/usr/bin/env bash
        printf x
        STUB
                done
  '';
  in builtins.replaceStrings ["\n"] ["\\n"] s'' 2>&1 | head -3
