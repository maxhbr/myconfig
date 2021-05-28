{ pkgs, ... }: {
  programs.fish = {
    functions = {
      ex = ''
        for file in $argv
              if test -f $file
                  set opt ( echo "$file" | tr '[:upper:]' '[:lower:]' )
                  switch $opt
                      case '*.tar.bz2'
                          tar xjf $file
                      case '*.tar.gz'
                          tar xzf $file
                      case '*.tar.xz'
                          tar xvfJ $file
                      case '*.xz'
                          ${pkgs.xz}/bin/xz -d $file
                      case '*.tar.lzma'
                          tar --lzma -xvf $file
                      case '*.bz2'
                          ${pkgs.bzip2}/bin/bunzip2 $file
                      case '*.rar'
                          unrar e $file
                      case '*.gz'
                          ${pkgs.gzip}/bin/gunzip $file
                      case '*.tar'
                          tar xf $file
                      case '*.tbz2'
                          tar xjf $file
                      case '*.tgz'
                          tar xzf $file
                      case '*.zip'
                          ${pkgs.unzip}/bin/unzip $file
                      case '*.Z'
                          uncompress $file
                      case '*.7z'
                          ${pkgs.p7zip}/bin/7z x $file
                      case '*.jar'
                          ${pkgs.unzip}/bin/unzip $file
                      case '*.war'
                          ${pkgs.unzip}/bin/unzip $file
                      case '*.ear'
                          ${pkgs.unzip}/bin/unzip $file
                      case '*.deb'
                          ${pkgs.binutils}/bin/ar xv $file
                      case '*'
                          echo "'$file' of type '$opt' cannot be extracted via ex(), more info:"
                          file $file
                  end
              else
                  echo "'$file' is not a valid file"
              end
          end
      '';
    };
  };
}
