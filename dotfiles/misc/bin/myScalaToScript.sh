#!/bin/sh

begin() {
  cat <<BEGIN
#!/bin/sh
exec scala -savecompiled "\$0" "\$@"
!#
BEGIN
}

end() {
  cat << END
//Class.main(Array(""))

// vim: set ft=scala :
END
}


begin > $1.sh
[[ -f $1 ]] && cat $1 >> $1.sh
end >> $1.sh

chmod +x $1.sh
