#!/bin/bash
#from:
# http://tex.stackexchange.com/questions/42236/pdfcrop-generates-larger-file

function usage () {
  echo "Usage: `basename $0` [Options] <input.pdf> [<output.pdf>]"
  echo
  echo " * Removes white margins from each page in the file. (Default operation)"
  echo " * Trims page edges by given amounts. (Alternative operation)"
  echo
  echo "If only <input.pdf> is given, it is overwritten with the cropped output."
  echo
  echo "Options:"
  echo
  echo " -m \"<left> [<top> [<right> <bottom>]]\""
  echo "    adds extra margins in default operation mode. Unit is bp. A single number"
  echo "    is used for all margins, two numbers \"<left> <top>\" are applied to the"
  echo "    right and bottom margins alike."
  echo
  echo " -t \"<left> [<top> [<right> <bottom>]]\""
  echo "    trims outer page edges by the given amounts. Unit is bp. A single number"
  echo "    is used for all trims, two numbers \"<left> <top>\" are applied to the"
  echo "    right and bottom trims alike."
  echo
  echo " -hires"
  echo "    %%HiResBoundingBox is used in default operation mode."
  echo
  echo " -help"
  echo "    prints this message."
}

c=0
mar=(0 0 0 0); tri=(0 0 0 0)
bbtype=BoundingBox

while getopts m:t:h: opt
do
  case $opt
  in
    m)
    eval mar=($OPTARG)
    [[ -z "${mar[1]}" ]] && mar[1]=${mar[0]}
    [[ -z "${mar[2]}" || -z "${mar[3]}" ]] && mar[2]=${mar[0]} && mar[3]=${mar[1]}
    c=0
    ;;
    t)
    eval tri=($OPTARG)
    [[ -z "${tri[1]}" ]] && tri[1]=${tri[0]}
    [[ -z "${tri[2]}" || -z "${tri[3]}" ]] && tri[2]=${tri[0]} && tri[3]=${tri[1]}
    c=1
    ;;
    h)
    if [[ "$OPTARG" == "ires" ]]
    then
      bbtype=HiResBoundingBox
    else
      usage 1>&2; exit 0
    fi
    ;;
    \?)
    usage 1>&2; exit 1
    ;;
  esac
done
shift $((OPTIND-1))

[[ -z "$1" ]] && echo "`basename $0`: missing filename" 1>&2 && usage 1>&2 && exit 1
input=$1;output=$1;shift;
[[ -n "$1" ]] && output=$1 && shift;

(
    [[ "$c" -eq 0 ]] && gs -dNOPAUSE -q -dBATCH -sDEVICE=bbox "$input" 2>&1 | grep "%%$bbtype"
    pdftk "$input" output - uncompress
) | perl -w -n -s -e '
  BEGIN {@m=split /\s+/, $mar; @t=split /\s+/, $tri;}
  if (/BoundingBox:\s+([\d\.\s]+\d)/) { push @bbox, $1; next;}
  elsif (/\/MediaBox\s+\[([\d\.\s]+\d)\]/) { @mb=split /\s+/, $1; next; }
  elsif (/pdftk_PageNum\s+(\d+)/) {
    $p=$1-1;
    if($c){
      $mb[0]+=$t[0];$mb[1]+=$t[1];$mb[2]-=$t[2];$mb[3]-=$t[3];
      print "/MediaBox [", join(" ", @mb), "]\n";
    } else {
      @bb=split /\s+/, $bbox[$p];
      $bb[0]+=$mb[0];$bb[1]+=$mb[1];$bb[2]+=$mb[0];$bb[3]+=$mb[1];
      $bb[0]-=$m[0];$bb[1]-=$m[1];$bb[2]+=$m[2];$bb[3]+=$m[3];
      print "/MediaBox [", join(" ", @bb), "]\n";
    }
  }
  print;
' -- -mar="${mar[*]}" -tri="${tri[*]}" -c=$c | pdftk - output "$output" compress
