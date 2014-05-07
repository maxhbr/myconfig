$pdflatex = 'pdflatex -interaction=nonstopmode --shell-escape'; 
$dvi_previewer = "start xdvi";
$pdf_previewer = "zathura -l error -s -x '/home/hubi/bin/myTexWrapper.sh %{line} \"%{input}\"' %O %S"
