$pdflatex = 'pdflatex -interaction=nonstopmode --shell-escape'; 
$dvi_previewer = "start xdvi";
# $pdf_previewer = "zathura -l error -s -x 'myTexWrapper.sh %{line} \"%{input}\"' %O %S"
$pdf_previewer = "zathura -l error -x 'myTexWrapper.sh %{line} \"%{input}\"' %O %S"
