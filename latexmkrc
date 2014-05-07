$pdflatex = 'pdflatex -interaction=nonstopmode --shell-escape'; 
$dvi_previewer = "start xdvi";
$pdf_previewer = "zathura -l error -s -x 'vim --servername TEX --remote +%{line} \"%{input}\"' %O %S"
