if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

setlocal sw=4
setlocal ts=4
setlocal et
setlocal tw=79
setlocal linebreak

setlocal foldmarker={{{,}}}

" setlocal fo+=croql
" setlocal comments=:%>,:%
setlocal commentstring=\%\ %s

if exists("loaded_matchit")
  let s:conditionalEnd = '\(([^()]*\)\@!\<end\>\([^()]*)\)\@!'
  let b:match_words = '\<classdef\>\|\<methods\>\|\<events\>\|\<properties\>\|\<if\>\|\<while\>\|\<for\>\|\<switch\>\|\<try\>\|\<function\>:' . s:conditionalEnd
endif
