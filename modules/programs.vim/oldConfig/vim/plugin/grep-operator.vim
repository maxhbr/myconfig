" Copyright 2014-2016 Maximilian Huber <oss@maximilian-huber.de>
" SPDX-License-Identifier: MIT

if 0 " grep operator?
  nnoremap <leader>g :set operatorfunc=<SID>GrepOperator<cr>g@
else
  nnoremap <leader>g :silent execute "grep! -R " . shellescape(expand("<cWORD>")) . " ." <cr>:copen<cr>
endif

vnoremap <leader>g :<c-u>call <SID>GrepOperator(visualmode())<cr>
function! s:GrepOperator(type)
  let saved_unnamed_register == @@
  if a:type ==# 'v'
    normal! `<v`>y
  elseif a:type ==# 'char'
    normal! `[v`]y
  else 
    return
  endif

  silent execute "grep! -R " . shellescape(@@) . " ."
  copen

  let @@ = saved_unnamed_register
endfunction

" vim:set ts=2 sw=2 sts=2 et fenc=utf-8 ff=unix foldmethod=marker foldmarker={{{,}}}:
