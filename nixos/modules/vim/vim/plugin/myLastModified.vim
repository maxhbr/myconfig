" Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
" SPDX-License-Identifier: MIT
" Mainly stolen from somewhere
if has("autocmd")
  " If buffer modified, update any 'Last modified: ' in the first 20 lines.
  " Restores cursor and window position using save_cursor variable.
  function! s:LastModified()
    if &modified
      let save_cursor = getpos(".")
      let n = min([20, line("$")])
      keepjumps exe '1,' . n . 's#^\(.\{,10}Last [mM]odified: \).*#\1' .
            \ strftime('%a %b %d, %Y  %I:%M%p') . '#e'
      call histdel('search', -1)
      call setpos('.', save_cursor)
    endif
  endfunction
  augroup lastModified
    autocmd!
    autocmd BufWritePre * call <SID>LastModified()
  augroup END
endif
" vim:set ts=2 sw=2 sts=2 et fenc=utf-8 ff=unix foldmethod=marker foldmarker={{{,}}}:
