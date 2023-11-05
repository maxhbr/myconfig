" Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
" SPDX-License-Identifier: MIT
setlocal sw=2 ts=2 et

noremap <c-F5> :w<CR>:!sh "%"<CR>

function! GenHeader(...)
if has("autocmd")
  return  repeat(a:1, a:2) . "\n"
  \       . a:1 . "\n"
  \       . a:1 . " Written by Maximilian-Huber.de\n"
  \       . a:1 . "\n"
  \       . a:1 . " Last Modified: \n"
  \       . "\n"
else
  return  repeat(a:1, a:2) . "\n"
  \       . a:1 . "\n"
  \       . a:1 . " Written by Maximilian-Huber.de\n"
  \       . "\n"
endif
endfunction

nnoremap <silent> __h "=GenHeader('#',80)<CR>:0put =<CR>
