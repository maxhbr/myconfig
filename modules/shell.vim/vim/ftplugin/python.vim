" Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
" SPDX-License-Identifier: MIT
let g:pydiction_location = '~/.vim/pydiction/complete-dict'
" au FocusLost *.py :write
" au FocusLost *.py :echo "autoSaved"
setlocal omnifunc=pythoncomplete#Complete
setlocal sw=4 ts=4 et
" setlocal tw=72

setlocal nowrap
set ttyfast

" folding
setlocal foldenable
setlocal foldmethod=indent
setlocal foldlevel=7

noremap <c-F5> :w<CR>:!python "%"<CR>
