" Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
" SPDX-License-Identifier: MIT
setlocal sw=2 ts=2 et
setlocal iskeyword+='

"setlocal formatprg=pointfree

let g:ycm_semantic_triggers = {'haskell' : ['.']}
setlocal omnifunc=necoghc#omnifunc

let s:width = 80

noremap <c-F5> :w<CR>:!ghci "%"<CR>

" set formatprg=pointfree

function! HaskellModuleSection(...)
let name = 0 < a:0 ? a:1 : inputdialog("Section name: ")

return  repeat('-', s:width) . "\n"
            \       . "--  " . name . "\n"
            \       . "\n"
endfunction
nnoremap <silent> __s "=HaskellModuleSection()<CR>gp

function! HaskellModuleHeader(...)
let name = 0 < a:0 ? a:1 : inputdialog("Module: ")
let note = 1 < a:0 ? a:2 : inputdialog("Note: ")
let description = 2 < a:0 ? a:3 : inputdialog("Describe this module: ")

return  repeat('-', s:width) . "\n"
\       . "-- | \n"
\       . "-- Module      : " . name . "\n"
\       . "-- Note        : " . note . "\n"
\       . "-- \n"
\       . "-- " . description . "\n"
\       . "-- \n"
\       . repeat('-', s:width) . "\n"
\       . "\n"
endfunction
nnoremap <silent> __h "=HaskellModuleHeader()<CR>:0put =<CR>

" ===================================================================
" syntastic
let g:syntastic_auto_loc_list=1
