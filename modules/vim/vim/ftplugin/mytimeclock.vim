" Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
" SPDX-License-Identifier: MIT
if exists("g:loaded_mytimeclock")
    finish
endif
let g:loaded_mytimeclock = 1

if !exists("*strftime")
    call add(s:errors, "Requires a system with strftime()")
endif

function! s:BeginTiming()
    let days=strftime("%j")
    let hours=strftime("%H")
    let mins=strftime("%M")

    if getline('.') =~? '\v^\s*$'
        execute 'normal! I' . days . " " . hours . ":" . mins . ': '
    else
        execute 'normal! o' . days . " " . hours . ":" . mins . ': '
    endif
endfunction

function! s:EndTiming()
    let splt=split(getline('.'),':')
    if len(splt) < 3
        return
    endif
    let subsplt=split(splt[0])
    let hours=strftime("%H") - subsplt[1] + 24*(strftime("%j") - subsplt[0])
    let mins=strftime("%M") - splt[1]
    let str=strftime("%y-%d-%m") . "    " . hours . ":" . mins . "   " . splt[2]
    execute 'normal! ddO' . str
endfunction

nnoremap _a :<c-u>call <SID>BeginTiming()<cr>
nnoremap _e :<c-u>call <SID>EndTiming()<cr>
