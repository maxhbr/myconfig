" Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
" SPDX-License-Identifier: MIT
" minimal git/svn wrapper written by maximilian-huber.de
"
" Last Modified: Mon Aug 25, 2014  05:26
"
" ====  Git  ========================================================{{{
function! myVersionControl#GitCommit()
  let msg = 0 < a:0 ? a:1 : inputdialog("Msg: ")
  execute '!git commit -a -m "' msg '"'
endfunction

function! myVersionControl#GitPush()
  execute '!git push'
endfunction

function! myVersionControl#GitPull()
  execute '!git pull'
endfunction

function! myVersionControl#GitAdd()
  execute '!git add %'
endfunction

function! myVersionControl#GitStatus()
  execute '!git status'
endfunction

function! myVersionControl#GitCheckout()
  let msg = 0 < a:0 ? a:1 : inputdialog("Branch: ")
  execute '!git checkout ' msg
endfunction

function! myVersionControl#GitBranch()
  let msg = 0 < a:0 ? a:1 : inputdialog("Create Branch: ")
  execute '!git checkout -b ' msg
endfunction

function! myVersionControl#GitAuto()
  call myVersionControl#GitCommit()
  call myVersionControl#GitPush()
endfunction

"                                                                    }}}
" ====  SVN  ========================================================{{{
function! myVersionControl#SVNCommit()
  let msg = 0 < a:0 ? a:1 : inputdialog("Msg: ")
  execute '!svn commit -m "' msg '"'
endfunction

function! myVersionControl#SVNUpdate()
  execute '!svn update'
endfunction

function! myVersionControl#SVNAdd()
  execute '!svn add %'
endfunction

" vim:set ts=2 sw=2 sts=2 et fenc=utf-8 ff=unix foldmethod=marker foldmarker={{{,}}}:
