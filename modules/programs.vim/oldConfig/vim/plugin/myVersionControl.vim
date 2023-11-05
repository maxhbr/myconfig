" Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
" SPDX-License-Identifier: MIT
" minimal git/svn wrapper
"
" Last Modified: Mon Aug 25, 2014  05:26
"
" ====  Git  ========================================================
command! GCommit   call myVersionControl#GitCommit()
command! GPush     call myVersionControl#GitPush()
command! GPull     call myVersionControl#GitPull()
command! GAdd      call myVersionControl#GitAdd()
command! GStatus   call myVersionControl#GitStatus()
command! GCheckout call myVersionControl#GitCheckout()
command! GBranch   call myVersionControl#GitBranch()
command! GAuto     call myVersionControl#GitAuto()

nnoremap <silent> _gc :call myVersionControl#GitCommit()<cr>

" ====  SVN  ========================================================
command! SVNCommit call myVersionControl#SVNCommit()
command! SVNUpdate call myVersionControl#SVNUpdate()
command! SVNAdd    call myVersionControl#SVNAdd()

" vim:set ts=2 sw=2 sts=2 et fenc=utf-8 ff=unix foldmethod=marker foldmarker={{{,}}}:
