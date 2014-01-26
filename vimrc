" ~/.vimrc
"
" Written by Maximilian-Huber.de
"
" Last modified: Sun Jan 26, 2014  02:47
"
" !!!
"       this config will automatically download Vundle from git, and then it
"       will install all plugins
" !!!

"tipps / Keybindings                                                 {{{
" write as root: :w !sudo tee % > /dev/null
"
":r! date
"
" Vundle help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" PYTHON
"   ]t      -- Jump to beginning of block
"   ]e      -- Jump to end of block
"   ]v      -- Select (Visual Line Mode) block
"   ]<      -- Shift block to left
"   ]>      -- Shift block to right
"   ]#      -- Comment selection
"   ]u      -- Uncomment selection
"   ]c      -- Select current/previous class
"   ]d      -- Select current/previous function
"   ]<up>   -- Jump to previous line with the same/lower indentation
"   ]<down> -- Jump to next line with the same/lower indentation
"
" Custom
"   ,r      -- reload vimrc
"   ,ev     -- edit vimrc
"   ,dt     -- DeleteTrailing
"   ,s      -- search and replace word under cursor
"   ,T      -- expand all Tabs
"
" Plugins
"   ,lj     -- LustyJuggler
"   ,ww     -- vimwiki
"   :MRU    -- mru
"   ,ig     -- vim-indent-guides
"                                                                    }}}

" auto reload when saving
autocmd! bufwritepost .vimrc source %

" This must be first, because it changes other options as a side effect.
set nocompatible

" ===================================================================
" ====  General  ====================================================
" ==================================================================={{{

if has("syntax")
  syntax on
endif

if has("autocmd")
  filetype plugin on
  filetype indent on

  " completion
  set ofu=syntaxcomplete#Complete
  "filetype plugin indent on
  set completeopt=longest,menuone

  " jump to the last position when reopening a file
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

  " resize splits
  au VimResized * exe "normal! \<c-w>="

  "save folding (shouldnt run every time)
  "au BufWinLeave * mkview
  "au BufWinEnter * silent loadview

  " leave paste mode, when exit insert mode
  au InsertLeave * set nopaste
endif

" Use Unix as the standard file type
set ffs=unix,dos,mac
set encoding=utf8

set virtualedit=all
set backspace=2
" set backspace=indent,eol,start
set showcmd   " Show (partial) command in status line.
set showmatch " Show matching brackets.
set autowrite " Automatically save before commands like :next and :make
if has("mouse")
  set mouse=a " Enable mouse usage (all modes) alternativ nvc
  set mousehide
endif
set visualbell " ausgehebelt durch noerrorbells ?
set noerrorbells
set hidden
set autoread
set magic      " For regular expressions turn magic on
set splitbelow " set splitright
set autochdir

set cpoptions+=n
set showbreak=\ \ \ ↳

set wildmenu "Kommando Zeilen Vervollständigung
"set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png
set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png,*.aux,*.bbl,*.blg,*.fdb_latexmk,*.fls,*.idx,*.ilg,*.ind,*.nlo,*.toc,*.hi
set wildmode=list:longest

" Suffixes that get lower priority when doing tab completion for filenames.
" These are files we are not likely to want to edit or read.
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc

" ====  Scrolling  =================================================={{{

set scrolljump=5
set scrolloff=5         "Start scrolling when we're x lines away from margins
set sidescrolloff=10
set sidescroll=1
"                                                                    }}}
" ====  Search  ====================================================={{{

" highlight searches, searches begin immediately
set hlsearch
set incsearch
" case sensitiv, if a uppercase letter is contained
set smartcase
set ignorecase
"                                                                    }}}
" ====  Folding / indenting  ========================================{{{

set foldmethod=marker
set foldmarker={{{,}}}

" indending
set autoindent
set smartindent
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab
set smarttab

set list
set listchars=tab:>.,trail:…,extends:#,nbsp:. " …°⎼
set fillchars=vert:┃,diff:⎼,fold:⎼
"                                                                    }}}
" ====  Performance  ================================================{{{
set ttyfast

" for powersave, stops the blinking cursor
let &guicursor = &guicursor . ",a:blinkon0"
" Speed hack
hi NonText cterm=NONE ctermfg=NONE
" stops slow responding in large files
set synmaxcol=128
"                                                                    }}}
" ====  Line Numbering  ============================================={{{
au InsertEnter * :set nu
au InsertLeave * :set rnu
au FocusLost * :set nu
au FocusGained * :set rnu
set rnu
"                                                                    }}}
" ====  Backup / Undo  =============================================={{{

set history=1000
set undolevels=1000

" Keep undo history across sessions, by storing in file.
silent !mkdir ~/.vim/backups > /dev/null 2>&1
set undodir=~/.vim/backups
set undofile

" swap
set swapfile
set dir=/tmp " tmpfs

" no backup!
set nobackup
" backup to ~/.tmp
"set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
"set backupskip=/tmp/*,/private/tmp/*
"set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
"set writebackup
"                                                                    }}}
" ====  Desing ======================================================{{{

set title
set cursorline
set nocursorcolumn " hat probleme mit acp (Popup)
set textwidth=0    " Don't wrap lines by default
set nowrap
set ruler

set showmode "show when you are in insert mode

" statusline
set laststatus=2
" default the statusline to green when entering Vim
"hi StatusLine term=reverse ctermfg=0 ctermbg=2 gui=bold,reverse


set statusline=%<%f\          " custom statusline
set stl+=[%{&ff}]             " show fileformat
set stl+=%y%m%r%=
set stl+=%-14.(%l,%c%V%)\ %P

if has("gui_running")
  " in gui
  map <S-Insert> <MiddleMouse>
  map! <S-Insert> <MiddleMouse>
  set guioptions-=T  " no toolbar
  " Use console messages instead of GUI dialogs
  set guioptions+=c

  let g:indent_guides_auto_colors = 0
  autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=black ctermbg=4
else
  " no gui
  set t_Co=256
  set background=dark
endif

if filereadable(expand("$VIMRUNTIME/colors/mustang.vim"))
  colorscheme mustang
elseif filereadable(expand("$HOME/.vim/colors/mustang.vim"))
  colorscheme mustang
else
  colorscheme default
endif

" ====  hilight to long lines  ======================================{{{
if exists('+colorcolumn')
    set colorcolumn=80
    highlight ColorColumn ctermbg=233 guibg=#592929
else
    highlight OverLength ctermbg=red ctermfg=white guibg=#592929
    match OverLength /\%81v.\+/
endif
"                                                                    }}}
"                                                                    }}}
" ===================================================================}}}
" ====  Functions  ==================================================
" ==================================================================={{{

" Kommenting fpr c bzw php
function! Komment()
  if getline(".") =~ '\/\*'
    let hls=@/
    s/^\/\*//
    s/*\/$//
    let @/=hls
  else
    let hls=@/
    s/^/\/*/
    s/$/*\//
    let @/=hls
  endif
endfunction

" delete all trails
" use :call DeleteTrailing
" or <Leader>dt
func! DeleteTrailing()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc

" If buffer modified, update any 'Last modified: ' in the first 20 lines.
" 'Last modified: ' can have up to 10 characters before (they are retained).
" Restores cursor and window position using save_cursor variable.
function! LastModified()
  if &modified
    let save_cursor = getpos(".")
    let n = min([20, line("$")])
    keepjumps exe '1,' . n . 's#^\(.\{,10}Last modified: \).*#\1' .
          \ strftime('%a %b %d, %Y  %I:%M%p') . '#e'
    call histdel('search', -1)
    call setpos('.', save_cursor)
  endif
endfun
autocmd BufWritePre * call LastModified()

" open files via ranger
fun! RangerChooser()
  exec "silent !ranger --choosefile=/tmp/chosenfile " . expand("%:p:h")
  if filereadable('/tmp/chosenfile')
    exec 'edit ' . system('cat /tmp/chosenfile')
    call system('rm /tmp/chosenfile')
  endif
  redraw!
endfun
map ,R :call RangerChooser()<CR>

function! InsertTabWrapper()
  let col = col('.') - 1
  if !col || getline('.')[col - 1] !~ '\k'
    return "\<tab>"
  else
    return "\<c-p>"
  endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
inoremap <s-tab> <c-n>

function! MyHtmlEscape()
  silent s/ö/\&ouml;/eg
  silent s/ä/\&auml;/eg
  silent s/ü/\&uuml;/eg
  silent s/Ö/\&Ouml;/eg
  silent s/Ä/\&Auml;/eg
  silent s/Ü/\&Uuml;/eg
  silent s/ß/\&szlig;/eg
endfunction

function! NumberToggle()
  if(&relativenumber == 1)
    set number
  else
    set relativenumber
  endif
endfunc

" ===================================================================}}}
" ====  Keymappings  ================================================
" ==================================================================={{{

" overwrite those annoying commands
cnoreabbrev <expr> W ((getcmdtype() is# ':' && getcmdline() is# 'W')?('w'):('W'))
cnoreabbrev <expr> Q ((getcmdtype() is# ':' && getcmdline() is# 'Q')?('q'):('Q'))

nnoremap ; :

let mapleader=","

nmap <Leader>r :source $MYVIMRC

"Make Y behave like other capitals
map Y y$

"nmap <silent> <leader>ev :tabedit $MYVIMRC<CR>
nmap <silent> <leader>ev :e $MYVIMRC<CR>
"nmap <silent> <leader>sv :so $MYVIMRC<CR>

" ersetzt durch NerdComment
"nmap <silent> <Leader>k :call Komment()<CR>

nmap <Leader>dt :call DeleteTrailing()

" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

" Make < > shifts keep selection
vnoremap < <gv
vnoremap > >gv

" Use Q for formatting the current paragraph (or selection)
vmap <leader>Q gq
nmap <leader>Q gqap

" http://vim.wikia.com/wiki/Avoid_the_escape_key
" Press i to enter insert mode, and kj to exit.
"imap kj <Esc>
" On gvim and Linux console Vim, you can use Alt-Space.
imap <M-Space> <Esc>

nnoremap K <nop>

inoremap <F1> <nop>
nnoremap <F1> <nop>
vnoremap <F1> <nop>

" clear hilighting
nmap <silent> ,/ :nohlsearch<CR>

"save without sudo
cmap w!! w !sudo tee % >/dev/null

noremap <silent> <F1> :split<CR>
noremap <silent> <F2> :vsplit<CR>
noremap <silent> <F3> :tabnew<CR>
noremap <silent> <F4> :close<CR>

noremap <silent> <F5> :update<CR>
inoremap <silent> <F5> <Esc>:update<CR>

" Use arrow key to change buffer
noremap <silent> <F7> :bp<CR>
noremap <silent> <F8> :bn<CR>
noremap <silent> <F9> :bd<CR>

noremap <silent> <F10> :q!<CR>
inoremap <silent> <F10> <Esc>:q!<CR>

set pastetoggle=<F11>

"Markdown to HTML
nmap <leader>md :%!~/bin/Markdown.pl --html4tags <cr>

"easyer increment/decrement
nnoremap + <C-a>
nnoremap - <C-x>

"Open last/alternate buffer
noremap <Leader><Leader> <C-^>

nnoremap <leader>T :set expandtab<cr>:retab!<cr>

nnoremap <Leader>s :%s/\<<C-r><C-w>\>//g<Left><Left>

map <Leader>S :SyntasticToggleMode<CR>

" ====  Movement  ===================================================

" jump to next row instead of next line
nnoremap j gj
nnoremap k gk

nnoremap <c-j> 5j
nnoremap <c-k> 5k

" control-left & right arrows switch between tabs
map <c-Left> :tabp<CR>
map <c-Right> :tabn<CR>

"inoremap <expr> <Tab>     pumvisible() ? "\<C-y>" : "\<Tab>"
"inoremap <expr> <CR>      pumvisible() ? "\<C-e><CR>" : "\<CR>"

" force vim keys
"nnoremap <up> <nop>
"nnoremap <down> <nop>
"nnoremap <left> <nop>
"nnoremap <right> <nop>

" Use arrow key to change buffer
"noremap <left> :bp<CR>
"noremap <right> :bn<CR>

" ===================================================================}}}
" ====  Git / SVN   =================================================
" ==================================================================={{{

" ====  Git  ========================================================
function! GitCommit() 
  let msg = 0 < a:0 ? a:1 : inputdialog("Msg: ")
  execute '!git commit -a -m "' msg '"'
endfunction

function! GitPush() 
  execute '!git push'
endfunction

function! GitPull() 
  execute '!git pull'
endfunction

function! GitAdd() 
  execute '!git add %'
endfunction

command! GCommit call GitCommit()
command! GPush   call GitPush()
command! GPull   call GitPull()
command! GAdd    call GitAdd()

nmap <silent> _gc :GCommit<cr>

" ====  SVN  ========================================================
function! SVNCommit() 
  let msg = 0 < a:0 ? a:1 : inputdialog("Msg: ")
  execute '!svn commit -m "' msg '"'
endfunction

function! SVNUpdate() 
  execute '!svn update'
endfunction

function! SVNAdd() 
  execute '!svn add %'
endfunction

command! SVNCommit call SVNCommit()
command! SVNUpdate call SVNUpdate()
command! SVNAdd    call SVNAdd()

nmap <silent> _sc :GCommit<cr>

" ===================================================================}}}
" ====  Filetype specific  ==========================================
" ==================================================================={{{

function! SetPythonFile()
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

  "invisible chars
  " autocmd BufWrite *.py :call DeleteTrailing()

  "map <F5> :w<CR>:!python "%"<CR>
endfunction

function! SetHaskellFile()
  setlocal sw=2 ts=2 et
  setlocal iskeyword+='

  "setlocal formatprg=pointfree

  let s:width = 80

  function! HaskellModuleSection(...)
    let name = 0 < a:0 ? a:1 : inputdialog("Section name: ")

    return  repeat('-', s:width) . "\n"
                \       . "--  " . name . "\n"
                \       . "\n"

  endfunction

  nmap <silent> __s "=HaskellModuleSection()<CR>gp

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

  nmap <silent> __h "=HaskellModuleHeader()<CR>:0put =<CR>

  " ===================================================================
  " syntastic
  let g:syntastic_auto_loc_list=1
endfunction

function! SetJavaFile()
  setlocal shiftwidth=4 softtabstop=4 tabstop=4 expandtab

  " folding
  setlocal foldenable
  setlocal foldmethod=syntax

  setlocal nowrap

  autocmd VimEnter * NERDTree
  autocmd VimEnter * wincmd p
endfunction

function! SetTextFile()
  " text

  setlocal wrap
  setlocal textwidth=79

  setlocal linebreak

endfunction

function! SetLaTeXFile()

  "nmap <leader>cl :! runlatex -pdf % > logfile 2>&1 &<CR><CR>
  "nmap <leader>oe :! llpp %:r.pdf > /dev/null 2>&1 &<CR><CR>
  "nmap <leader>oa :! llpp *.pdf > /dev/null 2>&1 &<CR><CR>

  setlocal wrap
  setlocal sw=2
  setlocal ts=2
  setlocal et
  setlocal textwidth=79
  setlocal cc=80

  setlocal linebreak

  setlocal foldmarker={{{,}}}
  "save folding
  "au BufWinLeave * mkview
  "au BufWinEnter * silent loadview

  "if has("gui_running")
    "set spell
    "set spelllang=de,en
    "setlocal spellsuggest=9
  "endif

  "map \gq ?^$\\|^\s*\(\\begin\\|\\end\\|\\label\)?1<CR>gq//-1<CR>
  "omap lp ?^$\\|^\s*\(\\begin\\|\\end\\|\\label\)?1<CR>//-1<CR>.<CR>

  "inoremap <expr>" getline('.')[col(".")-2] =~ "\\s" ? "\"`\"\'<left><left>" : "\"'"
  inoremap <expr>[ getline('.')[col(".")-2] =~ "\\" ? "[<C-v>u005c]<left><left>" : "[" 
  "inoremap <expr>{ getline('.')[col(".")-2] =~ "\\" ? "{<C-v>u005c}<left><left>" : "{" 

  set iskeyword+=: " type /ref{fig: and prec <C-n> to autocomplete references
  set iskeyword+=- " same with -
  set iskeyword+=_ " same with _

  "setlocal noautoindent
  setlocal nocindent
  setlocal nosmartindent
  setlocal indentexpr=

  setlocal autoindent

  "" Conceal
  "colorscheme topfunky-light
  "set cole=2
  "let g:tex_conceal= 'adgm'
  "hi Conceal guibg=White guifg=DarkMagenta

  setlocal spell
  set spelllang=de_de,en_us
  set spellfile=~/.vim/spellfile.add

  highlight clear SpellBad
  highlight SpellBad term=standout ctermfg=1
  highlight SpellBad term=underline cterm=underline
  highlight clear SpellCap
  highlight SpellCap term=underline cterm=underline
  highlight clear SpellRare
  highlight SpellRare term=underline cterm=underline
  highlight clear SpellLocal
  highlight SpellLocal term=underline cterm=underline
endfunction

function! SetCssFile()
  setlocal sw=4
  setlocal ts=4
  setlocal et
  setlocal tw=79
  setlocal linebreak
  if exists('+colorcolumn')
    setlocal colorcolumn=80
  endif
endfunction

function! SetMatlabFile()
  setlocal sw=4
  setlocal ts=4
  setlocal et
  setlocal tw=79
  setlocal linebreak

  setlocal foldmarker={{{,}}}
  if exists('+colorcolumn')
    setlocal colorcolumn=80
  endif
endfunction

function! SetMailFile()
  set textwidth=70 wrap nonumber
  set spell
  set spell spelllang=de_de
  set spellfile=~/.vim/spellfile.add

  highlight clear SpellBad
  highlight SpellBad term=standout ctermfg=1
  highlight SpellBad term=underline cterm=underline
  highlight clear SpellCap
  highlight SpellCap term=underline cterm=underline
  highlight clear SpellRare
  highlight SpellRare term=underline cterm=underline
  highlight clear SpellLocal
  highlight SpellLocal term=underline cterm=underline
endfunction

augroup filetypedetect
  au!
  autocmd BufRead,BufNewFile tmpmsg-*.txt set filetype=mail
  autocmd BufRead,BufNewFile *.tex set filetype=tex
  autocmd BufRead,BufNewFile *.log setlocal autoread
  autocmd BufRead,BufNewFile *.nlogo set filetype=nlogo
  "au! BufRead,BufNewFile *.m,*.oct setfiletype matlab
augroup END

augroup vimrc_autocmds
  au!
  autocmd FileType sh setlocal sw=2 ts=2 et
  autocmd FileType php setlocal sw=2 ts=2 et
  autocmd FileType tex     call SetLaTeXFile()
  autocmd FileType haskell call SetHaskellFile()
  autocmd FileType java    call SetJavaFile()
  autocmd FileType matlab  call SetMatlabFile()
  autocmd FileType python  call SetPythonFile()
  autocmd FileType human   call SetTextFile()
  autocmd FileType text    call SetTextFile()
  autocmd FileType mail    call SetMailFile()
  autocmd FileType txt     call SetTextFile()
  autocmd FileType css     call SetCssFile()
  autocmd FileType less    call SetCssFile()
  " in makefiles, don't expand tabs to spaces
  autocmd FileType make setlocal noexpandtab shiftwidth=8
augroup END

augroup Shebang
  autocmd BufNewFile *.sh 0put =\"#!/bin/sh\"|$
  autocmd BufNewFile *.py 0put =\"#!/usr/bin/env python\<nl># -*- coding: iso-8859-15 -*-\<nl>\"|$
  autocmd BufNewFile *.rb 0put =\"#!/usr/bin/env ruby\<nl># -*- coding: None -*-\<nl>\"|$
augroup END

" Transparent editing of gpg encrypted files.
" Placed Public Domain by Wouter Hanegraaff <wouter@blub.net>
" (asc support and sh -c"..." added by Osamu Aoki)
augroup aencrypted
  au!
  " First make sure nothing is written to ~/.viminfo while editing
  " an encrypted file.
  autocmd BufReadPre,FileReadPre      *.asc set viminfo=
  " We don't want a swap file, as it writes unencrypted data to disk
  autocmd BufReadPre,FileReadPre      *.asc set noswapfile
  " Switch to binary mode to read the encrypted file
  autocmd BufReadPre,FileReadPre      *.asc set bin
  autocmd BufReadPre,FileReadPre      *.asc let ch_save = &ch|set ch=2
  autocmd BufReadPost,FileReadPost    *.asc '[,']!sh -c "gpg --decrypt 2> /dev/null"
  " Switch to normal mode for editing
  autocmd BufReadPost,FileReadPost    *.asc set nobin
  autocmd BufReadPost,FileReadPost    *.asc let &ch = ch_save|unlet ch_save
  autocmd BufReadPost,FileReadPost    *.asc execute ":doautocmd BufReadPost " . expand("%:r")
  " Convert all text to encrypted text before writing
  autocmd BufWritePre,FileWritePre    *.asc   '[,']!sh -c "gpg --default-recipient-self -ae 2>/dev/null"
  " Undo the encryption so we are back in the normal text, directly
  " after the file has been written.
  autocmd BufWritePost,FileWritePost    *.asc   u
augroup END
augroup bencrypted
  au!
  " First make sure nothing is written to ~/.viminfo while editing
  " an encrypted file.
  autocmd BufReadPre,FileReadPre      *.gpg set viminfo=
  " We don't want a swap file, as it writes unencrypted data to disk
  autocmd BufReadPre,FileReadPre      *.gpg set noswapfile
  " Switch to binary mode to read the encrypted file
  autocmd BufReadPre,FileReadPre      *.gpg set bin
  autocmd BufReadPre,FileReadPre      *.gpg let ch_save = &ch|set ch=2
  autocmd BufReadPost,FileReadPost    *.gpg '[,']!sh -c "gpg --decrypt 2> /dev/null"
  " Switch to normal mode for editing
  autocmd BufReadPost,FileReadPost    *.gpg set nobin
  autocmd BufReadPost,FileReadPost    *.gpg let &ch = ch_save|unlet ch_save
  autocmd BufReadPost,FileReadPost    *.gpg execute ":doautocmd BufReadPost " . expand("%:r")
  " Convert all text to encrypted text before writing
  autocmd BufWritePre,FileWritePre    *.gpg   '[,']!sh -c "gpg --default-recipient-self --armor -ev 2>/dev/null"
  " Undo the encryption so we are back in the normal text, directly
  " after the file has been written.
  autocmd BufWritePost,FileWritePost    *.gpg   u
augroup END

" ===================================================================}}}
" ====  Plugin specific  ============================================
" ==================================================================={{{

" install vundle automatically, if not existend
if !isdirectory(expand('~').'/.vim/bundle/vundle')
  let src = 'http://github.com/gmarik/vundle.git'
  exec '!git clone '.src.' ~/.vim/bundle/vundle'
  au VimEnter * BundleInstall
endif
if isdirectory(expand('~').'/.vim/bundle/vundle')
"Vundle                                                              {{{
  filetype off " required!

  set rtp+=~/.vim/bundle/vundle/
  call vundle#rc()

  " let Vundle manage Vundle
  " required!
  Bundle 'gmarik/vundle'

  " My Bundles here:
  "general
  Bundle 'Vimball'
  Bundle 'The-NERD-Commenter'
  Bundle 'sudo.vim'
  Bundle 'Gundo'
  Bundle 'SearchComplete'
  Bundle 'ShowPairs'
  Bundle 'vimwiki'
  Bundle 'matchit.zip'
  Bundle 'AutoComplPop'
  Bundle 'git://github.com/nathanaelkane/vim-indent-guides.git'
  Bundle 'git://github.com/Lokaltog/vim-powerline.git'
  Bundle 'gmarik/snipmate.vim'
  Bundle 'honza/snipmate-snippets'
  Bundle 'https://github.com/scrooloose/syntastic'
  "manage files
  Bundle 'LustyJuggler'
  Bundle 'The-NERD-tree'
  Bundle 'mru.vim'
  "matlab
  Bundle 'git://github.com/djoshea/vim-matlab-fold.git'
  "haskell
  Bundle 'git://github.com/vim-scripts/Haskell-Conceal.git'
  Bundle 'git://github.com/Twinside/vim-hoogle.git'
  "csv
  Bundle 'csv.vim'
  "html
  Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
  "colorschemes
  Bundle 'Solarized'
  Bundle 'flazz/vim-colorschemes'
  "unsorted
  Bundle 'surround.vim'
  Bundle 'delete-surround-html'
  Bundle 'XML-Folding'
  Bundle 'git://github.com/Raimondi/delimitMate.git'
  Bundle 'vim-less'
  Bundle 'L9'
  Bundle 'tsaleh/vim-align.git'

  "testing
  "Bundle 'http://github.com/tpope/vim-fugitive'
  "Bundle 'https://github.com/jpalardy/vim-slime'
  "Bundle 'LatexParFormat'
  "Bundle 'Indent-Guides'
  "Bundle 'SuperTab'

  "frisch aussortiert
  "Bundle 'lastpos.vim'
  "Bundle 'git://github.com/vim-scripts/LaTeX-Box.git'

  " not used Bundles                                                   {{{
  "Bundle 'xoria255.vim'
  "Bundle 'neverland.vim--All-colorschemes-suck'
  "Bundle 'speeddating.vim'
  "Bundle 'snipMate'
  "Bundle 'taglist.vim'
  "Bundle 'FuzzyFinder'
  "Bundle 'Buffer-Search'
  "Bundle 'c.vim'
  "Bundle 'vimshell-ssh'
  "Bundle 'hexman.vim'
  "Bundle 'AutomaticLaTexPlugin'
  "Bundle 'SudoEdit.vim'
  "Bundle 'Tabular'
  "Bundle 'ProtoDef'
  "Bundle 'minibufexpl.vim'
  "Bundle 'unicode.vim'
  "Bundle 'unimpaired.vim'
  "Bundle 'endwise.vim'
  "Bundle 'repeat.vim'
  "Bundle 'recover.vim'
  "Bundle 'ShowMarks'
  "Bundle 'git.zip'
  "Bundle 'snipmate-snippets'
  "Bundle 'xptemplate'
  "Bundle 'VisIncr'
  "Bundle 'FSwitch'
  "Bundle 'EasyGrep'
  "Bundle 'cespare/vjde.git'
  "Bundle 'CheckAttach.vim'

  "Web
  "Bundle 'ragtag.vim'
  "Bundle 'vim-coffee-script'
  "Bundle 'rails.vim'
  "Bundle 'Haml'

  "Bundle 'mirell/vim-matchit.git'
  "Bundle 'tpope/vim-markdown.git'
  "Bundle 'Markdown-syntax'
  "Bundle 'vim-octopress'
  "Bundle 'tsaleh/vim-align.git'
  "                                                                    }}}
"                                                                    }}}
  filetype plugin indent on


  " MRU most recent files
  " :MRU

  "" ===================================================================
  "" commandT:
  ""Bundle 'git://git.wincent.com/command-t.git'
  ""cd ~/.vim/bundle/command-t/ruby/command-t
  ""ruby extconf.rb
  ""make
  "let g:CommandTAcceptSelectionMap = '<C-t>'
  "let g:CommandTAcceptSelectionTabMap = '<CR>'

  " ===================================================================
  " nerdTree
  map <silent> <C-N> :NERDTree<CR>
  " start NERDTree at startup
  " autocmd VimEnter * NERDTree

  " ===================================================================
  " LatexBox
  " vim --servername SOMETHING file.tex
  "let g:LatexBox_viewer = 'zathura'
  "let g:LatexBox_latexmk_options = '-pvc'
  " \ll   run latexmk
  " \lv   run pdf viewer

  " ===================================================================
  " AutoComplPop: acp
  " http://www.vim.org/scripts/script.php?script_id=1880

  " ===================================================================
  " ConqueTerm
  " map <C-B> :ConqueTerm bash

  " ===================================================================
  " minibufferexplorer
  " let g:miniBufExplMapWindowNavVim = 1
  " let g:miniBufExplMapWindowNavArrows = 1
  " let g:miniBufExplMapCTabSwitchBufs = 1
  " let g:miniBufExplModSelTarget = 1

  " ===================================================================
  "Gundo
  nnoremap <F6> :GundoToggle<CR>

  " ===================================================================
  " Vim-slime
 " let g:slime_target = "tmux"
 " let g:slime_paste_file = tempname()

endif
"                                                                    }}}

" vim:set ts=2 sw=2 sts=2 et fenc=utf-8 ff=unix foldmethod=marker foldmarker={{{,}}}:
