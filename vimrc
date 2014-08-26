" ~/.vimrc
"
" Written by Maximilian-Huber.de
"
" Worth reading:
"   Steve Losh: Learn Vimscript the Hard Way
"
" Last Modified: Tue Aug 26, 2014  08:23

" auto reload vimrc when saved ======================================{{{
if has("autocmd")
  augroup autoSourceVimrc
    autocmd!
    autocmd bufwritepost .vimrc source %
  augroup END
endif "==============================================================}}}

" This must be first, because it changes other options as a side effect.
set nocompatible

" ===================================================================
" ====  general  ====================================================
" ==================================================================={{{

if has("syntax")
  syntax on
endif

if has("autocmd")
  filetype plugin on
  filetype indent on

  " completion
  set ofu=syntaxcomplete#Complete
  set completeopt=longest,menuone

  augroup someAugroup
    autocmd!
    " jump to the last position when reopening a file
    autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") 
                          \ | exe "normal! g'\"" 
                        \ | endif
    " resize splits
    autocmd VimResized * exe "normal! \<c-w>="
    " leave paste mode, when exit insert mode
    autocmd InsertLeave * set nopaste
  augroup END
endif

" Use Unix as the standard file type
set ffs=unix,dos,mac
set encoding=utf8

set virtualedit=all
set showcmd   " Show (partial) command in status line.
set showmatch " Show matching brackets.
"set autowrite " Automatically save before commands like :next and :make
set noautowrite " don't automagically write on :next"
set visualbell " ausgehebelt durch noerrorbells ?
set noerrorbells
set hidden
set autoread
set magic      " For regular expressions turn magic on
set splitbelow " set splitright
set autochdir

set restorescreen=on 

set cpoptions+=n
set showbreak=\ \ \ ↳

" ====  wildmenu  ==================================================={{{
set complete=.,w,b,u,U,t,i,d    " do lots of scanning on tab completion"

set wildmenu "Kommando Zeilen Vervollständigung
"set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png
set wildignore=*.dll,*.o,*.obj,*.exe,*.hi,*.pyc,
  \*.bak,*.bac,
  \*.jpg,*.gif,*.png,
  \*.aux,*.bbl,*.blg,*.fdb_latexmk,*.fls,*.idx,*.ilg,*.ind,*.nlo,*.toc
set wildmode=list:longest

" Suffixes that get lower priority when doing tab completion for filenames.
" These are files we are not likely to want to edit or read.
set suffixes=.bak,.bac,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,
  \.idx,.ilg,.inx,.out,.toc
"                                                                    }}}
" ====  sessioning  ================================================={{{

set ssop-=options    " do not store global and local values in a session
set ssop-=folds      " do not store folds
"                                                                    }}}
" ====  scrolling  =================================================={{{

set scrolljump=5
set scrolloff=5         "Start scrolling when we're x lines away from margins
set sidescrolloff=10
set sidescroll=1
"                                                                    }}}
" ====  search  ====================================================={{{

" highlight searches, searches begin immediately
set hlsearch
set incsearch
set smartcase " case sensitiv, if a uppercase letter is contained
set ignorecase
"                                                                    }}}
" ====  folding / indenting  ========================================{{{

"default folding
set foldmethod=marker
set foldmarker={{{,}}}

" indending
set autoindent
set smartindent
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab
set smarttab
"                                                                    }}}
" ====  list  ======================================================={{{

set list
set listchars=tab:>.,trail:…,extends:#,nbsp:. " …°⎼
set fillchars=vert:┃,diff:⎼,fold:⎼
"                                                                    }}}
" ====  performance  ================================================{{{

set ttyfast

" for powersave, stops the blinking cursor
let &guicursor = &guicursor . ",a:blinkon0"
" Speed hack
hi NonText cterm=NONE ctermfg=NONE
" stops slow responding in large files
set synmaxcol=128
"                                                                    }}}
" ====  line numberig  ============================================={{{
" TODO: has problems?
"augroup autoChangeNumbering
"  autocmd!
"  autocmd InsertEnter * set nu
"  autocmd InsertLeave * set rnu
""  "au FocusLost * set nu
""  "au FocusGained * set rnu
"augroup END
set nu
"                                                                    }}}
" ====  backup / undo  =============================================={{{

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
" ====  spelling  ==================================================={{{
setlocal nospell
set spelllang=de_de,en_us
highlight clear SpellBad
highlight SpellBad term=standout ctermfg=1
highlight SpellBad term=underline cterm=underline
highlight clear SpellCap
highlight SpellCap term=underline cterm=underline
highlight clear SpellRare
highlight SpellRare term=underline cterm=underline
highlight clear SpellLocal
highlight SpellLocal term=underline cterm=underline

"                                                                    }}}
" ===================================================================}}}
" ====  appearance  =================================================
" ==================================================================={{{

set title
set cursorline
set nocursorcolumn " hat probleme mit acp (Popup)
set textwidth=0    " Don't wrap lines by default
set nowrap
set ruler

set showmode "show when you are in insert mode

" statusline
set laststatus=2

set statusline=%<%f\          " custom statusline
set stl+=[%{&ff}]             " show fileformat
set stl+=%y%m%r%=
set stl+=%-14.(%l,%c%V%)\ %P

if has("gui_running")
  set guioptions-=T  " no toolbar
  set guioptions+=c " Use console messages instead of GUI dialogs
else
  "set t_Co=256
  set background=dark
endif

" ====  choose colorscheme  =========================================
colorscheme mycolorscheme
"if filereadable(expand("$VIMRUNTIME/colors/mustang.vim"))
  "colorscheme mustang
"elseif filereadable(expand("$HOME/.vim/colors/mustang.vim"))
  "colorscheme mustang
"endif

"function! ToggleColorscheme()
  "if (g:colors_name ==? "mustang")
    "colorscheme solarized
  "else
    "colorscheme mustang
  "endif
"endfunction
"noremap <F12> :call ToggleColorscheme()<CR>

" ====  hilight to long lines  ======================================
if exists('+colorcolumn')
    set colorcolumn=80
    highlight ColorColumn ctermbg=233 guibg=#592929
else
    highlight OverLength ctermbg=red ctermfg=white guibg=#592929
    match OverLength /\%81v.\+/
endif

" ===================================================================}}}
" ====  functions  ==================================================
" ==================================================================={{{

" ====  for cleanup  ================================================{{{
" delete all trails
" use :call DeleteTrailing
" or <Leader>dt
function! DeleteTrailing()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunction
nnoremap <Leader>dt :call DeleteTrailing()

" DeleteTrailing and more
function! Cleanup()
  call DeleteTrailing()
  setlocal ff=unix
  setlocal expandtab
  retab!
endfunction
nnoremap <Leader>dT :call Cleanup()

"                                                                    }}}
" ====  simpel Html escapeing  ======================================{{{
function! MyHtmlEscape()
  silent s/ö/\&ouml;/eg
  silent s/ä/\&auml;/eg
  silent s/ü/\&uuml;/eg
  silent s/Ö/\&Ouml;/eg
  silent s/Ä/\&Auml;/eg
  silent s/Ü/\&Uuml;/eg
  silent s/ß/\&szlig;/eg
endfunction

"                                                                    }}}
" ====  open files via ranger  ======================================{{{
function! RangerChooser()
  exec "silent !ranger --choosefile=/tmp/chosenfile " . expand("%:p:h")
  if filereadable('/tmp/chosenfile')
    exec 'edit ' . system('cat /tmp/chosenfile')
    call system('rm /tmp/chosenfile')
  endif
  redraw!
endfunction
noremap ,R :call RangerChooser()<CR>

"                                                                    }}}
" ===================================================================}}}
" ====  keymappings / input  ========================================
" ==================================================================={{{
if has("mouse")
  set mouse=a " Enable mouse usage (all modes) alternativ nvc
  set mousehide
endif
set backspace=2
" set backspace=indent,eol,start

nnoremap ; :
let mapleader=","
let maplocalleader = "\\"

" ====  general  ===================================================={{{
"save without sudo
cnoremap w!! w !sudo tee % >/dev/null

"Make Y behave like other capitals
noremap Y y$

"Reload vimrc
nnoremap <Leader>r :source $MYVIMRC

"nnoremap <silent> <leader>ev :tabedit $MYVIMRC<CR>
nnoremap <silent> <leader>ev :e $MYVIMRC<CR>
"nnoremap <silent> <leader>sv :so $MYVIMRC<CR>

" Pressing ,ss will toggle and untoggle spell checking
"noremap <leader>ss :setlocal spell!<cr>

" Make < > shifts keep selection
vnoremap < <gv
vnoremap > >gv

" clear hilighting
nnoremap <silent> <leader>/ :nohlsearch<CR>

set pastetoggle=<F11>

" easyer increment/decrement
nnoremap + <C-a>
nnoremap - <C-x>

"                                                                    }}}
" ====  maps to nop ================================================{{{
nnoremap K <nop>

inoremap <F1> <nop>
nnoremap <F1> <nop>
vnoremap <F1> <nop>

inoremap <F2> <nop>
inoremap <F3> <nop>
inoremap <F4> <nop>

inoremap <F6> <nop>
inoremap <F7> <nop>
inoremap <F8> <nop>
inoremap <F9> <nop>

inoremap <F12> <nop>

" remove warning
nnoremap <c-c> <nop>

"                                                                    }}}
" ====  formatting  ================================================={{{
" Use Q for formatting the current paragraph (or selection)
vnoremap <leader>Q gq
nnoremap <leader>Q gqap

"for cleaning
nnoremap <leader>T :set expandtab<cr>:retab!<cr>

" Search and replace
nnoremap <Leader>s :%s/\<<C-r><C-w>\>//g<Left><Left>
" Search and change
nnoremap _s :%s/\<<C-r><C-w>\>/<C-r><C-w>/g<Left><Left>

" Word to UPPERCASE
inoremap <c-u> <esc>viwUi
nnoremap <c-u> viwUw

" souround with quotes
nnoremap <leader>" viw<esc>a"<esc>hbi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>hbi'<esc>lel

"                                                                    }}}
" ====  buffer  ====================================================={{{
"Open last/alternate buffer
noremap <Leader><Leader> <C-^>

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

" for fast quitting
noremap <silent> <F10> :q!<CR>
inoremap <silent> <F10> <Esc>:q!<CR>

"                                                                    }}}
" ====  movement  ==================================================={{{
" go back to last cursor position
nnoremap <leader>- ``

" jump to next row instead of next line
nnoremap j gj
nnoremap k gk

nnoremap <c-j> 5j
nnoremap <c-k> 5k

" control-left & right arrows switch between tabs
noremap <c-Left> :tabp<CR>
noremap <c-Right> :tabn<CR>

"                                                                    }}}
" ====  for external scripts  ======================================={{{

"Markdown to HTML
nnoremap <leader>md :%!~/bin/Markdown.pl --html4tags <cr>

"                                                                    }}}
" ====  testing  ===================================================={{{
function! FoldColumnToggle()
  if &foldcolumn
    setlocal foldcolumn=0
  else
    setlocal foldcolumn=4
  endif
endfunction
nnoremap <leader>f :call FoldColumnToggle()<cr>

"                                                                    }}}
" ===================================================================}}}
" ====  abbreviations  ==============================================
" ==================================================================={{{
" overwrite those annoying commands
cnoreabbrev <expr> W ((getcmdtype() is# ':' && getcmdline() is# 'W')?('w'):('W'))
cnoreabbrev <expr> Q ((getcmdtype() is# ':' && getcmdline() is# 'Q')?('q'):('Q'))

" handy abbreviations
iabbrev @@ mail@maximilian-huber.de
iabbrev VGr Viele Grüße<cr>Maximilian

" correct some typos
iabbrev adn and

" ===================================================================}}}
" ====  filetype specific  ==========================================
" ==================================================================={{{
" most filetype specific is coantained in the files under ftplugin/

if has("autocmd")
  augroup vimrc_autocmds
    autocmd!
    autocmd FileType human setlocal wrap tw=79 linebreak
    autocmd FileType text  setlocal wrap tw=79 linebreak
    autocmd FileType txt   setlocal wrap tw=79 linebreak
    " in makefiles, don't expand tabs to spaces
    autocmd FileType make  setlocal noexpandtab shiftwidth=8
  augroup END

  augroup Shebang
    autocmd!
    autocmd BufNewFile *.sh 0put =\"#!/bin/sh\"|$
    autocmd BufNewFile *.py 0put =\"#!/usr/bin/env python\<nl># -*- coding: iso-8859-15 -*-\<nl>\"|$
    autocmd BufNewFile *.rb 0put =\"#!/usr/bin/env ruby\<nl># -*- coding: None -*-\<nl>\"|$
  augroup END

  " Transparent editing of gpg encrypted files.                         {{{
  " Placed Public Domain by Wouter Hanegraaff <wouter@blub.net>
  " (asc support and sh -c"..." added by Osamu Aoki)
  augroup aencrypted
    autocmd!
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
    autocmd!
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
endif
"                                                                    }}}
" ===================================================================}}}
" ====  plugin specific  ============================================
" ==================================================================={{{
" $ ln -s $MYCONFIGDIR/vim/bundle ~/.vim/bundle

execute pathogen#infect()

" Used Plugins:
"   General:
"   * The-NERD-Commenter
"   * Gundo
nnoremap <F6> :GundoToggle<CR>
"   * vimwiki
"   * matchit
"   * delimitMate
"   * snipmate                                              <-- TODO
"   * synctastic
let g:syntastic_scala_checkers = []
noremap <Leader>S :SyntasticToggleMode<CR>
"let g:syntastic_haskell_checkers = ["hlint"]
"   * vim-powerline
"   Manage Files:
"   * CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
nnoremap <Leader>b :CtrlPBuffer<CR>
"   * (LustJuggle, MRU)
" ===================================================================
"   Haskell:
"   * Haskell-Concal
"   Clojure:
"   * vim-fireplace
"   CSV:
"   * csv.vim
"   HTML:
"   * sparkup
"   Arduino:
"   * vim-arduino-syntax
" ===================================================================
"   Completion:
"   * ultisnips + honza/vim-snippets
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-b>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
"let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
"   * YouCompleteMe
"       needs:
"       $ ./install.sh --clang-completer
let g:ycm_use_ultisnips_completer = 1
let g:ycm_min_num_of_chars_for_completion = 2
let g:ycm_filetype_blacklist = {
      \ 'tagbar' : 1,
      \ 'qf' : 1,
      \ 'notes' : 1,
      \ 'unite' : 1,
      \ 'pandoc' : 1,
      \ 'infolog' : 1,
      \}
" ===================================================================
"   Others:
"   * Clam
nnoremap ! :Clam<space>
vnorema ! :ClamVisual<space>
" ===================================================================}}}

" vim:set ts=2 sw=2 sts=2 et fenc=utf-8 ff=unix foldmethod=marker foldmarker={{{,}}}:
