" ~/.vimrc
"
" `call MyInstallAllPlugins()` for installing all plugins
"
" Worth reading:
"   Steve Losh: Learn Vimscript the Hard Way
"
" Written by Maximilian-Huber.de
"
" Last Modified: Mon Aug 25, 2014  05:22

" auto reload when saving
if has("autocmd")
  augroup autoSourceVimrc
    autocmd!
    autocmd bufwritepost .vimrc source %
  augroup END
endif

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
    autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
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
set autowrite " Automatically save before commands like :next and :make
set visualbell " ausgehebelt durch noerrorbells ?
set noerrorbells
set hidden
set autoread
set magic      " For regular expressions turn magic on
set splitbelow " set splitright
set autochdir

set cpoptions+=n
set showbreak=\ \ \ ↳

" ====  wildmenu  ==================================================={{{

set wildmenu "Kommando Zeilen Vervollständigung
"set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png
set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png,*.aux,*.bbl,*.blg,*.fdb_latexmk,*.fls,*.idx,*.ilg,*.ind,*.nlo,*.toc,*.hi
set wildmode=list:longest

" Suffixes that get lower priority when doing tab completion for filenames.
" These are files we are not likely to want to edit or read.
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
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
" ====  spelling  ===================================================
" ==================================================================={{{
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

" DeleteTrailing and more
function! Cleanup()
  call DeleteTrailing()
  setlocal ff=unix
  setlocal expandtab
  retab!
endfunction
"                                                                    }}}

function! MyHtmlEscape()
  silent s/ö/\&ouml;/eg
  silent s/ä/\&auml;/eg
  silent s/ü/\&uuml;/eg
  silent s/Ö/\&Ouml;/eg
  silent s/Ä/\&Auml;/eg
  silent s/Ü/\&Uuml;/eg
  silent s/ß/\&szlig;/eg
endfunction

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
nnoremap <Leader>dt :call DeleteTrailing()
nnoremap <leader>T :set expandtab<cr>:retab!<cr>
nnoremap <Leader>dT :call Cleanup()

" Search and replace
nnoremap <Leader>s :%s/\<<C-r><C-w>\>//g<Left><Left>
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

"                                                                    }}}
" ====  for external scripts  ======================================={{{

"Markdown to HTML
nnoremap <leader>md :%!~/bin/Markdown.pl --html4tags <cr>

"                                                                    }}}
" ====  testing  ===================================================={{{
nnoremap <leader>f :call FoldColumnToggle()<cr>
function! FoldColumnToggle()
  if &foldcolumn
    setlocal foldcolumn=0
  else
    setlocal foldcolumn=4
  endif
endfunction
"                                                                    }}}
" ===================================================================}}}
" ====  abbreviations  ==============================================
" ==================================================================={{{
iabbrev adn and
iabbrev @@ mail@maximilian-huber.de
iabbrev VGr Viele Grüße<cr>Maximilian

" overwrite those annoying commands
cnoreabbrev <expr> W ((getcmdtype() is# ':' && getcmdline() is# 'W')?('w'):('W'))
cnoreabbrev <expr> Q ((getcmdtype() is# ':' && getcmdline() is# 'Q')?('q'):('Q'))
" ===================================================================}}}
" ====  Git / SVN   =================================================
" ==================================================================={{{
command! GCommit   call myVersionControl#GitCommit()
command! GPush     call myVersionControl#GitPush()
command! GPull     call myVersionControl#GitPull()
command! GAdd      call myVersionControl#GitAdd()
command! GStatus   call myVersionControl#GitStatus()
command! GCheckout call myVersionControl#GitCheckout()
command! GBranch   call myVersionControl#GitBranch()
command! GAuto     call myVersionControl#GitAuto()

nnoremap <silent> _gc :call myVersionControl#GitCommit()<cr>

command! SVNCommit call myVersionControl#SVNCommit()
command! SVNUpdate call myVersionControl#SVNUpdate()
command! SVNAdd    call myVersionControl#SVNAdd()
" ===================================================================}}}
" ====  filetype specific  ==========================================
" ==================================================================={{{
if has("autocmd")
  augroup vimrc_autocmds
    autocmd!
    autocmd FileType human   setlocal wrap tw=79 linebreak
    autocmd FileType text    setlocal wrap tw=79 linebreak
    autocmd FileType txt     setlocal wrap tw=79 linebreak
    autocmd FileType php     setlocal sw=2 ts=2 et
    " in makefiles, don't expand tabs to spaces
    autocmd FileType make setlocal noexpandtab shiftwidth=8
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

function! MyInstallAllPlugins()
  " install vundle, if not existend
  if !isdirectory(expand('~').'/.vim/bundle/vundle')
    let src = 'http://github.com/gmarik/vundle.git'
    exec '!git clone '.src.' ~/.vim/bundle/vundle'
    au VimEnter * BundleInstall
  endif
endfunction

if isdirectory(expand('~').'/.vim/bundle/vundle')
  filetype off " required!

  set rtp+=~/.vim/bundle/vundle/
  call vundle#rc()

  " let Vundle manage Vundle
  " required!
  Bundle 'gmarik/vundle'

  "############################################################################
  " My Bundles here:
  "############################################################################
  "general
  Bundle 'Vimball'
  Bundle 'The-NERD-Commenter'
  Bundle 'sudo.vim'
  "if 1
    Bundle 'Gundo'
    nnoremap <F6> :GundoToggle<CR>
  "endif
  Bundle 'SearchComplete'
  Bundle 'ShowPairs'
  Bundle 'vimwiki'
  " move by % to matching bracket/tag/...
  Bundle 'matchit.zip'
  Bundle 'git://github.com/Raimondi/delimitMate.git'
  "Bundle 'git://github.com/nathanaelkane/vim-indent-guides.git'
  Bundle 'gmarik/snipmate.vim'
  Bundle 'honza/snipmate-snippets'
  if 1
    Bundle 'https://github.com/scrooloose/syntastic'
    let g:syntastic_scala_checkers = []
    noremap <Leader>S :SyntasticToggleMode<CR>
    "let g:syntastic_haskell_checkers = ["hlint"]
  endif
  Bundle 'git://github.com/Lokaltog/vim-powerline.git'

  "############################################################################
  "manage files
  Bundle 'LustyJuggler'
  "if 0
  "Bundle 'The-NERD-tree'
    "noremap <silent> <C-N> :NERDTree<CR>
    "" start NERDTree at startup
    "" autocmd VimEnter * NERDTree
    "" open a NERDTree automatically when vim starts up if no files were specified
    "autocmd vimenter * if !argc() | NERDTree | endif
  "endif
  Bundle 'mru.vim'
  "if 1 "CtrlP
    Bundle 'https://github.com/kien/ctrlp.vim'
    let g:ctrlp_map = '<c-p>'
    let g:ctrlp_cmd = 'CtrlP'
    let g:ctrlp_working_path_mode = 'ra'
    nnoremap <Leader>b :CtrlPBuffer<CR>
  "endif
  "if 0
    "Bundle 'minibufexpl.vim'
    "let g:miniBufExplMapWindowNavVim = 1
    "let g:miniBufExplMapWindowNavArrows = 1
    "let g:miniBufExplMapCTabSwitchBufs = 1
    "let g:miniBufExplModSelTarget = 1
  "endif
  "############################################################################
  "matlab
  "Bundle 'git://github.com/djoshea/vim-matlab-fold.git'

  "############################################################################
  "haskell
  Bundle 'git://github.com/vim-scripts/Haskell-Conceal.git'
  Bundle 'git://github.com/Twinside/vim-hoogle.git'

  "############################################################################
  "csv
  Bundle 'csv.vim'

  "############################################################################
  "html
  Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
  "Bundle 'vim-less'

  "############################################################################
  "arduino
  Bundle "sudar/vim-arduino-snippets"
  Bundle "sudar/vim-arduino-syntax"

  "############################################################################
  "colorschemes
  Bundle 'Solarized'
  "Bundle 'flazz/vim-colorschemes'
  Bundle 'CSApprox'
  "let g:CSApprox_hook_post = ['hi Normal ctermbg=none',
                              "\ 'hi NonText ctermbg=none',
                              "\ 'hi Conceal ctermbg=none']

  "############################################################################
  "completion
  if 1
    Bundle 'Shougo/neocomplcache'
    " === neocomplcache setup ==========================================={{{
    " Disable AutoComplPop.
    let g:acp_enableAtStartup = 0
    " Use neocomplcache.
    let g:neocomplcache_enable_at_startup = 1
    " Use smartcase.
    let g:neocomplcache_enable_smart_case = 1
    " Set minimum syntax keyword length.
    let g:neocomplcache_min_syntax_length = 3
    let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'
    let g:neocomplcache_snippets_dir='~/.vim/bundle/vim-snippets/snippets'

    " Enable heavy features.
    " Use camel case completion.
    "let g:neocomplcache_enable_camel_case_completion = 1
    " Use underbar completion.
    "let g:neocomplcache_enable_underbar_completion = 1

    " Define dictionary.
    let g:neocomplcache_dictionary_filetype_lists = {
        \ 'default' : '',
        \ 'vimshell' : $HOME.'/.vimshell_hist',
        \ 'scheme' : $HOME.'/.gosh_completions'
            \ }

    " Define keyword.
    if !exists('g:neocomplcache_keyword_patterns')
        let g:neocomplcache_keyword_patterns = {}
    endif
    let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

    " Plugin key-mappings.
    "inoremap <expr><C-g>     neocomplcache#undo_completion()
    "inoremap <expr><C-l>     neocomplcache#complete_common_string()

    " Recommended key-mappings.
    " <CR>: close popup and save indent.
    inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
    function! s:my_cr_function()
      return neocomplcache#smart_close_popup() . "\<CR>"
      " For no inserting <CR> key.
      "return pumvisible() ? neocomplcache#close_popup() : "\<CR>"
    endfunction
    " <TAB>: completion.
    inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
    " <C-h>, <BS>: close popup and delete backword char.
    "inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
    "inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
    "inoremap <expr><C-y>  neocomplcache#close_popup()
    "inoremap <expr><C-e>  neocomplcache#cancel_popup()


    " Enable snipMate compatibility feature.
    let g:neosnippet#enable_snipmate_compatibility = 1

    " Tell Neosnippet about the other snippets
    let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'

    " SuperTab like snippets behavior.
    imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
      \ "\<Plug>(neosnippet_expand_or_jump)"
      \: pumvisible() ? "\<C-n>" : "\<TAB>"
    smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
      \ "\<Plug>(neosnippet_expand_or_jump)"
      \: "\<TAB>"

    " For snippet_complete marker.
    if has('conceal')
      "set conceallevel=2 concealcursor=i
    endif
    " ===================================================================}}}
  else
    Bundle 'AutoComplPop'
    " === AutoComplPop setup ============================================{{{
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
    " ===================================================================}}}
  endif

  "############################################################################
  "unsorted
  Bundle 'surround.vim'
  Bundle 'delete-surround-html'
  Bundle 'XML-Folding'
  Bundle 'tsaleh/vim-align.git'
  Bundle 'L9'
  if 1
    Bundle 'https://github.com/sjl/clam.vim/'
    nnoremap ! :Clam<space>
    vnoremap ! :ClamVisual<space>
  endif

  filetype plugin indent on " required!
  " not used Bundles                                                   {{{
  "testing
  "Bundle 'http://github.com/tpope/vim-fugitive'
  "Bundle 'https://github.com/jpalardy/vim-slime'
  "Bundle 'LatexParFormat'
  "Bundle 'SuperTab'
  "Bundle 'git://github.com/scala/scala-dist/tree/master/tool-support/src/vim.git'

  "frisch aussortiert
  "Bundle 'lastpos.vim'
  "Bundle 'git://github.com/vim-scripts/LaTeX-Box.git'
  "Bundle 'git://github.com/tclem/vim-arduino.git'

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
  "                                                                  }}}

endif
"                                                                    }}}

" vim:set ts=2 sw=2 sts=2 et fenc=utf-8 ff=unix foldmethod=marker foldmarker={{{,}}}:
