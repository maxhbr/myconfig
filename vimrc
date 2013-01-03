" ~/.vimrc
"
" Written by Maximilian-Huber.de
"
" Last modified: Do Jan 03, 2013  02:14
"
" this config will automatically download Vundle from git, and then it will
" install all plugins

"tipps / Keybindings                                                 {{{
" write as root: :w !sudo tee % > /dev/null
"
":r! date
"
" Vim folding commands
"zf#j creates a fold from the cursor down # lines.
"zf/string creates a fold from the cursor to string .
"zj moves the cursor to the next fold.
"zk moves the cursor to the previous fold.
"zo opens a fold at the cursor.
"zO opens all folds at the cursor.
"zm increases the foldlevel by one.
"zM closes all open folds.
"zr decreases the foldlevel by one.
"zR decreases the foldlevel to zero -- all folds will be open.
"zd deletes the fold at the cursor.
"zE deletes all folds.
"[z move to start of open fold.
"]z move to end of open fold.
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
"                                                                    }}}

" auto reload when saving
autocmd! bufwritepost .vimrc source %

" This must be first, because it changes other options as a side effect.
set nocompatible
runtime! archlinux.vim

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
endif

" Use Unix as the standard file type
set ffs=unix,dos,mac
set encoding=utf8

set virtualedit=all
set backspace=2
" set backspace=indent,eol,start
set showcmd         " Show (partial) command in status line.
set showmatch       " Show matching brackets.
set ignorecase      " Do case insensitive matching
set autowrite       " Automatically save before commands like :next and :make
if has("mouse")
  set mouse=a         " Enable mouse usage (all modes) alternativ nvc
  set mousehide
endif
set visualbell
set noerrorbells
set hidden
set autoread
set magic " For regular expressions turn magic on
set splitbelow
set splitright
set autochdir

set cpoptions+=n
set showbreak=\ \ \ ↳

set wildmenu "Kommando Zeilen Vervollständigung
set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png
set wildmode=list:longest

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
" non-case-sensitive searches
set smartcase
"                                                                    }}}
" ====  Folding / indenting  ========================================{{{

set foldmethod=marker

" indending
set autoindent
set smartindent
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab
set smarttab

set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.
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
set textwidth=0                " Don't wrap lines by default
set nowrap
set ruler
set number

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
  set guioptions-=T  " no toolbar
  " Use console messages instead of GUI dialogs
  set guioptions+=c

  " show focus lost"
  " replaced by powerline
  "au FocusLost * : hi StatusLine gui=undercurl
  "au FocusGained * : hi StatusLine gui=bold,reverse

  let g:indent_guides_auto_colors = 0
  "autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=darkgrey   ctermbg=3
  autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=black ctermbg=4
else
  " no gui
  set t_Co=256
  set background=dark

  " replaced by powerline
  "hi IndentGuidesEven ctermbg=darkgrey
  "hi IndentGuidesOdd  ctermbg=black

  "if &term =~ "xterm"
  "elseif &term =~ "urxvt"
  "elseif &term =~ "urxvtc"
  "endif
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
" ====  Status hilighting  =========================================={{{
" replaced by powerline
"function! InsertStatuslineColor(mode)
  "if a:mode == 'i'
    ""hi statusline guibg=red
    "hi StatusLine term=reverse ctermbg=5 gui=undercurl guisp=red guibg=red
  "elseif a:mode == 'r'
    ""hi statusline guibg=magenta
    "hi StatusLine term=reverse ctermbg=5 gui=undercurl guisp=magenta guibg=magenta
  "else
    ""hi statusline guibg=blue
    "hi StatusLine term=reverse ctermbg=5 gui=undercurl guisp=blue guibg=blue
  "endif
"endfunction
"au InsertEnter * call InsertStatuslineColor(v:insertmode)
"au InsertLeave * hi StatusLine term=reverse ctermfg=0 ctermbg=2 gui=bold,reverse
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

" ===================================================================}}}
" ====  Keymappings  ================================================
" ==================================================================={{{

" overwrite those annoying commands
cnoreabbrev <expr> W ((getcmdtype() is# ':' && getcmdline() is# 'W')?('w'):('W'))
cnoreabbrev <expr> Q ((getcmdtype() is# ':' && getcmdline() is# 'Q')?('q'):('Q'))

nnoremap ; :

let mapleader=","

" quicksave
noremap <Leader>s :update<CR>

nmap <Leader>r :source $MYVIMRC

nmap <silent> <leader>ev :tabedit $MYVIMRC<CR>
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

noremap <silent> <F10> :q!<CR>
inoremap <silent> <F10> <Esc>:q!<CR>

"Markdown to HTML
nmap <leader>md :%!~/bin/Markdown.pl --html4tags <cr>

"easyer increment/decrement
nnoremap + <C-a>
nnoremap - <C-x>

"Open last/alternate buffer
noremap <Leader><Leader> <C-^>

" ====  Movement  ===================================================

" jump to next row instead of next line
nnoremap j gj
nnoremap k gk

nnoremap <c-j> 5j
nnoremap <c-k> 5k

" Easy window navigation
"map <C-h> <C-w>h
"map <C-j> <C-w>j
"map <C-k> <C-w>k
"map <C-l> <C-w:vnew \| CommandT>l

" control-left & right arrows switch between tabs
map <c-Left> :tabp<CR>
map <c-Right> :tabn<CR>

inoremap <expr> <Tab>     pumvisible() ? "\<C-y>" : "\<Tab>"
inoremap <expr> <CR>      pumvisible() ? "\<C-e><CR>" : "\<CR>"

" force vim keys
"map <up> <nop>
"map <down> <nop>
"map <left> <nop>
"map <right> <nop>
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>

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
  set foldlevel=7

  "invisible chars
  setlocal list listchars=tab:>~,trail:·
  " autocmd BufWrite *.py :call DeleteTrailing()

  map <F5> :w<CR>:!python "%"<CR>
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
  " mails / text

  set wrap
  set textwidth=79

  set linebreak

endfunction

function! SetLaTeXFile()

  set wrap
  set sw=2
  set ts=2
  set et
  set textwidth=79
  set cc=80

  set linebreak

  set foldmarker=%{{{,%}}}
  "save folding
  "au BufWinLeave * mkview
  "au BufWinEnter * silent loadview

  "if has("gui_running")
    "set spell
    "set spelllang=de,en
    "setlocal spellsuggest=9
  "endif
endfunction

function! SetCssFile()
  set sw=4
  set ts=4
  set et
  set tw=79
  set linebreak
  if exists('+colorcolumn')
    set colorcolumn=80
  endif
endfunction

function! SetMatlabFile()
  set sw=4
  set ts=4
  set et
  set tw=79
  set linebreak

  set foldmarker={{{,}}}
  if exists('+colorcolumn')
    set colorcolumn=80
  endif
endfunction

"augroup filetypedetect
  "au! BufRead,BufNewFile *.m,*.oct setfiletype matlab
"augroup END

augroup vimrc_autocmds
  au!
  autocmd FileType python call SetPythonFile()
  autocmd FileType java call SetJavaFile()
  autocmd FileType text call SetTextFile()
  autocmd FileType tex call SetLaTeXFile()
  autocmd FileType human call SetTextFile()
  autocmd FileType txt call SetTextFile()
  "autocmd FileType php call SetPHPFile()
  "autocmd FileType haskell call SetHaskellFile()
  autocmd FileType css call SetCssFile()
  autocmd FileType less call SetCssFile()
  autocmd FileType sh set sw=2 ts=2 et
  " in makefiles, don't expand tabs to spaces
  autocmd FileType matlab call SetMatlabFile()
  autocmd FileType make set noexpandtab shiftwidth=8
  autocmd FileType log setlocal autoread
augroup END

augroup Shebang
  autocmd BufNewFile *.py 0put =\"#!/usr/bin/env python\<nl># -*- coding: iso-8859-15 -*-\<nl>\"|$
  autocmd BufNewFile *.rb 0put =\"#!/usr/bin/env ruby\<nl># -*- coding: None -*-\<nl>\"|$
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
  Bundle 'Vimball'
  Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
  Bundle 'git://git.wincent.com/command-t.git'
  Bundle 'git://github.com/vim-scripts/LaTeX-Box.git'
  Bundle 'SearchComplete'
  Bundle 'ShowPairs'
  Bundle 'Gundo'
  Bundle 'lastpos.vim'
  Bundle 'sudo.vim'
  Bundle 'The-NERD-Commenter'
  Bundle 'The-NERD-tree'
  Bundle 'csv.vim'
  Bundle 'surround.vim'
  Bundle 'delete-surround-html'
  Bundle 'gmarik/snipmate.vim'
  Bundle 'honza/snipmate-snippets'
  Bundle 'mru.vim'
  Bundle 'XML-Folding'
  Bundle 'matchit.zip'
  Bundle 'git://github.com/Raimondi/delimitMate.git'
  Bundle 'LustyJuggler'
  Bundle 'vim-less'
  Bundle 'L9'
  Bundle 'AutoComplPop'
  Bundle 'vimwiki'
  Bundle 'git://github.com/nathanaelkane/vim-indent-guides.git'
  Bundle 'git://github.com/Lokaltog/vim-powerline.git'
  Bundle 'git://github.com/djoshea/vim-matlab-fold.git'
  Bundle 'tsaleh/vim-align.git'
  Bundle 'Solarized'

  "testing
  "Bundle 'Indent-Guides'
  "Bundle 'SuperTab'

  " not used Bundles                                                   {{{
  "Bundle 'xoria256.vim'
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
  "Bundle 'fugitive.vim'
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

  " ===================================================================
  " commandT:
  " open file in new tabs
  let g:CommandTAcceptSelectionMap = '<C-t>'
  let g:CommandTAcceptSelectionTabMap = '<CR>'

  " ===================================================================
  " nerdTree
  map <silent> <C-N> :NERDTree<CR>
  " start NERDTree at startup
  " autocmd VimEnter * NERDTree

  " ===================================================================
  " LatexBox
  " vim --servername SOMETHING file.tex
  let g:LatexBox_viewer = 'zathura'
  let g:LatexBox_latexmk_options = '-pvc'
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

endif
"                                                                    }}}

" ==== NEO keyboard layout  =========================================
"no s h
"no h s
"no n j
"no j n
"no r k
"no k r
"no t l
"no l k

" vim:set ts=2 sw=2 sts=2 et fenc=utf-8 ff=unix foldmethod=marker foldmarker={{{,}}}:
