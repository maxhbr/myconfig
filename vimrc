" ~/.vimrc
"
" The rest of my vim config can be foundat github:
"     https://github.com/maximilianhuber/myconfig
"
" Try it out:
"     simply call the script `tasteMyVimrc.sh` in my config repo. It will
"     generate a folder `/tmp/vimtest/` and install my vimrc completely in this
"     folder
"
" Written by Maximilian-Huber.de
"
" Worth reading:
"   Steve Losh: Learn Vimscript the Hard Way
"
" Last Modified: Tue Sep 16, 2014  06:41


" ===================================================================
" ====  initialize default settings  ================================
" ===================================================================
let s:settings = {}
" Good Colorscheme: mustang, jellybeans
" Also Good: seoul256(-light), badwolf...
let s:settings.Colorscheme="jellybeans"
let s:settings.InstallVundleAutomatically=1
let s:settings.useAirline=1                        " 1: Airline  0: Powerline
let s:settings.useNERDTree=0                       " 1: NERDTree 0: none
let s:settings.UndotreeOrGundo=1                   " 1: Undotree 0: Gundo
let s:settings.YcmOrNeocomplete=1                  " 1: YCM      0: Neocomplete
  let s:settings.YcmAlternativeKeybindings=1       " only if YCM is chosen
" ====  more settings  =============================================={{{
let s:settings.supportArduino=0
let s:settings.SupportClojure=1
let s:settings.SupportRails=0
let s:settings.SupportPerl=0
let s:settings.TestPlugins=1
let s:settings.useConcealEverywhere=0
" ===================================================================}}}

" auto reload vimrc when saved ======================================{{{
if has("autocmd")
  augroup autoSourceVimrc
    autocmd!
    autocmd bufwritepost .vimrc source % | set ts=2 sw=2 sts=2 et fenc=utf-8 ff=unix foldmethod=marker foldmarker={{{,}}}
  augroup END
endif "==============================================================}}}

" ===================================================================
" ====  plugins  ====================================================
" ==================================================================={{{
if s:settings.InstallVundleAutomatically " ========================={{{
  " install vundle automatically, if not present
  if !isdirectory(expand('~').'/.vim/bundle/Vundle.vim')
    let src = 'http://github.com/gmarik/vundle.git'
    exec '!git clone '.src.' ~/.vim/bundle/Vundle.vim'
    au VimEnter * BundleInstall
  endif
endif " ============================================================}}}
if isdirectory(expand('~').'/.vim/bundle/Vundle.vim')
  filetype off                 " required

  set rtp+=~/.vim/bundle/Vundle.vim
  call vundle#begin()

  " download the rest of my config with vundle, if no ~/ftplugin folder is
  " present
  " TODO: find better test
  if !isdirectory(expand('~').'/.vim/ftplugin')
    Plugin 'maximilianhuber/myconfig', {'rtp': 'vim/'}
  endif

  " Used Plugins:
  " ===================================================================
  "   General:
  if s:settings.UndotreeOrGundo
    Plugin 'mbbill/undotree'
    nnoremap <F6> :UndotreeToggle<CR>
  else
    Plugin 'sjl/gundo.vim'
    nnoremap <F6> :GundoToggle<CR>
  endif

  Plugin 'vim-scripts/matchit.zip'

  Plugin 'jiangmiao/auto-pairs'

  Plugin 'scrooloose/syntastic' "{{{
    noremap <Leader>S :SyntasticToggleMode<CR>
    let g:syntastic_scala_checkers = []
    "let g:syntastic_haskell_checkers = ["hlint"]
  "}}}

  Plugin 'scrooloose/nerdcommenter'

  " ===================================================================
  "   Design:
  if s:settings.useAirline
    Plugin 'bling/vim-airline' "{{{
      "let g:airline_powerline_fonts = 1
      if 0 " tabline
        let g:airline#extensions#tabline#enabled = 1
        let g:airline#extensions#tabline#left_sep=' '
        let g:airline#extensions#tabline#left_alt_sep='¦'
      endif
    "}}}
  else
    Plugin 'Lokaltog/vim-powerline' " DEPRECATED in favor of Lokaltog/powerline.
  endif

  " ===================================================================
  "  Colorschemes:  ==================================================={{{
  if s:settings.Colorscheme == "jellybeans"
    Plugin 'nanotech/jellybeans.vim'
  elseif s:settings.Colorscheme == "mustang"
    Plugin 'croaker/mustang-vim'
  elseif s:settings.Colorscheme == "seoul256" || s:settings.Colorscheme == "seoul256-light"
    Plugin 'junegunn/seoul256.vim'
  elseif s:settings.Colorscheme == "badwolf"
    Plugin 'sjl/badwolf' "{{{
      let g:badwolf_darkgutter = 0
      let g:badwolf_tabline = 3
      let g:badwolf_css_props_highlight = 1
    "}}}
  endif
  "                                                                    }}}
  " ===================================================================
  "   Manage Files:
  Plugin 'kien/ctrlp.vim' "{{{
    let g:ctrlp_map = '<c-p>'
    let g:ctrlp_cmd = 'CtrlP'
    let g:ctrlp_working_path_mode = 'ra'
    nnoremap <Leader>b :CtrlPBuffer<CR>
    nnoremap <Leader>p :CtrlPMRU<CR>
  "}}}

  if s:settings.useNERDTree
    Plugin 'scrooloose/nerdtree' "{{{
      "noremap <C-n> :NERDTreeToggle<CR>
      augroup autoNERDTree
        autocmd!
        autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
      augroup END
    "}}}
  else
    noremap <C-e> :Explore<CR>
    "noremap <C-e> :Ve<CR>
  endif

  " ===================================================================
  "   Completion:  ===================================================={{{
  Plugin 'honza/vim-snippets'

  if s:settings.YcmOrNeocomplete
    Plugin 'SirVer/ultisnips' "{{{
      " Trigger configuration. Do not use <tab> if you use YCM.
      let g:UltiSnipsExpandTrigger="<c-b>"
      let g:UltiSnipsJumpForwardTrigger="<c-b>"
      "let g:UltiSnipsJumpBackwardTrigger="<c-z>"
      " If you want :UltiSnipsEdit to split your window.
      let g:UltiSnipsEditSplit="vertical"
    "}}}

    Plugin 'Valloric/YouCompleteMe' "{{{
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
      let g:ycm_key_detailed_diagnostics = "<leader>Dt"
      let g:ycm_complete_in_comments_and_strings=1

      if s:settings.YcmAlternativeKeybindings
        let g:ycm_key_list_select_completion=['<C-n>', '<c-b>', '<Down>']
        let g:ycm_key_list_previous_completion=['<C-p>', '<Up>']
        let g:UltiSnipsExpandTrigger="<tab>"
        let g:UltiSnipsJumpForwardTrigger="<tab>"
      endif
    "}}}
  else
    " neocomplete / neocomplcache and neosnippet {{{
    if has('lua')
      Plugin 'Shougo/neocomplete.vim' " {{{
        " Use neocomplcache
        let g:neocomplete#enable_at_startup = 1
        " Use smartcase.
        let g:neocomplete#enable_smart_case = 1
        " Set minimum syntax keyword length.
        let g:neocomplete#sources#syntax#min_keyword_length = 2
        let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

        " For cursor moving in insert mode(Not recommended)
        inoremap <expr><Left>  neocomplete#close_popup() . "\<Left>"
        inoremap <expr><Right> neocomplete#close_popup() . "\<Right>"
        inoremap <expr><Up>    neocomplete#close_popup() . "\<Up>"
        inoremap <expr><Down>  neocomplete#close_popup() . "\<Down>"

        function! s:my_cr_function()
          return neocomplete#close_popup() . "\<CR>"
          " For no inserting <CR> key.
          "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
        endfunction
      " }}}
    else
      Plugin 'Shougo/neocomplcache.vim' " {{{
        " Use neocomplcache.
        let g:neocomplcache_enable_at_startup = 1
        " Use smartcase.
        let g:neocomplcache_enable_smart_case = 1
        " Set minimum syntax keyword length.
        let g:neocomplcache_min_syntax_length = 2
        let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

        " For cursor moving in insert mode(Not recommended)
        inoremap <expr><Left>  neocomplcache#close_popup() . "\<Left>"
        inoremap <expr><Right> neocomplcache#close_popup() . "\<Right>"
        inoremap <expr><Up>    neocomplcache#close_popup() . "\<Up>"
        inoremap <expr><Down>  neocomplcache#close_popup() . "\<Down>"

        function! s:my_cr_function()
          return neocomplcache#smart_close_popup() . "\<CR>"
          " For no inserting <CR> key.
          "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
        endfunction
      " }}}
      " }}}
    endif
    let g:acp_enableAtStartup = 0
    " <CR>: close popup and save indent.
    inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
    " <TAB>: completion.
    inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

    " Enable omni completion.
    augroup neoOmicActivations
      autocmd!
      autocmd FileType css           setlocal omnifunc=csscomplete#CompleteCSS
      autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
      autocmd FileType javascript    setlocal omnifunc=javascriptcomplete#CompleteJS
      autocmd FileType python        setlocal omnifunc=pythoncomplete#Complete
      autocmd FileType xml           setlocal omnifunc=xmlcomplete#CompleteTags
    augroup END

    Plugin 'Shougo/neosnippet-snippets'
    Plugin 'Shougo/neosnippet.vim' "{{{
      let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'
      let g:neosnippet#enable_snipmate_compatibility=1

      imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
            \ "\<Plug>(neosnippet_expand_or_jump)" :
            \ (pumvisible() ? "\<C-n>" : "\<TAB>")
      smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
            \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
      imap <expr><S-TAB> pumvisible() ? "\<C-p>" : ""
      smap <expr><S-TAB> pumvisible() ? "\<C-p>" : ""
    "}}}
  endif
  "}}}

  " ===================================================================
  "   Vimwiki:
  Plugin 'vimwiki/vimwiki'

  " ===================================================================
  "   Haskell:
  Plugin 'Twinside/vim-haskellConceal'

  " ===================================================================
  "   Clojure:
  if s:settings.SupportClojure
    Plugin 'tpope/vim-fireplace'
    "Plugin 'vim-scripts/VimClojure'
  endif
  Plugin 'amdt/vim-niji'

  " ===================================================================
  "   CSV:
  Plugin 'chrisbra/csv.vim'

  " ===================================================================
  "   HTML:
  Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}

  " ===================================================================
  "   Arduino:
  if s:settings.supportArduino
    Plugin 'sudar/vim-arduino-syntax'
    Plugin 'tclem/vim-arduino'
  endif

  " ===================================================================
  "   Ruby:
  if s:settings.SupportRails
    Plugin 'tpope/vim-rails'
  endif

  " ===================================================================
  "   Perl:
  if s:settings.SupportPerl
    Plugin 'c9s/perlomni.vim' " {{{

    if s:settings.YcmOrNeocomplete == 0
      if has('lua')
        if !exists('g:neocomplete#sources#omni#input_patterns')
          let g:neocomplete#sources#omni#input_patterns = {}
        endif
        let g:neocomplete#sources#omni#input_patterns.perl = '\h\w*->\h\w*\|\h\w*::'
      else
        if !exists('g:neocomplcache_force_omni_patterns')
          let g:neocomplcache_force_omni_patterns = {}
        endif
        let g:neocomplcache_force_omni_patterns.perl = '\h\w*->\h\w*\|\h\w*::'
      endif
    endif
    " }}}
  endif

  " ===================================================================
  "   Others:
  Plugin 'sjl/clam.vim' "{{{
    nnoremap ! :Clam<space>
    vnoremap ! :ClamVisual<space>
  "}}}

  Plugin 'tpope/vim-eunuch'

  " ===================================================================
  "   Testing:
  if s:settings.TestPlugins
    Plugin 'terryma/vim-multiple-cursors'

    Plugin 'chrisbra/NrrwRgn'
    " :NR - Open the selected region in a new narrowed window

    Plugin 'tpope/vim-surround'

    Plugin 'mileszs/ack.vim' "{{{
      nnoremap _a :silent execute "Ack " . expand("<cWORD>") <cr>
    "}}}

  endif

  " ===================================================================

  call vundle#end()            " required
  filetype plugin indent on    " required
endif
" ===================================================================}}}
" ====  general  ====================================================
" ==================================================================={{{
set nocompatible

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

set lazyredraw
set viewoptions=folds,options,cursor,unix,slash     "unix/windows compatibility

set restorescreen=on


if s:settings.useConcealEverywhere
  if has('conceal')
    set conceallevel=1
    set listchars+=conceal:Δ
  endif
endif

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
set display+=lastline
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
"set foldlevelstart=99

" indending
set autoindent
set smartindent
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab
set smarttab
"                                                                    }}}
" ====  list  ======================================================={{{

set list
set listchars=tab:>.,trail:…,extends:#,nbsp:. " …°⎼
"set listchars=tab:│\ ,trail:•,extends:❯,precedes:❮
"set fillchars=vert:┃,diff:⎼,fold:⎼
set fillchars=vert:┃,diff:\ ,fold:\

set cpoptions+=n
set showbreak=\ \ \ ↳

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
" ====  line numberig  =============================================={{{
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

set history=3000
set undolevels=3000

" Keep undo history across sessions, by storing in file.
if exists('+undofile')
  execute "silent !mkdir " . expand("~/.undodir/") . " > /dev/null 2>&1"
  set undofile
  let &undodir=expand("~/.undodir/")
endif

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
  set guioptions+=t " tear off menu items
  set guioptions-=T " no toolbar
  set guioptions+=c " Use console messages instead of GUI dialogs
else
  set t_Co=256
  set background=dark
endif

" ====  hilight to long lines  ======================================
if exists('+colorcolumn')
    set colorcolumn=80
else
    highlight OverLength ctermbg=red ctermfg=white guibg=#592929
    match OverLength /\%81v.\+/
endif

" ====  apply colorscheme  ==========================================
exec 'colorscheme '.s:settings.Colorscheme
if s:settings.Colorscheme == "mustang"
  highlight ColorColumn ctermbg=233 guibg=#592929
endif

"tweak the colorscheme
hi CursorLine cterm=none

"Set spell hilighting
if has('spell')
  highlight clear SpellBad
  highlight SpellBad term=standout ctermfg=1
  highlight SpellBad term=underline cterm=underline
  highlight clear SpellCap
  highlight SpellCap term=underline cterm=underline
  highlight clear SpellRare
  highlight SpellRare term=underline cterm=underline
  highlight clear SpellLocal
  highlight SpellLocal term=underline cterm=underline
endif

" ===================================================================}}}
" ====  keymappings / input  ========================================
" ==================================================================={{{
if has("mouse")
  set mouse=a " Enable mouse usage (all modes) alternativ nvc
  set mousehide
endif
set backspace=indent,eol,start

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
" ====  maps to nop  ================================================{{{
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
" ====  functions  ==================================================
" ==================================================================={{{

" stolen from: https://github.com/bling/dotvim/blob/master/vimrc
function! Preserve(command) "{{{
  " preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " do the business:
  execute a:command
  " clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction "}}}

" ====  for cleanup  ================================================{{{
" delete all trails
" use :call DeleteTrailing
" or <Leader>dt
function! DeleteTrailing()
  if 0
    exe "normal mz"
    %s/\s\+$//ge
    exe "normal `z"
  else
    "new implementation
    call Preserve("%s/\\s\\+$//e")
  endif
endfunction
nnoremap <Leader>dt :call DeleteTrailing()<cr>

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
" ====  abbreviations  ==============================================
" ==================================================================={{{
" overwrite those annoying commands
cnoreabbrev <expr> W ((getcmdtype() is# ':' && getcmdline() is# 'W')?('w'):('W'))
cnoreabbrev <expr> Q ((getcmdtype() is# ':' && getcmdline() is# 'Q')?('q'):('Q'))

" handy abbreviations
iabbrev @@ mail@maximilian-huber.de
iabbrev VGr Viele Grüße<cr>Maximilian
iabbrev vlt vielleicht
iabbrev mgl möglicherweise

" correct some typos
iabbrev adn and
iabbrev Kapittel Kapitel

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

" vim:set ts=2 sw=2 sts=2 et fenc=utf-8 ff=unix foldmethod=marker foldmarker={{{,}}}:
