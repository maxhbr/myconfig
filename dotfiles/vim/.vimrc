" ~/.vimrc
"
" The other files of my vim config can be found at github:
"     https://github.com/maximilianhuber/myconfig
"
" Try it out:
"     simply call the script `_tasteMyVimrc.sh` in my config repo. It will
"     generate a folder `/tmp/vimtest/` and install my vimrc and the other
"     files completely in this folder
"
" Written by Maximilian-Huber.de
"
" Worth reading:
"   Steve Losh: Learn Vimscript the Hard Way
"
" Last Modified: Mon Jun 15, 2015  09:25

" ===================================================================
" ====  initialize settings  ========================================
" ===================================================================
let s:settings = {}
let b:settings = {}
" Best Colorschemes: landscape, mustang, jellybeans, hybrid
" Light Colorscheme: lucius, hemisu
" Also Good: seoul256(-light), badwolf, kolor, molokai, wombat256...
" let b:settings.Colorscheme="jellybeans"
" let b:settings.Colorscheme="hemisu"
let s:settings.LightColorscheme="lucius"
let s:settings.DarkColorscheme="landscape"
let s:settings.ColorschemeVariant="dark"      " dark or light
let s:settings.InstallPluginManagerAutomatically=1
let s:settings.ChooseStatusline=2              " 2: lightline
                                               " 1: Airline
                                               " 0: Powerline
let s:settings.useNERDTree=0                   " 1: NERDTree     0: none
let s:settings.UndotreeOrGundo=1               " 1: Undotree     0: Gundo
let s:settings.ChooseCommenter=2               " 2: vim-commentry
                                               " 1: tComment
                                               " 0: NerdCommenter
let s:settings.YcmOrNeocomplete=1              " 1: YCM          0: Neocomplete
  let s:settings.YcmAlternativeKeybindings=1   " only if YCM is chosen
" Plugin Groups:
" hasekll, scheme, clojure, lisp, html, csv, arduino, ruby, perl
let s:settings.supportLanguages=['haskell', 'arduino', 'html', 'csv', 'lisp', 'perl', 'arduino']
let s:settings.TestPlugins=1

function! s:applyColorscheme() "{{{
  if s:settings.ColorschemeVariant == "light" "{{{
    let l:col = s:settings.LightColorscheme
    set background=light
  else
    let l:col = s:settings.DarkColorscheme
    set background=dark
  endif "}}}

  "configure the colorscheme
  if l:col == "badwolf"
    let g:badwolf_darkgutter = 0
    let g:badwolf_tabline = 3
    let g:badwolf_css_props_highlight = 1
  endif

  "apply the colorscheme
  exec 'colorscheme '.l:col

  "tweak the colorscheme
  if l:col == "mustang"
    highlight ColorColumn ctermbg=233 guibg=#592929
  elseif l:col == "badwolf"
    highlight LineNr ctermbg=235
  elseif l:col == "jellybeans"
    highlight LineNr ctermbg=234
  elseif l:col == "hybrid"
    highlight LineNr ctermfg=243 ctermbg=232
  endif
  hi CursorLine cterm=none

  " if s:settings.ChooseStatusline == 2
  "   if exists("s:lightline#colorscheme#default#palette")
  "     call lightline#enable()
  "   endif
  " endif
endfunction "}}}
" augroup AutoApplyColorscheme "{{{
"   autocmd!
"   autocmd BufEnter * call s:applyColorscheme()
" augroup END "}}}

let mapleader=","
" let maplocalleader = "\\"

" auto reload vimrc when saved ======================================{{{
if has("autocmd")
  augroup autoSourceVimrc
    autocmd!
    autocmd bufwritepost .vimrc source %
          \ | set ts=2 sw=2 sts=2 et fenc=utf-8 ff=unix foldmethod=marker foldmarker={{{,}}}
  augroup END
endif "==============================================================}}}

" ===================================================================
" ====  plugins  ====================================================
" ==================================================================={{{
if s:settings.InstallPluginManagerAutomatically " ========================={{{
  " install vundle automatically, if not present
  " if !isdirectory(expand('~').'/.vim/bundle/Vundle.vim')
  "   let src = 'http://github.com/gmarik/vundle.git'
  "   exec '!git clone '.src.' ~/.vim/bundle/Vundle.vim'
  "   au VimEnter * BundleInstall
  " endif
  if !filereadable(expand('~').'/.vim/autoload/plug.vim')
    exec '!mkdir -p '.expand('~').'/.vim/autoload/'
    exec '!curl -fLo '.expand('~').'/.vim/autoload/plug.vim '.
      \ 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    au VimEnter * PlugInstall
  endif
endif " ============================================================}}}
" if isdirectory(expand('~').'/.vim/bundle/Vundle.vim')
  " filetype off                      " required
  " set rtp+=~/.vim/bundle/Vundle.vim " required
  " call vundle#begin()               " required
if filereadable(expand('~').'/.vim/autoload/plug.vim')
  filetype off
  call plug#begin('~/.vim/plug')

  " " download the rest of my config with vundle, if no ~/ftplugin folder is
  " " present
  " " TODO: find better test
  " if !isdirectory(expand('~').'/.vim/ftplugin')
  "   Plugin 'maximilianhuber/myconfig', {'rtp': 'vim/'}
  " endif

  " Used Plugins:
  " ===================================================================
  "   General:
  if s:settings.UndotreeOrGundo
    Plug 'mbbill/undotree',  { 'on': 'UndotreeToggle' }
    nnoremap <F6> :UndotreeToggle<CR>
  else
    Plug 'sjl/gundo.vim',  { 'on': 'GundoToggle' }
    nnoremap <F6> :GundoToggle<CR>
  endif

  Plug 'vim-scripts/matchit.zip'

  Plug 'Raimondi/delimitMate'
  " or:
  "Plug 'jiangmiao/auto-pairs'

  Plug 'scrooloose/syntastic' "{{{
    let g:syntastic_error_symbol = '✗'
    let g:syntastic_style_error_symbol = '✠'
    let g:syntastic_warning_symbol = '∆'
    let g:syntastic_style_warning_symbol = '≈'
    noremap <Leader>S :SyntasticToggleMode<CR>
    let g:syntastic_scala_checkers = []
    
    let g:syntastic_always_populate_loc_list = 0
    let g:syntastic_auto_loc_list = 0
    let g:syntastic_check_on_open = 1
    let g:syntastic_check_on_wq = 0
    " let g:syntastic_haskell_checkers = ["hlint","scan"]
    let g:syntastic_haskell_checkers = ["hlint","scan","hdevtools"]
    " let g:syntastic_haskell_checkers = ["hlint","scan","hdevtools","ghc-mod"]
  "}}}

  if  s:settings.ChooseCommenter==2
    Plug 'tpope/vim-commentary'
  elseif  s:settings.ChooseCommenter==1
    Plug 'tomtom/tcomment_vim'
  else
    Plug 'scrooloose/nerdcommenter'
  end

  " ===================================================================
  "   Design:
  if s:settings.ChooseStatusline==2
    Plug 'itchyny/lightline.vim'
  elseif s:settings.ChooseStatusline==1
    Plug 'bling/vim-airline' "{{{
      "let g:airline_powerline_fonts = 1
      if 0 " tabline
        let g:airline#extensions#tabline#enabled = 1
        let g:airline#extensions#tabline#left_sep=' '
        let g:airline#extensions#tabline#left_alt_sep='¦'
      endif
    "}}}
  else
    Plug 'Lokaltog/vim-powerline' " DEPRECATED in favor of Lokaltog/powerline.
  endif

  " ===================================================================
  "  Colorschemes:  ==================================================={{{
  " Often used colorschemes
  Plug 'nanotech/jellybeans.vim'
  Plug 'itchyny/landscape.vim'
  Plug 'jonathanfilip/vim-lucius'
  Plug 'noahfrederick/vim-hemisu'
  Plug 'sjl/badwolf'
  " other good colorschemes
  " Plug 'croaker/mustang-vim'
  " Plug 'junegunn/seoul256.vim'
  " Plug 'w0ng/vim-hybrid'
  " Plug 'zeis/vim-kolor'
  " Plug 'tomasr/molokai'
  " Plug 'vim-scripts/wombat256.vim'
  "                                                                    }}}
  " ===================================================================
  "   Manage Files:
  Plug 'kien/ctrlp.vim' "{{{
    let g:ctrlp_map = '<c-p>'
    let g:ctrlp_cmd = 'CtrlP'
    let g:ctrlp_working_path_mode = 'ra'
    nnoremap <Leader>b :CtrlPBuffer<CR>
    nnoremap <Leader>p :CtrlPMRU<CR>
  "}}}

  Plug 'Shougo/unite.vim' "{{{
    if !has("gui_running") && !has("win32")
      nnoremap <silent> <Nul> :Unite -start-insert buffer file<CR>
    else
      nnoremap <silent> <C-space> :Unite -start-insert buffer file<CR>
    end
  "}}}
  Plug 'Shougo/vimfiler.vim', { 'on': ['VimFiler', 'VimFilerExplorer'] } "{{{
    let g:vimfiler_as_default_explorer = 1
    noremap <C-e> :VimFilerExplorer<CR>
  "}}}

  if s:settings.useNERDTree
    Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' } "{{{
      "noremap <C-n> :NERDTreeToggle<CR>
      augroup autoNERDTree
        autocmd!
        autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
      augroup END
    "}}}
  " else
    " noremap <C-e> :Explore<CR>
    "noremap <C-e> :Ve<CR>
  endif

  Plug 'mileszs/ack.vim', { 'on': 'Ack' } "{{{
    nnoremap _a :silent execute "Ack " . expand("<cWORD>") <cr>
    " use ggreer/the_silver_searcher
    let g:ackprg = 'ag --nogroup --nocolor --column'
  "}}}

  " ===================================================================
  "   Completion:  ===================================================={{{
  Plug 'honza/vim-snippets'

  if s:settings.YcmOrNeocomplete
    Plug 'SirVer/ultisnips' "{{{
      " Trigger configuration. Do not use <tab> if you use YCM.
      let g:UltiSnipsExpandTrigger="<c-b>"
      let g:UltiSnipsJumpForwardTrigger="<c-b>"
      "let g:UltiSnipsJumpBackwardTrigger="<c-z>"
      " If you want :UltiSnipsEdit to split your window.
      let g:UltiSnipsEditSplit="vertical"
    "}}}

    Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' } "{{{
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
      Plug 'Shougo/neocomplete.vim' " {{{
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
      Plug 'Shougo/neocomplcache.vim' " {{{
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

    Plug 'Shougo/neosnippet-snippets'
    Plug 'Shougo/neosnippet.vim' "{{{
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
    Plug 'ervandew/supertab' "{{{
      let g:SuperTabDefaultCompletionType = "<c-n>"
    "}}}
  "}}}

  " ===================================================================
  "   Vimwiki:
  Plug 'vimwiki/vimwiki'

  " ===================================================================
  "   Haskell:
  if (index(s:settings.supportLanguages, 'haskell') >= 0)
    " http://www.stephendiehl.com/posts/vim_haskell.html
    " https://wiki.haskell.org/Vim
    " http://blog.mno2.org/posts/2011-11-17-vim-plugins-for-haskell-programmers.html

    "needs $ cabal install ghc-mod
    Plug 'eagletmt/ghcmod-vim', { 'for': ['haskell','lhaskell'] } "{{{
      Plug 'Shougo/vimproc.vim', { 'do': 'make', 'for': ['haskell','lhaskell'] }
      hi ghcmodType ctermbg=yellow
      let g:ghcmod_type_highlight = 'ghcmodType'
    "}}}

    "needs $ cabal install hoogle
    Plug 'Twinside/vim-hoogle', { 'for': ['haskell','lhaskell'] }

    " In addition to Syntastic, it is recommended that you also use
    " vim-hdevtools for additional functionality.
    "needs $ cabal install hdevtools
    Plug 'bitc/vim-hdevtools', { 'for': ['haskell','lhaskell'] } "{{{
      let g:syntastic_haskell_hdevtools_args = '-g-Wall'
      " let g:syntastic_haskell_hdevtools_args = '-g-isrc -g-Wall'
    "}}}

    Plug 'eagletmt/neco-ghc', { 'for': ['haskell','lhaskell'] }
    
    if 0
      " Plug 'lukerandall/haskellmode-vim', { 'for': ['haskell','lhaskell'] } "{{{
      "   let g:haddock_browser = 'chromium'
      " "}}}
    else
      Plug 'dag/vim2hs', { 'for': ['haskell','lhaskell'] } "{{{
        let g:haskell_conceal_wide = 0
      "}}}
    endif

    " better highlighting
    " Plug 'vim-scripts/haskell.vim', { 'for': 'haskell' }

    " Plug 'Twinside/vim-haskellConceal', { 'for': ['haskell','lhaskell'] }

    " Plug 'Twinside/vim-haskellFold', { 'for': ['haskell','lhaskell'] }
  end

  " ===================================================================
  "   Clojure, Scheme(Racket) and Lisp:
  if (index(s:settings.supportLanguages, 'clojure') >= 0)
    Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
    "Plug 'vim-scripts/VimClojure'
  endif

  if (index(s:settings.supportLanguages, 'scheme') >= 0)
    Plug 'wlangstroth/vim-racket', { 'for': 'scheme' }
  endif

  " if (index(s:settings.supportLanguages, 'lisp') >= 0)
  "   Plug 'kovisoft/slimv'
  " endif

  "Rainbow parantheses:
  if (index(s:settings.supportLanguages, 'clojure') >= 0)
        \ || (index(s:settings.supportLanguages, 'scheme') >= 0)
        \ || (index(s:settings.supportLanguages, 'lisp') >= 0)
    " original but dead repo: 'amdt/vim-niji'
    Plug 'raymond-w-ko/vim-niji'
    "{{{
      let g:niji_matching_filetypes = ['lisp', 'scheme', 'clojure']
    "}}}
  endif

  " ===================================================================
  "   CSV:
  if (index(s:settings.supportLanguages, 'csv') >= 0)
    Plug 'chrisbra/csv.vim' , { 'for': 'csv' }
  endif

  " ===================================================================
  "   HTML:
  if (index(s:settings.supportLanguages, 'html') >= 0)
    Plug 'rstacruz/sparkup', {'rtp': 'vim/'}
  endif

  " ===================================================================
  "   Arduino:
  if (index(s:settings.supportLanguages, 'arduino') >= 0)
    Plug 'sudar/vim-arduino-syntax' , { 'for': 'arduino' }
    Plug 'tclem/vim-arduino' , { 'for': 'arduino' }
  endif

  " ===================================================================
  "   Ruby:
  if (index(s:settings.supportLanguages, 'ruby') >= 0)
    Plug 'tpope/vim-rails' , { 'for': 'ruby' }
  endif

  " ===================================================================
  "   Perl:
  if (index(s:settings.supportLanguages, 'perl') >= 0)
    Plug 'c9s/perlomni.vim' , { 'for': 'perl' } " {{{
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
  "   Tagbar:
  " needs Ctags in $PATH
  Plug 'majutsushi/tagbar', { 'on': 'TagbarToggle' } "{{{
    nmap <leader>tt :TagbarToggle<CR>
  "}}}

  " ===================================================================
  "   Others:
  Plug 'sjl/clam.vim', { 'on': ['Clam', 'ClamVisual'] } "{{{
    nnoremap ! :Clam<space>
    vnoremap ! :ClamVisual<space>
  "}}}

  Plug 'tpope/vim-eunuch'

  Plug 'chrisbra/NrrwRgn'
  " :NR - Open the selected region in a new narrowed window

  Plug 'tpope/vim-surround'
  " cs'"

  Plug 'terryma/vim-multiple-cursors'
  " Ctrl-n

  Plug 'Yggdroot/indentLine', { 'for': ['tex', 'vim', 'ocaml', 'haskell'] }

  Plug 'junegunn/limelight.vim' "{{{
  " Color name (:help cterm-colors) or ANSI code
    let g:limelight_conceal_ctermfg = 'gray'
    let g:limelight_conceal_ctermfg = 240

    " Color name (:help gui-colors) or RGB color
    let g:limelight_conceal_guifg = 'DarkGray'
    let g:limelight_conceal_guifg = '#777777'

    " Default: 0.5
    let g:limelight_default_coefficient = 0.7
  "}}}
  Plug 'junegunn/goyo.vim' "{{{
    " :Goyo
    " autocmd User GoyoEnter Limelight
    " autocmd User GoyoLeave Limelight!

    let g:goyo_margin_top = 1
    let g:goyo_margin_bottom = 1
    let g:goyo_width = 80
  "}}}

  " ===================================================================
  "   Notes:
  if isdirectory(expand('~').'/.notes')
    Plug 'xolox/vim-misc'
    Plug 'xolox/vim-notes'
    let g:notes_directories = ['~/.notes']
  endif

  " ===================================================================
  "   Testing:
  if s:settings.TestPlugins
    Plug 'tpope/vim-dispatch', { 'on': ['Start', 'Dispatch'] } "{{{
      augroup vim_dispatch_autocmds
        autocmd!
        autocmd FileType java let b:dispatch = 'javac %'
        autocmd FileType lisp let b:dispatch = '/usr/bin/env clisp %'
        autocmd FileType perl let b:dispatch = '/usr/bin/env perl %'
        autocmd FileType haskell let b:dispatch = 'ghci %'
      augroup END
      noremap <leader>ö :Dispatch<cr>
    "}}}

    Plug 'tpope/vim-fugitive'

    Plug 'airblade/vim-gitgutter'
  endif

  " ===================================================================
  " Not used plugins {{{
  "Plug 'justinmk/vim-sneak'

  "Plug 'goldfeld/ctrlr.vim'

  " Plug 'AndrewRadev/splitjoin.vim'
  " " gS gJ

  " Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }
  " }}}

  call plug#end()
  filetype plugin indent on
endif
  " call vundle#end()                 " required
  " filetype plugin indent on         " required
" endif
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
set showcmd     " Show (partial) command in status line.
set showmatch   " Show matching brackets.
"set autowrite  " Automatically save before commands like :next and :make
set noautowrite " don't automagically write on :next"
set visualbell  " ausgehebelt durch noerrorbells ?
set noerrorbells
set hidden
set autoread
set magic       " For regular expressions turn magic on
set splitbelow  " set splitright
set autochdir

set lazyredraw
set viewoptions=folds,options,cursor,unix,slash     "unix/windows compatibility

set restorescreen=on

" ====  wildmenu  ==================================================={{{
set complete=.,w,b,u,U,t,i,d    " do lots of scanning on tab completion"

set wildmenu "Kommando Zeilen Vervollständigung
"set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png
set wildignore=*.dll,*.o,*.obj,*.exe,*.hi,*.pyc,
  \*.bak,*.bac,
  \*.jpg,*.gif,*.png,
  \*.aux,*.bbl,*.blg,*.fdb_latexmk,*.fls,*.idx,*.ilg,*.ind,*.nlo,*.toc,
  \*.cmi,*.cmx,*.cmo
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
set foldlevel=99
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
" " stops slow responding in large files
" set synmaxcol=128
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

set textwidth=80    " Don't wrap lines by default
let &foldcolumn=(&columns - &textwidth - 2) / 2
let &synmaxcol=max([128, &columns])
augroup mySidepaddingAugroup
  autocmd!
  autocmd VimResized * let &foldcolumn=(&columns - &textwidth - 2) / 2
  autocmd VimResized * let &synmaxcol=max([128, &columns])
augroup END

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
endif

" ====  hilight to long lines  ======================================
if exists('+colorcolumn')
    set colorcolumn=80
else
    highlight OverLength ctermbg=red ctermfg=white guibg=#592929
    match OverLength /\%81v.\+/
endif

" ====  apply colorscheme  ==========================================
call s:applyColorscheme()

if has('spell') " Set spell hilighting =============================={{{
  highlight clear SpellBad
  highlight SpellBad term=standout ctermfg=1
  highlight SpellBad term=underline cterm=underline
  highlight clear SpellCap
  highlight SpellCap term=underline cterm=underline
  highlight clear SpellRare
  highlight SpellRare term=underline cterm=underline
  highlight clear SpellLocal
  highlight SpellLocal term=underline cterm=underline
endif " =============================================================}}}

" ===================================================================}}}
" ====  keymappings / input  ========================================
" ==================================================================={{{
if has("mouse")
  set mouse=nv
  set mousehide
endif
set backspace=indent,eol,start

"nnoremap ; :

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
inoremap <PageUp> <nop>
" nnoremap <PageUp> <nop>
" vnoremap <PageUp> <nop>
inoremap <PageDown> <nop>
" nnoremap <PageDown> <nop>
" vnoremap <PageDown> <nop>

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
vnoremap <silent> <F5> <Esc>:update<CR>

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
function! MyBigLeftPadding()
  let &foldcolumn = (&columns - &textwidth) / 2
endfunction
" ===================================================================}}}
" ====  abbreviations  ==============================================
" ==================================================================={{{
" overwrite those annoying commands
cnoreabbrev <expr> W ((getcmdtype() is# ':' && getcmdline() is# 'W')?('w'):('W'))
cnoreabbrev <expr> Q ((getcmdtype() is# ':' && getcmdline() is# 'Q')?('q'):('Q'))

" handy abbreviations
iabbrev @@ mail@maximilian-huber.de
iabbrev VGr Viele Grüße<cr>Maximilian
iabbrev BRr Best regards<cr>Maximilian
iabbrev KRr Kind regards<cr>Maximilian
iabbrev vlt vielleicht
iabbrev mgl möglicherweise
iabbrev mglw möglicherweise

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
    " Shell Sript:
    autocmd BufNewFile *.sh 0put =\"#!/usr/bin/env bash\"|$
    " Python Sript:
    autocmd BufNewFile *.py 0put =\"#!/usr/bin/env python\<nl># -*- coding: iso-8859-15 -*-\<nl>\"|$
    " Ruby Script:
    autocmd BufNewFile *.rb 0put =\"#!/usr/bin/env ruby\<nl># -*- coding: None -*-\<nl>\"|$
    " Perl Sript:
    autocmd BufNewFile *.pl 0put =\"#!/usr/bin/env perl\<nl>\<nl>use strict;\<nl>use warnings;\<nl>\"|$
    " Perl Module:
    autocmd BufNewFile *.pm 0put =\"package ;\<nl>\<nl>use strict;\<nl>use warnings;\<nl>\"|$
    " Lisp Sript:
    autocmd BufNewFile *.cl 0put =\"#!/usr/bin/env clisp\<nl>\"|$
    autocmd BufNewFile *.lisp 0put =\"#!/usr/bin/env clisp\<nl>\"|$
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

" vim:set ts=2 sw=2 sts=2 et fenc=utf-8 ff=unix foldmethod=marker foldlevel=0 foldmarker={{{,}}}:
