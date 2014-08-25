" Maintainer:	Maximilian Huber (mail@maximilian-huber.de)
" Version:      0.1
" Last Modified: Tue Aug 19, 2014  02:24
"
" Parts taken from:
"   mustang (great, but i want my own)
"   badwolf (to way much contrast)

" Preamble {{{
if !has("gui_running") && &t_Co != 88 && &t_Co != 256
    finish
endif

set background=dark

hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "mycolorscheme"
" }}}

" Palette {{{

let s:myc = {}

" The most basic of all our colors is a slightly tweaked version of the Molokai
" Normal text.
let s:myc.plain = ['f8f6f2', 15]

" Pure and simple.
let s:myc.snow = ['ffffff', 15]
let s:myc.coal = ['000000', 16]

" All of the Gravel colors are based on a brown from Clouds Midnight.
let s:myc.brightgravel   = ['d9cec3', 252]
let s:myc.lightgravel    = ['998f84', 245]
let s:myc.gravel         = ['857f78', 243]
let s:myc.mediumgravel   = ['666462', 241]
let s:myc.deepgravel     = ['45413b', 238]
let s:myc.deepergravel   = ['35322d', 236]
let s:myc.darkgravel     = ['242321', 235]
let s:myc.blackgravel    = ['1c1b1a', 233]
let s:myc.blackestgravel = ['141413', 232]

" A color sampled from a highlight in a photo of a glass of Dale's Pale Ale on
" my desk.
let s:myc.dalespale = ['fade3e', 221]

" A beautiful tan from Tomorrow Night.
let s:myc.dirtyblonde = ['f4cf86', 222]

" Delicious, chewy red from Made of Code for the poppiest highlights.
let s:myc.taffy = ['ff2c4b', 196]

" Another chewy accent, but use sparingly!
let s:myc.saltwatertaffy = ['8cffba', 121]

" The star of the show comes straight from Made of Code.
let s:myc.tardis = ['0a9dff', 39]

" This one's from Mustang, not Florida!
let s:myc.orange = ['ffa724', 214]

" A limier green from Getafe.
let s:myc.lime = ['aeee00', 154]

" Rose's dress in The Idiot's Lantern.
let s:myc.dress = ['ff9eb8', 211]

" Another play on the brown from Clouds Midnight.  I love that color.
let s:myc.toffee = ['b88853', 137]

" Also based on that Clouds Midnight brown.
let s:myc.coffee    = ['c7915b', 173]
let s:myc.darkroast = ['88633f', 95]

" }}}
" Highlighting Function {{{
function! s:HL(group, fg, ...)
    " Arguments: group, guifg, guibg, gui, guisp

    let histring = 'hi ' . a:group . ' '

    if strlen(a:fg)
        if a:fg == 'fg'
            let histring .= 'guifg=fg ctermfg=fg '
        else
            let c = get(s:myc, a:fg)
            let histring .= 'guifg=#' . c[0] . ' ctermfg=' . c[1] . ' '
        endif
    endif

    if a:0 >= 1 && strlen(a:1)
        if a:1 == 'bg'
            let histring .= 'guibg=bg ctermbg=bg '
        else
            let c = get(s:myc, a:1)
            let histring .= 'guibg=#' . c[0] . ' ctermbg=' . c[1] . ' '
        endif
    endif

    if a:0 >= 2 && strlen(a:2)
        let histring .= 'gui=' . a:2 . ' cterm=' . a:2 . ' '
    endif

    if a:0 >= 3 && strlen(a:3)
        let c = get(s:myc, a:3)
        let histring .= 'guisp=#' . c[0] . ' '
    endif

    " echom histring

    execute histring
endfunction

" Vim >= 7.0 specific colors
if version >= 700
  hi CursorLine guibg=#2d2d2d ctermbg=236
  hi CursorColumn guibg=#2d2d2d ctermbg=236
  hi MatchParen guifg=#d0ffc0 guibg=#2f2f2f gui=bold ctermfg=157 ctermbg=237 cterm=bold
  hi Pmenu 		guifg=#ffffff guibg=#444444 ctermfg=255 ctermbg=238
  hi PmenuSel 	guifg=#000000 guibg=#b1d631 ctermfg=0 ctermbg=148
endif

" General colors
hi Cursor 		guifg=NONE    guibg=#626262 gui=none ctermbg=241
hi Normal 		guifg=#e2e2e5 guibg=#202020 gui=none ctermfg=253 ctermbg=234
hi NonText 		guifg=#808080 guibg=#303030 gui=none ctermfg=244 ctermbg=235
hi LineNr 		guifg=#808080 guibg=#000000 gui=none ctermfg=244 ctermbg=232
hi StatusLine 	guifg=#d3d3d5 guibg=#444444 gui=italic ctermfg=253 ctermbg=238 cterm=italic
hi StatusLineNC guifg=#939395 guibg=#444444 gui=none ctermfg=246 ctermbg=238
hi VertSplit 	guifg=#444444 guibg=#444444 gui=none ctermfg=238 ctermbg=238
hi Folded 		guibg=#384048 guifg=#a0a8b0 gui=none ctermbg=4 ctermfg=248
hi Title		guifg=#f6f3e8 guibg=NONE	gui=bold ctermfg=254 cterm=bold
hi Visual		guifg=#faf4c6 guibg=#3c414c gui=none ctermfg=254 ctermbg=4
hi SpecialKey	guifg=#808080 guibg=#343434 gui=none ctermfg=244 ctermbg=236

" Syntax highlighting
hi Comment 		guifg=#808080 gui=italic ctermfg=244
hi Todo 		guifg=#8f8f8f gui=italic ctermfg=245
hi Boolean      guifg=#b1d631 gui=none ctermfg=148
hi String 		guifg=#b1d631 gui=italic ctermfg=148
hi Identifier 	guifg=#b1d631 gui=none ctermfg=148
hi Function 	guifg=#ffffff gui=bold ctermfg=255
hi Type 		guifg=#7e8aa2 gui=none ctermfg=103
hi Statement 	guifg=#7e8aa2 gui=none ctermfg=103
hi Keyword		guifg=#ff9800 gui=none ctermfg=208
hi Constant 	guifg=#ff9800 gui=none  ctermfg=208
hi Number		guifg=#ff9800 gui=none ctermfg=208
hi Special		guifg=#ff9800 gui=none ctermfg=208
hi PreProc 		guifg=#faf4c6 gui=none ctermfg=230
hi Todo         guifg=#000000 guibg=#e6ea50 gui=italic

" Code-specific colors
hi pythonOperator guifg=#7e8aa2 gui=none ctermfg=103

