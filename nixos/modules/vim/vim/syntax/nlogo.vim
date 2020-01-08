" Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
" SPDX-License-Identifier: MIT
" Vim syntax file
" Language:     NetLogo 3beta2
" Maintainer:   Steven Stoddard <sstoddar@uiuc.edu>
" Last Change:  7-7-05
" Filenames:    *.nlogo,*.nlogo~ 

" TODO
" separate out variables, sa screen-edge-x  XXX
" maybe separate out qualifiers, sa -of XXX
" do indents work properly? NO
" NetLogo distinctions: XXX
"  definitions: variables, to, end
"  functions : if, ifelse, while
"  constants etc
"  variables

syn case match

" Comment
syn match nlogoComment /;.*/

" Constant
" string enclosed in double quotes
syn region nlogoString start=/"/ skip=/\\\\\|\\"/ end=/"/
" string enclosed in single quotes
syn region nlogoString start=/'/ skip=/\\\\\|\\'/ end=/'/
" number with no fractional part or exponent
syn match nlogoNumber /\d\+/
" floating point number with integer and fractional parts and optional exponent
syn match nlogoFloat /\d\+\.\d*\([Ee][-+]\=\d\+\)\=/
" floating point number with no integer part and optional exponent
syn match nlogoFloat /\.\d\+\([Ee][-+]\=\d\+\)\=/
" floating point number with no fractional part and optional exponent
syn match nlogoFloat /\d\+[Ee][-+]\=\d\+/

" Statement
setlocal iskeyword+=-
setlocal iskeyword+=?
syn keyword nlogoStatement carefully error-message map report run runresult stop startup wait without-interruption 
syn keyword nlogoConditional if ifelse and ifelse-value not or xor 
syn keyword nlogoRepeat while repeat loop foreach 

" Constant
syn keyword nlogoConstant e pi black gray white red orange brown yellow green lime turquoise cyan sky blue violet magenta pink
syn keyword nlogoBoolean false true

" Type
syn keyword nlogoType breed color heading hidden? label label-color pen-down? shape size who xcor ycor pcolor plabel plabel-color pxcor pycor breeds globals patches-own turtles-own patches turtles turtle patch BREED-at BREED-here BREED-on patch-ahead patch-at-heading-and-distance patch-here patch-left-and-ahead patch-right-and-ahead myself turtles-at turtles-from turtles-here turtles-on neighbors neighbors4 patch-left-and-ahead patch-right-and-ahead  other-turtles-here other-BREED-here screen-edge-x screen-edge-y screen-size-x screen-size-y 

" Special
"syn match nlogoError      /[)\]}]/
"syn match nlogoCurlyError "]" 
"syn match nlogoCurlyError /[)\]]/ contained
"syn match nlogoParenError ")" 

syn keyword nlogoTodo contained TODO FIXME XXX ToDo 
syn cluster nlogoCommentGroup contains=vimTodo

" assignment
syn keyword nlogoDefine set let to end to-report

syn keyword nlogoSpecial with value-from values-from max-one-of min-one-of one-of random-n-of random-one-of with-max with-min but-first but-last empty?first last any?
syn keyword nlogoIdentifier back bk clear-turtles ct create-BREED create-custom-BREED create-custom-turtles cct create-turtles crt die distance distance-nowrap distancexy distancexy-nowrap downhill downhill4 dx dy forward fd hatch hatch-BREED hideturtle ht home inspect is-turtle? jump left lt no-label nobody pen-down pd pen-erase pe pen-up pu right rt self set-default-shape setxy shapes showturtle st sprout sprout-BREED stamp subtract-headings towards towards-nowrap towardsxy towardsxy-nowrap uphill clear-patches cp diffuse diffuse4 distance distance-nowrap distancexy distancexy-nowrap inspect is-patch? no-label nobody nsum nsum4 self sprout sprout-BREED ask at-points count histogram-from in-radius in-radius-nowrap is-agent? is-agentset? is-patch-agentset? is-turtle-agentset? extract-hsb extract-rgb hsb rgb scale-color shade-of? wrap-color clear-all ca clear-drawing clear-patches cp  display follow home no-display no-label watch beep clear-output export-view export-interface export-output export-plot export-all-plots export-world get-date-and-time import-world mouse-down? mouse-xcor mouse-ycor output-print output-show output-type output-write print read-from-string reset-timer set-current-directory show timer type user-choice user-choose-directory user-choose-file user-choose-new-file user-input user-message user-yes-or-no? write file-at-end? file-close file-close-all file-delete file-exists? file-open file-print file-read file-read-characters file-read-line file-show file-type file-write user-choose-directory user-choose-file user-choose-new-file  filter first foreach fput is-list? item length list lput map member? modes n-values position reduce remove remove-duplicates remove-item replace-item reverse sentence shuffle sort sort-by sublist is-string? item length member? position remove remove-item read-from-string replace-item reverse substring word  abs acos asin atan ceiling cos exp floor int ln log max mean median min mod modes precision random random-exponential random-float random-gamma random-int-or-float random-normal random-poisson random-seed remainder round sin sqrt standard-deviation subtract-headings sum tan variance autoplot? auto-plot-off auto-plot-on clear-all-plots clear-plot create-temporary-plot-pen export-plot export-all-plots histogram-from histogram-list plot plot-name plot-pen-down ppd plot-pen-reset plot-pen-up ppu plot-x-max plot-x-min plot-y-max plot-y-min plotxy ppd ppu set-current-plot set-current-plot-pen set-histogram-num-bars set-plot-pen-color set-plot-pen-interval set-plot-pen-mode set-plot-x-range set-plot-y-range movie-cancel movie-close movie-grab-view movie-grab-interface movie-set-frame-rate movie-start movie-status follow home ride setxyz watch

hi link nlogoComment  Comment
hi link nlogoConstant Constant
hi link nlogoString      String
hi link nlogoNumber      Number
hi link nlogoBoolean     Boolean
hi link nlogoFloat       Float
hi link nlogoStatement   Statement
hi link nlogoConditional Conditional
hi link nlogoRepeat      Repeat
hi link nlogoIdentifier  Identifier
hi link nlogoType	      Type
hi link nlogoDefine     Define
hi link nlogoSpecial    Special
hi link nlogoError       Error
hi link nlogoBraceError  Error
hi link nlogoCurlyError  Error
hi link nlogoParenError  Error
hi link nlogoTodo       Todo

let b:current_syntax="nlogo"
