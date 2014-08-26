let g:tex_flavor = "latex"
setlocal wrap
setlocal sw=2
setlocal ts=2
setlocal et
setlocal textwidth=79
setlocal cc=80

setlocal linebreak

setlocal foldmethod=marker
setlocal foldmarker=%{{{,%}}}

setlocal iskeyword+=: " type /ref{fig: and prec <C-n> to autocomplete references
setlocal iskeyword+=- " same with -
setlocal iskeyword+=_ " same with _

"setlocal noautoindent
setlocal nocindent
setlocal nosmartindent
setlocal indentexpr=

setlocal autoindent

setlocal spell
setlocal spelllang=de_de,en_us
setlocal spellfile=~/.vim/spellfile.add

"inoremap <expr>" getline('.')[col(".")-2] =~ "\\s" ? "\"`\"\'<left><left>" : "\"'"
inoremap <expr>[ getline('.')[col(".")-2] =~ "\\" ? "[<C-v>u005c]<left><left>" : "["
"inoremap <expr>{ getline('.')[col(".")-2] =~ "\\" ? "{<C-v>u005c}<left><left>" : "{"

nnoremap <leader>$ viw<esc>a$<esc>hbi$<esc>lel

iabbrev ... <bs>\dots
if 1
  iabbrev ρ \rho
  iabbrev μ \mu
  iabbrev π \pi
  iabbrev λ \lambda
end

"nnoremap <leader>cl :! runlatex -pdf % > logfile 2>&1 &<CR><CR>
"nnoremap <leader>oe :! llpp %:r.pdf > /dev/null 2>&1 &<CR><CR>
"nnoremap <leader>oa :! llpp *.pdf > /dev/null 2>&1 &<CR><CR>

function! SyncTexForward()
exec 'silent !myTexWrapper.sh % '.line('.')." ".col('.')
redraw!
endfunction
nnoremap <Leader>f :call SyncTexForward()<CR>
