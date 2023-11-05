" Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
" SPDX-License-Identifier: MIT
let b:ColorschemeVariant="light"

let g:tex_flavor = "latex"
setlocal wrap
setlocal sw=2
setlocal ts=2
setlocal et
setlocal textwidth=79
setlocal cc=80

setlocal showbreak=@

let g:tex_conceal= 'a'

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

setlocal nonu

"inoremap <expr>" getline('.')[col(".")-2] =~ "\\s" ? "\"`\"\'<left><left>" : "\"'"
inoremap <expr>[ getline('.')[col(".")-2] =~ "\\" ? "[<C-v>u005c]<left><left>" : "["
"inoremap <expr>{ getline('.')[col(".")-2] =~ "\\" ? "{<C-v>u005c}<left><left>" : "{"

nnoremap <leader>$ viw<esc>a$<esc>hbi$<esc>lel

iabbrev .. <bs>\dots
iabbrev ,, ,\dots
iabbrev ++ <bs>+\cdots+
iabbrev ** <bs>*\cdots*
iabbrev Leftrightarrow Leftrightarrow{}
iabbrev Rightarrow Rightarrow{}
if 1 " iabbrev's for greek letters and more (for neo-layout) {{{
  "iabbrev ξ
  iabbrev λ \lambda
  iabbrev χ \chi
  iabbrev ω \omega
  iabbrev κ \kappa
  iabbrev ψ \psi
  "iabbrev γ
  iabbrev φ \varphi
  iabbrev ϕ \phi
  iabbrev ς \varsigma
  "iabbrev ι
  iabbrev α \alpha
  iabbrev ε \varepsilon
  "iabbrev ο
  iabbrev σ \sigma
  iabbrev ν \nu
  iabbrev υ \nu
  iabbrev ρ \rho
  iabbrev τ \tau
  iabbrev δ \delta
  iabbrev ϵ \epsilon
  iabbrev η \eta
  iabbrev π \pi
  iabbrev ζ \zeta
  iabbrev β \beta
  iabbrev μ \mu
  "iabbrev ϱ
  "iabbrev ϑ
  iabbrev ϰ \xi

  iabbrev Ξ \Xi
  "iabbrev √
  "iabbrev Λ
  "iabbrev ℂ
  "iabbrev Ω
  "iabbrev ×
  "iabbrev Ψ
  "iabbrev Γ
  "iabbrev Φ
  "iabbrev ℚ
  "iabbrev ⊂
  "iabbrev ∫
  "iabbrev ∀
  "iabbrev ∃
  "iabbrev ∈
  "iabbrev Σ
  "iabbrev ℕ
  "iabbrev ℝ
  "iabbrev ∂
  "iabbrev Δ
  "iabbrev ∇
  "iabbrev ∪
  "iabbrev ∩
  "iabbrev ℵ
  "iabbrev Π
  "iabbrev ℤ
  iabbrev ⇐ \rightarrow
  iabbrev ⇒ \to
  iabbrev ↦ mapsto
  "iabbrev Θ

  "iabbrev ¬
  iabbrev ∨ \vee
  iabbrev ∧ \wedge
  "iabbrev ⊥
  "iabbrev ∥
  "iabbrev →
  iabbrev ∞ \infty
  "iabbrev ∅
end "}}}

"nnoremap <leader>cl :! runlatex -pdf % > logfile 2>&1 &<CR><CR>
"nnoremap <leader>oe :! llpp %:r.pdf > /dev/null 2>&1 &<CR><CR>
"nnoremap <leader>oa :! llpp *.pdf > /dev/null 2>&1 &<CR><CR>

function! s:SyncTexForward()
  " see ~/bin/myTexWrapper.sh
  exec 'silent !myTexWrapper.sh % '.line('.')." ".col('.')
  redraw!
endfunction
nnoremap <Leader>f :call <SID>SyncTexForward()<CR>

nnoremap <leader>kl :!killall lualatex<cr>
