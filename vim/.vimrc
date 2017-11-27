"Theme"
" colo darkblue

"Numbered lines"
" set number

"Update the window bar"
set title

"Highlight search hits and make the search case insensitive (unless UPPERCASE)"
set incsearch
set hlsearch
set ignorecase
set smartcase

"Turn on syntax highlighting"
syntax on

"Replace TABs with spaces and use 2 spaces for one level of indentation"
set expandtab
set tabstop=2
set shiftwidth=2

"Move the way I want, also with visual lines"
nmap j gj
nmap k gk

"Support basic Emacs shortcuts"
cnoremap <C-a>  <Home>
cnoremap <C-b>  <Left>
cnoremap <C-f>  <Right>
cnoremap <C-d>  <Delete>
cnoremap <C-k>  <C-U>
cnoremap <M-b>  <S-Left>
cnoremap <M-f>  <S-Right>
cnoremap <M-d>  <S-right><Delete>
cnoremap <C-g>  <C-c>

"Interact with vim using ; instead of :"
:nmap ; :

"Highlight trailing white space"
highlight ExtraWhitespace ctermbg=red guibg=red
au ColorScheme * highlight ExtraWhitespace guibg=red
au BufEnter * match ExtraWhitespace /\s\+$/
au InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
au InsertLeave * match ExtraWhiteSpace /\s\+$/

"Strip trailing white space on save"
function! TrimWhiteSpace()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfunction
autocmd BufWritePre *.* :call TrimWhiteSpace()

"Spelling, enable for certain files only"
"  set spell
set spelllang=en
autocmd BufRead,BufNewFile *.md setlocal spell
autocmd BufRead,BufNewFile *.txt setlocal spell
autocmd BufRead,BufNewFile COMMIT_EDITMSG setlocal spell

"Text wrapping"
set formatoptions=t
set textwidth=72

"Indent like the previous line"
set smartindent

"Detect file type and turn on indentation"
"Makes XML indentation work"
filetype plugin indent on

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬

" Can't believe vim doesn't have capitialisation by default
" gcw        - capitalize word (from cursor position to end of word)
" gcW        - capitalize WORD (from cursor position to end of WORD)
" gciw       - capitalize inner word (from start to end)
" gciW       - capitalize inner WORD (from start to end)
" gcis       - capitalize inner sentence
" gc$        - capitalize until end of line (from cursor postition)
" gcgc       - capitalize whole line (from start to end)
" gcc        - capitalize whole line
" {Visual}gc - capitalize highlighted text
if (&tildeop)
  nmap gcw guw~l
  nmap gcW guW~l
  nmap gciw guiw~l
  nmap gciW guiW~l
  nmap gcis guis~l
  nmap gc$ gu$~l
  nmap gcgc guu~l
  nmap gcc guu~l
  vmap gc gu~l
else
  nmap gcw guw~h
  nmap gcW guW~h
  nmap gciw guiw~h
  nmap gciW guiW~h
  nmap gcis guis~h
  nmap gc$ gu$~h
  nmap gcgc guu~h
  nmap gcc guu~h
  vmap gc gu~h
endif
