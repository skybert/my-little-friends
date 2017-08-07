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

"Spelling"
"  set spell
"  set spelllang=en

"Text wrapping"
set formatoptions=t
set textwidth=72

"Indent like the previous line"
set smartindent

