"Turn on syntax highlighting"
syntax on

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
set spell
set spelllang=en

"Text wrapping"
set formatoptions-=t
set textwidth=72
