" Vim syntax file
" Language: Lizp
"

"  Load Once:
" if exists("b:current_syntax")
"  finish
" endif

set iskeyword=a-z,A-Z,+,-,*,/,_,!,@,#,\',\?,=,<,>,\\

syntax keyword lizpSpecial def! let* fn* cond do quote '

syntax keyword lizpSpecialSymbol #t #f nil

syntax keyword lizpFunction + - * / = list? int? empty? count > < >= <= eval list
syntax keyword lizpFunction println pr-str prn str concat

syntax region lizpNumber start=/\s\d/ skip=/\d/ end=/\D/

syntax region lizpCommentLine start=";" end="$"

" String literals
syntax region lizpString start=/\v"/ skip=/\v\\./ end=/\v"/ contains=lizpEscapes
syntax match lizpEscapes display contained "\\[nrt\"']"

highlight default link lizpSpecial Keyword
highlight default link lizpSpecialSymbol Keyword
highlight default link lizpFunction Type
highlight default link lizpNumber Number
highlight default link lizpCommentLine Comment
highlight default link lizpString String

let b:current_syntax = "lizp"
