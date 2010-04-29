" Vim syntax file
" Language  : Join

syntax clear

syn case match
syn sync minlines=50

syn keyword joinKeyword def in match with or to return run do let
syn match joinKeyword "|>"
syn match joinKeyword "->"
syn match joinKeyword "&"
syn match joinKeyword "|[^>]"

syn match joinConstant "[A-Z][a-zA-Z0-9']*"

hi link joinKeyword Keyword
hi link joinConstant Constant
