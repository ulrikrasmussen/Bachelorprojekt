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

syn keyword joinConstant S Z

hi link joinKeyword Keyword
hi link joinConstant Constant
