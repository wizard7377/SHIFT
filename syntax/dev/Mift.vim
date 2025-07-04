if exists("b:current_syntax")
  finish
endif
syntax match miftLamed /\$?/
syntax match miftVoid /\$!/
syntax match miftWild /\$_/
syntax match miftLabel /\$:/
syntax match miftIdent /\$\@!\S\+/
syntax match miftDelimA /$(/
syntax match miftDelimB /$)/
hi def link miftLamed Keyword
hi def link miftVoid Error
hi def link miftWild Comment
hi def link miftLabel Label
hi def link miftIdent Identifier
hi def link miftDelimA Delimiter
hi def link miftDelimB Delimiter
