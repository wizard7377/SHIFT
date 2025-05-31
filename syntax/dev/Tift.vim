if exists("b:current_syntax")
  finish
endif
syntax region tiftComment start=+//+ end=+$+
syntax region tiftExpr start=+(+ end=+)+
syntax region tiftLamedHead start=+\[+ end=+\]+
syntax region tiftLamedBody start=+{+ end=+}+
syntax match tiftIdent "[^\[\]{}();]+"
hi def link tiftComment Comment
hi def link tiftExpr Statement
hi def link tiftLamedHead Identifier
