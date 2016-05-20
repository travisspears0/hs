signs = "abcdefghijklmnoprstuwxyzABCDEFGHIJKLMNOPRSTUWXYZ0123456789!@#$%^&*()`~,./<>?;\\:|'\"[]{}=+-_ "

getPos el list = getPosZero el list 0
getPosZero el (l:list) n = if el == l then n else getPosZero el list (n+1)

encode sign signs key = signs!!(((getPos sign signs) + key) `mod` length signs)
decode sign signs key = signs!!(((getPos sign signs) - key) `mod` length signs)

cezar text key = cezarArgs text signs key []
cezarArgs (x:l) signs key res = cezarArgs l signs key (res++[encode x signs key])
cezarArgs [] _ _ res = res

uncezar text key = uncezarArgs text signs key []
uncezarArgs (x:l) signs key res = uncezarArgs l signs key (res++[decode x signs key])
uncezarArgs [] _ _ res = res

breakCezar (a:_) (b:_) = breakCezarSign a b
breakCezarSign a b = breakCezarKey a b 0
breakCezarKey a b checkedKey = if encode a signs checkedKey == b then checkedKey else breakCezarKey a b (checkedKey+1)
