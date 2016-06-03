do
	expr1
	expr2
	expr3

--to samo co

expr1 >> expr2
>>expr 3
---
do
	p <- expr1
	expr2(p)

--to samo co

expr1 >>= \p -> expr(p)