```js
ident = r/[a-zA-Z_][a-zA-Z0-9_]*/ ;
number = r/[0-9]+(\.[0-9]+)?/ ;
string = r/"([^"\\]|\\.)*"/ ;

literal = number | string ;


program = expr ;

expr = ident
     | literal
     | lambda
     | lambda
     | block
     | map
     | "[" list "]"
     | unary
     | binary
     | app ;

binary = expr ("+" | "-" | "*" | "/" | "==" | "!=" | "<" | ">" | "<=" | ">=" | ".") expr ;
unary = ("-" | "!" ) expr ;

app = expr expr ;
key = ident | string ;

bind = key "=" expr ";"
     | key ";" ;

lambda = "{" bind* "}" ":" expr ;

block = "(" bind* expr ")" ;
map = "{" bind* "}" ;
list = expr | expr ";" list ;

```
