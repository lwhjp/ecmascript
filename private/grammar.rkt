#lang ragg

program: (statement | function-declaration)+

;; Expressions

primary-expression: "this" | identifier | numeric | string | "null" | "true" | "false" | "(" expression ")"
identifier: IDENTIFIER
numeric: NUMERIC
string: STRING
member-expression:
    primary-expression |
    member-expression "[" expression "]" |
    member-expression "." identifier |
    "new" member-expression arguments
new-expression:
    member-expression |
    "new" new-expression
call-expression:
    member-expression arguments |
    call-expression arguments |
    call-expression "[" expression "]" |
    call-expression "." identifier
arguments: "(" [ assignment-expression [ "," assignment-expression ]* ] ")"
left-hand-side-expression: new-expression | call-expression
postfix-expression: left-hand-side-expression [ "++" | "--" ]
unary-expression: postfix-expression | ("delete" | "void" | "typeof" | "++" | "--" | "+" | "-" | "~" | "!") unary-expression
multiplicative-expression: [multiplicative-expression ("*" | "/" | "%")] unary-expression
additive-expression: [additive-expression ("+" | "-")] multiplicative-expression
shift-expression: [shift-expression ("<<" | ">>" | ">>>")] additive-expression
relational-expression: [relational-expression ("<" | ">" | "<=" | ">=")] shift-expression
equality-expression: [equality-expression ("==" | "!=")] relational-expression
bitwise-and-expression: [bitwise-and-expression "&"] equality-expression
bitwise-xor-expression: [bitwise-xor-expression "^"] bitwise-and-expression
bitwise-or-expression: [bitwise-or-expression "|"] bitwise-xor-expression
logical-and-expression: [logical-and-expression "&&"] bitwise-or-expression
logical-or-expression: [logical-or-expression "||"] logical-and-expression
conditional-expression: logical-or-expression ["?" assignment-expression ":" assignment-expression]
assignment-expression: conditional-expression | left-hand-side-expression assignment-operator assignment-expression
assignment-operator: "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | ">>>=" | "&=" | "^=" | "|="
expression: [expression ","] assignment-expression

;; Statements

statement: block | variable-statement | empty-statement | expression-statement |
    if-statement | while-statement | for-statement | continue-statement |
    break-statement | return-statement | with-statement
block: "{" statement* "}"
variable-statement: "var" variable-declaration-list ";"
variable-declaration-list: variable-declaration ["," variable-declaration]*
variable-declaration: identifier [initializer]
initializer: "=" expression
empty-statement: ";"
expression-statement: expression ";"
if-statement: "if" "(" expression ")" statement ["else" statement]
while-statement: "while" "(" expression ")" statement
for-statement:
    "for" "(" ("var" variable-declaration-list | [expression]) ";" [expression] ";" [expression] ")" statement |
    "for" "(" ("var" identifier [initializer] | expression) "in" expression ")" statement
continue-statement: "continue" ";"
break-statement: "break" ";"
return-statement: "return" [expression] ";"
with-statement: "with" "(" expression ")" statement

;; Function definition

function-declaration: "function" identifier "(" [formal-parameter-list] ")" block
formal-parameter-list: identifier ("," identifier)*
