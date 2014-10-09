#lang ragg

program: (ws statement | ws function-declaration)+ ws

ws: (COMMENT | EOL | WS)*
ws-no-eol: (COMMENT | WS)*
eol-or-semicolon: EOL | ";"

;; Expressions

primary-expression: "this" | identifier | numeric | string | "null" | "true" | "false" | "(" ws expression ws ")"
identifier: IDENTIFIER
numeric: NUMERIC
string: STRING
member-expression:
    primary-expression |
    member-expression ws "[" ws expression ws "]" |
    member-expression ws "." ws identifier |
    "new" ws member-expression ws arguments
new-expression:
    member-expression |
    "new" ws new-expression
call-expression:
    member-expression ws arguments |
    call-expression ws arguments |
    call-expression ws "[" ws expression ws "]" |
    call-expression ws "." ws identifier
arguments: "(" [ ws assignment-expression [ ws "," ws assignment-expression ]* ] ws ")"
left-hand-side-expression: new-expression | call-expression
postfix-expression: left-hand-side-expression ws-no-eol [ "++" | "--" ]
unary-expression: postfix-expression | ("delete" | "void" | "typeof" | "++" | "--" | "+" | "-" | "~" | "!") ws unary-expression
multiplicative-expression: [multiplicative-expression ws ("*" | "/" | "%") ws] unary-expression
additive-expression: [additive-expression ws ("+" | "-") ws] multiplicative-expression
shift-expression: [shift-expression ws ("<<" | ">>" | ">>>") ws] additive-expression
relational-expression: [relational-expression ws ("<" | ">" | "<=" | ">=") ws] shift-expression
equality-expression: [equality-expression ws ("==" | "!=") ws] relational-expression
bitwise-and-expression: [bitwise-and-expression ws "&" ws] equality-expression
bitwise-xor-expression: [bitwise-xor-expression ws "^" ws] bitwise-and-expression
bitwise-or-expression: [bitwise-or-expression ws "|" ws] bitwise-xor-expression
logical-and-expression: [logical-and-expression ws "&&" ws] bitwise-or-expression
logical-or-expression: [logical-or-expression ws "||" ws] logical-and-expression
conditional-expression: logical-or-expression [ ws "?" ws assignment-expression ws ":" ws assignment-expression]
assignment-expression: conditional-expression | left-hand-side-expression ws assignment-operator ws assignment-expression
assignment-operator: "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | ">>>=" | "&=" | "^=" | "|="
expression: [expression ws "," ws] assignment-expression

;; Statements

statement: block | variable-statement | empty-statement | expression-statement |
    if-statement | while-statement | for-statement | continue-statement |
    break-statement | return-statement | with-statement
block: "{" [ws statement]* ws "}"
variable-statement: "var" ws variable-declaration-list eol-or-semicolon
variable-declaration-list: variable-declaration [ws "," ws variable-declaration]*
variable-declaration: identifier [ws initializer]
initializer: "=" ws expression
empty-statement: ";"
expression-statement: expression eol-or-semicolon
if-statement: "if" ws "(" ws expression ws ")" ws statement [ws "else" ws statement]
while-statement: "while" ws "(" ws expression ws ")" ws statement
for-statement:
    "for" ws "(" ws ("var" ws variable-declaration-list | [expression]) ws ";" [ws expression] ws ";" [ws expression] ws ")" ws statement |
    "for" ws "(" ws ("var" ws identifier [ws initializer] | expression) ws "in" ws expression ws ")" ws statement
continue-statement: "continue" eol-or-semicolon
break-statement: "break" eol-or-semicolon
return-statement: "return" ws-no-eol [expression] eol-or-semicolon
with-statement: "with" ws "(" ws expression ws ")" ws statement

;; Function definition

function-declaration: "function" ws identifier ws "(" ws formal-parameter-list ws ")" ws block
formal-parameter-list: identifier (ws "," ws identifier)*
