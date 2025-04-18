lexer grammar HlasmLexer;


// =========================================================
// WHITESPACE AND COMMENTS (skipped during parsing)
// =========================================================

// Whitespace: spaces, tabs, carriage returns, and line feeds
WHITESPACE
    : [ \t\r\n]+ -> skip
    ;

// Single-line comment: starts with // and continues until end of line
LINE_COMMENT
    : '//' ~[\r\n]* -> skip
    ;

// Multi-line comment: enclosed between /* and */
BLOCK_COMMENT
    : '/*' .*? '*/' -> skip
    ;

fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');
fragment ZERO: '0';
fragment ONE: '1';
fragment TWO: '2';
EQUALS: '=';
PLUS: '+';
MINUS: '-';
ASTERISK: '*';
COMMA: ',';
OPEN_PAREN: '(';
CLOSE_PAREN: ')';

// =========================================================
// IDENTIFIERS AND LITERALS
// =========================================================

//CHARACTER_LITERAL
//    : [a-zA-Z_ ][a-zA-Z_0-9 ]*
//    ;

// Identifier: starts with letter or underscore, followed by letters, digits, or underscores
IDENTIFIER
    : [a-zA-Z_][a-zA-Z_0-9]*
    ;

SINGLE_QUOTE: '\'';

// Integer literal: sequence of digits
INTEGER_LITERAL
    : [0-9]+
    ;

INTEGER_LITERAL_STRING: SINGLE_QUOTE (INTEGER_LITERAL | NEGATIVE_INTEGER_LITERAL) SINGLE_QUOTE;

LENGTH_SPECIFIED_LITERAL: C L INTEGER_LITERAL STRING_LITERAL;
UNPADDED_LITERAL: C STRING_LITERAL;

NEGATIVE_INTEGER_LITERAL : '-' INTEGER_LITERAL;

HEX_LITERAL : [0-9a-fA-F]+;

// String literal: text enclosed in single quotes, with proper escape handling
STRING_LITERAL
    : SINGLE_QUOTE ( ~["\r\n\\] | '\\' . )* SINGLE_QUOTE
    ;

