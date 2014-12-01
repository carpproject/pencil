/*
 * Copyright (c) 2013-2014, ARM Limited
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

grammar pencil;

options {
  output=AST;
  ASTLabelType=CommonTree;
  memoize=true;
  backtrack=true;
  k=2;
}

tokens
{
  PLUS='+';
  MINUS='-';
  MULT='*';
  DIV='/';
  MOD='%';

  LOR='||';
  LAND='&&';
  LNOT='!';

  QUESTION='?';
  DOT='.';

  BITOR='|';
  BITAND='&';
  BITNEG='~';
  BITXOR='^';
  BITLSHIFT='<<';
  BITRSHIFT='>>';

  GREATER='>';
  GEQ='>=';
  LESS='<';
  LEQ='<=';
  EQ='==';
  NEQ='!=';

  MOV='=';
  MOVPLUS='+=';
  MOVMINUS='-=';
  MOVMULT='*=';
  MOVDIV='/=';
  MOVMOD='%=';
  MOVLSHIFT='<<=';
  MOVRSHIFT='>>=';
  MOVLOR='|=';
  MOVLAND='&=';
  MOVLXOR='^=';

  PLUSPLUS='++';
  MINUSMINUS='--';

  ARRAY_LEFT='[';
  ARRAY_RIGHT=']';

  BLOCK_LEFT='{';
  BLOCK_RIGHT='}';

  BRACKET_LEFT='(';
  BRACKET_RIGHT=')';

  COMMA=',';
  SEMICOLON=';';
  COLON=':';
  PRAGMA_PREFIX='#';

  SIZEOF='sizeof';
  TYPEDEF='typedef';
  STRUCT='struct';
  IF='if';
  ELSE='else';
  FOR='for';
  WHILE='while';
  BREAK='break';
  CONTINUE='continue';
  RETURN='return';
  ATTRIBUTE='__attribute__';
  PRAGMA='pragma';
  PENCIL='pencil';
  IVDEP='ivdep';
  INDEPENDENT='independent';
  ACCESS='access';
  PENCIL_ACCESS='pencil_access';
  PROGRAM;
  FUNCTION;
  FUNCTION_ARGS;
  FUNCTION_ATTRS;
  FOR_ATTRS;
  TYPE;
  BLOCK;
  EMPTY_BODY;
  EMPTY_NAME_LIST;
  LABEL;
  NAMES;
  RANGE;
  INITIAL;
  GUARD;
  STEP;
  CAST;
  UMINIS;
  CONST_FUNCTION;
  PENCIL_FUNCTION;
  ACCESS_FUNCTION;
  INLINE_STRUCT_TYPE;
  CALL;
  ARRAY_SUBS;
  STRUCT_SUBS;
  DECL;
  DECL_AND_INIT;
  POINTER;
  DECLARATOR;
  DIRECT_DECLARATOR;
  INDIRECT_DECLARATOR;
  EMPTY_STATEMENT;
  ARRAY_TYPE;
  ARRAY_TYPE_ATTRIBUTES;
  STRUCT_FIELDS;
  BUILTIN_TYPE;
  USER_TYPE;
  USER_STRUCT_TYPE;
  EXPRESSION_STATEMENT;
  ANNOTATED_STATEMENT;
  TYPE_VOID='void';
  TYPE_SHORT='short';
  TYPE_INT='int';
  TYPE_BOOL='_Bool';
  TYPE_LONG='long';
  TYPE_FLOAT='float';
  TYPE_DOUBLE='double';
  TYPE_SIGNED='signed';
  TYPE_UNSIGNED='unsigned';
  TYPE_CONST='const';
  TYPE_RESTRICT='restrict';
  TYPE_CHAR='char';
  TYPE_HALF='half';
  TYPE_STATIC='static';
  MODIFY;
  ARRAY_INIT;
  SCALAR_INIT;
  TERNARY;
  SCOP='scop';
  ENDSCOP='endscop';
  TRUE='true';
  FALSE='false';
  BUILTIN_FUNCTION;
  PENCIL_USE='__pencil_use';
  PENCIL_DEF='__pencil_def';
  PENCIL_MAYBE='__pencil_maybe';
  PENCIL_KILL='__pencil_kill';
  PENCIL_ASSUME='__pencil_assume';
}

@parser::header {
  package com.arm.carp.pencil.parser;

}

@lexer::header {
  package com.arm.carp.pencil.parser;
}

@parser::members {
  private boolean correct = true;

  public boolean isCorrect() {
    return correct;
  }

  public void displayRecognitionError(String[] tokenNames, RecognitionException e) {
    String hdr = getErrorHeader(e);
    String msg = getErrorMessage(e, tokenNames);
    System.err.println (hdr + " " + msg);
    correct = false;
  }
}

// Skip whitespaces
WS: (' '|'\r'|'\t'|'\u000C'|'\n') {skip();};

//C like comments
COMMENT:'/*' ( options {greedy=false;} : . )* '*/' {skip();};

//C++ like comments
LINE_COMMENT:'//' ~('\n'|'\r')* '\r'? '\n' {skip();};

// Lexer

fragment LETTER: '$' | 'A'..'Z' | 'a'..'z' | '_';
fragment DIGIT: '0'..'9';
fragment HEX_DIGIT: (DIGIT|'a'..'f'|'A'..'F');
fragment EXPONENT:('e'|'E')('+'|'-'|)DIGIT+;

NAME: LETTER (LETTER|'0'..'9')*;

HEX_NUMBER: '0' ('x'|'X') HEX_DIGIT+;

NUMBER: ('0' | '1'..'9' DIGIT*);

OCTAL_NUMBER: '0' ('0'..'7')+;

DOUBLE_NUMBER: DIGIT* '.' DIGIT+ EXPONENT?| DIGIT EXPONENT;
FLOAT_NUMBER: DOUBLE_NUMBER 'f';

//Parser

program: top_level_def* EOF -> ^(PROGRAM top_level_def*);

top_level_def: type_definition | function | global_constant;

type_specifier:
TYPE_SHORT
| TYPE_INT
| TYPE_BOOL
| TYPE_LONG
| TYPE_FLOAT
| TYPE_HALF
| TYPE_CHAR
| TYPE_DOUBLE
| TYPE_SIGNED
| TYPE_UNSIGNED;

global_constant: variable_decl_init SEMICOLON -> variable_decl_init;

type_attributes: TYPE_CONST;
array_type_attributes: TYPE_RESTRICT | TYPE_STATIC | type_attributes;

type_definition: struct_type_definition SEMICOLON -> struct_type_definition| ptypedef SEMICOLON -> ptypedef;

struct_type_definition: STRUCT NAME? BLOCK_LEFT (variable_decl_int SEMICOLON)* BLOCK_RIGHT
  -> ^(STRUCT NAME? ^(STRUCT_FIELDS variable_decl_int*));

array_type_suffix: ARRAY_LEFT array_type_attributes* expression ARRAY_RIGHT
  -> ^(ARRAY_TYPE expression ^(ARRAY_TYPE_ATTRIBUTES array_type_attributes*));

pointer: '*' ;

declarator        : pointer* direct_declarator -> ^(DECLARATOR pointer* direct_declarator);

direct_declarator : NAME array_type_suffix*               -> ^(DIRECT_DECLARATOR NAME  array_type_suffix*)
                  | '(' declarator ')' array_type_suffix* -> ^(INDIRECT_DECLARATOR declarator array_type_suffix* );

variable_decl_int: base_type declarator                   -> ^(DECL base_type declarator);

variable_decl_init: variable_decl_int MOV init_expression -> ^(DECL_AND_INIT variable_decl_int init_expression);

init_expression: expression -> ^(SCALAR_INIT expression) | array_init_expression -> array_init_expression;

array_init_expression: BLOCK_LEFT BLOCK_RIGHT -> ^(ARRAY_INIT)
                      |BLOCK_LEFT array_init_expression (COMMA array_init_expression)* BLOCK_RIGHT ->
                        ^(ARRAY_INIT array_init_expression+)
                      |constant -> constant;


variable_decl: variable_decl_int SEMICOLON -> variable_decl_int
              |variable_decl_init SEMICOLON -> variable_decl_init;

ptypedef: TYPEDEF base_type NAME array_type_suffix* -> ^(TYPEDEF NAME ^(TYPE base_type array_type_suffix*));

ptype: base_type array_type_suffix* -> ^(TYPE base_type array_type_suffix*);

scalar_type_fragment: type_specifier | type_attributes;

base_type: type_attributes* NAME -> ^(USER_TYPE NAME type_attributes*)
            | type_attributes* STRUCT NAME -> ^(USER_STRUCT_TYPE NAME type_attributes*)
            | scalar_type_fragment+ -> ^(BUILTIN_TYPE scalar_type_fragment+)
            | struct_type_definition -> ^(INLINE_STRUCT_TYPE struct_type_definition);


function_args: -> ^(FUNCTION_ARGS)
  | (variable_decl_int (COMMA variable_decl_int)*) -> ^(FUNCTION_ARGS variable_decl_int+);

function_body: block_statement | SEMICOLON -> ^(EMPTY_BODY);

function_type: base_type -> ^(TYPE base_type) | TYPE_VOID -> ^(TYPE_VOID);

function:
TYPE_STATIC? function_type NAME BRACKET_LEFT function_args BRACKET_RIGHT attribute* function_body
  -> ^(FUNCTION function_type NAME function_args ^(FUNCTION_ATTRS TYPE_STATIC? attribute*) function_body);

attr: TYPE_CONST -> ^(CONST_FUNCTION)
      | PENCIL_ACCESS BRACKET_LEFT NAME BRACKET_RIGHT -> ^(ACCESS_FUNCTION NAME);

attribute:(ATTRIBUTE BRACKET_LEFT BRACKET_LEFT attr BRACKET_RIGHT BRACKET_RIGHT) -> attr;

block: block_statement | labeled_statement  -> ^(BLOCK labeled_statement);

block_statement: BLOCK_LEFT scop* BLOCK_RIGHT -> ^(BLOCK scop*);

scop: PRAGMA_PREFIX PRAGMA SCOP annotated_statement PRAGMA_PREFIX PRAGMA ENDSCOP
        -> ^(SCOP annotated_statement)
     | annotated_statement -> annotated_statement;

statement_access_pragma: PRAGMA_PREFIX PRAGMA PENCIL ACCESS block -> block;

annotated_statement: labeled_statement -> labeled_statement
                    | block -> block
                    | statement_access_pragma labeled_statement
                      -> ^(ANNOTATED_STATEMENT statement_access_pragma labeled_statement)
                    | statement_access_pragma block
                      -> ^(ANNOTATED_STATEMENT statement_access_pragma block);



labeled_statement: label COLON statement -> ^(LABEL label statement) | statement -> statement;

label: NAME;

statement: (modify SEMICOLON) -> modify| pfor | pwhile | pif | pbreak | pcontinue | preturn | variable_decl | expression_statement | empty_statement;

empty_statement: SEMICOLON -> ^(EMPTY_STATEMENT);

name_list: NAME (COMMA NAME)* -> ^(NAMES NAME+);

for_directive: (PRAGMA_PREFIX PRAGMA PENCIL ->) (ivdep_pragma -> ivdep_pragma
                                      |independent_pragma -> independent_pragma);

ivdep_pragma: IVDEP -> ^(IVDEP);

independent_pragma:
  INDEPENDENT -> ^(INDEPENDENT)
  | INDEPENDENT BRACKET_LEFT name_list BRACKET_RIGHT -> ^(INDEPENDENT name_list);

pfor_guard_op: GREATER|LESS|LEQ|GEQ;

pfor_step: PLUSPLUS NAME -> ^(MOVPLUS NAME NUMBER["1"])
          | MINUSMINUS NAME -> ^(MOVMINUS NAME NUMBER["1"])
          | NAME PLUSPLUS -> ^(MOVPLUS NAME NUMBER["1"])
          | NAME MINUSMINUS -> ^(MOVMINUS NAME NUMBER["1"])
          | NAME MOVPLUS constant -> ^(MOVPLUS NAME constant)
          | NAME MOVMINUS constant -> ^(MOVMINUS NAME constant);

pfor:
  for_directive* FOR
    BRACKET_LEFT base_type NAME MOV expression SEMICOLON
    NAME pfor_guard_op expression SEMICOLON
    pfor_step BRACKET_RIGHT block
    -> ^(FOR ^(FOR_ATTRS for_directive*)
              ^(RANGE ^(INITIAL NAME expression base_type)
                      ^(GUARD NAME pfor_guard_op expression)
                      ^(STEP pfor_step)) block);

pwhile: WHILE BRACKET_LEFT expression BRACKET_RIGHT block -> ^(WHILE expression block);

pif: IF BRACKET_LEFT expression BRACKET_RIGHT block ELSE block -> ^(IF expression block block)
     | IF BRACKET_LEFT expression BRACKET_RIGHT block -> ^(IF expression block);

pbreak: BREAK SEMICOLON -> ^(BREAK);
preturn: RETURN expression? SEMICOLON -> ^(RETURN expression?);
pcontinue: CONTINUE SEMICOLON -> ^(CONTINUE);

modify: assignment -> ^(MODIFY assignment);

expression_statement: call_expression SEMICOLON -> ^(EXPRESSION_STATEMENT call_expression) | built_in SEMICOLON -> ^(EXPRESSION_STATEMENT built_in);

built_in: pencil_use | pencil_def | pencil_kill | pencil_assume;

pencil_use: PENCIL_USE BRACKET_LEFT pencil_term BRACKET_RIGHT -> ^(PENCIL_USE pencil_term);
pencil_def: PENCIL_DEF BRACKET_LEFT pencil_term BRACKET_RIGHT -> ^(PENCIL_DEF pencil_term);
pencil_kill: PENCIL_KILL BRACKET_LEFT pencil_term BRACKET_RIGHT -> ^(PENCIL_KILL pencil_term);
pencil_assume: PENCIL_ASSUME BRACKET_LEFT expression BRACKET_RIGHT -> ^(PENCIL_ASSUME expression);

pencil_term:(NAME -> NAME) (ARRAY_LEFT expression ARRAY_RIGHT -> ^(ARRAY_SUBS $pencil_term expression))*;

assignment:
  (lvalue
  (((
     MOV^
     |MOVPLUS^
     |MOVMINUS^
     |MOVMOD^
     |MOVMULT^
     |MOVDIV^
     |MOVLXOR^
     |MOVLAND^
     |MOVLOR^
     |MOVLSHIFT^
     |MOVRSHIFT^) expression)
 | PLUSPLUS^
 | MINUSMINUS^))
 | PLUSPLUS^ lvalue
 | MINUSMINUS^ lvalue;

expression: ternary_expression;

ternary_expression: lor_expression (QUESTION expression COLON ternary_expression) ->
                      ^(TERNARY lor_expression expression ternary_expression)
                    |lor_expression;

lor_expression: land_expression (LOR^ land_expression)*;

land_expression: bitor_expression (LAND^ bitor_expression)*;

bitor_expression: bitxor_expression (BITOR^ bitxor_expression)*;

bitxor_expression: bitand_expression (BITXOR^ bitand_expression)*;

bitand_expression: eq_expression (BITAND^ eq_expression)*;

eq_expression: cmp_expression ((EQ^|NEQ^) cmp_expression)*;

cmp_expression: shift_expression ((GREATER^|LESS^|LEQ^|GEQ^) shift_expression)*;

shift_expression: plus_expression ((BITLSHIFT^|BITRSHIFT^) plus_expression)*;

plus_expression: mult_expression ((PLUS^|MINUS^) mult_expression)*;

mult_expression: cast_expression ((DIV^|MULT^|MOD^) cast_expression)*;

cast_expression: BRACKET_LEFT base_type BRACKET_RIGHT cast_expression  ->
                      ^(CAST base_type cast_expression)
                  | unary_expression -> unary_expression;

sizeof_expression: SIZEOF BRACKET_LEFT ptype BRACKET_RIGHT -> ^(SIZEOF ptype)
                   |SIZEOF BRACKET_LEFT unary_expression BRACKET_RIGHT -> ^(SIZEOF unary_expression);

unary_expression:
LNOT^ cast_expression |
MINUS cast_expression -> ^(UMINIS cast_expression)|
PLUS cast_expression -> cast_expression|
BITNEG^ cast_expression |
sizeof_expression|
postfix_expression;

lvalue: subscription;

postfix_expression: subscription;

call_expression: NAME BRACKET_LEFT BRACKET_RIGHT -> ^(CALL NAME)
                    | NAME BRACKET_LEFT expression (COMMA expression)* BRACKET_RIGHT
                      -> ^(CALL NAME expression+);

subscription: (term -> term) (
                  ARRAY_LEFT expression ARRAY_RIGHT
                  -> ^(ARRAY_SUBS $subscription expression)|
                  DOT NAME -> ^(STRUCT_SUBS $subscription NAME))*;

term: NAME | constant | BRACKET_LEFT expression BRACKET_RIGHT -> expression | call_expression
    | PENCIL_MAYBE BRACKET_LEFT BRACKET_RIGHT
      -> ^(PENCIL_MAYBE);

constant: HEX_NUMBER | NUMBER | DOUBLE_NUMBER | FLOAT_NUMBER | TRUE | FALSE;

