%token EOF

%token IF
%token THEN
%token GOTO
%token RET

%token ASSIGN
%token ADDRESS_OF
%token PLUS
%token GREATER_THAN
%token EQUAL
%token LESS_THAN
%token NOT_EQUAL
%token SEMI_COLON
%token COLON
%token COMMA

%token <string> VAR
%token <string> LABEL

%token <int> INT

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET

%{
open Instruction
%}

%start parse_program
%type <program> parse_program

%type <program> program
%type <instruction> instruction
%type <jump_instruction> jump_instruction
%type <basic_instruction> basic_instruction

%type <condition> condition
%type <expression> expression

%%

parse_program:
  program EOF { $1 }

%inline program:
  | instrs = list(instruction) 
    { instrs }


instruction:
  | basic_instr = basic_instruction
    ; SEMI_COLON
      { (basic_instr :> instruction) }
  | jump_instr = jump_instruction
    ; SEMI_COLON
      { (jump_instr :> instruction) }
  | label = LABEL
    ; COLON
      { `Label label }

%inline address_of_rhs:
  | var = VAR
    { `Var var }
  | label = LABEL 
    { `Label label }

basic_instruction:
  | var = VAR
    ; ASSIGN
    ; expr = expression
      { `Assign (var, expr) }
  | var = VAR
    ; ASSIGN
    ; ADDRESS_OF
    ; rhs = address_of_rhs
      { `Address_of (var, rhs) }
  | var = VAR
    ; ASSIGN
    ; LEFT_BRACKET
    ; expr = expression
    ; RIGHT_BRACKET
      { `Deref (var, expr) }
  | LEFT_BRACKET
    ; var = VAR
    ; RIGHT_BRACKET
    ; ASSIGN
    ; expr = expression
      { `Store (expr, var) }

jump_instruction:
  | label = LABEL
    ; LEFT_PAREN
    ; exprs = separated_list(COMMA, expression)
    ; RIGHT_PAREN
      { `Call (label, exprs) }
  | LEFT_BRACKET
    ; var = VAR
    ; RIGHT_BRACKET
    ; LEFT_PAREN
    ; exprs = separated_list(COMMA, expression)
    ; RIGHT_PAREN
      { `Calli (var, exprs) }
  | GOTO
    ; label = LABEL
      { `Jump label }
  | GOTO
    ; LEFT_BRACKET
    ; var = VAR
    ; RIGHT_BRACKET
      { `Jump_indir var }
  | IF
    ; cond = condition
    ; THEN
    ; GOTO
    ; label = LABEL
      { `Cond (cond, label) }
  | RET
      { `Ret }

expression:
  | var = VAR
      { Var var }
  | n = INT
      { Int n }
  | expr1 = expression
    ; PLUS
    ; expr2 = expression
      { Plus (expr1, expr2) }
  | LEFT_PAREN
    ; expr = expression
    ; RIGHT_PAREN
      { expr }

condition:
  | expr1 = expression
    ; op = condition_op
    ; expr2 = expression
      { op expr1 expr2 }

%inline condition_op:
  | EQUAL         { fun expr1 expr2 -> Equal (expr1, expr2) }
  | NOT_EQUAL     { fun expr1 expr2 -> Not_equal (expr1, expr2) }
  | LESS_THAN     { fun expr1 expr2 -> Less_than (expr1, expr2) }
  | GREATER_THAN  { fun expr1 expr2 -> Greater_than (expr1, expr2) }






