%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token TRUE
%token FALSE
%token NULL
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token COLON
%token COMMA
%token EOF

%start <OcamlJson.json option> prog

%%

prog:
  | EOF { None }
  | v = value { Some v }
  ;

value:
  | LEFT_BRACE; obj = object_fields; RIGHT_BRACE
    { `Object obj }
  | LEFT_BRACK; arr = array_values; RIGHT_BRACK
    { `Array arr }
  | s = STRING
    { `String s }
  | i = INT
    { `Int i }
  | f = FLOAT
    { `Float f }
  | TRUE
    { `Bool true }
  | FALSE
    { `Bool false }
  | NULL
    { `Null }
  ;

object_fields:
  | (* empty *) { [] }
  | obj = rev_object_fields { List.rev obj }
  ;

rev_object_fields:
  | k = ID; COLON; v = value
    { [(k, v)] }
  | obj = rev_object_fields; COMMA; k = ID; COLON; v = value
    { (k, v) :: obj }
  ;