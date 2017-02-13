{
module Parser (parseInput) where
import Token (Token (..))
import AST (
  Prog (..),
  Type (..),
  Decl (..),
  Stmt (..),
  ElseBlock (..),
  Expr (..)
  )
}
%name parseInput
%tokentype { Token }
%error { parseError }


%token
-- Types
    int { TINT }
    float { TFLOAT }
    string  { TSTRING }
    var    { TVAR }
    while  { TWHILE }
    do { TDO }
    done { TDONE }
    print { TPRINT }
    read { TREAD }
    if { TIF }
    else { TELSE }
    then { TTHEN }
    endif { TENDIF }
    '+' { TADD }
    '-' { TSUB }
    '*' { TMULT }
    '/' { TDIV }
    '=' { TEQUALS }
    '(' { T_OPAREN }
    ')' { T_CPAREN }
    '{' { T_OBRACE }
    '}' { T_CBRACE }
    '"' { T_DQUOTE }
    ';' { T_SCOLON }
    ':' { T_COLON }
    id_val { T_ID_VAL $$ }
    int_val { T_INT_VAL $$ }
    float_val { T_FLOAT_VAL $$ }
    string_val { T_STRING_VAL $$ }

%left '+' '-'
%left '*' '/'
%left UMINUS

%%

Program : Declarations Statements { Prog $1 $2 }

Declarations : Declaration Declarations { $1 : $2 }
      	     | {- empty -} { [] }

Statements : Statement Statements { $1 : $2 }
      	   | {- empty -} { [] } 

VariableType : int { T_Int }
     	     | float { T_Float }
       	     | string { T_String }

Declaration : var id_val ':' VariableType ';' { D_Decl (E_Iden $2) $4 }

Statement : read id_val ';' { S_Read (E_Iden $2) }
	  | print Expression ';' { S_Print $2 }
	  | id_val '=' Expression ';' { S_Assign (E_Iden $1) $3 }
	  | if Expression then Statements ElseBlockFactor { S_If $2 $4 $5 }
	  | while Expression do Statements done { S_While $2 $4 }

ElseBlockFactor : endif { B_Endif }
	        | else Statements endif { B_Else $2 }

Expression : '(' Expression ')' { E_Paren $2 }
	   | '-' Expression %prec UMINUS { E_UMinus $2 }
	   | Expression '+' Expression { E_Add $1 $3 }
	   | Expression '-' Expression { E_Sub $1 $3 }
	   | Expression '*' Expression { E_Mult $1 $3 }
	   | Expression '/' Expression { E_Div $1 $3 }
	   | int_val { E_Int $1 }
	   | float_val { E_Float $1 }
	   | id_val { E_Iden $1 }
	   | string_val { E_String $1 }
{
parseError :: [Token] -> a
parseError _ = error "Parse Error"
}
