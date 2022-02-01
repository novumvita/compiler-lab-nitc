{
module Grammar where
import Tokens
}

%name parseCalc
%tokentype { Token }
%error { parseError }

%token
    begin           { TokenBegin }
    end             { TokenEnd }
    read            { TokenRead }
    write           { TokenWrite }
    num             { TokenDig $$ }
    var             { TokenID $$ }
    '='             { TokenEq }
    '+'             { TokenPlus }
    '-'             { TokenMinus }
    '*'             { TokenTimes }
    '/'             { TokenDiv }
    '('             { TokenLParen }
    ')'             { TokenRParen }
    ';'             { TokenSemiColon }
    '<'             { TokenLT }
    '>'             { TokenGT }
    '!'             { TokenNot }
    '['             { TokenLSquare }
    ']'             { TokenRSquare }
    if              { TokenIf }
    then            { TokenThen }   
    else            { TokenElse }
    endif           { TokenEndIf }
    while           { TokenWhile }
    do              { TokenDo }
    endwhile        { TokenEndWhile }
    break           { TokenBreak }
    continue        { TokenContinue }
    decl            { TokenDecl }
    enddecl         { TokenEndDecl }
    int             { TokenTypeInt }
    str             { TokenTypeString }
    ','             { TokenComma }
    string          { TokenString $$ }
    '&'             { TokenAnd }


%nonassoc '>' '<' '!' 
%left '+' '-'
%left '*' '/'
%left '='

%%

Program : begin SList end ';'                      { ProgramStart $2 }
        | begin end ';'                            { EmptyProgram }

SList : SList Stmt                                 { $2 : $1 }
      | Stmt                                       { [$1] }

Stmt : Exp '=' Exp ';'                             { Assg $1 $3 }
     | read Exp ';'                                { Read $2 }
     | write Exp ';'                               { Write $2 }
     | if Exp then SList else SList endif ';'      { IfElse $2 $4 $6 }
     | if Exp then SList endif ';'                 { If $2 $4 }
     | while Exp do SList endwhile ';'             { While $2 $4 }
     | break ';'                                   { Break }
     | continue ';'                                { Continue }
     | Declarations ';'                            { DeclStmt $1 }

Declarations : decl DeclList enddecl               { Declarations $2 }
             | decl enddecl                        { EmptyDecl }

DeclList : DeclList DeclList                       { $2 ++ $1 } 
         | Type VarList ';'                        { map (Decl $1) $2 }
         | Decl                                    { [$1] }

Decl : Type VarDecl ';'                            { Decl $1 $2 }

Type : int                                         { "INT" }
     | str                                         { "STR" }

VarList : VarList ',' VarDecl                      { $3 : $1 }
        | VarDecl                                  { [$1] }

VarDecl : VarDecl '[' num ']'                      { VarArray $1 $3 }
        | '*' var                                  { PointerDecl $2 }
        | var '[' num ']'                          { VarArray (VarDecl $1) $3 }
        | var                                      { VarDecl $1 }

Variable : Variable '[' Exp ']'                    { VarExpArray $1 $3 }
         | var '[' Exp ']'                         { VarExpArray (VarExp $1) $3 }
         | var                                     { VarExp $1 }

Exp : Exp '+' Exp                                  { Plus $1 $3 }
    | Exp '-' Exp                                  { Minus $1 $3 }
    | Exp '*' Exp                                  { Times $1 $3 }
    | Exp '/' Exp                                  { Div $1 $3 }
    | Exp '<' Exp                                  { LessThan $1 $3 }
    | Exp '>' Exp                                  { GreaterThan $1 $3 }
    | Exp '<' '=' Exp                              { LessThanEq $1 $4 }
    | Exp '>' '=' Exp                              { GreaterThanEq $1 $4 }
    | Exp '!' '=' Exp                              { NotEq $1 $4 }
    | Exp '=' '=' Exp                              { Eq $1 $4 }
    | '(' Exp ')'                                  { $2 }
    | num                                          { Num $1 }
    | string                                       { Str $1 }
    | Variable                                     { Val $1 }
    | '&' Variable                                 { Mem $2 }
    | '*' Variable                                 { Pointer $2 }

{

parseError :: [Token] -> a
parseError tokens = error $ "Parse error" ++ show tokens

data Exp = Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Num Int
         | Val Variable
         | Str String
         | Mem Variable
         | Pointer Variable
         | LessThan Exp Exp
         | GreaterThan Exp Exp
         | LessThanEq Exp Exp
         | GreaterThanEq Exp Exp
         | NotEq Exp Exp
         | Eq Exp Exp
         deriving Show

data Stmt = Assg Exp Exp
          | Read Exp
          | Write Exp
          | IfElse Exp SList SList
          | If Exp SList
          | While Exp SList
          | Break
          | Continue
          | DeclStmt Declarations
          deriving Show

type SList =  [Stmt]

data Program = ProgramStart SList
             | EmptyProgram
             deriving Show

data Declarations = Declarations DeclList
                  | EmptyDecl
                  deriving Show

type DeclList = [Decl]

data Decl = Decl Type VarDecl
          deriving Show

data VarDecl = VarDecl String
             | PointerDecl String
             | VarArray VarDecl Int
              deriving Show

type Type = String

type VarList = [VarDecl] 

data Variable = VarExpArray Variable Exp
              | VarExp String
              deriving Show

}
