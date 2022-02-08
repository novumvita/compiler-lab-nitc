{
module Grammar(parseTokens) where
import Tokens
import SyntaxTree
import Control.Monad.State
import ParserState
import SymbolTable
import Data.Map
}

%name parse
%tokentype { Token }
%error { parseError }
%monad { State ParserState }

%token
    begin           { TokenBegin }
    end             { TokenEnd }
    read            { TokenRead }
    write           { TokenWrite }
    return          { TokenReturn }
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
    '{'             { TokenLBrace }
    '}'             { TokenRBrace }
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
    main            { TokenMain }


%nonassoc '>' '<' '!' 
%left '+' '-'
%left '*' '/'
%left '='

%%

Program : GDeclBlock FDefBlock MainBlock                                    { ($1, $2, $3) }
        | GDeclBlock MainBlock                                              { ($1, [], $2) }
        | MainBlock                                                         { (4096, [], $1) }

GDeclBlock : decl GDeclList enddecl                                         { % doGDecl $2 }
           | decl enddecl                                                   { 4096 }

GDeclList : GDeclList GDecl                                                 { $2 : $1 }
          | GDecl                                                           { [$1] }

GDecl : Type GIDList ';'                                                    { ($1, $2) }

GIDList : GIDList ',' GID                                                   { $3 : $1 }
        | GID                                                               { [$1] }

GID : ID                                                                    { BaseVar $1 [1] }
    | ID '[' num ']'                                                        { BaseVar $1 [1, $3] }
    | GID '[' num ']'                                                       { let (BaseVar name subsize) = $1 in let size = subsize ++ [$3] in BaseVar name size }
    | ID '(' ParamList ')'                                                  { BaseFunc $1 $3 }


FDefBlock : FDefBlock FDef                                                  { $2 : $1 }
          | FDef                                                            { [$1] }

FDef : Type ID '(' Params ')' '{' LDeclBlock FBody '}'                      { % fnDefTypeCheck $1 $2 $4 $7 $8 }

Params : ParamList                                                          { % doPDecl $1 }

FBody : begin SList RetStmt end                                             { let (NodeConnector s) = $2 in NodeConnector ($3 : s) }
      | begin RetStmt end                                                   { NodeConnector [$2] }

RetStmt : return Exp ';'                                                    { NodeReturn $2 }

ParamList : ParamList ',' Param                                             { $3 : $1 }
          | Param                                                           { [$1] }
          | {- no params -}                                                 { [] }

Param : Type ID                                                             { ($1, $2) }

Type : int                                                                  { "int" }
     | str                                                                  { "str" }

LDeclBlock : decl LDeclList enddecl                                         { % doLDecl $2 }
           | decl enddecl                                                   { % doLDecl [] }
           | {-no local declarations-}                                      { % doLDecl [] }

LDeclList : LDeclList LDecl                                                 { $2 : $1 }
          | LDecl                                                           { [$1] }

LDecl : Type IDList ';'                                                     { ($1, $2) }

IDList : IDList ',' ID                                                      { $3 : $1 }
       | ID                                                                 { [$1] }

ID : var                                                                    { $1 }

SList : SList Stmt                                                          { let (NodeConnector s) = $1 in NodeConnector (s ++ [$2]) }
      | Stmt                                                                { NodeConnector [$1] }

Stmt : Variable '=' Exp ';'                                                 { NodeAssg $1 $3 }
     | read Variable ';'                                                    { NodeRead $2 }
     | read '(' Variable ')' ';'                                            { NodeRead $3 }
     | write Exp ';'                                                        { NodeWrite $2 }
     | if Exp then SList else SList endif ';'                               { NodeIf $2 $4 $6 }
     | if Exp then SList endif ';'                                          { NodeIf $2 $4 (NodeConnector []) }
     | while Exp do SList endwhile ';'                                      { NodeWhile $2 $4 }
     | break ';'                                                            { NodeBreak }
     | continue ';'                                                         { NodeContinue }
     | RetStmt                                                              { $1 }

Variable : Variable '[' Exp ']'                                             { let (NodeArray var indexes) = $1 in (NodeArray var ($3 : indexes)) }
         | var '[' Exp ']'                                                  { NodeArray $1 [$3] }
         | var                                                              { NodeVar $1 }

Function : var '(' ArgList ')'                                              { % fnCallTypeCheck $1 $3 >>= \p -> return (NodeFnCall $1 p) }

ArgList : ArgList ',' Exp                                                   { $3 : $1 }
        | Exp                                                               { [$1] }

Exp : Exp '+' Exp                                                           { NodeOp "+" $1 $3 }
    | Exp '-' Exp                                                           { NodeOp "-" $1 $3 }
    | Exp '*' Exp                                                           { NodeOp "*" $1 $3 }
    | Exp '/' Exp                                                           { NodeOp "/" $1 $3 }
    | Exp '<' Exp                                                           { NodeBool "<" $1 $3 }
    | Exp '>' Exp                                                           { NodeBool ">" $1 $3 }
    | Exp '<' '=' Exp                                                       { NodeBool "<=" $1 $4 }
    | Exp '>' '=' Exp                                                       { NodeBool ">=" $1 $4 }
    | Exp '!' '=' Exp                                                       { NodeBool "!=" $1 $4 }
    | Exp '=' '=' Exp                                                       { NodeBool "==" $1 $4 }
    | '(' Exp ')'                                                           { $2 }
    | num                                                                   { NodeInt $1 }
    | string                                                                { NodeStr $1 }
    | Variable                                                              { $1 }
    | '*' Variable                                                          { NodePtr $2 }
    | '&' Variable                                                          { NodeRef $2 }
    | Function                                                              { $1 }

MainBlock : int main '(' ')' '{' LDeclBlock FBody '}'                       { ($6, $7) }

{

parseError :: [Token] -> a
parseError tokens = error $ "Parse error" ++ show tokens

parseTokens tokenStream = (gSymTable, sp, fDecl, main)
  where
    ((sp, fDecl, main), (gSymTable, _)) = runState (parse tokenStream) startState

type Program = (Int, [FDefinition], (SymbolTable, Node))
}
