{
module Grammar(parseTokens) where
import Tokens
import SyntaxTree
import Control.Monad.State
import ParserState
import SymbolTable
import TypeTable
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
    '.'             { TokenPeriod }
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
    type            { TokenType }
    endtype         { TokenEndType }
    alloc           { TokenAlloc }
    free            { TokenFree }
    initialize      { TokenInit }
    null            { TokenNull }
    class           { TokenClass }
    endclass        { TokenEndClass } 
    extends         { TokenExtends }
    self            { TokenSelf }
    new             { TokenNew }
    delete          { TokenDelete }


%nonassoc '>' '<' '!' 
%left '+' '-'
%left '*' '/'
%left '='

%%

Program : TypeDefBlock ClassDefBlock GDeclBlock FDefBlock MainBlock          { ($1, $2, $3, $4, $5) }
        | TypeDefBlock ClassDefBlock GDeclBlock MainBlock                    { ($1, $2, $3, [], $4) }

        -- TYPEDEF

TypeDefBlock : type TypeDefList endtype                                      { % doTDecl $2 }
             | type endtype                                                  { % doTDecl [] }
             | {- no type definitions -}                                     { % doTDecl [] }

TypeDefList : TypeDefList TypeDef                                            { $2 : $1 }
            | TypeDef                                                        { [$1] }  

TypeDef : ID '{' FieldDeclList '}'                                           { ($1, $3) }

FieldDeclList : FieldDeclList FieldDecl                                      { $2 : $1 }
              | FieldDecl                                                    { [$1] }

FieldDecl : Type ID ';'                                                      { ($2, $1) }

        -- CLASSDEF

ClassDefBlock : class ClassDefList endclass                                  { (unions $ fst $ unzip $2, concat $ snd $ unzip $2) }
              | class endclass                                               { (empty, []) }
              | {- no class definitions -}                                   { (empty, []) } 

ClassDefList : ClassDefList ClassDef                                         { $2 : $1 }
             | ClassDef                                                      { [$1] }

ClassDef : ClassDecl ClassBody                                               { ($1, $2) }

ClassBody : CFDefBlock '}'                                                   { $1 }
          | '}'                                                              { [] }

ClassDecl : ClassName '{' decl CVDeclList CFDeclBlock enddecl                { % doCDecl $1 $4 $5 }
          | ClassName '{' decl CFDeclBlock enddecl                           { % doCDecl $1 [] $4 }
          | ClassName '{' decl CVDeclList enddecl                            { % doCDecl $1 $4 [] }

CFDeclBlock : CFDeclList                                                     { % doGDecl $1 >> return $1 }

ClassName : ID                                                               { ($1, "Null") }
          | ID extends ID                                                    { ($1, $3) }

CVDeclList : CVDeclList CVDecl                                               { $2 : $1 }
           | CVDecl                                                          { [$1] }

CVDecl : Type ID ';'                                                         { ($2, $1) }

CFDeclList : CFDeclList CFDecl                                               { $2 : $1 }
           | CFDecl                                                          { [$1] }

CFDecl : Type ID '(' ParamList ')' ';'                                       { ($1, [BaseFunc $2 $4]) }

        -- GDECL

GDeclBlock : decl GDeclList enddecl                                         { % doGDecl $2 }
           | decl enddecl                                                   { % doGDecl [] }
           | {- no global declarations -}                                   { % doGDecl [] }

GDeclList : GDeclList GDecl                                                 { $2 : $1 }
          | GDecl                                                           { [$1] }

GDecl : Type GIDList ';'                                                    { ($1, $2) }

GIDList : GIDList ',' GID                                                   { $3 : $1 }
        | GID                                                               { [$1] }

GID : ID                                                                    { BaseVar $1 [1] }
    | ID '[' num ']'                                                        { BaseVar $1 [1, $3] }
    | GID '[' num ']'                                                       { let (BaseVar name subsize) = $1 in let size = subsize ++ [$3] in BaseVar name size }
    | ID '(' ParamList ')'                                                  { BaseFunc $1 $3 }


        -- FDEF

FDefBlock : FDefBlock FDef                                                  { $2 : $1 }
          | FDef                                                            { [$1] }

CFDefBlock : CFDefBlock CFDef                                               { $2 : $1 }
           | CFDef                                                          { [$1] }

FDef : Type ID '(' Params ')' '{' LDeclBlock FBody '}'                      { % fnDefTypeCheck $1 $2 $4 $7 $8 }

CFDef : Type ID '(' CParams ')' '{' LDeclBlock FBody '}'                    { % fnDefTypeCheck $1 $2 $4 $7 $8 }

CParams : ParamList                                                         { % doCPDecl $1 }

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
     | ID                                                                   { $1 }

LDeclBlock : decl LDeclList enddecl                                         { % doLDecl $2 }
           | decl enddecl                                                   { % doLDecl [] }
           | {- no local declarations -}                                    { % doLDecl [] }

LDeclList : LDeclList LDecl                                                 { $2 : $1 }
          | LDecl                                                           { [$1] }

LDecl : Type IDList ';'                                                     { ($1, $2) }

IDList : IDList ',' ID                                                      { $3 : $1 }
       | ID                                                                 { [$1] }

ID : var                                                                    { $1 }

SList : SList Stmt                                                          { let (NodeConnector s) = $1 in NodeConnector (s ++ [$2]) }
      | Stmt                                                                { NodeConnector [$1] }

Stmt : Variable '=' Exp ';'                                                 { % assgTypeCheck $1 $3 }
     | Variable '=' alloc '(' ')' ';'                                       { NodeAlloc $1 }
     | Variable '=' initialize '(' ')' ';'                                  { NodeInit }
     | Variable '=' free '(' Variable ')' ';'                               { NodeFree $5 }
     | Variable '=' new '(' Type ')' ';'                                    { % newTypeCheck $1 $5 }
     | delete '(' Variable ')' ';'                                          { NodeDelete $3 }
     | read Variable ';'                                                    { NodeRead $2 }
     | read '(' Variable ')' ';'                                            { NodeRead $3 }
     | write Exp ';'                                                        { NodeWrite $2 }
     | if Exp then SList else SList endif ';'                               { % nodeToType $2 >>= \p -> if p == "bool" then return (NodeIf $2 $4 $6) else typeError "at if." }
     | if Exp then SList endif ';'                                          { % nodeToType $2 >>= \p -> if p == "bool" then return (NodeIf $2 $4 (NodeConnector [])) else typeError "at if." }
     | while Exp do SList endwhile ';'                                      { % nodeToType $2 >>= \p -> if p == "bool" then return (NodeWhile $2 $4) else typeError "at if." }
     | break ';'                                                            { NodeBreak }
     | continue ';'                                                         { NodeContinue }
     | RetStmt                                                              { $1 }

Variable : Field                                                            { $1  }
         | Variable '[' Exp ']'                                             { let (NodeArray var indexes) = $1 in (NodeArray var ($3 : indexes)) }
         | var '[' Exp ']'                                                  { NodeArray $1 [$3] }
         | var                                                              { NodeVar $1 }

Field : ID '.' ID                                                           { NodeField (NodeVar $1) $3 }
      | Field '.' ID                                                        { NodeField $1 $3 }
      | self '.' ID                                                         { NodeField NodeSelf $3 }

Function : var '(' ArgList ')'                                              { % fnCallTypeCheck $1 $3 >>= \p -> return (NodeFnCall $1 p) }
         | Field '(' ArgList ')'                                            { % classFnCallTypeCheck $1 $3 >>= \p -> return (NodeClassFnCall $1 p) }

ArgList : ArgList ',' Exp                                                   { $3 : $1 }
        | Exp                                                               { [$1] }
        | {- no args -}                                                     { [] }

Exp : Exp '+' Exp                                                           { % opTypeCheck "+" $1 $3 }
    | Exp '-' Exp                                                           { % opTypeCheck "-" $1 $3 }
    | Exp '*' Exp                                                           { % opTypeCheck "*" $1 $3 }
    | Exp '/' Exp                                                           { % opTypeCheck "/" $1 $3 }
    | Exp '<' Exp                                                           { % boolTypeCheck "<" $1 $3 }
    | Exp '>' Exp                                                           { % boolTypeCheck ">" $1 $3 }
    | Exp '<' '=' Exp                                                       { % boolTypeCheck "<=" $1 $4 }
    | Exp '>' '=' Exp                                                       { % boolTypeCheck ">=" $1 $4 }
    | Exp '!' '=' Exp                                                       { % boolTypeCheck "!=" $1 $4 }
    | Exp '=' '=' Exp                                                       { % boolTypeCheck "==" $1 $4 }
    | '(' Exp ')'                                                           { $2 }
    | num                                                                   { NodeInt $1 }
    | string                                                                { NodeStr $1 }
    | Variable                                                              { $1 }
    | '*' Variable                                                          { NodePtr $2 }
    | '&' Variable                                                          { NodeRef $2 }
    | Function                                                              { $1 }
    | null                                                                  { NodeNull }

MDeclBlock : decl LDeclList enddecl                                         { % doMDecl $2 }
           | decl enddecl                                                   { % doMDecl [] }
           | {- no local declarations -}                                    { % doMDecl [] }

MainBlock : int main '(' ')' '{' MDeclBlock FBody '}'                       { ($6, $7) }

{

parseError :: [Token] -> a
parseError tokens = error $ "Parse error" ++ show tokens

parseTokens tokenStream = (typeTable, cTable, gSymTable, sp, fDecl, main)
  where
    ((typeTable, cTable, sp, fDecl, main), (gSymTable, _, _, _, _, _)) = runState (parse tokenStream) startState

type Program = (TypeTable, (ClassTable, [FDefinition]), Int, [FDefinition], (SymbolTable, Node))
}
