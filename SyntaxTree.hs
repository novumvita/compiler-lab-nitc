module SyntaxTree where

data Node = NodeConnector [Node]
          | NodeAssg Node Node
          | NodeRead Node
          | NodeWrite Node
          | NodeIf Node Node Node
          | NodeWhile Node Node
          | NodeBreak
          | NodeContinue
          | NodeReturn Node
          | NodeArray String [Node]
          | NodeVar String
          | NodeField Node String
          | NodeOp String Node Node
          | NodeBool String Node Node
          | NodeInt Int 
          | NodeStr String 
          | NodePtr Node
          | NodeRef Node
          | NodeFnCall String [Node]
          | NodeClassFnCall Node [Node]
          | NodeAlloc Node
          | NodeFree Node
          | NodeInit
          | NodeNull
          | NodeSelf
          | NodeNew Node String
          | NodeDelete Node
          deriving Show