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
          | NodeOp String Node Node
          | NodeBool String Node Node
          | NodeInt Int 
          | NodeStr String 
          | NodePtr Node
          | NodeRef Node
          | NodeFnCall String [Node]
          deriving Show