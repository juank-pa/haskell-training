module Chapter11.Content.SumOfPoducts where

type AuthorName = String

data Author
  = Fiction AuthorName
  | Nonfiction AuthorName
  deriving Show

data Expr
  = Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr
