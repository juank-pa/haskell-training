module Chapter11.Content.StricterSumOfProducts where

type Number = Int
type Add = (Expr, Expr)
type Minus = Expr
type Mult = (Expr, Expr)
type Divide = (Expr, Expr)

type Expr =
  Either Number
    (Either Add
      (Either Minus
        (Either Mult Divide)))

-- Produces error: cycle in type synonym declarations
