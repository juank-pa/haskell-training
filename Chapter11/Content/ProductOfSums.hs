module Chapter11.Content.ProductOfSums where

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show

type AuthorName = String

data Author = Author (AuthorName, BookType) deriving Show
