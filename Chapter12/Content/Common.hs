module Chapter12.Content.Common where

-- See Chapter12/Content/EvenAdd1.hs
-- See Chapter12/Content/EvenAdd2.hs

-- See Chapter12/Content/Maybe.hs
-- See Chapter12/Content/Either1.hs
-- See Chapter12/Content/Either2.hs
-- See Chapter12/Content/Either3.hs

data Example a = Blah | RoofGoats | Woot a

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail [_]    = Nothing
safeTail (_:xs) = Just xs
