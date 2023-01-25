
module Language.LSP.Notebook.Util where


countNewLines :: String -> Int
countNewLines ('\n':xs) = 1 + countNewLines xs
countNewLines (_:xs) = countNewLines xs
countNewLines [] = 0

getLinesStartingAt :: String -> Int -> [Int]
getLinesStartingAt t startingAt = [startingAt..(startingAt + countNewLines t)]
