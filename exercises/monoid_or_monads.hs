foo = do
    filename <- getLine
    contents <- readFile filename
    putStrLn contents