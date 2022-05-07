
main :: IO ()
main = do
    putStrLn "Spanish bullshit?"
    spanish <- getLine
    print spanish
    writeFile "output.txt" spanish