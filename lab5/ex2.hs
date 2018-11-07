actSeq = putChar 'A' >> putChar 'G' >> putChar 'H' >> putChar '\n'

doActSeq = do
    putChar 'A'
    putChar 'G'
    putChar 'H'
    putChar '\n'

echo1 = getLine >>= putStrLn

deEcho1 = do
    line <- getLine
    putStrLn line

echo2 = getLine >>= \line -> putStrLn $ line ++ "!"
deEcho2 = do
    line <- getLine
    putStrLn $ line ++ "!"

echo3 :: IO ()
echo3 = getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO ()
dialog = putStr "What is your happy number? "
            >> getLine
            >>= \n -> let num = read n :: Int in
                    if num == 7
                    then putStrLn "Ah, lucky 7!"
                    else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hm, even number? Unusual!"

doEcho3 :: IO ()
doEcho3 = do
    l1 <- getLine
    l2 <- getLine
    putStrLn $ l1 ++ l2

doDialog :: IO ()
doDialog = do
    putStr "What is your happy number? "
    ans <- getLine
    let n = read ans :: Int
    if n == 7
        then putStrLn "Ah, lucky 7!"
        else if odd n
            then putStrLn "Odd number! That's most people's choice..."
            else putStrLn "Hm, even number? Unusual!"

twoQuestions :: IO ()
twoQuestions = putStr "What is your name? "
                >> getLine
                >>= \name -> putStr "How old are you? "
                >> getLine
                >>= \age -> print (name, age)

getLine' :: IO String
getLine' = do
    c <- getChar
    if c == '\n'
        then return []
        else do
            cs <- getLine'
            return $ c : cs

getLine'' :: IO String
getLine'' = getChar
        >>= \c -> if c == '\n'
                then return []
                else getLine'' >>= \cs -> return $ c : cs
