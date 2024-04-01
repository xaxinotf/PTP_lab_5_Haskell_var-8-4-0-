import Text.Read (readMaybe)

sqrtLogFunction :: (Floating a, Ord a) => a -> a -> Maybe a
sqrtLogFunction x n
  | n > 0 = let value = x^2 - logBase 10 n
            in if value >= 0 then Just (sqrt value) else Nothing
  | otherwise = Nothing

logFunction :: (Floating a, Ord a) => a -> Maybe a
logFunction x = let value = x - (1 / 40) in
                if value > 0 then Just (logBase 10 value) else Nothing

sqrtFunction1 :: (Floating a, Ord a) => a -> Maybe a
sqrtFunction1 x = let value = x + 40 in
                  if value >= 0 then Just (1 / sqrt value) else Nothing

-- sуперпозиція з do-нотацією
superpositionFunctionDo :: (Floating a, Ord a) => a -> Maybe a
superpositionFunctionDo x = do
    xResult <- logFunction x
    nResult <- sqrtFunction1 x
    sqrtLogFunction xResult nResult

-- sуперпозиція без do-нотації
superpositionFunction :: (Floating a, Ord a) => a -> Maybe a
superpositionFunction x =
    logFunction x >>= (\xResult -> sqrtFunction1 x >>= (\nResult -> sqrtLogFunction xResult nResult))

main :: IO ()
main = loop where
    loop = do
        putStrLn "Enter a number for x ('Stop' to exit):"
        input <- getLine
        case input of
            "Stop" -> putStrLn "Program terminated."
            _ -> case readMaybe input of
                Just x -> do
                    putStrLn "Result with do-notation:"
                    let resultDo = superpositionFunctionDo x
                    print resultDo

                    putStrLn "Result without do-notation:"
                    let result = superpositionFunction x
                    print result

                    loop  
                Nothing -> do
                    putStrLn "Invalid input, please enter a valid number or 'Stop' to exit."
                    loop  
