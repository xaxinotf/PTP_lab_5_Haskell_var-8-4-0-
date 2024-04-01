module Main where
import Text.Read (readMaybe)

t1 :: (Floating a, Ord a) => a -> Maybe a
t1 x = let value = x - (1 / 40) in
       if value > 0 then Just (logBase 10 value) else Nothing

t2 :: (Floating a, Ord a) => a -> Maybe a
t2 x = let value = x + 40 in
       if value > 0 then Just (1 / sqrt value) else Nothing

t3 :: (Floating a, Ord a) => a -> Maybe a
t3 x = let value = x^2 - logBase 10 40 in
       if value >= 0 then Just (sqrt value) else Nothing

-- без do-нотації
composeFunctions :: (Floating a, Ord a) => a -> Maybe a
composeFunctions x = t3 x >>= t2 >>= t1

-- do-нотацією
composeFunctionsDo :: (Floating a, Ord a) => a -> Maybe a
composeFunctionsDo x = do
    resultT3 <- t3 x
    resultT2 <- t2 resultT3
    t1 resultT2

-- Тестова функція, яка виводить результати
testFunction :: (Floating a, Ord a, Show a) => a -> IO ()
testFunction x = do
    putStrLn "Result without do-notation:"
    print $ composeFunctions x
    putStrLn "Result with do-notation:"
    print $ composeFunctionsDo x

-- Основна функція
main :: IO ()
main = loop where
    loop = do
        
        putStrLn "Task 2: Enter a number for x ('Stop' to exit):"
        input <- getLine
        case input of
            "Stop" -> putStrLn "Program terminated."
            _ -> case readMaybe input of
                Just x -> do
                    testFunction x
                    loop  -- Repeat the loop
                Nothing -> do
                    putStrLn "Invalid input, please enter a valid number or 'Stop' to exit."
                    loop  -- Repeat the loop
