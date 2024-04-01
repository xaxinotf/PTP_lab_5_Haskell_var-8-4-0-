import Text.Read (readMaybe)

-- Функція для обчислення заданої формули
sqrtLogFunction :: (Floating a, Ord a) => a -> a -> Maybe a
sqrtLogFunction x n
  | n > 0 = let value = x^2 - logBase 10 n
            in if value >= 0 then Just (sqrt value) else Nothing
  | otherwise = Nothing

-- Тестова функція для перевірки різних значень
testFunction :: (Floating a, Ord a, Show a) => a -> a -> IO ()
testFunction x n = do
    putStrLn $ "Calculating Sqrt[" ++ show x ++ "^2 - log10(" ++ show n ++ ")]:"
    print $ sqrtLogFunction x n

-- Основна функція
main :: IO ()
main = loop where
    loop = do
        putStrLn "Enter a number for x ('Stop' to exit):"
        xInput <- getLine
        case xInput of
            "Stop" -> putStrLn "Program terminated."
            _ -> do
                putStrLn "Enter a number for n ('Stop' to exit):"
                nInput <- getLine
                case nInput of
                    "Stop" -> putStrLn "Program terminated."
                    _ -> case (readMaybe xInput, readMaybe nInput) of
                        (Just x, Just n) -> do
                            testFunction x n
                            loop  
                        _ -> do
                            putStrLn "Invalid input, please enter valid numbers."
                            loop  
