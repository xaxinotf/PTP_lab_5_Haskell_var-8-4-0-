import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

data FunctionType = LogFunction | SqrtFunction1 | SqrtFunction2

-- Основна функція
main :: IO ()
main = do
    putStrLn "Select function to compute (1 for Log, 2 for Sqrt1, 3 for Sqrt2 or 'Stop' to exit):"
    hFlush stdout
    funcTypeInput <- getLine
    case funcTypeInput of
        "Stop" -> putStrLn "Program terminated."
        _ -> do
            funcType <- getFunctionType funcTypeInput
            putStrLn "Enter a number for x (or 'Stop' to exit):"
            hFlush stdout
            input <- getLine
            case input of
                "Stop" -> putStrLn "Program terminated."
                _ -> case readMaybe input of
                    Just x -> do
                        computeAndDisplayResult funcType x
                        main -- repeat the loop
                    Nothing -> do
                        putStrLn "Invalid input, please enter a valid number."
                        main -- repeat the loop

getFunctionType :: String -> IO FunctionType
getFunctionType input = case input of
    "1" -> return LogFunction
    "2" -> return SqrtFunction1
    "3" -> return SqrtFunction2
    _ -> do
        putStrLn "Invalid input, please enter 1, 2, 3, or 'Stop'."
        newInput <- getLine
        getFunctionType newInput

computeAndDisplayResult :: (Floating a, Ord a, Show a) => FunctionType -> a -> IO ()
computeAndDisplayResult funcType x = do
    let result = case funcType of
            LogFunction -> show $ logFunction x
            SqrtFunction1 -> show $ sqrtFunction1 x
            SqrtFunction2 -> show $ sqrtFunction2 x
    putStrLn $ "Result: " ++ result


logFunction :: (Floating a, Ord a) => a -> Either String a
logFunction x = let value = x - (1 / 40) in
                if value > 0 then Right (logBase 10 value) else Left "Value under log is non-positive"

sqrtFunction1 :: (Floating a, Ord a) => a -> Either String a
sqrtFunction1 x = let value = x + 40 in
                  if value >= 0 then Right (1 / sqrt value) else Left "Value under square root is negative"

sqrtFunction2 :: (Floating a, Ord a) => a -> Either String a
sqrtFunction2 x = let value = x^2 - logBase 10 40 in
                  if value >= 0 then Right (sqrt value) else Left "Value under square root is negative"
