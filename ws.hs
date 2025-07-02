module Main where
import System.Environment
import System.IO
import Control.Monad
import Text.Read (readMaybe)
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf, intercalate)
import Data.Bits (shiftL, shiftR, xor, complement)
import Data.Maybe (fromMaybe)

------------------------------- Defining data type -----------------------------------
data Lambda = Lambda Int [String] deriving (Show, Eq) -- Lambda function data type

-- Define token data type
data Token = Int_ Int | Float_ Float | String_ String | Bool_ Bool | Op_ String | Vector_ [Token] | Matrix_ [[Token]] | Lambda_ Lambda | NullToken deriving (Show, Eq)

data Stack = Stack [Token] deriving Show -- Define Stack type

type CompOp a = a -> a -> Bool -- Define a type for comparison operations
-----------------------------------------------------------------------------------------

------------------------------- Defining stack method -----------------------------------
-- Create an empty stack
emptyStack :: Stack
emptyStack = Stack []

-- Push a token onto the stack
push :: Token -> Stack -> Stack
push token (Stack tokens) = Stack (token : tokens)

-- Pop a token from the stack
pop :: Stack -> (Token, Stack)
pop (Stack []) = (NullToken, Stack [])
pop (Stack (t:stack)) = (t, Stack stack)

-- Pop n tokens from the stack
popN :: Int -> Stack -> ([Token], Stack)
popN 0 stack = ([], stack)
popN n stack =
    let (token, stack1) = pop stack
    in case token of
        NullToken -> ([], stack) -- Not enough elements
        _ -> let (tokens, stack2) = popN (n-1) stack1 in (token:tokens, stack2)
-----------------------------------------------------------------------------------------

------------------------------- Helper method -------------------------------------------
-- numeric operation for the rest 
numericOperator :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> Stack -> Stack
numericOperator intOp floatOp stack =
  let
    (b, stack1) = pop stack
    (a, stack2) = pop stack1
  in case (a, b) of
    (Int_ a, Int_ b) -> push (Int_ (intOp a b)) stack2
    (Float_ a, Float_ b) -> let result = floatOp a b in push (Float_ result) stack2
    (Int_ a, Float_ b) -> let result = floatOp (fromIntegral a) b in push (Float_ result) stack2
    (Float_ a, Int_ b) -> let result = floatOp a (fromIntegral b) in push (Float_ result) stack2
    _ -> stack -- not enough values or wrong types

-- Helper function for comparison operator that base on the operator is given 
compareTokens :: (CompOp Int) -> (CompOp Float) -> (CompOp String) -> (CompOp Bool) -> Token -> Token -> Token
compareTokens intOp floatOp stringOp boolOp a b = case (a, b) of
  (Int_ aVal, Int_ bVal) -> Bool_ (intOp aVal bVal)
  (Float_ aVal, Float_ bVal) -> Bool_ (floatOp aVal bVal)
  (Int_ aVal, Float_ bVal) -> Bool_ (floatOp (fromIntegral aVal) bVal)
  (Float_ aVal, Int_ bVal) -> Bool_ (floatOp aVal (fromIntegral bVal))
  (String_ aVal, String_ bVal) -> Bool_ (stringOp aVal bVal)
  (Bool_ aVal, Bool_ bVal) -> Bool_ (boolOp aVal bVal)
  _ -> Bool_ False

-- Helper function for comparison operator base on spaceship symbol '<=>'
spaceshipOperator :: Token -> Token -> Token
spaceshipOperator a b = case (a, b) of
  (Int_ aVal, Int_ bVal) -> Int_ (comparing (compare aVal bVal))
  (Float_ aVal, Float_ bVal) -> Int_ (comparing (compare aVal bVal))
  (Int_ aVal, Float_ bVal) -> Int_ (comparing (compare (fromIntegral aVal) bVal))
  (Float_ aVal, Int_ bVal) -> Int_ (comparing (compare aVal (fromIntegral bVal)))
  (String_ aVal, String_ bVal) -> Int_ (comparing (compare aVal bVal))
  (Bool_ aVal, Bool_ bVal) -> Int_ (comparing (compare aVal bVal))
  _ -> Int_ (-2)  -- Error value
  where
    comparing :: Ordering -> Int
    comparing LT = -1
    comparing EQ = 0
    comparing GT = 1

-- numeric operation for vector and matrix 
vectorMatrixNumericOp  :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> Token -> Token -> Token
vectorMatrixNumericOp  intOp floatOp (Int_ a) (Int_ b) = let result = floatOp (fromIntegral a) (fromIntegral b)
                                                          in if fromIntegral (round result) == result then Int_ (intOp a b) else Float_ result
vectorMatrixNumericOp  _ floatOp (Float_ a) (Float_ b) = Float_ (floatOp a b)
vectorMatrixNumericOp  _ floatOp (Int_ a) (Float_ b) = Float_ (floatOp (fromIntegral a) b)
vectorMatrixNumericOp  _ floatOp (Float_ a) (Int_ b) = Float_ (floatOp a (fromIntegral b))

-- Element-wise operation on vectors or matrices
elementwiseOperation :: (Token -> Token -> Token) -> Stack -> Stack
elementwiseOperation op stack =
  let (b, stack1) = pop stack
      (a, stack2) = pop stack1
  in case (a, b) of
    (Vector_ v1, Vector_ v2) -> if length v1 == length v2 then push (Vector_ (zipWith op v1 v2)) stack2 else stack
    (Matrix_ m1, Matrix_ m2) -> if not (null m1) && not (null m2) && length m1 == length m2 && all (\(row1, row2) -> length row1 == length row2) (zip m1 m2)
                                                then let resultMatrix = zipWith (zipWith op) m1 m2 in push (Matrix_ resultMatrix) stack2
                                                else stack  -- No same dimensions on matrices 
    _ -> stack  -- Not vectors or matrices

-- Dot product between two vectors
dotProduct :: [Token] -> [Token] -> Token
dotProduct v1 v2 =
  if not (null v1) && not (null v2) && length v1 == length v2 then
    let product = zipWith (vectorMatrixNumericOp (*) (*)) v1 v2
        head_ = if null product then Int_ 0 else head product
        tail_ = if null product then [] else tail product
    in foldl (vectorMatrixNumericOp (+) (+)) head_ tail_
  else Int_ 0  -- Empty vectors or mismatched lengths

-- Vector cross product
vectorCrossOperator :: Stack -> Stack
vectorCrossOperator stack =
    let (b, stack1) = pop stack
        (a, stack2) = pop stack1
    in case (a, b) of
        (Vector_ v1, Vector_ v2) -> if length v1 == 3 && length v2 == 3 then
          let getValue (Int_ n) = fromIntegral n
              getValue (Float_ f) = f
              getValue _ = 0

              [a1, a2, a3] = map getValue v1
              [b1, b2, b3] = map getValue v2
              c1 = a2 * b3 - a3 * b2 -- Cross product formula
              c2 = a3 * b1 - a1 * b3
              c3 = a1 * b2 - a2 * b1

              token t = if fromIntegral (round t) == t then Int_ (round t) else Float_ t -- Convert back to tokens
              result = Vector_ [token c1, token c2, token c3]
          in push result stack2
        else stack  -- Not vectors
        _ -> stack

-- Vector dot product operation
vectorDotProduct :: Stack -> Stack
vectorDotProduct stack =
  let (b, stack1) = pop stack
      (a, stack2) = pop stack1
  in case (a, b) of
    (Vector_ v1, Vector_ v2) -> push (dotProduct v1 v2) stack2
    _ -> stack

-- Transpose function
transpose :: [[Token]] -> [[Token]]
transpose [] = []
transpose ([]:m) = transpose m
transpose m = map head m : transpose (map tail m)

-- Matrix multiplication operation between matrices and a vector 
matrixMultiplyOp :: Stack -> Stack
matrixMultiplyOp stack =
    let (b, stack1) = pop stack
        (a, stack2) = pop stack1
    in case (a, b) of
        (Matrix_ m1, Matrix_ m2) -> -- Matrix multiplication operation
            if validMatrixMultiply m1 m2
            -- Transpose the second matrix and calculate the matrix result
            then let m2T = transpose m2 in push (Matrix_ [ [ dotProduct row col | col <- m2T ] | row <- m1 ]) stack2
            else stack  -- Dimensions don't match

        (Matrix_ m, Vector_ v) -> -- Matrix-vector multiplication operation
            if validMatrixVectorMultiply m v
            -- For each row in the matrix, calculate dot product with the vector
            then let result = map (`dotProduct` v) m in push (Vector_ result) stack2
            else stack
        _ -> stack  -- Not correct types or not enough values
    where
        -- Check dimension similarity
        validMatrixMultiply m1 m2 = not (null m1) && not (null m2) && not (null (head m1)) && not (null (head m2)) && length (head m1) == length m2
        validMatrixVectorMultiply m v = not (null m) && not (null v) && not (null (head m)) && length (head m) == length v

-- Execute the body of a lambda function with recursion by processing tokens one by one
executeLambda :: [String] -> [(String, Token)] -> Lambda -> Stack -> Stack
executeLambda [] _ _ stack = stack -- no tokens remain
executeLambda (token : tokens) condition lambda stack -- process tokens one by one
  -- When the token is "SELF", push the lambda itself onto the stack for recursion
  | token == "SELF" = let newStack = push (Lambda_ lambda) stack in executeLambda tokens condition lambda newStack
  -- When the token is a variable that starts with 'x' followed by digits, eg x0, x1
  | not (null token) && head token == 'x' && all isDigit (tail token) = case lookup token condition of
                                                                          Just value -> executeLambda tokens condition lambda (push value stack)
                                                                          Nothing -> executeLambda tokens condition lambda stack
  -- For the rest of tokens, process them using processToken method as standard processor                                                                       
  | otherwise = let newStack = processToken token stack in executeLambda tokens condition lambda newStack
------------------------------------------------------------------------------------------------

-------------------------------------- Operator Method -----------------------------------------
-- Performs arithmetic operations on numeric values and strings
arithmeticOperator :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> String -> Stack -> Stack
arithmeticOperator intOp floatOp opName stack =
  case stack of
    Stack (b:a:rest) ->
      case (a, b) of
        (Matrix_ m1, Matrix_ m2) -> case opName of
                                      "*" -> matrixMultiplyOp stack
                                      _ -> elementwiseOperation (vectorMatrixNumericOp intOp floatOp) stack
        (Matrix_ m, Vector_ v) -> if opName == "*" then matrixMultiplyOp stack else elementwiseOperation (vectorMatrixNumericOp intOp floatOp) stack
        (Vector_ v1, Vector_ v2) -> if opName == "*" then vectorDotProduct stack else elementwiseOperation (vectorMatrixNumericOp intOp floatOp) stack
        (String_ s1, String_ s2) -> if opName == "+" then Stack (String_ (s1 ++ s2) : rest) else numericOperator intOp floatOp stack
        (String_ s, Int_ n) -> if opName == "*" then Stack (String_ (concat $ replicate n s) : rest) else numericOperator intOp floatOp stack
        (Int_ n, String_ s) -> if opName == "*" then Stack (String_ (concat $ replicate n s) : rest) else numericOperator intOp floatOp stack
        _ -> numericOperator intOp floatOp stack
    _ -> stack

-- Roll operation on stack elements
rollOperator :: Bool -> Stack -> Stack
rollOperator isRoll (Stack (num:stack)) =
  case num of
    Int_ n ->
      if n <= 0 || n > length stack then Stack (num:stack) else
        let
          (tokens, rest) = splitAt n stack
          rotated = if isRoll then last tokens : init tokens else tail tokens ++ [head tokens]  -- ROLL else ROLLD
        in Stack (rotated ++ rest)
    _ -> Stack (num:stack)
rollOperator _ stack = stack

-- Comparison operation
comparisonOperator :: String -> Stack -> Stack
comparisonOperator op (Stack (b:a:stack)) =
  let result = case op of
        "==" -> Bool_ (a == b)
        "!=" -> Bool_ (a /= b)
        ">" -> compareTokens (>) (>) (>) (>) a b
        "<" -> compareTokens (<) (<) (<) (<) a b
        ">=" -> compareTokens (>=) (>=) (>=) (>=) a b
        "<=" -> compareTokens (<=) (<=) (<=) (<=) a b
        "<=>" -> spaceshipOperator a b
        _ -> Bool_ False
  in Stack (result : stack)
comparisonOperator _ stack = stack

-- Boolean logic operation
booleanOperator :: String -> Stack -> Stack
booleanOperator op (Stack (b:a:stack)) =
  case (a, b) of
    (Bool_ a, Bool_ b) ->
      let result = case op of
            "&" -> Bool_ (a && b)  -- AND
            "|" -> Bool_ (a || b)  -- OR
            "^" -> Bool_ (a /= b)  -- XOR
            _ -> Bool_ False
      in Stack (result : stack)
    _ -> Stack (b:a:stack)  -- return non-booleans
booleanOperator _ stack = stack

-- Ifelse conditional operation
ifElseOperator :: Stack -> Stack
ifElseOperator (Stack (condition:falseVal:trueVal:stack)) =
  case condition of
    Bool_ b -> if b then Stack (trueVal : stack) else Stack (falseVal : stack)
    _ -> Stack (condition:falseVal:trueVal:stack)  -- Invalid condition
ifElseOperator stack = stack

-- XOR operation
xorOperator :: Stack -> Stack
xorOperator (Stack (b:a:stack)) =
  case (a, b) of
    (Int_ a, Int_ b) -> Stack (Int_ (a `xor` b) : stack)
    (Bool_ a, Bool_ b) -> Stack (Bool_ (a /= b) : stack)
    _ -> Stack (b:a:stack)
xorOperator stack = stack

-- Bitwise and shifts operation
bitshiftOperator :: String -> Stack -> Stack
bitshiftOperator op (Stack (b:a:stack)) =
  case (a, b) of
    (Int_ x, Int_ y) ->
      let result = case op of
            "<<" -> Int_ (x `shiftL` y)  -- Left shift
            ">>" -> Int_ (x `shiftR` y)  -- Right shift
            _ -> Int_ 0
      in Stack (result : stack)
    _ -> Stack (b:a:stack)  -- both not integers
bitshiftOperator _ stack = stack

-- Unary operation
unaryOperator :: String -> Stack -> Stack
unaryOperator op stack =
  let (a, stack1) = pop stack
  in case (op, a) of
    ("!", Bool_ b) -> push (Bool_ (not b)) stack1
    ("~", Int_ n) -> push (Int_ (complement n)) stack1
    _ -> stack

-- Cross product operation
crossProductOperator :: Stack -> Stack
crossProductOperator stack =
  case stack of
    Stack (b:a:rest) ->
      case (a, b) of
        (Vector_ _, Vector_ _) -> vectorCrossOperator stack
        _ -> stack  -- Not vectors, return unchanged
    _ -> stack

-- Matrix transpose operation
matrixTranspose :: Stack -> Stack
matrixTranspose stack =
    let (a, stack1) = pop stack in case a of
        Matrix_ m ->
          if not (null m) && not (null (head m)) then let result = transpose m in push (Matrix_ result) stack1
          else stack  -- Empty matrix
        _ -> stack  -- Not a matrix

-- Processes lambda expressions
lambdaOperator :: Lambda -> Stack -> Stack
lambdaOperator lambda@(Lambda count bodyTokens) stack =
    let (args, remainingStack) = popN count stack -- Pop the required number of arguments
        reversedArgs = reverse args -- Reverse args to match parameter order e.g x0 is the first parameter
        condition = zip (map (\i -> "x" ++ show i) [0..(count-1)]) reversedArgs -- Create a new condition with the arguments eg. [("x0",Int_ 1),("x1",Int_ 5)]
        result = executeLambda bodyTokens condition lambda remainingStack -- Execute the body tokens with the condition then passing the lambda for 'SELF' 
    in result

-- Eval operation
evalOperator :: Stack -> Stack
evalOperator stack =
    let (a, stack1) = pop stack
    in case a of
        Lambda_ lambda -> lambdaOperator lambda stack1
        Op_ op -> stackDeluxe op stack1
        _ -> stack  -- Not an operator or lambda
--------------------------------------------------------------------------------------------------------------

-- Validates token format
checkToken :: String -> String -> Bool
checkToken tokenType str
  | null str = False
  | tokenType == "vector" = head str == '[' && last str == ']' && not (length str >= 2 && str !! 1 == '[')
  | tokenType == "matrix" = head str == '[' && length str >= 2 && str !! 1 == '[' && last str == ']'
  | tokenType == "lambda" = head str == '{' && last str == '}' && elem '|' str
  | tokenType == "quoted" = length str >= 2 && head str == '"' && last str == '"'
  | tokenType == "quoted_token" = not (null str) && head str == '\'' && length str > 1
  | otherwise = False
--------------------------------------------------------------------------------------------------------

--------------------------------- Parsing Method -------------------------------------------------------
-- Parse a string token into a specific type
parseToken :: String -> Token
parseToken str
  | checkToken "lambda" str = parseLambda str
  | checkToken "matrix" str = parseMatrix str
  | checkToken "vector" str = parseVector str
  | str == "true" = Bool_ True
  | str == "false" = Bool_ False
  | checkToken "quoted" str = String_ (init (tail str))
  | str `elem` operators = Op_ str
  | otherwise = parseNumeric str
  where
    parseNumeric s = case readMaybe s :: Maybe Int of
                      Just n -> Int_ n
                      Nothing -> case readMaybe s :: Maybe Float of
                                  Just f -> Float_ f
                                  Nothing -> String_ s

    -- List of all operators
    operators = ["+", "-", "*", "/", "**", "%", "DROP", "DUP", "SWAP", "ROT", "ROLL", "ROLLD", "==", "!=", ">", "<", ">=", "<=", "<=>", "IFELSE", "&", "|", "^", "<<", ">>", "!", "~", "x", "TRANSP", "EVAL"]

-- Parse vector from a string 
parseVector :: String -> Token
parseVector str =
    let content = tail (takeWhile (/= ']') $ dropWhile (/= '[') str)  -- Remove the leading '['
        elements = splitByComma content
        tokens = map parseToken elements
    in Vector_ tokens
    where
        splitByComma [] = []
        splitByComma s = let (first, rest) = break (== ',') s in filter (not . isSpace) first : if null rest then [] else splitByComma (tail rest)

-- Parse matrix from a string 
parseMatrix :: String -> Token
parseMatrix str =
    let content = init (tail (filter (not . isSpace) str)) -- Remove outer brackets
        rowsStr = splitRows content
        rows = map parseRowVector rowsStr
    in Matrix_ rows
    where
        parseRowVector rowStr = -- Parse a row as a vector
            case parseVector ("[" ++ rowStr ++ "]") of
              Vector_ tokens -> tokens
              _ -> []

        -- Split the matrix string into rows
        splitRows :: String -> [String]
        splitRows str = let parts = splitByBrackets str in parts

        -- Split by top-level brackets
        splitByBrackets :: String -> [String]
        splitByBrackets [] = []
        splitByBrackets ('[':rest) =
            let (row, remaining) = recursionDepth rest 0 ""
            in row : if null remaining then [] else if head remaining == ','
                      then splitByBrackets (tail remaining) else splitByBrackets remaining
        splitByBrackets (_:rest) = splitByBrackets rest

        -- Helper method to extract content between matching brackets
        -- Reference from https://stackoverflow.com/questions/40438591/writing-a-recursive-function-to-count-the-depth-of-a-tree 
          -- https://stackoverflow.com/questions/10243290/determining-matching-parenthesis-in-haskell 
          -- https://softwareengineering.stackexchange.com/questions/375241/patterns-for-tracking-state-in-recursive-haskell-code
        recursionDepth :: String -> Int -> String -> (String, String)
        recursionDepth [] _ collectedChars = (collectedChars, "")
        recursionDepth (']':rest) 0 collectedChars = (collectedChars, rest)
        recursionDepth ('[':rest) depth collectedChars = recursionDepth rest (depth + 1) (collectedChars ++ "[")
        recursionDepth (']':rest) depth collectedChars = recursionDepth rest (depth - 1) (collectedChars ++ "]")
        recursionDepth (c:rest) depth collectedChars = recursionDepth rest depth (collectedChars ++ [c])

-- Parse a lambda from a string 
parseLambda :: String -> Token
parseLambda str =
    let trimmed = filter (\char -> char /= '\n' && char /= '\t') str -- trimming the current string for parsing
        inner = init (tail trimmed) -- Remove outer braces
        parts = break (== '|') inner -- Split at the '|'
        countStr = filter (not . isSpace) (fst parts)
        body = if null (snd parts) then "" else tail (snd parts)  -- Remove the '|'
        
        count =  if null countStr || not (all isDigit countStr) then 0 else read countStr
        bodyTokens = words body -- Tokenise the body
    in Lambda_ (Lambda count bodyTokens)
--------------------------------------------------------------------------------------------------------------------

-- Main operation to process token 
stackDeluxe :: String -> Stack -> Stack
stackDeluxe op stack
  | op `elem` ["==", "!=", ">", "<", ">=", "<=", "<=>"] = comparisonOperator op stack
  | op `elem` ["<<", ">>"] = bitshiftOperator op stack
  | op `elem` ["&", "|"] = booleanOperator op stack
  | op `elem` ["!", "~"] = unaryOperator op stack
  | otherwise = case op of
    "+" -> arithmeticOperator (+) (+) "+" stack
    "-" -> arithmeticOperator (-) (-) "-" stack
    "*" -> arithmeticOperator (*) (*) "*" stack
    "/" -> arithmeticOperator div (/) "/" stack
    "**" -> arithmeticOperator (^) (**) "**" stack
    "%" -> arithmeticOperator mod (\x y -> fromIntegral (round x `mod` round y)) "%" stack
    "DROP" -> snd (pop stack) -- Swaps the top two elements of the stack
    "DUP" -> let (token, _) = pop stack in if token == NullToken then stack else push token stack

    "SWAP" ->
        let (b, stack1) = pop stack
            (a, stack2) = pop stack1
        in case (a, b) of
            (NullToken, _) -> stack
            (_, NullToken) -> stack
            _ -> push a (push b stack2)

    "ROT" -> case stack of
            Stack (c:b:a:rest) -> Stack (a:c:b:rest)
            _ -> stack

    "ROLL" -> rollOperator True stack
    "ROLLD" -> rollOperator False stack
    "IFELSE" -> ifElseOperator stack
    "^" -> xorOperator stack
    "x" -> crossProductOperator stack
    "TRANSP" -> matrixTranspose stack
    _ -> stack

-- Process a token by parsing them 
processToken :: String -> Stack -> Stack
processToken token stack
  | checkToken "quoted_token" token = let unquotedToken = tail token in push (parseToken unquotedToken) stack
  | checkToken "lambda" token = let lambda = parseLambda token in case lambda of
                                                                        Lambda_ l -> lambdaOperator l stack
                                                                        _ -> push lambda stack
  | checkToken "matrix" token = push (parseMatrix token) stack
  | checkToken "vector" token = push (parseVector token) stack
  | otherwise = case parseToken token of
                  Op_ op -> if op == "EVAL" then evalOperator stack else stackDeluxe op stack
                  t -> push t stack

-- toString method 
toString :: Token -> String
toString (Int_ n) = show n
toString (Float_ f) = show f
toString (String_ s) = "\"" ++ s ++ "\""
toString (Bool_ True) = "true"
toString (Bool_ False) = "false"
toString (Op_ op) = op
toString (Vector_ vs) = "[" ++ intercalate ", " (map toString vs) ++ "]"
toString (Matrix_ rows) = "[[" ++ intercalate "], [" (map (intercalate ", " . map toString) rows) ++ "]]"
toString (Lambda_ (Lambda condition bodyTokens)) = "{" ++ show condition ++ " | " ++ unwords bodyTokens ++ "}"
toString NullToken = ""  -- shouldn't appear in output

-- tokenise method 
tokenise :: String -> [String]
tokenise = processInput []
  where
    processInput tokenList [] = reverse tokenList -- when input is empty
    processInput tokenList inputString@(currentChar:remainingChars)
      | isSpace currentChar = processInput tokenList (dropWhile isSpace remainingChars)  -- Skip whitespace
        -- Handle double quotes
      | currentChar == '"' =
          let (stringContent, afterQuote) = span (/= '"') remainingChars
              quotedString = '"' : stringContent ++ "\""
          in processInput (quotedString : tokenList) (drop 1 afterQuote)
        -- Handle lambda
      | currentChar == '{' =
          let (bracedToken, afterBrace) = balancedDelimiter '{' '}' inputString
          in processInput (bracedToken : tokenList) afterBrace
        -- Handle matrix
      | currentChar == '[' && not (null remainingChars) && head remainingChars == '[' =
          let (doubleBracketToken, afterDoubleBracket) = balancedDelimiter '[' ']' inputString
          in processInput (doubleBracketToken : tokenList) afterDoubleBracket
        -- Handle vector   
      | currentChar == '[' =
          let (bracketContent, afterBracket) = span (/= ']') inputString
              bracketToken = bracketContent ++ "]"
          in processInput (bracketToken : tokenList) (drop 1 afterBracket)
        -- Handle single quote  
      | currentChar == '\'' =
          let (quotedToken, afterQuoted) = break isSpace inputString
          in processInput (quotedToken : tokenList) afterQuoted
        -- Handle the rest
      | otherwise =
          let (plainToken, afterToken) = break isSpace inputString
          in processInput (plainToken : tokenList) afterToken

    -- Helper method to collect balanced delimiters
    -- Reference from https://stackoverflow.com/questions/7209260/checking-if-a-string-consists-of-balanced-parenthesis
      -- https://stackoverflow.com/questions/10243290/determining-matching-parenthesis-in-haskell 
      -- https://softwareengineering.stackexchange.com/questions/375241/patterns-for-tracking-state-in-recursive-haskell-code
    balancedDelimiter :: Char -> Char -> String -> (String, String)
    balancedDelimiter openChar closeChar inputStr = recursionDepth inputStr 0 ""
      where
        recursionDepth [] _ collectedChars = (collectedChars, [])
        recursionDepth (currentChar:remainingStr) depth collectedChars
          | currentChar == openChar = recursionDepth remainingStr (depth + 1) (collectedChars ++ [currentChar])
          | currentChar == closeChar = if depth == 1 then (collectedChars ++ [currentChar], remainingStr)
                                      else recursionDepth remainingStr (depth - 1) (collectedChars ++ [currentChar])
          | otherwise = recursionDepth remainingStr depth (collectedChars ++ [currentChar])

-- Process the input string from input file and generate output
process :: String -> String
process input =
  let
    tokens = tokenise input   -- Split input into tokens 
    stack = foldl (flip processToken) emptyStack tokens  -- Processed token in the stack
    (Stack resultStack) =  stack
    reversedStack = reverse resultStack
    result = unlines (map toString reversedStack) -- Convert to string
  in result

main = do
  args <- getArgs
  let inputFile = head args
  let digits = filter isDigit inputFile
  let outputFile = "output-" ++ digits ++ ".txt"

  contents <- readFile inputFile
  writeFile outputFile (process contents)