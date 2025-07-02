module Main where 
import System.Environment
import System.IO  
import Control.Monad
import Text.Read (readMaybe)
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf, intercalate)
import Data.Bits (shiftL, shiftR, xor, complement)

-- Lambda function data type
data Lambda = Lambda Int [String] -- Arity and body tokens
  deriving (Show, Eq)

-- Define token data type
data Token = Int_ Int
  | Float_ Float 
  | String_ String
  | Bool_ Bool
  | Op_ String
  | Vector_ [Token]   
  | Matrix_ [[Token]]  
  | Lambda_ Lambda
  deriving (Show, Eq)

-- Define Stack type
data Stack = Stack [Token]
  deriving Show

-- Create an empty stack
emptyStack :: Stack
emptyStack = Stack []

-- Push a token onto the stack
push :: Token -> Stack -> Stack
push token (Stack tokens) = Stack (token : tokens)

-- Pop a token from the stack
pop :: Stack -> (Maybe Token, Stack)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (t:stack)) = (Just t, Stack stack)

-- Pop n elements from the stack
popN :: Int -> Stack -> ([Token], Stack)
popN 0 stack = ([], stack)
popN n stack = 
    let (maybeToken, stack1) = pop stack
    in case maybeToken of
        Just token -> 
            let (tokens, stack2) = popN (n-1) stack1
            in (token:tokens, stack2)
        Nothing -> ([], stack)  -- Not enough elements

-- Helper function to apply a binary operation to tokens
applyBinaryOp :: (Float -> Float -> Float) -> Token -> Token -> Token
applyBinaryOp op (Int_ a) (Int_ b) = 
    let result = op (fromIntegral a) (fromIntegral b)
    in if fromIntegral (round result) == result
       then Int_ (round result)
       else Float_ result
applyBinaryOp op (Float_ a) (Float_ b) = Float_ (op a b)
applyBinaryOp op (Int_ a) (Float_ b) = Float_ (op (fromIntegral a) b)
applyBinaryOp op (Float_ a) (Int_ b) = Float_ (op a (fromIntegral b))
applyBinaryOp _ _ _ = Int_ 0  -- Default case for non-numeric tokens

-- Vector binary operation - only for vector-vector operations
vectorBinaryOp :: (Token -> Token -> Token) -> Stack -> Stack
vectorBinaryOp op stack =
    let (b, stack1) = pop stack
        (a, stack2) = pop stack1
    in case (a, b) of
        (Just (Vector_ v1), Just (Vector_ v2)) -> 
            if length v1 == length v2 
            then push (Vector_ (zipWith op v1 v2)) stack2
            else stack  -- Vectors must be same length
        _ -> stack  -- Not vectors, return unchanged

-- Matrix binary operation - only for matrix-matrix operations
matrixBinaryOp :: (Token -> Token -> Token) -> Stack -> Stack
matrixBinaryOp op stack =
    let (b, stack1) = pop stack
        (a, stack2) = pop stack1
    in case (a, b) of
        (Just (Matrix_ m1), Just (Matrix_ m2)) -> 
            if not (null m1) && not (null m2) && 
               length m1 == length m2 && 
               all (\(r1, r2) -> length r1 == length r2) (zip m1 m2)
            then 
                let resultMatrix = zipWith (\row1 row2 -> zipWith op row1 row2) m1 m2
                in push (Matrix_ resultMatrix) stack2
            else stack  -- Matrices must have same dimensions
        _ -> stack  -- Not matrices, return unchanged

arithmeticOperator :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> Stack -> Stack
arithmeticOperator intOp floatOp stack =
  let
    (b, stack1) = pop stack
    (a, stack2) = pop stack1
  in case (a, b) of
    (Just (Int_ x), Just (Int_ y)) -> push (Int_ (intOp x y)) stack2
    (Just (Float_ x), Just (Float_ y)) ->
      let result = floatOp x y
      in push (Float_ result) stack2
    (Just (Int_ x), Just (Float_ y)) ->
      let result = floatOp (fromIntegral x) y
      in push (Float_ result) stack2
    (Just (Float_ x), Just (Int_ y)) ->
      let result = floatOp x (fromIntegral y)
      in push (Float_ result) stack2
    _ -> stack -- not enough values or wrong types

additionOperator :: Stack -> Stack
additionOperator stack = 
  -- Check for different types
  case stack of
    Stack (b:a:rest) -> 
      case (a, b) of
        (Matrix_ _, Matrix_ _) -> matrixBinaryOp (applyBinaryOp (+)) stack
        (Vector_ _, Vector_ _) -> vectorBinaryOp (applyBinaryOp (+)) stack
        (String_ s1, String_ s2) -> 
          Stack (String_ (s1 ++ s2) : rest)
        _ -> arithmeticOperator (+) (+) stack  -- Fall back to normal addition
    _ -> arithmeticOperator (+) (+) stack

subtractionOperator :: Stack -> Stack
subtractionOperator stack =
  case stack of
    Stack (b:a:rest) -> 
      case (a, b) of
        (Matrix_ _, Matrix_ _) -> matrixBinaryOp (applyBinaryOp (-)) stack
        (Vector_ _, Vector_ _) -> vectorBinaryOp (applyBinaryOp (-)) stack
        _ -> arithmeticOperator (-) (-) stack
    _ -> arithmeticOperator (-) (-) stack

-- Vector dot product (for vector-vector multiplication)
vectorDotProduct :: Stack -> Stack
vectorDotProduct stack =
    let (b, stack1) = pop stack
        (a, stack2) = pop stack1
    in case (a, b) of
        (Just (Vector_ v1), Just (Vector_ v2)) -> 
            if length v1 == length v2 
            then 
                let products = zipWith (applyBinaryOp (*)) v1 v2
                    -- Sum the products
                    sumOp = applyBinaryOp (+)
                    initial = if null products then Int_ 0 else head products
                    result = foldl sumOp initial (if null products then [] else tail products)
                in push result stack2
            else stack
        _ -> stack

-- A more robust transpose function
transpose :: [[Token]] -> [[Token]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose xss = map head xss : transpose (map tail xss)

-- Helper for dot product between two vectors (sequences of tokens)
dotProduct :: [Token] -> [Token] -> Token
dotProduct v1 v2 =
    if not (null v1) && not (null v2) && length v1 == length v2
    then 
        let products = zipWith (applyBinaryOp (*)) v1 v2
            -- Sum the products
            sumOp = applyBinaryOp (+)
            initial = if null products then Int_ 0 else head products
            result = foldl sumOp initial (if null products then [] else tail products)
        in result
    else Int_ 0  -- Default for empty vectors or mismatched lengths

-- Matrix multiplication
matrixMultiply :: Stack -> Stack
matrixMultiply stack =
    let (b, stack1) = pop stack
        (a, stack2) = pop stack1
    in case (a, b) of
        (Just (Matrix_ m1), Just (Matrix_ m2)) -> 
            if not (null m1) && not (null m2) && 
               not (null (head m1)) && not (null m2) &&
               not (null (head m2)) &&
               length (head m1) == length m2
            then
                -- Transpose the second matrix to make column access easier
                let m2T = transpose m2
                    -- Compute the result matrix
                    result = [ [ dotProduct row col | col <- m2T ] | row <- m1 ]
                in push (Matrix_ result) stack2
            else stack  -- Matrices dimensions don't match for multiplication
        _ -> stack  -- Not matrices or not enough values

-- Matrix-vector multiplication
matrixVectorMultiply :: Stack -> Stack
matrixVectorMultiply stack =
    let (b, stack1) = pop stack
        (a, stack2) = pop stack1
    in case (a, b) of
        (Just (Matrix_ m), Just (Vector_ v)) -> 
            if not (null m) && not (null v) && 
               not (null (head m)) &&
               length (head m) == length v
            then
                -- For each row in the matrix, compute dot product with the vector
                let result = map (\row -> dotProduct row v) m
                in push (Vector_ result) stack2
            else stack  -- Dimensions don't match for multiplication
        _ -> stack  -- Not correct types or not enough values

multiplyOperator :: Stack -> Stack
multiplyOperator stack = 
  -- Check for different types
  case stack of
    Stack (b:a:rest) -> 
      case (a, b) of
        (Matrix_ _, Matrix_ _) -> matrixMultiply stack
        (Matrix_ _, Vector_ _) -> matrixVectorMultiply stack
        (Vector_ _, Vector_ _) -> vectorDotProduct stack
        (String_ s, Int_ n) -> Stack (String_ (concat $ replicate n s) : rest)
        (Int_ n, String_ s) -> Stack (String_ (concat $ replicate n s) : rest)
        _ -> arithmeticOperator (*) (*) stack
    _ -> arithmeticOperator (*) (*) stack

-- Element-wise vector division
vectorDivideElementWise :: Stack -> Stack
vectorDivideElementWise = vectorBinaryOp (applyBinaryOp (/))

-- Element-wise matrix division
matrixDivideElementWise :: Stack -> Stack
matrixDivideElementWise = matrixBinaryOp (applyBinaryOp (/))

divideOperator :: Stack -> Stack
divideOperator stack =
  case stack of
    Stack (b:a:rest) -> 
      case (a, b) of
        (Matrix_ _, Matrix_ _) -> matrixDivideElementWise stack
        (Vector_ _, Vector_ _) -> vectorDivideElementWise stack
        _ -> arithmeticOperator div (/) stack
    _ -> arithmeticOperator div (/) stack

exponentOperator :: Stack -> Stack
exponentOperator = arithmeticOperator (^) (**)

moduloOperator :: Stack -> Stack
moduloOperator = arithmeticOperator mod (\x y -> fromIntegral (round x `mod` round y))

dropOperator :: Stack -> Stack
dropOperator stack = 
  let (_, newStack) = pop stack in newStack

dupOperator :: Stack -> Stack
dupOperator stack =
  let (token, _) = pop stack
  in case token of
       Just token -> push token stack
       Nothing -> stack  -- If the stack is empty

swapOperator :: Stack -> Stack
swapOperator stack =
  let
    (b, stack1) = pop stack
    (a, stack2) = pop stack1
  in case (a, b) of
       (Just a, Just b) -> push a (push b stack2)
       _ -> stack  -- If there are no two elements

rotOperator :: Stack -> Stack
rotOperator (Stack (c:b:a:stack)) = Stack (a:c:b:stack)
rotOperator stack = stack

rollHelper :: Bool -> Stack -> Stack
rollHelper isRoll (Stack (num:stack)) =
  case num of
    Int_ n -> 
      if n <= 0 || n > length stack then 
        Stack (num:stack)
      else
        let 
          (items, rest) = splitAt n stack
          rotated = if isRoll
                    then last items : init items  -- ROLL
                    else tail items ++ [head items]  -- ROLLD
        in
          Stack (rotated ++ rest)
    _ -> Stack (num:stack)
rollHelper _ stack = stack

rollOperator :: Stack -> Stack
rollOperator = rollHelper True

rolldOperator :: Stack -> Stack
rolldOperator = rollHelper False

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
    _ -> Stack (b:a:stack)  -- return when both are not booleans
booleanOperator _ stack = stack

comparisonOperator :: String -> Stack -> Stack
comparisonOperator op (Stack (b:a:stack)) =
  let result = case op of
        "==" -> Bool_ (a == b)
        "!=" -> Bool_ (a /= b)
        ">" -> Bool_ (toString a > toString b)
        "<" -> Bool_ (toString a < toString b)
        ">=" -> Bool_ (toString a >= toString b)
        "<=" -> Bool_ (toString a <= toString b)
        "<=>" -> Int_ (case compare (toString a) (toString b) of
                         LT -> -1
                         EQ -> 0
                         GT -> 1)
        _ -> Bool_ False
  in Stack (result : stack)
comparisonOperator _ stack = stack

ifElseOperator :: Stack -> Stack
ifElseOperator (Stack (condition:falseVal:trueVal:stack)) =
  case condition of
  Bool_ b -> if b then Stack (trueVal : stack) else Stack (falseVal : stack)
  Int_ n -> if n /= 0 then Stack (trueVal : stack) else Stack (falseVal : stack)
  Float_ f -> if f /= 0.0 then Stack (trueVal : stack) else Stack (falseVal : stack)
  String_ s -> if s /= "" then Stack (trueVal : stack) else Stack (falseVal : stack)
  _ -> Stack (condition:falseVal:trueVal:stack)  -- Invalid condition
ifElseOperator stack = stack 

xorOperator :: Stack -> Stack
xorOperator (Stack (b:a:stack)) =
  case (a, b) of
    (Int_ x, Int_ y) -> Stack (Int_ (x `xor` y) : stack)  --  XOR for integers
    (Bool_ x, Bool_ y) -> Stack (Bool_ (x /= y) : stack)  --  XOR for booleans
    _ -> Stack (b:a:stack)  -- If not matching types
xorOperator stack = stack

bitshiftOperator :: String -> Stack -> Stack
bitshiftOperator op (Stack (b:a:stack)) =
  case (a, b) of
    (Int_ x, Int_ y) ->
      let result = case op of
            "<<" -> Int_ (x `shiftL` y)  -- Left shift
            ">>" -> Int_ (x `shiftR` y)  -- Right shift
            _ -> Int_ 0
      in Stack (result : stack)
    _ -> Stack (b:a:stack)  -- If not both integers
bitshiftOperator _ stack = stack

-- NOT operator (!)
notOperator :: Stack -> Stack
notOperator stack =
  let (a, stack1) = pop stack
  in case a of
    Just (Bool_ b) -> push (Bool_ (not b)) stack1
    Just (Int_ n) -> push (Bool_ (n == 0)) stack1  -- Convert to boolean (0 is False)
    Just (Float_ f) -> push (Bool_ (f == 0.0)) stack1 -- Convert to boolean (0.0 is False)
    Just (String_ s) -> push (Bool_ (s == "")) stack1 -- Convert to boolean (empty string is False)
    _ -> stack -- Not enough values or wrong type

-- One's complement operator (~)
complementOperator :: Stack -> Stack
complementOperator stack =
  let (a, stack1) = pop stack
  in case a of
    Just (Int_ n) -> push (Int_ (complement n)) stack1
    _ -> stack -- Not enough values or wrong type

-- Parse a vector from a string like "[1, 2, 3]"
parseVector :: String -> Token
parseVector str = 
    let content = takeWhile (/= ']') $ dropWhile (/= '[') str
        cleanContent = tail content  -- Remove the leading '['
        elements = splitByComma cleanContent
        tokens = map parseToken elements
    in Vector_ tokens
    where 
        trim = filter (not . isSpace)
        splitByComma [] = []
        splitByComma s = 
            let (first, rest) = break (== ',') s
            in trim first : if null rest then [] else splitByComma (tail rest)

-- Improved parseMatrix that better handles the matrix syntax
parseMatrix :: String -> Token
parseMatrix str = 
    let trimmed = filter (not . isSpace) str
        -- Remove outer brackets
        content = init (tail trimmed)
        rowsStr = splitRows content
        rows = map parseRowVector rowsStr
    in Matrix_ rows
    where
        -- Parse a row as a vector
        parseRowVector rowStr = 
            case parseVector ("[" ++ rowStr ++ "]") of
                Vector_ tokens -> tokens
                _ -> []
                
        -- Split the matrix string into rows
        splitRows :: String -> [String]
        splitRows str = 
            let parts = splitByBrackets str
            in parts
            
        -- Split by top-level brackets
        splitByBrackets :: String -> [String]
        splitByBrackets [] = []
        splitByBrackets ('[':rest) = 
            let (row, remaining) = getBracketContent rest 0 ""
            in row : if null remaining then [] 
                     else if head remaining == ',' 
                          then splitByBrackets (tail remaining)
                          else splitByBrackets remaining
        splitByBrackets (_:rest) = splitByBrackets rest
        
        -- Extract content between matching brackets
        getBracketContent :: String -> Int -> String -> (String, String)
        getBracketContent [] _ acc = (acc, "")
        getBracketContent (']':rest) 0 acc = (acc, rest)
        getBracketContent ('[':rest) depth acc = 
            getBracketContent rest (depth + 1) (acc ++ "[")
        getBracketContent (']':rest) depth acc =
            getBracketContent rest (depth - 1) (acc ++ "]")
        getBracketContent (c:rest) depth acc =
            getBracketContent rest depth (acc ++ [c])

-- Parse a lambda from a string like "{2 | x0 x1 +}"
parseLambda :: String -> Token
parseLambda str = 
    let -- Prepare the string for parsing
        trimmed = filter (\c -> c /= '\n' && c /= '\t') str
        -- Remove outer braces
        content = init (tail trimmed)
        -- Split at the pipe
        parts = break (== '|') content
        arityStr = filter (not . isSpace) (fst parts)
        body = if null (snd parts) then "" else tail (snd parts)  -- Remove the |
        
        -- Parse the arity
        arity = case readMaybe arityStr :: Maybe Int of
                  Just n -> n
                  Nothing -> 0
                  
        -- Tokenize the body
        bodyTokens = words body
    in Lambda_ (Lambda arity bodyTokens)

-- Check if a string represents a vector (starts with '[' and ends with ']')
isVector :: String -> Bool
isVector str = not (null str) && head str == '[' && last str == ']' && notMatrix str

-- Check if a string represents a matrix
isMatrix :: String -> Bool
isMatrix str = not (null str) && head str == '[' && 
               length str >= 2 && str !! 1 == '[' &&
               last str == ']'

-- Check if a string is not a matrix
notMatrix :: String -> Bool
notMatrix str = not (length str >= 2 && str !! 1 == '[')

-- Check if a string represents a lambda (starts with '{' and ends with '}')
isLambda :: String -> Bool
isLambda str = not (null str) && head str == '{' && last str == '}' && elem '|' str

-- Vector cross product (only for 3D vectors)
vectorCrossOperator :: Stack -> Stack
vectorCrossOperator stack =
    let (b, stack1) = pop stack
        (a, stack2) = pop stack1
    in case (a, b) of
        (Just (Vector_ v1), Just (Vector_ v2)) -> 
            if length v1 == 3 && length v2 == 3 
            then 
                let getValue (Int_ n) = fromIntegral n
                    getValue (Float_ f) = f
                    getValue _ = 0
                    
                    [a1, a2, a3] = map getValue v1
                    [b1, b2, b3] = map getValue v2
                    
                    -- Cross product formula: a × b = (a2b3 - a3b2, a3b1 - a1b3, a1b2 - a2b1)
                    c1 = a2 * b3 - a3 * b2
                    c2 = a3 * b1 - a1 * b3
                    c3 = a1 * b2 - a2 * b1
                    
                    -- Convert back to tokens
                    makeToken x = if fromIntegral (round x) == x 
                                  then Int_ (round x) 
                                  else Float_ x
                                  
                    result = Vector_ [makeToken c1, makeToken c2, makeToken c3]
                in push result stack2
            else stack  -- Not 3D vectors
        _ -> stack

-- Cross product operator for vectors
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
    let (a, stack1) = pop stack
    in case a of
        Just (Matrix_ m) -> 
            if not (null m) && not (null (head m))
            then
                -- Use our transpose function to flip rows and columns
                let result = transpose m
                in push (Matrix_ result) stack1
            else stack  -- Empty matrix
        _ -> stack  -- Not a matrix

-- Execute the body of a lambda function with an environment
executeLambdaBody :: [String] -> [(String, Token)] -> Lambda -> Stack -> Stack
executeLambdaBody [] _ _ stack = stack
executeLambdaBody (token:tokens) env lambda stack =
    -- Check for SELF keyword, which refers to the lambda itself
    if token == "SELF"
    then
        -- Push the lambda function onto the stack and continue execution
        let newStack = push (Lambda_ lambda) stack
        in executeLambdaBody tokens env lambda newStack
    -- Check if the token is a variable reference (x0, x1, etc.)
    else if not (null token) && head token == 'x' && 
            all isDigit (tail token) 
    then
        -- Look up the variable in the environment
        case lookup token env of
            Just value -> executeLambdaBody tokens env lambda (push value stack)
            Nothing -> executeLambdaBody tokens env lambda stack  -- Variable not found
    else
        -- Process regular token
        let newStack = processToken token stack
        in executeLambdaBody tokens env lambda newStack

-- Evaluate a lambda function
evalLambda :: Lambda -> Stack -> Stack
evalLambda lambda@(Lambda arity bodyTokens) stack =
    -- Pop the required number of arguments
    let (args, remainingStack) = popN arity stack
        -- Reverse args to match parameter order (x0 is the first parameter)
        reversedArgs = reverse args
        
        -- Create a new environment with the arguments
        env = zip (map (\i -> "x" ++ show i) [0..(arity-1)]) reversedArgs
        
        -- Execute the body tokens with the environment, passing the lambda for SELF reference
        resultStack = executeLambdaBody bodyTokens env lambda remainingStack
    in resultStack

-- Evaluate an operator or lambda that's on the stack
evalOperator :: Stack -> Stack
evalOperator stack =
    let (a, stack1) = pop stack
    in case a of
        Just (Lambda_ lambda) -> evalLambda lambda stack1
        Just (Op_ op) -> 
            -- Apply the operator
            case op of
                "+" -> additionOperator stack1
                "-" -> subtractionOperator stack1
                "*" -> multiplyOperator stack1
                "/" -> divideOperator stack1
                "**" -> exponentOperator stack1
                "%" -> moduloOperator stack1
                "DROP" -> dropOperator stack1
                "DUP" -> dupOperator stack1
                "SWAP" -> swapOperator stack1
                "ROT" -> rotOperator stack1
                "ROLL" -> rollOperator stack1
                "ROLLD" -> rolldOperator stack1
                "==" -> comparisonOperator "==" stack1
                "!=" -> comparisonOperator "!=" stack1
                ">" -> comparisonOperator ">" stack1
                "<" -> comparisonOperator "<" stack1
                ">=" -> comparisonOperator ">=" stack1
                "<=" -> comparisonOperator "<=" stack1
                "<=>" -> comparisonOperator "<=>" stack1
                "IFELSE" -> ifElseOperator stack1
                "<<" -> bitshiftOperator "<<" stack1
                ">>" -> bitshiftOperator ">>" stack1
                "&" -> booleanOperator "&" stack1
                "|" -> booleanOperator "|" stack1
                "^" -> xorOperator stack1
                "!" -> notOperator stack1
                "~" -> complementOperator stack1
                "x" -> crossProductOperator stack1
                "TRANSP" -> matrixTranspose stack1
                _ -> stack1  -- Unknown operator
        _ -> stack  -- Not an operator or lambda

-- Process a token, handling quoted tokens and EVAL
processToken :: String -> Stack -> Stack
processToken token stack =
  -- Handle quoted tokens: if token begins with a single quote (')
  if not (null token) && head token == '\'' && length token > 1
    then 
      -- Remove the quote and push the token as is (without evaluation)
      let unquotedToken = tail token
      in push (parseToken unquotedToken) stack
  else if isLambda token
    then 
      -- Parse and immediately evaluate the lambda
      let lambda = parseLambda token
      in case lambda of
          Lambda_ l -> evalLambda l stack
          _ -> push lambda stack  -- Shouldn't happen, but just in case
  else if isMatrix token
    then push (parseMatrix token) stack
  else if isVector token
    then push (parseVector token) stack
  else case parseToken token of
    Op_ op -> 
      case op of
        "+" -> additionOperator stack
        "-" -> subtractionOperator stack
        "*" -> multiplyOperator stack
        "/" -> divideOperator stack
        "**" -> exponentOperator stack
        "%" -> moduloOperator stack
        "DROP" -> dropOperator stack
        "DUP" -> dupOperator stack
        "SWAP" -> swapOperator stack
        "ROT" -> rotOperator stack
        "ROLL" -> rollOperator stack
        "ROLLD" -> rolldOperator stack
        "==" -> comparisonOperator "==" stack
        "!=" -> comparisonOperator "!=" stack
        ">" -> comparisonOperator ">" stack
        "<" -> comparisonOperator "<" stack
        ">=" -> comparisonOperator ">=" stack
        "<=" -> comparisonOperator "<=" stack
        "<=>" -> comparisonOperator "<=>" stack
        "IFELSE" -> ifElseOperator stack
        "<<" -> bitshiftOperator "<<" stack
        ">>" -> bitshiftOperator ">>" stack
        "&" -> booleanOperator "&" stack
        "|" -> booleanOperator "|" stack
        "^" -> xorOperator stack
        "!" -> notOperator stack       
        "~" -> complementOperator stack 
        "x" -> crossProductOperator stack
        "TRANSP" -> matrixTranspose stack
        "EVAL" -> evalOperator stack
    t -> push t stack

-- Parse a string token into a specific type
parseToken :: String -> Token
parseToken str = 
  if isLambda str then parseLambda str
  else if isMatrix str then parseMatrix str
  else if isVector str then parseVector str
  else case str of
  "true" -> Bool_ True
  "false" -> Bool_ False
  "+" -> Op_ "+"
  "-" -> Op_ "-"
  "*" -> Op_ "*"
  "/" -> Op_ "/"
  "**" -> Op_ "**"
  "%" -> Op_ "%"
  "DROP" -> Op_ "DROP"
  "DUP" -> Op_ "DUP"
  "SWAP" -> Op_ "SWAP"
  "ROT" -> Op_ "ROT"
  "ROLL" -> Op_ "ROLL"
  "ROLLD" -> Op_ "ROLLD"
  "==" -> Op_ "=="
  "!=" -> Op_ "!="
  ">" -> Op_ ">"
  "<" -> Op_ "<"
  ">=" -> Op_ ">="
  "<=" -> Op_ "<="
  "<=>" -> Op_ "<=>"
  "IFELSE" -> Op_ "IFELSE"
  "&" -> Op_ "&"
  "|" -> Op_ "|"
  "^" -> Op_ "^"
  "<<" -> Op_ "<<"
  ">>" -> Op_ ">>"
  "!" -> Op_ "!" 
  "~" -> Op_ "~"  
  "x" -> Op_ "x"
  "TRANSP" -> Op_ "TRANSP"
  "EVAL" -> Op_ "EVAL"
  _ | length str >= 2 && head str == '"' && last str == '"' -> String_ (init (tail str))  -- Remove the quotes
    | otherwise -> case readMaybe str :: Maybe Int of
        Just n  -> Int_ n
        Nothing -> case readMaybe str :: Maybe Float of
          Just f  -> Float_ f
          Nothing -> String_ str

toString :: Token -> String
toString (Int_ n) = show n
toString (Float_ f) = show f
toString (String_ s) = "\"" ++ s ++ "\""
toString (Bool_ True) = "true"
toString (Bool_ False) = "false"
toString (Op_ op) = op
toString (Vector_ vs) = "[" ++ intercalate ", " (map toString vs) ++ "]"
toString (Matrix_ rows) = 
    "[[" ++ intercalate "], [" (map (\row -> intercalate ", " (map toString row)) rows) ++ "]]"
toString (Lambda_ (Lambda arity bodyTokens)) = 
    "{" ++ show arity ++ " | " ++ unwords bodyTokens ++ "}"

tokenize :: String -> [String]
tokenize = go []
  where
    go acc [] = reverse acc
    go acc xs@(c:cs)
     
      | isSpace c = go acc (dropWhile isSpace xs)  -- skip leading spaces

      | c == '"' =  -- quoted string: grab up to the next '"'
          let (inside, rest) = span (/= '"') cs
              token = '"' : inside ++ "\""      -- include both quotes
          in go (token : acc) (drop 1 rest)    -- drop the closing "

      -- Lambda token handling (starts with "{")
      | c == '{' =
          let (lambdaContent, rest) = getLambdaContent xs 0 ""
              token = lambdaContent  -- include the braces
          in go (token : acc) rest  -- move past the lambda

      -- Matrix token handling (starts with "[[")
      | c == '[' && not (null cs) && head cs == '[' =
          let 
              (content, rest) = getMatrixContent xs 0 ""
              token = content      -- include all brackets
          in 
              go (token : acc) rest    -- move past the matrix

      -- Vector token handling (starts with "[" but not "[[")
      | c == '[' =
          let 
              (content, rest) = span (/= ']') xs
              token = content ++ "]"      -- include the closing bracket
          in 
              go (token : acc) (drop 1 rest)    -- drop the closing bracket

      -- Handle quoted tokens (starts with a quote mark)
      | c == '\'' =
          let (tok, rest) = break isSpace xs
          in go (tok : acc) rest

      -- unquoted word: grab until next space
      | otherwise =
          let (tok, rest) = break isSpace xs
          in go (tok : acc) rest
          
    -- Helper to extract a complete matrix token with balanced brackets
    getMatrixContent [] _ acc = (acc, [])
    getMatrixContent (c:cs) depth acc
        | c == '[' = getMatrixContent cs (depth + 1) (acc ++ [c])
        | c == ']' = if depth == 1
                     then (acc ++ [c], cs)  -- Found the closing bracket of the matrix
                     else getMatrixContent cs (depth - 1) (acc ++ [c])
        | otherwise = getMatrixContent cs depth (acc ++ [c])
        
    -- Helper to extract a complete lambda token with balanced braces
    getLambdaContent [] _ acc = (acc, [])
    getLambdaContent (c:cs) depth acc
        | c == '{' = getLambdaContent cs (depth + 1) (acc ++ [c])
        | c == '}' = if depth == 1
                     then (acc ++ [c], cs)  -- Found the closing brace of the lambda
                     else getLambdaContent cs (depth - 1) (acc ++ [c])
        | otherwise = getLambdaContent cs depth (acc ++ [c])

-- Process the input string from input file and generate output
process :: String -> String
process input =
  let
    tokens = tokenize input   -- Split input into tokens 
    stack = foldl (flip processToken) emptyStack tokens  -- Processed token in the stack
    (Stack resultStack) =  stack 
    reversedStack = reverse resultStack
    result = unlines (map toString reversedStack) -- Convert to string
  in
      result

main = do 
  args <- getArgs
  let inputFile = head args
  let outputFile = "output" ++ drop 5 inputFile  -- Replace with "output"
  
  contents <- readFile inputFile
  writeFile outputFile (process contents)