{-# LANGUAGE LambdaCase #-}

import Data.Char (isSpace, isAlphaNum, isDigit)
import Control.Applicative
import Numeric (readFloat)
import Data.Bifunctor
import Control.Monad

newtype Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = p >>= return . f

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = P $ \inp -> Just (x, inp)

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> q = pf >>= (<$> q)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (parse p >=> (\ (x, out) -> parse (f x) out))

instance Alternative Parser where
    -- empty :: Parser a
    empty = P $ const Nothing

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P $ \inp -> parse p inp <|> parse q inp

anyChar :: Parser Char
anyChar = P $ \case
    []     -> Nothing
    (x:xs) -> Just (x, xs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = do
    x <- anyChar
    if pred x then return x else empty

char :: Char -> Parser Char
char c = satisfy (== c)

spaces :: Parser String
spaces = many $ satisfy isSpace

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

token :: Parser a -> Parser a
token = between spaces spaces

symbol :: Char -> Parser Char
symbol c = token $ char c

optionalS :: Parser String -> Parser String
optionalS p = p <|> pure ""

optionalC :: Parser Char -> Parser String
optionalC p = optionalS $ (: []) <$> p

digits :: Parser String
digits = many $ satisfy isDigit

sign :: Parser String
sign = optionalC $ char '+' <|> char '-'

usign :: Parser String
usign = optionalC $ symbol '+' <|> symbol '-'

double :: Parser Double
double = token $ do
    int_part  <- digits
    frac_part <- optionalS $ char '.' >> digits
    exp_part  <- optionalS $ do
        exp_sign   <- (char 'e' <|> char 'E') >> sign
        exp_digits <- some $ satisfy isDigit
        return $ exp_sign ++ exp_digits
    if not (null int_part) || not (null frac_part)
        then return $ read $ int_part ++
            (if not (null frac_part) then '.':frac_part else "") ++
            (if not (null exp_part) then 'e':exp_part else "")
        else empty

list2Maybe :: [a] -> Maybe a
list2Maybe []  = Nothing
list2Maybe [x] = Just x

double2 :: Parser Double
double2 = token $ P (list2Maybe . readFloat)

rest :: Parser a -> (a -> Parser a) -> Parser (a -> a -> a) -> a -> Parser a
rest p ff op x = do { f <- op;
                      y <- p;
                      ff (f x y)
                    }
              <|> return x

chainl1 :: (Num a) => Parser a -> Parser (a -> a -> a) -> Bool -> Parser a
chainl1 p op negate_first = do { x <- p; rest_l $ if negate_first then -x else x }
  where rest_l = rest p rest_l op

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
  where
    scan   = do { x <- p; rest_r x }
    rest_r = rest scan return op

name :: String -> Parser String
name n = token $ do
    s <- some $ satisfy $ \c -> isAlphaNum c || c == '_'
    if s == n then return n else empty

sqr :: (Num a) => a -> a
sqr x = x * x

defObject :: String -> a -> Parser a
defObject n x = name n >> return x

functions :: [Parser (Double -> Double)]
functions = [
    defObject "sin"   sin,
    defObject "cos"   cos,
    defObject "asin"  asin,
    defObject "acos"  acos,
    defObject "sinh"  sinh,
    defObject "cosh"  cosh,
    defObject "asinh" asinh,
    defObject "acosh" acosh,
    defObject "tan"   tan,
    defObject "log"   log,
    defObject "exp"   exp,
    defObject "sqrt"  sqrt,
    defObject "sqr"   sqr
  ]

func :: Parser (Double -> Double)
func = foldl1 (<|>) functions

constants :: [Parser Double]
constants = [
    defObject "E"        2.71828182845904523536,  -- e
    defObject "PI"       pi,
    defObject "LOG2E"    1.44269504088896340736,  -- log2(e)
    defObject "LOG10E"   0.434294481903251827651, -- log10(e)
    defObject "LN2"      0.693147180559945309417, -- ln(2)
    defObject "LN10"     2.30258509299404568402,  -- ln(10)
    defObject "PI_2"     1.57079632679489661923,  -- pi/2
    defObject "PI_4"     0.785398163397448309616, -- pi/4
    defObject "1_PI"     0.318309886183790671538, -- 1/pi
    defObject "2_PI"     0.636619772367581343076, -- 2/pi
    defObject "2_SQRTPI" 1.12837916709551257390,  -- 2/sqrt(pi)
    defObject "SQRT2"    1.41421356237309504880,  -- sqrt(2)
    defObject "SQRT1_2"  0.707106781186547524401  -- 1/sqrt(2)
  ]

_const :: Parser Double
_const = foldl1 (<|>) constants

op2 :: Char -> (Double -> Double -> Double) -> Parser (Double -> Double -> Double)
op2 c f = symbol c >> return f

expr :: Parser Double
expr = do
    sgn <- usign
    chainl1 term (add <|> sub) (sgn == "-")
  where
    add = op2 '+' (+)
    sub = op2 '-' (-)

term :: Parser Double
term = chainl1 factor (mul <|> divide) False where
    mul    = op2 '*' (*)
    divide = op2 '/' (/)

factor0 :: Parser Double
factor0 = expr_in_brackets
    <|> func <*> expr_in_brackets
    <|> _const
    <|> double
  where expr_in_brackets = between (symbol '(') (symbol ')') expr

factor :: Parser Double
factor = factor0 `chainr1` pow where
    pow = op2 '^' fpow
    fpow x y = exp $ y * log x

calculate :: String -> Maybe Double
calculate s = (\(x, "") -> x) <$> parse expr s

main :: IO ()
main = do
    --putStrLn "Input expression: "
    --inp <- getLine
    let inp = "sin ( 2_SQRTPI * sqr ( 2 ) - 1 )" -- "7 - 1 - 2"
    case calculate inp of
        Nothing -> putStrLn "Wrong expression"
        Just a -> print a
