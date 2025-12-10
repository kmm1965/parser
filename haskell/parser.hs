{-# LANGUAGE LambdaCase #-}

import Data.Char (isSpace, isAlphaNum, isDigit)
import Control.Applicative
import Numeric (readFloat)
import Data.Bifunctor
import Control.Monad
import Data.Functor

newtype Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = p >>= return . f
    -- fmap = flip (>>=) . (return .) -- pointfree version

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = P $ \inp -> Just (x, inp)

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> q = pf >>= (<$> q)
    -- (<*>) = (. flip (<$>)) . (>>=) -- pointfree version

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
symbol = token . char

optionalS :: Parser String -> Parser String
optionalS p = p <|> pure ""

optionalC :: Parser Char -> Parser String
optionalC p = optionalS $ p <&> (: [])

digit :: Parser Char
digit = satisfy isDigit

digits :: Parser String
digits = many digit

sign :: Parser String
sign = optionalC $ char '+' <|> char '-'

-- Unary sign
usign :: Parser String
usign = token sign

double :: Parser Double
double = token $ do
    int_part  <- digits
    frac_part <- optionalS $ char '.' >> digits
    exp_part  <- optionalS $ do
        exp_sign   <- (char 'e' <|> char 'E') >> sign
        exp_digits <- some digit
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

identifier :: Parser String
identifier = (token . some . satisfy) (\c -> isAlphaNum c || c == '_')

sqr :: (Num a) => a -> a
sqr x = x * x

funcs :: Parser (Double -> Double)
funcs = do
    n <- identifier
    foldl1 (<|>) [
        guard (n == "sin")   $> sin,
        guard (n == "cos")   $> cos,
        guard (n == "asin")  $> asin,
        guard (n == "acos")  $> acos,
        guard (n == "sinh")  $> sinh,
        guard (n == "cosh")  $> cosh,
        guard (n == "asinh") $> asinh,
        guard (n == "acosh") $> acosh,
        guard (n == "tan")   $> tan,
        guard (n == "log")   $> log,
        guard (n == "exp")   $> exp,
        guard (n == "sqrt")  $> sqrt,
        guard (n == "sqr")   $> sqr]

consts :: Parser Double
consts = do
    n <- identifier
    foldl1 (<|>) [
        guard (n == "E")        $> 2.71828182845904523536,  -- e
        guard (n == "PI")       $> pi,
        guard (n == "LOG2E")    $> 1.44269504088896340736,  -- log2(e)
        guard (n == "LOG10E")   $> 0.434294481903251827651, -- log10(e)
        guard (n == "LN2")      $> 0.693147180559945309417, -- ln(2)
        guard (n == "LN10")     $> 2.30258509299404568402,  -- ln(10)
        guard (n == "PI_2")     $> 1.57079632679489661923,  -- pi/2
        guard (n == "PI_4")     $> 0.785398163397448309616, -- pi/4
        guard (n == "1_PI")     $> 0.318309886183790671538, -- 1/pi
        guard (n == "2_PI")     $> 0.636619772367581343076, -- 2/pi
        guard (n == "2_SQRTPI") $> 1.12837916709551257390,  -- 2/sqrt(pi)
        guard (n == "SQRT2")    $> 1.41421356237309504880,  -- sqrt(2)
        guard (n == "SQRT1_2")  $> 0.707106781186547524401] -- 1/sqrt(2)

expr :: Parser Double
expr = do
    sgn <- usign
    chainl1 term (add <|> sub) (sgn == "-")
  where
    add = symbol '+' $> (+)
    sub = symbol '-' $> (-)

term :: Parser Double
term = chainl1 factor (mul <|> divide) False where
    mul    = symbol '*' $> (*)
    divide = symbol '/' $> (/)

factor0 :: Parser Double
factor0 = expr_in_brackets
    <|> funcs <*> expr_in_brackets
    <|> consts
    <|> double
  where expr_in_brackets = between (symbol '(') (symbol ')') expr

factor :: Parser Double
factor = factor0 `chainr1` pow where
    pow = symbol '^' $> (**)

calculate :: String -> Maybe Double
calculate s = parse expr s <&> (\(x, "") -> x)

main :: IO ()
main = do
    --putStrLn "Input expression: "
    --inp <- getLine
    --let inp = "sin ( 2_SQRTPI * sqr ( 2 ) - 1 )" -- "7 - 1 - 2"
    --let inp = "7 - 1 - 2"
    --let inp = "( 2_SQRTPI * sqr ( 2 ) - 1 )"
    --let inp = "sqr(sin(2)) + sqr(cos(2))"
    --let inp = "3^2^3"
    let inp = "PI^E"
    case calculate inp of
        Nothing -> putStrLn "Wrong expression"
        Just a -> print a
