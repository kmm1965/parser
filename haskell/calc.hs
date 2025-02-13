import Data.Char (isSpace, isAlphaNum)
import Control.Applicative -- Alternative
import Numeric (readFloat)

newtype Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) inp = p inp

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P $ \inp -> (\(x, out) -> (f x, out)) <$> parse p inp

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = P $ \inp -> Just (x, inp)

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> q = do f <- pf; f <$> q

instance Monad Parser where
    return = pure
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    -- p >>= f = P $ \inp -> do (x, out) <- parse p inp; parse (f x) out
    p >>= f = P $ \inp -> parse p inp >>= (\(x, out) -> parse (f x) out)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P $ \_ -> Nothing

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P $ \inp -> parse p inp <|> parse q inp

anyChar :: Parser Char
anyChar = P $ \inp -> case inp of
    []     -> Nothing
    (x:xs) -> Just (x, xs)
  
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
    x <- anyChar
    if f x then return x else empty

space :: Parser String
space = many $ satisfy isSpace

token :: Parser a -> Parser a
token p = do
    space; q <- p
    space; return q

symbol :: Char -> Parser Char
symbol c = token (char c) where
    char :: Char -> Parser Char
    char x = satisfy (== x)

list2Maybe :: [a] -> Maybe a
list2Maybe []  = Nothing
list2Maybe [x] = Just x

double :: Parser Double
double = P (list2Maybe . readFloat)
    
rest :: a -> Parser (a -> a -> a) -> Parser a -> (a -> Parser a) -> Parser a
rest x op p ff = do { f <- op;
                      y <- p;
                      ff (f x y)
                    }
              <|> return x

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do { x <- p; rest_l x }
  where rest_l x = rest x op p rest_l

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
  where
    scan   = do { x <- p; rest_r x }
    rest_r x = rest x op scan return

name :: String -> Parser String
name n = do
    s <- some alnum
    if s == n then return s else empty
  where
    alnum :: Parser Char
    alnum = satisfy $ \c -> isAlphaNum c || c == '_'

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
    open;  x <- p
    close; return x

sqr :: (Num a) => a -> a
sqr x = x * x

def_object :: String -> a -> Parser a
def_object n x = name n >> return x

functions :: [Parser (Double -> Double)]
functions = [
    def_object "sin"   sin,
    def_object "cos"   cos,
    def_object "asin"  asin,
    def_object "acos"  acos,
    def_object "sinh"  sinh,
    def_object "cosh"  cosh,
    def_object "asinh" asinh,
    def_object "acosh" acosh,
    def_object "tan"   tan,
    def_object "log"   log,
    def_object "exp"   exp,
    def_object "sqrt"  sqrt,
    def_object "sqr"   sqr
  ]

func :: Parser (Double -> Double)
func = foldl1 (<|>) functions

m_E        = 2.71828182845904523536   -- e
m_LOG2E    = 1.44269504088896340736   -- log2(e)
m_LOG10E   = 0.434294481903251827651  -- log10(e)
m_LN2      = 0.693147180559945309417  -- ln(2)
m_LN10     = 2.30258509299404568402   -- ln(10)
-- m_PI       = 3.14159265358979323846   -- pi
m_PI_2     = 1.57079632679489661923   -- pi/2
m_PI_4     = 0.785398163397448309616  -- pi/4
m_1_PI     = 0.318309886183790671538  -- 1/pi
m_2_PI     = 0.636619772367581343076  -- 2/pi
m_2_SQRTPI = 1.12837916709551257390   -- 2/sqrt(pi)
m_SQRT2    = 1.41421356237309504880   -- sqrt(2)
m_SQRT1_2  = 0.707106781186547524401  -- 1/sqrt(2)

constants :: [Parser Double]
constants = [
    def_object "e"        m_E,
    def_object "log2e"    m_LOG2E,
    def_object "log10e"   m_LOG10E,
    def_object "ln2"      m_LN2,
    def_object "ln10"     m_LN10,
    def_object "pi"       pi,
    def_object "pi_2"     m_PI_2,
    def_object "pi_4"     m_PI_4,
    def_object "1_pi"     m_1_PI,
    def_object "2_pi"     m_2_PI,
    def_object "2_sqrtpi" m_2_SQRTPI,
    def_object "sqrt2"    m_SQRT2,
    def_object "sqrt1_2"  m_SQRT1_2
  ]

_const :: Parser Double
_const = foldl1 (<|>) constants

op2 :: Char -> (Double -> Double -> Double) -> Parser (Double -> Double -> Double)
op2 c f = symbol c >> return f
-- op2 c f = const f <$> symbol c

expr :: Parser Double
expr = term `chainl1` (add <|> sub) where
    add = op2 '+' (+)
    sub = op2 '-' (-)

term :: Parser Double
term = factor `chainl1` (mul <|> divide) where
    mul    = op2 '*' (*)
    divide = op2 '/' (/)

factor0 :: Parser Double
factor0 = expr_in_brackets
    <|> func <*> expr_in_brackets
    <|> _const
    <|> double
  where
    expr_in_brackets = between br_open br_close expr
    br_open  = symbol '('
    br_close = symbol ')'

factor :: Parser Double
factor = factor0 `chainr1` pow where
    pow = op2 '^' fpow
    fpow x y = exp $ y * log x

calculate :: String -> Maybe Double
calculate s = (\(x, "") -> x) <$> parse expr s

main :: IO ()
main = do
    putStrLn "Input expression: "
    inp <- getLine
    case calculate inp of
        Nothing -> putStrLn "Wrong expression"
        Just a -> print a
