import Data.Char (isSpace, isAlphaNum, isDigit)
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
satisfy pred = do
    x <- anyChar
    if pred x then return x else empty

char :: Char -> Parser Char
char c = satisfy (== c)

spaces :: Parser String
spaces = many $ satisfy isSpace

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
    open;  x <- p
    close; return x

token :: Parser a -> Parser a
token p = between spaces spaces p

symbol :: Char -> Parser Char
symbol c = token $ char c

optional_s :: Parser String -> Parser String
optional_s p = p <|> pure ""

optional_c :: Parser Char -> Parser String
optional_c p = optional_s $ (\c -> [c]) <$> p

digits :: Parser String
digits = many $ satisfy isDigit

double :: Parser Double
double = token $ do
    sign_part <- sign
    int_part  <- digits
    frac_part <- optional_s $ char '.' >> digits
    exp_part  <- optional_s $ do
        exp_sign   <- (char 'e' <|> char 'E') >> sign
        exp_digits <- some $ satisfy isDigit
        return $ exp_sign ++ exp_digits
    if length int_part > 0 || length frac_part > 0
        then return $ read $ sign_part ++ int_part ++
            (if length frac_part > 0 then '.':frac_part else "") ++
            (if length exp_part > 0 then 'e':exp_part else "")
        else empty
    where
        sign :: Parser String
        sign = optional_c $ char '+' <|> char '-'

-- list2Maybe :: [a] -> Maybe a
-- list2Maybe []  = Nothing
-- list2Maybe [x] = Just x

-- double = token $ P (list2Maybe . readFloat)
    
rest :: Parser a -> (a -> Parser a) -> Parser (a -> a -> a) -> a -> Parser a
rest p ff op x = do { f <- op;
                      y <- p;
                      ff (f x y)
                    }
              <|> return x

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do { x <- p; rest_l x }
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
    def_object "E"        m_E,
    def_object "LOG2E"    m_LOG2E,
    def_object "LOG10E"   m_LOG10E,
    def_object "LN2"      m_LN2,
    def_object "LN10"     m_LN10,
    def_object "PI"       pi,
    def_object "PI_2"     m_PI_2,
    def_object "PI_4"     m_PI_4,
    def_object "1_PI"     m_1_PI,
    def_object "2_PI"     m_2_PI,
    def_object "2_SQRTPI" m_2_SQRTPI,
    def_object "SQRT2"    m_SQRT2,
    def_object "SQRT1_2"  m_SQRT1_2
  ]

_const :: Parser Double
_const = foldl1 (<|>) constants

op2 :: Char -> (Double -> Double -> Double) -> Parser (Double -> Double -> Double)
op2 c f = symbol c >> return f

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
  where expr_in_brackets = between (symbol '(') (symbol ')') expr

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
