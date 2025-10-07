from parser import Parser, satisfy

alnum = satisfy(lambda c: c.isalnum() or c == '_')

def name(n: str) -> Parser[str]:
    return alnum.some().and_then(lambda s: Parser.pure(n) if s == n else Parser.empty()).token()

def char(c: str):
   return satisfy(lambda x: x == c)

def symbol(c: str) -> Parser[str]:
    return char(c).token()

def optional_s(p: Parser[str]) -> "Parser[str]":
    return p | Parser.pure("")

digit = satisfy(str.isdigit)

digits = digit.many()

sign = optional_s(char('+') | char('-'))

usign = optional_s(symbol('+') | symbol('-'))

double = digits.and_then(
    lambda int_part: optional_s(char('.') >> digits).and_then(
    lambda frac_part: optional_s(((char('e') | char('E')) >> sign).and_then(
        lambda exp_sign: digit.some().and_then(
        lambda exp_digits: Parser.pure(exp_sign + exp_digits)))).and_then(
    lambda exp_part:
        Parser.pure(float(int_part +
            ('.' + frac_part if len(frac_part) > 0 else "") +
            ('e' + exp_part if len(exp_part) > 0 else "")))
        if len(int_part) > 0 or len(frac_part) > 0
        else Parser.empty()))).token()
