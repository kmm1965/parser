require_relative 'Parser'

def alnum = satisfy { |c| c.match(/\w/) != nil }

def char(c)
    satisfy { |x| x == c }
end

def symbol(c)
    char(c).token
end

def _name(n)
    alnum.some.and_then { |s| s == n ? Parser.pure(n) : Parser.empty }.token
end

def optional_s(p)
    p | empty_string
end

def digits = satisfy { |c| c.match(/\d/) != nil }.many

def sign = optional_s(char('+') | char('-'))

def double = sign.and_then {
        |sign_part| digits.and_then {
        |int_part| (optional_s(char('.') >> digits)).and_then {
        |frac_part| optional_s(
            ((char('e') | char('E')) >> sign).and_then {
                |exp_sign| satisfy { |c| c.match(/\d/) != nil }.some.and_then {
                |exp_digits| Parser.pure(exp_sign + exp_digits)
            }}).and_then {
        |exp_part| !int_part.empty? || !frac_part.empty? ?
            Parser.pure((sign_part + int_part +
                (!frac_part.empty? ? '.' + frac_part : "") +
                (!exp_part.empty? ? 'e' + exp_part : "")).to_f) :
            Parser.empty
    }}}}.token
