let char_ = (c) => satisfy((x) => x === c)

let symbol = (c) => char_(c).token();

let optional_s = (p) => p.orElse(empty_string)

let alnum = satisfy((c) => /[a-zA-Z0-9_]/.test(c));

let identifier = alnum.some().token();

let name = (n) => identifier.flatMap((s) => s === n ? Parser.pure(n) : Parser.empty());

let digit = satisfy(isDigit);

let digits = digit.many();

let sign = optional_s(char_('+').orElseGet(() => char_('-')));

// Unary sign
let usign = sign.token();

let double_ = digits.flatMap(
    (int_part) => optional_s(char_('.').skip(() => digits)).flatMap(
    (frac_part) => optional_s(char_('e').orElse(char_('E')).skip(() => sign).flatMap(
      (exp_sign) => digit.some().flatMap(
      (exp_digits) => Parser.pure(exp_sign + exp_digits)))).flatMap(
    (exp_part) => int_part.length > 0 || frac_part.length > 0 ?
      Parser.pure(Number(int_part +
        (frac_part.length > 0 ? '.' + frac_part : "") +
        (exp_part.length > 0 ? 'e' + exp_part : ""))) :
      Parser.empty()))
  ).token();