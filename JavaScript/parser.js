class Parser {
  constructor(p){
    this.parse = p;
  }

  map(fn){
    return new Parser((inp) => this.parse(inp).map(([a, s]) => [fn(a), s]));
  }

  flatMap(fn){
    return new Parser((inp) => this.parse(inp).flatMap(([a, s]) => fn(a).parse(s)));
  }
  
  skip(fp){
    return this.flatMap((_) => fp());
  }
  
  orElseGet(fp){
    return new Parser((inp) => this.parse(inp).orElseGet(() => fp().parse(inp)));
  }

  orElse(p){
    return this.orElseGet(() => p);
  }

  ap(fp){
    return this.flatMap((f) => fp().map(f));
  }
  
  static pure(x){
    return new Parser((inp) => Maybe.Just([x, inp]));
  }

  static empty(){
    return new Parser((_) => Maybe.Nothing());
  }

  some(){
    return this.map((c) => (s) => c + s).ap(() => this.many());
  }
  
  many(){
    return this.some().orElse(empty_string);
  }
  
  token(){
    return between(spaces, spaces, () => this);
  }

  static rest(fp, ff, op, a){
    return op.flatMap((f) => fp().flatMap((b) => ff(f(a, b))))
      .orElseGet(() => Parser.pure(a));
  }
  
  rest_l(op, a){
    return Parser.rest(() => this, (b) => this.rest_l(op, b), op, a);
  }

  rest_r(op, a){
    return Parser.rest(() => this.chainr1(op), Parser.pure, op, a);
  }
  
  chainl1(op, negate_first){
    return this.flatMap((a) => this.rest_l(op, negate_first ? -a : a));
  }

  chainr1(op){
    return this.flatMap((a) => this.rest_r(op, a));
  }
}

let empty_string = Parser.pure("");

let anyChar = new Parser((inp) => inp.length === 0 ? Maybe.Nothing() : Maybe.Just([inp[0], inp.substring(1)]));

let satisfy = (pred) => anyChar.flatMap((c) => pred(c) ? Parser.pure(c) : Parser.empty());

let isWhitespace = (c) => /\s/.test(c);

let isDigit = (c) => /\d/.test(c);

let spaces = satisfy(isWhitespace).many();

let between = (open, close, fp) => open.skip(fp).flatMap((x) => close.skip(() => Parser.pure(x)));
