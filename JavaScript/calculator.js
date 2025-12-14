class Calculator {
  constructor(){
    this.add = Calculator.op2('+', (x, y) => x + y);
    this.sub = Calculator.op2('-', (x, y) => x - y);
    this.mul = Calculator.op2('*', (x, y) => x * y);
    this.div = Calculator.op2('/', (x, y) => x / y);
    this.pow = Calculator.op2('^', (x, y) => Math.exp(y * Math.log(x)));
    
    this.funcs = identifier.flatMap((n) => Calculator.fold([
      Calculator.guard(n === "sin",   Math.sin),
      Calculator.guard(n === "cos",   Math.cos),
      Calculator.guard(n === "asin",  Math.asin),
      Calculator.guard(n === "acos",  Math.acos),
      Calculator.guard(n === "sinh",  Math.sinh),
      Calculator.guard(n === "cosh",  Math.cosh),
      Calculator.guard(n === "asinh", Math.asinh),
      Calculator.guard(n === "acosh", Math.acosh),
      Calculator.guard(n === "tan",   Math.tan),
      Calculator.guard(n === "log",   Math.log),
      Calculator.guard(n === "log10", Math.log10),
      Calculator.guard(n === "exp",   Math.exp),
      Calculator.guard(n === "sqrt",  Math.sqrt),
      Calculator.guard(n === "sqr",   (x) => x * x)
    ]));

    this.consts = identifier.flatMap((n) => Calculator.fold([
      Calculator.guard(n === "E",        Math.E),
      Calculator.guard(n === "PI",       Math.PI),
      Calculator.guard(n === "LOG2E",    1.44269504088896340736),  // log2(e)
      Calculator.guard(n === "LOG10E",   0.434294481903251827651), // log10(e)
      Calculator.guard(n === "LN2",      0.693147180559945309417), // ln(2)
      Calculator.guard(n === "LN10",     2.30258509299404568402),  // ln(10)
      Calculator.guard(n === "PI_2",     1.57079632679489661923),  // pi/2
      Calculator.guard(n === "PI_4",     0.785398163397448309616), // pi/4
      Calculator.guard(n === "1_PI",     0.318309886183790671538), // 1/pi
      Calculator.guard(n === "2_PI",     0.636619772367581343076), // 2/pi
      Calculator.guard(n === "2_SQRTPI", 1.12837916709551257390),  // 2/sqrt(pi)
      Calculator.guard(n === "SQRT2",    1.41421356237309504880),  // sqrt(2)
      Calculator.guard(n === "SQRT1_2",  0.707106781186547524401)  // 1/sqrt(2)
    ]));
  }
  
  expr_in_brackets(){
    return between(symbol('('), symbol(')'), () => this.expr());
  }  
    
  expr(){
    return usign.flatMap((sgn) => this.term().chainl1(this.add.orElse(this.sub), sgn === '-'));
  }
    
  term(){
    return this.factor().chainl1(this.mul.orElse(this.div), false);
  }
    
  factor(){
    return this.factor0().chainr1(this.pow);
  }

  factor0(){
    return this.expr_in_brackets()
      .orElseGet(() => this.funcs.ap(() => this.expr_in_brackets()))
      .orElse(this.consts)
      .orElse(double_);
  }
  
  calc(inp){
    return this.expr().parse(inp);
  }
  
  static op2(c, f){
    return symbol(c).skip(() => Parser.pure(f));
  }
  
  static fold(parsers){
    let p0 = Parser.empty();
    for(let p of parsers){
      p0 = p0.orElse(p);
    }
    return p0;
  }
  
  static guard(b, value){
    return b ? Parser.pure(value) : Parser.empty();
  }
}
