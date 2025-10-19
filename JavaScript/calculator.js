class Calculator {
  constructor(){
    this.add = Calculator.op2('+', (x, y) => x + y);
    this.sub = Calculator.op2('-', (x, y) => x - y);
    this.mul = Calculator.op2('*', (x, y) => x * y);
    this.div = Calculator.op2('/', (x, y) => x / y);
    this.pow = Calculator.op2('^', (x, y) => Math.exp(y * Math.log(x)));
    
    this.funcs = Calculator.fold([
      Calculator.def_object("sin", Math.sin),
      Calculator.def_object("cos", Math.cos),
      Calculator.def_object("asin", Math.asin),
      Calculator.def_object("acos", Math.acos),
      Calculator.def_object("sinh", Math.sinh),
      Calculator.def_object("cosh", Math.cosh),
      Calculator.def_object("asinh", Math.asinh),
      Calculator.def_object("acosh", Math.acosh),
      Calculator.def_object("tan", Math.tan),
      Calculator.def_object("log", Math.log),
      Calculator.def_object("log10", Math.log10),
      Calculator.def_object("exp", Math.exp),
      Calculator.def_object("sqrt", Math.sqrt),
      Calculator.def_object("sqr", (x) => x * x)
    ]);

    this.consts = Calculator.fold([
      Calculator.def_object("E",        Math.E),
      Calculator.def_object("PI",       Math.PI),
      Calculator.def_object("LOG2E",    1.44269504088896340736),  // log2(e)
      Calculator.def_object("LOG10E",   0.434294481903251827651), // log10(e)
      Calculator.def_object("LN2",      0.693147180559945309417), // ln(2)
      Calculator.def_object("LN10",     2.30258509299404568402),  // ln(10)
      Calculator.def_object("PI_2",     1.57079632679489661923),  // pi/2
      Calculator.def_object("PI_4",     0.785398163397448309616), // pi/4
      Calculator.def_object("1_PI",     0.318309886183790671538), // 1/pi
      Calculator.def_object("2_PI",     0.636619772367581343076), // 2/pi
      Calculator.def_object("2_SQRTPI", 1.12837916709551257390),  // 2/sqrt(pi)
      Calculator.def_object("SQRT2",    1.41421356237309504880),  // sqrt(2)
      Calculator.def_object("SQRT1_2",  0.707106781186547524401)  // 1/sqrt(2)
    ]);
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
  
  static def_object(n, value){
    return name(n).skip(() => Parser.pure(value));
  }
}
