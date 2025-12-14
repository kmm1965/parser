<?php
declare(strict_types=1);

namespace MonadParser\Parser;

final class Calculator
{
    public  $add, $sub, $mul, $div, $pow, $funcs, $consts;

    public function __construct()
    {
        $this->add = Calculator::op2('+', function ($x, $y){ return $x + $y; });
        $this->sub = Calculator::op2('-', function ($x, $y){ return $x - $y; });
        $this->mul = Calculator::op2('*', function ($x, $y){ return $x * $y; });
        $this->div = Calculator::op2('/', function ($x, $y){ return $x / $y; });
        $this->pow = Calculator::op2('^', function ($x, $y){ return exp($y * log($x)); });
        
        $this->funcs = SomeParsers::identifier()->flatMap(function (string $n){
            return Calculator::fold([
                Calculator::guard($n == "sin",   function ($x){ return sin($x); }),
                Calculator::guard($n == "cos",   function ($x){ return cos($x); }),
                Calculator::guard($n == "asin",  function ($x){ return asin($x); }),
                Calculator::guard($n == "acos",  function ($x){ return acos($x); }),
                Calculator::guard($n == "sinh",  function ($x){ return sinh($x); }),
                Calculator::guard($n == "cosh",  function ($x){ return cosh($x); }),
                Calculator::guard($n == "asinh", function ($x){ return asinh($x); }),
                Calculator::guard($n == "acosh", function ($x){ return acosh($x); }),
                Calculator::guard($n == "tan",   function ($x){ return tan($x); }),
                Calculator::guard($n == "log",   function ($x){ return log($x); }),
                Calculator::guard($n == "log10", function ($x){ return log10($x); }),
                Calculator::guard($n == "exp",   function ($x){ return exp($x); }),
                Calculator::guard($n == "sqrt",  function ($x){ return sqrt($x); }),
                Calculator::guard($n == "sqr",   function ($x){ return $x * $x; })
            ]);
        });

        $this->consts = SomeParsers::identifier()->flatMap(function (string $n){
            return Calculator::fold([
                Calculator::guard($n == "E",        2.71828182845904523536),
                Calculator::guard($n == "PI",       3.14159265358979323846),
                Calculator::guard($n == "LOG2E",    1.44269504088896340736),  // log2(e)
                Calculator::guard($n == "LOG10E",   0.434294481903251827651), // log10(e)
                Calculator::guard($n == "LN2",      0.693147180559945309417), // ln(2)
                Calculator::guard($n == "LN10",     2.30258509299404568402),  // ln(10)
                Calculator::guard($n == "PI_2",     1.57079632679489661923),  // pi/2
                Calculator::guard($n == "PI_4",     0.785398163397448309616), // pi/4
                Calculator::guard($n == "1_PI",     0.318309886183790671538), // 1/pi
                Calculator::guard($n == "2_PI",     0.636619772367581343076), // 2/pi
                Calculator::guard($n == "2_SQRTPI", 1.12837916709551257390),  // 2/sqrt(pi)
                Calculator::guard($n == "SQRT2",    1.41421356237309504880),  // sqrt(2)
                Calculator::guard($n == "SQRT1_2",  0.707106781186547524401)  // 1/sqrt(2)
            ]);
        });
    }
    
    static function op2(string $c, callable $f) : Parser {
        return SomeParsers::symbol($c)->skip(function () use($f){ return Parser::pure($f); });
    }
  
    static function guard(bool $b, $value) : Parser {
        return $b ? Parser::pure($value) : Parser::empty();
    }

    static function fold($parsers) : Parser
    {
        $p0 = Parser::empty();
        foreach($parsers as $p){
            $p0 = $p0->orElse($p);
        }
        return $p0;
    }
  
    function expr_in_brackets() : Parser {
        return Parser::between(SomeParsers::symbol('('), SomeParsers::symbol(')'),
            function (){ return $this->expr(); });
    }  
    
    function expr() : Parser {
        return SomeParsers::usign()->flatMap(function (string $sgn){
            return $this->term()->chainl1($this->add->orElse($this->sub), $sgn == '-');
        });
    }
    
    function term() : Parser {
        return $this->factor()->chainl1($this->mul->orElse($this->div), false);
    }
    
    function factor() : Parser {
        return $this->factor0()->chainr1($this->pow);
    }

    function factor0() : Parser {
        return $this->expr_in_brackets()
            ->orElseGet(function(){ return $this->funcs->ap(function (){ return $this->expr_in_brackets(); }); })
            ->orElse($this->consts)
            ->orElseGet(function (){ return SomeParsers::double_(); });
    }

  public function calc(string $inp){
    return $this->expr()->parse($inp);
  }
}
