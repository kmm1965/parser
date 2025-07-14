<?php
declare(strict_types=1);

namespace MonadParser\Parser;

final class Calculator
{
    public function __construct()
    {
        $this->add = Calculator::op2('+', function ($x, $y){ return $x + $y; });
        $this->sub = Calculator::op2('-', function ($x, $y){ return $x - $y; });
        $this->mul = Calculator::op2('*', function ($x, $y){ return $x * $y; });
        $this->div = Calculator::op2('/', function ($x, $y){ return $x / $y; });
        $this->pow = Calculator::op2('^', function ($x, $y){ return exp($y * log($x)); });
        
        $this->funcs = Calculator::fold([
            Calculator::def_object("sin", function ($x){ return sin($x); }),
            Calculator::def_object("cos", function ($x){ return cos($x); }),
            Calculator::def_object("asin", function ($x){ return asin($x); }),
            Calculator::def_object("acos", function ($x){ return acos($x); }),
            Calculator::def_object("sinh", function ($x){ return sinh($x); }),
            Calculator::def_object("cosh", function ($x){ return cosh($x); }),
            Calculator::def_object("tan", function ($x){ return tan($x); }),
            Calculator::def_object("log", function ($x){ return log($x); }),
            Calculator::def_object("log10", function ($x){ return log10($x); }),
            Calculator::def_object("exp", function ($x){ return exp($x); }),
            Calculator::def_object("sqrt", function ($x){ return sqrt($x); }),
            Calculator::def_object("sqr", function ($x){ return $x * $x; })
        ]);

        $this->M_E        = 2.71828182845904523536;
        $this->M_PI       = 3.14159265358979323846;
        $this->M_LOG2E    = 1.44269504088896340736;  // log2(e)
        $this->M_LOG10E   = 0.434294481903251827651; // log10(e)
        $this->M_LN2      = 0.693147180559945309417; // ln(2)
        $this->M_LN10     = 2.30258509299404568402;  // ln(10)
        $this->M_PI_2     = 1.57079632679489661923;  // pi/2
        $this->M_PI_4     = 0.785398163397448309616; // pi/4
        $this->M_1_PI     = 0.318309886183790671538; // 1/pi
        $this->M_2_PI     = 0.636619772367581343076; // 2/pi
        $this->M_2_SQRTPI = 1.12837916709551257390;  // 2/sqrt(pi)
        $this->M_SQRT2    = 1.41421356237309504880;  // sqrt(2)
        $this->M_SQRT1_2  = 0.707106781186547524401; // 1/sqrt(2)

        $this->consts = Calculator::fold([
            Calculator::def_object("E",        $this->M_E),
            Calculator::def_object("PI",       $this->M_PI),
            Calculator::def_object("LOG2E",    $this->M_LOG2E),
            Calculator::def_object("LOG10E",   $this->M_LOG10E),
            Calculator::def_object("LN2",      $this->M_LN2),
            Calculator::def_object("LN10",     $this->M_LN10),
            Calculator::def_object("PI_2",     $this->M_PI_2),
            Calculator::def_object("PI_4",     $this->M_PI_4),
            Calculator::def_object("1_PI",     $this->M_1_PI),
            Calculator::def_object("2_PI",     $this->M_2_PI),
            Calculator::def_object("2_SQRTPI", $this->M_2_SQRTPI),
            Calculator::def_object("SQRT2",    $this->M_SQRT2),
            Calculator::def_object("SQRT1_2",  $this->M_SQRT1_2)
        ]);
    }
    
    static function op2(string $c, callable $f) : Parser {
        return SomeParsers::symbol($c)->skip(function () use($f){ return Parser::pure($f); });
    }
  
    static function def_object(string $n, $value) : Parser {
        return SomeParsers::name($n)->skip(function () use($value){ return Parser::pure($value); });
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
