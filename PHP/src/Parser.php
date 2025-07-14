<?php
declare(strict_types=1);

namespace MonadParser\Parser;

use MonadParser\Maybe\Maybe;

final class Parser
{
    public function __construct(callable $unp){
        $this->unp = $unp;
    }
    
    public function parse(string $inp) : Maybe {
        return ($this->unp)($inp);
    }
    
    /**
     * Functor implemenattion
     */
    public function map(callable $f) : Parser {
        return new Parser(function (string $inp) use($f){
            return $this->parse($inp)->map(function ($pair) use($f){
                return [$f($pair[0]), $pair[1]];
            });
        });
    }

    /**
     * Applicative implementation.
     */
    public function ap(callable $fp) : Parser {
        return $this->flatMap(function (callable $f) use ($fp){
            return $fp()->map($f);
        });
    }

    public function ap1(callable $fp) : Parser {
        return $this->ap_get(function () use ($fp){ return $fp(); });
    }

    /**
     * Monad implementation.
     */
    public function flatMap(callable $f) : Parser {
        return new Parser(function (string $inp) use($f){
            return $this->parse($inp)->flatMap(function ($pair) use($f){
                return $f($pair[0])->parse($pair[1]);
            });
        });
    }

    public function skip(callable $fp) : Parser {
        return $this->flatMap(function ($_) use($fp){ return $fp(); });
    }

    /**
     * Alternative implementation
     */
    public function orElseGet(callable $fp) : Parser {
        return new Parser(function (string $inp) use($fp){
            return $this->parse($inp)->orElseGet(function () use($fp, $inp){
                return $fp()->parse($inp);
            });
        });
    }

    public function orElse(Parser $p) : Parser {
        return $this->orElseGet(function () use ($p){ return $p; });
    }
    
    public static function pure($x) : Parser {
        return new Parser(function (string $inp) use ($x){ return Maybe::just([$x, $inp]); });
    }

    public static function empty() : Parser {
        return new Parser(function ($_){ return Maybe::nothing(); });
    }
    
    public function some() : Parser {
        return $this->map(function ($c){
            return function ($s) use($c){
                return $c . $s;
            };
        })->ap(function (){
            return $this->many();
        });
    }
    
    public function many() : Parser {
        return $this->Some()->orElse(Parser::empty_string());
    }
    
    public static function isWhitespace(string $c) : bool {
        //return IntlChar::iswhitespace($c);
        return $c == ' ';
    }

    public static function isDigit(string $c) : bool {
        //return IntlChar::isdigit($c);
        return ctype_digit($c);
    }

    public static function between(Parser $open, Parser $close, callable $fp) : Parser {
        return $open->skip($fp)->flatMap(function ($x) use($close){
            return $close->skip(function() use($x){
                return Parser::pure($x);
            });
        });
    }

    public static function anyChar() : Parser {
        return new Parser(function (string $inp){
            return strlen($inp) == 0 ?
                Maybe::nothing() :
                Maybe::just([$inp[0], substr($inp, 1)]);
        });
    }

    public static function satisfy(callable $pred) : Parser {
        return Parser::anyChar()->flatMap(function ($c) use($pred){
            return $pred($c) ? Parser::pure($c) : Parser::empty();
        });
    }

    public static function spaces() : Parser {
        return Parser::satisfy(function(string $c){ return Parser::isWhitespace($c); })->many();
    }

    public function token() : Parser {
        $spaces = Parser::spaces();
        return Parser::between($spaces, $spaces, function (){ return $this; });
    }
    
    static function rest(callable $fp, callable $ff, Parser $op, $a) : Parser {
        return $op->flatMap(function (callable $f) use($fp, $ff, $a){
            return $fp()->flatMap(function ($b) use($ff, $f, $a){
                return $ff($f($a, $b));
            });
        })->orElseGet(function () use($a){ return Parser::pure($a); });
    }
    
    function rest_l(Parser $op, $a) : Parser {
        return Parser::rest(function (){ return $this; },
            function ($b) use($op, $a){ return $this->rest_l($op, $b); }, $op, $a);
    }
    
    function rest_r(Parser $op, $a) : Parser {
        return Parser::rest(function () use($op){ return $this->chainr1($op); },
            function ($b){ return Parser::pure($b); }, $op, $a);
    }
    
    public function chainl1(Parser $op, bool $negate_first) : Parser {
        return $this->flatMap(function ($a) use($op, $negate_first){
            return $this->rest_l($op, $negate_first ? -$a : $a);
        });
    }

    public function chainr1(Parser $op) : Parser {
        return $this->flatMap(function ($a) use($op){
            return $this->rest_r($op, $a);
        });
    }

    public static function empty_string() : Parser {
        return Parser::pure("");
    }
}
