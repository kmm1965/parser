<?php
declare(strict_types=1);

namespace MonadParser\Parser;

use MonadParser\Maybe\Maybe;

final class SomeParsers
{
    public static function char_(string $c) : Parser {
        return Parser::satisfy(function (string $x) use($c) : bool {
            return $x == $c;
        });
    }

    public static function symbol(string $c) : Parser {
        return SomeParsers::char_($c)->token();
    }

    public static function optional_s(Parser $p) : Parser {
        return $p->orElseGet(function (){ return Parser::empty_string(); });
    }

    public static function alnum() : Parser {
        return Parser::satisfy(function(string $c) : bool {
            return ctype_alnum($c) || $c == '_';
        });
    }

    public static function identifier() : Parser {
        return SomeParsers::alnum()->some()->token();
    }

    public static function name(string $n) : Parser {
        return SomeParsers::identifier()->flatMap(function(string $s) use($n){
            return $s == $n ? Parser::pure($n) : Parser::empty();
        });
    }

    public static function digit() : Parser {
        return Parser::satisfy(function (string $c){
            return Parser::isDigit($c);
        });
    }

    public static function digits() : Parser {
        return SomeParsers::digit()->many();
    }

    public static function sign() : Parser {
        return SomeParsers::optional_s(SomeParsers::char_('+')->orElseGet(function (){
            return SomeParsers::char_('-');
        }));
    }

    // Unary sign
    public static function usign() : Parser {
        return SomeParsers::sign()->token();
    }

    public static function double_() : Parser
    {
        return SomeParsers::digits()->flatMap(
            function ($int_part){ return SomeParsers::optional_s(
                SomeParsers::char_('.')->skip(function (){ return SomeParsers::digits(); }))->flatMap(
            function ($frac_part) use($int_part){
                return SomeParsers::optional_s(SomeParsers::char_('e')->orElse(SomeParsers::char_('E'))->skip(
                        function (){ return SomeParsers::sign(); })->flatMap(
                    function ($exp_sign){
                        return SomeParsers::digit()->some()->flatMap(
                    function ($exp_digits) use($exp_sign){
                        return Parser::pure($exp_sign . $exp_digits); }); }) )->flatMap(
            function ($exp_part) use($int_part, $frac_part){
                return strlen($int_part) > 0 || strlen($frac_part) > 0 ?
                    Parser::pure(floatval($int_part .
                        (strlen($frac_part) > 0 ? '.' . $frac_part : "") .
                        (strlen($exp_part) > 0 ? 'e' . $exp_part : ""))) :
                    Parser::empty();
            }); }); })->token();
    }
}
