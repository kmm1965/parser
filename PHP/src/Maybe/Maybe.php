<?php
declare(strict_types=1);

namespace MonadParser\Maybe;

use MonadParser\Maybe\Constructor\{Just, Nothing};

abstract class Maybe
{
    final public static function just($x) : Just
    {
        return new Just($x);
    }

    final public static function nothing() : Nothing
    {
        return new Nothing();
    }

    /**
     * Functor implemenattion
     */
    abstract public function map(callable $f) : Maybe;

    /**
     * Applicative implementation.
     */
    public function ap_get(callable $fthat) : Maybe
    {
        return $this->flatMap(function ($f) use ($fthat){ return $fthat()->map($f); });
    }

    public function ap(Maybe $that) : Maybe
    {
        return $this->ap_get(function () use ($that){ return $that; });
    }

    /**
     * Monad implementation.
     * PHP implemenattion of Haskell Maybe's >>=.
     */
    abstract public function flatMap(callable $f) : Maybe;

    /**
     * Alternative implementation
     */
    abstract public function orElseGet(callable $fthat) : Maybe;

    public function orElse(Maybe $that) : Maybe
    {
        return this->orElseGet(function () use ($that){ return $that; });
    }

    abstract public function equals(Maybe $that) : bool;

    /**
     * Fork this Maybe, with a default for Nothing.
     * @param mixed $default In case of Nothing.
     * @return mixed Whatever the Maybe's inner type is.
     */
    abstract public function fork($_);
}
