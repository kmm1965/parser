<?php
declare(strict_types=1);

namespace MonadParser\Maybe\Constructor;

use MonadParser\Maybe\Maybe;

final class Nothing extends Maybe
{
    /**
     * Functor implemenattion
     */
    public function map(callable $_) : Maybe
    {
        return $this;
    }

    /**
     * Alternative implementation
     */
    public function orElseGet(callable $fthat) : Maybe
    {
        return $fthat();
    }

    /**
     * Monad implementation.
     * PHP implemenattion of Haskell Maybe's >>=.
     */
    public function flatMap(callable $f) : Maybe
    {
        return $this;
    }

    public function equals(Maybe $that) : bool
    {
        return $that instanceof Nothing;
    }

    /**
     * Fork this Maybe, with a default for Maybe.
     * @param mixed $default Returned for Nothing.
     * @return mixed For Nothing, this is $default.
     */
    public function fork($default)
    {
        return $default;
    }
}
