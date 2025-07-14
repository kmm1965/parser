<?php
declare(strict_types=1);

namespace MonadParser\Maybe\Constructor;

use MonadParser\Maybe\Maybe;

final class Just extends Maybe
{
    /**
     * Functor implemenattion
     */
    public function map(callable $f) : Maybe
    {
        return Maybe::just($f($this->value));
    }

    /**
     * Alternative implementation
     */
    public function orElseGet(callable $fthat) : Maybe
    {
        return $this;
    }

    /**
     * Monad implementation.
     * PHP implemenattion of Haskell Maybe's >>=.
     */
    public function flatMap(callable $f) : Maybe
    {
        return $f($this->value);
    }

    public function equals(Maybe $that) : bool
    {
        return $that instanceof Just
            && $this->value->equals($that->fork(null));
    }

    /**
     * Fork this Maybe, with a default for Nothing.
     * @param mixed $default In case of Nothing.
     * @return mixed Whatever the Maybe's inner type is.
     */
    public function fork($_)
    {
        return $this->value;
    }

    /**
     * The inner value for this Maybe.
     * @var mixed
     */
    private $value = null;

    protected function __construct($value)
    {
        $this->value = $value;
    }
    
    public function get(){
        return $this->value;
    }
}
