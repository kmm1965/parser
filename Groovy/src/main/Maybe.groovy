package main

class Maybe {
    static Optional orElseGet(Optional o, Closure f){
        return o.isPresent() ? o : f() as Optional;
    }
}
