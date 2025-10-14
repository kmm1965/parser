class Maybe {
  constructor(val){
      this.__value = val;
  }

  static Just(val){
    return new Maybe(val);
  }

  static Nothing(){
    return Maybe.Just(null);
  }

  map(fn){
    return this.flatMap((a) => Maybe.Just(fn(a)));
  }
  
  flatMap(fn){
    return this.isNothing() ? this : fn(this.__value);
  }

  orElse(other){
    return this.isNothing() ? other : this;
  }

  orElseGet(fother){
    return this.isNothing() ? fother() : this;
  }

  isNothing(){
    return this.__value === null || this.__value === undefined;
  }

  ap(other){
    return this.flatMap((f) => other.map(f));
  }
  
  toString(){
    return this.isNothing() ? "Nothing" : "Just(" + this.__value.toString() + ")";
  }
}
