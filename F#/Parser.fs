module Parser

open System
open Maybe

type Parser<'A>(unp: string -> Option<'A * string>) =
    member this.unp = unp

    member this.Parse (inp: string): Option<'A * string> = this.unp(inp)

    // Functor
    member this.Map (f: 'A -> 'B) = Parser(fun inp -> this.Parse inp |> (</>) (fun (pair: 'A * string) -> (f(fst pair), snd pair)))

    // Monad
    member this.FlatMap (f: 'A -> Parser<'B>): Parser<'B> = Parser(fun inp -> this.Parse inp >>= (fun (pair: 'A * string) -> f(fst pair).Parse(snd pair)))

    member this.Skip (p: Parser<'B>): Parser<'B> = this.FlatMap(fun unit -> p)

    member this.Skip (fp: unit -> Parser<'B>): Parser<'B> = this.FlatMap(fun unit -> fp())

    // Applicative
    static member Pure (x: 'A) = Parser(fun inp -> Some((x, inp)))

    static member Apply (pf: Parser<'A -> 'B>) (q: unit -> Parser<'A>): Parser<'B> = pf.FlatMap(fun f -> q().Map f)

    // Alternative
    static member Empty unit = Parser(fun inp -> None)

    member this.OrElse (p: Parser<'A>): Parser<'A> = Parser(fun inp -> this.Parse inp <|> fun unit -> p.Parse inp)

    member this.Some unit: Parser<string> =
        ((fun c -> (fun (s: string) -> c.ToString() + s)) |> this.Map |> Parser.Apply) this.Many 

    member this.Many unit: Parser<string> = (this.Some()).OrElse(Parser.Pure "")

    static member anyChar: Parser<char> = Parser(fun inp -> if inp.Length > 0 then Some (inp[0], inp.Substring 1) else None)

    static member Satisfy(pred: char -> bool): Parser<char> = Parser<char>.anyChar.FlatMap(fun (c: char) -> if pred c then Parser.Pure c else Parser<char>.Empty())

    static member alnum: Parser<char> = Parser<char>.Satisfy(fun c -> Char.IsLetterOrDigit(c) || c.Equals '_')

    static member spaces: Parser<string> = Parser<char>.Satisfy(Char.IsWhiteSpace).Many()

    member this.Token unit: Parser<'A> = Parser<string>.spaces.Skip(fun unit -> this).FlatMap(fun a -> Parser<string>.spaces.Skip(fun unit -> Parser.Pure a))

    static member Symbol(x: char): Parser<char> = Parser<char>.Satisfy(_.Equals(x)).Token()

    static member Name (n: string): Parser<string> = Parser<char>.alnum.Some().FlatMap(fun s -> if s.Equals(n) then Parser.Pure(n) else Parser<string>.Empty()).Token()

    static member natural: Parser<float> = (Parser<char>.Satisfy Char.IsDigit).Some().FlatMap(fun (s: string) -> Parser.Pure(Double.Parse s)).Token()

    static member Rest (fval: unit -> Parser<'A>) (ff: 'A -> Parser<'A>) (op: Parser<'A -> 'A -> 'A>) (a: 'A): Parser<'A> =
        op.FlatMap(fun f -> fval().FlatMap(fun b -> ff(f a b)))
          .OrElse(Parser.Pure a)

    member this.Rest_l (op: Parser<'A -> 'A -> 'A>) (a: 'A): Parser<'A> = Parser.Rest (fun unit -> this) (this.Rest_l op) op a

    member this.Rest_r (op: Parser<'A -> 'A -> 'A>) = Parser.Rest (fun unit -> this.Chainr1 op) Parser.Pure op

    member this.Chainl1 (op: Parser<'A -> 'A -> 'A>): Parser<'A> = this.FlatMap (this.Rest_l op)

    member this.Chainr1 (op: Parser<'A -> 'A -> 'A>): Parser<'A> = this.FlatMap (this.Rest_r op)

    static member Between (_open: Parser<'Open>) (_close: Parser<'Close>) (fp: unit -> Parser<'A>): Parser<'A> =
        _open.Skip(fp).FlatMap(fun a -> _close.Skip(Parser.Pure(a)))
