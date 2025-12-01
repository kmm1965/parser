package kmm.parser;

import kmm.utils.Pair;
import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ParserTests {
    @Test
    public void testParserPure() {
        assertEquals(Optional.of(Pair.of(1, "abc")), Parser.pure(1).parse("abc"));
        assertEquals(Optional.of(Pair.of(1.0, "abc")), Parser.pure(1.0).parse("abc"));
        assertEquals(Optional.of(Pair.of("1", "abc")), Parser.pure("1").parse("abc"));
    }

    @Test
    public void testParserMap() {
        assertEquals(Optional.of(Pair.of("1", "abc")), Parser.pure(1).map(String::valueOf).parse("abc"));
        assertEquals(Optional.of(Pair.of("1.0", "abc")), Parser.pure(1.0).map(String::valueOf).parse("abc"));
        assertEquals(Optional.of(Pair.of(1, "abc")), Parser.pure("1").map(Integer::valueOf).parse("abc"));


        assertEquals(Optional.empty(), Parser.<Integer>empty().map(String::valueOf).parse("abc"));
        assertEquals(Optional.empty(), Parser.<Double>empty().map(String::valueOf).parse("abc"));
        assertEquals(Optional.empty(), Parser.<String>empty().map(Integer::valueOf).parse("abc"));
    }

    @Test
    public void testParserApply() {
        Parser<Function<Double, Double>> psin = Parser.pure(Math::sin);
        Parser<Function<Double, Double>> pempty = Parser.empty();
        Supplier<Parser<Double>> fd = () -> Parser.pure(1.0);

        assertEquals(Optional.of(Pair.of(Math.sin(1.0), "abc")), Parser.apply(psin, fd).parse("abc"));
        assertEquals(Optional.empty(), Parser.apply(psin, Parser::empty).parse("abc"));
        assertEquals(Optional.empty(), Parser.apply(pempty, fd).parse("abc"));
        assertEquals(Optional.empty(), Parser.apply(pempty, Parser::empty).parse("abc"));
    }

    @Test
    public void testParserFlatMap() {
        Parser<Integer>
            i1 = Parser.pure(1),
            pempty = Parser.empty();
        Function<Integer, Parser<String>>
            eat = i -> new Parser<>(inp -> Optional.of(Pair.of(i + inp, ""))),
            cancel = _ -> new Parser<>(_ -> Optional.empty());

        assertEquals(Optional.of(Pair.of("1abc", "")), i1.flatMap(eat).parse("abc"));
        assertEquals(Optional.empty(), i1.flatMap(cancel).parse("abc"));
        assertEquals(Optional.empty(), pempty.flatMap(eat).parse("abc"));
        assertEquals(Optional.empty(), pempty.flatMap(cancel).parse("abc"));
    }
}
