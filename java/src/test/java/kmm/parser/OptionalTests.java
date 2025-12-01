package kmm.parser;

import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class OptionalTests {
    static Optional<Double> safe_sqrt(double x){
        return x >= 0 ? Optional.of(Math.sqrt(x)) : Optional.empty();
    }

    static Optional<Double> safe_log(double x){
        return x > 0 ? Optional.of(Math.log(x)) : Optional.empty();
    }

    @Test
    public void test_safe_sqrt() {
        assertEquals(Optional.of(Math.sqrt(2.0)), safe_sqrt(2.0));
        assertEquals(Optional.of(0.0), safe_sqrt(0.0));
        assertEquals(Optional.empty(), safe_sqrt(-2.0));
    }

    @Test
    public void test_safe_log() {
        assertEquals(Optional.of(Math.log(2.0)), safe_log(2.0));
        assertEquals(Optional.empty(), safe_log(0.0));
        assertEquals(Optional.empty(), safe_log(-2.0));
    }

    @Test
    public void test_safe_sqrt_flatMap() {
        assertEquals(Optional.of(Math.sqrt(2.0)), Optional.of(2.0).flatMap(OptionalTests::safe_sqrt));
        assertEquals(Optional.of(0.0), Optional.of(0.0).flatMap(OptionalTests::safe_sqrt));
        assertEquals(Optional.empty(), Optional.of(-2.0).flatMap(OptionalTests::safe_sqrt));
        assertEquals(Optional.empty(), Optional.<Double>empty().flatMap(OptionalTests::safe_sqrt));
    }

    @Test
    public void test_safe_log_flatMap() {
        assertEquals(Optional.of(Math.log(2.0)), Optional.of(2.0).flatMap(OptionalTests::safe_log));
        assertEquals(Optional.empty(), Optional.of(0.0).flatMap(OptionalTests::safe_log));
        assertEquals(Optional.empty(), Optional.of(-2.0).flatMap(OptionalTests::safe_log));
        assertEquals(Optional.empty(), Optional.<Double>empty().flatMap(OptionalTests::safe_log));
    }

    static Optional<Double> safe_sqrt_safe_log(double x){
        return Optional.of(x)
            .flatMap(OptionalTests::safe_sqrt)
            .flatMap(OptionalTests::safe_log);
    }

    @Test
    public void test_safe_sqrt_flatMap_safe_log() {
        assertEquals(Optional.of(Math.log(Math.sqrt(2.0))), safe_sqrt_safe_log(2.0));
        assertEquals(Optional.empty(), safe_sqrt_safe_log(0.0));
        assertEquals(Optional.empty(), safe_sqrt_safe_log(-2.0));
    }

    static Optional<String> toString(int i) {
        return i % 2 == 0 ? Optional.of(String.valueOf(i)) : Optional.empty();
    }

    @Test
    public void test_toString() {
        assertEquals(Optional.of("2"), Optional.of(2).flatMap(OptionalTests::toString));
        assertEquals(Optional.empty(), Optional.of(1).flatMap(OptionalTests::toString));
        assertEquals(Optional.empty(), Optional.<Integer>empty().flatMap(OptionalTests::toString));
    }
}
