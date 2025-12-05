package kmm.utils;

import static java.util.Objects.requireNonNull;

public record Pair<T1, T2>(T1 first, T2 second) {

    public static <T1, T2> Pair<T1, T2> of(T1 value1, T2 value2) {
        return new Pair<>(requireNonNull(value1), requireNonNull(value2));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Pair<?, ?> pair = (Pair<?, ?>) o;
        return first.equals(pair.first) && second.equals(pair.second);
    }

    @Override
    public String toString() {
        return "(" + first + ", " + second + ")";
    }
}