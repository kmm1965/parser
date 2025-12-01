package kmm.utils;

import java.util.Optional;
import java.util.function.Supplier;

public class Alternative {
    public static <A> Optional<A> orElseGet(Optional<A> o, Supplier<Optional<A>> f){
        return o.isPresent() ? o : f.get();
    }
}
