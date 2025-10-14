namespace MonadParser
{
    public class Maybe
    {
        /// <summary>
        /// Wraps an existing value in an Maybe&lt;T&gt; instance.
        /// </summary>
        /// <param name="value">The value to be wrapped.</param>
        /// <returns>An optional containing the specified value.</returns>
        public static Maybe<T> Some<T>(T value) => new Maybe<T>(value, true);

        /// <summary>
        /// Creates an empty Maybe&lt;T&gt; instance.
        /// </summary>
        /// <returns>An empty optional.</returns>
#pragma warning disable 8604
        public static Maybe<T> None<T>() => new Maybe<T>(default(T), false);
#pragma warning restore 8604
    }

    public struct Maybe<T>
    {
        private readonly bool hasValue;
        private readonly T value;

        /// <summary>
        /// Checks if a value is present.
        /// </summary>
        public bool HasValue => hasValue;

        internal T Value => value;
        internal Maybe(T value, bool hasValue)
        {
            this.value = value;
            this.hasValue = hasValue;
        }

        // Alternative
        public static Maybe<T> Empty() => Maybe.None<T>();

        public Maybe<T> OrElse(Maybe<T> other) => hasValue ? this : other;

        public static Maybe<T> operator |(Maybe<T> m, Maybe<T> other) => m.OrElse(other);

        public Maybe<T> OrElseGet(Func<Maybe<T>> fm) => hasValue ? this : fm();

        public static Maybe<T> operator |(Maybe<T> m, Func<Maybe<T>> fm) => m.OrElseGet(fm);

        public TResult Match<TResult>(Func<T, TResult> some, Func<TResult> none) => hasValue ? some(value) : none();

        public Maybe<TResult> Map<TResult>(Func<T, TResult> f) => FlatMap(a => Maybe<TResult>.Pure(f(a)));

        public Maybe<TResult> FlatMap<TResult>(Func<T, Maybe<TResult>> mapping) => Match(
            some: mapping,
            none: Maybe.None<TResult>);

        // Applicative
        public static Maybe<T> Pure(T value) => Maybe.Some(value);

        public Maybe<TResult> Apply<TResult>(Maybe<Func<T, TResult>> f)
        {
            Maybe<T> self = this;
            return f.FlatMap(func => self.Map(func));
        }
    }
}
