import java.util.concurrent.Future

typealias Parser<T> = (String) -> Pair<T, String>?

object Parsec {
    fun <T> pure(t: T): Parser<T> = { t to it }

    infix fun <S, T> Parser<T>.bind(other: (T) -> Parser<S>): Parser<S> {
        return {
            this(it)?.let { (x, rest) ->
                other(x)(rest)
            }
        }
    }

    fun r(r: String): Parser<String> = {
        Regex("""^($r)(.*)$""").find(it)?.destructured?.let { (x, y) -> x to y }
    }

    fun <S, T> Parser<S>.map(f: (S) -> T): Parser<T> =
        this bind { pure(f(it)) }

    val int: Parser<Int> = r("""\d+""").map { it.toInt() }

    fun exactly(text: String): Parser<Unit> = {
        if (it.startsWith(text)) {
            Unit to it.removePrefix(text)
        } else {
            null
        }
    }

    operator fun <T> Parser<T>.plus(other: Parser<T>): Parser<T> = { this(it) ?: other(it) }
    operator fun <S, T> Parser<S>.times(other: Parser<T>): Parser<Pair<S, T>> =
        this bind { s ->
            other bind { u ->
                pure (s to u)
            }
        }

    fun <T: Any> maybe(p: Parser<T>): Parser<T?> {
        return {
            when (val result = p(it)) {
                null -> null to it
                else -> result
            }
        }
    }

    fun <T> Parser<T>.between(left: Parser<*>, right: Parser<*>): Parser<T> =
        (left * this * right).map { it.first.second }

    class Box<T>(var box: T?)

    val <T> Lazy<Parser<T>>.lazy: Parser<T>
        get() = {
            this.value(it)
        }


    fun <T: Any, S: Any> Parser<T>.list(sep: Parser<S>): Parser<List<T>> {
        val self = Box<Parser<List<T>>>(null)
        self.box = maybe(this) bind { head ->
            when (head) {
                null -> pure(listOf())
                else ->
                    maybe(sep) bind {
                        when (it) {
                            null -> pure(listOf(head))
                            else -> self.box!!.map { tail -> listOf(head) + tail }
                        }
                    }
            }
        }
        return self.box!!
    }
}

fun <T> String.parse(p: Parser<T>): T {
    val (x, y) = p(this) ?:
    throw IllegalArgumentException("unable to parse '$this'")
    require(y == "") { "residue input after parse: '$y'" }
    return x
}

