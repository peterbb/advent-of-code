import java.lang.Integer.max
import java.math.BigInteger


typealias Matrix<T> = List<List<T>>

typealias Coord = Pair<Int, Int>

fun Sequence<String>.split(on: String) = map { it.split(on) }

val Sequence<List<String>>.pairs: Sequence<Pair<String, String>>
    get() = map { it[0] to it[1] }

fun <T : Comparable<T>> ClosedRange<T>.disjoint(other: ClosedRange<T>) =
    endInclusive < other.start || other.endInclusive < start

fun <T : Comparable<T>> ClosedRange<T>.overlap(other: ClosedRange<T>) = !disjoint(other)

fun <A, B> Sequence<Pair<A, A>>.mapPair(f: (A) -> B) =
    map { f(it.first) to f(it.second) }

fun String.splitPair(on: String): Pair<String, String> =
    this.split(on).asPair

val Pair<Int, Int>.asClosedRange: IntRange
    get() = IntRange(first, second)

fun <A : Comparable<A>> ClosedRange<A>.subsumes(other: ClosedRange<A>) =
    start <= other.start && other.endInclusive <= endInclusive
fun <A0, A1, B0, B1> Pair<A0, A1>.map(f: (A0) -> B0, g: (A1) -> B1): Pair<B0, B1> = f(first) to g(second)
fun <A, B> Pair<A, A>.map(f: (A) -> B): Pair<B, B> = map(f, f)

fun <T> Iterable<Set<T>>.bigIntersect(): Set<T> =
    reduce { a, b -> a.intersect(b) }

val <T> Pair<Set<T>, Set<T>>.intersect: Set<T>
    get() = first.intersect(second)

val CharSequence.split: Pair<CharSequence, CharSequence>
    get() =
        subSequence(0, length / 2) to subSequence(length / 2, length)

fun Sequence<String>.batchByNewline(): Sequence<List<String>> = sequence {
    var batch = mutableListOf<String>()
    for (line in this@batchByNewline.asIterable()) {
        if (line == "") {
            yield(batch)
            batch = mutableListOf()
        } else {
            batch.add(line)
        }
    }
    if (batch.isNotEmpty()) {
        yield(batch)
    }
}
fun <T> T.repeated(times: Int): Sequence<T> = sequence {
    repeat(times) {
        yield(this@repeated)
    }
}


fun <S, T, U> Iterable<Pair<S, T>>.map2(f: (S, T) -> U): Iterable<U> =
    map { f(it.first, it.second)}

fun <S, T, U> Sequence<Pair<S, T>>.mapRight(f: (T) -> U): Sequence<Pair<S, U>> =
    map { it.first to f(it.second) }

fun <T> createMutableList(size: Int, elem: (Int) -> T): MutableList<T> =
    createSequence(size, elem).toMutableList()

typealias MutableMatrix<T> = MutableList<MutableList<T>>

val <T> Matrix<T>.asMutableMatrix: MutableMatrix<T>
    get() = map { it.toMutableList() }.toMutableList()
fun <T> createMutableMatrix(width: Int, height: Int, elem: (Coord) -> T): MutableMatrix<T> =
    createMutableList(width) { x ->
        createMutableList(height) { y ->
            elem(x to y)
        }
    }

fun <T> createList(size: Int, elem: (Int) -> T): List<T> =
    createSequence(size, elem).toList()

fun <T> createSequence(size: Int, elem: (Int) -> T): Sequence<T> =
    sequence {
        repeat(size) { i ->
            yield(elem(i))
        }
    }

val <T> Matrix<T>.rotateRight: Matrix<T>
    get() {
        val elems = this
        val height = this.size
        val width = this.maxOf { it.size }

        return createList(width) { i ->
            createList(height) { j ->
                elems[height - 1 - j][i]
            }
        }
    }

fun <S, T> List<S>.zipWithNull(other: List<T>): List<Pair<S?, T?>> =
    createList(max(this.size, other.size)) {
        this.getOrNull(it) to other.getOrNull(it)
    }


val <T> List<T>.asPair: Pair<T, T>
    get() {
        require(size == 2)
        return this[0] to this[1]
    }

fun <T> Sequence<T>.inject(vararg elem: T): Sequence<T> = sequence {
    yieldAll(elem.asSequence())
    yieldAll(this@inject)
}

val Sequence<Int>.product: Int
    get() = fold(1) { a, b -> a * b }

fun <T> Sequence<T>.productOf(project: (T) -> Int): Int =
    map(project).product

fun Sequence<String>.findCoord(char: Char): Coord =
    withIndex().map { it.index to it.value.indexOf(char) }
        .find { it.second != -1 }!!


operator fun <A> Matrix<A>.get(coord: Coord): A =
    this[coord.first][coord.second]

val <T> Matrix<T>.coordinates: Sequence<Coord>
    get() =
        asSequence()
            .flatMapIndexed { i, row ->
                row.indices.map { j -> i to j }
            }



fun List<Int>.product(): Int = fold(1) { a, b -> a * b }
fun List<Long>.product(): Long = fold(1) { a, b -> a * b }
fun List<BigInteger>.product(): BigInteger = fold(BigInteger.ONE) { a, b -> a.times(b) }


val Pair<Long, Long>.product: Long
    get() = first * second
fun Char.parseInt() = code - '0'.code

val Int.nn: String
    get() = this.toString().padStart(2, '0')

inline fun <reified T: Enum<T>> findEnum(p: (T) -> Boolean): T? =
    enumValues<T>().find(p)!!
