fun day14input(lines: Lines): MutableSet<Coord> =
    lines
        .split(" -> ")
        .flatMap {
            it
                .map { it.splitPair(",") .map(String::toInt) }
                .windowed(2)
                .flatMap { it.asPair.expandLine }
        }
        .toMutableSet()


fun day14part1(lines: Lines): Int {
    val state = day14input(lines)
    val lowestStone = state.maxOf { it.second }
    return generateSequenceWithoutSeed(state) { state ->
        nextCaveState(state, lowestStone) { it in state }
    }
        .count()
}

fun Sequence<Coord>.restingPlace(cutoff: Int): Coord? {
    var prev: Coord? = null

    for (coord in this) {
        prev = coord

        if (coord.second > cutoff) {
            return null
        }
    }
    return prev
}

//// more than doubles running-time
//fun Sequence<Coord>.restingPlace(cutoff: Int): Coord? {
//    for (w in windowed(2, 1, true)) {
//        when {
//            w.size == 1 -> return w[0]
//            w[0].second > cutoff -> return null
//        }
//    }
//    return null
//}


fun day14part2(lines: Lines): Int {
    val state = day14input(lines)
    val floor = state.maxOf { it.second } + 2
    return generateSequenceWithoutSeed(state) { state ->
        nextCaveState(state, floor) { it in state || it.second == floor }
    }
        .count()
}

fun nextCaveState(state: MutableSet<Coord>, fallCutoff: Int, occupied: (Coord) -> Boolean): MutableSet<Coord>? {
    val coord = sandPath(Coord(500, 0), occupied)
        .restingPlace(fallCutoff)
    return if (coord == null) {
        null
    } else {
        state.adding(coord)
    }
}

val Pair<Coord, Coord>.expandLine: Sequence<Coord> get() = sequence {
    val target = this@expandLine.second
    var current = this@expandLine.first

    while (current != target) {
        yield(current)
        current = current.stepTowards(target)
    }
    yield(current)
}

fun Coord.stepTowards(other: Coord): Coord =
    (first + (other.first - this.first).coerceIn(-1, 1)) to (second + (other.second - this.second).coerceIn(-1, 1))


fun <T> T.nullIf(predicate: (T) -> Boolean): T? =
    if (predicate(this)) null else this

fun sandPath(startPosition: Coord, occupied: (Coord) -> Boolean): Sequence<Coord> =
    generateSequence(startPosition.nullIf { occupied(it) }) {
        when {
            !occupied(it.down) -> it.down
            !occupied(it.down.left) -> it.down.left
            !occupied(it.down.right) -> it.down.right
            else -> null
        }
    }

fun <T> MutableSet<T>.adding(t: T) = this.apply { add(t) }

val Coord.down: Coord get() = first to (second + 1)
val Coord.left: Coord get() = (first - 1) to second
val Coord.right: Coord get() = (first + 1) to second

val Iterable<Coord>.asMatrix: Matrix<Boolean> get() {
    val xs = map { it.first }
    val ys = map { it.second }
    val xOffset = xs.min()
    val yOffset = ys.min()
    val width = xs.max() - xOffset
    val height = ys.max() - yOffset

    return createMutableMatrix(width + 1, height + 1) { it.map({ it + xOffset}, { it + yOffset }) in this }
        .rotateRight
        .asMutableMatrix
        .map { it.reversed() }
}


