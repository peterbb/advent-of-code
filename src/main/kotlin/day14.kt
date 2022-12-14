import java.lang.Thread.yield

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
    val void = state.maxOf { it.second }
    return generateSequence(state) { addSand(Pair(500, 0), it, void, null) }
        .count() - 1 /* initial element not caused by sand */
}

fun day14part2(lines: Lines): Int {
    val state = day14input(lines)
    val void = state.maxOf { it.second }
    val floor = state.map { it.second }.max() + 2
    return generateSequence(state) { addSand(Pair(500, 0), it, void, floor) }
        .count() - 1 /* initial element not caused by sand */
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


fun addSand(
    sand: Coord,
    state: MutableSet<Coord>,
    void: Int,
    floor: Int?,
): MutableSet<Coord>? {
    var sand: Coord = sand

    while (true) {
        when {
            sand in state -> {
                return null
            }
            floor != null && sand.second == floor - 1 -> {
                return state.apply { add(sand) }
            }
            sand.second > void -> {
                return null
            }
            sand.down !in state -> {
                sand = sand.down
                continue
            }
            sand.down.left !in state -> {
                sand = sand.down.left
                continue
            }
            sand.down.right !in state -> {
                sand = sand.down.right
                continue
            }
            else -> {
                return state.apply { add(sand) }
            }
        }
    }
}

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


