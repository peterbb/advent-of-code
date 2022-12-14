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
    return generateSequence(state) { state -> addSand(state, lowestStone, floor = false) }
        .drop(1) /* initial element is start state, not caused by sand. */
        .count()
}

fun day14part2(lines: Lines): Int {
    val state = day14input(lines)
    val lowestStone = state.maxOf { it.second }
    return generateSequence(state) { state -> addSand(state, lowestStone, floor = true) }
        .drop(1) /* initial element is start state, not caused by sand. */
        .count()
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
    state: MutableSet<Coord>,
    lowestStone: Int,
    floor: Boolean,
    sand: Coord = Pair(500, 0),
): MutableSet<Coord>? = when {
    sand in state -> null
    floor && sand.second == lowestStone + 1 -> state.apply { add(sand) }
    !floor && sand.second == lowestStone -> null
    sand.down !in state -> addSand(state, lowestStone, floor, sand.down)
    sand.down.left !in state -> addSand(state, lowestStone, floor, sand.down.left)
    sand.down.right !in state -> addSand(state, lowestStone, floor, sand.down.right)
    else -> state.apply { add(sand) }
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


