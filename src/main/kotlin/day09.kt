import kotlin.math.abs

fun day9(lines: Lines, knots: Int): Int {
    val steps = lines.split(" ").pairs.flatMap { it.first.repeated(it.second.toInt()) }
    val visitedTail = mutableSetOf<Knot>()
    val rope = Knot(0, 0).repeated(knots).toMutableList()

    visitedTail.add(rope.last())

    for (step in steps) {
        rope[0] = rope[0].doStep(step)
        for (i in 1 until knots) {
            rope[i] = rope[i].follow(rope[i - 1])
        }
        visitedTail.add(rope.last())
    }
    return visitedTail.size
}

fun day9part2(lines: Lines) = day9(lines, 10)
fun day9part1(lines: Lines) = day9(lines, 2)

data class Knot(
    val x: Int,
    val y: Int,
) {
    fun doStep(step: String): Knot = when (step) {
        "U" -> copy(y = y + 1)
        "D" -> copy(y = y - 1)
        "R" -> copy(x = x + 1)
        "L" -> copy(x = x - 1)
        else -> error("unknown step $step")
    }

    fun touching(other: Knot) = abs(this.x - other.x) <= 1 && abs(this.y - other.y) <= 1

    fun follow(head: Knot): Knot {
        if (touching(head)) return this
        return Knot(
            y = y + (head.y - this.y).coerceIn(-1, 1),
            x = x + (head.x - this.x).coerceIn(-1, 1),
        )
    }
}

