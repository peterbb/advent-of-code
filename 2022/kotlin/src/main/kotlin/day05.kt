import java.util.regex.Pattern

fun day5Input(lines: Lines): Pair<List<MutableList<Char>>, Matrix<Int>> {
    val p = Pattern.compile("\\s+")
    val r = Regex("""[\[\]]""")
    val (state, stepDescriptions) = lines.batchByNewline().toList()
    val steps = stepDescriptions.map { it.split(p).mapNotNull(String::toIntOrNull) }

    val maxWidth = state.maxOf { it.length }
    val zstate = state
        .dropLast(1)
        .map { it.padEnd(maxWidth, ' ') }
        .map { it.replace(r, " ").toList() }
        .rotateRight
        .map { it.joinToString("").trim() }
        .filter { it != "" }
        .map { it.toMutableList() }
    return zstate to steps
}

fun day5part1(lines: Lines): String {
    val (zstate, steps) = day5Input(lines)

    for ((count, from, to) in steps) {
        repeat(count) {
            val crate = zstate[from - 1].removeLast()
            zstate[to - 1].add(crate)
        }
    }

    return zstate.map { it.last() }.joinToString("")
}

fun day5part2(lines: Lines): String {
    val (zstate, steps) = day5Input(lines)

    for ((count, from, to) in steps) {
        val stack = mutableListOf<Char>()
        repeat(count) {
            stack.add(zstate[from - 1].removeLast())
        }
        repeat(count) {
            zstate[to - 1].add(stack.removeLast())
        }
    }

    return zstate.map { it.last() }.joinToString("")
}
