
fun day6part1(lines: Lines) = day6(lines, 4)
fun day6part2(lines: Lines) = day6(lines, 14)
fun day6(lines: Lines, unique: Int) =
    lines
        .single()
        .asSequence()
        .withIndex()
        .windowed(unique)
        .find { it.map { it.value }.toSet().size == unique } !!
        .last().index + 1
