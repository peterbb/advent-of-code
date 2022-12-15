fun day1part1(lines: Lines): Int =
    lines
        .batchByNewline()
        .maxOfOrNull { batch ->
            batch.sumOf { it.toInt() }
        } !!

fun day1part2(lines: Lines): Int =
    lines
        .batchByNewline()
        .map { batch -> batch.sumOf { it.toInt() } }
        .sortedDescending()
        .take(3)
        .sum()
