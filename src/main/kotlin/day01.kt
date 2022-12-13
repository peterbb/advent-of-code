fun day1part1(lines: Lines): Int =
    lines
        .batchByNewline()
        .maxOfOrNull { batch ->
            batch.asSequence().sumOf { it.toInt() }
        } !!

fun <S, T> ((S) -> Sequence<T>).sumOf(select: (T) -> Int): (S) -> Int =
    { this(it).sumOf(select) }

fun day1part2(lines: Lines): Int =
    lines
        .batchByNewline()
        .map { batch -> batch.asSequence().sumOf { it.toInt() } }
        .sortedDescending()
        .take(3)
        .sum()