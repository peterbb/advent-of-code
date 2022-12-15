
fun day4part1(lines: Lines) =
    lines
        .split(",")
        .pairs
        .mapPair { it.splitPair("-").map(String::toInt).asClosedRange }
        .filter { (a, b) -> a.subsumes(b) || b.subsumes(a) }
        .count()

fun day4part2(lines: Lines) =
    lines
        .split(",")
        .pairs
        .mapPair { it.splitPair("-").map(String::toInt).asClosedRange }
        .filter { (a, b) -> a.overlap(b) }
        .count()
