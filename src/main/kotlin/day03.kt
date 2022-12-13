fun day3part1(lines: Lines) = lines
    .sumOf {
        it.split
            .map { it.toSet() }
            .intersect
            .single()
            .rucksackItemScore
    }

fun day3part2(lines: Lines) = lines
    .chunked(3)
    .sumOf {
        it.map { it.toSet() }
            .bigIntersect()
            .single()
            .rucksackItemScore
    }

val Char.rucksackItemScore: Int
    get() = when (this) {
        in 'a'..'z' -> 1 + code - 'a'.code
        in 'A'..'Z' -> 27 + code - 'A'.code
        else -> error("invalid rucksack item")
    }
