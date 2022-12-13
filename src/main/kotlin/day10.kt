fun day10part2(lines: Lines): String {
    val xs = sequence {
        var x = 1L

        for (instr in lines) {
            if (instr == "noop") {
                yield(x)
            } else if (instr.startsWith("addx ")) {
                yield(x)
                yield(x)
                x += instr.removePrefix("addx ").toLong()
            } else {
                error("unknown instruction")
            }
        }
    }
    val rows = generateSequence(0) { (it + 1) % 40 }
    return rows.zip(xs).map { (position, sprite) ->
        if (position in (sprite - 1 .. sprite + 1)) "#" else "."
    }
        .chunked(40)
        .map { it.joinToString("") }
        .toList()
        .joinToString(separator="\n")
}

fun day10part1(lines: Lines): Long {
    val cycle = generateSequence(1L) { it + 1 }

    val xs = sequence {
        var x = 1L

        for (instr in lines) {
            if (instr == "noop") {
                yield(x)
            } else if (instr.startsWith("addx ")) {
                yield(x)
                yield(x)
                x += instr.removePrefix("addx ").toLong()
            } else {
                error("unknown instruction")
            }
        }
    }

    return cycle.zip(xs).drop(19).windowed(1, step = 40).sumOf {
        it.single().product
    }
}
