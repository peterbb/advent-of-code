fun main() {
    run(day = 1, ::day1part1 to listOf(24000, 70509), ::day1part2 to listOf(45000, 208567))
    run(day = 2, ::day2part1 to listOf(15, 8392), ::day2part2 to listOf(12, 10116))
    run(day = 3, ::day3part1 to listOf(157, 8176), ::day3part2 to listOf(70, 2689))
    run(day = 4, ::day4part1 to listOf(2, 518), ::day4part2 to listOf(4, 909))
    run(day = 5, ::day5part1 to listOf("CMZ", "TLFGBZHCN"), ::day5part2 to listOf("MCD", "QRQFHFWCL"))
    run(day = 6, ::day6part1 to listOf(7, 5, 6, 10, 11, 1093), ::day6part2 to listOf(19, 23, 23, 29, 26, 3534))
    run(day = 7, ::day7part1 to listOf(95437, 1555642), ::day7part2 to listOf(24933642, 5974547))
    run(day = 8, ::day8part1 to listOf(21, 1829), ::day8part2 to listOf(8, 291840))
    run(day = 9, ::day9part1 to listOf(13, 5930), ::day9part2 to listOf(1, 2443, 36))
    run(day = 10, ::day10part1 to listOf(13140, 15120))
    run(day = 10, ::day10part2 to listOf(
        """
            ##..##..##..##..##..##..##..##..##..##..
            ###...###...###...###...###...###...###.
            ####....####....####....####....####....
            #####.....#####.....#####.....#####.....
            ######......######......######......####
            #######.......#######.......#######.....
        """.trimIndent(),
        """
            ###..#..#.###....##.###..###..#.....##..
            #..#.#.#..#..#....#.#..#.#..#.#....#..#.
            #..#.##...#..#....#.###..#..#.#....#..#.
            ###..#.#..###.....#.#..#.###..#....####.
            #.#..#.#..#....#..#.#..#.#....#....#..#.
            #..#.#..#.#.....##..###..#....####.#..#.
        """.trimIndent(),
    ))
    run(day = 11, ::day11part1 to listOf(10605, 54036), ::day11part2 to listOf(2713310158, 13237873355))
    run(day = 12, ::day12part1 to listOf(31, 517), ::day12part2 to listOf(29, 512))
    run(day = 13, ::day13part1 to listOf(13, 6623), ::day13part2 to listOf(140, 23049))
    run(day = 13, ::day14part1 to listOf(0))
}



/**** TASK RUNNER *****/
typealias Lines = Sequence<String>

fun <Result> run(day: Int, vararg parts: Pair<(Lines) -> Result, List<Result>>) {
    parts.forEachIndexed { part, (implementation, solutions) ->
        solutions.forEachIndexed { dataSet, solution ->
            assert("Day $day part $part", solution, implementation(loadLines("${day.nn}-${dataSet}-data.txt")))
        }
    }
}

fun <T> assert(ctx: String, expect: T, actual: T) {
    if (expect == actual) {
        println("$ctx => $expect")
    } else {
        println("ERROR: $ctx")
        println("ERROR: expect:")
        println("ERROR: |$expect|")
        println("ERROR: actual:")
        println("ERROR: |$actual|")
        println("ERROR: +${"-".repeat(10)}")
    }
}

fun loadAll(filename: String): String =
    Thread.currentThread()
        .contextClassLoader
        .getResourceAsStream(filename)
        .also {
            if (it == null) error("unknown file '$filename'")
        }
        .readAllBytes()
        .let { String(it) }

fun loadLines(filename: String): Lines =
    loadAll(filename)
        .split("\n")
        .asSequence()

