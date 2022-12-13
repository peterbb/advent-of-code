import GameOutcome.DRAW
import GameOutcome.LOSE
import GameOutcome.WIN
import RockPaperScissor.PAPER
import RockPaperScissor.ROCK
import RockPaperScissor.SCISSOR
import java.lang.Integer.max
import java.lang.Math.addExact
import java.lang.Math.multiplyExact
import java.math.BigInteger
import java.util.regex.Pattern
import kotlin.math.abs

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
    run(day = 13, ::day13part1 to listOf(13))
}

interface Packet {
    data class Leaf(val value: Int): Packet
    data class Node(val list: List<Packet>) : Packet

    fun parse(expr: String): Pair<Packet, String> {
    }

    fun num(inp: String): Pair<Int, String> {

    }

}

fun day13part1(lines: Lines): Int {
    lines.batchByNewline()
        .map { it.map(Packet::parse) }
    return 0
}

fun Sequence<String>.findCoord(char: Char): Pair<Int, Int> =
    withIndex().map { it.index to it.value.indexOf(char) }
        .find { it.second != -1 }!!

class Vertex(
    val height: Int,
) {
    val inEdge = mutableListOf<Vertex>()
    var distance = Long.MAX_VALUE

    fun tryAdd(other: Vertex?) {
        if (other == null) return
        if (other.height <= height + 1) {
            other.inEdge.add(this)
        }
    }
}

fun day12part1(lines: Lines): Long {
    val start = lines.findCoord('S')
    return day12(lines)[start].distance
}

fun day12part2(lines: Lines): Long {
    return day12(lines)
        .asSequence()
        .flatMap { it.asSequence() }
        .filter { it.height == 0 }
        .map { it.distance }
        .min()
}

fun day12(lines: Lines): List<List<Vertex>> {
    val graph = lines.map {
        it.replace("S", "a").replace("E", "z").map { Vertex(it.code - 'a'.code) }
    }
        .toList()
    for (i in graph.indices)  {
        for (j in graph[0].indices) {
            graph[i][j].tryAdd(graph.getOrNull(i)?.getOrNull(j - 1))
            graph[i][j].tryAdd(graph.getOrNull(i)?.getOrNull(j + 1))
            graph[i][j].tryAdd(graph.getOrNull(i - 1)?.getOrNull(j))
            graph[i][j].tryAdd(graph.getOrNull(i + 1)?.getOrNull(j))
        }
    }

    val end = lines.findCoord('E')

    val unvisited = graph.flatten().toMutableList()
    graph[end].distance = 0

    while (unvisited.isNotEmpty()) {
        unvisited.sortBy { it.distance }
        val current = unvisited.removeFirst()
        for (n in current.inEdge) {
            if (n in unvisited) {
                val alt = if (current.distance == Long.MAX_VALUE) Long.MAX_VALUE else current.distance + 1
                if (alt < n.distance) {
                    n.distance = alt
                }
            }
        }
    }

    return graph
}

operator fun <A> List<List<A>>.get(coord: Pair<Int, Int>): A =
    this[coord.first][coord.second]

val <T> List<List<T>>.coordinates: Sequence<Pair<Int, Int>>
    get() =
        asSequence()
            .flatMapIndexed { i, row ->
                row.indices.map { j -> i to j }
            }

data class Monkey(
    val number: Int,
    val items: MutableList<Long>,
    val operationLhs: String,
    val operationOp: String,
    val operationRhs: String,
    val divisibleTest: Long,
    val ifTrue: Int,
    val ifFalse: Int,
) {
    var inspections = 0L

    val op: (Long, Long) -> Long = when (operationOp) {
        "+" -> ::addExact
        "*" -> ::multiplyExact
        else -> error("unknown op")
    }
    fun eval(old: Long, expr: String): Long = when (expr) {
        "old" -> old
        else -> expr.toLong()
    }

    fun newWorryLevel(old: Long) =
        op(eval(old, operationLhs), eval(old, operationRhs))
}

fun day11(lines: Lines, rounds: Int, worryManagement: (Long) -> Long): Long {
    val monkeyRegex = Regex("""Monkey (\d+):""")
    val itemsRegex = Regex("""Starting items: ([\d\s,]*)""")
    val operationRegex = Regex("""Operation: new = ([\w\d]+) ([-+*]) ([\w\d]+)""")
    val testRegex = Regex("""Test: divisible by (\d*)""")
    val ifTrueRegex = Regex("""If true: throw to monkey (\d+)""")
    val ifFalseRegex = Regex("""If false: throw to monkey (\d+)""")
    val monkeys =  lines.batchByNewline()
        .map {
            val (_, lhs, op, rhs) = operationRegex.find(it[2])!!.groupValues
            Monkey(
                number = monkeyRegex.find(it[0])!!.groupValues[1].toInt(),
                items = itemsRegex.find(it[1])!!.groupValues[1].split(",").map { it.trim().toLong() }.toMutableList(),
                operationLhs = lhs,
                operationRhs = rhs,
                operationOp = op,
                divisibleTest = testRegex.find(it[3])!!.groupValues[1].toLong(),
                ifTrue = ifTrueRegex.find(it[4])!!.groupValues[1].toInt(),
                ifFalse = ifFalseRegex.find(it[5])!!.groupValues[1].toInt(),
            )
        }
        .toList()

    val foobar = monkeys.map { it.divisibleTest }.product()
    repeat(rounds) { round ->
        for (monkey in monkeys) {
            while (true) {
                val worry = monkey.items.removeFirstOrNull() ?: break
                monkey.inspections = addExact(monkey.inspections, 1)
                val newWorry = worryManagement(monkey.newWorryLevel(worry)).rem(foobar)
                val toMonkey = if (newWorry % monkey.divisibleTest == 0L)
                    monkey.ifTrue
                else
                    monkey.ifFalse
                monkeys[toMonkey].items.add(newWorry)
            }
        }
    }
    return  monkeys.map { it.inspections }.sortedDescending().take(2).product()
}

fun day11part2(lines: Lines) = day11(lines, 10_000) { it }
fun day11part1(lines: Lines) = day11(lines, 20) { it / 3 }

fun List<Int>.product(): Int = fold(1) { a, b -> a * b }
fun List<Long>.product(): Long = fold(1) { a, b -> a * b }
fun List<BigInteger>.product(): BigInteger = fold(BigInteger.ONE) { a, b -> a.times(b) }

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

val Pair<Long, Long>.product: Long
    get() = first * second

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

fun day8part2(lines: Lines): Int {
    var treeMap = lines.map { it.map { Tree(it.parseInt()) } }.toList()
    val baseBackdrop = (0..9).map { it to 0 }.toTypedArray()
    repeat(4) {
        for (row in treeMap) {
            val backdrop = mutableMapOf(*baseBackdrop)
            for (tree in row) {
                tree.scenicScore *= backdrop[tree.height]!!
                for (i in 0 .. tree.height) {
                    backdrop[i] = 1
                }
                for (i in tree.height + 1 .. 9) {
                    backdrop[i] = backdrop[i]!! + 1
                }
            }
        }

        treeMap = treeMap.rotateRight
    }
    return treeMap.maxOf { it.maxOf { it.scenicScore } }
}

fun day8part1(lines: Lines): Int {
    var treeMap = lines.map { it.map { Tree(it.parseInt()) } }.toList()
    repeat(4) {
        for (row in treeMap) {
            var highline = -1
            for (tree in row) {
                if (highline < tree.height) {
                    tree.visible = true
                }
                highline = max(highline, tree.height)
            }
        }

        treeMap = treeMap.rotateRight
    }

    return treeMap.sumOf { it.count { it.visible } }
}
class Tree(
    val height: Int
) {
    var visible = false
    var scenicScore = 1
}

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

class ShellState {
    val root = Directory()
    private var currentDirectory = root

    fun cd(arg: String){
        currentDirectory = when (arg) {
            ".." -> currentDirectory.parent!!
            "/" -> root
            else -> currentDirectory.cd(arg)
        }
    }

    fun ls(info: String, name: String) {
        if (info == "dir") {
            currentDirectory.addDirectory(name)
        } else {
            currentDirectory.addFile(name, info.toLong())
        }
    }
}

fun day7part2(lines: Lines): Long {
    val root = day7(lines)
    val free = 70000000 - root.size
    val missing = 30000000 - free

    return day7(lines)
        .directories
        .map { it.size }
        .filter { it >= missing }
        .min()
}
fun day7part1(lines: Lines): Long {
    return day7(lines).directories.map { it.size }.filter { it <= 100_000 }.sum()
}
fun day7(lines: Lines): Directory {
    val state = ShellState()
    lines.forEach {
        if (it.startsWith("$ cd ")) {
            state.cd(it.removePrefix("$ cd "))
        } else if (it.startsWith("$ ls")) {
            /* noop */
        } else {
            val (info, name) = it.split(" ")
            state.ls(info, name)
        }
    }
    return state.root
}

class Directory(val parent: Directory? = null) {
    private val files = mutableMapOf<String, Long>()
    private val dirs = mutableMapOf<String, Directory>()

    val size: Long
        get() = files.values.sum() + dirs.values.sumOf { it.size }

    fun cd(dir: String) = dirs[dir]!!

    fun addFile(name: String, size: Long) {
        files[name] = size
    }

    fun addDirectory(name: String) {
        dirs.putIfAbsent(name, Directory(this))
    }

    val directories: Sequence<Directory>
        get() =
            sequenceOf(this) + dirs.values.asSequence().map { it.directories }.flatten()
}


fun day5Input(lines: Lines): Pair<List<MutableList<Char>>, List<List<Int>>> {
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


val Char.rucksackItemScore: Int
    get() = when (this) {
        in 'a'..'z' -> 1 + code - 'a'.code
        in 'A'..'Z' -> 27 + code - 'A'.code
        else -> error("invalid rucksack item")
    }

fun day3part1(lines: Lines) = lines
    .map { it.subSequence(0, it.length / 2) to it.subSequence(it.length / 2, it.length) }
    .map { it.map { it.toSet()} }
    .map { (a, b) -> a.intersect(b) }
    .map { it.single() }
    .map { it.rucksackItemScore }
    .sum()

fun day3part2(lines: Lines) = lines
    .chunked(3)
    .map { it.map { it.toSet() }.bigIntersect().single() }
    .map { it.rucksackItemScore }
    .sum()

enum class GameOutcome {
    WIN,
    LOSE,
    DRAW,
}

enum class RockPaperScissor {
    ROCK,
    PAPER,
    SCISSOR,
}

val RockPaperScissor.beats: RockPaperScissor
    get() = when (this) {
        ROCK -> SCISSOR
        PAPER -> ROCK
        SCISSOR -> PAPER
    }
val RockPaperScissor.beatenBy: RockPaperScissor
    get() = RockPaperScissor.values().find { it.beats == this } !!

fun RockPaperScissor.playedAgainst(other: RockPaperScissor) = when {
    this == other -> DRAW
    this.beats == other -> WIN
    else -> LOSE
}

fun instructionAsAction(a: String) = when (a) {
    "A", "X" -> ROCK
    "B", "Y" -> PAPER
    "C", "Z" -> SCISSOR
    else -> error("unknown action $a")
}

fun instructionAsOutcome(a: String) = when (a) {
    "X" -> LOSE
    "Y" -> DRAW
    "Z" -> WIN
    else -> error("unknown action $a")
}

val Sequence<Pair<RockPaperScissor, RockPaperScissor>>.score: Int
    get() {
        val actionScore = this
            .map { (_, myPlay) ->
                when (myPlay) {
                    ROCK -> 1
                    PAPER -> 2
                    SCISSOR -> 3
                } }
            .sum()
        val outcomeScore = this
            .map { (them, me) ->
                when (me.playedAgainst(them)) {
                    WIN -> 6
                    DRAW -> 3
                    LOSE -> 0
                }
            }
            .sum()
        return actionScore + outcomeScore
    }


/***** DAY 2 *****/
fun day2part1(lines: Lines): Int {
    return lines
        .split(" ")
        .pairs
        .map { it.map(::instructionAsAction) }
        .score
}

fun day2part2(lines: Lines): Int {
    fun foo(other: RockPaperScissor, outcome: GameOutcome): RockPaperScissor =
        when (outcome) {
            DRAW -> other
            WIN -> other.beatenBy
            LOSE -> other.beats
        }

    return lines
        .split(" ")
        .pairs
        .map { it.map(::instructionAsAction, ::instructionAsOutcome) }
        .map { (otherPlays, wantedOutcome) ->
            (otherPlays to foo(otherPlays, wantedOutcome))
        }
        .score
}

/***** DAY 1 ****/

fun day1part1(lines: Lines): Int =
    lines
        .batchByNewline()
        .maxOfOrNull { batch ->
            batch.asSequence().map { it.toInt() }.sum()
        } !!

fun day1part2(lines: Lines): Int =
    lines
        .batchByNewline()
        .map { batch -> batch.asSequence().map { it.toInt() }.sum() }
        .sortedDescending()
        .take(3)
        .sum()


/**** UTIL *****/
fun <T> T.repeated(times: Int): Sequence<T> = sequence {
    repeat(times) {
        yield(this@repeated)
    }
}


fun <S, T, U> Iterable<Pair<S, T>>.map2(f: (S, T) -> U): Iterable<U> =
    map { f(it.first, it.second)}

fun <S, T, U> Sequence<Pair<S, T>>.mapRight(f: (T) -> U): Sequence<Pair<S, U>> =
    map { it.first to f(it.second) }

fun <T> createList(size: Int, elem: (Int) -> T): List<T> =
    sequence {
        repeat(size) { i ->
            yield(elem(i))
        }
    }
        .toList()

val <T> List<List<T>>.rotateRight: List<List<T>>
    get() {
        val elems = this
        val height = this.size
        val width = this.maxOf { it.size }

        return createList(width) { i ->
            createList(height) { j ->
                elems[height - 1 - j][i]
            }
        }
    }
fun Char.parseInt() = code - '0'.code

fun <T : Comparable<T>> ClosedRange<T>.disjoint(other: ClosedRange<T>) =
    endInclusive < other.start || other.endInclusive < start

fun <T : Comparable<T>> ClosedRange<T>.overlap(other: ClosedRange<T>) = !disjoint(other)

fun <A, B> Sequence<Pair<A, A>>.mapPair(f: (A) -> B) =
    map { f(it.first) to f(it.second) }

fun String.splitPair(on: String): Pair<String, String> =
    split(on).let { it[0] to it[1] }

val Pair<Int, Int>.asClosedRange: IntRange
    get() = IntRange(first, second)

fun <A : Comparable<A>> ClosedRange<A>.subsumes(other: ClosedRange<A>) =
    start <= other.start && other.endInclusive <= endInclusive
fun <A0, A1, B0, B1> Pair<A0, A1>.map(f: (A0) -> B0, g: (A1) -> B1): Pair<B0, B1> = f(first) to g(second)
fun <A, B> Pair<A, A>.map(f: (A) -> B): Pair<B, B> = map(f, f)

fun <T> Iterable<Set<T>>.bigIntersect(): Set<T> =
    reduce { a, b -> a.intersect(b) }

fun Sequence<String>.batchByNewline(): Sequence<List<String>> = sequence {
    var batch = mutableListOf<String>()
    for (line in this@batchByNewline.asIterable()) {
        if (line == "") {
            yield(batch)
            batch = mutableListOf()
        } else {
            batch.add(line)
        }
    }
    if (batch.isNotEmpty()) {
        yield(batch)
    }
}


val Int.nn: String
    get() = this.toString().padStart(2, '0')


fun Sequence<String>.split(on: String) = map { it.split(on) }

val Sequence<List<String>>.pairs: Sequence<Pair<String, String>>
    get() = map { it[0] to it[1] }

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

