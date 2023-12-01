import java.lang.Math.addExact
import java.lang.Math.multiplyExact

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
