import java.lang.Integer.max

fun day8ForTreeLine(lines: Lines, body: (List<Tree>) -> Unit): Matrix<Tree> {
    var treeMap = lines.map { it.map { Tree(it.parseInt()) } }.toList()
    repeat(4) {
        for (row in treeMap) {
            body(row)
        }

        treeMap = treeMap.rotateRight
    }
    return treeMap
}

fun day8part2(lines: Lines): Int {
    val baseBackdrop = (0..9).map { it to 0 }.toTypedArray()
    return day8ForTreeLine(lines) { row ->
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
        .maxOf { it.maxOf { it.scenicScore } }
}

fun day8part1(lines: Lines): Int =
    day8ForTreeLine(lines) { row ->
        var highline = -1
        for (tree in row) {
            if (highline < tree.height) {
                tree.visible = true
            }
            highline = max(highline, tree.height)
        }
    }
        .sumOf { it.count { it.visible } }

class Tree(
    val height: Int
) {
    var visible = false
    var scenicScore = 1
}

