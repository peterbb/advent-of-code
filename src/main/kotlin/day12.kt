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

fun day12(lines: Lines): Matrix<Vertex> {
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
                val alt = n.distance.saturatedSuccessor
                if (alt < n.distance) {
                    n.distance = alt
                }
            }
        }
    }

    return graph
}

val Long.saturatedSuccessor: Long
    get() = if (this == Long.MAX_VALUE) this else this + 1


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

