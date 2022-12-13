import Parsec.between
import Parsec.exactly
import Parsec.int
import Parsec.lazy
import Parsec.list
import Parsec.map
import Parsec.plus

fun day13part2(lines: Lines): Int {
    val start = "[[2]]".parse(Packet.parser.value)
    val end = "[[6]]".parse(Packet.parser.value)
    return lines
        .filter { it.isNotBlank() }
        .map { it.parse(Packet.parser.value) }
        .inject(start, end)
        .sortedWith(PacketCompare)
        .withIndex()
        .filter { it.value == start || it.value == end }
        .take(2)
        .productOf { it.index + 1 }
}

fun day13part1(lines: Lines): Int {
    return lines.batchByNewline()
        .map { it.map { it.parse(Packet.parser.value) }.asPair }
        .map {
            when (PacketCompare.compare(it.first, it.second)) {
                1 -> false
                -1 -> true
                else -> throw IllegalArgumentException("nope")
            }
        }
        .withIndex()
        .filter { it.value }
        .sumOf { it.index + 1 }
}

object PacketCompare: Comparator<Packet> {
    override fun compare(left: Packet, right: Packet) =
        try {
            packetCompare(left, right)
            error("partial equivalence relation")
        } catch (r: CompareResult) {
            if (r.inOrder) -1 else 1
        }
}

interface Packet {
    val promoted: Packet
    data class Leaf(val value: Int): Packet {
        override val promoted: Packet
            get() = Node(listOf(this))

        override fun toString() = value.toString()
    }
    data class Node(val list: List<Packet>) : Packet {
        override val promoted: Packet
            get() = this

        override fun toString() =
            list.joinToString(",", "[", "]") { it.toString() }
    }

    companion object {
        val nodeParser = lazy {
            parser.lazy.list(exactly(",")).between(exactly("["), exactly("]")).map {
                Node(it)
            }
        }
        val leafParser: Lazy<Parser<Packet>> = lazy { int.map { Leaf(it) } }

        val parser: Lazy<Parser<Packet>> = lazy { leafParser.lazy + nodeParser.lazy }
    }
}

class CompareResult(val inOrder: Boolean): Throwable()

fun packetCompare(left: Packet, right: Packet) {
    if (left is Packet.Leaf && right is Packet.Leaf) {
        when {
            left.value < right.value -> throw CompareResult(true)
            left.value > right.value -> throw CompareResult(false)
            else -> Unit
        }
    } else if (left is Packet.Node && right is Packet.Node) {
        for ((l, r) in left.list.zipWithNull(right.list)) {
            if (l == null || r == null) {
                throw CompareResult(l == null)
            }
            packetCompare(l, r)
        }
    } else {
        packetCompare(left.promoted, right.promoted)
    }
}

