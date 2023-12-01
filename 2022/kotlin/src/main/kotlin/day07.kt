fun day7part2(lines: Lines): Long {
    val root = day7(lines)
    val free = 70_000_000 - root.size
    val missing = 30_000_000 - free

    return day7(lines)
        .directories
        .map { it.size }
        .filter { it >= missing }
        .min()
}
fun day7part1(lines: Lines) =
    day7(lines).directories.map { it.size }.filter { it <= 100_000 }.sum()

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
        get() = sequenceOf(this) + dirs.values.asSequence().flatMap { it.directories }
}


