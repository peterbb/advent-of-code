import Parsec.between
import Parsec.exactly
import Parsec.lazy
import Parsec.list
import Parsec.map
import Parsec.maybe
import Parsec.plus
import Parsec.r
import Parsec.times

sealed interface Sexp {
    val asList: List<Sexp>
    val isAtom: Boolean
    val isAtomList: Boolean
    val asStringList: List<String>
    val asString: String
    val asValue: Value
}
class Atom(val atom: String): Sexp {
    override val asList: List<Sexp> get() = error("not list")
    override val isAtom = true
    override val isAtomList = false
    override val asStringList: List<String>
        get() = error("not coericable to  string list")
    override val asValue get() = Atzom(atom)
    override val asString: String
        get() = atom

    override fun toString() = atom
}
class Seq(val seq: List<Sexp>): Sexp {
    override val asList: List<Sexp> get() = seq
    override val isAtom = false
    override val isAtomList = seq.all { it.isAtom }
    override val asStringList: List<String>
        get() = seq.map { it.asString }
    override val asString: String
        get() = error("not a stringy thing")
    override val asValue
        get() = Lizt(seq.map { it.asValue })

    override fun toString() = seq.joinToString(" ", "(", ")") { it.toString() }
}

object SexpParser {
    val sexp: Lazy<Parser<Sexp>> = lazy { atom + seq }

    val space = r("""\s+""").map { }

    fun <T> skipSpace(p: Parser<T>): Parser<T> =
        (maybe(space) * p).map { it.second }

    val atom: Parser<Sexp> = skipSpace(r("""[?!\w_*\\-]+""").map { Atom(it) })

    val openBrace = skipSpace(exactly("("))
    val closeBrace = skipSpace(exactly(")"))
    val seqElements = skipSpace(sexp.lazy).list()
    val seq: Parser<Sexp> = seqElements.between(openBrace, closeBrace).map { Seq(it) }

    val sexpStandalone = (sexp.lazy * maybe(space)).map { it.first }
}

val String.asSexp: Sexp
    get() = parse(SexpParser.sexpStandalone)

fun main() {
    println("""
        (
        for-each
         (line *input*) (display-line line))""".trimIndent().asSexp)
}

sealed interface Value

class Atzom(var atzom: String): Value {
    override fun toString() = atzom
}
class Lizt(val lizt: List<Value>): Value {
    override fun toString() = lizt.joinToString(" ", "(", ")") { it.toString() }
}

class Closure(
    val args: List<String>,
    val body: Sexp,
    val env: Map<String, Value>,
): Value {
    companion object {
        fun create(tail: List<Sexp>, env: Env): Closure {
            require(tail.size == 2)
            val (args, body) = tail
            return Closure(args.asStringList, body, env)
        }
    }
}

class Builtin(val name: String, val exec: (List<Value>) -> Value): Value {
    override fun toString() = "#<builtin: $name>"

}

typealias Env = Map<String, Value>

fun String.eval(lines: Lines) =
    this.asSexp.eval(
        mapOf(
            "*input*" to Lizt(lines.map { Atzom(it) }.toList()),
            "blank?" to Builtin("blank?") {
                require(it.size == 1)
                val (x) = it
                when (x) {
                    is Atzom -> if (x.atzom.isBlank()) Atzom("true") else Atzom("false")
                    else -> error("can't blank? non-atom")
                }
            },
            "print-line" to Builtin("print-line") {
                require(it.size == 1)
                val (x) = it
                println(x)
                Lizt(emptyList())
            },
            "nil" to Lizt(listOf()),
            "cons" to Builtin("cons") {
                require(it.size == 2)
                val (h, t) = it
                when (t) {
                    is Lizt -> Lizt(listOf(h) + t.lizt)
                    else -> error("cons only lizt >:(")
                }
            },
            "fold-right" to Builtin("fold-right") {
                require(it.size == 3)
                val (proc, init, list) = it
                when (list) {
                    is Closure, is Builtin, is Atzom -> error("cant fold this")
                    is Lizt -> {
                        list.lizt.foldRight(init) { elem, acc ->
                            proc.appl(listOf(elem, acc))
                        }
                    }
                }
            }
        )
    )

fun Sexp.eval(env: Env): Value =
    when (this) {
        is Atom -> env[atom] ?: throw IllegalArgumentException("free variable $atom")
        is Seq -> {
            val head = seq.firstOrNull() ?: throw IllegalArgumentException("illegal scheme expression: ()")
            val tail = seq.subList(1, seq.size)
            when (head) {
                is Atom -> head.dispatchEval(tail, env)
                is Seq -> head.eval(env).appl(tail.eval(env))
            }
        }
    }

fun List<Sexp>.eval(env: Env) = map { it.eval(env) }

fun Atom.dispatchEval(args: List<Sexp>, env: Env) = when (this.atom) {
    """\""" -> Closure.create(args, env)
    """quote""" -> {
        require(args.size == 1)
        val (x) = args
        x.asValue
    }
    "begin" -> {
        args.fold<Sexp, Value>(Lizt(listOf())) { _, e ->
            e.eval(env)
        }
    }
    """let""" -> {
        require(args.size == 2)
        val (bindings, body) = args
        body.eval(
            env + bindings.asList.map {
                val (name, expr) = it.asList
                name.asString to expr.eval(env)
            }.toMap()
        )
    }
    """if""" -> {
        require(args.size == 3)
        val (cond, then, elze) = args
        when (val r = cond.eval(env)) {
            is Atzom -> if (r.atzom == "false") elze.eval(env) else then.eval(env)
            else -> then.eval(env)
        }
    }
    else -> this.eval(env).appl(args.eval(env))
}

fun Value.appl(args: List<Value>): Value = when (this) {
    is Closure -> {
        require(this.args.size == args.size)
        body.eval(this.env + this.args.zip(args).toMap())
    }
    is Builtin -> exec(args)
    else -> error("not a function")
}


