import GameOutcome.DRAW
import GameOutcome.LOSE
import GameOutcome.WIN
import RockPaperScissor.PAPER
import RockPaperScissor.ROCK
import RockPaperScissor.SCISSOR

enum class GameOutcome(val score: Int, val repr: String) {
    LOSE(0, "X"),
    DRAW(3, "Y"),
    WIN(6, "Z"),
}
enum class RockPaperScissor(val score: Int, val repr: String) {
    ROCK(1, "AX"),
    PAPER(2, "BY"),
    SCISSOR(3, "CZ"),
}

val RockPaperScissor.beats: RockPaperScissor
    get() = when (this) {
        ROCK -> SCISSOR
        PAPER -> ROCK
        SCISSOR -> PAPER
    }


val RockPaperScissor.beatenBy: RockPaperScissor
    get() = findEnum<RockPaperScissor> { it.beats == this } !!

val String.asAction: RockPaperScissor
    get() = findEnum<RockPaperScissor> { it.repr.contains(this) } !!

val String.asOutcome: GameOutcome
    get() = findEnum<GameOutcome> { it.repr == this } !!

fun RockPaperScissor.against(other: RockPaperScissor) = when {
    this == other -> DRAW
    this.beats == other -> WIN
    else -> LOSE
}

val Pair<RockPaperScissor, RockPaperScissor>.score: Int
    get() = second.against(first).score + second.score

val Sequence<Pair<RockPaperScissor, RockPaperScissor>>.score: Int
    get() = sumOf { it.score }

fun day2part1(lines: Lines) = lines
    .split(" ")
    .pairs
    .map { it.map(String::asAction) }
    .score

fun day2part2(lines: Lines): Int {
    fun strategy(other: RockPaperScissor, outcome: GameOutcome) = when (outcome) {
        DRAW -> other
        WIN -> other.beatenBy
        LOSE -> other.beats
    }

    return lines
        .split(" ")
        .pairs
        .map { it.map(String::asAction, String::asOutcome) }
        .map { (otherPlays, wantedOutcome) ->
            (otherPlays to strategy(otherPlays, wantedOutcome))
        }
        .score
}