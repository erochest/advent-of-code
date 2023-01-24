package com.ericrochester.advent2022

class Day02: DayRuns<Int, Int> {
    override fun runA(inputData: String): Int =
        inputData.lines()
            .filter { it.isNotEmpty() }
            .fold(0) { accum: Int, line: String ->
                val opponent = RockPaperScissors.parse(line[0])
                val player = RockPaperScissors.parse(line[2])
                val score = GameScore.score(opponent, player)
                accum + score
            }

    override fun runB(inputData: String): Int =
        inputData.lines()
            .filter { it.isNotEmpty() }
            .fold(0) { accum: Int, line: String ->
                val opponent = RockPaperScissors.parse(line[0])
                val target = GameOutcome.parse(line[2])
                val player = GameOutcome.targetOutcome(opponent, target)
                val score = GameScore.score(opponent, player)
                accum + score
            }
}

enum class RockPaperScissors {
    ROCK, PAPER, SCISSORS;

    companion object {
        fun parse(c: Char): RockPaperScissors {
            return when (c) {
                'A', 'X' -> ROCK
                'B', 'Y' -> PAPER
                else -> SCISSORS
            }
        }
    }
}

enum class GameOutcome {
    Win, Lose, Draw;

    companion object {
        fun parse(c: Char): GameOutcome {
            return when (c) {
                'Z' -> Win
                'X' -> Lose
                else -> Draw
            }
        }

        fun targetOutcome(opponent: RockPaperScissors, target: GameOutcome): RockPaperScissors {
            return when (target) {
                Win -> RockPaperScissors.values()[(opponent.ordinal + 1) % 3]
                Lose -> RockPaperScissors.values()[(opponent.ordinal + 2) % 3]
                Draw -> opponent
            }
        }
    }
}

object GameScore {
    fun score(opponent: RockPaperScissors, player: RockPaperScissors): Int {
        var score = 0

        score = player.ordinal + 1
        score += when (won(opponent, player)) {
            GameOutcome.Win -> 6
            GameOutcome.Lose -> 0
            GameOutcome.Draw -> 3
        }

        return score
    }

    private fun won(opponent: RockPaperScissors, player: RockPaperScissors): GameOutcome {
        return if (opponent == player) {
            GameOutcome.Draw
        } else if ((opponent.ordinal + 1) % 3 == player.ordinal) {
            GameOutcome.Win
        } else {
            GameOutcome.Lose
        }
    }
}