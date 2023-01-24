package com.ericrochester.advent2022

import kotlin.math.roundToInt
import kotlin.math.pow

class Day11: DayRuns<Int,Int> {
    val maxRoundCount = 20

    override fun runA(inputData: String): Int {
        val monkeys = paragraphs(inputData.lines()).map { Monkey.parse(it) }
        repeat(20) {
            for (monkey in monkeys) {
                for (throws in monkey.move()) {
                    monkeys[throws.target].catchItem(throws.worryAmount)
                }
            }
            dumpRound(it, monkeys)
        }
        dumpItemsSeen(monkeys)

        val busiest = monkeys.sortedByDescending { it.itemsSeen }.take(2)
        return busiest[0].itemsSeen * busiest[1].itemsSeen
    }

    private fun dumpRound(i: Int, monkeys: List<Monkey>) {
        println("Round $i")
        for (monkey in monkeys) {
            println("Monkey ${monkey.n}: ${monkey.items.joinToString(", ")}")
        }
        println()
    }

    private fun dumpItemsSeen(monkeys: List<Monkey>) {
        for (monkey in monkeys) {
            println("Monkey ${monkey.n} inspected items ${monkey.itemsSeen} times.")
        }
    }

    override fun runB(inputData: String): Int {
        TODO("Not yet implemented")
    }

}

data class Monkey(
    val n: Int,
    val items: MutableList<Int>,
    val worryOp: OpPair,
    val testOp: OpPair,
    val testThrows: ThrowTo
) {
    var itemsSeen: Int = 0

    fun move(): List<ThrowInfo> {
        val throws = mutableListOf<ThrowInfo>()

        while (items.size > 0) {
            var current = items.removeFirst()
            itemsSeen += 1
            current = worryOp.exec(current)
            current = current.floorDiv(3)
            val target = if (testOp.exec(current) == 0) {
                testThrows.ifTrue
            } else {
                testThrows.ifFalse
            }
            throws.add(ThrowInfo(target, current))
        }

        println("### ${n}. $throws")
        return throws
    }

    fun catchItem(item: Int) {
        items.add(item)
    }

    companion object {
        fun parse(lines: List<String>): Monkey {
            var n: Int = -1
            var items: List<Int>? = null
            var worryOp: OpPair? = null
            var testOp: OpPair? = null
            var testThrows: ThrowTo? = null

            for (line in lines) {
                if (line.startsWith("Monkey ")) {
                    n = line.substring(7, line.length-1).toInt()
                } else if (line.startsWith("  Starting items: ")) {
                    items = line.substring(18)
                        .split(',')
                        .map { it.trim().toInt() }
                } else if (line == "  Operation: new = old * old") {
                    worryOp = OpPair.pow(2)
                } else if (line.startsWith("  Operation: ")) {
                    val parts = line.substring(13).split(' ')
                    val op = when (parts[3]) {
                        "+" -> Op.Plus
                        "*" -> Op.Multiply
                        else -> Op.Plus
                    }
                    val value = parts.last().toInt()
                    worryOp = OpPair(op, value)
                } else if (line.startsWith("  Test: ")) {
                    val parts = line.substring(8).split(' ')
                    val op = when(parts[0]) {
                        "divisible" -> Op.Divisible
                        else -> Op.Plus
                    }
                    val value = parts.last().toInt()
                    testOp = OpPair(op, value)
                    testThrows = ThrowTo(-1, -1)
                } else if (line.startsWith("    If true: ")) {
                    val ifTrue = line.split(' ').last().toInt()
                    testThrows = testThrows?.copy(ifTrue = ifTrue)
                } else if (line.startsWith("    If false: ")) {
                    val ifFalse = line.split(' ').last().toInt()
                    testThrows = testThrows?.copy(ifFalse = ifFalse)
                }
            }

            return Monkey(
                n,
                items?.toMutableList() ?: mutableListOf(),
                worryOp ?: OpPair.plus(-1),
                testOp ?: OpPair.divisible(1),
                testThrows ?: ThrowTo(-1, -1)
            )
        }
    }
}

data class OpPair(val op: Op, val value: Int) {
    fun exec(current: Int): Int {
        return op.exec(current, value)
    }

    companion object {
        fun plus(value: Int): OpPair {
            return OpPair(Op.Plus, value)
        }

        fun pow(value: Int): OpPair {
            return OpPair(Op.Power, value)
        }

        fun divisible(value: Int): OpPair {
            return OpPair(Op.Divisible, value)
        }
    }
}

enum class Op {
    Plus {
         override fun exec(x: Int, y: Int): Int = x + y
     },
    Subtract {
        override fun exec(x: Int, y: Int): Int = x - y
    },
    Multiply {
        override fun exec(x: Int, y: Int): Int = x * y
    },
    Divide {
        override fun exec(x: Int, y: Int): Int = x / y
    },
    Power {
        override fun exec(x: Int, y: Int): Int = x.toDouble().pow(y).roundToInt()
    },
    Divisible {
        override fun exec(x: Int, y: Int): Int = x % y
    };

    abstract fun exec(x: Int, y: Int): Int
}

data class ThrowTo(val ifTrue: Int, val ifFalse: Int)
data class ThrowInfo(val target: Int, val worryAmount: Int)
