package com.ericrochester.advent2022

import kotlin.math.abs
import kotlin.math.sign

class Day09: DayRuns<Int, Int> {
    override fun runA(inputData: String): Int =
        inputData.lines()
            .map { DirectionPair.parse(it) }
            .fold(Board(Pair(0, 0), mutableListOf(Pair(0, 0)))) { board, dirPair ->
                board.move(dirPair)
                board
            }
            .tailTrack
            .size

    override fun runB(inputData: String): Int {
        val tail = mutableListOf<Pair<Int,Int>>()
        repeat(9) { tail.add(Pair(0, 0)) }

        val startBoard = Board(Pair(0, 0), tail)

        return inputData.lines()
            .map { DirectionPair.parse(it) }
            .fold(startBoard) { board, dirPair ->
                board.move(dirPair)
                board
            }
            .tailTrack
            .size
    }


}

data class DirectionPair(val direction: Direction, val distance: Int) {
    companion object {
        fun parse(input: String): DirectionPair {
            val parts = input.split(" ")
            val dir = when (parts[0]) {
                "R"  -> Direction.Right
                "L"  -> Direction.Left
                "U"  -> Direction.Up
                else -> Direction.Down
            }
            val dist = parts[1].toInt()
            return DirectionPair(dir, dist)
        }
    }
}

enum class Direction {
    Right,
    Left,
    Up,
    Down
}

data class Board(var head: Pair<Int, Int>, var tail: MutableList<Pair<Int, Int>>) {
    val tailTrack = mutableSetOf<Pair<Int, Int>>()

    init {
        tailTrack.add(tail.last())
//        println(">>> ${tail.size} / ${tail.last()}")
    }

    fun move(direction: Direction) {
        head = when (direction) {
            Direction.Right -> head.copy(first=head.first+1)
            Direction.Left -> head.copy(first=head.first-1)
            Direction.Up -> head.copy(second=head.second+1)
            Direction.Down -> head.copy(second=head.second-1)
        }

        var parent = head
        for (pair in tail.withIndex()) {
            val child = pair.value

            if (parent != child && (abs(parent.first -child.first) > 1 || abs(parent.second -child.second) > 1)) {
                tail[pair.index] = if (parent.first ==child.first) {
                    child.copy(second =child.second + (parent.second -child.second).sign)
                } else if (parent.second ==child.second) {
                    child.copy(first =child.first + (parent.first -child.first).sign)
                } else {
                    Pair(
                        child.first + (parent.first -child.first).sign,
                        child.second + (parent.second -child.second).sign
                    )
                }
            }
//            println("\t#${pair.index}. ${parent} / ${child} -> ${tail[pair.index]}")

            parent = tail[pair.index]
        }

        tailTrack.add(tail.last())
//        println(">>> ${tail.last()} ! ${tailTrack.size}")
    }

    fun move(directionPair: DirectionPair) {
        repeat(directionPair.distance) {
            move(directionPair.direction)
        }
    }

}
