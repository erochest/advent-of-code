package com.ericrochester.advent2022

typealias AssignmentPair = Pair<Set<Int>, Set<Int>>

class Day04: DayRuns<Int, Int> {
    override fun runA(inputData: String): Int =
        inputData.lines()
            .map { parseAssignmentPairs(it) }
            .count { isOneSubsumed(it) }

    override fun runB(inputData: String): Int =
        inputData.lines()
            .map { parseAssignmentPairs(it) }
            .count { it.first.intersect(it.second).isNotEmpty() }

    fun parseAssignmentPairs(input: String): AssignmentPair {
        return input.split(',')
            .map {
                val endpoints = it.split('-')
                IntRange(endpoints[0].toInt(), endpoints[1].toInt()).toSet()
            }
            .zipWithNext()
            .first()
    }

    fun isOneSubsumed(pair: AssignmentPair): Boolean =
        (pair.first.containsAll(pair.second)) ||
                (pair.second.containsAll(pair.first))

}
