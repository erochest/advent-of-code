package com.ericrochester.advent2022

class Day06: DayRuns<Int, Int> {
    override fun runA(inputData: String): Int =
        findFirstIndexAfterUniqueSequence(inputData, 4)

    private fun findFirstIndexAfterUniqueSequence(input: String, size: Int): Int {
        val first = input
            .withIndex()
            .windowed(size)
            .first { chunk ->
                chunk.map { it.value }.toSet().size == size
            }
        return first.last().index + 1
    }

    override fun runB(inputData: String): Int =
        findFirstIndexAfterUniqueSequence(inputData, 14)

}
