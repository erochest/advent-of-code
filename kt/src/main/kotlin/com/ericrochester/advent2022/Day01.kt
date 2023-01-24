package com.ericrochester.advent2022

class Day01: DayRuns<Int, Int> {
    override fun runA(inputData: String): Int {
        val maxCalorieSums = paragraphs(inputData.lines()).map {
            it.sumOf { line: String -> line.toInt() }
        }.sortedDescending()
        return maxCalorieSums.first()
    }

    override fun runB(inputData: String): Int {
        val maxCalorieSums = paragraphs(inputData.lines()).map {
            it.sumOf { line: String -> line.toInt() }
        }.sortedDescending()
        return maxCalorieSums.take(3).sum()
    }


}
