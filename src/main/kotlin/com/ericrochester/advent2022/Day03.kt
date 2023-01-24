package com.ericrochester.advent2022

typealias RuckSackA = Pair<Set<Char>, Set<Char>>
typealias ElfGroup = List<RuckSackA>

class Day03 : DayRuns<Int, Int> {
    override fun runA(inputData: String): Int {
        return inputData.lines()
            .sumOf { line: String ->
                val ruckSackA = parseRuckSackA(line)
                val common = ruckSackA.first.intersect(ruckSackA.second)
                common.sumOf { itemPriority(it) }
            }
    }

    override fun runB(inputData: String): Int =
        inputData.lines().chunked(3)
            .sumOf { groupLines: List<String> ->
                groupChecksum(parseGroup(groupLines))
            }

    private fun groupChecksum(group: ElfGroup): Int = group
        .map { it.first.union(it.second) }
        .reduce { accum: Set<Char>, item: Set<Char> ->
            accum.intersect(item)
        }
        .sumOf { itemPriority(it) }

    fun parseRuckSackA(line: String): RuckSackA {
        val midpoint = line.length / 2
        val first = line.subSequence(0, midpoint).toSet()
        val second = line.subSequence(midpoint, line.length).toSet()
        return Pair(first, second)
    }

    fun itemPriority(item: Char): Int {
        return if (item.isLowerCase())
            (item.code - 'a'.code) + 1
        else
            (item.code - 'A'.code) + 27
    }

    fun parseGroup(input: List<String>): ElfGroup =
        input.map { parseRuckSackA(it) }

}
