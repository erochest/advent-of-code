package com.ericrochester.advent2022

import java.io.File
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class TestDay03 {
    private val inputData = File("../sample/2022/day03.txt").readText(Charsets.UTF_8)
    private val day03 = Day03()

    @Test
    fun testDay03RunA() {
        val output = day03.runA(inputData)
        assertEquals(157, output)
    }

    @Test
    fun testDay03RunB() {
        val output = day03.runB(inputData)
        assertEquals(70, output)
    }

    @Test
    fun testParseRuckSackA() {
        val left = "vJrwpWtwJgWr".toSet()
        val right = "hcsFMMfFFhFp".toSet()
        val ruckSack = Pair(left, right)
        assertEquals(ruckSack, day03.parseRuckSackA("vJrwpWtwJgWrhcsFMMfFFhFp"))
    }

    @Test
    fun testItemPriority() {
        // 16 (p), 38 (L), 42 (P), 22 (v), 20 (t), and 19 (s)
        assertEquals(16, day03.itemPriority('p'))
        assertEquals(38, day03.itemPriority('L'))
    }

    @Test
    fun testParseGroup() {
        val input = ("vJrwpWtwJgWrhcsFMMfFFhFp\n" +
                "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n" +
                "PmmdzqPrVvPwwTWBwg").lines()
        val expected = listOf(
            Pair("vJrwpWtwJgWr".toSet(), "hcsFMMfFFhFp".toSet()),
            Pair("jqHRNqRjqzjGDLGL".toSet(), "rsFMfFZSrLrFZsSL".toSet()),
            Pair("PmmdzqPrV".toSet(), "vPwwTWBwg".toSet())
        )
        assertEquals(expected, day03.parseGroup(input))
    }
}