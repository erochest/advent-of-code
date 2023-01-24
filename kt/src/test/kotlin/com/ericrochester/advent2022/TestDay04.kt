package com.ericrochester.advent2022

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse

class TestDay04 {
    private val inputData = javaClass.getResource("/examples/day04.txt")?.readText() ?: ""
    private val day04 = Day04()

    @Test
    fun testDay04RunA() {
        val output = day04.runA(inputData)
        assertEquals(2, output)
    }

    @Test
    fun testDay04RunB() {
        val output = day04.runB(inputData)
        assertEquals(4, output)
    }

    @Test
    fun testParseAssignmentPairs() {
        val expected = Pair(setOf(2, 3, 4), setOf(6, 7, 8))
        assertEquals(expected, day04.parseAssignmentPairs("2-4,6-8"))
    }

    @Test
    fun testIsOneSubsumed() {
        var input = Pair(setOf(2, 3, 4), setOf(6, 7, 8))
        assertFalse(day04.isOneSubsumed(input))

        input = Pair(setOf(5, 6, 7), setOf(7, 8, 9))
        assertFalse(day04.isOneSubsumed(input))

        input = Pair(setOf(2, 3, 4, 5, 6, 7, 8), setOf(3, 4, 5, 6, 7))
        assert(day04.isOneSubsumed(input))

        input = Pair(setOf(6), setOf(4, 5, 6))
        assert(day04.isOneSubsumed(input))
    }
}