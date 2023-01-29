package com.ericrochester.advent2022

import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class TestDay08 {
    private val inputData = File("../sample/2022/day08.txt").readText(Charsets.UTF_8)
    private val day = Day08()

    private val heights = listOf(
        listOf(3, 0, 3, 7, 3),
        listOf(2, 5, 5, 1, 2),
        listOf(6, 5, 3, 3, 2),
        listOf(3, 5 ,5, 4, 9),
        listOf(3, 5, 3, 9, 0)
    )

    @Test
    fun testDay08RunA() {
        val output = day.runA(inputData)
        assertEquals(21, output)
    }

    @Test
    fun testDay08RunB() {
        val output = day.runB(inputData)
        assertEquals(8, output)
    }

    @Test fun testIsVisibleReturnsTrueOnEdges() {
        assertTrue(day.isVisible(heights, 0, 3))
        assertTrue(day.isVisible(heights, 4, 3))
        assertTrue(day.isVisible(heights, 3, 0))
        assertTrue(day.isVisible(heights, 3, 4))
    }

    @Test fun testIsVisibleReturnsFalseWhenHiddenAllDirections() {
        assertFalse(day.isVisible(heights, 1, 3))
        assertFalse(day.isVisible(heights, 2, 2))
        assertFalse(day.isVisible(heights, 3, 3))
    }

    @Test fun testIsVisibleReturnsTrueIfSeenFromSides() {
        // visible from the right
        assertTrue(day.isVisible(heights, 2, 1))
        assertTrue(day.isVisible(heights, 2, 3))
    }

    @Test fun testIsVisibleReturnsTrueIfSeenFromTopOrBottom() {
        assertTrue(day.isVisible(heights, 1, 1))
        assertTrue(day.isVisible(heights, 1, 2))
        assertTrue(day.isVisible(heights, 2, 1))
        assertTrue(day.isVisible(heights, 3, 2))
    }

    @Test fun testScenicScore() {
        assertEquals(4, day.scenicScore(heights, 1, 2))
        assertEquals(4, day.scenicScore(heights, 3, 2))
        assertEquals(1, day.scenicScore(heights, 2, 2))
    }
}