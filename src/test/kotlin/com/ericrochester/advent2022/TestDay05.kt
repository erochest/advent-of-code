package com.ericrochester.advent2022

import kotlin.test.Test
import kotlin.test.assertEquals

class TestDay05 {
    private val inputData = javaClass.getResource("/examples/day05.txt")?.readText() ?: ""
    private val day = Day05()

    @Test
    fun testDay05RunA() {
        val output = day.runA(inputData)
        assertEquals("CMZ", output)
    }

    @Test
    fun testDay05RunB() {
        val output = day.runB(inputData)
        assertEquals("MCD", output)
    }

    @Test
    fun testParseStacks() {
        val input = """
            |    [D]
            |[N] [C]
            |[Z] [M] [P]
            | 1   2   3
            """.trimMargin().lines()
        val elfStacks = day.parseStacks(input)
        val expected = ElfStacks.withStacks(
            ArrayDeque(listOf('Z', 'N')),
            ArrayDeque(listOf('M', 'C', 'D')),
            ArrayDeque(listOf('P'))
        )
        assertEquals(expected, elfStacks)
    }

    @Test
    fun testParseStackMove() {
        val input = "move 1 from 2 to 1"
        val expected = StackMove(1, 2, 1)
        assertEquals(expected, day.parseStackMove(input))
    }

    @Test
    fun testMakeStackMoveSingle() {
        val stacks = ElfStacks.withStacks(
            ArrayDeque(listOf('Z', 'N')),
            ArrayDeque(listOf('M', 'C', 'D')),
            ArrayDeque(listOf('P'))
        )
        val move = StackMove(1, 2, 1)
        val expected = ElfStacks.withStacks(
            ArrayDeque(listOf('Z', 'N', 'D')),
            ArrayDeque(listOf('M', 'C')),
            ArrayDeque(listOf('P'))
        )
        stacks.makeStackMove(move)
        assertEquals(expected, stacks)
    }

    @Test
    fun testMakeStackMoveMultiple() {
        val stacks = ElfStacks.withStacks(
            ArrayDeque(listOf('Z', 'N', 'D')),
            ArrayDeque(listOf('M', 'C')),
            ArrayDeque(listOf('P'))
        )
        val move = StackMove(3, 1, 3)
        val expected = ElfStacks.withStacks(
            ArrayDeque(listOf()),
            ArrayDeque(listOf('M', 'C')),
            ArrayDeque(listOf('P', 'D', 'N', 'Z'))
        )
        stacks.makeStackMove(move)
        assertEquals(expected, stacks)
    }

    @Test
    fun testMakeStackMoveBSingle() {
        val stacks = ElfStacks.withStacks(
            ArrayDeque(listOf('Z', 'N')),
            ArrayDeque(listOf('M', 'C', 'D')),
            ArrayDeque(listOf('P'))
        )
        val move = StackMove(1, 2, 1)
        val expected = ElfStacks.withStacks(
            ArrayDeque(listOf('Z', 'N', 'D')),
            ArrayDeque(listOf('M', 'C')),
            ArrayDeque(listOf('P'))
        )
        stacks.makeStackMoveB(move)
        assertEquals(expected, stacks)
    }

    @Test
    fun testMakeStackMoveBMultiple() {
        val stacks = ElfStacks.withStacks(
            ArrayDeque(listOf('Z', 'N', 'D')),
            ArrayDeque(listOf('M', 'C')),
            ArrayDeque(listOf('P'))
        )
        val move = StackMove(3, 1, 3)
        val expected = ElfStacks.withStacks(
            ArrayDeque(listOf()),
            ArrayDeque(listOf('M', 'C')),
            ArrayDeque(listOf('P', 'Z', 'N', 'D'))
        )
        stacks.makeStackMoveB(move)
        assertEquals(expected, stacks)
    }


    @Test
    fun testPeekAll() {
        val input = ElfStacks.withStacks(
            ArrayDeque(listOf('Z', 'N')),
            ArrayDeque(listOf('M', 'C', 'D')),
            ArrayDeque(listOf('P'))
        )
        assertEquals(listOf('N', 'D', 'P'), input.peekAll())
    }
}