package com.ericrochester.advent2022

import java.io.File
import kotlin.test.Test
import kotlin.test.assertContains
import kotlin.test.assertEquals

class TestDay09 {
    private val inputData = File("../sample/2022/day09.txt").readText(Charsets.UTF_8)
    private val day = Day09()

    @Test
    fun testDay09RunA() {
        val output = day.runA(inputData)
        assertEquals(13, output)
    }

    @Test
    fun testDay09RunB() {
        val inputDataB = javaClass.getResource("/examples/day09b.txt")?.readText() ?: ""
        val output = day.runB(inputDataB)
        assertEquals(36, output)
    }

    @Test fun testParseDirection() {
        val direction = DirectionPair.parse("R 4")
        assertEquals(DirectionPair(Direction.Right, 4), direction)
    }

    @Test fun testMoveDirection() {
        val board = Board(Pair(0, 0), mutableListOf(Pair(0, 0)))
        board.move(Direction.Right)
        assertEquals(Pair(1, 0), board.head)
    }

    @Test fun testMoveTailFollowsRight() {
        val board = Board(Pair(1, 0), mutableListOf(Pair(0, 0)))
        board.move(Direction.Right)
        assertEquals(Pair(2, 0), board.head)
        assertEquals(Pair(1, 0), board.tail.first())
    }

    @Test fun testMoveTailFollowsDiagonal() {
        val board = Board(Pair(1, 1), mutableListOf(Pair(0, 0)))
        board.move(Direction.Up)
        assertEquals(Pair(1, 2), board.head)
        assertEquals(Pair(1, 1), board.tail.first())
    }

    @Test fun testBoardTracksTail() {
        val board = Board(Pair(1, 0), mutableListOf(Pair(0, 0)))
        assertContains(board.tailTrack, Pair(0, 0))
    }

    @Test fun testMoveTracksTail() {
        val board = Board(Pair(1, 0), mutableListOf(Pair(0, 0)))
        board.move(Direction.Right)
        assertContains(board.tailTrack, Pair(1, 0))
    }

    @Test fun testMoveDirectionPair() {
        val board = Board(Pair(0, 0), mutableListOf(Pair(0, 0)))
        board.move(DirectionPair(Direction.Right, 4))
        assertEquals(Pair(4, 0), board.head)
        assertEquals(Pair(3, 0), board.tail.first())
        assertEquals(setOf(
            Pair(0, 0), Pair(1, 0), Pair(2, 0), Pair(3, 0)
        ), board.tailTrack)
    }

    @Test fun testBoardHasMultipleTails() {
        val board = Board(Pair(0, 0), mutableListOf(Pair(0, 0), Pair(0, 0)))
        board.move(DirectionPair(Direction.Right, 4))
        assertEquals(Pair(4, 0), board.head)
        assertEquals(
            listOf(Pair(3, 0), Pair(2, 0)),
            board.tail
        )
        assertEquals(setOf(
            Pair(0, 0), Pair(1, 0), Pair(2, 0),
        ), board.tailTrack)
    }

}