package com.ericrochester.advent2022

import kotlin.test.assertEquals
import kotlin.test.Ignore
import kotlin.test.Test
import kotlin.test.assertContains

class TestDay11 {
    private val inputData = javaClass.getResource("/examples/day11.txt")?.readText() ?: ""
    private val day = Day11()

    @Test fun testDay11RunA() {
        val output = day.runA(inputData)
        assertEquals(10605, output)
    }

    @Ignore @Test fun testDay11RunB() {
        val output = day.runB(inputData)
        assertEquals(TODO(), output)
    }
}

class TestMonkey {
    val monkey = Monkey(
        1,
        mutableListOf(54, 65, 75, 74),
        OpPair.plus(6),
        OpPair.divisible(19),
        ThrowTo(2, 0)
    )

    @Test fun testParse() {
        val input = """
            |Monkey 1:
            |  Starting items: 54, 65, 75, 74
            |  Operation: new = old + 6
            |  Test: divisible by 19
            |    If true: throw to monkey 2
            |    If false: throw to monkey 0
            |
        """.trimMargin()
        val expected = monkey

        assertEquals(expected, Monkey.parse(input.lines()))
    }

    @Test fun testParseOldOld() {
        val input = """
            |Monkey 2:
            |  Starting items: 79, 60, 97
            |  Operation: new = old * old
            |  Test: divisible by 13
            |    If true: throw to monkey 1
            |    If false: throw to monkey 3
        """.trimMargin()
        val expected = Monkey(
            2,
            mutableListOf(79, 60, 97),
            OpPair.pow(2),
            OpPair.divisible(13),
            ThrowTo(1, 3)
        )
        assertEquals(expected, Monkey.parse(input.lines()))
    }

    @Test fun testMove() {
        val expected = listOf(
            ThrowInfo(0, 20),
            ThrowInfo(0, 23),
            ThrowInfo(0, 27),
            ThrowInfo(0, 26)
        )
        assertEquals(expected, monkey.move())
        assertEquals(4, monkey.itemsSeen)
    }

    @Test fun testTracksItemsSeen() {
        monkey.catchItem(42)
        assertContains(monkey.items, 42)
    }
}