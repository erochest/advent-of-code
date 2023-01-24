package com.ericrochester.advent2022

import kotlin.test.Test
import kotlin.test.assertEquals

class TestDay01 {
    private val inputData = javaClass.getResource("/examples/day01.txt").readText()
    private val day01 = Day01()

    @Test
    fun testRunA() {
        val output = day01.runA(inputData)
        assertEquals(24000, output)
    }

    @Test
    fun testParagraphs() {
        val paras = paragraphs(inputData.lines())
        val expected = listOf(
            listOf("1000", "2000", "3000"),
            listOf("4000"),
            listOf("5000", "6000"),
            listOf("7000", "8000", "9000"),
            listOf("10000")
        )
        assertEquals(expected, paras)
    }

    @Test
    fun testRunB() {
        val output = day01.runB(inputData)
        assertEquals(45000, output)
    }
}