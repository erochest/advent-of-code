package com.ericrochester.advent2022

interface DayRuns<A, B> {
    fun runA(inputData: String): A
    fun runB(inputData: String): B
}