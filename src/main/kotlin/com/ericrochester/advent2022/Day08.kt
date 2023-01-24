package com.ericrochester.advent2022

class Day08: DayRuns<Int, Int> {
    override fun runA(inputData: String): Int {
        val heights = inputData.lines()
            .map { it.map { c -> c.toString().toInt() } }
        val gridHeight = heights.size
        val gridWidth = heights[0].size
        var hidden = 0

        for (r in 0 until gridHeight) {
            for (c in 0 until gridWidth) {
//                println("${r}x${c} | ${isVisible(heights, r, c)}")
                if (isVisible(heights, r, c)) {
                    hidden += 1
                }
            }
        }

        return hidden
    }

    override fun runB(inputData: String): Int {
        val heights = inputData.lines()
            .map { it.map { c -> c.toString().toInt() } }
        val gridHeight = heights.size
        val gridWidth = heights[0].size
        var maxScore = 0

        for (r in 0 until gridHeight) {
            for (c in 0 until gridWidth) {
//                println("${r}x${c} | ${isVisible(heights, r, c)}")
                maxScore = maxOf(maxScore, scenicScore(heights, r, c))
            }
        }

        return maxScore
    }

    fun isVisible(heights: List<List<Int>>, r: Int, c: Int): Boolean {
        val gridHeight = heights.size
        val gridWidth = heights[0].size
        val treeHeight = heights[r][c]

        if (r == 0 || r == gridHeight - 1 || c == 0 || c == gridWidth - 1) {
            return true
        }

        val fromTop = (0 until r).map { heights[it][c] }.all { it < treeHeight }
        val fromBottom = ((r + 1) until gridHeight).map { heights[it][c] }.all { it < treeHeight }

        val fromLeft = heights[r].subList(0, c).all { it < treeHeight }
        val fromRight = heights[r].subList(c + 1, gridWidth).all { it < treeHeight }

        return fromTop || fromBottom || fromRight || fromLeft
    }

    fun scenicScore(heights: List<List<Int>>, r: Int, c: Int): Int {
        val gridHeight = heights.size
        val gridWidth = heights[0].size
        val treeHeight = heights[r][c]

        var left = 0
        for (c1 in (c-1) downTo 0) {
            left += 1
            if (heights[r][c1] >= treeHeight) {
                break
            }
        }

        var above = 0
        for (r1 in (r-1) downTo 0) {
            above += 1
            if (heights[r1][c] >= treeHeight) {
                break
            }
        }

        var right = 0
        for (c1 in (c+1) until gridWidth) {
            right += 1
            if (heights[r][c1] >= treeHeight) {
                break
            }
        }

        var below = 0
        for (r1 in (r+1) until gridHeight) {
            below += 1
            if (heights[r1][c] >= treeHeight) {
                break
            }
        }

        return above * left * right * below
    }

}
