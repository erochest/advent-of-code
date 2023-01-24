package com.ericrochester.advent2022

fun paragraphs(lines: List<String>): List<List<String>> {
    var buffer = lines
    val output = emptyList<List<String>>().toMutableList()
    while (buffer.isNotEmpty()) {
        val chunk = buffer.takeWhile { it.isNotEmpty() }
        buffer = buffer.drop(chunk.size)
        output.add(chunk)
        buffer = buffer.drop(buffer.takeWhile { it.isEmpty() }.size)
    }
    return output
}
