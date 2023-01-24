package com.ericrochester.advent2022

class Day10: DayRuns<Int,String> {

    companion object {
        val screenWidth = 40
    }

    override fun runA(inputData: String): Int {
        val history = execute(inputData.lines())
        return (20..(history.size) step 40)
            .sumOf { signalStrength(history, it) }
    }

    override fun runB(inputData: String): String {
        val history = execute(inputData.lines())
        // TODO the line below is partially also in a test case. extract it
//        println("HISTORY (${history.size}) ${history}")
        val output = history
            .dropLast(1)
            .indices
            .map { drawPixel(history[it], it) }.joinToString("")
//        println("OUTPUT ${output}")
        return output.chunked(40).joinToString("\n")
    }

}

data class Cpu(var register: Int) {
    val history: MutableList<Int> = mutableListOf()

    val output: List<Int>
        get() {
            val output = mutableListOf<Int>()
            output.addAll(history)
            output.add(register)
            return output.toList()
        }

    fun noop() {
        history.add(register)
    }

    fun addx(x: Int) {
        history.add(register)
        history.add(register)
        register += x
    }
}

fun execute(commands: List<String>): List<Int> {
    return commands.fold(Cpu(1)) { cpu, command ->
        if (command == "noop") {
            cpu.noop()
        } else {
            val parts = command.split(' ')
            val value = parts[1].toInt()
            cpu.addx(value)
        }
        cpu
    }.output
}

fun signalStrength(cpuOutput: List<Int>, cycle: Int): Int {
    return cycle * cpuOutput[cycle - 1]
}

fun drawPixel(register: Int, pixel: Int): Char {
    val column = pixel % Day10.screenWidth
//    println("*** ${pixel}. ${register}, ${column}\t*** ${column == (register - 1)} || ${column == register} || ${column == (register + 1)}")
    return if (column == (register - 1) || column == register || column == (register + 1)) {
        '#'
    } else {
        '.'
    }
}
