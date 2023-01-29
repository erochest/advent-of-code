import java.io.File
import com.ericrochester.advent2022.DayRuns
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.types.int

class Advent2022: CliktCommand() {
    val test by option("--test", "-t").flag()
    val year by option("--year", "-y").int().default(2022)
    val day by option("--day", "-d")

    override fun run() {
        val dayNumber = day?.substring(0, 2)?.toInt() ?: 1

        val resourceName = if (test) {
            "../sample/%04d/day%02d.txt".format(year, dayNumber)
        } else {
            "../data/%04d/day%02d.txt".format(year, dayNumber)
        }

        val inputData =  File(resourceName).readText(Charsets.UTF_8)
        val className = "com.ericrochester.advent%04d.Day%02d".format(year, dayNumber)
        val cls = Class.forName(className)
        val dayInstance = cls.getDeclaredConstructor().newInstance() as DayRuns<*, *>
        val output = if (day?.endsWith('a', true) != false) {
            dayInstance.runA(inputData)
        } else {
            dayInstance.runB(inputData)
        }
        println(output)
    }
}

fun main(args: Array<String>) = Advent2022().main(args)

private class ResourceReader {
    companion object {
        fun readResource(name: String): String {
            return ResourceReader::class.java.getResource(name)?.readText() ?: ""
        }
    }
}