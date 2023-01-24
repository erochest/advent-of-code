import com.ericrochester.advent2022.Day01
import com.ericrochester.advent2022.DayRuns
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option

class Advent2022: CliktCommand() {
    val test by option("--test", "-t").flag()
    val day by option("--day", "-d")

    override fun run() {
        val dayNumber = day?.substring(0, 2)?.toInt() ?: 1

        val resourceName = if (test) {
            "/examples/day%02d.txt".format(dayNumber)
        } else {
            "/data/day%02d.txt".format(dayNumber)
        }

        val inputData =  ResourceReader.readResource(resourceName)
        val className = "com.ericrochester.advent2022.Day%02d".format(dayNumber)
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