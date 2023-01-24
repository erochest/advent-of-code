import kotlin.test.Test
import com.github.stefanbirkner.systemlambda.SystemLambda.tapSystemOut
import kotlin.test.assertEquals

class TestMain {
    @Test
    fun testMainRunsWithoutException() {
        main(emptyArray())
    }

    @Test
    fun testDay01ARuns() {
        val systemOut = tapSystemOut {
            main(arrayListOf("--test", "--day", "01A").toTypedArray())
        }
        assertEquals("24000", systemOut.trim())
    }

    @Test
    fun testDay01BRuns() {
        val systemOut = tapSystemOut {
            main(arrayListOf("--test", "--day", "01B").toTypedArray())
        }
        assertEquals("45000", systemOut.trim())
    }

    @Test
    fun testDay02ARuns() {
        val systemOut = tapSystemOut {
            main(arrayListOf("--test", "--day", "02A").toTypedArray())
        }
        assertEquals("15", systemOut.trim())
    }
}