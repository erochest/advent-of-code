package com.ericrochester.advent2022

class Day07: DayRuns<Int, Int> {
    val fullDiskSpace = 70000000
    val diskSpaceRequired = 30000000

    override fun runA(inputData: String): Int =
        inputData.lines()
            .fold(DirectoryBuilder()) { builder, line ->
                builder.exec(line)
                builder
            }
            .allDirectories()
            .filter { it.size <= 100000 }
            .sumOf { it.size }

    override fun runB(inputData: String): Int {
        val directoryBuilder = inputData.lines()
            .fold(DirectoryBuilder()) { builder, line ->
                builder.exec(line)
                builder
            }

        val currentSizeUsed = directoryBuilder.tree.size
        val currentFreeSpace = fullDiskSpace - currentSizeUsed
        val needToFree = diskSpaceRequired - currentFreeSpace

        return directoryBuilder
            .allDirectories()
            .sortedBy { it.size }
            .dropWhile { it.size < needToFree }
            .first()
            .size
    }

}

class DirectoryBuilder() {
    val tree = Directory("/", null)
    var current = tree

    fun exec(command: String) {
        if (command == "$ ls") {
        } else if (command == "$ cd /") {
            current = tree
        } else if (command == "$ cd ..") {
            current = current.parent ?: tree
        } else if (command.startsWith("$ cd ")) {
            val name = command.substring(5)
            val child = Directory(name, current)
            current.children.put(name, child)
            current = child
        } else if (command.startsWith("dir ")) {
            val name = command.split("\\s+".toRegex())[1]
            current.children[name] = Directory(name, current)
        } else {
            val parts = command.split("\\s+".toRegex())
            val size = parts[0].toInt()
            val name = parts[1]
            val fileInfo = FileInfo(name, size)
            current.files[name] = fileInfo
        }
    }

    fun allDirectories(): List<Directory> {
        val accum = mutableListOf<Directory>()
        val queue = ArrayDeque<Directory>()
        queue.add(tree)

        while (queue.size > 0) {
            val cursor = queue.removeLast()
            accum.add(cursor)
            queue.addAll(cursor.children.values)
        }

        return accum.toList()
    }
}

data class FileInfo(val name: String, val size: Int)

data class Directory(val name: String, val parent: Directory?) {
    val children = mutableMapOf<String, Directory>()
    val files = mutableMapOf<String, FileInfo>()

    val size: Int
        get() =
            children.values.sumOf { it.size } + files.values.sumOf { it.size }
}