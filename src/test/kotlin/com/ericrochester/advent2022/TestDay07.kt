package com.ericrochester.advent2022

import kotlin.test.Test
import kotlin.test.assertContains
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals

class TestDay07 {
    private val inputData = javaClass.getResource("/examples/day07.txt")?.readText() ?: ""
    private val day = Day07()

     @Test
     fun testDay07RunA() {
         val output = day.runA(inputData)
         assertEquals(95437, output)
     }

     @Test
     fun testDay07RunB() {
         val output = day.runB(inputData)
         assertEquals(24933642, output)
     }

    @Test
    fun testParseCdRoot() {
        val builder = DirectoryBuilder()
        builder.exec("$ cd /")
        assertEquals("/", builder.current.name)
        assertEquals("/", builder.tree.name)
        assertContentEquals(listOf(), builder.tree.children.values)
        assertContentEquals(listOf(), builder.tree.files.values)
    }

    @Test
    fun testCdRootFromCdild() {
        val builder = DirectoryBuilder()
        val directoryA = Directory("a", builder.tree)
        builder.tree.children.put("a", directoryA)
        builder.current = directoryA
        builder.exec("$ cd /")
        assertEquals("/", builder.current.name)
    }

    @Test
    fun testParseCdChild() {
        val builder = DirectoryBuilder()
        builder.exec("$ cd /")
        builder.exec("$ cd a")
        assertEquals("a", builder.current.name)
        assertContains(builder.tree.children.values, Directory("a", builder.tree))
    }

    @Test
    fun testAddDirectoryOnlyOnce() {
        val builder = DirectoryBuilder()
        builder.exec("$ cd /")
        builder.exec("$ cd a")
        builder.exec("$ cd /")
        builder.exec("$ cd a")
        assertContains(builder.current.children.values, Directory("a", builder.tree))
        assertEquals(1, builder.tree.children.size)
    }

    @Test
    fun testParseCdParent() {
        val builder = DirectoryBuilder()
        builder.exec("$ cd /")
        builder.exec("$ cd a")
        builder.exec("$ cd b")
        builder.exec("$ cd ..")
        assertEquals("a", builder.current.name)
    }

    @Test fun testParseLsFileList() {
        val builder = DirectoryBuilder()
        builder.exec("14848514 b.txt")
        assertEquals(1, builder.tree.files.size)
        assertContains(builder.tree.files.keys, "b.txt")
        assertContains(builder.tree.files.values, FileInfo("b.txt", 14848514))
    }

    @Test fun testParseLsChildDirectory() {
        val builder = DirectoryBuilder()
        builder.exec("dir a")
        assertEquals(1, builder.tree.children.size)
        assertContains(builder.tree.children.keys, "a")
        val child = builder.tree.children["a"]
        assertEquals("a", child?.name ?: "")
        assertEquals(builder.current, child?.parent)
    }

    @Test fun testDirectorySize() {
        val builder = DirectoryBuilder()
        builder.exec("$ cd e")
        builder.exec("$ ls")
        builder.exec("584 i")
        val e = builder.current
        assertEquals(584, e.size)
    }

    @Test fun testdirectorytreesize() {
        val builder = DirectoryBuilder()
        builder.exec("$ ls")
        builder.exec("14848514 b.txt")
        builder.exec("8504156 c.dat")
        builder.exec("$ cd e")
        builder.exec("$ ls")
        builder.exec("584 i")
        assertEquals(584+14848514+8504156, builder.tree.size)
    }

    @Test fun testAllDirectories() {
        val builder = DirectoryBuilder()
        builder.exec("$ cd e")
        builder.exec("$ ls")
        builder.exec("584 i")
        builder.exec("$ cd ..")
        builder.exec("$ cd a")
        builder.exec("$ cd b")
        builder.exec("$ cd ..")
        builder.exec("$ cd c")
        val directories = builder.allDirectories().map { it.name }.toSet()
        assertEquals(setOf("/", "a", "b", "c", "e"), directories)
    }
}