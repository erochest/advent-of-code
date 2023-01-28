
# For some reason these aren't working on my (Windows, ofc) machine,
# so I'm keeping these for documentation.

TEST_WORD?=test-all

all: day01 day02 day03 day04 day05 day06 day07

day01:
	gforth s/2020/day01.fth t/2020/test-day01.fth -e bye

day02:
	gforth s/2020/day02.fth t/2020/test-day02.fth -e bye

day03:
	gforth s/2020/day03.fth t/2020/test-day03.fth -e bye

day04:
	gforth s/2020/day04.fth t/2020/test-day04.fth -e '$(TEST_WORD)' -e bye
	gforth s/2020/day04.fth -e parta -e bye
	gforth s/2020/day04.fth -e partb -e bye

day05:
	gforth s/day05.fth t/test-day05.fth -e '$(TEST_WORD)' -e bye
	gforth s/day05.fth -e day05a -e bye
	gforth s/day05.fth -e day05b -e bye

files:
	gforth s/debug.fth s/files.fth t/test-files.fth -e '$(TEST_WORD)' -e bye

paragraphs:
	gforth s/files.fth s/paragraphs.fth t/test-paragraphs.fth -e '$(TEST_WORD)' -e bye

bytes:
	gforth s/bytes.fth t/test-bytes.fth -e '$(TEST_WORD)' -e bye

array:
	gforth s/debug.fth s/array.fth t/test-array.fth -e '$(TEST_WORD)' -e bye

queue-set:
	gforth s/debug.fth s/queue-set.fth t/test-queue-set.fth -e '$(TEST_WORD)' -e bye

strings:
	gforth s/debug.fth s/strings.fth t/test-strings.fth -e '$(TEST_WORD)' -e bye

.PHONY: day01 day02 day03 day04 day05
.PHONY: files paragraphs bytes array queue-set strings

day06:
	gforth s/day06.fth t/test-day06.fth -e '$(TEST_WORD)' -e bye
	gforth s/day06.fth -e day06a -e bye
	gforth s/day06.fth -e day06b -e bye

.PHONY: day06

day07:
	gforth s/day07.fth t/test-day07.fth -e '$(TEST_WORD)' -e bye
	gforth s/day07.fth -e day07a -e bye
	gforth s/day07.fth -e day07b -e bye

.PHONY: day07
