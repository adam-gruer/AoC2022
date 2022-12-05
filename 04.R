input <- readLines("04_input.txt")

# Part 1 ------------------------------------------------------------------


pattern <- "(\\d+)-(\\d+),(\\d+)-(\\d+)"

matches <- regmatches(input, regexec(pattern, input))

pairs <- lapply(matches, function(x) {
  list(
    elf_1 = seq(x[2], x[3]),
    elf_2 = seq(x[4], x[5])
  )
})

intersects <- lapply(pairs, function(pair) {
  intersect(pair$elf_1, pair$elf_2)
})

mapply(
  function(pair, intersection) {
    setequal(pair$elf_1, intersection) |
      setequal(pair$elf_2, intersection)
  }, pairs,
  intersects
) |>
  sum()


# Part 2 ------------------------------------------------------------------

sum(lengths(intersects) > 0)
