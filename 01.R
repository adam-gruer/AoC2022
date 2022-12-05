# Part A ------------------------------------------------------------------
puzzle_input <- readLines("01a_input.txt")

runs <- rle(nchar(puzzle_input) > 0)

end_index <- cumsum(runs$lengths)[seq(from = 2, to = length(runs$lengths), by = 2)]
start_index <- c(1, end_index + 1)[-length(end_index)]

calories_per_elf <- mapply(function(x, y){
  sum(as.integer(puzzle_input)[x:y], na.rm = TRUE)
  },
  start_index,
  end_index)
max(calories_per_elf)


# PArt B ------------------------------------------------------------------
sum(sort.int(calories_per_elf, decreasing = TRUE)[1:3])

