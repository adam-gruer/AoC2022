library(stringr)
procedure <- readLines("05_input.txt")[-(1:10)]
starting_stacks <- read.fwf("05_input.txt", widths = rep(4,9), n = 8)


starting_stacks_clean <- lapply(starting_stacks, str_remove_all, pattern = "\\s+|\\[|\\]")


starting_stacks_clean <- lapply(starting_stacks_clean, function(stack){
  stack[nchar(stack) > 0]
})


move <- function(instruction, stacks){
  parsed <- str_extract_all(instruction,"\\d+")
  parsed <- parsed[[1]] |> as.integer()

  n <-  parsed[1]
  from <- parsed[2]
  to <-  parsed[3]
  crates <-  stacks[[from]][seq(n)]
   stacks[[from]] <- stacks[[from]][-seq(n)]
   stacks[[to]] <-  c(rev(crates), stacks[[to]])
   stacks
}
debugonce(move)

move(procedure[1], starting_stacks_clean)

move(6,9,3, starting_stacks_clean)

