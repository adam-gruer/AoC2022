library(stringr)
library(purrr)
procedure <- readLines("05_input.txt")[-(1:10)]
starting_stacks <- read.fwf("05_input.txt", widths = rep(4,9), n = 8)

starting_stacks_clean <- lapply(starting_stacks, str_remove_all, pattern = "\\s+|\\[|\\]")


starting_stacks_clean <- lapply(starting_stacks_clean, function(stack){
  stack[nchar(stack) > 0]
})


# Part 1 ------------------------------------------------------------------

move <- function(stacks, instruction, crate_mover=9000){
  parsed <- str_extract_all(instruction,"\\d+")
  parsed <- parsed[[1]] |> as.integer()

  n <-  parsed[1]
  from <- parsed[2]
  to <-  parsed[3]
  crates <-  stacks[[from]][seq(n)]
   stacks[[from]] <- stacks[[from]][-seq(n)]
  if(crate_mover == 9000){
    crates <- rev(crates)
  }
   stacks[[to]] <-  c(crates, stacks[[to]])
   stacks
}


final_stack <- purrr::reduce(procedure, move,  .init = starting_stacks_clean)

sapply(final_stack,`[`, 1) |>
  paste0(collapse = "")


# Part 2 ------------------------------------------------------------------

final_stack <- purrr::reduce(procedure, move, crate_mover = 9001,  .init = starting_stacks_clean)

sapply(final_stack,`[`, 1) |>
  paste0(collapse = "")

