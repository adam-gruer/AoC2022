
# Part A ------------------------------------------------------------------

input <- readLines("03a_input.txt")

find_duplicate_item <- function(rucksack){
size <- nchar(rucksack)
mid <- size / 2

x <- stringr::str_sub(rucksack,1, mid) |>
stringr::str_split(stringr::boundary("character")) |>
  unlist() |>
  unique()

y <- stringr::str_sub(rucksack, mid + 1) |>
  stringr::str_split(stringr::boundary("character")) |>
  unlist() |>
  unique()

x[x %in% y]
}

priority <- seq_along(c(letters, LETTERS))
names(priority) <- c(letters, LETTERS)

priority[ purrr::map_chr(input, find_duplicate_item)] |>
  sum()


# Part B ------------------------------------------------------------------

start <- seq(from = 1, by = 3, to = length(input))
end <- seq(from = 0, by = 3, to = length(input))[-1]

find_dupe <- function(rucksack1, rucksack2){
  x <- stringr::str_split(rucksack1, stringr::boundary("character")) |>
    unlist() |>
    unique()

  y <- stringr::str_split(rucksack2, stringr::boundary("character")) |>
    unlist() |>
    unique()

  x[x %in% y]
}

find_badge <- function(group){
  r1 <- find_dupe(group[1], group[2]) |>
    paste0()

  find_dupe(r1, group[3])
}

priority[mapply(function(start, end){
  group <- input[start:end]
  find_badge(group)
},
start,
end)
] |>
  sum()

