
# Read and process input --------------------------------------------------------------


input <- readLines("06_input.txt")
input <- strsplit(input, "")[[1]]

# Helper function(s) --------------------------------------------------------


detect_packet <- function(input, packet = character(0), characters_processed = 0) {
  # end of input reached. exit recursion
  if (length(input) == 0) {
    return(list(char_count = NA_integer_, char = "no start of packet marker received"))
  }

  # check if packet is free of duplicates

  # if so stop recurring & return result
  if (length(packet) == 4 & anyDuplicated(packet) == 0) {
    return(list(char_count = characters_processed, char = tail(packet,1)))

  }

  #otherwise update and recur

  # get first letter of input vector
  new_char <- input[1]

  # remove first letter from input
  input <- input[-1]

   # increment characters_rpocessed
  characters_processed <- characters_processed + 1

  #update packet
  if (length(packet) < 4){
    new_packet <- c(packet, new_char)} else {
  new_packet <- c(packet[-1], new_char)
    }

  #recursion
 detect_packet(input, new_packet, characters_processed)

}

detect_packet2 <- function(input, n_char = 4){

  packet <- head(input, n_char)
  characters_processed <- n_char
  input <- input[-(1:n_char)]

  while(anyDuplicated(packet) != 0 ){

    packet <- c(packet[-1], input[1])
    input <- input[-1]
    characters_processed <- characters_processed + 1

  }

  list(char_count = characters_processed, char = tail(packet,1))

}

# Part 1 ------------------------------------------------------------------
detect_packet(input)
detect_packet2(input)


# Part 2 ------------------------------------------------------------------

#detect_packet(input)
#Error: C stack usage  15924544 is too close to the limit

detect_packet2(input, 14)
