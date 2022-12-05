
# Part A ------------------------------------------------------------------

input <- read.csv("02a_input.txt", sep = " ", header = FALSE)

draw <- c(X = "A",
  Y = "B",
  Z = "C")

draws <- input$V1 == draw[input$V2]
draws_score <- draws * 3

win <- c(X = "C",
         Y = "A",
         Z = "B")
wins <- input$V1 == win[input$V2]
wins_score <- wins * 6

play <- c(X = 1,
          Y = 2,
          Z = 3)
played_score <- play[input$V2]

score <- sum(played_score + wins_score + draws_score)
score


# PArt B ------------------------------------------------------------------

lose_draw_win_score <- c(X = 0,
                  Y = 3,
                  Z = 6)

result_score <- lose_draw_win_score[input$V2]

win <- c(A = "B",
         B = "C",
         C = "A")

lose <- c(A = "C",
         B = "A",
         C = "B")

play <- c(A = 1,
          B = 2,
          C = 3)

draws <- input$V1[input$V2 == "Y"]

losses <- lose[input$V1[input$V2 == "X"]]

wins <- win[input$V1[input$V2 == "Z"]]
draws_play <- play[draws]
losses_play <- play[losses]
wins_play <- play[wins]

sum(draws_play) + sum(losses_play) + sum(wins_play) +
  sum(result_score)
