# I create the class Coordinates, useful to check if the coordinates of a ship's position are valid (so, both 1<x<10 and 1<y<10)

Coordinates <- setRefClass("Coordinates",
                           fields=list(
                             col = "numeric",
                             row = "numeric", 
                             isValid = "logical"
                           ),
                           methods=list(
                             initialize = function(newCol, newRow) {
                               col <<- newCol
                               row <<- newRow
                               # We need to check that col and row are valid
                               # coordinates for the chessboard (so >= 1 and <= 10)
                               if ((col >= 1) && (col <= 10) && 
                                   (row >= 1) && (row <= 10)) {
                                 isValid <<- TRUE
                               } else {
                                 isValid <<- FALSE
                               }
                             }
                           ))

# I am creating the class Battlefield.

Battlefield <- setRefClass("Battlefield",
                          fields=list(
                            battlefield = "matrix"
                          ),
                          methods=list(
                            initialize = function() {
                            # We initialize a 10x10 matrix and we fill it with empty lists. (Copied from Nardinocchi's code).
                            # I used "dimnames" to have all rows named with a letter, as in the battleship game.
                            battlefield <<- matrix(list(NULL), nrow = 10, ncol = 10, dimnames = list(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")))
                            },
                            # here I define the look of the grid, putting | and --- to create the "drawing".
                            draw = function() {
                              cat("    ")
                              for (header in rownames(battlefield)) {
                                cat(header, "   ", sep="")
                              }
                              cat("\n")
                              for (row in seq(1, 10)) {
                                if (row != 10){
                                cat(row, " ", sep="")
                                }  else {
                                   cat(row, "", sep="")
                                }
                                for (col in seq(1, 10)) {
                                  cat("|   ")
                                }
                                cat("|\n  ")
                                for (col in seq(1, 10)) {
                                  cat("----")
                                }
                                cat("-\n")
                              }
                            }))

# Here I create the class ship.

Ship <- setRefClass("Ship",
                    fields=list(
                      player = "numeric", #1 or 2
                      type = "character", #P, S, D, C
                      coordinates = "vector", #x and y
                      orientation = "character", #V or O
                      life = "numeric", #ranges from 1 to 4 - is equal to the lenght
                      isSunk = "logical", #to check if a ship is sunk or not (work in progress)
                      isValid = "logical"
                    ),
                    methods=list(
                      # We initialize the class ship.
                      initialize = function() {
                        # This part is to check if players provided valid imputs in the questions asked below. But it doesn't quite work so I commented it.
                        # if ((ship_type != "P") || (ship_type != "S") || (ship_type != "D") || (ship_type != "C")) {
                        #   stop ("You don't own a", ship_type,
                        #         "! Your arsenal includes n Patrol Boats, n Submarines, n Destroyers and n Carriers.")
                        # } else if ((ship_orientation != "O") || (ship_orientation != "V")) {
                        #   stop ("Your ships can only be placed orizontally or vertically. Please use 'O' or 'V'.")
                        # } else {
                        #   type <<- ship_type
                        #   orientation <<- ship_orientation
                        #   isSunk <<- FALSE
                        # }
                      }
                    ))

# Here I am creating every ship we need (1 carrier, 2 destroyers, 3 submarines and 4 patrol boat for each player, so 20 ships).
carrier1 <- Ship()
carrier2 <- Ship()

destroyer1_1 <- Ship()
destroyer1_2 <- Ship()

destroyer2_1 <- Ship()
destroyer2_2 <- Ship()

submarine1_1 <- Ship()
submarine1_2 <- Ship()
submarine1_3 <- Ship()

submarine2_1 <- Ship()
submarine2_2 <- Ship()
submarine2_3 <- Ship()

patrol_boat1_1 <- Ship()
patrol_boat1_2 <- Ship()
patrol_boat1_3 <- Ship()
patrol_boat1_4 <- Ship()

patrol_boat2_1 <- Ship()
patrol_boat2_2 <- Ship()
patrol_boat2_3 <- Ship()
patrol_boat2_4 <- Ship()

# Here I give to each ship all the attributes we need. I positionated them in a random-chosen position.

attributes(carrier1) <- list(player = 1, type = "C", coordinates = c(10, 10), orientation = "O", life = 4, isSunk = FALSE)
attributes(carrier2) <- list(player = 2, type = "C", coordinates = c(10, 10), orientation = "O", life = 4, isSunk = FALSE)

attributes(destroyer1_1) <- list(player = 1, type = "D", coordinates = c(10, 10), orientation = "O", life = 3, isSunk = FALSE)
attributes(destroyer1_2) <- list(player = 1, type = "D", coordinates = c(10, 10), orientation = "O", life = 3, isSunk = FALSE)

attributes(destroyer2_1) <- list(player = 2, type = "D", coordinates = c(10, 10), orientation = "O", life = 3, isSunk = FALSE)
attributes(destroyer2_2) <- list(player = 2, type = "D", coordinates = c(10, 10), orientation = "O", life = 3, isSunk = FALSE)

attributes(submarine1_1) <- list(player = 1, type = "S", coordinates = c(10, 10), orientation = "O", life = 2, isSunk = FALSE)
attributes(submarine1_2) <- list(player = 1, type = "S", coordinates = c(10, 10), orientation = "O", life = 2, isSunk = FALSE)
attributes(submarine1_3) <- list(player = 1, type = "S", coordinates = c(10, 10), orientation = "O", life = 2, isSunk = FALSE)

attributes(submarine2_1) <- list(player = 2, type = "S", coordinates = c(10, 10), orientation = "O", life = 2, isSunk = FALSE)
attributes(submarine2_2) <- list(player = 2, type = "S", coordinates = c(10, 10), orientation = "O", life = 2, isSunk = FALSE)
attributes(submarine2_3) <- list(player = 2, type = "S", coordinates = c(10, 10), orientation = "O", life = 2, isSunk = FALSE)

attributes(patrol_boat1_1) <- list(player = 1, type = "P", coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)
attributes(patrol_boat1_2) <- list(player = 1, type = "P", coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)
attributes(patrol_boat1_3) <- list(player = 1, type = "P", coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)
attributes(patrol_boat1_4) <- list(player = 1, type = "P", coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)

attributes(patrol_boat2_1) <- list(player = 2, type = "P", coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)
attributes(patrol_boat2_2) <- list(player = 2, type = "P", coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)
attributes(patrol_boat2_3) <- list(player = 2, type = "P", coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)
attributes(patrol_boat2_4) <- list(player = 2, type = "P", coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)

# Here I create a list with all ships owned by player 1 and player 2.

ships <- list(patrol_boat1_1, patrol_boat1_2, patrol_boat1_3, patrol_boat1_4, submarine1_1, submarine1_2, submarine1_3, destroyer1_1, destroyer1_2, carrier1, patrol_boat2_1, patrol_boat2_2, patrol_boat2_3, patrol_boat2_4, submarine2_1, submarine2_2, submarine2_3, destroyer2_1, destroyer2_2, carrier2)
# ships2 <- list(patrol_boat2_1, patrol_boat2_2, patrol_boat2_3, patrol_boat2_4, submarine2_1, submarine2_2, submarine2_3, destroyer2_1, destroyer2_2, carrier2)
# print(ships[[1]]@player)

# This is to assign the class "Battlefield" to the object "battlefield_1" and then display it on the console.
battlefield_1 <- Battlefield()
battlefield_1$draw()

# The part that follows is to ask questions to the players. It still doesn't work so well and it's incomplete, but you can uncomment it and try to run the code to see how it works.

# turn <- 0
# playerNames <- c("WHITE", "BLACK")
# scores <- c(0, 0)
# while (TRUE) {
#   battlefield_1$draw()
#   cat("Current turn: ", (turn + 1), "\n", sep="")
#   # cat("Score: ", playerNames[1], " is ", scores[1], " | ", playerNames[2], " is ", scores[2], "\n", sep="")
#   # cat("Player ", currentPlayer, " moves", sep="")
#   cat("\n", "Your arsenal includes 4 Patrol Boats, 3 Submarines, 2 Destroyers and 1 Carrier.", sep="")
#   cat("\n", "\n", "", sep="")
#   ship_orientation <- readline("ship orientation? ") 
#   if (ship_orientation == "q") {
#     break;
#   }
#   newCol <- readline("ship column? ")
#   if (newCol == "q") {
#     break;
#   }
#   newRow <- readline("ship row? ")
#   if (newRow == "q") {
#     break;
#   }
# }
# 

# attr(patrol_boat1_1, "orientation") <- "V"
#comment1