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
                             battlefield = "matrix",
                             player = "numeric"
                           ),
                           methods=list(
                             initialize = function() {
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
                             }
                           ))

Shots <- setRefClass("Shots",
                     fields=list(
                       battlefield = "matrix",
                       player = "numeric"
                     ),
                     methods=list(
                       initialize = function() {
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
                       }
                     ))

Carrier <- setRefClass("carrier",
                       fields=list(
                         player = "numeric", #1 or 2
                         coordinates = "vector", #x and y
                         orientation = "character", #V or O
                         life = "numeric", #ranges from 1 to 4 - is equal to the lenght
                         isSunk = "logical", #to check if a ship is sunk or not
                       ),
                       methods=list(
                         initialize = function() { 
                         }
                       ))

Destroyer <- setRefClass("destroyer",
                         fields=list(
                           player = "numeric", #1 or 2
                           coordinates = "vector", #x and y
                           orientation = "character", #V or O
                           life = "numeric", #ranges from 1 to 4 - is equal to the lenght
                           isSunk = "logical", #to check if a ship is sunk or not
                         ),
                         methods=list(
                           initialize = function() { 
                           }
                         ))

Submarine <- setRefClass("submarine",
                         fields=list(
                           player = "numeric", #1 or 2
                           coordinates = "vector", #x and y
                           orientation = "character", #V or O
                           life = "numeric", #ranges from 1 to 4 - is equal to the lenght
                           isSunk = "logical", #to check if a ship is sunk or not 
                         ),
                         methods=list(
                           initialize = function() { 
                           }
                         ))

Patrol <- setRefClass("patrol",
                      fields=list(
                        player = "numeric", #1 or 2
                        coordinates = "vector", #x and y
                        orientation = "character", #V or O
                        life = "numeric", #ranges from 1 to 4 - is equal to the lenght
                        isSunk = "logical", #to check if a ship is sunk or not 
                      ),
                      methods=list(
                        initialize = function() { 
                        }
                      ))

# Here I am creating every ship we need (1 carrier, 2 destroyers, 3 submarines and 4 patrol boat for each player, so 20 ships).
carrier1 <- carrier()
carrier2 <- carrier()

destroyer1_1 <- destroyer()
destroyer1_2 <- destroyer()

destroyer2_1 <- destroyer()
destroyer2_2 <- destroyer()

submarine1_1 <- submarine()
submarine1_2 <- submarine()
submarine1_3 <- submarine()

submarine2_1 <- submarine()
submarine2_2 <- submarine()
submarine2_3 <- submarine()

patrol_boat1_1 <- patrol()
patrol_boat1_2 <- patrol()
patrol_boat1_3 <- patrol()
patrol_boat1_4 <- patrol()

patrol_boat2_1 <- patrol()
patrol_boat2_2 <- patrol()
patrol_boat2_3 <- patrol()
patrol_boat2_4 <- patrol()

# Here I give to each ship all the attributes we need. I positionated them in a random-chosen position.

attributes(carrier1) <- list(player = 1, coordinates = c(10, 10), orientation = "O", life = 4, isSunk = FALSE)
attributes(carrier2) <- list(player = 2, coordinates = c(10, 10), orientation = "O", life = 4, isSunk = FALSE)

attributes(destroyer1_1) <- list(player = 1, coordinates = c(10, 10), orientation = "O", life = 3, isSunk = FALSE)
attributes(destroyer1_2) <- list(player = 1, coordinates = c(10, 10), orientation = "O", life = 3, isSunk = FALSE)

attributes(destroyer2_1) <- list(player = 2, coordinates = c(10, 10), orientation = "O", life = 3, isSunk = FALSE)
attributes(destroyer2_2) <- list(player = 2, coordinates = c(10, 10), orientation = "O", life = 3, isSunk = FALSE)

attributes(submarine1_1) <- list(player = 1, coordinates = c(10, 10), orientation = "O", life = 2, isSunk = FALSE)
attributes(submarine1_2) <- list(player = 1, coordinates = c(10, 10), orientation = "O", life = 2, isSunk = FALSE)
attributes(submarine1_3) <- list(player = 1, coordinates = c(10, 10), orientation = "O", life = 2, isSunk = FALSE)

attributes(submarine2_1) <- list(player = 2, coordinates = c(10, 10), orientation = "O", life = 2, isSunk = FALSE)
attributes(submarine2_2) <- list(player = 2, coordinates = c(10, 10), orientation = "O", life = 2, isSunk = FALSE)
attributes(submarine2_3) <- list(player = 2, coordinates = c(10, 10), orientation = "O", life = 2, isSunk = FALSE)

attributes(patrol_boat1_1) <- list(player = 1, coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)
attributes(patrol_boat1_2) <- list(player = 1, coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)
attributes(patrol_boat1_3) <- list(player = 1, coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)
attributes(patrol_boat1_4) <- list(player = 1, coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)

attributes(patrol_boat2_1) <- list(player = 2, coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)
attributes(patrol_boat2_2) <- list(player = 2, coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)
attributes(patrol_boat2_3) <- list(player = 2, coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)
attributes(patrol_boat2_4) <- list(player = 2, coordinates = c(10, 10), orientation = "O", life = 1, isSunk = FALSE)

# Here I create a list with all ships owned by player 1 and player 2.

ships <- list(patrol_boat1_1, patrol_boat1_2, patrol_boat1_3, patrol_boat1_4, submarine1_1, submarine1_2, submarine1_3, destroyer1_1, destroyer1_2, carrier1, patrol_boat2_1, patrol_boat2_2, patrol_boat2_3, patrol_boat2_4, submarine2_1, submarine2_2, submarine2_3, destroyer2_1, destroyer2_2, carrier2)

ship1 <- list(patrol_boat1_1, patrol_boat1_2, patrol_boat1_3, patrol_boat1_4, submarine1_1, submarine1_2, submarine1_3, destroyer1_1, destroyer1_2, carrier1)
ship2 <- list(patrol_boat2_1, patrol_boat2_2, patrol_boat2_3, patrol_boat2_4, submarine2_1, submarine2_2, submarine2_3, destroyer2_1, destroyer2_2, carrier2)

#location phase player 1

battlefield_1 <- Battlefield()
battlefield_1$draw()
shots_1<- Shots()
shots_1$draw()

patrols1 <-4
nsub1 <- 3
ndest1 <- 2
ncarr1 <- 1

patrols2 <-4
nsub2 <- 3
ndest2 <- 2
ncarr2 <- 1


currentPlayer <- "White"

#ask player 1 about the location of each ship

while(TRUE){
  battlefield_1$draw()
  cat("Player ", currentPlayer, " locates Ships", sep="")
  cat("\n", "Your arsenal include ", patrols1, " Patrol Boats ", nsub1, " Submarines ", ndest1, " Destroyers and ", ncarr1,  " Carrier.", sep="")
  cat("\n", "\n", "", sep="")
  
  #ask to locate patrol 1
  
  cat("Locate Patrol 1", sep="")
  cat("\n", "Patrol is of size 1", sep="\n")
  
  ship_orientation <- readline("ship orientation? (H or V)") 
  if (ship_orientation == "q") {
    break;
  }
  
  newCol <- readline("ship column? (from 1 to 10)")
  if (newCol == "q") {
    break;
  }
  
  newRow <- readline("ship row? ")
  if (newRow == "q") {
    break;}
  
  
  destination <- Coordinates(as.integer(newRow), as.integer(newCol))
