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
