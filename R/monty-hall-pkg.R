#' @title
#'   Create a new Monty Hall Problem game.
#' 
#' @description
#'   create_game() generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#' 
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#' indicating the positions of goats and the car.
#' 
#' @examples
#' create_game()
#' create_game()
#' create_game()
#' 
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
} 



#' @title
#' Selects a door or doors from the doors available.
#' 
#' @description
#' 'select_door' returns a random door or doors (depending
#' on what is specified in the sample function) from the
#' previously determined number of doors.
#' 
#' @details
#' 'select_door' returns a random door from the previously
#' determined number of doors. this is determined by the
#' doors <- c(1, 2, 3). a.pick is the randomization process
#' as it includes the function sample() that uses the
#' previously created "door" as a input and "size = 1" to
#' determine how many doors to return. return() uses
#' "a.pick" as an input to provide the random door number
#' requested as an output.
#' 
#' @param ... This function does not input arguments.
#' 
#' @return ... Returns one numeric vector/output.For example, 1.
#' 
#' @examples 
#' 
#' select_door()
#' select_door()
#' select_door()
#' 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' The host opens a goat door.
#' 
#' @description
#' This function decides which door the host is allowed
#' to open. Meaning that it determines which is the car
#' door and which is the door that the contestant has
#' already opened and then decides the goat door the host
#' ought to open based on this determination.
#' 
#' @details
#' This function (open_goat_door()) represents the host's opening
#' of a door post-contestant opening a door. The open_goat_door()
#' function needs information from the "game" (created  first
#' function) and "a.pick" created in the second function. First,
#' if the contestant originally picked a car, then the available
#' doors that can be chosen from cannot be a car. Thus, randomly
#' sample (using the sample() function) one goat door from the two
#' remaining goat doors and open it (using the opened.door in
#' the return()). If the contestant originally selected a goat,
#' then the opened door cannot be a car and it cannot be the
#' contestant's original pick (a.pick), thus return the only
#' remaining door (using the opened.door in the return()).
#' 
#' @param ... uses argument "game" and "a.pick". "game"
#' represents what is behind the door, per se. a.pick is
#' the original door that was picked by the contestant.
#' the function needs this information for the later logical
#' statements wrapped within the function. game is used to
#' ensure there is no "car" behind the door that the host
#' will open, and a.pick is used to ensure that the pick
#' originally made by the participant will not be chosen
#' to be opened either. game is a character vector and
#' requires character input, whereas a.pick is numeric and
#' requires numeric input.
#' 
#' @return ... Returns one numeric output/vector. For example, 1.
#' 
#' @examples
#' 
#' this.game <- c("goat","car","goat")
#' the.initial.pick <- 1
#' open_goat_door( this.game, the.initial.pick )
#' 
#' the.initial.pick <- 2
#' open_goat_door( this.game, the.initial.pick )
#' 
#' the.initial.pick <- 3
#' open_goat_door( this.game, the.initial.pick )
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Stay at original pick or change the door.
#' 
#' @description
#' This function represents the choice of the participant
#' to stay or to change doors (stay = T means to stay,
#' stay = F means to switch). Staying simply means remaining
#' at the original pick, while switching means not picking
#' the door that was originally chosen and not picking the
#' door that was opened by the host.
#' 
#' @details
#' This function (change_door()) uses the arguments stay = T,
#' opened.door and a.pick. stay = T represents staying, versus
#' stay = F represents switching. opened.door is used to ensure
#' if the participant switches, that the participant does not
#' choose the door already opened by the host. a.pick is used to
#' represent the choice of staying, but also to ensure that if
#' the participant switches, that they don't choose the door
#' they already chose. If a participant stays, then they simply
#' remain with their original pick (a.pick), but if they switch
#' then they cannot choose the door that was opened by the host
#' (goat door, as represented by opened.door) and they can't
#' choose their original pick (a.pick) and thus the if
#' statement will return the only door that meets those two
#' requirements (not opened.door and definitely not a.pick).
#' 
#' @param ... stay=T or stay = TRUE refers to staying at the
#' original pick. stay=F or stay = FALSE refers to switching.
#' opened.door is the previously opened goat door by the host
#' and a.pick is the original pick by the participant.
#' opened.door and a.pick are necessary for the logical
#' statements wrapped in this function to ensure that we aren't
#' choosing the already chosen or opened door from before if
#' the contestant decides to switch. opened.door and a.pick
#' is numeric and requires numeric input. stay needs either
#' TRUE or FALSE input.
#' 
#' @return ... Returns a number(door)/numeric vector.
#' 
#' @examples
#' 
#' change_door( stay=T, opened.door=1, a.pick=3 )
#' change_door( stay=F, opened.door=1, a.pick=3 )
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' The function that determines if the participant
#' won or lost based on what is behind the chosen
#' door.
#' 
#' @description
#' "determine winner()" allows us to see if the
#' contestant won based on whether the final door
#' picked by the contestant has a goat or car.
#' 
#' @details
#' This function uses the arguments of final.pick and
#' game. final.pick represents the final pick of the
#' participant: whether they chose to stay or switch.
#' "Game" provides information about the ordering
#' of the goats and cars. If the final.pick door has
#' the car behind it, then the function will return win.
#' If the final.pick door has a goat behind it, then
#' the function will return lose.
#' 
#' @param ... final.pick is an argument that holds the
#' choice of the participant as to whether or not to
#' stay or switch doors. game is an argument that holds
#' information about what is behind each door (e.g., a
#' car or goat). the game argument tells us what is
#' behind the door of the final.pick argument (which is
#' what the contestant chose) which is then used in the
#' function  determine_winner() to see if the contestant
#' won or lost. final.pick is numeric and requires numeric
#' input whereas game is a character vector that requires
#' input of the same type.
#' 
#' @return ... this will print either "WIN" or "LOSE"
#' depending on the if statements in the function.
#' 
#' @examples
#' 
#' this.game <- c("goat","car","goat")
#' determine_winner( final.pick=1, game=this.game )
#' determine_winner( final.pick=2, game=this.game )
#' determine_winner( final.pick=3, game=this.game )
#' 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Creates a data frame of the outcome of one game if
#' a contestant stays or if they switch.
#' 
#' @description
#' Condenses the all functions and wraps them in the
#' play_game function. It produces the outcome winning
#' for both staying and switching based on prior information
#' from created functions, namely, create_game(),
#' select_door(), opened_goat_door(), etc. which are stored in
#' new_game, first.pick, and opened.door, respectively.
#' 
#' @details
#' play_game does not need any arguments inside the
#' function as all of the necessary pieces of information
#' are provided later. create_game(), select_door(), and
#' open_goat_door() are assigned to an object, namely,
#' new_game, first.pick, and opened.door, respectively.
#' Next are the strategies, which is either to stay or
#' switch: final.pick.stay and final.pick.switch.
#' This is represented by change_door, stay=TRUE
#' and stay=FALSE, respectively. This function needs
#' information from opened.door and first.pick. The
#' determine_winner() function is then stored in either
#' outcome.stay or outcome.switch, corresponding to the
#' aforementioned stored objects of final.pick.stay and
#' final.pick.switch. determine_winner requires information
#' from new_game() in order to determine what is behind the
#' doors and know whether or not the contestant won. The
#' final objects in the function create the data frame.
#' "strategy" represents one of the columns in the data frame,
#' which will be either stay or switch. outcome represents the
#' other column, which will be either outcome.stay or
#' outcome.switch. Finally, game.results stores the strategy
#' and outcome objects stored prior, and transforms them into
#' a data frame using data.frame(), ensuring that it does not
#' become a factor (through stringsAsFactors=F).
#' return() returns the data frame.
#' 
#' @param ... No arguments are used.
#' 
#' @return ... A data frame that details the outcomes of
#' each strategy (stay/switch).
#' 
#' @examples
#' play_game()
#' 
#' premeditated.game <- c("goat","car","goat")
#' contestant_first_pick <- 1
#' play_game <- function( )
#' {
#'  new.game <- premeditated.game 
#'  first.pick <- contestant_first_pick
#'  opened.door <- open_goat_door( new.game, first.pick )
#'  final.pick.stay <- change_door( stay=T, 
#'  opened.door = opened.door, 
#'  a.pick = first.pick )
#'  final.pick.switch <- change_door( stay=F, 
#'  opened.door = opened.door, 
#'  a.pick = first.pick )
#' outcome.stay <- determine_winner( final.pick.stay, new.game  )
#' outcome.switch <- determine_winner( final.pick.switch, new.game )
#' strategy <- c("stay","switch")
#' outcome <- c(outcome.stay,outcome.switch)
#' game.results <- data.frame( strategy, outcome,
#'                              stringsAsFactors=F )
#'  return( game.results )
#' }
#' play_game()
#' premeditated.game <- c("goat","car","goat")
#' contestant_first_pick <- 2
#' play_game <- function( )
#' {
#'  new.game <- premeditated.game 
#'  first.pick <- contestant_first_pick
#'  opened.door <- open_goat_door( new.game, first.pick )
#'  final.pick.stay <- change_door( stay=T, 
#'  opened.door = opened.door, 
#'  a.pick = first.pick )
#'  final.pick.switch <- change_door( stay=F, 
#'  opened.door = opened.door, 
#'  a.pick = first.pick )
#' outcome.stay <- determine_winner( final.pick.stay, new.game  )
#' outcome.switch <- determine_winner( final.pick.switch, new.game )
#' strategy <- c("stay","switch")
#' outcome <- c(outcome.stay,outcome.switch)
#' game.results <- data.frame( strategy, outcome,
#'                              stringsAsFactors=F )
#'  return( game.results )
#' }
#' play_game()
#' premeditated.game <- c("goat","car","goat")
#' contestant_first_pick <- 3
#' play_game <- function( )
#' {
#'  new.game <- premeditated.game 
#'  first.pick <- contestant_first_pick
#'  opened.door <- open_goat_door( new.game, first.pick )
#' final.pick.stay <- change_door( stay=T, 
#' opened.door = opened.door, 
#' a.pick = first.pick)
#' final.pick.switch <- change_door( stay=F, 
#' opened.door = opened.door, 
#' a.pick = first.pick )
#' outcome.stay <- determine_winner( final.pick.stay, new.game  )
#' outcome.switch <- determine_winner( final.pick.switch, new.game )
#' strategy <- c("stay","switch")
#' outcome <- c(outcome.stay,outcome.switch)
#' game.results <- data.frame( strategy, outcome,
#'                             stringsAsFactors=F )
#' return( game.results )
#' }
#' play_game()
#'
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door = opened.door, 
                                  a.pick = first.pick )
  final.pick.switch <- change_door( stay=F, opened.door = opened.door, 
                                    a.pick = first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Repeatedly run monty hall games for a specified
#' number of times.
#' 
#' @description
#' play_n_games runs the monty hall games for a given
#' number of times and then then produces an table
#' displaying the proportion of the time that each
#' result would occur given the strategy taken (stay
#' or switch).
#' 
#' @details
#' play_n_games requires the specification of how many
#' times one wants to run the function, provided by n=100
#' dplyr is then called. the function list() is stored
#' in results.list in order to create a tally of the
#' outcomes. 1 is then stored in the in loop.count
#' for( i in 1:n ) specifies the number of times the
#' games will be run. game.outcome stores the play_game()
#' function that holds all of the necessary components
#' of the game like create_game(), select_door(),
#' open_goat_door() and more. game.outcome is then stored
#' in results.listloop.count which applies the results
#' of the game to the list. loop.count + 1 is then assigned
#' to the object loop.count, binding to it the number of
#' loops.  dplyr::bind_rows( results.list ) is assigned
#' to results.df which works to organize the list into rows.
#' Then, table(results.df) is called to create the table
#' and piped into prop.table(margin = 1) to provide the
#' probabilities of winning and losing for each strategy,
#' rounding to two decimal places. print() prints this table
#' and return() provides the table. dplyr used for piping.
#' 
#' @param ... n=100 or whatever desired number is the
#' argument in this function. It's used to specify the
#' number of times you want to repeatedly run the
#' function and then runs it. numeric and requires numeric
#' input.
#' 
#' @return ... A table of the probability of winning
#' or losing according to strategy (switching or staying).
#' 
#' @examples
#' 
#' my.premeditated.game <- c("car","goat","goat")
#' my_contestant_first_pick <- 1
#' my_play_game <- function( )
#' {
#'  new.game <- my.premeditated.game
#'  first.pick <- my_contestant_first_pick
#'  opened.door <- open_goat_door( new.game, first.pick )
#'  final.pick.stay <- change_door( stay=T, 
#'  opened.door = opened.door, 
#'  a.pick = first.pick )
#' final.pick.switch <- change_door( stay=F, 
#' opened.door = opened.door, 
#' a.pick = first.pick )
#' outcome.stay <- determine_winner( final.pick.stay, new.game  )
#' outcome.switch <- determine_winner( final.pick.switch, new.game )
#' strategy <- c("stay","switch")
#' outcome <- c(outcome.stay,outcome.switch)
#' game.results <- data.frame( strategy, outcome,
#'                             stringsAsFactors=F )
#' return( game.results )
#' }
#' play_n_games <- function( n=100 )
#' {
#'  library( dplyr )
#' results.list <- list()   # collector
#' loop.count <- 1
#' for( i in 1:n )  # iterator
#' {
#' game.outcome <- my_play_game()
#' results.list[[ loop.count ]] <- game.outcome 
#' loop.count <- loop.count + 1
#' }
#' results.df <- dplyr::bind_rows( results.list )
#' table( results.df ) %>% 
#' prop.table( margin=1 ) %>%  # row proportions
#' round( 2 ) %>% 
#' print()
#' return( results.df )
#' }
#' play_n_games(n=100)
#' my.premeditated.game <- c("car","goat","goat")
#' my_contestant_first_pick <- 2
#' my_play_game <- function( )
#' {
#' new.game <- my.premeditated.game
#' first.pick <- my_contestant_first_pick
#' opened.door <- open_goat_door( new.game, first.pick )
#' final.pick.stay <- change_door( stay=T, 
#' opened.door = opened.door, 
#' a.pick = first.pick )
#' final.pick.switch <- change_door( stay=F, 
#' opened.door = opened.door, 
#' a.pick = first.pick )
#' outcome.stay <- determine_winner( final.pick.stay, new.game  )
#' outcome.switch <- determine_winner( final.pick.switch, new.game )
#' strategy <- c("stay","switch")
#' outcome <- c(outcome.stay,outcome.switch)
#' game.results <- data.frame( strategy, outcome,
#'                              stringsAsFactors=F )
#' return( game.results )
#' }
#' play_n_games <- function( n=100 )
#' {
#' library( dplyr )
#' results.list <- list()   # collector
#' loop.count <- 1
#' for( i in 1:n )  # iterator
#' {
#' game.outcome <- my_play_game()
#' results.list[[ loop.count ]] <- game.outcome 
#' loop.count <- loop.count + 1
#' }
#' results.df <- dplyr::bind_rows( results.list )
#' table( results.df ) %>% 
#' prop.table( margin=1 ) %>%  # row proportions
#' round( 2 ) %>% 
#' print()
#' return( results.df )
#' }
#' play_n_games(n=100)
#' my.premeditated.game <- c("car","goat","goat")
#' my_contestant_first_pick <- 3
#' my_play_game <- function( )
#' {
#' new.game <- my.premeditated.game
#' first.pick <- my_contestant_first_pick
#' opened.door <- open_goat_door( new.game, first.pick )
#' final.pick.stay <- change_door( stay=T, 
#' opened.door = opened.door, 
#' a.pick = first.pick )
#' final.pick.switch <- change_door( stay=F, 
#' opened.door = opened.door, 
#' a.pick = first.pick )
#' outcome.stay <- determine_winner( final.pick.stay, new.game  )
#' outcome.switch <- determine_winner( final.pick.switch, new.game )
#' strategy <- c("stay","switch")
#' outcome <- c(outcome.stay,outcome.switch)
#' game.results <- data.frame( strategy, outcome,
#'                               stringsAsFactors=F )
#' return( game.results )
#' }
#' play_n_games <- function( n=100 )
#' {
#' library( dplyr )
#' results.list <- list()   # collector
#' loop.count <- 1
#' for( i in 1:n )  # iterator
#' {
#' game.outcome <- my_play_game()
#' results.list[[ loop.count ]] <- game.outcome 
#' loop.count <- loop.count + 1
#' }
#' results.df <- dplyr::bind_rows( results.list )
#' table( results.df ) %>% 
#' prop.table( margin=1 ) %>%  # row proportions
#' round( 2 ) %>% 
#' print()
#' return( results.df )
#' }
#' play_n_games(n=100)
#' 
#' my.premeditated.game <- c("car","goat","goat")
#' my_play_game <- function( )
#' {
#' new.game <- my.premeditated.game
#' first.pick <- select_door()
#' opened.door <- open_goat_door( new.game, first.pick )
#' final.pick.stay <- change_door( stay=T, 
#' opened.door = opened.door, 
#'                               a.pick = first.pick )
#' final.pick.switch <- change_door( stay=F, 
#' opened.door = opened.door, 
#'                                a.pick = first.pick )
#' outcome.stay <- determine_winner( final.pick.stay, new.game  )
#' outcome.switch <- determine_winner( final.pick.switch, new.game )
#' strategy <- c("stay","switch")
#' outcome <- c(outcome.stay,outcome.switch)
#' game.results <- data.frame( strategy, outcome,
#'                          stringsAsFactors=F )
#' return( game.results )
#' }
#' play_n_games <- function( n=100 )
#' {
#' library( dplyr )
#' results.list <- list()   # collector
#' loop.count <- 1
#' for( i in 1:n )  # iterator
#' {
#' game.outcome <- my_play_game()
#' results.list[[ loop.count ]] <- game.outcome 
#' loop.count <- loop.count + 1
#' }
#' results.df <- dplyr::bind_rows( results.list )
#' table( results.df ) %>% 
#' prop.table( margin=1 ) %>%  # row proportions
#' round( 2 ) %>% 
#' print()
#' return( results.df )
#' }
#' play_n_games(n=100)
#' my_contestant_first_pick <- 3
#' my_play_game <- function( )
#' {
#' new.game <- create_game()
#' first.pick <- my_contestant_first_pick
#' opened.door <- open_goat_door( new.game, first.pick )
#' final.pick.stay <- change_door( stay=T, 
#' opened.door = opened.door, 
#'                           a.pick = first.pick )
#' final.pick.switch <- change_door( stay=F, 
#' opened.door = opened.door, 
#'                         a.pick = first.pick )
#' outcome.stay <- determine_winner( final.pick.stay, new.game  )
#' outcome.switch <- determine_winner( final.pick.switch, new.game )
#' strategy <- c("stay","switch")
#' outcome <- c(outcome.stay,outcome.switch)
#' game.results <- data.frame( strategy, outcome,
#'                               stringsAsFactors=F )
#'                               return( game.results )
#' }
#' play_n_games <- function( n=100 )
#' {
#' library( dplyr )
#' results.list <- list()   # collector
#' loop.count <- 1
#' for( i in 1:n )  # iterator
#' {
#' game.outcome <- my_play_game()
#' results.list[[ loop.count ]] <- game.outcome 
#' loop.count <- loop.count + 1
#' }
#' results.df <- dplyr::bind_rows( results.list )
#' table( results.df ) %>% 
#' prop.table( margin=1 ) %>%  # row proportions
#' round( 2 ) %>% 
#' print()
#' return( results.df )
#' }
#' play_n_games(n=100)
#' play_n_games(n=100)
#' play_n_games(n=100)
#' play_n_games(n=100)
#'
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
