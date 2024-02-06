#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
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
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'    Randomly select a door from 1-3 in the game.
#'
#' @description
#'    The 'select_door()' function randomly selects a number between 1 and 3,
#'    which correspond to numbers on the doors in the game.
#'
#' @details
#'    The contestant must randomly
#'    select a door in order to begin playing the game. One door has a
#'    car behind it, while two doors have goats. The 'select_door' function
#'     serves to allow the "contestant" to choose one
#'    of the three doors created by the 'create_game' function.
#'
#' @param
#'    'a.pick' - one door numbered either 1, 2, or 3, which is the contestant's
#'    chosen door
#'    'doors' - the numbers 1, 2, 3 that correspond to the the three doors in
#'    the game
#'
#' @return
#'    The function returns 'a.pick', which is a number between 1 to 3 that
#'    represents the door the contestant has chosen.
#'
#' @examples
#'     select_door()
#'
#' @export
#'
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'    Host opens a door with a goat behind it
#'
#' @description
#'    The 'open_goat_door()' function generates a number between one and three
#'    that corresponds to the door the host opens, which cannot be the door the
#'    contestant previously chose nor the door with the car behind it.
#'
#' @details
#'    After the contestant chooses a door, the host continues the game by
#'    eliminating one of the other doors. The host does this by choosing and
#'    opening one of the two doors left that were not selected by the
#'    contestant.The host also cannot choose the door with the car behind it as
#'    this would spoil the game. Thus, the host, who has insider information on
#'    which doors have goats and which has the car, opens an unselected door
#'    with a goat behind it in order to keep the contestant stumped and the
#'    game going.
#'
#' @param
#'    'doors' - the numbers 1, 2, 3 that correspond to the the three doors in
#'    the game
#'    'goat.doors' - a door that has a goat behind it, aka does not have a car
#'    behind it
#'    'a.pick' - one door numbered either 1, 2, or 3, which is the contestant's
#'    previously selected door
#'    'opened.door' - the door that the host opens that is not the door the
#'    contestant selected nor has a car behind it
#'
#' @return
#'   The function returns 'opened.door', which is a number from 1 to 3 that
#'   corresponds to the door the host opens, which must be a door that has a
#'   goat behind it but is not the door the contest previously chose
#'
#' @examples
#'     open_goat_door()
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
#'    Contestant stays or switches
#'
#' @description
#'    The 'change_door()' function generates a number from 1 to 3 that
#'    determines if the contestant stays with the original door or switches to
#'    a different door.
#'
#' @details
#'    After the host opens a goat door, the contestant is given the choice to
#'    stay with their original pick of door or to switch to the other
#'    unopened door. This decision determines which door is the contestant's
#'    final pick.
#'
#' @param
#'    'doors' - the numbers 1, 2, 3 that correspond to the the three doors in
#'    the game
#'    'a.pick' - one door numbered either 1, 2, or 3, which is the contestant's
#'    initially selected door
#'    'stay' - the contestant's initial pick is their final pick
#'    'switch' - the contestant's final pick is the door that they did  not
#'    initially pick that the host has not opened
#'
#'
#' @return
#'    The function returns 'final.pick', which is a number between 1 and 3 that
#'    corresponds to the door the contestant ended up with at the end of the
#'    game depening on if they chose to stay or to switch.
#'
#' @examples
#'     change_door()
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
#' ``Determine if contestant has won
#'
#' @description
#'    The function 'determine_winner()' returns 'WIN' if the contestant's door
#'    is the door with the car behind it or 'LOSE' if it was a door with a goat
#'   behind it.
#'
#' @details
#'    After deciding whether to stay or switch, the contestant's final choice
#'    of door must be opened. If the host opens the contestant's door and the
#'    car is behind it, they win. If the host opens the contestant's door and
#'    a goat is behind it, they lose.
#'
#' @param
#'    'final.pick' - a number between 1 and 3 that
#'    corresponds to the door the contestant ended up with at the end
#'    'game' - the game in there are 3 doors consisting of 2 goats and 1 car
#'    "WIN" - occurs when the contestant's final pick has a car behind it
#'    "LOSE" - occurs when the contestant's final pick has a goat behind it
#'
#' @return
#'    The function returns the word "WIN" if final.pick equals car or the word
#'    "LOSE" of final.pick equals goat.
#'
#' @examples
#'   determine_winner()
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
#'    Play the Game
#'
#' @description
#'    The 'play_game()' sets up the game and plays it in full. It shows if the
#'     contestant lost or
#'    won based on the strategy chosen.
#'
#' @details
#'    The game is played in full using this function. The strategy refers to
#'    whether or not the contestant chose to stay with their original choice in
#'    or switch to a new door. It is then determined whether the contestant
#'    ended up with a goat door, meaning they lost,  or car door, meaning they
#'    won, based on their strategy.
#'
#' @param
#'    'final.pick.stay' - the contestant stays with their first pick of door
#'
#'    'final.pick.switch' - the contestant switches to the door that is not
#'    their initial pick nor the goat door opened by the host
#'
#' @return
#'    The function returns 'game.results', a data frame of strategy and outcome.
#'    Strategy is either stay or switch depending of whether or not the
#'    contestant stayed with their initial door. Outcome is either win or lose
#'    depening on if a car or a goat was behind the door.
#'
#' @examples
#'     play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'    Play the game 100 times
#'
#' @description
#'   'play_n_game()' allows the game to be played many times, 100 in this case
#'
#' @details
#'    The n can be changed to whatever number a user wants. The game can be
#'    played many times. An important purpose of this is to determine the
#'    probability of a win versus a lost based on switching or staying, so
#'    that it can be determined which strategy is more likely to have a good
#'    outcome.
#'
#' @param
#'    'n' - can be set to equal the number of times the game is going to be
#'    played
#'
#' @return
#'    'results.df' is returned and shows a table with proportions of wins and
#'    loses based on strategy.
#'
#' @examples
#'     play_n_games()
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
