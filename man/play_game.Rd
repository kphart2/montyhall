% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{play_game}
\alias{play_game}
\title{Play the Game}
\usage{
play_game()
}
\arguments{
\item{'final.pick.stay'}{\itemize{
\item the contestant stays with their first pick of door

'final.pick.switch' - the contestant switches to the door that is not
their initial pick nor the goat door opened by the host
}}
}
\value{
The function returns 'game.results', a data frame of strategy and outcome.
Strategy is either stay or switch depending of whether or not the
contestant stayed with their initial door. Outcome is either win or lose
depening on if a car or a goat was behind the door.
}
\description{
The 'play_game()' sets up the game and plays it in full. It shows if the
contestant lost or
won based on the strategy chosen.
}
\details{
The game is played in full using this function. The strategy refers to
whether or not the contestant chose to stay with their original choice in
or switch to a new door. It is then determined whether the contestant
ended up with a goat door, meaning they lost,  or car door, meaning they
won, based on their strategy.
}
\examples{
    play_game()

}
