
#'Converts a data set into a series of trajectories
#'
#'This function converts data stored in long format into an object of class \code{trajectory}, which can then be plotted.
#'
#'@aliases as.trajectory
#'@param data A data frame containing variables a subject identifier, an $x$ variable and a $y$ variable.
#'@param names A character vector with three elements indicating the names in \code{data} that correspond to \code{subjid}), \code{x} and \code{y}. The default is \code{names = c("subjid", "x", "y")}. 
#'@param ids A vector of subject identifiers used to select a subset of rows in \code{data}. If not specified, all rows are selected.
#'@return An object of class \code{trajectory}
#'@author Stef van Buuren 2016
#'@examples 
#'# impute the nhanes dataset
#'traj <- as(smocc_hgtwgt, names = ("subjid", "age", "haz"), ids = c(10001, 10003))
#'is.trajectory(traj)
#'@keywords trajectory
setClass("trajectory", representation(a = "character"))
setAs(from = "data.frame", to = "trajectory", function(from, to) {
  new(to)
})
