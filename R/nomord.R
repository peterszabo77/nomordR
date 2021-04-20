#' get_statistics_for_randomized
#'
#' An internal function that returns the list of statistics for 
#' n number of randomized versions of the input vector of categorical variables
#' 
#' @keywords internal
#'
#' @param seq A vector of categorical variables
#'
#' @param statfunc The used statistic function
#'
#' @param n The number of randomized samples
#'
#' @return Vector of integers
#'
get_statistics_for_randomized = function(seq, statfunc, n)
{
	result = c()
	for(i in c(1:n))
	{
		result = c(result, statfunc(sample(seq)))
	}
	return(result)
}

#' An S4 class to represent numord test result
#'
#' @slot input The vectorized input sequence
#' @slot p p-value of the statistic test
#'
setClass("nomord", slots=list(input="vector", p="numeric"))

#' nomord class generics method
#' @param object An object
methods::setMethod("show","nomord",
	function(object)
	{
	cat('ahoj')
## cat(object@name, "\n")
## cat(object@age, "years old\n")
## cat("GPA:", object@GPA, "\n")
	}
)

#' test_association
#'
#' Returns the probability p
#' 
#' @param seq A vector of categorical variables
#'
#' @param statfunc The used statistic function
#'
#' @return p probability value
#'
#' @export
#'
#' @examples
#' test_association(c(2,3,1,2,2,3,3,2), 'A')
#'
test_association = function(seq, statname="U")
{
 if (statname=="U") {statfunc = get_U}
 else if (statname=="T") {statfunc = get_T}
 else
 {
  print('invalid method')
  return(-1)
 }
 N_samples = 10000
 sample_value = statfunc(seq)
 randomized_values = get_statistics_for_randomized(seq, statfunc, N_samples)
 p = sum(sample_value>randomized_values)/N_samples
# if(alterhyp=='smaller')
# {
#	p = sum(sample_value>randomized_values)/N_samples
# }
# if(alterhyp=='larger')
# {
#	p = sum(sample_value<randomized_values)/N_samples
# }
# if(alterhyp=='equal')
# {
#	p = 1-sum(sample_value<randomized_values)/N_samples - sum(sample_value>randomized_values)/N_samples
# }
 return(p)
}
