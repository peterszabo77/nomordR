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
setClass("nomord", slots=list(input="vector", statname="character", statvalue="numeric", althyp="character", p="numeric"))

#' nomord class generics method
#' @param object An object
methods::setMethod("show","nomord",
	function(object)
	{
	cat('ahoj\n')
	cat(object@statname,"-statistic: ", object@statvalue, "\n")
	cat("alternative hypothesis: ", object@althyp, "\n")
	cat("p-value: ", object@p, "\n")
## cat(object@age, "years old\n")
## cat("GPA:", object@GPA, "\n")
	}
)

#' test_association
#'
#' Returns the probability p
#' 
#' @importFrom methods new
#'
#' @param seq A vector of categorical variables
#'
#' @param statname The used statistic function
#'
#' @return result A nomord class S4 object
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
 # calc unnormalized statistic
 stat = statfunc(seq)
 # calc normalized statistic
 if (statname=="U")
 {
  statmin = get_min_stat(seq, "U")
  statmax = get_max_stat(seq, "U")
 }
 else if (statname=="T")
 {
  statmin = get_min_stat(seq, "T")
  if(length(seq)<=10)
  {
   statmax = get_max_stat(seq, "T", 10000)
  }
  else
  {
   statmax = get_max_stat(seq, "T")
  }
 }
 stat_norm = 1-2*((stat-statmin) / (statmax-statmin))
 # perform test
 N_samples = 10000
 sample_value = statfunc(seq)
 randomized_values = get_statistics_for_randomized(seq, statfunc, N_samples)
 p_right = sum(stat<randomized_values)/N_samples
 p_left = sum(stat>randomized_values)/N_samples
 # define alternative hypothesis
 if(p_right<p_left)
 {
  althyp = paste(statname," statistic is larger than expected from a random sequence")
  test_p = p_right
 }
 else
 {
  althyp = paste(statname," statistic is smaller than expected from a random sequence")
  test_p = p_left
 }

 result = new("nomord")
 result@statname = statname
 result@statvalue = stat_norm
 result@althyp = althyp
 result@p = test_p
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
 return(result)
}
