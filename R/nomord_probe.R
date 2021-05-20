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

#' An S4 class to represent nomord test result
#'
#' @keywords internal
#'
#' @slot input The vectorized input sequence
#' @slot p p-value of the statistic test
#'
setClass("nomord", slots=list(input="vector", statname="character", statvalue="numeric", althyp="character", p="numeric"))

#' nomord class generics method
#'
#' @keywords internal
#'
#' @param object An object
methods::setMethod("show","nomord",
	function(object)
	{
	cat("Association test for a sequence of nominal values\n")
	cat("input: ", object@input, "\n")
	cat(object@statname,"-statistic: ", object@statvalue, "\n")
	cat("alternative hypothesis: ", object@althyp, "\n")
	cat("p-value: ", object@p, "\n")
	}
)

#' nomord_probe
#'
#' Returns the probability p
#' 
#' @importFrom methods new
#'
#' @param seq A vector of categorical variables
#'
#' @param statname The used statistic function ("U" or "T")
#'
#' @return A nomord class S4 object with the following slots: input, statname, statvalue, althyp, p 
#'
#' @export
#'
#' @examples
#' nomord_probe(c("B","A","A","C","C","B","A","A"), 'U')
#'
nomord_probe = function(seq, statname)
{
 seq = unlist(seq)
 if (statname=="U") {statfunc = get_U}
 else if (statname=="T") {statfunc = get_T}
 else
 {
  print('statname required')
  return(-1)
 }
 # calc unnormalized statistic
 stat = statfunc(seq)
 # calc normalized statistic
 if (statname=="U")
 {
  statmin = get_min_stat(seq, "U")
  statmax = max(get_max_stat(seq, "U"), stat)
 }
 else if (statname=="T")
 {
  statmin = get_min_stat(seq, "T")
  statmax = max(get_max_stat(seq, "T"), stat)
 }
 stat_norm = 1-2*((stat-statmin) / (statmax-statmin))
 # perform test
 N_samples = 10000
 randomized_values = get_statistics_for_randomized(seq, statfunc, N_samples)
 p_right = sum(stat>randomized_values)/N_samples # flip side do to randomization
 p_left = sum(stat<randomized_values)/N_samples # flip side do to randomization
 # define alternative hypothesis
 if(p_right<p_left)
 {
  althyp = paste("The",statname,"statistic is larger than expected from a random sequence.")
  test_p = p_right
 }
 else
 {
  althyp = paste("The",statname,"statistic is smaller than expected from a random sequence.")
  test_p = p_left
 }

 result = new("nomord")
 result@input = seq
 result@statname = statname
 result@statvalue = stat_norm
 result@althyp = althyp
 result@p = test_p
 return(result)
}
