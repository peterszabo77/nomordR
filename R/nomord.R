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
#' test_association('AABACCB', 'B')
test_association = function(seq, statfunc)
{
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
