#' get_min_stat
#'
#' An internal function that returns the exact minimum value of a given statistic 
#' for randomized samples of the input vector of categorical variables
#' 
#' @keywords internal
#'
#' @param seq A vector of categorical variables
#'
#' @param statfunc The used statistic function
#'
#' @return Integer
#'
get_min_stat = function(seq, statfunc)
{
	min_value = 0
	if(identical(statfunc,get_sum_of_rank_differences))
	{
		min_value = 0
		unique_values = unique(seq)
		for (h in unique_values)
		{
			nh = sum(seq==h)
			min_value = min_value + (nh^3-nh)/6
		}
		n = length(unique_values)
	}
	return(min_value)
}


#' get_max_stat
#'
#' An internal function that returns the approximate maximum value of a given statistic 
#' for randomized samples of the input vector of categorical variables.
#' It uses the Metropolis–Hastings algorithm for the approximation.
#'
#' @import stats
#' 
#' @keywords internal
#'
#' @param seq A vector of categorical variables
#'
#' @param statfunc The used statistic function
#'
#' @param n The number of randomized samples
#'
#' @return Integer
#'
get_max_stat = function(seq, statfunc, n)
{
	temperature = 0.5
	
	result = statfunc(seq)
	for(i in c(1:n))
	{
		idxs = sample(1:length(seq), 2)
		seq_alt = seq
		seq_alt[idxs[1]] = seq[[idxs[2]]]
		seq_alt[idxs[2]] = seq[[idxs[1]]]
		stat = statfunc(seq)
		stat_alt = statfunc(seq_alt)
		if(stat_alt>stat)
		{
			result = stat_alt
			seq = seq_alt
		}
		else
		{
			p = exp(-(stat-stat_alt)/temperature)
			if(runif(1)<p)
			{
				seq = seq_alt
			}
		}
	}
	return(result)
}
