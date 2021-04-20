#' get_U
#'
#' An internal function that returns the U statistic of
#' the input vector of categorical variables.
#' 
#' @keywords internal
#'
#' @param seq A vector of categorical variables
#'
#' @return The integer-valued statistic
#'
get_U = function(seq)
{
	result = 0
	if (length(seq)==1) return(0)
	if (length(seq)==0) return(-1)
	for (left_idx in c(1:(length(seq)-2)))
	{
		for (right_idx in c((left_idx+2):length(seq)))
		{
			if(seq[left_idx]!=seq[right_idx]) {next}
			inter_idxs = c(left_idx:right_idx)
			spoiled = length(unique(seq[inter_idxs]))-1
			result = result + spoiled
		}
	}
	return(result)
}

#' get_T
#'
#' An internal function that returns the T statistic of
#' the input vector of categorical variables.
#' 
#' @keywords internal
#'
#' @param seq A vector of categorical variables
#'
#' @return The integer-valued statistic
#'
get_T = function(seq)
{
	L = length(seq)
	k = length(unique(seq))
	result = 0
	for (i in c(1:(L-k+1)))
	{
		sub_seq = seq[i:(i+k-1)]
		result = result+length(unique(sub_seq))
	}
	return(result)
}








#' get_sum_of_rank_differences
#'
#' An internal function that returns the sum of ranks statistic of
#' the input vector of categorical variables.
#' 
#' @keywords internal
#'
#' @param seq A vector of categorical variables
#'
#' @return The integer-valued statistic
#'
get_sum_of_rank_differences = function(seq)
{
	result = 0
	if (length(seq)==1) return(0)
	if (length(seq)==0) return(-1)
	for (left_idx in c(1:length(seq)))
	{
		for (right_idx in c(left_idx:length(seq)))
		{
			if (seq[left_idx] == seq[right_idx])
			{
				result = result + abs(left_idx-right_idx)
			}
		}
	}
	return(result)
}


#' get_qualitative_spoiledness
#'
#' An internal function that returns the qualitative spiledness statistic of
#' the input vector of categorical variables.
#' 
#' @keywords internal
#'
#' @param seq A vector of categorical variables
#'
#' @return The integer-valued statistic
#'
get_qualitative_spoiledness = function(seq)
{
	result = 0
	if (length(seq)==1) return(0)
	if (length(seq)==0) return(-1)
	for (left_idx in c(1:(length(seq)-2)))
	{
		for (right_idx in c((left_idx+2):length(seq)))
		{
			if(seq[left_idx]!=seq[right_idx]) {next}
			inter_idxs = c(left_idx:right_idx)
			spoiled = length(unique(seq[inter_idxs]))-1
			if(spoiled>0)
			{
				result = result + 1
			}
		}
	}
	return(result)
}


#' get_quantitative_spoiledness
#'
#' An internal function that returns the quantitative spiledness statistic of
#' the input vector of categorical variables.
#' 
#' @keywords internal
#'
#' @param seq A vector of categorical variables
#'
#' @return The integer-valued statistic
#'
get_quantitative_spoiledness = function(seq)
{
	result = 0
	if (length(seq)==1) return(0)
	if (length(seq)==0) return(-1)
	for (left_idx in c(1:(length(seq)-2)))
	{
		for (right_idx in c((left_idx+2):length(seq)))
		{
			if(seq[left_idx]!=seq[right_idx]) {next}
			inter_idxs = c(left_idx:right_idx)
			spoiled = length(unique(seq[inter_idxs]))-1
			result = result + spoiled
		}
	}
	return(result)
}
