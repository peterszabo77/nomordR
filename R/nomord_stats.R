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
