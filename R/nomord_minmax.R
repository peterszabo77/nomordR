#' get_min_stat
#'
#' An internal function that returns the exact minimum value of a given statistic 
#' for randomized samples of an input vector of categorical variables
#' 
#' @keywords internal
#'
#' @param seq A vector of categorical values
#'
#' @param statname The name of the statistic
#'
#' @return Integer
#'
get_min_stat = function(seq, statname)
{
	if(statname == "U")
	{
		return(0)
	}
	if(statname == "T")
	{
		t = table(seq)
		values = names(t)
		occurrences = as.vector(t)
		names(occurrences) = values
		occurrences = sort(occurrences)
		sorted_values = names(occurrences)
		
		L = length(seq)
		minseq = rep("*",L)
		l_c = 0
		r_c = 0
		for(i in 1:length(sorted_values))
		{
			v = sorted_values[i]
			occ = occurrences[v]
			if(l_c<=r_c)
			{
				minseq[(l_c+1):(l_c+occ)] = v
				l_c = l_c + occ
			}
			else
			{
				minseq[(L-r_c-occ+1):(L-r_c)] = v
				r_c = r_c + occ
			}
		}
		return(get_T(minseq))
	}
}

#' get_max_stat
#'
#' An internal function that returns the approximate maximum value of a given statistic 
#' for randomized samples of an input vector of categorical variables
#' using heuristic rules
#'
#' @import stats
#' 
#' @keywords internal
#'
#' @param seq A vector of categorical values
#'
#' @param statfunc The used statistic formula
#'
#' @return Integer
#'
get_max_stat = function(seq, statname)
{
	if(statname=="U")
	{
		result = get_max_stat_heur_U(seq)
	}
	else if (statname=="T")
	{
		result = get_max_stat_heur_T(seq)
	}
	
	return(result)
}

#' get_max_stat_heur_U
#'
#' An internal function that returns the approximate maximum value of the U statistic 
#' for randomized samples of an input vector of categorical variables
#' using a heuristic rule.
#'
#' @import stats
#' 
#' @keywords internal
#'
#' @param seq A vector of categorical values
#'
#' @return Integer
#'
get_max_stat_heur_U = function(seq)
{
	df = as.data.frame(table(seq), stringsAsFactors=FALSE)
	colnames(df) = c("x", "n")
	df = df[order(df$n, decreasing=TRUE),]
	df$priority = c(1:nrow(df))
	
	L = length(seq)
	k = nrow(df)
	x_max = df$x[1]
	n_max = df$n[1]
	
	root_seq = rep(x_max, n_max)
	x = c()
	for (i in c(1:k))
	{
		x = c(x, rep(df$x[i], df$n[i]))
	}
	rank = rep(0,L)
	df_e = data.frame(x,rank)
	df_e$priority = 0

	for (i in c(1:k))
	{
		x = df$x[i]
		n = df$n[i]
		priority = df$priority[i]
		if(i==1)
		{
			row_bools = df_e$x==x
			df_e$priority[row_bools] = priority
			d_pos = 1/(n+1)
			df_e$rank[row_bools] = seq(from=d_pos, by=d_pos, length.out=n)
		}
		else
		{
			row_bools = df_e$x==x
			df_e$priority[row_bools] = priority
			d_pos = 1/(n+1)
			df_e$rank[row_bools] = seq(from=d_pos, by=d_pos, length.out=n)
		}
	}

	df_e = df_e[order(df_e$rank,df_e$priority),]

	max_seq = as.vector(df_e$x)
	return(get_U(max_seq))
}

#' get_max_stat_heur_T
#'
#' An internal function that returns the approximate maximum value of the T statistic 
#' for randomized samples of an input vector of categorical variables
#' using a heuristic rule.
#'
#' @import stats
#' 
#' @keywords internal
#'
#' @param seq A vector of categorical values
#'
#' @return Integer
#'
get_max_stat_heur_T = function(seq)
{
	L = length(seq)
	cont_table = table(seq)
	type_names = dimnames(cont_table)[[1]]
	n_values = as.vector(cont_table)
	names(n_values) = type_names
	n_values = sort(n_values)
	k = length(n_values)
	max_seq = vector(mode="character", length=L)
	center = ceiling(L/2)
	for(i in 1:L)
	{
		if(((i-1) %% 2) == 0) # odd
		{
			pos = as.integer(center - (i-1)/2)
		}
		else
		{
			pos = as.integer(center + i/2)
		}
		ce = type_names[(pos %% k) + 1]
		if (n_values[ce] == 0)
		{
			ce = names(sort(n_values, decreasing=TRUE))[1]
		}
		n_values[ce] = n_values[ce] - 1
		n_values = sort(n_values)
		max_seq[pos] = ce
	}
	return(get_T(max_seq))
}
