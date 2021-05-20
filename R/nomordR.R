#' nomordR: Implementation of a Randomization Test for Sequences of Nominal Values
#'
#' The included \code{nomord_probe} function performs
#' a statistical analysis on an input vector (sequence) of nominal (categorical) values.
#' The vector positions and the corresponding nominal values define ordinal-nominal value pairs,
#' which can be associated. 
#' The function first calculates an association statistic, then
#' performs a randomization test to check for a significant association.
#'
#' @section Usage:
#' The \code{nomord_probe} function has two mandatory arguments; the input sequence as a vector (obligatory) and the
#' chosen statistic ('U' or 'T').
#' The command returns an S4 object with slot names \code{input}, \code{statname}, \code{statvalue}, \code{althyp} and \code{p},
#' standing for the input sequence, the chosen statistic, the (normalized) statistic value, the textual alternative hypothesis,  
#' and the p-value of the statistical test, respectively.
#' Printing the result also displays these values.
#' A statistic ('U' or 'T') value of –1 indicates maximum segregation, while a value of 1 indicates maximum aggregation
#' of the nominal values along the ordinal dimension. 
#'
#' Examples:
#' \preformatted{
#' > v = c("B","A","A","C","C","B","A","A")
#' > nomord_probe(v, "U")
#' Association test for a sequence of nominal values
#' input:  B A A C C B A A
#' U -statistic:  -0.4285714
#' alternative hypothesis:  U statistic is smaller than expected from a random sequence
#' p-value:  0.2723
#' >
#' > result = nomord_probe(v, "U")
#' > result@p
#' [1] 0.2723
#' }
#'
#' @section Computational background:
#' Two association metrics are available.
#' The U statistic expresses the topological segregation of the sequence by counting the 
#' number of other types appearing between every pair of vector elements, denoted by \eqn{w_{ij}}.
#' The unnormalized U' is calculated as \eqn{U' = \sum_{i<j} w_{ij}}, and
#' the normalized U value is calculated as \eqn{U=(U'-U_{min})/(U_{max}-U_{min})},
#' where \eqn{U'_{max}} and \eqn{U'_{min}} are the theoretical largest and smallest U' values
#' for a sequence with the same length and the same nominal value distribution.
#' 
#' The T statistic expresses the subsequence diversity by counting the number of different
#' elements in all possible \eqn{v}-long substrings. The window width \eqn{v}
#' is equal to the number of distinct nominal values. 
#' Denoting the number of different elements in a substring starting at index \eqn{i} as \eqn{d_i},
#' the unnormalized T' is calculated as \eqn{U' = \sum_{i} d_{i}}, and
#' the normalized T value is calculated as \eqn{T=(T'-T_{min})/(T_{max}-T_{min})},
#' where \eqn{T'_{max}} and \eqn{T'_{min}} are the theoretical largest and smallest T' values
#' for a sequence with the same length and the same nominal value distribution.
#'
#' \eqn{U_{min}} is equal to zero. The other extremum values are calculated from algorithmically 
#' generated sequences, with the same length (\emph{L}) and nominal-value occurrences (\eqn{n_c, c = \{1,2, ... , k\}})
#' as the input vector. (The validity of these algoriths was checked with a Metropolis–Hastings algorithm.)
#'
#' ----------
#'
#' \eqn{U_{max}}:
#'
#' 1. Assign \emph{p} priority values to all elements \eqn{(i = \{1,2, ... , n_c\})} of all types \eqn{(c = \{1,2, ... , k\})}
#' according to the rule \eqn{p = i/(n_c + 1)}, where \eqn{k} is the number of different nominal values.
#'
#' 2. Order the elements according to their priorities. Break ties between elements of
#' different types consistently according to any arbitrary ordering of types.
#'
#' ----------
#'
#' \eqn{T_{min}}
#'
#' Create a sequence consisting of homogeneous blocks of the different elements.
#' Place shorter blocks (with smaller \eqn{n_c}) as close to either the left or right ends, as possible.
#'
#' ----------
#'
#' \eqn{T_{max}}
#'
#' 1. Create an indexed list of the different types (e.g. [1, 2, 3, 4]). Initially, the number of
#' available elements for all types \eqn{(m_c)} is equal to their occurrence values \eqn{(n_c)}.
#'
#' 2. Assign elements to the different places of the sequence, starting from the middle and
#' going towards the left and right ends, using an auxiliary variable i ranging from 1 to
#' L (sequence length). For each value of i:
#'
#' (a) The place to assign a value to is; \emph{p = floor(L/2)}, if i is odd,
#' \emph{p = floor(L/2) + i/2}, if i is even.
#'
#' (b) Let the candidate to this place be the type with the index \emph{p mod k}, denoted by c.
#'
#' (c) If \eqn{m_c} is zero, then let c be the type with the highest remaining number \eqn{(m_c)}.
#'
#' (d) Assign c to position \emph{p} of the sequence and decrease \eqn{m_c} with one.
#'
#' This algorithm fills a sequence with elements starting from the middle and going towards the
#' left and right ends. It tries to place different types next to each other, but if it is not possible
#' (there are no more elements of the desired type), then it is replaced by an element of the most
#' abundant type.
#'
#' @docType package
#' @name nomordR
NULL
#> NULL
