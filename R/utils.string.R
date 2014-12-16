#' 'Fuzzy' string matching
#' 
#' @param source.string the string for which similarity is to be calculated
#' @param similarityset vector of strings to be matched
#' @export
string.get.most.similar <- function(source.string, similarityset){
  similarityset.different <- setdiff(unique(similarityset), unique(source.string))
  if (length(similarityset.different)>0){
    diffvals <- sort(similarityset.different)
    str.similarity <- utils::adist(diffvals, source.string, ignore.case=TRUE)
    
    suspects.idx <- (str.similarity[,1] == min(str.similarity))
    return(diffvals[suspects.idx])
  }
  c()
}
