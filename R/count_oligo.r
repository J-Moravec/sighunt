#' Count oligonucleotide frequency
#'
#' Counts occurence of one or more oligonucleotides in on or more sequences.
#' This is wraper around \code{stringr} function
#' \code{\link[stringr]{str_count}}. Do to its limitations, you cannot search
#' multiple oligos in multiple sequences at once.
#'
#' @param oligo character (or character vector) of oligonucleotide.
#' @param sequence character (or character vector) of DNA (or RNA)
#' sequence.
#'
#' @return vector of occurences
#'
#' @export
count_oligo = function(oligo, sequence){
    stringr::str_count(pattern=stringr::fixed(oligo), string=sequence)
    }
