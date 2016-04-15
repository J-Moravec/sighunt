#' Generating oligonucleotides
#'
#' Generate every possible oligonucleotide of certain length.
#'
#' @param length of generated oligonucleotides
#'
#' @return character vector of oligonucleotides
#'
#' @export
generate_oligo = function(length){
    nucleotides = c("A", "C", "G", "T")
    lst_nuc = list(nucleotides)
    combinations = expand.grid(rep(lst_nuc, length))[length:1]
    connected = do.call(paste, c(combinations, list(sep="")))
    return(connected)
    }
