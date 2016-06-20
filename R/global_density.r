#' Calculate DIAS using global density method
#'
#' Function calculate DIAS score from signature using global density method.
#'
#' First, signature is filtered and \code{quality} is removed. If \code{oligos}
#' were specified, only specified oligos (either columns, if \code{oligos} was
#' numeric vector, or specific oligonucleotides, if \code{oligos} was character
#' vector) are kept. Then global density for all retained oligonucleotide is
#' calculated and each window of signature is scored if it falls to confidence
#' interval defined by alpha. Then all DIAS from all oligonucleotides are
#' summed and final DIAS is returned.
#'
#' @param signature signature from \code{\link{get_signature}} function
#' @param oligos either numeric vector or character vector of oligonucleotides
#'    that will be used. If not specified, whole signature is used.
#' @param alpha significance level of scoring intervals.
#'
#' @return DIAS score for given signature.
#' @export
global_density = function(signature, oligos=NULL, alpha=c(0.05, 0.025, 0.01)){
    signature = filter_signature(signature, oligos)
    score = DIAS_scores_sum(signature, alpha=alpha)
    return(score)
    }
