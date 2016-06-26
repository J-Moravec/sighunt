#' Calculate DIAS using Eye of storm variant of sliding density method
#'
#' Eye of storm score each segment in signature according to composition of
#' local signature but without segment itself and some closest segments around
#' it.
#'
#' Eye of storm is similar to sliding density, it uses subset of signature
#' centered around single segment, removes mentioned segment and some other
#' negihbouring segments specified by \code{eye} parameter and then produce
#' density from this filtered subset of signature. Segment around which this
#' subset is centered is then scored based on credibility interval constructed
#' from estimated density.
#'
#' Parameter \code{window} does not specify whole size of subset taken, but only
#' left and right part. That means that it takes a window left from central
#' segment and window right. As first segment of signature doesn't have anything
#' on left and the last segment of signature doesn't have anything on right,
#' size of actuall subset range from (window+1) to (2*window + 1)
#'
#' Parameter \code{eye} define number of segments in signature (single number
#' that show frequency of oligonucleotide in part of sequence) that are excluded
#' from density estimation.
#'
#' @param signature signature from \code{\link{get_signature}} function
#' @param oligos either numeric or character vector of oligonucleotides
#'    that will be used. If nos specified, whole signature is used.
#' @param window size of subset to the left
#' @param eye number of segments around centered segment which are excluded
#'    from density estimation.
#' @param alpha significance level of scoring intervals.
#' @oaram bar show progress bar
#'
#' @return DIAS score for given signature.
#' @export
# TODO WARNING for situation where signature/windows is almost as big as eye
# TODO (after filtering and everything) so density cannot be estimated well!
eye_of_storm = function(signature, oligos=NULL, window=100, eye=5,
                           alpha=c(0.05, 0.025, 0.01), bar=TRUE
                           ){
    score = sliding_density_methods(
        signature=signature, oligos=oligos,
        window=window, eye=eye, alpha=alpha,
        bar=bar
        )
    return(score)
    }
