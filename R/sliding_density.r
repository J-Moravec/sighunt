#' Calculate DIAS using sliding density method.
#'
#' Sliding density scores each segment in signature according to composition
#' of nearby signature.
#'
#' Sliding density uses subset of signature centered around single segment
#' and constructs density out of this subset. From this density, credibility
#' intervals are constructed according to significance level alpha and DIAS
#' score is calculated for single segment around which density is centered.
#'
#' This differs from global density method as DIAS score is calculated only on
#' centered segment, while in global density, DIAS score is calculated for every
#' segment of signature from which density was constructed.
#'
#' Parameter \code{window} does not specify whole size of subset taken, but only
#' left and right part. That means that it takes a window left from central
#' segment and window right. As first segment of signature doesn't have anything
#' on left and the last segment of signature doesn't have anything on right,
#' size of actuall subset range from (window+1) to (2*window + 1)
#'
#' @param signature signature from \code{\link{get_signature}} function
#' @param oligos either numeric or character vector of oligonucleotides
#'    that will be used. If nos specified, whole signature is used.
#' @param window size of subset to the left
#' @param alpha significance level of scoring intervals.
#'
#' @return DIAS score for given signature.
#' @export
# TODO function that given DIAS, original sequence and DIAS treshold,
# TODO produce subset of sequences as individual as multifasta
sliding_density = function(signature, oligos=NULL, window=100,
                           alpha=c(0.05, 0.025, 0.01)
                           ){
    signature = filter_signature(signature, oligos)
    # take subset according to sliding_window, score it, add it to score
    # nonono, it actually look only on X sample.
    score = vector(mode="numeric", length=nrow(signature))
    names(score) = rownames(signature)

    for(i in 1:nrow(signature)){
        from = max(i - window, 1)
        to = min(i + window, nrow(signature))
        pos = i - from + 1

        subset_signature = signature[from:to, ]
        subset_score = DIAS_scores_sum(signature, pos, alpha=alpha)
        score[i] = subset_score
        }
    return(score)
    }
