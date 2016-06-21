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
#' size of actuall subset range from (window+1) to (2\times window + 1)
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
#'
#' @return DIAS score for given signature.
#' @export
# TODO a lot of similar code with sliding_density that could be put somewhere
# TODO else and tested
# TODO DIAS functions seem clunky, rewrite them
eye_of_storm = function(signature, oligos=NULL, window=100, eye=5,
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
        from_eye = ifelse(i - eye < 1, 1, i - eye)
        to_eye = ifelse(i + eye > nrow(signature), nrow(signature), i + eye)
        eye_interval = from_eye:to_eye

        subset_signature = signature[from:to, ]
        subset_score = DIAS_subset_eye_scores_sum(signature, pos, eye_interval,
                                                  alpha=alpha)
        score[i] = subset_score
        }
    return(score)
    }
