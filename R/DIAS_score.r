#- Produce DIAS score out of signature
#-
#- Functions produce DIAS scoring of single signature (signature for single
#- oligonucleotide) or whole signature matrix. Scoring is based on how extreme
#- is individual segment from which oligonucleotide frequency was computed.
#-
#- Measure of extremity is confidence interval. By default, three confidence
#- are used: 95\%, 97.5\% and 99\%, these are specified by significance levels.
#-
#- Function \code{DIAS_score} is the underlying scoring function. Other
#- functions provide additional interface. Function \code{DIAS_scores} applies
#- \code{DIAS_score} on whole signature matrix and function
#- \code{DIAS_signature_sum} produce single score based on all oligonucleotides.
#-
#- @param signature single column from signature table from
#-    \code{\link{get_signature}}
#- @param signatures signature table from \code{\link{get_signature}}
#- @param alpha vector of significance levels for scoring
#-
#- @return DIAS score, either single vector for single oligonucleotide, list
#- of vectors, total score for all oligonucleotides or matrix of all scores.
DIAS_score = function(signature, alpha=c(0.05, 0.025, 0.01)){
    if(length(dim(signature)) > 1){
        stop("ERROR: signature must be vector!")
        }

    # get density
    density = stats::density(signature, from=0)

    # get credibility interval
    credibility_intervals = get_credibility_interval(density, alpha=alpha)

    score = vector(mode="numeric", length=length(signature))
    names(score) = names(signature)

    for(single_credibility_interval in credibility_intervals){
        # score vector of data based on single credibility interval
        score = score + ifelse(
            signature < single_credibility_interval[1] |
             signature > single_credibility_interval[2], 1, 0)
        }

    return(score)
    }

#- @usage NULL
#- @rdname DIAS_score
DIAS_scores = function(signatures, alpha=c(0.05, 0.025, 0.01)){
    scores = apply(signatures, 2, DIAS_score, alpha=alpha)
    #names(scores) = colnames(signatures)
    return(scores)
    }

#- @usage NULL
#- @rdname DIAS_score
DIAS_scores_sum = function(signatures, alpha=c(0.05, 0.025, 0.01)){
    scores = DIAS_scores(signatures, alpha)
    score = rowSums(scores)
    return(score)
    }

# Redundant as apply in DIAS_scores is already returning matrix
# @usage NULL
# @rdname DIAS_score
#DIAS_scores_mat = function(signatures, alpha=c(0.05, 0.025, 0.01)){
#    scores = DIAS_scores(signatures, alpha)
#    score_mat = do.call(cbind, scores)
#    return(score_mat)
#    }
