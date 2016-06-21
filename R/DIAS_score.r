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
#- \code{DIAS_subset_score} and \code{DIAS_subset_scores_sum} than work
#- similarly, but return score only for specific \code{position} as specified by
#- sliding density.
#-
#- @param signature single column from signature table from
#-    \code{\link{get_signature}}
#- @param signatures signature table from \code{\link{get_signature}}
#- @param alpha vector of significance levels for scoring
#- @position position in density that is scored
#-
#- @return DIAS score, either single vector for single oligonucleotide, list
#- of vectors, total score for all oligonucleotides or matrix of all scores.
# TODO can I merge various functions while retaining robustness and simplicity?
# TODO wouldnt merging function and making more general, instead of specialized
# TODO functions increase probability of error? And shouldn't tests solve this?
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


#- @usage NULL
#- @rdname DIAS_score
DIAS_subset_score = function(signature, position, alpha=c(0.05, 0.025, 0.01)){
    if(length(dim(signature)) > 1){
        stop("ERROR: signature must be vector!")
        }

    # get density
    density = stats::density(signature, from=0)

    # get credibility interval
    credibility_intervals = get_credibility_interval(density, alpha=alpha)

    score = 0
    names(score) = names(signature)[position]

    for(single_credibility_interval in credibility_intervals){
        # score vector of data based on single credibility interval
        score = score + ifelse(
            signature[position] < single_credibility_interval[1] |
             signature[position] > single_credibility_interval[2], 1, 0)
        }

    return(score)
    }


#- @usage NULL
#- @rdname DIAS_score
DIAS_subset_scores_sum = function(signatures, position,
                                  alpha=c(0.05, 0.025, 0.01)
                                  ){
    scores = apply(signatures, 2, DIAS_subset_score, alpha=alpha,
                   position=position
                   )
    score = sum(scores)
    names(score) = rownames(signatures)[position]
    return(score)
    }


#- @usage NULL
#- @rdname DIAS_score
DIAS_subset_eye_score = function(signature, position, eye,
                                 alpha=c(0.05, 0.025, 0.01)){
    if(length(dim(signature)) > 1){
        stop("ERROR: signature must be vector!")
        }

    # get density
    filtered_signature = signature[-eye]
    density = stats::density(filtered_signature, from=0)

    # get credibility interval
    credibility_intervals = get_credibility_interval(density, alpha=alpha)

    score = 0
    names(score) = names(signature)[position]

    for(single_credibility_interval in credibility_intervals){
        # score vector of data based on single credibility interval
        score = score + ifelse(
            signature[position] < single_credibility_interval[1] |
             signature[position] > single_credibility_interval[2], 1, 0)
        }

    return(score)
    }


#- @usage NULL
#- @rdname DIAS_score
DIAS_subset_eye_scores_sum = function(signatures, position, eye,
                                  alpha=c(0.05, 0.025, 0.01)
                                  ){
    scores = apply(signatures, 2, DIAS_subset_score, alpha=alpha,
                   position=position, eye=eye
                   )
    score = sum(scores)
    names(score) = rownames(signatures)[position]
    return(score)
    }
