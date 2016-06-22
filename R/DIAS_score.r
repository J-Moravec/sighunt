#- Produce DIAS score out of signature
#-
#- Functions produce DIAS scoring of single signature (signature for single
#- oligonucleotide) or whole signature matrix. Scoring is based on how extreme
#- is individual segment from which oligonucleotide frequency was computed.
#-
#- Measure of extremity is confidence interval. By default, three confidence
#- are used: 95\%, 97.5\% and 99\%, these are specified by significance levels.
#-
#- Function \code{DIAS_score} is the underlying scoring function. It scores
#- windows, or single window, according to credibility interval derived from
#- estimated density. Its behaviour is driven by two optional parameters,
#- according which function change slightly its behaviour.
#-
#- Function \code{DIAS_scores_sum} than applies \code{DIAS_score} on each
#- tetranucleotide and sums results.
#-
#- @param signature single column from signature table from
#-    \code{\link{get_signature}}
#- @param signatures signature table from \code{\link{get_signature}}
#- @param alpha vector of significance levels for scoring
#- @position position in density that is scored
#- @eye number of windows excluded from density estimation
#-
#- @return DIAS score, either single vector for single oligonucleotide
#- or total score for all oligonucleotides.
DIAS_score = function(signature, position=NULL, eye=NULL,
                                 alpha=c(0.05, 0.025, 0.01)){
    if(length(dim(signature)) > 1){
        stop("ERROR: signature must be vector!")
        }

    if(is.null(position) & !is.null(eye)){
        stop("ERROR: If parameter eye is supplied, parameter position must be",
             " supplied as well!")
        }

    # get density
    if(is.null(eye)){
        filtered_signature = signature
        } else {
        filtered_signature = signature[-eye]
        }

    if(is.null(position)){
        score = vector(mode="numeric", length=length(signature))
        names(score) = names(signature)
        } else {
        score = 0
        names(score) = names(signature)[position]
        }

    density = stats::density(filtered_signature, from=0)

    # get credibility interval
    credibility_intervals = get_credibility_interval(density, alpha=alpha)

    for(single_credibility_interval in credibility_intervals){
        # score vector of data based on single credibility interval
        if(is.null(position)){
            score = score + ifelse(
                signature < single_credibility_interval[1] |
                signature > single_credibility_interval[2], 1, 0)
            } else {
            score = score + ifelse(
                signature[position] < single_credibility_interval[1] |
                signature[position] > single_credibility_interval[2], 1, 0)
            }
        }
    return(score)
    }


#- @usage NULL
#- @rdname DIAS_score
DIAS_scores_sum = function(signatures, position=NULL, eye=NULL,
                                  alpha=c(0.05, 0.025, 0.01)
                                  ){
    if(is.null(position) & !is.null(eye)){
        stop("ERROR: If parameter eye is supplied, parameter position must be",
             " supplied as well!")
        }

    scores = apply(signatures, 2, DIAS_score, alpha=alpha,
                   position=position, eye=eye
                   )
    if(is.null(position)){
        score = rowSums(scores)
        } else {
        score = sum(scores)
        names(score) = rownames(signatures)[position]
        }
    return(score)
    }
