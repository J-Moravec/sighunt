#- Sliding density methods
#-
#- This is base function for both \code{sliding_density} and \code{eye_of_storm}
#- functions as they differ only by single step. Thus both above mentioned
#- methods are gained from this function by different parametrisation.
#-
#- @param signature signature from \code{\link{get_signature}} function
#- @param oligos either numeric or character vector of oligonucleotides
#-    that will be used. If nos specified, whole signature is used.
#- @param window size of subset to the left
#- @param eye number of segments around centered segment which are excluded
#-    from density estimation.
#- @param alpha significance level of scoring intervals.
#- @param bar progress bar
#-
#- @return DIAS score for given signature
sliding_density_methods = function(signature, oligos=NULL, window=100, eye=NULL,
                           alpha=c(0.05, 0.025, 0.01), bar=TRUE
                           ){
    signature = filter_signature(signature, oligos)
    score = vector(mode="numeric", length=nrow(signature))
    names(score) = rownames(signature)
    if(bar){
        pbar = utils::txtProgressBar(
            min=1, max=nrow(signature), initial=1, style=3
            )
        }

    max_length = nrow(signature)
    for(i in 1:max_length){
        interval = get_interval(i, window, max_length)
        pos = i - interval["from"] + 1

        subset_signature = signature[interval["from"]:interval["to"], ]
        if(is.null(eye)){
            subset_score = DIAS_scores_sum(subset_signature, pos, alpha=alpha)
            } else {
            eye_interval = get_interval(i, eye, max_length)
            subset_score = DIAS_scores_sum(
                subset_signature, pos,
                eye=eye_interval["from"]:eye_interval["to"],
                alpha=alpha
                )
            }
        score[i] = subset_score
        if(bar){
            setTxtProgressBar(pbar, i)
            }
        }
    return(score)
    }


#- Get interval around window
#-
#- Calculate maximal allowed interval around current position and window.
#- Borders are set by taking distance from current \code{position} specified by
#- \code{window} parameter. Lower border of interval cannot be lower than 1
#- and greater than \code{max_length}.
#-
#- @param position current position around which interval is calculated
#- @param window distance from current position
#- @max_length maximum length of higher border of interval.
#-
#- @return vector of left and right border of interval
get_interval = function(position, window, max_length){
    from = max(position - window, 1)
    to = min(position + window, max_length)
    interval = c(from, to)
    names(interval) = c("from", "to")
    return(interval)
    }
