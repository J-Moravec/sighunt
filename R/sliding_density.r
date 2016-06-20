# Sliding density differs from global density by dividing signature into
# additional segments (according to sliding window) and analysing them
# individually.
# TODO I need bigger example of signature to test this.
# TODO function that given DIAS, original sequence and DIAS treshold,
# produce subset of sequences as individual as multifasta

sliding_density = function(signature, oligos=NULL, sliding_window=100,
                           alpha=c(0.05, 0.025, 0.01)
                           ){
    signature = filter_signature(signature, oligos)

     
    score = DIAS_scores_sum(signature, alpha=alpha)
    return(score)
    }
