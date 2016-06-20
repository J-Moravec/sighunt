#- Filters signature
#-
#- Filters signature according to required oligonucleotides provided by
#- \code{oligos} and removes "quality" column.
#-
#- This function is used as first step before all DIAS functions to prepare
#- and test for correctness of input signature.
#-
#- @param signature signature from \code{\link{get_signature}} function
#- @param oligos character vector of oligonucleotides or numeric vector of
#-    their positions in signature.
#-
#- @return filtered signature according to oligos vector and without quality
#-    column.
filter_signature = function(signature, oligos=NULL){
    # filter quality
    contains_quality = colnames(signature) == "quality"
    if(any(contains_quality)){
        signature = signature[, !contains_quality]
        }

    if(is.null(oligos)){
        oligos = 1:ncol(signature)
        }

    # filter signature by oligos
    if(is.numeric(oligos)){
        signature = signature[, oligos, drop=FALSE]
        } else if(is.character(oligos)){
        if(!any(oligos %in% colnames(signature))){
            message("WARNING: Some oligos are not contained in signature.")
            }
        signature = signature[, colnames(signature) %in% oligos, drop=FALSE]
        } else {
        stop("ERROR: oligos must be either numeric or character vector!")
        }
    return(signature)
    }
