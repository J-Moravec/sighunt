#' Create fake islands
#'
#' Function substitute part of target sequence for part of source sequence,
#' creating an artificial genomic island.
#'
#' 
#' @param target sequence that will be modified
#' @param source sequence from which islands are taken
#' @param islands list of vectors of three elements. First is start position of
#' island in source sequence, second is start position of island in target
#' sequenec. Third is length of island.
#'
#' @return modified target sequence with short segments from source sequence
#' @export
create_fake_island = function(target, source, islands){
    sequence = target

    if(typeof(target) != "character"){
        stop("ERROR: Type of target must be character.")
        }

    if(typeof(source) != "character"){
        stop("ERROR: Type of source must be character.")
        }

    l_target = nchar(target)
    l_source = nchar(source)

    if(l_target == 0){
        stop("ERROR: length of target must be greater than 0.")
        }
    if(l_source == 0){
        stop("ERROR: length of source must be greater than 0.")
        }


    # testing for valid input
    if(!is.list(islands)){
        stop("ERROR: islands must be a list!")
        }

    for(island in islands){
        if(length(island) < 3){
            stop("ERROR: must provide valid number of parameters!")
            }
        }

    for(island in islands){
        stop_s = island[1] + island[3] - 1
        if(stop_s > l_source){
            stop("ERROR: Source sequence ends before island stop position.",
                 " Set shorter island length.")
            }
        stop_t = island[2] + island[3] - 1
        if(stop_t > l_target){
            stop("ERROR: Target sequence ends before island stop position.",
                 " Set shorter island length.")
            }
        }

    # substitution of source to target
    for(island in islands){
        start_t = island[2]
        stop_t = island[2] + island[3] - 1
        start_s = island[1]
        stop_s = island[1] + island[3] - 1
        stringr::str_sub(sequence, start_t, stop_t) = 
            stringr::str_sub(source, start_s, stop_s)
        }

    return(sequence)
    }
