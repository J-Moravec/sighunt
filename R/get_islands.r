#' Get islands from score
#'
#' Get sequence of islands for which DIAS \code{score} is greater than
#' \code{threshold}.
#'
#' This function will export continuous segments of score, that are greater
#' than required threshold. Sequences are returned as matrix, first column is
#' name, which in this stage is just left and right border connected by \"-\",
#' and second column is sequence itself.
#'
#' @param score DIAS score from one of scoring function
#' @param sequence original sequence for which score was computed
#' @threshold threshold value, from which every scored segment is considered
#' a potential island. Segments with the same value as threshold are included
#'
#' @return matrix of names and sequences
#' @export
get_islands = function(score, sequence, threshold){ # flanking){
    thresholded = ifelse(score >= threshold, 1, 0)
    island_segments = get_segments(thresholded)
    island_borders = get_borders(names(score), island_segments)
    islands = lapply(island_borders, get_island, sequence=sequence)
    islands = matrix(unlist(islands), ncol=2, byrow=TRUE)
    colnames(islands) = c("name", "sequence")
    return(islands)
    }


#- Get sequence of island from whole sequence
#-
#- Get sequence of island from whole sequence according to prepared island
#- borders.
#-
#- @param island_border name and borders of island
#- @param sequence original sequence
#-
#- @return list of name and sequence
get_island  = function(island_border, sequence){
    subseq = stringr::str_sub(
        sequence, as.numeric(island_border[2]),
        as.numeric(island_border[3])
        )
    return(list("name"=island_border[1], "sequence"=subseq))
    }


#- Get continuous elements from vector
#-
#- Gain continuous elements (marked as ones) from provided vector.
#-
#- @param segments vector of ones and zeros, ones marking presence of segment.
#-
#- @return positions of first element and length of segments.
get_segments = function(segments){
    segments_rle = rle(segments)
    segments_rle$cumsum = cumsum(segments_rle$lengths)
    pos = which(segments_rle$values == 1)
    positions = segments_rle$cumsum - segments_rle$lengths + 1
    positions = positions[pos]
    lengths = segments_rle$lengths[pos]
    names(lengths) = NULL
    names(positions) = NULL
    return(list("positions"=positions, "lengths"= lengths))
    }


#- Get borders from island names according to segments
#-
#- Function will take prepared segments and names from score and
#- applies \code{get_border} function on all segments.
#- This create a list of vectors for each segment. Each
#- vector contain name of segment and starting and ending position.
#-
#- @param score_names character vector of names from score
#- @param island_segments processed segments from \code{rle} function.
#-      Starting position and length of each segment.
#- @return list of 3 element vectors.
get_borders = function(score_names, island_segments){
    borders = mapply(
        get_border, island_segments$positions, island_segments$lengths,
        MoreArgs=list("score_names" = score_names), SIMPLIFY=FALSE
        )
    return(borders)
    }


#- Get border from island names according to position and length
#-
#- @param position starting position of segment
#- @param length total length of segment
#- @score_names character vector of names from score
#-
#- @return three element vector of name and starting and finishing border
#- in sequence.
get_border = function(position, length, score_names){
    from = split_name(score_names[position], 1)
    to = split_name(score_names[position + length - 1], 2)
    name = paste(from, to, sep="-")
    return(c(name, from, to))
    }


#- Splits text separated by \code{-} and extract specified part.
#-
#- @param name string separated by \code{-}
#- @param position part that will be extracted
#-
#- @return n-th substring gained from \code{name} after its splitted
split_name = function(name, position){
    new_name = unlist(strsplit(name, split="-", fixed=TRUE))[position]
    return(new_name)
    }
