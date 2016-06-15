#' Get signature
#'
#' Get signature for sequence. Signature is frequency of oligonucleotides
#' computed for smaller overlapping subsets -- windows.
#' Function \code{get_signature} will partition sequence into individual
#' windows, neighbouring windows overlap by \code{window-step}.
#' For these windows, frequency of oligonucleotides is computed as
#' count of oligonucleotide divided by maximum number of oligonucleotides of
#' given length in window.
#'
#' Quality of sequence is also outputted, this is computed as frequency of
#' non-ACGT characters.
#'
#' @param sequence string of DNA bases (ACGT)
#' @param oligos character (or character vector) of oligonucleotide.
#' @param window size of windows on which frequency is calculated
#' @param step amount of bases by which each window is moved #TODO nonsense, better description
#' @param file If specified, output will be piped file (or connection) instead
#'
#'
#' @return matrix of frequencies of \code{oligos} with quality of sequence
#'
#' @export
get_signature = function(sequence, oligos, window=5000, step=1000, file=NULL){
    start_f = function(i){1 + step * (i - 1)}    

    nonACGT = "[^ACGT]"
    sequence = gsub(nonACGT, "N", sequence)

    #TODO S3 class for sequence?
    oligo_len = nchar(oligos)
    n_oligos = window - oligo_len + 1
    n_windows = (nchar(sequence) - window) %/% step + 1
    oligo_freq_mat = matrix(0, ncol=length(oligos)+1, nrow=n_windows)
    colnames(oligo_freq_mat) = c(oligos, "quality")

    starts = start_f(1:n_windows)
    stops = starts - 1 + window
    rownames(oligo_freq_mat) = paste(starts, stops, sep="-")
    N = stringr::fixed("N")
    for (i_step in 1:n_windows){
        start = start_f(i_step)
        stop = start - 1 + window
        subseq = stringr::str_sub(sequence, start, stop)
        oligo_freq = count_oligo(oligos, subseq)
        oligo_freq = oligo_freq / n_oligos
        N_count = stringr::str_count(pattern=N, string=subseq)
        N_freq = N_count/window
        oligo_freq_mat[i_step,] = c(oligo_freq, N_freq)
        }    

    return(oligo_freq_mat)
    }
