#' Reads fasta file.
#'
#' Reads fasta file from provided path and returns list of sequences (strings)
#' with sequence names as names of items in list. This include some manipulation
#' with sequence names to remove trailing whitespace and standardization of
#' sequences themselves (uppercase) for future smooth processing.
#' Also, if illegal characters are found in sequence, throws warning.
#'
#' @param file Name of fasta file
#' @inheritParams base::scan
#'
#' @return List of sequences.
#' @export
read_fasta = function(file, skip=0, nlines=0, comment.char=""){
    text = scan(file=file, what="character", sep="\n", quiet=TRUE,
                 skip=skip, nlines=nlines, comment.char=comment.char)
    names_pos = grep("^>", text)
    seq_names = sub("^>", "", text[names_pos])

    #remove trailing whitespace
    seq_names = sub("^\\s*", "", seq_names)
    seq_names = sub("\\s*$", "", seq_names)

    n_seq = length(seq_names)
    fasta = vector("list", n_seq)
    names(fasta) = seq_names
    seq_ends = c(names_pos[-1] - 1, length(text))
    seq_lengths = seq_ends - names_pos
    illegal_characters = "[^-ACGTUMRWSYKVHDBN?]"

    for (i in seq(1, length=n_seq)){
        fasta[[i]] = paste0(
            text[seq(from=names_pos[i]+1, length=seq_lengths[i])],
            collapse="")
        fasta[[i]] = toupper(fasta[[i]])

        #if illegal characters are found, throw warning
        if (grepl(illegal_characters, fasta[[i]])){
            warning(paste0("WARNING: sequence \"", names(fasta)[i],
                           "\" contains one or more illegal characters."))
            }
        }

    return(fasta)
    }
