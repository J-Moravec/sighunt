#' Computes credibility interval
#'
#' Function computes credibility interval for various alphas estimated density.
#'
#'
#' @param density estimated density with \code{\link[stats]{density}} function
#' @param alpha vector of significance levels for which credibility interval
#'    will be computed
#'
#' @return list of credibility intervals
get_credibility_interval = function(density, alpha=c(0.05, 0.025, 0.01)){
    if(class(density) != "density"){
        stop("ERROR: Density must be \"density\" class!")
        }
    if(!is.numeric(alpha)){
        stop("ERROR: Alpha must be numeric vector!")
        }

    get_ci = function(density, cumsum, alpha){
        lower_ci = density$x[which(cumsum > (alpha/2))[1]]
        upper_ci = density$x[which(cumsum > (1-alpha/2))[1]]
        return(c(lower_ci, upper_ci))
        }

    dx = mean(diff(density$x))
    dx = dx / (sum(density$y) * dx)
    cum_dens = density$y * dx
    cum_sum = cumsum(cum_dens)
    
    credibility_intervals = lapply(alpha, get_ci, density=density, cumsum=cum_sum)
    names(credibility_intervals) = as.character(alpha)
    return(credibility_intervals)
    }
