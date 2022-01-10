## constructor ----------------------------------------------------------------

#' Constructing a pwm object
#'
#' @description This function constructs an object of class pwm from a matrix. It checks that the matrix has correct dimensions and that columns add up to 1.0.
#'
#' @param pwm matrix. Numerical matrix representing the position weight matrix.
#' @param alphabet character. The alphabet making up the sequence. Currently, only 'DNA' and 'RNA' is supported.
#'
#' @return An object of class pwm.
#'
#' @author Oliver Bembom
#'
#' @import grid
#' @import methods
#' @import stats4
#'
#' @examples
#' mFile <- system.file("extdata/pwm1", package = "seqLogo")
#' m <- read.table(mFile)
#' pwm <- makePWM(m)
#' @export
makePWM <- function(pwm, alphabet = "DNA") {
    if (is.data.frame(pwm)) {
          pwm <- as.matrix(pwm)
      }
    if (!is.matrix(pwm)) {
          stop("pwm must be a matrix or a dataframe")
      }

    if (!alphabet %in% c("DNA", "RNA", "AA")) {
          stop("alphabet must be either DNA, RNA or AA")
      }
    if (alphabet == "DNA") {
        if (nrow(pwm) != 4) {
              stop("PWM for DNA motifs must have 4 rows")
          }
        rownames(pwm) <- c("A", "C", "G", "T")
    } else if (alphabet == "RNA") {
        if (nrow(pwm) != 4) {
              stop("PWM for RNA motifs must have 4 rows")
          }
        rownames(pwm) <- c("A", "C", "G", "U")
    } else if (alphabet == "AA") {
        if (nrow(pwm) != 20) {
              stop("PWM for amino acid motifs must have 20 rows")
          }
        rownames(pwm) <- c(
            "A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M",
            "N", "P", "Q", "R", "S", "T", "V", "W", "Y"
        )
    }
    if (any(abs(1 - apply(pwm, 2, sum)) > 0.01)) {
        warning("Columns of PWM must add up to 1.0")
    }

    width <- ncol(pwm)
    colnames(pwm) <- seq_len(width)

    cons <- pwm2cons(pwm)
    ic <- pwm2ic(pwm)

    new("pwm", pwm = pwm, consensus = cons, ic = ic, width = width, alphabet = alphabet)
}
## class functions ------------------------------------------------------------

#' @param object object of `pwm-class`
#'
#' @describeIn pwm-class Shows the position weight matrix.
#'
#' @export
#'
setMethod("show", signature(object = "pwm"), function(object) {
    print(round(object@pwm, 4))
})

#' @param object object of `pwm-class`
#'
#' @describeIn pwm-class Prints the summary information about position weight
#'   matrix.
#'  
#' @export
#'
setMethod("summary", signature(object = "pwm"), function(object, ...) {
    cat("Position weight matrix:\n")
    print(round(object@pwm, 4))
    cat("\n\nInformation content:\n")
    print(round(object@ic, 4))
    cat("\n\nConsensus sequence:\n")
    print(object@consensus)
})

#' @param x object of `pwm-class`
#' @param y default (missing) for `plot` function
#' @param ... additional parameters for `plot` function
#'
#' @describeIn pwm-class Plots the sequence logo of the position weight matrix.
#' 
#' @export
#'
setMethod("plot", signature(x = "pwm"), function(x, y = "missing", ...) {
    seqLogo(x)
})


## getters --------------------------------------------------------------------

#' @param pwm  object of `pwm-class`
#' @describeIn pwm-class Access to 'pwm' slot
#' @export
setMethod("pwm", "pwm", function(pwm) pwm@pwm)

#' @param pwm  object of `pwm-class`
#' @describeIn pwm-class Access to 'ic' slot
#' @export
setMethod("ic", "pwm", function(pwm) pwm@ic)

#' @param pwm  object of `pwm-class`
#' @describeIn pwm-class Access to 'consensus' slot
#' @export
setMethod("consensus", "pwm", function(pwm) pwm@consensus)

# #' @param pwm object of `pwm-class` #' @describeIn pwm-class Access to
# 'alphabet' slot #' @export setMethod('alphabet', 'pwm', function(pwm)
# pwm@alphabet)

## helpers --------------------------------------------------------------------

#' Calculates information content profile from PWM
#'
#' @param pwm matrix. Position weight matrix.
#'
#' @return numeric. Vector of length identical to with of the 'pwm'.
#'
#' @keywords internal
#' @noRd
pwm2ic <- function(pwm) {
    if (!is.matrix(pwm) || !is.numeric(pwm)) {
        warning("pwm argument must be of class matrix (numeric)")
    }
    npos <- ncol(pwm)
    ic <- numeric(length = npos)
    for (i in seq_len(npos)) {
        ic[i] <- 2 + sum(vapply(pwm[, i], function(x) {
            if (x > 0) {
                x * log2(x)
            } else {
                0
            }
        }, FUN.VALUE = numeric(1)))
    }
    ic
}

#' Extracts consensus sequence from PWM
#'
#' @param pwm matrix. Position weight matrix.
#'
#' @return character. Consensus sequence pf the length identical to the width of 'pwm'.
#'
#' @keywords internal
#' @noRd
pwm2cons <- function(pwm) {
    if (!is.matrix(pwm) || !is.numeric(pwm)) {
        warning("pwm argument must be of class matrix")
    }
    letters <- rownames(pwm)
    paste(apply(pwm, 2, function(x) {
        letters[rev(order(x))[1]]
    }), collapse = "")
}
