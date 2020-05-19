#' An S4 class to represent a PWM matrix.
#'
#' @description An object of class `'pwm'` represents the alphabet*width
#'   position weight matrix of a sequence motif. In case of DNA sequence motif,
#'   the entry in row i, column j gives the probability of observing nucleotide
#'   `c('A','C','G','T')[i]` in position j of the motif.
#'
#' @slot pwm matrix. The position weight matrix.
#' @slot width numeric. The width of the motif.
#' @slot ic numeric. The information content (IC).
#' @slot alphabet character. The sequence alphabet. Currently, only 'DNA' and
#'   'RNA' is supported.
#' @slot consensus character. The consensus sequence.
#'
#' @return `pwm-class` object with slots: `pwm`, `width`, `ic` and `alphabet`.
#'
#' @author Oliver Bembom
#'
#' @aliases
#' pwm
#' ic
#' consensus
#' alphabet
#'
#' @examples
#' mFile <- system.file("extdata/pwm1", package = "seqLogo")
#' m <- read.table(mFile)
#' p <- makePWM(m)
#' #
#' # slot access
#' pwm(p)
#' ic(p)
#' consensus(p)
#' @export
setClass("pwm", representation(
    pwm = "matrix", width = "numeric", ic = "numeric",
    alphabet = "character", consensus = "character"
))
