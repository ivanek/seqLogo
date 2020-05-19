## helpers --------------------------------------------------------------------

#' Rescales the values (internal)
#'
#' @param x numeric. Original values.
#' @param to numeric. Output range,  vector of length two.
#'
#' @return numeric. Rescaled values. Same length as `x`
#' @keywords internal
#' @noRd
.rescale <- function(x, to = c(0.025, 0.975)) {
    from <- range(x, na.rm = TRUE, finite = TRUE)
    (x - from[1]) / diff(from) * diff(to) + to[1]
}


#' Check if the colors are valid
#'
#' @param x character. Vectors of colors to check.
#'
#' @return logical. Same length as `x`.
#' @importFrom grDevices col2rgb
#' @keywords internal
#' @noRd
.isValidColor <- function(x) {
    vapply(x, function(X) {
        tryCatch(is.matrix(col2rgb(X)), error = function(e) FALSE)
    }, FUN.VALUE = logical(1), USE.NAMES = FALSE)
}

## letters --------------------------------------------------------------------


#' Draw A letter
#'
#' @param x.pos numeric. X position for the letter.
#' @param y.pos numeric. Y position for the letter.
#' @param ht numeric. Height of the letter.
#' @param wt numeric. Width of the letter.
#' @param fill character. Color of the letter (polygon shape fill)
#' @param id integer. Shape id.
#'
#' @return list. List with 'x', 'y' coordinates, 'id' and 'fill' of the polygon
#' shape (letter).
#'
#' @keywords internal
#' @noRd
letterA <- function(x.pos, y.pos, ht, wt, fill = "#61D04F", id = NULL) {
    x <- c(0, 4, 6, 2, 0, 4, 6, 10, 8, 4, 3.2, 6.8, 6.4, 3.6, 3.2)
    y <- c(0, 10, 10, 0, 0, 10, 10, 0, 0, 10, 3, 3, 4, 4, 3)
    x <- 0.1 * x
    y <- 0.1 * y

    x <- .rescale(x)
    x <- x.pos + wt * x
    y <- y.pos + ht * y

    if (is.null(id)) {
        id <- c(rep(1L, 5), rep(2L, 5), rep(3L, 5))
    } else {
        id <- c(rep(id, 5), rep(id + 1L, 5), rep(id + 2L, 5))
    }

    fill <- rep(fill, 3) # green

    list(x = x, y = y, id = id, fill = fill)
}

#' Draw T letter
#'
#' @param x.pos numeric. X position for the letter.
#' @param y.pos numeric. Y position for the letter.
#' @param ht numeric. Height of the letter.
#' @param wt numeric. Width of the letter.
#' @param fill character. Color of the letter (polygon shape fill)
#' @param id integer. Shape id.
#'
#' @return list. List with 'x', 'y' coordinates, 'id' and 'fill' of the polygon
#' shape (letter).
#'
#' @keywords internal
#' @noRd
letterT <- function(x.pos, y.pos, ht, wt, fill = "#DF536B", id = NULL) {
    x <- c(0, 10, 10, 6, 6, 4, 4, 0)
    y <- c(10, 10, 9, 9, 0, 0, 9, 9)
    x <- 0.1 * x
    y <- 0.1 * y

    x <- .rescale(x)
    x <- x.pos + wt * x
    y <- y.pos + ht * y

    if (is.null(id)) {
        id <- rep(1, 8)
    } else {
        id <- rep(id, 8)
    }

    fill <- rep(fill, 1) # red

    list(x = x, y = y, id = id, fill = fill)
}


#' Draw U letter
#'
#' @param x.pos numeric. X position for the letter.
#' @param y.pos numeric. Y position for the letter.
#' @param ht numeric. Height of the letter.
#' @param wt numeric. Width of the letter.
#' @param fill character. Color of the letter (polygon shape fill)
#' @param id integer. Shape id.
#'
#' @return list. List with 'x', 'y' coordinates, 'id' and 'fill' of the polygon
#' shape (letter).
#'
#' @keywords internal
#' @noRd
letterU <- function(x.pos, y.pos, ht, wt, fill = "#DF536B", id = NULL) {
    angle1 <- seq(pi / 2, pi, length = 100)
    angle2 <- seq(pi, 1.5 * pi, length = 100)
    x.l1 <- 0.5 + 0.5 * sin(angle1)
    y.l1 <- 0.5 + 0.5 * cos(angle1)
    x.l2 <- 0.5 + 0.5 * sin(angle2)
    y.l2 <- 0.5 + 0.5 * cos(angle2)

    x.l <- c(x.l1, x.l2)
    y.l <- c(y.l1, y.l2)

    x.i1 <- 0.5 + 0.3 * sin(angle1)
    y.i1 <- 0.5 + 0.35 * cos(angle1)
    x.i1 <- x.i1[y.i1 <= max(y.l1)]
    y.i1 <- y.i1[y.i1 <= max(y.l1)]
    y.i1[1] <- max(y.l1)

    x.i2 <- 0.5 + 0.3 * sin(angle2)
    y.i2 <- 0.5 + 0.35 * cos(angle2)

    x.i <- c(x.i1, x.i2)
    y.i <- c(y.i1, y.i2)

    x <- c(x.l, 0, 0.2, 0.2, rev(x.i), 0.8, 0.8, 1, 1)
    y <- c(y.l, 1, 1, 0.5, rev(y.i), 0.5, 1, 1, 0.85)

    x <- .rescale(x)
    x <- x.pos + wt * x
    y <- y.pos + ht * y

    if (is.null(id)) {
        id <- rep(1, length(x))
    } else {
        id <- rep(id, length(x))
    }

    fill <- rep(fill, 1) # blue

    list(x = x, y = y, id = id, fill = fill)
}

#' Draw C letter
#'
#' @param x.pos numeric. X position for the letter.
#' @param y.pos numeric. Y position for the letter.
#' @param ht numeric. Height of the letter.
#' @param wt numeric. Width of the letter.
#' @param fill character. Color of the letter (polygon shape fill)
#' @param id integer. Shape id.
#'
#' @return list. List with 'x', 'y' coordinates, 'id' and 'fill' of the polygon
#' shape (letter).
#'
#' @keywords internal
#' @noRd
letterC <- function(x.pos, y.pos, ht, wt, fill = "#2297E6", id = NULL) {
    angle1 <- seq(0.3 + pi / 2, pi, length = 100)
    angle2 <- seq(pi, 1.5 * pi, length = 100)
    x.l1 <- 0.5 + 0.5 * sin(angle1)
    y.l1 <- 0.5 + 0.5 * cos(angle1)
    x.l2 <- 0.5 + 0.5 * sin(angle2)
    y.l2 <- 0.5 + 0.5 * cos(angle2)

    x.l <- c(x.l1, x.l2)
    y.l <- c(y.l1, y.l2)

    x <- c(x.l, rev(x.l))
    y <- c(y.l, 1 - rev(y.l))

    x.i1 <- 0.5 + 0.35 * sin(angle1)
    y.i1 <- 0.5 + 0.35 * cos(angle1)
    x.i1 <- x.i1[y.i1 <= max(y.l1)]
    y.i1 <- y.i1[y.i1 <= max(y.l1)]
    y.i1[1] <- max(y.l1)

    x.i2 <- 0.5 + 0.35 * sin(angle2)
    y.i2 <- 0.5 + 0.35 * cos(angle2)

    x.i <- c(x.i1, x.i2)
    y.i <- c(y.i1, y.i2)

    x1 <- c(x.i, rev(x.i))
    y1 <- c(y.i, 1 - rev(y.i))

    x <- c(x, rev(x1))
    y <- c(y, rev(y1))

    x <- .rescale(x)
    x <- x.pos + wt * x
    y <- y.pos + ht * y

    if (is.null(id)) {
        id <- rep(1, length(x))
    } else {
        id <- rep(id, length(x))
    }

    fill <- rep(fill, 1) # blue

    list(x = x, y = y, id = id, fill = fill)
}

#' Draw G letter
#'
#' @param x.pos numeric. X position for the letter.
#' @param y.pos numeric. Y position for the letter.
#' @param ht numeric. Height of the letter.
#' @param wt numeric. Width of the letter.
#' @param fill character. Color of the letter (polygon shape fill)
#' @param id integer. Shape id.
#'
#' @return list. List with 'x', 'y' coordinates, 'id' and 'fill' of the polygon
#' shape (letter).
#'
#' @keywords internal
#' @noRd
letterG <- function(x.pos, y.pos, ht, wt, fill = "#F5C710", id = NULL) {
    angle1 <- seq(0.3 + pi / 2, pi, length = 100)
    angle2 <- seq(pi, 1.5 * pi, length = 100)
    x.l1 <- 0.5 + 0.5 * sin(angle1)
    y.l1 <- 0.5 + 0.5 * cos(angle1)
    x.l2 <- 0.5 + 0.5 * sin(angle2)
    y.l2 <- 0.5 + 0.5 * cos(angle2)

    x.l <- c(x.l1, x.l2)
    y.l <- c(y.l1, y.l2)

    x <- c(x.l, rev(x.l))
    y <- c(y.l, 1 - rev(y.l))

    x.i1 <- 0.5 + 0.35 * sin(angle1)
    y.i1 <- 0.5 + 0.35 * cos(angle1)
    x.i1 <- x.i1[y.i1 <= max(y.l1)]
    y.i1 <- y.i1[y.i1 <= max(y.l1)]
    y.i1[1] <- max(y.l1)

    x.i2 <- 0.5 + 0.35 * sin(angle2)
    y.i2 <- 0.5 + 0.35 * cos(angle2)

    x.i <- c(x.i1, x.i2)
    y.i <- c(y.i1, y.i2)

    x1 <- c(x.i, rev(x.i))
    y1 <- c(y.i, 1 - rev(y.i))

    x <- c(x, rev(x1))
    y <- c(y, rev(y1))

    h1 <- max(y.l1)
    r1 <- max(x.l1)

    h1 <- 0.4

    x.add <- c(r1, 0.5, 0.5, r1 - 0.2, r1 - 0.2, r1, r1)
    y.add <- c(h1, h1, h1 - 0.1, h1 - 0.1, 0, 0, h1)

    if (is.null(id)) {
        id <- c(rep(1, length(x)), rep(2, length(x.add)))
    } else {
        id <- c(rep(id, length(x)), rep(id + 1, length(x.add)))
    }

    x <- c(rev(x), x.add)
    y <- c(rev(y), y.add)

    x <- .rescale(x)
    x <- x.pos + wt * x
    y <- y.pos + ht * y


    fill <- rep(fill, 2) # orange/yellow

    list(x = x, y = y, id = id, fill = fill)
}

## addLetter ------------------------------------------------------------------

#' Add letter (polygon shape ) to existing list of shapes.
#'
#' @param letters list. List with 'x', 'y' coordinates, 'id' and 'fill' of the polygon shape (letter).
#' @param which character. Letter to be added.
#' @param x.pos numeric. X position for the letter.
#' @param y.pos numeric. Y position for the letter.
#' @param ht numeric. Height of the letter.
#' @param wt numeric. Width of the letter.
#' @param fill character. Color of the letter (polygon shape fill)
#' @param id integer. Shape id.
#'
#' @return list. List with 'x', 'y' coordinates, 'id' and 'fill' of the polygon
#' shape (letter).
#'
#' @keywords internal
#' @noRd
addLetter <- function(letters, which, x.pos, y.pos, ht, wt, fill) {
    if (which == "A") {
        letter <- letterA(x.pos, y.pos, ht, wt, fill = fill["A"])
    } else if (which == "C") {
        letter <- letterC(x.pos, y.pos, ht, wt, fill = fill["C"])
    } else if (which == "G") {
        letter <- letterG(x.pos, y.pos, ht, wt, fill = fill["G"])
    } else if (which == "T") {
        letter <- letterT(x.pos, y.pos, ht, wt, fill = fill["T"])
    } else if (which == "U") {
        letter <- letterU(x.pos, y.pos, ht, wt, fill = fill["U"])
    } else {
        stop(sprintf("\"which\" must be one of %s", paste(names(fill), collapse = ", ")))
    }

    letters$x <- c(letters$x, letter$x)
    letters$y <- c(letters$y, letter$y)

    lastID <- ifelse(is.null(letters$id), 0, max(letters$id))
    letters$id <- c(letters$id, lastID + letter$id)
    letters$fill <- c(letters$fill, as.character(letter$fill)) # remove name
    letters
}


## plotting -------------------------------------------------------------------

#' Plot a sequence logo for a given position weight matrix
#'
#' @description This function takes the alphabet*width position weight matrix of
#'   a sequence motif and plots the corresponding sequence logo.
#'
#' @usage
#' seqLogo(pwm, ic.scale=TRUE, xaxis=TRUE, yaxis=TRUE, xfontsize=15, yfontsize=15,
#'         fill=c(A='#61D04F', C='#2297E6', G='#F5C710', T='#DF536B'))
#'
#' @param pwm numeric. The alphabet*width position weight matrix.
#' @param ic.scale logical. If TRUE, the height of each column is proportional to its information content. Otherwise, all columns have the same height.
#' @param xaxis logical. If TRUE, an X-axis will be plotted.
#' @param yaxis logical. If TRUE, a Y-axis will be plotted.
#' @param xfontsize numeric. Font size to be used for the X-axis.
#' @param yfontsize numeric.  Font size to be used for the Y-axis.
#' @param fill character. Fill color to be used for the letters. Must be a named character vector of length equal to number of rows in `pwm` slot and names identical to its rownames.
#'
#' @return NULL.
#'
#' @author Oliver Bembom
#'
#' @import grid
#' @import methods
#'
#' @examples
#' mFile <- system.file("extdata/pwm1", package = "seqLogo")
#' m <- read.table(mFile)
#' p <- makePWM(m)
#' seqLogo(p)
#' @export
seqLogo <- function(pwm, ic.scale = TRUE, xaxis = TRUE, yaxis = TRUE, xfontsize = 15,
                    yfontsize = 15, fill = c(A = "#61D04F", C = "#2297E6", G = "#F5C710", T = "#DF536B")) {
    if (is.data.frame(pwm) || is.matrix(pwm)) {
        pwm <- makePWM(pwm)
    }

    if (!is(pwm, "pwm")) {
        stop("pwm must be of class pwm, matrix or data.frame")
    }

    # adjust default color setting if alphabet set to RNA
    if (pwm@alphabet == "RNA") {
        if ("T" %in% names(fill)) {
            names(fill)[which(names(fill) == "T")] <- "U"
        }
    }

    if (!is.character(fill) || length(fill) != nrow(pwm(pwm)) || !all(rownames(pwm(pwm)) %in%
        names(fill)) || !all(.isValidColor(fill))) {
        stop(
            "\"fill\" must be named character vector of length ",
            sprintf(
                "%d, with names %s (matching the rownames of pwm)",
                nrow(pwm(pwm)), paste(rownames(pwm(pwm)), collapse = ", ")
            ), "and valid colors as values"
        )
    }

    chars <- rownames(pwm(pwm))
    letters <- list(x = NULL, y = NULL, id = NULL, fill = NULL)
    npos <- ncol(pwm(pwm))


    if (ic.scale) {
        ylim <- 2
        ylab <- "Information content"
        facs <- ic(pwm)
    } else {
        ylim <- 1
        ylab <- "Probability"
        facs <- rep(1, npos)
    }

    wt <- 1
    x.pos <- 0
    for (j in seq_len(npos)) {
        column <- pwm(pwm)[, j]
        hts <- 0.95 * column * facs[j]
        letterOrder <- order(hts)

        y.pos <- 0
        for (i in seq_len(4)) {
            letter <- chars[letterOrder[i]]
            ht <- hts[letterOrder[i]]
            if (ht > 0) {
                  letters <- addLetter(letters, letter, x.pos, y.pos, ht, wt, fill)
              }
            y.pos <- y.pos + ht + 0.01
        }
        x.pos <- x.pos + wt
    }

    grid.newpage()
    bottomMargin <- ifelse(xaxis, 2 + xfontsize / 3.5, 2)
    leftMargin <- ifelse(yaxis, 2 + yfontsize / 3.5, 2)
    pushViewport(plotViewport(c(bottomMargin, leftMargin, 2, 2)))
    pushViewport(dataViewport(0:ncol(pwm(pwm)), 0:ylim, name = "vp1"))
    grid.polygon(
        x = unit(letters$x, "native"), y = unit(letters$y, "native"), id = letters$id,
        gp = gpar(fill = letters$fill, col = "transparent")
    )
    if (xaxis) {
        grid.xaxis(
            at = seq(0.5, ncol(pwm(pwm)) - 0.5), label = seq_len(ncol(pwm(pwm))),
            gp = gpar(fontsize = xfontsize)
        )
        grid.text("Position", y = unit(-3, "lines"), gp = gpar(fontsize = xfontsize))
    }
    if (yaxis) {
        grid.yaxis(gp = gpar(fontsize = yfontsize))
        grid.text(ylab, x = unit(-3, "lines"), rot = 90, gp = gpar(fontsize = yfontsize))
    }
    popViewport()
    popViewport()
}
