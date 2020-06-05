#' ASCII chart of vector. Generate an ascii chart for a series of numbers.
#'
#' \code{asciiPlot} returns a character string of a chart for given series.
#
#' @param series A numeric vector, missing data values in the
#' series can be specified as a NA.
#'
#' @param cfg A named list with some options: \code{height} specifies the
#' number of rows the graph should occupy. It can be  used to scale down a
#' graph with large data values. \code{format} specifies a C format string
#' used to format the labels on the y-axis. The default value is "%.2f".
#' \code{symbols} provides a list of single characters to use for drawing
#' the curve. \code{offset} changes what column will the y-axis be drawn.
#' Default is 3. \code{min} and \code{max} will clamp the y-axis and all
#' values.
#'
#' @return A character string
#'
#' @examples
#' series <- c(1,2,3,4,4,3,2,1)
#' cat(asciiPlot(series))
#'
#' series <- c(1,2,3,NA,4,3,NA,1)
#' cat(asciiPlot(series))
#'
#' series <- c(1,2,3,4,NA,4,3,2,1)
#' cat(asciiPlot(series, list('min' = 0)))
#'
#' \dontrun{
#' cat(asciiPlot(c(NA, NA)))
#' }
#' @export
asciiPlot <- function(series, cfg=list()) {
  if (length(series) == 0 || sum(!is.na(series)) == 0) {
    return()
  }

  minimum <- ifelse(is.null(cfg[["min"]]), min(series, na.rm=TRUE), cfg[["min"]])
  maximum <- ifelse(is.null(cfg[["max"]]), max(series, na.rm=TRUE), cfg[["max"]])
  symbols <- if (is.null(cfg[["symbols"]])) {
    sapply(c(9532, 9508, 9590, 9588, 9472, 9584, 9581, 9582, 9583, 9474), intToUtf8)
  } else {
    cfg[["symbols"]]
  }

  if (minimum > maximum) {
    stop("The minimum value cannot exceed the maximum value.")
  }

  interval <- maximum - minimum
  offset <- ifelse(is.null(cfg[["offset"]]), 3, cfg[["offset"]])
  height <- ifelse(is.null(cfg[["height"]]), interval, cfg[["height"]])
  ratio <- ifelse(interval > 0, height / interval, 1)

  min2 <- as.integer(floor(minimum * ratio))
  max2 <- as.integer(ceiling(maximum * ratio))
  
  clamp <- function(n) {
    return(min(max(n, minimum), maximum))
  }
  
  scaled <- function(y) {
    return(round(clamp(y) * ratio) - min2)
  }
  
  rows <- max2 - min2
  width <- length(series) + offset
  placeholder <- ifelse(is.null(cfg[["format"]]), "%8.2f", cfg[["format"]])

  result <- array(' ' , c(rows+1, width))
  
  # axis and labels
  for (y in min2:max2) {
    label <- sprintf(placeholder, maximum - ((y - min2) * interval / ifelse(rows == 0, 1, rows)))
    result[y - min2+1, max(offset - length(label)-1, 1)] <- label
    result[y - min2-1, offset] <- ifelse(y == 0, symbols[1], symbols[2])  # zero tick mark
  }
  
  # first value is a tick mark across the y-axis
  d0 <- series[[1]]
  if (is.numeric(d0)) {
    result[rows - scaled(d0)+1, offset] <- symbols[1]
  }
  
  # plot the line
  for (x in 1:(length(series) - 1)) {
    d0 <- series[[x]]
    d1 <- series[[x + 1]]
    
    if (is.na(d0) && is.na(d1)) {
      next()
    }
    
    if (is.na(d0) && is.numeric(d1)) {
      result[rows - scaled(d1)+1, x + offset] <- symbols[3]
      next()
    }

    if (is.numeric(d0) && is.na(d1)) {
      result[rows - scaled(d0)+1, x + offset] <- symbols[4]
      next()
    }
    
    y0 <- scaled(d0)
    y1 <- scaled(d1)
    if (y0 == y1) {
      result[rows - y0+1, x + offset] <- symbols[5]
      next()
    }

    result[rows - y1+1, x + offset] <- ifelse(y0 > y1, symbols[6], symbols[7])
    result[rows - y0+1, x + offset] <- ifelse(y0 > y1, symbols[8], symbols[9])

    start <- min(y0, y1)+1
    end <- max(y0, y1) - 1
    if (end < start) {
      next()
    }
    for (y in start:end) {
      result[rows - y+1, x + offset] <- symbols[10]
    }
  }
  
  return(paste(apply(res, 1, function(x) paste(x, collapse="")), collapse='\n'))
}

