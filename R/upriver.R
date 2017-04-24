#' Calculate positions after some time.
#'
#' Calculate positions after \code{ndays} has elapsed, given the movement
#' parameters provided in \code{parameters}.
#'
#' \code{parameters} should be a list with three elements, \code{rates}, and
#' \code{distances}, each of equal size.
#' \code{rates} should be a numeric vector of reach-specific daily movement
#' rates, in whatever unit your analysis needs.
#' \code{distances} should be a numeric vector of reach-specific reach lengths
#' (end-to-end), in the (ideally) the same or compatible units to the units used
#'  in \code{rates}
#'
#' @param ndays (numeric) The number of days to calculate positions for.
#' @param parameters (list) List with names reaches, rates, distances. See
#' details.
#'
#' @export
#' @rdname positions
#'
#' @examples
#' # Simple upriver movement
#' positions(10, list(rates = 50, distances = 1000))
#' # Movement with reach-to-reach variation
#' positions(10,
#'           list(rates = c(1, 2, 3, 4, 5),
#'           distances = c(20, 20, 20, 20, 20)))
positions <- function(ndays, parameters) {
  ndays <- as.numeric(ndays)
  stopifnot("rates" %in% names(parameters))
  stopifnot("distances" %in% names(parameters))

  # Expand parameters to add 0th and n+1th elements
  parameters <- expand_parameters(parameters)
  rates <- parameters$rates
  distances <- parameters$distances

  stopifnot(length(rates) == length(distances))

  # Store distances
  result <- rep(NA, length(ndays))

  # Pre-calculate t and tc
  t <- distances / rates
  tc <- cumsum(t)

  for (i in seq_along(ndays)) {
    day <- ndays[i]

    if (day < 0) next()

    spent <- rep(0, length(rates))
    spent[c(which(as.numeric(day >= tc) == 1))] <- 1
    which_reach <- length(which(spent == 1)) + 1
    spent[which_reach] <- (day - tc[which_reach - 1]) / t[which_reach]
    distance <- sum(spent * distances)

    result[i] <- distance
  }

  result
}

#' Calculate run timings
#'
#' Calculates run timing given a set of parameters.
#'
#' \code{arrival} should be a \code{data.frame} describing a set of daily
#' arrivals of fish at \code{arrival_position} as a series of daily proportions.
#' It should have a column \code{day} and a corresponding column
#' \code{proportion}, which should sum to one.
#'
#' @param location (numeric) The location at which to calculate to run timing.
#' @param arrival (data.frame) A data.frame of daily arrivals by proportion.
#' See details.
#' @param parameters (list) See \code{\link{positions}} for details.
#' @param arrival_position (numeric) Defaults 0. Where to start movement.
#'
#' @export
#' @rdname timings
#'
#' @examples
#' timings(1000,
#'         data.frame(day=0:40, proportion = dnorm(-20:20, 0, 5)/sum(dnorm(-20:20, 0, 5))),
#'         list(rates = 50, distances = 1000))
timings <- function(location, arrival, parameters, arrival_position = 0) {
  stopifnot(is.numeric(arrival_position))

  parameters <- expand_parameters(parameters)
  result <- rep(NA, nrow(arrival))

  for (i in seq_along(arrival$day)) {
    d <- arrival[i,"day"][[1]]
    max_day <- round(
      cumsum(parameters$distances /
               parameters$rates)[length(parameters$rates) - 1] + d + 20)
    pos <- rep(NA, max_day)

    for (j in seq_len(max_day)) {
      pos[j] <- arrival_position + positions(j - d, parameters)
    }

    result[i] <- which(pos >= location)[1]
  }

  result
}

#' Calculate median timing
#'
#' Calculate median timing given a set of parameters
#'
#' #' \code{arrival} should be a \code{data.frame} describing a set of daily
#' arrivals of fish at \code{arrival_position} as a series of daily proportions.
#' It should have a column \code{day} and a corresponding column
#' \code{proportion}, which should sum to one.
#'
#' @param location (numeric) The location at which you want to calculate median
#' timing.
#' @param arrival (data.frame) A data.frame of daily arrivals by proportion.
#' See details.
#' @param parameters (list) See \code{\link{positions}} for details.
#' @param arrival_position (numeric) Defaults 0. Where to start movement.
#'
#' @export
#' @rdname median_timing
#' @examples
#' median_timing(1000,
#'               data.frame(day=0:40, proportion=dnorm(-20:20, 0, 5)/sum(dnorm(-20:20, 0, 5))),
#'               list(rates = 50, distances = 1000))
median_timing <- function(location, arrival, parameters, arrival_position = 0) {
  stopifnot(location >= 0)
  stopifnot(length(arrival) > 0)
  stopifnot(c("day", "proportion") %in% names(arrival))
  stopifnot(abs(1 - sum(arrival$proportion)) < 0.01) # Allow for a fudge factor

  parameters <- expand_parameters(parameters)

  # Calculate an upper-bound on the time this could take
  max_time <- round(
    cumsum(parameters$distances /
             parameters$rates)[length(parameters$rates) - 1] +
      arrival[nrow(arrival),"day"][[1]])

  d <- NA
  p <- NA

  for (d in seq_len(max_time)) {
    # Calculate positions
    pos <- arrival_position + positions(d - arrival$day, parameters)

    # Calculate the proportion of the run beyond or at location
    p <- sum(arrival[which(pos >= location),"proportion"])

    if (p >= 0.5) {
      break
    }
  }

  # Calculate the first date where the proportion at or above location is
  # greater than or equal to 0.5 (median timing)
  d
}

#' Calculate an arbitrary timing percentile
#'
#' Calculate an arbitrary timing percentile given a set of parameters
#'
#' #' \code{arrival} should be a \code{data.frame} describing a set of daily
#' arrivals of fish at \code{arrival_position} as a series of daily proportions.
#' It should have a column \code{day} and a corresponding column
#' \code{proportion}, which should sum to one.
#'
#' @param percentile (numeric) An arbitrary timing percentile (between 0 and 1)
#' @param location (numeric) The location at which you want to calculate median
#' timing.
#' @param arrival (data.frame) A data.frame of daily arrivals by proportion.
#' See details.
#' @param parameters (list) See \code{\link{positions}} for details.
#' @param arrival_position (numeric) Defaults 0. Where to start movement.
#'
#' @export
#' @rdname percentile_timing
#' @examples
#' percentile_timing(0.75,
#'                   1000,
#'                   data.frame(day=0:40, proportion=dnorm(-20:20, 0, 5)/sum(dnorm(-20:20, 0, 5))),
#'                   list(rates = 50, distances = 1000))
percentile_timing <- function(percentile,
                              location,
                              arrival,
                              parameters,
                              arrival_position = 0) {
  stopifnot(percentile < 1 && percentile > 0)
  stopifnot(location >= 0)
  stopifnot(length(arrival) > 0)
  stopifnot(c("day", "proportion") %in% names(arrival))
  stopifnot(sum(arrival$proportion) == 1)

  parameters <- expand_parameters(parameters)

  # Calculate an upper-bound on the time this could take
  max_time <- round(
    cumsum(parameters$distances /
             parameters$rates)[length(parameters$rates) - 1] +
      arrival[nrow(arrival),"day"][[1]])

  d <- NA
  p <- NA

  for (d in seq_len(max_time)) {
    # Calculate positions
    pos <- arrival_position + positions(d - arrival$day, parameters)

    # Calculate the proportion of the run beyond or at location
    p <- sum(arrival[which(pos >= location),"proportion"])

    if (p >= percentile) {
      break
    }
  }

  # Calculate the first date where the proportion at or above location is
  # greater than or equal to 0.5 (median timing)
  d
}

#' Expand movement parameters
#'
#' Expands the movement parameters to include a 0th and n+1th reach.
#'
#' @param parameters (list) Movement parameters (names: \code{rates},
#' \code{distances})
#'
#' @return (list) Expanded parameters
expand_parameters <- function(parameters) {
  if (!is.null(attr(parameters, "expanded", TRUE))) { return(parameters) }

  stopifnot("rates" %in% names(parameters))
  stopifnot("distances" %in% names(parameters))

  parameters$rates <- c(1,
                        parameters$rates,
                        parameters$rates[length(parameters$rates)])
  parameters$distances <- c(0,
                            parameters$distances,
                            1e6)

  # Set a flag on the list so we don't doubly-expand
  attr(parameters, "expanded") <- TRUE

  parameters
}
