#' @export
#' @rdname positions

positions <- function(days, parameters) {
  stopifnot("reaches" %in% names(parameters))
#' positions(10, list(rates = c(1, 2, 3, 4, 5), distances = c(20, 20, 20, 20, 20)))
positions <- function(ndays, parameters) {
  ndays <- as.numeric(ndays)
  stopifnot("rates" %in% names(parameters))
  stopifnot("distances" %in% names(parameters))

  rates <- parameters$rates
  distances <- parameters$distances

  # Add a 0'th and N+1'th reach
  rates <- c(1, rates, rates[length(rates)])
  distances <- c(0, distances, 1e6)

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


#' @export
#' @rdname timings

timings <- function(location, arrival, parameters, start_position = 0) {
  stopifnot(is.numeric(start_position))

  result <- rep(NA, nrow(arrival))

  for (i in seq_along(arrival$day)) {
    d <- arrival[i,"day"][[1]]
    max_day <- round(cumsum(parameters$distances / parameters$rates)[length(parameters$rates) - 1] + d + 20)
    pos <- rep(NA, max_day)

    for (j in seq_len(max_day)) {
      pos[j] <- start_position + positions(j - d, parameters)
    }

    result[i] <- which(pos >= location)[1]
  }

  result
}


#' @export
#' @rdname median_timing

median_timing <- function(location, arrival, parameters, start_position = 0) {
  stopifnot(location >= 0)
  stopifnot(length(arrival) > 0)
  stopifnot(c("day", "proportion") %in% names(arrival))
  stopifnot(abs(1 - sum(arrival$proportion)) < 0.01) # Allow for a fudge factor

  # Calculate an upper-bound on the time this could take
  max_time <- round(cumsum(parameters$distances / parameters$rates)[length(parameters$rates) - 1] + arrival[nrow(arrival),"day"][[1]])

  d <- NA
  p <- NA

  for (d in seq_len(max_time)) {
    # Calculate positions
    pos <- start_position + positions(d - arrival$day, parameters)

    # Calculate the proportion of the run beyond or at location
    p <- arrival[which(pos >= location),] %>%
      select(proportion) %>%
      summarise(sum(proportion))

    if (p >= 0.5) {
      break
    }
  }

  # Calculate the first date where the proportion at or above location is
  # greater than or equal to 0.5 (median timing)
  d
}

#' @export
#' @rdname percentile_timing
percentile_timing <- function(percentile, location, arrival, parameters, start_position = 0) {
  stopifnot(percentile < 1 && percentile > 0)
  stopifnot(location >= 0)
  stopifnot(length(arrival) > 0)
  stopifnot(c("day", "proportion") %in% names(arrival))
  stopifnot(sum(arrival$proportion) == 1)

  # Calculate an upper-bound on the time this could take
  max_time <- round(cumsum(parameters$distances / parameters$rates)[length(parameters$rates) - 1] + arrival[nrow(arrival),"day"][[1]])

  d <- NA
  p <- NA

  for (d in seq_len(max_time)) {
    # Calculate positions
    pos <- start_position + positions(d - arrival$day, parameters)

    # Calculate the proportion of the run beyond or at location
    p <- arrival[which(pos >= location),] %>%
      select(proportion) %>%
      summarise(sum(proportion))

    if (p >= percentile) {
      break
    }
  }

  # Calculate the first date where the proportion at or above location is
  # greater than or equal to 0.5 (median timing)
  d
}
