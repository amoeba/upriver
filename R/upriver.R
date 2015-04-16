#' @export
#' @rdname positions

positions <- function(days, parameters) {
  stopifnot("reaches" %in% names(parameters))
  stopifnot("rates" %in% names(parameters))
  stopifnot("distances" %in% names(parameters))

  reaches <- parameters$reaches
  rates <- parameters$rates
  distances <- parameters$distances

  stopifnot(length(reaches) == length(rates))
  stopifnot(length(reaches) == length(distances))
  stopifnot(as.numeric(days) == days)

  # Store distances
  result <- rep(NA, length(days))

  for(i in 1:length(days))
  {
    day <- days[i]

    if(day < 0) {
      next()
    }

    t <- distances / rates
    tc <- cumsum(t)

    spent <- rep(0, length(reaches))
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

  for(i in 1:length(arrival$day)) {
    d <- arrival[i,"day"][[1]]
    max_day <- round(cumsum(parameters$distances / parameters$rates)[length(parameters$rates) - 1] + d + 20)
    pos <- rep(NA, max_day)

    for(j in 1:max_day) {
      pos[j] <- start_position + positions(j - d, parameters)
    }

    result[i] <- which(pos >= location)[1]
  }

  result
}


#' @export
#' @rdname median_timing

median_timing <- function(location, arrival, parameters, start_position = 0) {

# Debug
#   location <- 250
#   arrival <- data.frame(day = 1:10, proportions = c(0, 0, 0.1, 0.2, 0.2, 0.2, 0.2, 0.1, 0, 0))
#   parameters <- params_simple

  stopifnot(location >= 0)
  stopifnot(length(arrival) > 0)
  stopifnot(c("day", "proportion") %in% names(arrival))
  stopifnot(sum(arrival$proportion) == 1)

  # Calculate an upper-bound on the time this could take
  max_time <- round(cumsum(parameters$distances / parameters$rates)[length(parameters$rates) - 1] + arrival[nrow(arrival),"day"][[1]])

  d <- NA
  p <- NA

  for(d in 1:max_time) {
    # Calculate positions
    pos <- start_position + positions(d - arrival$day, parameters)

    # Calculate the proportion of the run beyond or at location
    p <- arrival[which(pos >= location),] %>%
      select(proportion) %>%
      summarise(sum(proportion))

    if(p >= 0.5) {
      break
    }
  }

  # Calculate the first date where the proportion at or above location is
  # greater than or equal to 0.5 (median timing)
  d
}
