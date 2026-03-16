#' Calculate the number of cores to use, which is limited by both the number
#' available, and the number of replications required
#'
#' @param replications integer. Number of simulation replications required
#' @param cores integer. Number of processor cores available
#'
#' @returns integer. The number of cores to use (1 minimum)
#'
processor_cores_required <- function(replications, cores = parallel::detectCores()) {
  limit_to_replications <- max(replications, 1)

  limit_to_cores <- min(cores - 1, replications)

  max(limit_to_cores, 1) # we need at least one core!
}
