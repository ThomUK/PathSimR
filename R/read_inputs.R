#' Read and prepare a PathSimR network template CSV
#'
#' Reads a network template CSV file and returns the inputs in the form
#' expected by [run_simulation()].
#'
#' @param path Character. Path to the network template CSV file.
#'
#' @returns A list with three elements:
#'   \describe{
#'     \item{var_input}{data.frame. Node parameters and transition
#'       probabilities, with numeric row and column indices.}
#'     \item{syst_names}{matrix. Two-column matrix of (numeric index,
#'       node name) for all nodes including exits.}
#'     \item{syst_names_single}{character vector. Node names only
#'       (column 2 of syst_names).}
#'   }
#' @export
read_network_template <- function(path) {
  raw <- read.csv(path, header = TRUE)

  syst_names <- cbind(
    as.numeric(seq_len(nrow(raw))),
    as.character(raw[, 1])
  )
  syst_names_single <- syst_names[, 2]

  var_input <- raw[, -1]
  rownames(var_input) <- seq_len(nrow(var_input))
  colnames(var_input)[seq_len(nrow(var_input))] <- seq_len(nrow(var_input))

  list(
    var_input         = var_input,
    syst_names        = syst_names,
    syst_names_single = syst_names_single
  )
}


#' Read and prepare a PathSimR calendar template CSV
#'
#' Reads a calendar template CSV file and returns the input data.frame
#' expected by [run_simulation()].
#'
#' @param path Character. Path to the calendar template CSV file.
#'
#' @returns A data.frame with columns: metric, node, start, end, value.
#' @export
read_calendar_template <- function(path) {
  read.csv(path, header = TRUE) |>
    transform(node = as.character(node))
}
