#' @title predict_the_future
#'
#' @param f a function that takes a row-wise subset of \code{data} and returns
#' a scalar prediction
#' @param data a data.frame used by \code{f} to make predictions.  Must contain
#' a column named \code{t} which indicates the time of each observation.
#' @param start_at_nth start running the predictions on the \code{n}th
#' observation. Useful when predictions on fewer than \code{n} observations
#' are known to be unreliable.
#' @param ... additional parameters to \code{f}
#'
#' @return a vector of predictions
#' @export
#'
#' @examples
#' predict_the_future(f = function(t, x) base::mean(x),
#'                    data = data.frame(t = 1:6, x = 1.3^(1:6)))
#'
predict_the_future <- function(f, data, start_at_nth = 1, ...) {

  # validate f
  if (!is.function(f))
    rlang::abort(pasten("`f` must be a function.",
                        paste("You've supplied a", class(f))))

  # validate data
  if (is.null(data$t))
    rlang::abort("`data`` must have a column `t`")
  if (any(duplicated(data$t)))
    rlang::abort("`data` must not have ")
  if (any(data$t != cummax(data$t)))
    rlang::abort("`data` must be ordered by `t`")

  # validate start point
  if (any(start_at_nth < 1, start_at_nth > nrow(data)))
    rlang::abort("`start_at_nth` must be between `1` and `nrow(data)`")


  n <- nrow(data)
  preds <- rep(NA, n)

  for(i in start_at_nth:n) {
    preds[i] <- do.call(what = f,
                        args = data[1:i,])
  }

  return(preds)

}
