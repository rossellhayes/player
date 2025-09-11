#' Flip a coin
#'
#' @param n How many coins to flip.
#'   Defaults to `NULL`, which interactively asks how many to flip.
#' @param animate If `TRUE`, play an animation before revealing the result.
#'   Defaults to `TRUE` if the session is [interactive] and `FALSE` otherwise.
#' @export
#'
#' @examples
#' play_coin_flip(1)
#'
#' if (rlang::is_interactive()) play_coin_flip()
play_coin_flip <- function(n = NULL, animate = rlang::is_interactive()) {
  repeat {
    n <- suppressWarnings(as.numeric(n))

    if (rlang::is_integerish(n) && length(n) == 1) break

    if (!rlang::is_interactive()) {
      cli::cli_abort("{.arg n} must be a single integer.")
    }

    n <- input("How many coins do you want to flip? ")
    cat("\b\b\r                                     ")
    if (nzchar(n)) cat(strrep(" ", stringr::str_width(n)))
    cat("\n")
  }

  flip_coin(n = n, animate = animate)

  if (rlang::is_interactive()) {
    selection <- input("Press ENTER to play again or ESC to quit. ")
    cat("\b\b\r                                          ")

    play_coin_flip(n = n, animate = animate)
  }
}

flip_coin <- function(n = 1, animate = TRUE) {
  n <- suppressWarnings(as.numeric(n))

  if (!rlang::is_integerish(n) || length(n) != 1) {
    cli::cli_abort("{.arg n} must be a single integer.")
  }

  result <- sample(c("heads", "tails"), n, replace = TRUE)

  coins <- character(length(result))
  coins[result == "heads"] <- "Ⓗ"
  coins[result == "tails"] <- "Ⓣ"

  if (isTRUE(animate)) {
    coin_flip_animation(coins)
  } else {
    cat(coins, "\n")
  }

  if (length(result) > 1) {
    if (length(unique(result)) == 1) {
      cat(length(result), stringr::str_to_title(unique(result)))
    } else {
      heads <- sum(result == "heads")
      tails <- sum(result == "tails")
      cat(heads, "Heads,", tails, "Tails")
    }
  } else {
    cat0(stringr::str_to_title(result))
  }

  invisible(result)
}

coin_flip_animation <- function(coins) {
  n <- length(coins)

  cat(strrep("  ", n), strrep("  ", n), strrep("⬭ ", n), sep = "\n")
  flush.console()
  Sys.sleep(0.1)

  cat(strrep("\b", n * 6 + 3))
  cat(strrep("  ", n), strrep("— ", n), strrep("  ", n), sep = "\n")
  flush.console()
  Sys.sleep(0.1)

  cat(strrep("\b", n * 6 + 3))
  cat(strrep("⬭ ", n), strrep("  ", n), strrep("  ", n), sep = "\n")
  flush.console()
  Sys.sleep(0.1)

  cat(strrep("\b", n * 6 + 3))
  cat(strrep("  ", n), strrep("— ", n), strrep("  ", n), sep = "\n")
  Sys.sleep(0.1)

  cat(strrep("\b", n * 6 + 3))
  cat(strrep("  ", n), strrep("  ", n), paste(coins, collapse = " "), sep = "\n")
  flush.console()
}
