clear_console <- function() {
  if (rstudioapi::isAvailable()) {
    # Within the RStudio console, "\f" clears output
    return(cat("\f"))
  }

  # In the terminal, "\033c\033[3J" clears output
  cat("\033c\033[3J")
}

input <- function(prompt = "> ", transform = tolower) {
  if (rlang::is_installed("rstudioapi") && rstudioapi::isAvailable()) {
    old <- rstudioapi::readRStudioPreference("console_code_completion", TRUE)
    on.exit(rstudioapi::writeRStudioPreference("console_code_completion", old))
    rstudioapi::writeRStudioPreference("console_code_completion", FALSE)
  }

  transform(readline(prompt = prompt))
}

choose_menu <- function(options, title = NULL) {
  if (!is.null(title)) cat_tnl(title)

  if (!rlang::is_named(options)) names(options) <- options

  numbers <- cli::col_grey(paste0(seq_along(options), "."))
  numbers <- stringr::str_pad(numbers, max(stringr::str_width(numbers)), "left")
  cat_tnl(paste(" ", numbers, names(options), collapse = "\n"))
  cat_blank_line()

  repeat {
    input <- input("Selection: ")

    if (input %in% seq_along(options)) {
      return(options[[as.numeric(input)]])
    }

    fuzzy_match <- grep(input, options, ignore.case = TRUE)
    if (length(fuzzy_match) == 1) {
      return(options[fuzzy_match])
    }

    cat_tnl("I didn't understand that input.")
    cat_tnl(
      stringr::str_wrap(
        glue::glue(
          "Please enter a digit ({and::or(seq_along(options))})
          or a command ({and::or(options)})."
        ),
        width = cli::console_width()
      )
    )
  }
}

map_chr <- function(.x, .f, ...) {
  vapply(.x, .f, character(1), ...)
}

#' @importFrom and or

cat0 <- function(...) cat(..., sep = "")
cat_pnl <- function(...) cat0("\n", ...)
cat_tnl <- function(...) cat0(..., "\n")
cat_ptnl <- function(...) cat0("\n", ..., "\n")
cat_over <- function(..., sep = "") cat("\r", paste(..., sep = sep), sep = "")
cat_over_tnl <- function(...) cat0("\r", ..., "\n")
cat_blank_line <- function() cat_tnl()

h1 <- function(...) cli::cat_rule(cli::col_magenta(...), col = "cyan")
h1_center <- function(...) {
  cat_tnl(cli::rule(center = cli::col_magenta(...), col = "cyan"))
}
h3 <- function(...) cat_ptnl(cli::col_grey("\u2500\u2500 "), ...)
