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
  if (!is.null(title)) cli::cli_text(title)

  if (!rlang::is_named(options)) names(options) <- options

  cli::cli_ol(names(options))
  cli::cat_line()

  repeat {
    input <- input("Selection: ")

    if (input %in% seq_along(options)) {
      return(options[[as.numeric(input)]])
    }

    fuzzy_match <- grep(input, options, ignore.case = TRUE)
    if (length(fuzzy_match) == 1) {
      return(options[fuzzy_match])
    }

    cli::cli_text("I didn't understand that input.")
    cli::cli_text(
      "Please enter a digit ({.or {seq_along(options)}})
    or a command ({.or {options}})."
    )
  }
}

map_chr <- function(.x, .f, ...) {
  vapply(.x, .f, character(1), ...)
}

cat0 <- function(...) cat(..., sep = "")
cat_line <- function(...) cat0("\n", ...)
cat_over <- function(...) cat0("\r", ...)

h1 <- function(text) {
  text_width <- crayon::col_nchar(text)
  console_width <- cli::console_width()

  if (console_width < text_width) {
    return(cat(crayon::magenta(text), "\n"))
  }

  cat(
    crayon::cyan("──"),
    crayon::magenta(text),
    crayon::cyan(strrep("─", console_width - text_width - 4)),
    "\n"
  )
}
