#' Ask a Magic 8 Ball a question in the console
#'
#' @param question A character string.
#'   Ask the Magic 8 Ball anything your heart desires.
#'   If [`NULL`], the default, you will be prompted to type a question
#'   interactively in the console.
#' @param header If `TRUE`, prints a header for the game.
#'   Defaults to `TRUE` if the session is [interactive] and `FALSE` otherwise.
#'
#' @return A character string answering your `question`.
#' @export
#'
#' @examples
#' play_magic_8_ball("Will I ever find love?")
#'
#' if (rlang::is_interactive()) play_magic_8_ball()
play_magic_8_ball <- function(question = NULL, header = rlang::is_interactive()) {
  h1("\U1F3B1 Magic 8 Ball \U1F3B1")

  if (is.null(question) && rlang::is_interactive()) {
    cat_tnl("What is your question?")
    input("Question: ")
  } else if (!is.null(question)) {
    cat_tnl("Question: ", question)
  }

  response <- magic_8_ball()

  if (rlang::is_interactive()) {
    selection <- input(
      cli::col_grey("Press [ENTER] to ask another question or [ESC] to quit. ")
    )
    cat0("\b\b\r", strrep(" ", 46), strrep(" ", stringr::str_width(selection)))
    cat0(strrep("\b", 46), strrep("\b", stringr::str_width(selection)))

    play_magic_8_ball(header = FALSE)
  } else {
    invisible(response)
  }
}

magic_8_ball <- function() {
  triangles <- cli::col_magenta(c("\u25b2", "\u25b6", "\u25bc", "\u25c4"))

  response <- sample(magic_8_ball_responses, 1)

  steps <- seq(
    from = sample(1:4, 1),
    length.out = stringr::str_width(response)
  )
  steps <- steps * sample(c(1, -1), 1)

  for (i in seq_along(steps)) {
    message <- paste(triangles[steps[seq(1, i)] %% 4 + 1], collapse = "")
    cat_over(message)
    Sys.sleep(stats::runif(1, 0.05, 0.2))
  }

  cat_over(cli::col_magenta(response))

  invisible(response)
}

magic_8_ball_responses <- c(
  "It is certain.",
  "It is decidedly so.",
  "Without a doubt.",
  "Yes definitely.",
  "You may rely on it.",
  "As I see it, yes.",
  "Most likely.",
  "Outlook good.",
  "Yes.",
  "Signs point to yes.",
  "Reply hazy, try again.",
  "Ask again later.",
  "Better not tell you now.",
  "Cannot predict now.",
  "Concentrate and ask again.",
  "Don't count on it.",
  "My reply is no.",
  "My sources say no.",
  "Outlook not so good.",
  "Very doubtful."
)
