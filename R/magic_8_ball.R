#' Ask a Magic 8 Ball a question in the console
#'
#' @param question A character string.
#'  Ask the Magic 8 Ball anything your heart desires.
#'  If [`NULL`], the default, you will be prompted to type a question
#'  interactively in the console.
#'
#' @return A character string answering your `question`.
#' @export
#'
#' @examples
#' play_magic_8_ball("Will I ever find love?")
play_magic_8_ball <- function(question = NULL) {
  h1("\U1F3B1 Magic 8 Ball \U1F3B1")

  if (is.null(question)) {
    cli::cli_text("What is your question?")
    input("Question: ")
  } else {
    cli::cli_text("Question: {question}")
  }

  magic_8_ball()
}

magic_8_ball <- function() {
  withr::local_options("cli.progress_show_after" = 0)

  message <- " "
  cli::cli_progress_message("{message}")
  triangles <- cli::col_magenta(c("\u25b2", "\u25b6", "\u25bc", "\u25c4"))

  steps <- seq(from = sample(1:4, 1), length.out = sample(8:12, 1)) *
    sample(c(1, -1), 1)

  for (i in seq_along(steps)) {
    Sys.sleep(stats::runif(1, 0.1, 0.5))
    message <- paste(triangles[steps[seq(1, i)] %% 4], collapse = "")
    cli::cli_progress_update()
  }

  Sys.sleep(0.5)

  response <- sample(magic_8_ball_responses, 1)

  cli::cli_text(cli::col_magenta(response))

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
