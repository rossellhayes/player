game_env <- new.env()

#' Play a game in the console
#'
#' Interact with the game by typing commands into the console and
#' pressing enter.
#'
#' @param game The name of a game to play.
#'   If `NULL`, you can choose a game interactively in the console.
#' @param ... Additional arguments passed to the chosen game.
#'
#' @return Calls the selected game function.
#' @export
#'
#' @examples
#' play()
#'
#' play("poker")
#' play("hangman")
#' play("jumble")
#' play("2048")
#'
#' play("poker", colors = 4)
#' play("hangman", difficulty = "easy")
#' play("jumble", difficulty = "hard")
#' play("2048", size = 5)

play <- function(game = NULL, ...) {
  if (!rlang::is_interactive() || length(game) == 1) {
    game <- rlang::arg_match(game, game_list)
  } else {
    game_list <- c(game_list, "Quit" = "quit")
    game <- choose_menu(game_list, title = "Choose a game:")
  }

  game <- to_lower_snake_case(game)

  if (game == "quit") {
    return(invisible())
  }

  get(paste0("play_", game), envir = asNamespace("play"))(...)
}

resume <- function(game) {
  if (is.null(game_env[[game]])) {
    stop("No ongoing ", game, " game to resume.", call. = FALSE)
  }

  game_env[[game]]$play()
}

ask_resume <- function(game) {
  if (is.null(game_env[[game]]) || game_env[[game]]$game_over) {
    return(FALSE)
  }

  cat("There is an ongoing", game, "game.", "Do you want to resume? (y/n)")

  response <- substr(input(), 1, 1)

  while (TRUE) {
    switch(
      response,
      y = return(TRUE),
      n = return(FALSE),
      {response <- invalid_response()}
    )
  }
}

invalid_response <- function() {
  cli::cli_text(
    'I didn\'t understand that input. Please type "y" or "n" or press ESC.'
  )
  tolower(substr(input("> "), 1, 1))
}

quit_game <- function() {
  do.call("return", list(clear_console()), envir = sys.frame(1))
}

to_lower_snake_case <- function(x) {
  tolower(gsub(" ", "_", x))
}
