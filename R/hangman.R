#' Play a game of hangman in the console
#'
#' @inheritParams play_jumble
#'
#' @source The default word list is derived from
#' [12dicts](http://wordlist.aspell.net/12dicts/) created by
#' [Alan Beale](http://www.wyrdplay.org/).
#'
#' This word list is passed through the
#' [Offensive/Profane Word List](https://www.cs.cmu.edu/~biglou/resources/)
#' created by [Luis Von Ahn](https://www.cs.cmu.edu/~biglou/) to filter out
#' potentially unwanted words.
#'
#' @details # Problematic words?
#' You can report any words you find objectionable to
#' \url{https://github.com/rossellhayes/hangman/issues}.
#'
#' Please report:
#' * offensive words
#' * words that deal with uncomfortable topics
#' * proper nouns
#' * text that is not a common English word
#'
#' @return Generates interactive output in the console.
#' @aliases hangman
#' @export
#' @importFrom R6 R6Class
#'
#' @examples
#' play_hangman()
#'
#' play_hangman("beginner")
#' play_hangman("easy")
#' play_hangman("medium")
#' play_hangman("hard")
#' play_hangman("expert")
#'
#' cars <- unique(gsub(" .*", "", rownames(mtcars)))
#' play_hangman(word_list = cars)

play_hangman <- function(
  difficulty = c("beginner", "easy", "medium", "hard", "expert"),
  word_list = NULL
) {
  if (!rlang::is_interactive()) {
    return(invisible(NULL))
  }

  if (
    !is.null(game_env$hangman) &&
    !game_env$hangman$game_over &&
    (
      identical(word_list, game_env$hangman$word_list) ||
      (is.null(word_list) && difficulty == game_env$hangman$difficulty)
    )
  ) {
    return(resume("hangman"))
  }

  if (!is.null(word_list)) {word_list <- unique(word_list)}

  game_env$hangman <- Hangman$new(difficulty, word_list)
}

Hangman <- R6::R6Class(
  "Hangman",
  public = list(
    difficulties = c("Beginner", "Easy", "Medium", "Hard", "Expert"),

    initialize = function(difficulty = self$difficulties, word_list = NULL) {
      if (!is.null(word_list)) {
        word_list      <- gsub("[^a-z]", "", tolower(word_list))
        self$word_list <- word_list[nchar(word_list) != 0]
      }

      if (length(self$word_list) == 0) {
        if (
          length(difficulty) != 1 ||
          !(tolower(difficulty) %in% tolower(self$difficulties))
        ) {
          clear_console()
          h1("\U1FAA2 Hangman \U1FAA2")

          difficulty <- choose_menu(
            c(self$difficulties, "Quit"),
            title = "Choose a difficulty:"
          )

          if (difficulty == "Quit") {
            quit_game()
          }
        }

        difficulty <- tolower(difficulty)
        self$difficulty <- tolower(difficulty)

        classes <- switch(
          difficulty,
          "beginner" = 1:9,
          "easy"     = 4:12,
          "medium"   = 7:15,
          "hard"     = 10:18,
          "expert"   = 13:21
        )

        self$word_list <- jumble_wordlist[jumble_wordlist$class %in% classes, ]
        self$word_list <- self$word_list$lemma
        self$word_list <- self$word_list[nchar(self$word_list) >= 4]
      }

      self$word_list <- self$word_list[
        !self$word_list %in% c("help", "restart", "quit", "surrender")
      ]

      private$refresh()
    },

    play = function() {
      self$print()
    },

    print = function() {
      if (!rlang::is_interactive()) {
        return(invisible(NULL))
      }

      clear_console()
      h1("\U1FAA2 Hangman \U1FAA2")
      cat_tnl(
        cli::col_grey('Type "quit" to exit. Type "help" for commands.')
      )

      switch(
        private$n_wrong,
        `1` = {private$body[2] <- private$head},
        `2` = {private$body[3] <- private$torso},
        `3` = {private$body[3] <- private$one_arm},
        `4` = {private$body[3] <- private$two_arm},
        `5` = {private$body[4] <- private$one_leg},
        `6` = {private$body[4] <- private$two_leg}
      )

      if (private$n_wrong > 0) {
        private$board[1] <- paste(
          c("\u250c", rep("\u2500", private$n_wrong), "\u2510"), collapse = ""
        )
        private$board[2] <- paste(
          c("\u2502", private$wrong, "\u2502"), collapse = ""
        )
        private$board[3] <- paste(
          c("\u2514", rep("\u2500", private$n_wrong), "\u2518"), collapse = ""
        )
      }

      private$board[5] <- paste(
        rep("\u203e", length(private$word)),
        collapse = " "
      )

      display <- private$word
      display[!private$word %in% private$guesses] <- " "
      private$board[4] <- paste(display, collapse = " ")

      cat(
        paste(
          paste0(private$gallows, private$body, private$board),
          collapse = "\n"
        )
      )

      cat("\n")

      if (private$surrendered) {
        cat("You surrendered.")
        private$ask_restart()
      } else if (private$n_wrong >= 6) {
        cat(paste0('The word was "', paste(private$word, collapse = ''), '".'))
        private$ask_restart()
      } else if (any(!private$word %in% private$guesses)) {
        private$guess()
      } else {
        cat("You win!")
        private$ask_restart()
      }
    },

    difficulty = NULL,
    word_list  = NULL,
    game_over  = FALSE
  ),
  private = list(
    word        = NULL,
    letters     = NULL,
    guesses     = NULL,
    wrong       = NULL,
    n_wrong     = 0,
    body        = NULL,
    surrendered = FALSE,

    guess = function() {
      cat(crayon::silver("Guess a letter."))
      letter <- gsub(" ", "", input())

      if (grepl("help", letter)) {
        cat(
          'Type "restart" for a new word,',
          '"surrender" to reveal the current word,',
          'or "quit" to exit.',
          sep = "\n"
        )
        return(private$guess())
      }

      if (grepl("restart", letter)) {return(private$refresh())}
      if (grepl("quit", letter))    {quit_game()}

      if (grepl("surrender", letter)) {
        private$surrendered <- TRUE
        private$guesses     <- c(private$guesses, private$letters)
        return(self$print())
      }

      if (nchar(letter) != 1 || !grepl("[a-z]", letter)) {
        cat(crayon::silver("I couldn't understand that input. "))
        return(private$guess())
      }

      private$guesses <- unique(c(private$guesses, letter))
      private$wrong   <- private$guesses[!private$guesses %in% private$letters]
      private$n_wrong <- length(private$wrong)

      return(self$print())
    },

    refresh = function() {
      private$word        <- strsplit(sample(self$word_list, 1), "")[[1]]
      private$letters     <- unique(private$word)
      private$body        <- private$air
      private$board       <- rep("", 5)
      private$guesses     <- NULL
      private$wrong       <- NULL
      private$n_wrong     <- 0
      private$surrendered <- FALSE
      self$game_over      <- FALSE

      self$print()
    },

    ask_restart = function() {
      self$game_over <- TRUE

      cat(' Press ENTER to try again or type "quit" to exit.')

      if (grepl("quit", input())) {
        quit_game()
      } else {
        private$refresh()
      }
    },

    gallows = c(
      "\u250c\u2500\u2500\u2510",
      "\u2502",
      "\u2502",
      "\u2502",
      "\u2534"
    ),

    air = c("  ", rep(strrep(" ", 5), 4)),
    board = rep("", 5),

    head    = "  O  ",
    torso   = "  |  ",
    one_arm = "  |\\ ",
    two_arm = " /|\\ ",
    one_leg = "   \\ ",
    two_leg = " / \\ "
  )
)
