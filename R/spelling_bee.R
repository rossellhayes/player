#' Play a game of Spelling Bee in the console
#'
#' Inspired by the [New York Times Spelling Bee](https://www.nytimes.com/puzzles/spelling-bee),
#' created by Sam Ezersky.
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
#' @aliases spelling_bee
#' @export
#' @importFrom R6 R6Class
#'
#' @examples
#' play_spelling_bee()

play_spelling_bee <- function() {
  if (!rlang::is_interactive()) {
    return(invisible(NULL))
  }

  if (!is.null(game_env$spelling_bee) && !game_env$spelling_bee$game_over) {
    return(resume("spelling_bee"))
  }

  game_env$spelling_bee <- Spelling_Bee$new()
  game_env$spelling_bee$play()
}

Spelling_Bee <- R6::R6Class(
  "Spelling_Bee",
  public = list(
    initialize = function() {
      private$build_grid()
    },

    play = function() {
      self$print()
      private$guess()
    },

    print = function() {
      if (!rlang::is_interactive()) {
        return(invisible(NULL))
      }

      clear_console()

      cli::cli_h1("\U1F41D Spelling Bee \U1F41D")
      cli::cli_text(
        cli::col_grey(
          'Type "/quit" to exit,
          "/shuffle" to rearrange letters,
          and "/restart" to start a new game.'
        )
      )
      cli::cat_line("Score: ", private$score, " (", private$rank, ")")
      cli::cat_line()
      cli::cat_line("  ", private$letters[[1]], "   ", private$letters[[2]])
      cli::cat_line()
      cli::cat_line(
        private$letters[[3]], "  ",
        cli::bg_yellow(" ", private$center, " "), "  ",
        private$letters[[4]]
      )
      cli::cat_line()
      cli::cat_line("  ", private$letters[[5]], "   ", private$letters[[6]])
      cli::cat_line()

      if (!is.null(private$answers)) {
        cat(
          format(private$answers, width = max(nchar(private$possibilities$word))),
          "\n",
          fill = TRUE
        )
      }

      if (!is.null(private$score_plus)) {
        cli::cat_line("+", private$score_plus, " points!")
      }

      private$guess()
    },

    game_over  = FALSE
  ),
  private = list(
    center = NULL,
    letters = NULL,
    possibilities = NULL,
    answers = NULL,
    score = 0,
    score_plus = NULL,
    rank = "Beginner",

    guess = function() {
      answer <- gsub(" ", "", input("Word: "))

      switch(
        answer,
        "/quit" = return(quit_game()),
        "/shuffle" = return(private$shuffle()),
        "/restart" = return(private$refresh())
      )

      if (grepl("[^a-z]", answer)) {
        cli::cli_text("Your answer included a non-letter character.")
        return(private$guess())
      }

      if (nchar(answer) < 4) {
        cli::cli_text("Your answer has to be at least four letters long.")
        return(private$guess())
      }

      if (!grepl(private$center, answer, ignore.case = TRUE)) {
        cli::cli_text(
          "Your answer has to include the center letter: {private$center}"
        )
        return(private$guess())
      }

      if (
        grepl(
          paste0(
            "[^", private$center, paste(private$letters, collapse = ""), "]"
          ),
          answer,
          ignore.case = TRUE
        )
      ) {
        cli::cli_text(
          "Your answer includes a letter that isn't in available in this game."
        )
        return(private$guess())
      }

      if (answer %in% private$possibilities$word) {
        private$answers <- sort(c(private$answers, answer))

        private$score_plus <-
          private$possibilities[private$possibilities$word == answer, ]$score

        private$score <- private$score + private$score_plus
        private$rank <- tail(bee_ranks[bee_ranks$score < private$score, ], 1)$rank
        return(self$play())
      }

      cli::cli_text("I don't recognize that word.")
      private$guess()
    },

    build_grid = function() {
      commons <- sample(private$commons, 2)
      vowels  <- sample(private$vowels, 2)
      others  <- sample(letters[!letters %in% c(commons, vowels)], 3)

      if ("q" %in% others && !"u" %in% vowels) {
        return(private$build_grid())
      }

      private$center  <- commons[[1]]
      private$letters <- c(commons[[2]], vowels, others)

      combns <- c(
        lapply(
          1:6,
          function(m) {
            lapply(
              utils::combn(private$letters, m, simplify = FALSE),
              function(x) {paste(sort(c(private$center, x)), collapse = "")}
            )
          }
        ),
        recursive = TRUE
      )

      private$possibilities <- spelling_bee_wordlist[
        spelling_bee_wordlist$letters %in% combns,
      ]

      if (
        nrow(private$possibilities) < 50 ||
        max(nchar(private$possibilities$letters)) < 7
      ) {
        return(private$build_grid())
      }

      private$center <- toupper(private$center)
      private$letters <- toupper(sample(private$letters))

      self$game_over <- FALSE
    },

    refresh = function() {
      private$build_grid()
      self$play()
    },

    shuffle = function() {
      private$letters <- sample(private$letters)
      self$play()
    },

    ask_restart = function() {
      self$game_over <- TRUE

      cat('\nPress ENTER to try again or type "quit" to exit.\n')

      if (grepl("^[^A-Za-z]![^A-Za-z]", tolower(input("> ")))) {
        return(quit_game())
      } else {
        private$refresh()
      }
    },

    vowels  = c("a", "e", "i", "o", "u"),
    commons = c("s", "r", "n", "t", "l", "c", "d", "g", "p", "m", "h", "b")
  )
)

bee_ranks <- data.frame(
  rank = c(
    "Beginner",
    "Good start",
    "Moving up",
    "Good",
    "Solid",
    "Nice",
    "Great",
    "Amazing",
    "Genius"
  ),
  score = c(0, 5, 10, 20, 30, 40, 50, 75, 100)
)
