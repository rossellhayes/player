#' Play a game of jumbles in the console
#'
#' @param difficulty Determines the length of words used for the game and,
#'   if `word_list` is `NULL`, how common the words are
#' @param word_list Optionally, a character vector of words used to play the
#'   game.
#'   If unspecified, a default word list will be used based on `difficulty`.
#'
#' @details # Difficulty
#' Difficulty levels are defined as follows:
#' * `beginner`: very common English words with 3 to 5 letters
#' * `easy`: common English words with 4 to 6 letters
#' * `medium`: slightly less common English words with 5 to 8 letters
#' * `hard`: somewhat less common English words with 6 to 11 letters
#' * `expert`: less common English words with 7 to 15 letters
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
#' @aliases jumble
#' @export
#' @importFrom R6 R6Class
#'
#' @examples
#' play_jumble()
#'
#' play_jumble("beginner")
#' play_jumble("easy")
#' play_jumble("medium")
#' play_jumble("hard")
#' play_jumble("expert")
#'
#' cars <- unique(gsub(" .*", "", rownames(mtcars)))
#' play_jumble(word_list = cars)

play_jumble <- function(
  difficulty = c("beginner", "easy", "medium", "hard", "expert"),
  word_list  = NULL
) {
  if (!interactive()) {return(invisible(NULL))}

  if (
    !is.null(game_env$jumble) &&
    !game_env$jumble$game_over &&
    (
      identical(word_list, game_env$jumble$word_list) ||
      (is.null(word_list) && difficulty == game_env$jumble$difficulty)
    )
  ) {
    return(resume("jumble"))
  }

  if (!is.null(word_list)) {word_list <- unique(word_list)}

  game_env$jumble <- Jumble$new(difficulty, word_list)
}

Jumble <- R6::R6Class(
  "Jumble",
  public = list(
    difficulties = c("Beginner", "Easy", "Medium", "Hard", "Expert"),
    word_list  = NULL,
    difficulty = NULL,
    game_over  = FALSE,

    initialize = function(
      difficulty = self$difficulties,
      word_list = NULL
    ) {
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
          cli::cli_h1("\U1F500 Jumble \U1F500")

          difficulty <- choose_menu(
            c(self$difficulties, "Quit"),
            title = "Choose a difficulty:"
          )

          if (difficulty == "Quit") {
            quit_game()
          }
        }

        difficulty <- tolower(difficulty)
        self$difficulty <- difficulty

        classes <- switch(
          difficulty,
          "beginner" = 1:9,
          "easy"     = 4:12,
          "medium"   = 7:15,
          "hard"     = 10:18,
          "expert"   = 13:21
        )

        lengths <- switch(
          difficulty,
          "beginner" = 3:5,
          "easy"     = 4:6,
          "medium"   = 5:8,
          "hard"     = 6:11,
          "expert"   = 7:15
        )

        self$word_list <- jumble_wordlist[jumble_wordlist$class %in% classes, ]
        self$word_list <- self$word_list$lemma
        self$word_list <- self$word_list[nchar(self$word_list) %in% lengths]
      }

      self$word_list <- self$word_list[
        !self$word_list %in% c("help", "restart", "quit", "surrender")
      ]

      private$refresh()
    },

    play = function() {
      self$print()
      private$guess()
    },

    print = function() {
      if (!interactive()) {return(invisible(NULL))}

      clear_console()
      cli::cli_h1("\U1F500 Jumble \U1F500")
      cli::cat_line(
        'Type "quit" to exit. Type "help" for commands.',
        col = "grey"
      )

      cli::cat_line(toupper(space_out_letters(private$jumble)))
      cli::cat_line()
    }
  ),
  private = list(
    word    = NULL,
    jumble  = NULL,
    answers = NULL,

    guess = function() {
      answer <- gsub(" ", "", input())

      if (answer == private$word) {
        cat("You win!")
        return(private$ask_restart())
      }

      if (answer %in% private$answers) {
        cat(
          "I was looking for ", toupper(private$word), ", but that works!",
          sep = ""
        )
        return(private$ask_restart())
      }

      if (grepl("help", answer)) {
        cat(
          'Type "restart" for a new word,',
          '"surrender" to reveal the current word,',
          'or "quit" to exit.',
          sep = "\n"
        )
        return(private$guess())
      }

      if (grepl("restart", answer)) {
        return(private$refresh())
      }

      if (grepl("quit", answer)) {
        quit_game()
      }

      if (grepl("surrender", answer)) {
        cat(
          "You surrendered. The word was ", toupper(private$word), ".", sep = ""
        )
        private$ask_restart()
      }

      if (grepl("[^a-z]", answer)) {
        cat(crayon::silver("I couldn't understand that input. "))
        return(private$guess())
      }

      cat("Sorry, the word was ", toupper(private$word), ".", sep = "")
      private$ask_restart()
    },

    refresh = function() {
      private$word    <- sample(self$word_list, 1)
      letters         <- sort(strsplit(private$word, "")[[1]])
      jumble          <- sample(letters, length(letters))
      private$jumble  <- paste(jumble, collapse = "")

      if (private$jumble %in% jumble_checklist$word) {
        return(private$refresh())
      }

      private$answers <- c(
        private$word,
        jumble_checklist[
          jumble_checklist$letters == paste(letters, collapse = ""),
        ]$word
      )

      self$game_over  <- FALSE

      self$play()
    },

    ask_restart = function() {
      self$game_over <- TRUE

      cat('\nPress ENTER to try again or type "quit" to exit.\n')

      if (grepl("quit", tolower(input("> ")))) {
        return(quit_game())
      } else {
        private$refresh()
      }
    }
  )
)

space_out_letters <- function(x) {
  gsub("(?<=.)(?!$)", " ", x, perl = TRUE)
}
