#' Play a game of shut the box in the console
#'
#' Your goal is to flip down all nine tiles numbered from 1 to 9.
#' Each turn, you roll two dice and flip down any number of tiles that add up
#' to the same number as the sum of your dice.
#' After you have flipped down the 7, 8, and 9 tiles, you may choose whether to
#' roll one or two dice.
#' You win the game if you flip down all nine tiles!
#' But if you ever can't flip down a combination of tiles that sums to your
#' die roll, it's game over.
#'
#' @return Generates interactive output in the console.
#' @aliases shut_the_box
#' @export
#' @importFrom R6 R6Class
#'
#' @examples
#' play_shut_the_box()
play_shut_the_box <- function() {
  if (!rlang::is_interactive()) {
    return(invisible(NULL))
  }

  if (!is.null(game_env$shut_the_box) && ask_resume("shut_the_box")) {
    return(resume("shut_the_box"))
  }

  game_env$shut_the_box <- ShutTheBox$new()
  game_env$shut_the_box$play()
}

ShutTheBox <- R6::R6Class(
  "ShutTheBox",

  public = list(
    game_over = FALSE,

    initialize = function() {
      self$game_over <- FALSE
      private$tiles <- 1:9
      private$dice <- integer(0)
    },

    play = function() {
      self$print()

      if (length(private$dice) == 0) {
        private$roll()

        self$print(roll = TRUE)
      }

      private$assert_possible_moves()

      cli::cli_text("Enter tiles to shut.")
      private$shut()

      private$dice <- integer(0)

      self$play()
    },

    print = function(roll = FALSE) {
      clear_console()

      cli::cli_h1(cli::col_magenta("\U1F3B2 Shut the Box \U1F3B2"))

      print_tiles(private$tiles)

      if (length(private$dice) > 0) {
        print_dice(private$dice, prefix = "Roll: ", roll = roll)
      }
    }
  ),

  private = list(
    tiles = 1:9,
    dice = integer(0),

    roll = function() {
      if (max(private$tiles) < 7) {
        cli::cli_text("Would you like to roll one or two dice?")

        input <- input("Dice: ")

        if (input == "quit") quit_game()

        if (!input %in% 1:2) {
          cli::cli_text(
            "I didn't understand that input.
            Make sure to enter either {.or {.val {1:2}}}."
          )

          return(private$roll())
        }

        n <- input
      } else {
        cli::cli_text("Press {.key ENTER} to roll.")
        input <- input()
        if (input == "quit") quit_game()
        n <- 2
      }

      private$dice <- sample(1:6, n, replace = TRUE)
    },

    shut = function() {
      input <- input("Shut: ")

      if (input == "quit") quit_game()

      input <- try(
        as.integer(unlist(strsplit(input, "", fixed = TRUE))),
        silent = TRUE
      )

      if (!is.integer(input)) {
        cli::cli_text(
          "I didn't understand that input. Make sure to only enter digits."
        )
        private$shut()
      }

      if (!all(input %in% private$tiles)) {
        cli::cli_text("Make sure to only select tiles that are up.")
        private$shut()
      }

      if (sum(input) != sum(private$dice)) {
        cli::cli_text(
          "The sum of your selected tiles must be equal to
          {.val {sum(private$dice)}."
        )
      }

      private$tiles <- setdiff(private$tiles, input)

      if (length(private$tiles) == 0) {
        private$win()
      }
    },

    assert_possible_moves = function() {
      tile_combinations <- private$tiles %>%
        lapply(`*`, 0:1) %>%
        expand.grid() %>%
        rowSums()

      dice_sum <- sum(private$dice)

      if (!dice_sum %in% tile_combinations) {
        private$lose()
      }
    },

    lose = function() {
      self$game_over <- TRUE
      cli::cli_text("Sorry, there are no possible moves.")
      private$ask_replay()
    },

    win = function() {
      self$game_over <- FALSE
      cli::cli_text("You win!")
      private$ask_replay()
    },

    ask_replay = function() {
      cli::cli_text(
        "Press {.key ENTER} to play again or {.key ESC} to quit."
      )
      input <- input()
      private$replay()
    },

    replay = function() {
      self$initialize()
      self$play()
    }
  )
)

print_tiles <- function(up) {
  is_up <- seq_len(9) %in% up
  up_rle <- rle(is_up)
  tiles <- split(seq_len(9), rep(seq_along(up_rle$lengths), up_rle$lengths))

  top <- middle <- character(length(up_rle$values))

  for (i in seq_along(up_rle$values)) {
    is_up <- up_rle$values[[i]]

    top[[i]] <- if (is_up) {
      paste0("┌", paste(rep("─", length(tiles[[i]])), collapse = "┬"), "┐")
    } else {
      paste(rep(" ", length(tiles[[i]])), collapse = " ")
    }

    middle[[i]] <- if (is_up) {
      paste(c("", tiles[[i]], ""), collapse = "│")
    } else {
      paste(rep(" ", length(tiles[[i]])), collapse = " ")
    }
  }

  if (up_rle$values[[1]] == FALSE) {
    top <- c(" ", top)
    middle <- c(" ", middle)
  }

  bottom <- paste0("└", paste(rep("─", 9), collapse = "┴"), "┘")

  cat(
    paste(top, collapse = ""),
    paste(middle, collapse = ""),
    paste(bottom, collapse = ""),
    sep = "\n"
  )
}

print_dice <- function(x, prefix = NULL, roll = FALSE, ...) {
  if (isTRUE(roll)) {
    withr::local_options("cli.progress_show_after" = 0)

    message <- ""

    cli::cli_progress_message("{prefix}{message}")

    for (i in seq_len(60)) {
      random_dice <- sample(1:6, length(x), replace = TRUE)
      message <- paste0(paste(random_dice, collapse = " "))
      cli::cli_progress_update()
      Sys.sleep(1/60)
    }

    cli::cli_status_clear()
  }

  cli::cat_line(prefix, paste(x, collapse = " "))
}
