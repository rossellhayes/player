# TODO: Discard "quit" does not work

#' Play five-card draw poker
#'
#' Interact with the game by typing commands into the console and
#' pressing enter.
#'
#' @param colors One of `1`, `2`, or `4`.
#'   The number of colors to use to render the suits.
#'   * For `1`, all suits are the default console color.
#'   * For `2`, hearts and diamonds are red and spades and clubs are
#'   the default console color.
#'   * For `4`, hearts are red, clubs are green, diamonds are blue, and spades
#'   are the default console color.
#'
#' @section Scoring:
#'
#' **Hand**        | **Points** | **Description**
#' --------------- | ---------- | ----------------------------------------------
#' Junk            |        -10 | Nothing of value.
#' Low pair        |          0 | Two cards of the same rank, ten or below.
#' High pair       |          5 | Two aces, kings, queens, or jacks.
#' Two pair        |         10 | Two cards of one rank and two cards of another.
#' Three of a kind |         20 | Three cards of the same rank.
#' Straight        |         50 | Five cards in sequential order.
#' Flush           |         75 | Five cards of the same suit.
#' Full house      |        100 | Three cards of one rank and two cards of another.
#' Four of a kind  |      1,000 | Four cards of the same rank.
#' Straight flush  |     50,000 | Five cards of the same suit in sequential order.
#' Royal flush     |  1,000,000 | Ace, king, queen, jack, and ten of the same suit.
#'
#' For straights, aces may be considered the highest card (above king) or the
#' lowest card (below two).
#' However, aces may not be used to connect a king and a two
#' (e.g. Q-K-A-2-3 is not a straight).
#'
#' @return Generates an interactive game of poker in the console.
#' @aliases poker
#' @export
#' @importFrom R6 R6Class
#'
#' @examples
#' play_poker()
#' play_poker(colors = 4)
#' play_poker(colors = 1)

play_poker <- function(colors = c(2, 4, 1)) {
  if (!rlang::is_interactive()) {
    return(invisible(NULL))
  }

  if (!is.null(game_env$poker) && ask_resume("poker")) {
    if (length(colors) == 1) {game_env$poker$colors <- colors}
    return(resume("poker"))
  }

  colors <- match.arg(as.character(colors), c("2", "4", "1"))

  game_env$poker <- Poker$new(colors)
  game_env$poker$play()
}

Poker <- R6::R6Class(
  "Poker",
  public = list(
    score      = 0,
    hand       = NULL,
    game_over  = FALSE,
    colors     = 2,

    initialize = function(colors = 2) {
      self$colors <- colors
      self$hand   <- Poker_Hand$new()
    },

    play = function() {
      clear_console()
      h1("\U1F0CF Poker \U1F0CF")

      print(self, index = TRUE)

      private$discard()
      print(self, index = FALSE)

      value <- self$hand$check_hand()

      winnings <- switch(
        value,
        "Junk"            = -10,
        # "High card"       = -10,
        "Low pair"        = 0,
        "High pair"       = 5,
        "Two pair"        = 10,
        "Three of a kind" = 20,
        "Straight"        = 50,
        "Flush"           = 75,
        "Full house"      = 100,
        "Four of a kind"  = 1000,
        "Straight flush"  = 50000,
        "Royal flush"     = 1000000
      )

      self$score <- self$score + winnings

      pos <- winnings > 0

      value[value == "Royal flush"] <- private$royal_flush()

      cat(
        value, if (pos) {"!"}, " (", if (pos) {"+"},
        format(winnings, scientific = FALSE, big.mark = ","), ")", "\n",
        sep = ""
      )

      private$continue()
    },

    print = function(index = FALSE) {
      if (index) {
        cat(
          crayon::silver(
            "Score:",
            format(self$score, scientific = FALSE, big.mark = ","),
            "\n"
          )
        )
      }
      print(self$hand, colors = self$colors, index = index)
    }
  ),

  private = list(
    discard = function() {
      input <- input("Discard: ")

      if (grepl("quit|exit", input)) {quit_game()}

      if (agrepl("color|setting|option", input, fixed = FALSE)) {
        private$options()

        clear_console()

        print(self, index = TRUE)

        return(private$discard())
      }

      if (
        input == "" ||
        input != tryCatch(as.integer(input), warning = function(w) "__no_way__")
      ) {
        return(private$invalid_discard())
      }

      input <- as.integer(unlist(strsplit(as.character(input), "")))

      if (any(!input %in% 0:5)) {return(private$invalid_discard())}

      self$hand$discard(input)
    },

    invalid_discard = function() {
      clear_console()

      print(self, index = TRUE)

      cat(
        stringr::str_wrap(
          paste(
            'Please enter digits between 1 and 5 to discard,',
            '0 to hold all cards, "quit" to exit, or "options" for settings.'
          ),
          width = cli::console_width()
        )
      )

      return(private$discard())
    },

    continue = function() {
      self$hand <- Poker_Hand$new()

      cat(
        cli::col_grey(
          stringr::str_wrap(
            c(
              'Type "quit" to exit or "options" to change color settings.',
              'Press ENTER to continue.'
            ),
            width = cli::console_width()
          )
        ),
        sep = "\n"
      )

      choice <- tolower(input("> "))

      if (grepl("quit|exit", choice)) {quit_game()}

      if (agrepl("color|option|setting", choice, fixed = FALSE)) {
        private$options()
      }

      self$play()
    },

    options = function() {
      choice <- choose_menu(
        c("One color", "Two colors", NA, "Four colors"),
        "Choose your color settings:"
      )

      if (choice == 1) {
        self$colors <- 1
      } else if (choice == 2) {
        self$colors <- 2
      } else if (choice %in% 3:4) {
        self$colors <- 4
      } else {
        cat("I didn't understand that input. Please enter a digit.")
        return(private$options())
      }
    },

    royal_flush = function() {
      if (self$colors == 1) {
        "Royal flush!"
      } else if (self$colors == 4) {
        paste0(
          crayon::red("R"),
          crayon::blue("o"),
          crayon::green("y"),
          "a",
          crayon::red("l"),
          " ",
          crayon::blue("f"),
          crayon::green("l"),
          "u",
          crayon::red("s"),
          crayon::blue("h"),
          crayon::green("!")
        )
      } else {
        paste0(
          crayon::red("R"), "o", crayon::red("y"), "a", crayon::red("l"), " ",
          "f", crayon::red("l"), "u", crayon::red("s"), "h", crayon::red("!")
        )
      }
    }
  )
)

Poker_Hand <- R6::R6Class(
  "Poker_Hand",
  public = list(
    hand  = NULL,

    initialize = function() {
      private$deal <- private$deck[sample(nrow(private$deck), 10), ]
      private$get_hand()
    },

    discard = function(discards) {
      discards <- discards[discards %in% 1:5]

      if (length(discards)) {
        private$deal <- private$deal[discards * -1, ]
        private$get_hand()
      }

      private$order()
      private$deal[1:5, ] <- self$hand
    },

    check_hand = function() {
      flush    <- private$check_flush()
      straight <- private$check_straight()
      table    <- table(self$hand$rank)
      trip     <- any(table == 3)
      pair     <- any(table == 2)

      if (straight && flush) {
        if (all(self$hand$rank %in% c(1, 10:13))) {return("Royal flush")}

        return("Straight flush")
      }

      if (any(table == 4))   {return("Four of a kind")}
      if (trip && pair)      {return("Full house")}
      if (flush)             {return("Flush")}
      if (straight)          {return("Straight")}
      if (trip)              {return("Three of a kind")}

      if (pair) {
        if (table(table)[["2"]] == 2) {return("Two pair")}

        if (names(table[table > 1]) %in% c("1", "11", "12", "13")) {
          return("High pair")
        }

        return("Low pair")
      }

      # if (any(self$hand$rank %in% c(1, 11:13)))  {return("High card")}

      return("Junk")
    },

    print = function(index = TRUE, colors = 2) {
      print_hand <- self$hand

      ranks <- c("A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K")
      print_hand$rank <- format(
        ranks[print_hand$rank],
        justify = "left",
        width = 2
      )

      suits <- c(
        "spades"   = "\u2660 ",
        "hearts"   = "\u2665 ",
        "clubs"    = "\u2663 ",
        "diamonds" = "\u2666 "
      )
      print_hand$suit <- suits[match(print_hand$suit, names(suits))]

      print_hand <- apply(
        as.matrix(print_hand)[1:5, ],
        1,
        function(x) {
          if (colors == 1) {
            x
          } else if (colors == 4) {
            if (grepl("\u2665", x[[2]])) {
              crayon::red(x)
            } else if (grepl("\u2663", x[[2]])) {
              crayon::green(x)
            } else if (grepl("\u2666", x[[2]])) {
              crayon::blue(x)
            } else {
              x
            }
          } else {
            if (grepl("\u2665|\u2666", x[[2]])) {crayon::red(x)} else {x}
          }
        }
      )

      cat(
        if (index) {crayon::silver("1  2  3  4  5 \n")},
        paste(c(print_hand[1, ], "\n"), collapse = " "),
        paste(c(print_hand[2, ], "\n"), collapse = " "),
        sep = ""
      )
    }
  ),

  private = list(
    deal = NULL,

    deck = expand.grid(
      rank = 1:13,
      suit = c("spades", "hearts", "clubs", "diamonds")
    ),

    get_hand = function() {self$hand <- private$deal[1:5, ]},

    check_flush = function() {length(unique(self$hand$suit)) == 1},

    check_straight = function() {
      rank <- self$hand$rank
      diff <- diff(sort(rank))

      if (all(diff == 1)) {return(TRUE)}

      rank[rank == 1] <- 14

      diff <- diff(sort(rank))

      if (all(diff == 1)) {return(TRUE)}

      FALSE
    },

    order = function() {
      if (!all(self$hand$rank %in% 1:5)) {
        self$hand$rank[self$hand$rank == 1] <- 14
      }

      frequencies <- table(self$hand$rank)
      frequencies <- frequencies[match(self$hand$rank, names(frequencies))]

      self$hand <- self$hand[
        order(frequencies, self$hand$rank, decreasing = TRUE),
      ]

      self$hand$rank[self$hand$rank == 14] <- 1
    }
  )
)
