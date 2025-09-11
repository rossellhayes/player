rock_paper_scissors_hands <- c(
  "Rock" = "✊",
  "Paper" = "✋",
  "Scissors" = "✌️",
  "Shoot!" = "👉"
)

rock_paper_scissors_icons <- c(
  "Rock" = "🪨",
  "Paper" = "📄",
  "Scissors" = "✂️",
  "Shoot!" = "🔫"
)

rps_throw <- function(shape, thrower) {
  shapes <- c("rock", "paper", "scissors", "shoot")
  if (shape %in% shapes) shape <- match(shape, shapes)
  stopifnot(shape %in% 1:4)
  stopifnot(thrower %in% c("player", "computer"))

  if (thrower == "player") {
    paste(
      stringr::str_pad(
        names(rock_paper_scissors_hands)[[shape]],
        width = max(stringr::str_width(names(rock_paper_scissors_hands))),
        side = "left"
      ),
      rock_paper_scissors_hands[[shape]]
    )
  } else {
    paste(
      rock_paper_scissors_icons[[shape]],
      stringr::str_pad(
        names(rock_paper_scissors_icons)[[shape]],
        width = max(stringr::str_width(names(rock_paper_scissors_icons))),
        side = "right"
      )
    )
  }
}

rps_match <- function(player_shape, computer_shape) {
  paste(
    rps_throw(player_shape, "player"),
    " ",
    rps_throw(computer_shape, "computer")
  )
}

rock_paper_scissors_shoot <- function() {
  messages <- map_chr(
    1:4,
    function(shape) rps_match(shape, shape)
  )

  messages <- rps_center_message(messages)

  for (message in messages) {
    cat("\r", message, sep = "")
    Sys.sleep(1/3)
  }

  cat("\r", strrep(" ", stringr::str_width(message)), sep = "")
}

rps_record_default <- data.frame(
  player = c(
    "rock", "rock", "scissors", "rock", "scissors", "rock", "rock", "paper",
    "scissors", "paper"
  ),
  computer = c(
    "scissors", "paper", "scissors", "paper", "scissors", "scissors", "rock",
    "paper", "rock", "scissors"
  ),
  win = rep(FALSE, 10),
  lose = rep(FALSE, 10),
  tie = rep(FALSE, 10),
  stringsAsFactors = FALSE
)

#' Play a game of rock, paper, scissors in the console
#'
#' @param selection Whether to throw rock, paper, or scissors.
#'   Can be abbreviated as "r", "p", or "s".
#'   Defaults to `NA`, which interactively asks which to throw.
#' @param predict If `TRUE`, the computer player fits a model on past games to
#'   predict what you will throw and try to beat you.
#' @param animate If `TRUE`, play a "rock, paper, scissors, shoot!" animation
#'   before revealing what you and the computer throw.
#' @export
#'
#' @examples
#' play_rock_paper_scissors("rock")
#'
#' if (rlang::is_interactive()) play_rock_paper_scissors()
play_rock_paper_scissors <- function(
  selection = c(NA, "rock", "paper", "scissors"),
  predict = TRUE,
  animate = TRUE
) {
  rps_record_file <- file.path(tools::R_user_dir("player"), "rps", "record.rds")

  if (!dir.exists(file.path(tools::R_user_dir("player"), "rps"))) {
    dir.create(file.path(tools::R_user_dir("player"), "rps"), recursive = TRUE)
  }

  if (file.exists(rps_record_file)) {
    rps_record <- readRDS(rps_record_file)
  } else {
    rps_record <- rps_record_default
  }

  selection <- as.character(selection)
  player_selection <- match.arg(selection)

  options <- c("rock", "paper", "scissors")

  repeat {
    player_selection <- pmatch(player_selection, options)

    if (player_selection %in% 1:3) break

    if (!rlang::is_interative()) {
      cli::cli_abort("{.arg selection} must be one of {.value {.or {c('rock', 'paper', 'scissors')}}}.")
    }

    player_selection <- input("Will you throw [r]ock, [p]aper, or [s]cissors? ")
    cat("\b\b\r")
  }

  if (!isTRUE(predict)) {
    computer_selection <- sample(1:3, 1)
  } else {
    capture.output(
      model <- try(
        nnet::multinom(
          dplyr::lead(player) ~ player * computer + lag(player) * lag(computer),
          data = rps_record,
          weights = rev(1 / log(seq_len(nrow(rps_record)) + 10))
        ),
        silent = TRUE
      )
    )

    if (!inherits(model, "multinom")) {
      capture.output(
        model <- try(
          nnet::multinom(
            dplyr::lead(player) ~ player * computer,
            data = rps_record,
            weights = rev(1 / log(seq_len(nrow(rps_record)) + 10))
          ),
          silent = TRUE
        )
      )
    }

    if (inherits(model, "multinom")) {
      prediction <- try(
        predict(model, rps_record[nrow(rps_record), ], type = "probs"),
        silent = TRUE
      )
    } else {
      prediction <- NULL
    }

    if (
      length(prediction) != 3 ||
      !inherits(prediction, "numeric") ||
      identical(rps_record, rps_record_default)
    ) {
      prediction <- c("paper" = 1/3, "rock" = 1/3, "scissors" = 1/3)
    }

    prediction <- sample(prediction)

    computer_selection <- dplyr::case_match(
      names(prediction)[which.max(prediction)],
      "rock" ~ 2,
      "paper" ~ 3,
      "scissors" ~ 1
    )

    if (!computer_selection %in% 1:3) {
      computer_selection <- sample(1:3, 1)
    }
  }

  if (isTRUE(animate)) rock_paper_scissors_shoot()

  cat_over(rps_center_message(rps_match(player_selection, computer_selection)))

  difference <- (player_selection - computer_selection) %% 3

  if (difference == 0) {
    result <- "tied"
  } else if (difference == 1) {
    result <- "win!"
  } else if (difference == 2) {
    result <- "lose"
  }

  cat_line(rps_center_message(paste("You", result)))

  if (isTRUE(predict)) {
    prediction <- round(rev(sort(prediction)) * 100)
    prediction_message <- paste(
      "I predicted you would throw:",
      paste0(prediction[[1]], "% ", names(prediction)[[1]], ","),
      paste0(prediction[[2]], "% ", names(prediction)[[2]], ","),
      paste0(prediction[[3]], "% ", names(prediction)[[3]], ".")
    )
    cat_line(rps_center_message(prediction_message))
  }

  this_rps_record <- data.frame(
    player = options[[player_selection]],
    computer = options[[computer_selection]],
    win = result == "win!",
    lose = result == "lose",
    tie = result == "tied"
  )

  rps_record <- rbind(rps_record, this_rps_record)
  saveRDS(rps_record, rps_record_file)

  record_message <- paste(
    "Your record:",
    plu::ral("{n} win,", n = sum(rps_record[["win"]])),
    plu::ral("{n} loss,", n = sum(rps_record[["lose"]])),
    plu::ral("{n} tie.", n = sum(rps_record[["tie"]]))
  )

  cat_line(rps_center_message(record_message))

  if (rlang::is_interactive()) {
    selection <- input("Press ENTER to play again or ESC to quit. ")
    cat("\b\b\r                                          ")

    if (!nzchar(selection)) {
      selection <- NA
    } else {
      cat(" ", strrep(" ", stringr::str_width(selection)))
      cat_line()
    }

    play_rock_paper_scissors(selection, predict = predict, animate = animate)
  }
}

rps_center_message <- function(message) {
  stringr::str_pad(
    message,
    width = max(
      getOption("width", 0),
      max(stringr::str_width(rps_match(3, 3)))
    ),
    side = "both"
  )
}
