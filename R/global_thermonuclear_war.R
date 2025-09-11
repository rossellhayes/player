play_global_thermonuclear_war <- function() {
  withr::local_options(
    "cli.progress_show_after" = 0,
    "cli.progress_clear" = FALSE
  )

  line_1 <- "A strange game."
  line_2 <- "The only winning move is not to play."
  line_3 <- paste0(
    "How about a nice game of ",
    sample(names(game_list)[!game_list == "Global Thermonuclear War"], 1),
    "?"
  )

  Sys.sleep(1)
  clear_console()
  h1("\U2622\UFE0F Global Themonuclear War \U2622\UFE0F")

  cli::cli_progress_bar("Preparing AI...        ", total = 100)

  for (i in seq_len(100)) {
    cli::cli_progress_update()
    Sys.sleep(0.03 + abs(stats::rnorm(1, sd = 0.01)))
  }

  Sys.sleep(0.5)

  cli::cli_progress_bar("Analyzing strategies...", total = 100)

  for (i in seq_len(100)) {
    cli::cli_progress_update()
    Sys.sleep(0.03 + abs(stats::rnorm(1, sd = 0.01)))
  }

  Sys.sleep(0.5)

  clear_console()
  h1("\U2622\UFE0F Global Themonuclear War \U2622\UFE0F")
  message_1 <- " "
  message_2 <- " "
  message_3 <- " "
  cli::cli_progress_message("{message_1}")

  for (i in seq_len(nchar(line_1) + 1)) {
    Sys.sleep(0.1)
    message_1 <- substr(line_1, 1, i)
    cli::cli_progress_update()
  }

  Sys.sleep(0.5)

  cli::cli_progress_message("{message_2}")

  for (i in seq_len(nchar(line_2) + 1)) {
    Sys.sleep(0.1)
    message_2 <- substr(line_2, 1, i)
    cli::cli_progress_update()
  }

  Sys.sleep(0.5)

  cli::cli_progress_message("{message_3}")

  for (i in seq_len(nchar(line_3) + 1)) {
    Sys.sleep(0.1)
    message_3 <- substr(line_3, 1, i)
    cli::cli_progress_update()
  }

  Sys.sleep(1)

  cli::cli_progress_done()
  cat_blank_line()
  play()
}
