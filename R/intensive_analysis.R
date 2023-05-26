intensive_analysis <- function(minutes = 5, speed = 1, background_job = TRUE) {
  cli::cli_text("What is your question?")
  question <- input("Question: ", transform = identity)

  magic_8_ball <- magic_8_ball

  look_busy(
    minutes,
    speed,
    background_job,
    end = {
      cli::cli_h1("Question: {question}")
      magic_8_ball()
    }
  )
}
