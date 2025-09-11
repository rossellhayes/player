intensive_analysis <- function(minutes = 0.8, speed = 2, lambda = 1, background_job = TRUE) {
  cli::cli_text("What is your question?")
  question <- input("Question: ", transform = identity)

  magic_8_ball <- magic_8_ball

  look_busy(
    minutes,
    speed,
    lambda,
    background_job,
    end = cli::cli_alert_success("Results are ready!")
  )

  delayedAssign(
    "intensive_analysis_results",
    {
      h1("Question: {question}")
      magic_8_ball()
    },
    assign.env = .GlobalEnv
  )
}
