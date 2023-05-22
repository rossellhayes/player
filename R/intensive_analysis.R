intensive_analysis <- function(minutes = 5) {
  cli::cli_text("What is your question?")
  question <- input("Question: ", transform = identity)

  rlang::check_installed("job", "to perform an intensive analysis.")

  job::job(
    {
      loading_messages <- paste0(sample(loading_messages), "...")
      i <- 1
      start_time <- Sys.time()

      while (difftime(Sys.time(), start_time, units = "mins") < minutes) {
        cli::cli_h3(loading_messages[[i]])
        i <- i + 1
        if (i > length(loading_messages)) {
          loading_messages <- sample(loading_messages)
          i <- i - length(loading_messages)
        }
        Sys.sleep(stats::runif(1, 1, 5))

        for (j in seq_len(1 + stats::rpois(1, 3))) {
          cli::cat_line(
            sample(c(letters, greek_letters), 1),
            if (stats::runif(1) < 1/4) {
              paste0("_", sample(c("i", 1:9), 1, prob = 1 / log(2:11)))
            },
            " = ",
            signif(1000 ^ stats::rnorm(1), 1 + stats::rpois(1, 3))
          )

          Sys.sleep(stats::runif(1, 0.1, 0.5))
        }
      }

      cli::cat_line()
      cli::cli_h1("Question: {question}")
      magic_8_ball()
      job::export(NULL)
    },
    title = "Performing an intensive analysis...",
    import = c(
      question,
      minutes,
      loading_messages,
      greek_letters,
      magic_8_ball
    ),
    packages = c("cli", "job", "stats")
  )
}

loading_message <- function() {
  paste0(sample(loading_messages, 1), "...")
}

loading_messages <- c(
  "Reticulating splines",
  "Crunching numbers",
  "Carrying the one",
  "Squaring the circle",
  "Proving Fermat's last theorem",
  "Determining the final digit of pi",
  "FOILing",
  "Dividing by zero",
  "Taking it to the limit",
  "Drawing a Sierpinski triangle",
  "Finding the partial derivative",
  "Predicting the stock market",
  "Hacking into NORAD",
  "Asking ChatGPT",
  "Enrolling in Stats 101 at the local community college",
  "Checking StackOverflow",
  "Googling the error message",
  "Doing long division",
  "Injecting SQL",
  "Rendering LaTeX",
  "Using the chain rule",
  "Counting on toes",
  "Drawing the line of best fit",
  "Taking the square root of a negative",
  "Converting to factor",
  "Adding dummy variables",
  "p hacking",
  "Updating rlang",
  "Taking the log of income",
  "Creating a reprex",
  "Setting the seed to 123",
  "Building stringi from source",
  "Running in a Docker container",
  "Installing Rust",
  "Discovering a memory leak",
  "Subsetting object of type closure",
  "Reading the documentation",
  "Summing the squares",
  "Cleaning data",
  "Removing NAs",
  "Imputing the mean",
  "Climbing the hill",
  "Descending the gradient",
  "Cracking the secret key",
  "Guessing and checking",
  "Plugging and chugging",
  "Refactoring source code",
  "Tilting at windmills"
)

greek_letters <- c(
  "alpha",
  "beta",
  "gamma",
  "delta",
  "epsilon",
  "zeta",
  "eta",
  "theta",
  "iota",
  "kappa",
  "lambda",
  "mu",
  "nu",
  "xi",
  "omicron",
  "pi",
  "rho",
  "sigma",
  "tau",
  "upsilon",
  "phi",
  "chi",
  "psi",
  "omega"
)
