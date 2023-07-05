#' Look like you're working hard when you're hardly working
#'
#' @author Alexander Rossell Hayes \email{alexander@rossellhayes.com}
#'   ([ORCID](https://orcid.org/0000-0001-9412-0457))
#'
#'   With status messages contributed by:
#'
#'   - Kristin Bott \email{kristin.bott@posit.co}
#'   - Daniel Chen \email{daniel.chen@posit.co}
#'     ([ORCID](https://orcid.org/0000-0003-3857-1741))
#'   - Steven Smallberg \email{steven.smallberg@posit.co}
#'
#' @param minutes How long to keep this up for.
#'   Defaults to `Inf`, which runs forever.
#' @param speed How fast to produce output.
#'   The default value, `1`,
#'   takes between 0.1 and 0.5 seconds to produce a calculation
#'   and between 1 and 5 seconds to produce a status update.
#' @param background_job Whether to produce output in in a background job
#'   (if `TRUE`) or the console (if `FALSE`).
#'   Defaults to `FALSE`.
#'   Running in a background process requires the [job][job::job()] package.
#' @param end Code to run after `minutes`.
#'   If `minutes` is `Inf`, `end` is never run.
#'
#' @return `end` if `minutes` is finite.
#' @export
#'
#' @examples
#' look_busy()
look_busy <- function(
  minutes = if (rlang::is_interactive()) Inf else 0,
  speed = 1,
  background_job = FALSE,
  end = invisible(NULL)
) {
  if (!background_job) {
    look_busy_internal(minutes, speed)
    return(end)
  }

  rlang::check_installed("job", "to look busy in a background job.")

  end <- rlang::enexpr(end)

  # Prepare objects for `job::job()` to import
  status_messages <- status_messages
  variables <- variables
  look_busy_internal <- look_busy_internal
  shuffle <- shuffle

  purrr::walk(
    sys.frames(),
    function(env, current_env) {
      purrr::iwalk(
        as.list(env),
        function(object, name, current_env) {
          assign(name, object, envir = current_env)
        },
        current_env = current_env
      )
    },
    current_env = rlang::current_env()
  )

  job::job(
    {
      look_busy_internal(minutes, speed)
      rlang::eval_bare(end)
    },
    title = "Performing an intensive analysis...",
    import = "all",
    packages = NULL
  )
}

look_busy_internal <- function(minutes, speed) {
  shuffled_status_messages <- paste0(shuffle(status_messages), "...")

  i <- 1
  start_time <- Sys.time()

  while (difftime(Sys.time(), start_time, units = "mins") < minutes) {
    cli::cli_h3(shuffled_status_messages[[i]])
    i <- i + 1
    if (i > length(shuffled_status_messages)) {
      shuffled_status_messages <- paste0(shuffle(status_messages), "...")
      i <- i - length(shuffled_status_messages)
    }
    Sys.sleep(stats::runif(1, 1, 5) * (1/speed))

    for (j in seq_len(1 + stats::rpois(1, 3))) {
      variable <- sample(variables, 1)
      variables <- unique(c(
        variables,
        if (grepl("_", variable)) {
          number <- as.numeric(sub("^.+_", "", variable, perl = TRUE))
          sub("[^_]+$", number + 1, variable, perl = TRUE)
        } else {
          paste0(variable, "_1")
        }
      ))

      result <- signif(1000 ^ stats::rnorm(1), 1 + stats::rpois(1, 3))
      if (result > 1 & runif(1) > 1/4) result <- round(result)

      cli::cat_line(variable, " = ", result)

      Sys.sleep(stats::runif(1, 0.1, 0.5) * (1/speed))
    }
  }
}

shuffle <- function(list = status_messages) {
  unlist(sample(list))
}

status_messages <- list(
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
  c("Counting on fingers", "Counting on toes"),
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
  "Summing squares",
  "Cleaning data",
  "Removing NAs",
  "Imputing the mean",
  "Descending the gradient",
  "Cracking the secret key",
  "Guessing and checking",
  "Plugging and chugging",
  "Refactoring source code",
  "Tilting at windmills",
  "Rolling a boulder up a hill",
  "Fitting a square peg into a round hole",
  "Taking a breather",
  "Proving P = NP",
  "Rubber ducking",
  "Waiting for CI to run",
  "Flipping a coin",
  "Adjusting priors",
  "Mining bitcoin",
  "Approaching infinity",
  "Bogo sorting",
  "Rolling dice",
  "Shaking a magic 8 ball",
  "Envisioning success",

  # Thanks to Daniel Chen (@chendaniely)
  "Opening the pod bay doors",

  # Thanks to Kristin Bott (@RhoBott)
  "Calculating Delaunay triangulation",
  "Drawing Theissen polygons",
  "Diagramming on a napkin",
  "Performing back of the envelope calculations",
  "Completing Punnett squares",
  "Identifying chirality",
  "Critically examining the concept of objectivity",
  "Debating validity of metrics",
  "Avoiding highways",
  "Recalculating route",
  "Karyotyping proposed subspecies",
  "Spot checking",
  "Validating by random sample",
  "Eagerly awaiting Paul Hollywood's handshake",
  "Moasaicing rasters",
  "Calculating weighted centroids",
  "Processing point cloud",
  "Adjusting opacity",
  "Interpolating surface from point data",
  "Drinking another cup of coffee",
  "Explaining this all to an in-law",
  "Identifying outliers",
  "Regretting past choices",
  "Staring out window",
  "Pondering the universality of impermanence",

  # Thanks to Steven Smallberg (@stevensmallberg)
  "Accounting for the curvature of the earth",
  "Adding hidden layers",
  "Blowing on the cartridge",
  "Bubble sorting",
  "Calculating the price of tea in China",
  "Calling in the cavalry",
  "Centering and scaling",
  "Deploying kubernetes",
  "Excusing my dear Aunt Sally",
  "Letting x be a continuous random variable",
  "Licking finger and holding it into the wind",
  "Overfitting model",
  "Phoning a friend",
  "Pushing to main",
  "Reading the manual",
  "Reciting affirmations",
  "Restoring to factory settings",
  "Stroking chin",
  "Zooming and enhancing"
)

variables <- c(
  letters,
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
