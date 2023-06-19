library(fs)
library(magrittr)
library(purrr)
library(stringr)

dir_ls("data-raw", glob = "*.R") %>%
  str_subset("internal_data", negate = TRUE) %>%
  walk(source)

usethis::use_data(
  game_list,
  jumble_checklist, jumble_wordlist,
  spelling_bee_wordlist,
  internal = TRUE, overwrite = TRUE, compress = "xz"
)

rm(
  list = c(
    "game_list",
    "jumble_checklist", "jumble_wordlist",
    "spelling_bee_wordlist"
  )
)
