remotes::install_github("rossellhayes/words")

library(dplyr)
library(tibble)
library(words)

jumble_wordlist <- words::lemmas_frq %>%
  select(-variant) %>%
  filter(nchar(lemma) >= 3, !grepl("[^a-z]", lemma), !lemma %in% offensive)

jumble_checklist <- words::words %>%
  enframe(name = NULL, value = "word") %>%
  mutate(split = strsplit(word, "")) %>%
  rowwise() %>%
  mutate(letters = paste(sort(split), collapse = ""))

spelling_bee_wordlist <- tibble(word = words::words) %>%
  mutate(
    letters = word %>%
      strsplit("") %>%
      purrr::map_chr(~ paste(sort(unique(.)), collapse = ""))
  ) %>%
  filter(nchar(word) >= 4, nchar(letters) <= 7) %>%
  mutate(
    score = (nchar(word) == 4) +
      ((nchar(word) > 4) * nchar(word)) +
      ((nchar(letters) == 7) * 14),
    offensive = word %in% offensive
  )
