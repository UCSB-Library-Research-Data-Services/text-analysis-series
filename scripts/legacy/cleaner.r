library(tidyverse)
library(tidytext)
library(stringr)
library(stringi)
library(dplyr)
library(qdap)
# install.packages("devtools")
# devtools::install_github("hadley/emo")
library(emo)
library(textstem)

emoji_dict <- emo::jis[, c("emoji", "name")]

comments <- readr::read_csv("data/raw/comments_variables.csv")

# Patterns
apostrophes <- "[‘’‛ʼ❛❜＇`´′]"
url_pattern <- regex("(?i)\\b(?:https?://|www\\.)\\S+\\b", ignore_case = TRUE)
mentions <- regex("@[A-Za-z0-9_]+")

validate_pattern_replacement <- function(dftibble, strpattern) {
  found_before <- any(str_detect(dftibble$text, strpattern), na.rm = TRUE)
  found_after <- any(str_detect(dftibble$clean_text, strpattern), na.rm = TRUE)

  if (!found_before) {
    message("Pattern not found in raw text.")
  } else if (!found_after) {
    message("Pattern was successfuly cleaned")
  } else {
    message(sprintf("Pattern is still present after cleaning."))
  }
}

replace_emojis <- function(text, emoji_dict) {
  stri_replace_all_fixed(
    str = text,                  # The text to process
    pattern = emoji_dict$emoji,  # The emojis to find
    replacement = paste0(emoji_dict$name, " "), # Their corresponding names
    vectorize_all = FALSE        # element-wise replacement in a same string
  )
}

comments_clean <- comments %>%
  mutate(
    clean_text = text %>%
      str_replace_all("\u00A0", " ") %>%
      str_replace_all(apostrophes, "'") %>%
      replace_contraction() %>%
      str_replace_all("#([A-Za-z])([A-Z])", "#\\1 \\2") %>%
      str_replace_all("(?<![#@])([a-z])([A-Z])", "\\1 \\2") %>%
      str_to_lower() %>%
      str_replace_all(url_pattern, " ") %>%
      str_replace_all(mentions, "") %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_replace_all("[[:digit:]]+", " ") %>%
      str_squish() %>%
      str_replace_all("(.)\\1{2,}", "\\1") %>%
      replace_emojis(emoji_dict)
  )

# Validate cleaning
validate_pattern_replacement(comments_clean, url_pattern)
validate_pattern_replacement(comments_clean, mentions)
validate_pattern_replacement(comments_clean, apostrophes)

# Tokenization
comments_tokens <- comments_clean %>%
  unnest_tokens(word, clean_text)

# Remove stop words
data("stop_words")
comments_tokens <- comments_tokens %>%
  anti_join(stop_words, by = "word")


# Save cleaned data
write_csv(comments_clean, "data/clean/comments_clean.csv")
write_csv(select(comments_tokens, id, sex, region, "emotion-bw", "sentiment-bw", word), "data/clean/comments_tokens.csv")
