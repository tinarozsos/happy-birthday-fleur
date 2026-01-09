library(tidyverse)
library(rvest)

people <- read_html("https://taskmaster.fandom.com/wiki/Contestants") |>
  html_elements("td a") |>
  html_text2() |>
  str_subset("^[A-Z]") |>
  c("Alex Horne", "Greg Davies", "Fred the Swede")

series <- paste0("https://taskmaster.fandom.com/wiki/Series_", 1:19)
episodes <- map(series, \(x) {
  read_html(x) |>
    html_elements("td > span > a") |>
    html_attr("href")
}) |>
  unlist() |>
  str_subset("/wiki/") |>
  str_replace("/wiki/", "https://taskmaster.fandom.com/wiki/")

eps_html <- map(episodes, read_html)

titles <- map_chr(eps_html, \(x) {
  html_element(x, ".mw-page-title-main") |>
    html_text()
}) |>
  str_remove("\\.$")

title_chars <- map_chr(eps_html, \(x) {
  html_element(x, "h2+ p") |>
    html_text()
})

title_pairs <- tibble(quote = titles, text = title_chars) |>
  mutate(name = str_extract(text, str_c(people, collapse = "|"))) |>
  filter(!is.na(name))
