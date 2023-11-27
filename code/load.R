#load packages
packages = c(
  "here",
  "feather",
  "lubridate",
  "tidyverse",
  "XML"
)

sapply(packages, require, character.only = TRUE, quietly = TRUE)

#make and apply a simple ggplot theme
my_theme <- function() {
  theme_classic() +
  theme(
    axis.ticks = element_blank(),
    text = element_text(family = "Helvetica"),
    plot.margin = unit(c(.1,.6,.1,.1), 'cm')
    )
}

theme_set(my_theme())

#update a text file with today's date, used to keep everything up to date.
Sys.Date() |>
  as.character() |>
  writeLines(file(here("text/today.txt")))
