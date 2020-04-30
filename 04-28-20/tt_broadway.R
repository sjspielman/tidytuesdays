library(tidyverse)
library(lubridate)
library(magrittr)
theme_set(theme_minimal())


# Read data and define Sondheim ----------------
grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
 

sondheim <- c("Sunday in the Park with George",
              "Gypsy",
              "West Side Story",
              "Company",
              "Follies",
              "Sweeney Todd",
              "Assassins",
              "A Funny Thing Happened on the Way to the Forum",
              "A Little Night Music",
              "Into the Woods",
              "The Frogs",
              "Pacific Overtures",
              "Passion")

# Remove non-Sondheim -------------------------
sondheim_expr <- paste(sondheim, collapse = "|")
lies <- c("The Will Rogers Follies",
          "Gypsy Passion",
          "Sid Caesar & Company")

grosses %>% 
  filter(str_detect(show, sondheim_expr)) %>%
  filter(!(show %in% lies)) %>%
  arrange(show) %>%
  mutate(show = case_when(show == "A Funny Thing Happened on the Way to the Forum" ~ "Forum",
                                        show == "Sunday in the Park with George" ~ "Sunday", 
                                        TRUE ~ show))-> sondheim_shows

sondheim_shows %>% 
  select(show) %>% 
  distinct() %>% 
  nrow() -> num_found
if (num_found != length(sondheim)) stop("Wrong shows") 


# How many times each? ---------------------------

## PROBLEM: Gypsy single run in several theatres. We're hacking it. NO APOLOGIES.
sondheim_shows %<>%
  mutate(year = year(week_ending)) %>%
  mutate(theatre = ifelse(show == "Gypsy" & year <=1991, "firstrun", theatre)) %>%
  group_by(show, theatre) %>%
  mutate(week_number = ifelse(show == "Gypsy" & year == 1989 & week_number == 22, 0, week_number)) %>%
  ungroup()
  
sondheim_shows %>%
  select(theatre, show, year) %>%
  distinct() %>%
  group_by(theatre, show) %>%
  filter(year == min(year)) %>%
  ungroup() %>%
  count(show) %>%
  ggplot(aes(x = fct_reorder(show,n,.desc=T), y = n)) + 
  geom_col(color = "#DA3D40", fill = "#8B3B74", alpha = 0.8) +
  labs(x = "Show", 
       y = "Count", 
       title = "Sondeim on Broadway Since 1985", 
       subtitle = "How many times has each show been put on?") +
  theme(axis.text.x = element_text(angle = 30, size = 7)) -> plot_show_counts



sondheim_shows %>%
  select(theatre, show, year, week_number, pct_capacity) %>%
  distinct() %>%
  group_by(theatre, show) %>%
  filter(year == min(year)) %>%
  filter(week_number == min(week_number)) %>%
  mutate(show_year = paste0(show, " (", year, ")")) %>%
  mutate(pct_capacity = 100 * mean(pct_capacity)) %>%
  ggplot(aes(x = fct_reorder(show_year, pct_capacity, .desc=T), y = pct_capacity)) + 
  geom_col(color = "#DA3D40", fill = "#8B3B74", alpha = 0.8) +
  labs(x = "Show", 
       y = "Theatre percent capacity on opening week", 
       title = "Sondeim on Broadway Since 1985") +
  coord_flip() -> plot_capacity

ggsave("plot_show_counts.png", plot_show_counts, width = 6, height =4.5)
ggsave("plot_capacity.png", plot_capacity, width = 5, height =7)
