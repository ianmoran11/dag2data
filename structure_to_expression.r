library(furrr);library(tidyverse);library(magrittr)
list.files("./functions/",full.names = T) %>% map(source)

structure_text_df <- 
  read_lines("./structural_edit.R") %>% 
  tibble(term = .) %>% 
  mutate(group = cumsum(str_detect(term,"^# "))) %>% 
  mutate(group_name= str_extract(term,"^#.+") %>% str_remove_all("#|\\-+") %>% str_trim() %>% ifelse(row_number() == 1, "first",.) %>%  zoo::na.locf()) %>% 
  filter(!str_detect(term,"^#") & term != "" &  term != "rm(list = ls())")

structure_text_df_01 <- 
  structure_text_df %>% 
  mutate(term = map(term,~ rlang::parse_expr(.x))) %>% 
  group_by(group,group_name) %>% 
  summarise(expressions = list(term))

plan(multisession, workers = 7)

first_run <- c(structure_text_df_01$expressions) %>% unlist() %>% c(.,expression(as_tibble(as.list(e))))
subsequent_runs <- rep(c(unlist(structure_text_df_01$expressions[-1]),expression(as_tibble(as.list(e)))),10)

t1 <- Sys.time()
result <-  evaluate_runs(first_run,subsequent_runs,1000)
t2 <- Sys.time()
t2 - t1

result %>%
  group_by(id) %>% mutate(growth = y.t1 - lag(y.t1,1)) %>% View

result %>%  
  ggplot(aes(x = t, y = exp(y.t1), group = id)) + geom_line(alpha = .30)

result %>% group_by(id) %>% mutate(growth = y.t1 - lag(y.t1,1)) %$% qplot(growth) 

result %>%  
  ggplot(aes(x = l.t1)) + geom_histogram()