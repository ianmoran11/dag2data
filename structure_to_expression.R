library(tidyverse)
library(magrittr)
list.files("functions",full.names = T) %>% map(source)

structure_text_df <- 
  read_lines("./structural_edit.R") %>% 
  tibble(term = .) %>% 
  mutate(group = cumsum(str_detect(term,"^# "))) %>% 
  mutate(group_name= str_extract(term,"^#.+") %>% str_remove_all("#|\\-+") %>% str_trim() %>% ifelse(row_number() == 1, "first",.) %>%  zoo::na.locf()) %>% 
  filter(!str_detect(term,"^#") & term != "" &  term != "rm(list = ls())") %>% 
  mutate(term = term %>% str_remove_all("#.+"))  

structure_text_df_01 <- 
  structure_text_df %>% 
  mutate(term = map(term,~ rlang::parse_expr(paste0("list(",.x,")")))) %>% 
  group_by(group,group_name) %>% 
  summarise(expressions = list(term))



sim_data <- 
  map_dfr(
    1:100,
    function(var) {
      
      expr_df <- 
        bind_rows(
          tibble(exprs = list(append(list(list(x = 0)), structure_text_df_01$expressions %>% unlist()) %>% reduce(add_term) )),
          filter(structure_text_df_01, group >1) %>% pull(expressions) %>% unlist() %>% list() %>%  rep(x = ., 20) %>% tibble(exprs = . )
        )
      
      expr_df$exprs %>%
        accumulate(next_period) %>%
        map(as_tibble) %>%
        bind_rows() %>%
        mutate(id = var) %>%
        mutate(t = row_number())
    }
  )


sim_data %>% 
  ggplot(aes(x = t, y = y.t1, group = id)) + geom_line()

