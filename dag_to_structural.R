library(tidyverse)
library(igraph)

dag_gathered <- 
readLines("dag.txt") %>% 
  tibble(equation = .) %>% 
  separate(equation, c("dv", "iv"), sep = "<=") %>% 
  mutate(iv = map(iv, ~ .x %>% str_split(",") %>% .[[1]])) %>% 
  unnest(iv) %>% 
  mutate_all(.funs = funs(str_trim(.)))

order_df <- 
dag_gathered %>% select(iv,dv) %>% igraph::graph_from_data_frame() %>% topological.sort() %>% as_ids() %>% 
  keep(~ . %in% dag_gathered$dv) %>% 
  tibble(dv =  .) %>% 
  mutate(order = 1:nrow(.)) %>% 
  arrange(order) %>% filter(dv != "")
 

equation_df <- 
  readLines("dag.txt") %>% 
    tibble(equation = .) %>% 
    separate(equation, c("dv", "iv"), sep = "<=") %>% 
    mutate(iv = map(iv, ~ .x %>% str_split(",") %>% .[[1]])) %>% 
    unnest(iv) %>% 
    mutate_all(.funs = funs(str_trim(.))) %>% 
    left_join(order_df) %>%  
    filter(iv != "") %>% 
    mutate_all(funs(str_pad(string = .,width = max(nchar(.)),side = "right",pad = " "))) %>% 
    mutate(coefficient = paste("b",iv %>% str_remove_all("_|[:space:]"),dv %>% str_remove_all("_|[:space:]"),sep ="_")) %>% 
    mutate(term = ifelse(str_detect(coefficient,"[a-z]"), paste(coefficient,iv,sep = "*"),"")) %>% 
    mutate(term = str_pad(string = term,width = max(nchar(term)),side = "right",pad = " ")) %>% 
    group_by(dv) %>% 
    summarise(term = paste(term, collapse  = "+ ") %>% str_trim() %>% str_remove("\\*$")) %>% 
  mutate(term  = str_pad(string = term,width = max(nchar(term)),side = "right",pad = " ") %>% paste0(.," %>%  rnorm(n = 1.00, mean = ., sd = 0.00)"))

exog_equation_df <- 
readLines("dag.txt") %>% 
  tibble(equation = .) %>% 
  separate(equation, c("dv", "iv"), sep = "<=") %>% 
  mutate(iv = map(iv, ~ .x %>% str_split(",") %>% .[[1]])) %>% 
  unnest(iv) %>% 
  mutate_all(.funs = funs(str_trim(.))) %>% 
  filter(iv == "")

endog_vars <- equation_df$dv %>% str_trim()

parameter_df <- 
c(exog_equation_df$dv,equation_df$term) %>% str_remove_all("\\%\\>\\%.+") %>%  str_trim() %>% str_split("\\*|\\+") %>% unlist() %>% str_trim() %>% keep( ~ !. %in% endog_vars) %>% unique() %>% 
  tibble(term = .) %>% 
  mutate(term  = str_pad(string = term,width = max(nchar(term)),side = "right",pad = " ") %>% paste0(.," = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)"))

equation_df_01 <- 
equation_df %>% 
  mutate(term = paste(dv,term, sep = " = " )) %>% 
  select(-dv)

transition_df <-  
full_df$term %>% str_remove_all(" %>%.+") %>% str_remove_all(" = 1.00") %>% str_split("[:space:]|[\\+]|[\\=]|[\\*+]") %>% unlist %>% 
  keep(~ . != "" & !str_detect(.,"b")) %>% keep(str_detect(.,"0")) %>% unique() %>% 
  tibble(dv = .) %>% 
  mutate(iv = dv %>% str_replace_all("0","1")) %>% 
  mutate(term = paste0(dv," = ", iv)) %>% 
  select(term) %>% 
  mutate(order = 100)

full_df <- bind_rows(parameter_df,equation_df_01) 

library(magrittr)

separator_df <- tribble(
  ~order , ~term,
  -3     , "\n# Constant parameters ----------------------------------------------------------\n",
  -1     , "\n# Each period - Exogenous ------------------------------------------------------\n",
   0.5   , "\n# Each period - Endogenous -----------------------------------------------------\n",
  99     , "\n# Transition to next period -----------------------------------------------------\n"
)

full_df %>% mutate(dv = str_extract(term,"^[[a-z]|[:punct:]|[_]|[0-9]]+(?=[:space:])")) %>% 
  left_join(order_df) %>% 
  mutate(order = ifelse(is.na(order) & str_detect(term,"1\\.00 %>%"),-2,order)) %>% 
  mutate(order = ifelse(dv %in% exog_equation_df$dv, 0,order)) %>% 
  bind_rows(.,separator_df,transition_df ) %>% 
  arrange(order, term) %>% 
  pull(term) %>% 
  c("rm(list = ls())",.) %>% 
  paste(collapse = "\n") %T>% 
  cat() %>% 
  write_lines("structural.R")




  