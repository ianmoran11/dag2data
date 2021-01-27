add_term <- function(list1, new_term_exp) {
  
  var_name <- new_term_exp %>%
    names() %>%
    keep(. != "") %>%
    .[[1]]

  new_list <- list1 %$% c(., eval(new_term_exp))

  to_drop <- which(names(new_list) == var_name) %>% .[-length(.)]
  
  browser()
  
  if(length(to_drop)>0){
    
    return(new_list[-to_drop])
    
  }else{
  
    return(new_list)  
    
  }
  
}