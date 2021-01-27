add_term <- function(list1, new_term_exp) {
  
  var_name <- new_term_exp %>%
    names() %>%
    keep(. != "") %>%
    .[[1]]

  new_list <- list1 %$% c(., eval(new_term_exp))

  to_drop <- which(names(new_list) == var_name) %>% .[-length(.)]
  
  # browser()
  
  if(length(to_drop)>0){
    
    new_list <- new_list[-to_drop]
    
    return(new_list)
    
  }else{
   
    new_list
    
    return(new_list)  
    
  }
  
}