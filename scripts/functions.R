
gev_level_lmom <- function(x, q){
  
  if(anyNA(x)){
    
    results <- c(val = NA,
                 gof = NA,
                 diff = NA)
    
  } else {
    
    # obtain dist parameters
    lmom::samlmu(x) %>% suppressWarnings() -> lmom
    try(lmom::pelgev(lmom), silent = T) -> params
    
    # **********
    
    if(class(params) == "try-error"){
      
      results <- c(val = NA,
                   gof = NA,
                   diff = NA)
      
    } else {
      
      # calculate levels 
      lmom::quagev(q, params) %>% 
        unname() -> val
      
      # calculate gof
      lmom::quagev(seq(0.01,0.99,0.01), params) %>%
        quantile(probs = seq(0,1, length.out = length(x))) -> p_fitted
      
      x %>%
        quantile(probs = seq(0,1, length.out = length(x))) -> p_observed
      
      cor(p_fitted, p_observed) -> gof
      
      # calculate diff
      x %>% 
        quantile(probs = q) %>% 
        unname() %>% 
        {. - val} -> diff
      
      # join
      results <- c(val = val,
                   gof = gof,
                   diff = diff)
      
    }
  }
  return(results)
}
