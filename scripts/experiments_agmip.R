
source("scripts/00_setup.R")
library(colorspace)

"~/bucket_risk/AgMIP/co2/" %>% 
  list.files(full.names = T) %>% 
  .[str_detect(., "noirr")] %>% 
  .[str_detect(., "maize")] %>% 
  .[str_detect(., "(epic|pdssat|pegasus)")] %>% 
  
  map(read_ncdf, make_time = F) -> s

tribble(
  ~model, ~wl, ~yr,
  "gfdl", "1", 2020,
  "gfdl", "2", 2053,
  "had", "1", 2014,
  "had", "2", 2035,
  "ipsl", "1", 1997,
  "ipsl", "2", 2028,
  "miroc", "1", 2009,
  "miroc", "2", 2030,
  "nor", "1", 2019,
  "nor", "2", 2050) -> mid_yr
  


map(c(1,2), function(w){
  
  mid_yr %>% 
    pull(model) %>% 
    unique() %>% 
    
    map(function(mod){
      
      map(s, function(cm){
        
        cm %>% 
          select(contains(mod)) %>% 
          filter(time >= mid_yr %>% filter(model == mod, wl == w) %>% pull(yr) %>% {.-10},
                 time <= mid_yr %>% filter(model == mod, wl == w) %>% pull(yr) %>% {.+10}) %>% 
          st_set_dimensions("time", names = "t", values = rep(NA, 21)) %>% 
          setNames("yield")
        
      }) %>% 
        {do.call(c, c(., along = 3))}
      
    }) %>% 
    {do.call(c, c(., along = 3))}
  
}) -> s_ens



# mean yield at 1 degree
s_ens %>% 
  map(st_apply, c(1,2), median, na.rm = T, .fname = "yield") -> mean_yield # na.rm: some layers have NAs
  

# classes
mean_yield %>%
  pluck(1) %>% 
  pull(1) %>% 
  as.vector() %>% 
  quantile(c(0, 0.5, 0.75, 1), na.rm = T) -> bp

mean_yield %>%
  map(function(sy){
    
    sy %>% 
      mutate(suitability = case_when(is.na(yield) ~ NA_character_,
                                     yield > bp[3] ~ "optimal",
                                     yield > bp[2] ~ "high",
                                     yield > bp[1] ~ "marginal",
                                     TRUE ~ "unsuitable") %>% 
               fct_inorder())
    
  }) -> s_suitability

s_suitability %>%
  do.call(c, .) %>% 
  select(contains("suitability")) %>%
  setNames(c("1_deg", "2_deg")) %>%
  merge(name = "wl") %>% 
  as_tibble() %>% 
  
  ggplot(aes(longitude, latitude, fill = X)) +
  geom_raster() +
  coord_equal() +
  scale_fill_discrete_sequential("ag_GrnYl", name = "Suitability") +
  facet_wrap(~wl, ncol = 1) +
  labs(subtitle = "Climatic suitability to grow maize under different warming levels")


# raw change
mean_yield %>% 
  do.call(c, .) %>% 
  setNames(c("deg_1", "deg_2")) %>% 
  mutate(diff = deg_2 - deg_1) %>% 
  select(diff) -> s_diff_yield 

s_diff_yield %>%
  mutate(diff = case_when(diff < -4 ~ -4,
                          diff > 4 ~ 4,
                          TRUE ~ diff)) %>% 
  as_tibble() %>% 
  ggplot(aes(longitude, latitude, fill = diff)) +
  geom_raster() +
  coord_equal() +
  scale_fill_continuous_divergingx("Spectral",
                                   na.value = "transparent",
                                   name = "ton/ha") +
  labs(subtitle = "How maize average yields will change under 2 degrees relative to 1 degree")
  
  
# s_diff_yield %>% 
#   mutate(diff = case_when(diff > 2 ~ "High gain",
#                           diff > 0.5 ~ "Moderate gain",
#                           diff > -0.5 ~ "No change",
#                           diff > -2 ~ "Moderate loss",
#                           diff > -5 ~ "High loss"
#                           ) %>% 
#            factor(levels = c("High gain",
#                              "Moderate gain",
#                              "No change",
#                              "Moderate loss",
#                              "High loss"))) %>%
#   
#   as_tibble() %>% 
#   ggplot(aes(longitude, latitude, fill = diff)) +
#   geom_raster() +
#   coord_equal() +
#   scale_fill_discrete_divergingx("Spectral",
#                                  na.value = "transparent",
#                                  rev = T)




# ECDF
s_ens %>% 
  pluck(1) %>% 
  slice(longitude, sample(720,1)) %>% 
  slice(latitude, sample(360,1)) %>% 
  pull(1) %>% 
  as.vector() -> x

s_ens %>% 
  pluck(1) %>% 
  st_apply(c(1,2), function(x){
    
    quantile(x, 0.2, na.rm = T)
    
  }) -> s_deg1_failure_yield

s_deg1_failure_yield %>% 
  mutate(yield = ifelse(yield > 10, 10, yield)) %>% 
  as_tibble() %>% 
  ggplot(aes(longitude, latitude, fill = yield)) +
  geom_raster() +
  coord_equal() +
  scale_fill_continuous_sequential("Plasma",
                                   na.value = "transparent",
                                   rev = F) +
  labs(subtitle = "Crop failure yields under 1 degree")


s_ens %>% 
  pluck(2) %>%
  
  pull(1) %>% 
  abind(pull(s_deg1_failure_yield,1), along = 3) -> arr
  
names(dim(arr)) <- c("longitude", "latitude", "t")
st_as_stars(arr) -> s_t

s_t %>% 
  st_apply(c(1,2), function(x){
    
    if(sum(!is.na(x)) == 0){
      NA
    } else {
      last(x) -> q
      x[-length(x)] -> x
      ecdf(na.omit(x))(q)
    }
    
  },
  .fname = "prob") -> s_deg2_failure_prob

st_dimensions(s_deg2_failure_prob) <- st_dimensions(s_deg1_failure_yield)

s_deg2_failure_prob %>% 
  as_tibble() %>% 
  ggplot(aes(longitude, latitude, fill = prob)) +
  geom_raster() +
  coord_equal() +
  scale_fill_continuous_sequential("Viridis",
                                   na.value = "transparent",
                                   rev = F) +
  labs(subtitle = "Probability of experiencing crop failure under 2 degrees")
