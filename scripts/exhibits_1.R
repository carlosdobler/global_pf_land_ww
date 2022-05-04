# source("~/00-mount.R")
source("scripts/00_setup.R")
source("scripts/functions.R")


# LAND MASK ---------------------------------------------------------------------------------------

c(st_point(c(-180, -90)),
  st_point(c(180, 90))) %>% 
  st_bbox() %>% 
  st_set_crs(4326) %>% 
  st_as_stars(dx = 0.05, dy = 0.05, values = -9999) -> rast_ref_0.05

c(st_point(c(-180, -90)),
  st_point(c(180, 90))) %>% 
  st_bbox() %>%
  st_set_crs(4326) %>% 
  st_as_stars(dx = 0.2, dy = 0.2, values = -9999) -> rast_ref_remo

"~/bucket_mine/misc_data/ne_50m_land/ne_50m_land.shp" %>% 
  st_read() %>% 
  mutate(a = 1) %>% 
  select(a) %>% 
  st_rasterize(rast_ref_0.05) -> land

land %>% 
  st_warp(rast_ref_remo, use_gdal = T, method = "mode") %>% 
  setNames("a") %>% 
  mutate(a = ifelse(a == -9999, "ocean", "land") %>% factor()) -> land

land %>% 
  st_set_dimensions(c(1,2), names = c("lon", "lat")) -> land


# *****************

tribble(
  ~m, ~deg1, ~deg1.5, ~deg2, ~deg2.5, ~deg3,
  "Had", 2014, 2030, 2035, 2048, 2057,
  "MPI", 2006, 2025, 2038, 2050, 2062,
  "Nor", 2019, 2033, 2050, 2061, 2073
) -> tb_mod_thres


# *****************

dom <- "EUR" # ARG
var <- c("gsl", )

# plan(multicore, workers = 4)

map(c("deg1", "deg2", "deg3"), function(deg){
  
  print(str_glue("Processing  degree {deg}"))
  
  imap(c(Had = 1, MPI = 2, Nor = 3), function(mod, i){
    
    print(str_glue("   Model {i}"))
    
    "~/bucket_mine/results/global_climchgeindices_ww/" %>% 
      list.files(full.names = T) %>% 
      .[str_detect(., dom)] %>% 
      .[str_detect(., "gsl")] %>% 
      
      .[mod] %>% 
      
      read_ncdf() %>% 
      suppressMessages() -> gsl
    
    land %>% 
      st_crop(gsl) %>% 
      suppressMessages() -> land_crop
    
    gsl[land_crop == "ocean"] <- NA
    
    
    # *****************
    
    gsl %>% 
      filter(year(time) >= tb_mod_thres %>% filter(m == i) %>% pull(deg) %>% {.-10},
             year(time) <= tb_mod_thres %>% filter(m == i) %>% pull(deg) %>% {.+10}) -> gsl_d
    
    gsl_d %>% 
      st_apply(c(1,2),
               
               # gev_level_lmom, q = 0.05,
               # .fname = "func", 
               
               mean,
               
               FUTURE = F) %>% 
      setNames(i) -> gsl_p
    
    return(gsl_p)
    
  }) -> gsl_models
  
  gsl_models %>% 
    unname() %>% 
    do.call(c, .) %>% 
    merge(name = "model") %>% 
    
    st_apply(c(1,2),
             mean,
             .fname = deg)
  
}) -> gsl_f

gsl_f %>% 
  do.call(c, .) -> gsl_f


gsl_f %>% 
  mutate(diff_2_1 = deg2 - deg1,
         diff_3_1 = deg3 - deg1) %>% 
  
  select(5) %>% 
  mapview::mapview()


# *************************************************************************************************

dom <- "SAM"

dir_down <- "~/pers_disk/pr_EUR_MPI/"

str_glue("~/bucket_risk/RCM_regridded_data/REMO2015/{dom}/daily/precipitation/") %>%
  list.files() %>% 
  .[str_detect(., "MPI")] %>% 
  .[str_length(.) > 80] %>% 
  {str_glue("~/bucket_risk/RCM_regridded_data/REMO2015/{dom}/daily/precipitation/{.}")} -> list_files

plan(multicore, workers = 7)

list_files %>% 
  future_walk(
        ~file.copy(.x,
                   dir_down)
  )



# ******

# after tiling

# chunks_ind
    
lon_ch <- 5 # inc to east
lat_ch <- 4 # inc to north

imap(list_files, function(f, i){
  
  tic(str_glue("   Imported {i} / {length(list_files)}"))
  
  read_ncdf(f, ncsub = cbind(start = c(lon_chunks[[lon_ch]][1],
                                       lat_chunks[[lat_ch]][1],
                                       1),
                             count = c(lon_chunks[[lon_ch]][2] - lon_chunks[[lon_ch]][1] +1,
                                       lat_chunks[[lat_ch]][2] - lat_chunks[[lat_ch]][1] +1,
                                       NA))) %>%
    suppressMessages() -> s_imp
  
  toc() # 5
  
  return(s_imp)
  
}) -> s

s %>%
  do.call(c, .) -> s

s %>%
  mutate(pr = pr %>% set_units(kg/m^2/d)) -> s
    
s %>% 
  st_get_dimension_values("time") %>% 
  as_date() -> dates


# ********

# 1 pixel

s %>% 
  slice(lon, 25) %>% slice(lat, 25) %>% pull(1) %>% as.vector() -> x

qhat <- mean(x)

tibble(pr = x,
       time = dates) %>% 
  
  filter(!(month(time) == 2 & day(time) == 29)) %>% 
  
  mutate(doy = yday(time)) %>% 
  filter(doy != 366) %>%
  
  mutate(doy = ifelse(doy >= 182, doy - 181, doy + 184)) %>%
  
  group_by(doy) %>% 
  summarize(doy_mean_pr = mean(pr)) %>% 
  mutate(anomaly = doy_mean_pr - qhat,
         cumanom = cumsum(anomaly)) -> tb

  
tb %>%
  select(-cumanom) %>% 
  pivot_longer(-doy, names_to = "var", values_to = "mm") %>% 
  
  ggplot(aes(x = doy, y = mm, color = var, group = var)) +
  geom_line()


tibble(pr = x,
       time = dates) %>% 
  
  filter(year(time) == 1972) %>% 
  
  mutate(doy = yday(time),
         anomaly = pr - qhat,
         cum = cumsum(anomaly)) %>% 
  
  filter(cum == max(cum))

# paper ref: /media/cdobler/Neobxbaumia/Lit_all/Cutter/Methods_2/Precipitation analysis
# script: /home/cdobler/Documents/Insync_ciga/Research/Project_calakmul_social_diff
  

