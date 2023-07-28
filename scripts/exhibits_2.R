# source("~/00-mount.R")

source("scripts/00_setup.R")
library(colorspace)
library(patchwork)


"~/bucket_mine/misc_data/ne_110m_land/ne_110m_land.shp" %>% 
  st_read() -> land

sample(1560,1) -> d

seq(as_date("1970-01-01"), as_date("2099-12-31"), by = "1 month") %>% 
  .[d] -> dd

# spei
"~/bucket_mine/results/global_spei_ww/NAM_Had_monthly_pdsi-06_1970_2099_cal0p5.nc" %>% 
  read_ncdf(ncsub = cbind(start = c(1,1,d),
                          count = c(NA,NA,1))) %>% 
  mutate(pdsi = case_when(pdsi < -5 ~ -5,
                          pdsi > 5 ~ 5,
                          TRUE ~ pdsi)) -> NAM_Had_pdsi

"~/bucket_mine/results/global_spei_ww/NAM_MPI_monthly_pdsi-06_1970_2100_cal0p5.nc" %>% 
  read_ncdf(ncsub = cbind(start = c(1,1,d),
                          count = c(NA,NA,1))) %>% 
  mutate(pdsi = case_when(pdsi < -5 ~ -5,
                          pdsi > 5 ~ 5,
                          TRUE ~ pdsi)) -> NAM_MPI_pdsi

"~/bucket_mine/results/global_spei_ww/NAM_Nor_monthly_pdsi-06_1970_2100_cal0p5.nc" %>% 
  read_ncdf(ncsub = cbind(start = c(1,1,d),
                          count = c(NA,NA,1))) %>% 
  mutate(pdsi = case_when(pdsi < -5 ~ -5,
                          pdsi > 5 ~ 5,
                          TRUE ~ pdsi)) -> NAM_Nor_pdsi
  
c(adrop(NAM_Had_pdsi),
  adrop(NAM_MPI_pdsi),
  adrop(NAM_Nor_pdsi)) %>% 
  merge(name = "model") %>% 
  st_apply(c(1,2), mean, na.rm = T, .fname = "pdsi") -> NAM_ens_pdsi


# pdsi
"~/bucket_mine/results/global_spei_ww/NAM_Had_monthly_spei-06_1970_2099_cal0p5.nc" %>% 
  read_ncdf(ncsub = cbind(start = c(1,1,d),
                          count = c(NA,NA,1))) %>% 
  mutate(spei = case_when(spei < -2 ~ -2,
                          spei > 2 ~ 2,
                          TRUE ~ spei)) -> NAM_Had_spei

"~/bucket_mine/results/global_spei_ww/NAM_MPI_monthly_spei-06_1970_2100_cal0p5.nc" %>% 
  read_ncdf(ncsub = cbind(start = c(1,1,d),
                          count = c(NA,NA,1))) %>% 
  mutate(spei = case_when(spei < -2 ~ -2,
                          spei > 2 ~ 2,
                          TRUE ~ spei)) -> NAM_MPI_spei

"~/bucket_mine/results/global_spei_ww/NAM_Nor_monthly_spei-06_1970_2100_cal0p5.nc" %>% 
  read_ncdf(ncsub = cbind(start = c(1,1,d),
                          count = c(NA,NA,1))) %>% 
  mutate(spei = case_when(spei < -2 ~ -2,
                          spei > 2 ~ 2,
                          TRUE ~ spei)) -> NAM_Nor_spei

c(adrop(NAM_Had_spei),
  adrop(NAM_MPI_spei),
  adrop(NAM_Nor_spei)) %>% 
  merge(name = "model") %>% 
  st_apply(c(1,2), mean, na.rm = T, .fname = "spei") -> NAM_ens_spei


# plot
ggplot() +
  geom_stars(data = NAM_ens_pdsi) +
  geom_sf(data = land, fill = NA) +
  coord_sf(xlim = c(-165, -38), ylim = c(15,75)) +
  scale_fill_continuous_divergingx("Spectral",
                                   na.value = "transparent") +
  theme(axis.title = element_blank()) -> plot_1

ggplot() +
  geom_stars(data = NAM_ens_spei) +
  geom_sf(data = land, fill = NA) +
  coord_sf(xlim = c(-165, -38), ylim = c(15,75)) +
  scale_fill_continuous_divergingx("Spectral",
                                   na.value = "transparent") +
  theme(axis.title = element_blank()) -> plot_2

wrap_plots(plot_1, plot_2, nrow = 1) + plot_annotation(title = dd)



# frequencies

# pdsi

# had
"~/bucket_mine/results/global_spei_ww/NAM_Had_monthly_pdsi-06_1970_2099_cal0p5.nc" %>% 
  read_ncdf() %>% 
  mutate(pdsi = case_when(pdsi < -5 ~ -5,
                          pdsi > 5 ~ 5,
                          TRUE ~ pdsi)) -> NAM_Had_pdsi

NAM_Had_pdsi %>% 
  filter(year(time) >= 2014-10,
         year(time) <= 2014+10) -> NAM_Had_pdsi_1p0

NAM_Had_pdsi %>% 
  filter(year(time) >= 2057-10,
         year(time) <= 2057+10) -> NAM_Had_pdsi_3p0

NAM_Had_pdsi_1p0 %>% 
  st_apply(c(1,2), function(x){
    
    sum(x <= -4)/252
    
  }, .fname = "prob") -> NAM_Had_pdsi_1p0_prob_extr

NAM_Had_pdsi_3p0 %>% 
  st_apply(c(1,2), function(x){
    
    sum(x <= -4)/252
    
  }, .fname = "prob") -> NAM_Had_pdsi_3p0_prob_extr


# mpi
"~/bucket_mine/results/global_spei_ww/NAM_MPI_monthly_pdsi-06_1970_2100_cal0p5.nc" %>% 
  read_ncdf() %>% 
  mutate(pdsi = case_when(pdsi < -5 ~ -5,
                          pdsi > 5 ~ 5,
                          TRUE ~ pdsi)) %>% 
  slice(time, 1:1560) -> NAM_MPI_pdsi

NAM_MPI_pdsi %>% 
  filter(year(time) >= 2006-10,
         year(time) <= 2006+10) -> NAM_MPI_pdsi_1p0

NAM_MPI_pdsi %>% 
  filter(year(time) >= 2062-10,
         year(time) <= 2062+10) -> NAM_MPI_pdsi_3p0

NAM_MPI_pdsi_1p0 %>% 
  st_apply(c(1,2), function(x){
    
    sum(x <= -4)/252
    
  }, .fname = "prob") -> NAM_MPI_pdsi_1p0_prob_extr

NAM_MPI_pdsi_3p0 %>% 
  st_apply(c(1,2), function(x){
    
    sum(x <= -4)/252
    
  }, .fname = "prob") -> NAM_MPI_pdsi_3p0_prob_extr


# nor
"~/bucket_mine/results/global_spei_ww/NAM_Nor_monthly_pdsi-06_1970_2100_cal0p5.nc" %>% 
  read_ncdf() %>% 
  mutate(pdsi = case_when(pdsi < -5 ~ -5,
                          pdsi > 5 ~ 5,
                          TRUE ~ pdsi)) %>%  
  slice(time, 1:1560) -> NAM_Nor_pdsi

NAM_Nor_pdsi %>% 
  filter(year(time) >= 2019-10,
         year(time) <= 2019+10) -> NAM_Nor_pdsi_1p0

NAM_Nor_pdsi %>% 
  filter(year(time) >= 2073-10,
         year(time) <= 2073+10) -> NAM_Nor_pdsi_3p0

NAM_Nor_pdsi_1p0 %>% 
  st_apply(c(1,2), function(x){
    
    sum(x <= -4)/252
    
  }, .fname = "prob") -> NAM_Nor_pdsi_1p0_prob_extr

NAM_Nor_pdsi_3p0 %>% 
  st_apply(c(1,2), function(x){
    
    sum(x <= -4)/252
    
  }, .fname = "prob") -> NAM_Nor_pdsi_3p0_prob_extr


# prob 1 deg
c(NAM_Had_pdsi_1p0_prob_extr,
  NAM_MPI_pdsi_1p0_prob_extr,
  NAM_Nor_pdsi_1p0_prob_extr) %>% 
  merge(name = "model") %>% 
  st_apply(c(1,2), mean, na.rm = T, .fname = "prob") -> NAM_ens_pdsi_1p0_prob_extr

# prob 3 deg
c(NAM_Had_pdsi_3p0_prob_extr,
  NAM_MPI_pdsi_3p0_prob_extr,
  NAM_Nor_pdsi_3p0_prob_extr) %>% 
  merge(name = "model") %>% 
  st_apply(c(1,2), mean, na.rm = T, .fname = "prob") -> NAM_ens_pdsi_3p0_prob_extr


 
# spei

# had
"~/bucket_mine/results/global_spei_ww/NAM_Had_monthly_spei-06_1970_2099_cal0p5.nc" %>% 
  read_ncdf() %>% 
  mutate(spei = case_when(spei < -2 ~ -2,
                          spei > 2 ~ 2,
                          TRUE ~ spei)) -> NAM_Had_spei

NAM_Had_spei %>% 
  filter(year(time) >= 2014-10,
         year(time) <= 2014+10) -> NAM_Had_spei_1p0

NAM_Had_spei %>% 
  filter(year(time) >= 2057-10,
         year(time) <= 2057+10) -> NAM_Had_spei_3p0

NAM_Had_spei_1p0 %>% 
  st_apply(c(1,2), function(x){
    
    sum(x <= -1.6)/252
    
  }, .fname = "prob") -> NAM_Had_spei_1p0_prob_extr

NAM_Had_spei_3p0 %>% 
  st_apply(c(1,2), function(x){
    
    sum(x <= -1.6)/252
    
  }, .fname = "prob") -> NAM_Had_spei_3p0_prob_extr


# mpi
"~/bucket_mine/results/global_spei_ww/NAM_MPI_monthly_spei-06_1970_2100_cal0p5.nc" %>% 
  read_ncdf() %>% 
  mutate(spei = case_when(spei < -2 ~ -2,
                          spei > 2 ~ 2,
                          TRUE ~ spei)) %>% 
  slice(time, 1:1560) -> NAM_MPI_spei

NAM_MPI_spei %>% 
  filter(year(time) >= 2006-10,
         year(time) <= 2006+10) -> NAM_MPI_spei_1p0

NAM_MPI_spei %>% 
  filter(year(time) >= 2062-10,
         year(time) <= 2062+10) -> NAM_MPI_spei_3p0

NAM_MPI_spei_1p0 %>% 
  st_apply(c(1,2), function(x){
    
    sum(x <= -1.6)/252
    
  }, .fname = "prob") -> NAM_MPI_spei_1p0_prob_extr

NAM_MPI_spei_3p0 %>% 
  st_apply(c(1,2), function(x){
    
    sum(x <= -1.6)/252
    
  }, .fname = "prob") -> NAM_MPI_spei_3p0_prob_extr


# nor
"~/bucket_mine/results/global_spei_ww/NAM_Nor_monthly_spei-06_1970_2100_cal0p5.nc" %>% 
  read_ncdf() %>% 
  mutate(spei = case_when(spei < -2 ~ -2,
                          spei > 2 ~ 2,
                          TRUE ~ spei)) %>%  
  slice(time, 1:1560) -> NAM_Nor_spei

NAM_Nor_spei %>% 
  filter(year(time) >= 2019-10,
         year(time) <= 2019+10) -> NAM_Nor_spei_1p0

NAM_Nor_spei %>% 
  filter(year(time) >= 2073-10,
         year(time) <= 2073+10) -> NAM_Nor_spei_3p0

NAM_Nor_spei_1p0 %>% 
  st_apply(c(1,2), function(x){
    
    sum(x <= -1.6)/252
    
  }, .fname = "prob") -> NAM_Nor_spei_1p0_prob_extr

NAM_Nor_spei_3p0 %>% 
  st_apply(c(1,2), function(x){
    
    sum(x <= -1.6)/252
    
  }, .fname = "prob") -> NAM_Nor_spei_3p0_prob_extr


# prob 1 deg
c(NAM_Had_spei_1p0_prob_extr,
  NAM_MPI_spei_1p0_prob_extr,
  NAM_Nor_spei_1p0_prob_extr) %>% 
  merge(name = "model") %>% 
  st_apply(c(1,2), mean, na.rm = T, .fname = "prob") -> NAM_ens_spei_1p0_prob_extr

# prob 3 deg
c(NAM_Had_spei_3p0_prob_extr,
  NAM_MPI_spei_3p0_prob_extr,
  NAM_Nor_spei_3p0_prob_extr) %>% 
  merge(name = "model") %>% 
  st_apply(c(1,2), mean, na.rm = T, .fname = "prob") -> NAM_ens_spei_3p0_prob_extr


# plot
c(NAM_ens_pdsi_1p0_prob_extr,
  NAM_ens_pdsi_3p0_prob_extr,
  NAM_ens_spei_1p0_prob_extr,
  NAM_ens_spei_3p0_prob_extr) %>% 
  setNames(c("pdsi_1", "pdsi_3", "spei_1", "spei_3")) %>% 
  as_tibble() %>% 
  pivot_longer(-c(lon,lat)) %>% 
  {
    ggplot() +
      geom_raster(data = ., aes(x = lon, y = lat, fill = value)) +
      geom_sf(data = land, fill = NA, color = "white", size = 0.3) +
      coord_sf(xlim = c(-165, -38), ylim = c(15,75)) +
      scale_fill_binned_sequential("Plasma",
                                   rev = F,
                                   na.value = "transparent",
                                   n.breaks = 7
                                   ) +
      theme(axis.title = element_blank()) +
      facet_wrap(~name, ncol = 2)
  }


















# comparison of pet
"~/bucket_mine/results/global_spei_ww/NAM_Had_monthly_pet_1970_2099.nc" %>% 
  read_ncdf(ncsub = cbind(start = c(1,1,d),
                          count = c(NA,NA,1))) %>% 
  adrop() -> pet_mine

"~/bucket_risk/RCM_regridded_data/REMO2015/NAM/monthly/potential_evapotranspiration/pet_NAM_HadGEM2-ES_1970_2100.nc" %>% 
  read_ncdf(make_units = F,
            ncsub = cbind(start = c(1,1,d),
                          count = c(NA,NA,1))) %>% 
  adrop() -> pet_ir

ggplot() +
  geom_stars(data = pet_mine) +
  geom_sf(data = land, fill = NA) +
  coord_sf(xlim = c(-165, -38), ylim = c(15,75)) +
  scale_fill_continuous_sequential("Viridis",
                                   na.value = "transparent") +
  theme(axis.title = element_blank()) -> plot_1

ggplot() +
  geom_raster(data = pet_ir %>% as_tibble(), aes(x = lon, y = lat, fill = pet)) +
  geom_sf(data = land, fill = NA) +
  coord_sf(xlim = c(-165, -38), ylim = c(15,75)) +
  scale_fill_continuous_sequential("Viridis",
                                   na.value = "transparent") +
  theme(axis.title = element_blank()) -> plot_2

wrap_plots(plot_1, plot_2, nrow = 1) + plot_annotation(title = dd)


# comparison of pdsi
"~/bucket_mine/results/global_spei_ww/NAM_Had_monthly_pdsi-06_1970_2099_cal0p5.nc" %>% 
  read_ncdf(ncsub = cbind(start = c(1,1,d),
                          count = c(NA,NA,1))) %>% 
  adrop() %>% 
  mutate(pdsi = case_when(pdsi < -5 ~ -5,
                          pdsi > 5 ~ 5,
                          TRUE ~ pdsi)) -> pdsi_mine

"~/bucket_risk/RCM_regridded_data/REMO2015/NAM/monthly/palmer_drought_severity_index/pdsi_NAM_HadGEM2-ES_1970_2099.nc" %>% 
  read_ncdf(make_units = F,
            make_time = F,
            ncsub = cbind(start = c(1,1,d),
                          count = c(NA,NA,1))) %>% 
  adrop() %>% 
  mutate(PDSI = case_when(PDSI < -5 ~ -5,
                          PDSI > 5 ~ 5,
                          TRUE ~ PDSI)) -> pdsi_ir

ggplot() +
  geom_stars(data = pdsi_mine) +
  geom_sf(data = land, fill = NA) +
  coord_sf(xlim = c(-165, -38), ylim = c(15,75)) +
  scale_fill_continuous_divergingx("Spectral",
                                   na.value = "transparent") +
  theme(axis.title = element_blank()) -> plot_1

ggplot() +
  geom_raster(data = pdsi_ir %>% as_tibble(), aes(x = lon, y = lat, fill = PDSI)) +
  geom_sf(data = land, fill = NA) +
  coord_sf(xlim = c(-165, -38), ylim = c(15,75)) +
  scale_fill_continuous_divergingx("Spectral",
                                   na.value = "transparent") +
  theme(axis.title = element_blank()) -> plot_2

wrap_plots(plot_1, plot_2, nrow = 1) + plot_annotation(title = dd)






