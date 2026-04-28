library(tidyverse)

here::here()

fieldweather <- read_cs(here("data", "fieldd12weatherdata.csv"))


fieldweather <- read_csv("../data/fieldd12weatherdata.csv")

soil <- read_csv("../data/training/training_soil.csv")

trait <- read_csv("../data/training/training_trait.csv")

fieldweather
soil
trait


fieldsoil <- soil %>%
  separate(site, into = c("site", "year"), sep = "_") %>% 
  mutate(year = as.double(year)) #to keep the joining coulmn all in same data type in the different datset

fieldprp <- fieldweather %>% 
  left_join(fieldsoil, 
            by = c("year", "site")
            ) %>% 
  left_join(trait,
            by = c("year", "site")
            )

   

summary(fieldprp)

fieldprpty <- fieldprp %>% 
  dplyr::select(dayl_s:grain_moisture, -replicate, -block, -hybrid, -date_planted, -date_harvested)

knitr::purl("2_featureeng_partial.qmd", output = "2_sapelo_script.R", documentation = 0)


fieldprpty %>%
  pivot_longer(cols = dayl_s:grain_moisture) %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~name, scales = "free")


fieldweather

fe_month <- fieldweather %>%
  # Selecting needed variables
  dplyr::select(year, site, lat, lon,
                #strength_gtex,
                yday,
                dayl.s = dayl_s, 
                prcp.mm = prcp_mm_day,
                srad.wm2 = srad_w_m_2, 
                tmax.c = tmax_deg_c, 
                tmin.c = tmin_deg_c,
                vp.pa = vp_pa
                ) %>%
  # Creating a date class variable  
  mutate(date_chr = paste0(year, "/", yday)) %>%
  mutate(date = as.Date(date_chr, "%Y/%j")) %>%
  # Extracting month from date  
  mutate(month = month(date)) %>%
  mutate(month_abb = month(date, label = T))
  

#help(paste0)
fe_month

fe_month_sum <- fe_month %>%
  group_by(year, site, month_abb, strength_gtex) %>%
  summarise(across(.cols = c(dayl.s,
                             srad.wm2,
                             tmax.c,
                             tmin.c,
                             vp.pa
                             ),
                   .fns = mean, 
                   .names = "mean_{.col}"
                   ),
            across(.cols = prcp.mm,
                   .fns = sum,
                   .names = "sum_{.col}"
                   )
            ) %>%
  ungroup()
  


fe_month_sum

fe_month %>%
  filter(year == 1980 & 
           site == "Altus, OK" &
           month_abb == "Jan") %>%
  summarise(tmax.c = mean(tmax.c),
            prcp.mm = sum(prcp.mm)) #%>%
  

fe_month_sum_wide <- fe_month_sum %>%
  pivot_longer(mean_dayl.s:sum_prcp.mm)%>%
  mutate(varname = paste0(name, "_",month_abb)) %>%
  dplyr::select(-name, -month_abb) %>% #another way of excluding some column
  pivot_wider(names_from = varname,
              values_from = value
              ) %>%
  # Rounding to one decimal point
  mutate(across(c(3:75), ~round(., 1)))


fe_month_sum_wide  

#install.packages("ggridges")
library(ggridges)

ggplot(data = fe_month_sum,
       aes(x = mean_tmax.c,
           y = month_abb,
           fill = stat(x)
           )
       ) +
  geom_density_ridges_gradient(scale = 2,
                               rel_min_height = .01
                               ) +
  scale_fill_viridis_c(option = "C", alpha = .8) +
  theme(legend.position = "none") +
  labs(x = "Monthly-average maximum temp (C)",
       y = "Month"
       )

finalplots <- fe_month_sum %>%
  pivot_longer(mean_dayl.s:sum_prcp.mm) %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map2(data,
                     name,
                     ~ggplot(data = .x,
       aes(x = value,
           y = month_abb,
           fill = stat(x)
           )
       ) +
  geom_density_ridges_gradient(scale = 2,
                               rel_min_height = .01
                               ) +
  scale_fill_viridis_c(option = "C", alpha = .8) +
  theme(legend.position = "none") +
  labs(x = .y,
       y = "Month"
       )
                     ))

  
finalplots$plot

finalplots$plot

write_csv(fe_month_sum_wide,
          "../data/weather_monthsum.csv")
