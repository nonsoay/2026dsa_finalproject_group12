# install.packages("tidyverse")
# install.packages("janitor")
# install.packages("lubridate")
# install.packages("here")
# install.packages("vip")
# install.packages("xgboost")
# install.packages("doParallel")
# install.packages("shapviz")
# unlink("/home/cja04613/R/x86_64-pc-linux-gnu-library/4.5/00LOCK-survival", recursive = TRUE)
# install.packages("recipes")
# install.packages(c("purrr", "rsample"))
# install.packages("resample")
# # install.packages("finetune")
# install.packages("purrr")




#| message: false
#| warning: false


.libPaths(c("/work/crss8030/instructor_data/shared_R_libs", .libPaths()))
library(tidyverse)
library(janitor)
library(lubridate)
library(here)
library(vip)          
library(xgboost)      
library(ranger)       
library(doParallel)   
library(shapviz)
library(recipes)
library(resample)
library(finetune)     
library(parsnip)
library(daymetr)
#library(caret)   

#knitr::purl("finalproject_code.qmd", output = "finalproject_script.R", documentation = 0)


getwd()
setwd("/home/cja04613/2026dsa_finalproject_group12")

train_meta <- read_csv("../data/training/training_meta.csv") %>% 
  janitor::clean_names() 
train_trait <- read_csv("../data/training/training_trait.csv") %>% 
  janitor::clean_names() 
train_soil <- read_csv("../data/training/training_soil.csv") %>% 
  janitor::clean_names()


train_weather <- read_csv("../data/train_weather.csv")

train_full <- train_trait %>%
  left_join(train_weather, by = c("site", "year")) %>%
  left_join(train_soil, by = c("site", "year")) #%>% 
  mutate(across(soilp_h:soilp_ppm, as.numeric))

  summary(train_full)

train_full_month <- train_full %>%
  # Selecting needed variables
  dplyr::select(year, site, lat, lon,
                yield_mg_ha,
                grain_moisture,
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

train_full_month_sum <- train_full_month %>%
  group_by(year, site, month_abb, yield_mg_ha, grain_moisture,) %>%
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

train_full_month_sum_wide <- train_full_month_sum %>%
  pivot_longer(mean_dayl.s:sum_prcp.mm)%>%
  mutate(varname = paste0(name, "_",month_abb)) %>%
  dplyr::select(-name, -month_abb) %>% #another way of excluding some column
  pivot_wider(names_from = varname,
              values_from = value
              ) %>%
  # Rounding to one decimal point
  mutate(across(c(3:82), ~round(., 1)))

test_meta <- read_csv("../data/testing/testing_meta.csv") %>% 
  janitor::clean_names() 
test_soil <- read_csv("../data/testing/testing_soil.csv") %>% 
  janitor::clean_names() 
test_sub  <- read_csv("../data/testing/testing_submission.csv") %>% 
  janitor::clean_names() 

head(test_sub)
head(test_meta)
head(test_soil)

table(test_meta$previous_crop)
summary(test_soil)
summary(test_sub)

 test_meta <- test_meta %>% filter (latitude >= 14.0749 & latitude <= 82.9143 &
                  longitude >= -178.133 & longitude <= -53.0567)# information on daymet website(https://www.earthdata.nasa.gov/data/catalog/ornl-cloud-daymet-daily-v4r1-2129-4.1) shows that US is within this coordinates



test_meta_weather <- test_meta %>%
  mutate(weather = pmap(list(.y = year,
                             .site = site,
                             .lat = latitude,
                             .lon = longitude),
                        function(.y, .site, .lat, .lon)
                          download_daymet(
                            site = .site,
                            lat = .lat,
                            lon = .lon,
                            start = .y,
                            end = .y,
                            simplify = T,
                            silent = T
                          ) %>%
                          rename(.year = year,
                                 .site = site
                                 )


                        ))

test_weather <- test_meta_weather %>%
  rename(lat = latitude,
         lon = longitude) %>%
  unnest(weather) %>%
  pivot_wider(names_from = measurement,
              values_from = value
              ) %>%
  janitor::clean_names() %>%
  unnest(dayl_s: vp_pa) %>%
  dplyr::select(site, year, lon, lat,yday, dayl_s:vp_pa)

write_csv(test_weather,
          "../data/test_weather.csv"
          )

test_weather <- read_csv("../data/test_weather.csv")

test_weather_full <- test_weather %>%
  left_join(train_soil, by = c("site", "year")) #%>% 
  mutate(across(soilp_h:soilp_ppm, as.numeric))

  summary(train_full)

test_full_month <- test_full %>%
  # Selecting needed variables
  dplyr::select(year, site, lat, lon,
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

test_full_month_sum <- test_full_month %>%
  group_by(year, site, month_abb) %>%
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

test_full_month_sum_wide <- train_full_month_sum %>%
  pivot_longer(mean_dayl.s:sum_prcp.mm)%>%
  mutate(varname = paste0(name, "_",month_abb)) %>%
  dplyr::select(-name, -month_abb) %>% #another way of excluding some column
  pivot_wider(names_from = varname,
              values_from = value
              ) %>%
  # Rounding to one decimal point
  mutate(across(c(3:9), ~round(., 1)))

# Create recipe for data preprocessing
weather_recipe <- recipe(yield_mg_ha ~ ., data = train_full_month_sum_wide) %>% # Remove identifier columns and months not in growing season
  step_rm(
    year,       # Remove year identifier
    site,       # Remove site identifier
    matches("Jan|Feb|Mar|Apr|Nov|Dec")  # Remove non-growing season months
  ) %>% 

  # updating the role of the 2 columns as ID, so they arenot used as predictors
update_role(c(year, site), new_role = "ID")

weather_prep <- weather_recipe %>% 
  prep()

xgb_spec <- #Specifying XgBoost as our model type, asking to tune the hyperparameters
  boost_tree(
   # Total number of boosting iterations
  trees = tune(),
         # Maximum depth of each tree
  tree_depth = tune(),
             # Minimum samples required to split a node
  min_n = tune(),
        # Step size shrinkage for each boosting step
   learn_rate = tune()
  ) %>% 
        #specify engine 
  set_engine("xgboost") %>% 
       # Set to mode
  set_mode("regression")
xgb_spec


library(rsample)

set.seed(235) 
resampling_foldcv <- vfold_cv(train_full_month_sum_wide, # Create 5-fold cross-validation resampling object from training data
                              v = 10)


# Create leave one year out cv object from the sampling data
resampling_fold_loyo <- group_vfold_cv(train_full_month_sum_wide,
                                       group = year
                                       )

# Create leave one location out cv object from the sampling data
resampling_fold_loso <- group_vfold_cv(train_full_month_sum_wide,
                                       group = site
                                       )


resampling_foldcv
resampling_foldcv$splits[[1]]


library(dials)

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  learn_rate(),
  trees(),
  size = 10
)


# lets look into our current environment to see the available cores
n_cores <- as.numeric(Sys.getenv("SLURM_CPUS_ON_NODE"))

if (is.na(n_cores)) n_cores <- parallel::detectCores() - 1

# Start the Cluster
cl <- makePSOCKcluster(n_cores)

registerDoParallel(cl)

cat(paste0("\nFound and registered ", n_cores, " cores to work with\n"))




library(tidymodels) 
set.seed(76544)

# Create the list of CV techniques so we can loop through them
cv_list <- list(
  vfold = resampling_foldcv,
  year = resampling_fold_loyo,
  location = resampling_fold_loso
  
)

# Create a empty list to store the results from the loop
results <- list()
# Create our loop
for (i in seq_along(cv_list)) {
  name <- names(cv_list) [i]
  
  results[[name]] <- tune_race_anova(object = xgb_spec,
                                    preprocessor = weather_recipe,
                                    resamples = cv_list[[i]],
                                    grid = xgb_grid,
                                    control = control_race(save_pred = TRUE,
                                                           parallel_over = "everything")
                                    )
}


stopCluster(cl)


library(dplyr)

# Create a dataframe structure from the list obtained after running the loop
results_df <-  tibble(method = names(results),
                      diff_cv = results
                      ) %>% 
  # Collect the metrices for each CV techniques using map function
mutate(metrices = map2(diff_cv, method,
                       ~.x %>%
                         collect_metrics() %>% 
                         mutate (method = .y, .before = "trees")
                       ))
results_df

# bind all the metrices together so to select the best performing one
all_metrices <- do.call(bind_rows, results_df$metrices)

# Automating to pull the best method out of 3 we ran

best_method <- all_metrices %>%
  filter(.metric == "rmse") %>% 
  slice_min(mean, n = 1) %>% 
  pull(method)
  

# Getting the metrice (hyperparameter values of the best performing CV)
best_cv_object <- results_df %>%
  filter(method == best_method) %>%
  pull(diff_cv) %>% 
  first()
  

# Best RMSE
best_rmse <- best_cv_object %>% 
      select_best(metric = "rmse")%>% 
  mutate(source = "best_rmse")

best_rmse

# Based on greatest R2
best_r2 <- best_cv_object %>% 
  select_best(metric = "rsq")%>% 
  mutate(source = "best_r2")

best_r2

best_rmse %>% 
  bind_rows(best_rmse, 
            best_r2) %>%
  dplyr::select(source, everything())

final_spec <- boost_tree(
  trees = best_r2$trees,           # Number of boosting rounds (trees)
  tree_depth = best_r2$tree_depth, # Maximum depth of each tree
  min_n = best_r2$min_n,           # Minimum number of samples to split a node
  learn_rate = best_r2$learn_rate  # Learning rate (step size shrinkage)
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

final_spec

library(workflows)

final_workflow <- workflow() %>%
  add_model(final_spec) %>%
  add_recipe(weather_recipe)

final_fit <- final_workflow %>%
  fit(data = train_full_month_sum_wide)

set.seed(10)
final_fit <- last_fit(final_spec,
                weather_recipe,
                split = weather_split)

final_fit %>%
  collect_predictions()

