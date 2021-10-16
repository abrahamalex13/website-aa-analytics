#presentation_objects.R

#this script supplies R data objects for both
#presentation and blog outputs.

library(xaringanthemer)
style_mono_light(base_color = "#23395b")

library(tidyverse)
library(kableExtra)


path_home <- "../../simulations/clearly_linear/"
path_results <- paste(path_home, "results/", sep = "")

#pithy lines
model_essence <- "A rule by which _x_ predicts _y_, minimizing error"
title_case <- paste("Data come from a controlled experiment (simulation). \n\n", 
                    "Setup: true relationships are linear, with predictors correctly anticipated by industry expertise.",
                    sep = "")
footnote_src <- "Source: AA Analytics."





#line of best fit intro -------------

set.seed(2347)

n <- 50
x <- rnorm(n) * 5 + 70
beta <- 2
epsilon <- rnorm(n)*5

y <- x * beta + epsilon

df_plot <- data.frame("Units" = y, "Temperature" = x, 
                      "VT_Temperature" = x*beta)

p_ex1 <- 
  ggplot(df_plot) + 
    geom_point(aes(Temperature, Units)) + 
    geom_smooth(aes(Temperature, Units), method = lm, 
                linetype = 2, se = FALSE) + 
    
    theme(plot.subtitle = element_text(size = 20)) + 
    theme_xaringan() + 
    labs(title = "Manufacturing Production Increases with Temperature", 
         subtitle = "A linear model - a 'line of best fit' (dashed) - highlights a general pattern.",
         y = "Units Produced", x = "Temperature (degrees F)")

# ----------



# true & estimated best fit lines ---

p_ex1_true <- 
  ggplot(df_plot) + 
    
    geom_point(aes(Temperature, Units)) + 
    geom_smooth(aes(Temperature, Units, color = "Estimated Linear Relationship"), method = lm, 
                linetype = 2, se = FALSE) +
    
    geom_smooth(aes(Temperature, VT_Temperature, color = "True Linear Relationship"), method = lm, se = FALSE) + 
    
    theme(legend.position = "top",
          plot.subtitle = element_text(size = 20, lineheight = .5)) + 
    guides(color = guide_legend(ncol = 1)) + 
    theme_xaringan() + 
  
    labs(title = "Manufacturing Production Increases with Temperature", 
         subtitle = paste("A linear model estimates the true relationship between predictor(s) and an outcome. \n\n",
                          "The true relationship is unclear because of random noise in the observed outcome.", sep = ""),
         y = "Units Produced", x = "Temperature (degrees F)",
         color = NULL)

#--------------



#comparing linear model varieties ---
tbl_lm_varieties <- read_csv("linear_model_varieties.csv")
tbl_lm_num_props <- 
  tibble("Numerical Property" = c("Calculation formula", "Estimation behavior"),
         "Classical Statistics Variety" = c("- Well-defined \n (one formula to compute any best-fit line)",
                                            "- Model stability improves with more data \n \t - Under key assumptions, quantify closeness of _estimated_ and _true_ lines"),
         "Machine Learning Variety" = c("- Flexible \n (no one formula minimizes prediction errors _plus_ model flexibility)",
                                            " - Model stability improves with more data"),
         
         )
#--------



#parallel a real project and one trial in an experiment ----
tbl_real_proj_exp_trial <- read_csv("simulation_real_project_parallels.csv")
tbl_datasets_train_test <- read_csv("datasets_train_test.csv")
# ------




#organize trial 1 training data for illustration ------

#execute one piecewise iteration of experiment.
#to choose erroneous var, use varImp with elastic net model.
# df_plot <- data.frame(x, y)
# 
# df_plot_t <- df_plot %>%
#   gather(key = series, value = value, -y)
# 
# lbls <- data.frame(series = c("x1", "x2", "x3", "x4", "x39"),
#                    label = c("Indoor temperature", "Laborers' average wage",
#                              "Laborers' average hours of sleep", "Foreman's average wage",
#                              "S&P500 return from day previous"),
#                    stringsAsFactors = FALSE)
# df_plot_t <- left_join(df_plot_t, lbls)
# 
# p <- ggplot(df_plot_t %>% dplyr::filter(!is.na(label))) +
#   geom_point(aes(value, y)) +
#   facet_wrap(~label, scales = "free_x") +
#   labs(y = "Units of Output (hourly)", x = "Value",
#        title = "Case: Four Series Truly Predict Manufacturing Output")
# 
# saveRDS(p, file = paste(path_results, "dummy_data_plot.rds", sep = ""))



p_dummy_data <- readRDS(paste(path_results, "dummy_data_plot.rds", sep = "")) +
  
  theme(plot.subtitle = element_text(size = 20, lineheight = .65)) + 
  
  labs(y = "Units of Output (hourly)", 
       x = "Value",
       title = paste("Training Data Plots Show Clear Linear Relationships for True Predictors"), 
       subtitle = title_case,
       caption = footnote_src) + 
  theme_xaringan()

#--------------





# assess model predictions on training data --------

note_ml_fit <- paste("ML model: Elastic Net, ", 
                     "estimated via cross-validation on training data and default tuning parameter grid from R's caret package.", 
                     sep = "")


#summary table all trials
tbl_results_1k_obs <- readRDS(paste(path_results, "1k obs.rds", sep = "")) %>% 
  mutate(train_result = ifelse(mse_train_mdl_lm < mse_train_mdl_ml, 
                               "Classical Statistics Has Lower Training Data Error",
                               "Machine Learning Has Lower Training Data Error"),
         
         test_result = ifelse(mse_test_mdl_lm < mse_test_mdl_ml, 
                              "Classical Statistics Has Lower Test Data Error",
                              "Machine Learning Has Lower Test Data Error"))

tab_results_training_pred <- tbl_results_1k_obs %>% 
  group_by(train_result) %>% 
  summarize(n = n()) %>% 
  spread(key = train_result, value = n)
if (! ("Classical Statistics Has Lower Training Data Error") %in% colnames(tab_results_training_pred) ) {
  
  tab_results_training_pred <- tab_results_training_pred %>% 
    mutate(`Classical Statistics Has Lower Training Data Error` = 0)
  
}


#first few trials
p_results_training_pred <- 
  ggplot(tbl_results_1k_obs %>% dplyr::filter(iter <= 10)) + 
    geom_line(aes(iter, mse_train_mdl_lm, color = "Classical Statistics")) + 
    geom_point(aes(iter, mse_train_mdl_lm, color = "Classical Statistics")) + 
    
    geom_line(aes(iter, mse_train_mdl_ml, color = "Machine Learning")) +
    geom_point(aes(iter, mse_train_mdl_ml, color = "Machine Learning")) +
    
    theme(legend.position = "top", plot.subtitle = element_text(size = 20, lineheight = .55)) + 
    guides(color = guide_legend(ncol = 1)) + 
  
    labs(title = "In This Case, Training Data Best Predicted by Machine Learning",
         subtitle = paste(title_case, "\n\n", note_ml_fit, sep = ""),
         y = "Mean Squared Error (MSE)", 
         x = "Trial", color = "Legend",
         caption = footnote_src) + 
    
    theme_xaringan()

# ----------






# assess model predictions on test data ---------

tab_results_test_pred <- tbl_results_1k_obs %>% 
  group_by(test_result) %>% 
  summarize(n = n()) %>% 
  spread(key = test_result, value = n)
if (! ("Machine Learning Has Lower Test Data Error") %in% colnames(tab_results_test_pred) ) {
  
  tab_results_test_pred <- tab_results_test_pred %>% 
    mutate(`Machine Learning Has Lower Test Data Error` = 0)
  
}


#first few trials
p_results_test_pred <- 
  ggplot(tbl_results_1k_obs %>% dplyr::filter(iter <= 10)) + 
    geom_line(aes(iter, mse_test_mdl_lm, color = "Classical Statistics")) + 
    geom_point(aes(iter, mse_test_mdl_lm, color = "Classical Statistics")) + 
    
    geom_line(aes(iter, mse_test_mdl_ml, color = "Machine Learning")) +
    geom_point(aes(iter, mse_test_mdl_ml, color = "Machine Learning")) +
    
    theme(legend.position = "top", plot.subtitle = element_text(size = 20, lineheight = .55)) + 
    guides(color = guide_legend(ncol = 1)) + 

    labs(title = "In This Case, Test Data Best Predicted by Classical Statistics",
         subtitle = paste(title_case, "\n\n", note_ml_fit, sep = ""),
         y = "Mean Squared Error (MSE)", 
         x = "Trial", color = "Legend",
         caption = footnote_src) + 
    
    theme_xaringan()

# --------------







#extension: n = 10k ----------

# tbl_results_10k_obs <- readRDS(paste(path_results, "10k obs.rds", sep = "")) %>% 
#   mutate(train_result = ifelse(sse_train_mdl_lm < sse_train_mdl_ml, 
#                                "Classical Statistics Has Lower Training Data Error",
#                                "Machine Learning Has Lower Training Data Error"),
#          
#          test_result = ifelse(sse_test_mdl_lm < sse_test_mdl_ml, 
#                               "Classical Statistics Has Lower Test Data Error",
#                               "Machine Learning Has Lower Test Data Error"))
# 
# tab_results_training_pred <- tbl_results_10k_obs %>% 
#   group_by(train_result) %>% 
#   summarize(n = n()) %>% 
#   spread(key = train_result, value = n)
# 
# if (! ("Classical Statistics Has Lower Training Data Error") %in% colnames(tab_results_training_pred) ) {
#   
#   tab_results_training_pred <- tab_results_training_pred %>% 
#     mutate(`Classical Statistics Has Lower Training Data Error` = 0)
#   
# }
# 
# 
# 
# tab_results_test_pred <- tbl_results_10k_obs %>% 
#   group_by(test_result) %>% 
#   summarize(n = n()) %>% 
#   spread(key = test_result, value = n)
# 
# if (! ("Machine Learning Has Lower Test Data Error") %in% colnames(tab_results_test_pred) ) {
#   
#   tab_results_test_pred <- tab_results_test_pred %>% 
#     mutate(`Machine Learning Has Lower Test Data Error` = 0)
#   
# }

# --------------