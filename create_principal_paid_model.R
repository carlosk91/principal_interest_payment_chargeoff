#### XGBoost Fits ####

# Identify predictors and response
y <- "principal_paid_share_delta"
x <- setdiff(names(monthly_payments_for_bt), y)
nfolds <- 5

## Boosting tree

# GBM hyperparameters
pp_data <- as.h2o(monthly_payments_for_bt %>%
                 select(all_of(c(y, x))))
pp_ss <- h2o.splitFrame(pp_data, seed = 1, ratios = 0.9)
train_principal_paid <- pp_ss[[1]]
test_principal_paid <- pp_ss[[2]]

# Train and validate a cartesian grid of GBMs

gbm_params <- list(
  learn_rate = seq(0.01, 0.1, 0.01),
  max_depth = seq(2, 10, 1),
  sample_rate = seq(0.5, 1.0, 0.1),
  col_sample_rate = seq(0.1, 1.0, 0.1)
)
search_criteria <-
  list(strategy = "RandomDiscrete",
       max_models = 36,
       seed = 1)

# Train and validate a cartesian grid of GBMs
gbm_grid_principal_paid <- h2o.grid(
  "gbm",
  x = x,
  y = y,
  grid_id = "gbm_grid_principal_paid",
  training_frame = train_principal_paid,
  validation_frame = test_principal_paid,
  ntrees = 100,
  seed = 1,
  hyper_params = gbm_params,
  search_criteria = search_criteria
)

# Get the grid results, sorted by validation MAE
pp_gbm_gridperf <- h2o.getGrid(grid_id = "gbm_grid_principal_paid",
                            sort_by = "mae")

print(pp_gbm_gridperf)

# Grab the top GBM model, chosen by validation AUC
pp_gbm <- h2o.getModel(pp_gbm_gridperf@model_ids[[1]])

shap_values <-
  h2o.shap_summary_plot(model = pp_gbm,
                        newdata =  pp_data,
                        columns = x)


pp_best_ntrees <- pp_gbm@model[["model_summary"]]$number_of_trees
pp_best_max_depth <- pp_gbm@model[["model_summary"]]$max_depth
pp_best_col_sample_rate <-
  as.numeric(pp_gbm_gridperf@summary_table$col_sample_rate[1])
pp_best_learn_rate <- pp_gbm_gridperf@summary_table$col_learn_rate[1]
pp_best_sample_rate <- as.numeric(pp_gbm_gridperf@summary_table$sample_rate[1])

h2o.rm(pp_gbm)

gbm_principal_paid <-
  h2o.gbm(x = x,
          y = y,
          training_frame = train_principal_paid,
          ntrees = pp_best_ntrees,
          max_depth = pp_best_max_depth,
          learn_rate = pp_best_learn_rate,
          col_sample_rate = pp_best_col_sample_rate, 
          sample_rate = pp_best_sample_rate,
          nfolds = nfolds,
          keep_cross_validation_predictions = TRUE,
          seed = 1)

# Save model when re estimate
model_path <- h2o.saveModel(object = gbm_principal_paid,
                            path = getwd(),
                            force = T)

mojo_destination <- h2o.save_mojo(object = gbm_principal_paid,
                                  path = getwd(),
                                  force = T)

varimp <- h2o.varimp(gbm_principal_paid)

gbm_perf <- h2o.performance(model = gbm_principal_paid,
                            newdata = pp_data)
gbm_perf

# Look at the hyperparameters for the best model
print(gbm_principal_paid@model[["model_summary"]])

shap_values <-
  h2o.shap_summary_plot(model = gbm_principal_paid,
                        newdata =  pp_data,
                        columns = varimp$variable)

#### Deep learning model ####

dlm <- 
  h2o.deeplearning(
    x = x,
    y = y,
    seed = 1,
    training_frame = train_principal_paid, 
    nfolds = nfolds,
    keep_cross_validation_predictions = T)

glm <- 
  h2o.glm(
    x = x,
    y = y,
    seed = 1,
    training_frame = train_principal_paid, 
    nfolds = nfolds,
    keep_cross_validation_predictions = T)

sem <- 
  h2o.stackedEnsemble(x = x, 
                      y = y,
                      training_frame = train_principal_paid,
                      base_models = list(dlm, gbm_principal_paid))


pp_predict <- cbind(monthly_payments_for_bt,
                 estimated_principal_paid_delta =
                   as.vector(h2o.predict(object = sem,
                                         newdata = pp_data)))

pp_predict %>%
  pivot_longer(c(principal_paid_share_delta, estimated_principal_paid_delta), 
               names_to = "real_estimated", values_to = "delta_share") %>%
  ggplot(aes(x = delta_share, color = real_estimated)) +
  geom_density()


plotly::ggplotly(pp_predict %>% 
                   mutate(delta = 
                            principal_paid_share_delta - estimated_principal_paid_delta) %>%
                   ggplot(aes(x = delta)) +
                   geom_histogram())
