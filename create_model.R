#### XGBoost Fits ####

# Identify predictors and response
y <- "principal_paid_share_delta"
x <- setdiff(names(monthly_payments_for_bt), y)
nfolds <- 5

## Boosting tree

# GBM hyperparameters
data <- as.h2o(monthly_payments_for_bt %>%
                 select(all_of(c(y, x))))
ss <- h2o.splitFrame(data, seed = 1, ratios = 0.9)
train_principal_paid <- ss[[1]]
test_principal_paid <- ss[[2]]

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
gbm_gridperf <- h2o.getGrid(grid_id = "gbm_grid_principal_paid",
                            sort_by = "rmse")

print(gbm_gridperf)

# Grab the top GBM model, chosen by validation AUC
gbm <- h2o.getModel(gbm_gridperf@model_ids[[1]])

shap_values <-
  h2o.shap_summary_plot(model = gbm,
                        newdata =  data,
                        columns = x)

gbm_principal_paid <-
  h2o.gbm(x = x,
          y = y,
          training_frame = train_principal_paid,
          ntrees = 100,
          max_depth = 10,
          learn_rate = 0.05,
          col_sample_rate = 0.8, 
          sample_rate = 0.6,
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
                            newdata = data)
gbm_perf

# Look at the hyperparameters for the best model
print(gbm_principal_paid@model[["model_summary"]])

shap_values <-
  h2o.shap_summary_plot(model = gbm_principal_paid,
                        newdata =  data,
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


predict <- cbind(monthly_payments_for_bt,
                 estimated_principal_paid_delta =
                   as.vector(h2o.predict(object = sem,
                                         newdata = data)))

plotly::ggplotly(predict %>% 
                   mutate(delta = 
                            principal_paid_share_delta - estimated_principal_paid_delta) %>%
                   ggplot(aes(x = delta)) +
                   geom_density())

plotly::ggplotly(predict %>% 
                   mutate(delta = 
                            principal_paid_share_delta - estimated_principal_paid_delta) %>%
                   ggplot(aes(x = delta)) +
                   geom_histogram())
