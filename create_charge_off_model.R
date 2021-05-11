#### XGBoost Fits ####

# Identify predictors and response
y <- "chargeoff_share"
x <- setdiff(names(monthly_chargeoff_for_bt), y)
nfolds <- 5

## Boosting tree

# GBM hyperparameters
co_data <- as.h2o(monthly_chargeoff_for_bt %>%
                 select(all_of(c(y, x))))
co_ss <- h2o.splitFrame(co_data, seed = 1, ratios = 0.9)
co_train <- co_ss[[1]]
co_test <- co_ss[[2]]

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
co_gbm_grid <- h2o.grid(
  "gbm",
  x = x,
  y = y,
  grid_id = "co_gbm_grid",
  training_frame = co_train,
  validation_frame = co_test,
  ntrees = 100,
  seed = 1,
  hyper_params = gbm_params,
  search_criteria = search_criteria
)

# Get the grid results, sorted by validation MAE
co_gbm_gridperf <- h2o.getGrid(grid_id = "co_gbm_grid",
                            sort_by = "rmse")

print(co_gbm_gridperf)

# Grab the top GBM model, chosen by validation AUC
co_gbm <- h2o.getModel(co_gbm_gridperf@model_ids[[1]])

co_varimp <- h2o.varimp(co_gbm)

co_shap_values <-
  h2o.shap_summary_plot(model = co_gbm,
                        newdata = co_data,
                        columns = x)

co_best_ntrees <- co_gbm@model[["model_summary"]]$number_of_trees
co_best_max_depth <- co_gbm@model[["model_summary"]]$max_depth
co_best_col_sample_rate <-
  as.numeric(co_gbm_gridperf@summary_table$col_sample_rate[1])
co_best_learn_rate <- co_gbm_gridperf@summary_table$col_learn_rate[1]
co_best_sample_rate <- as.numeric(co_gbm_gridperf@summary_table$sample_rate[1])

h2o.rm(co_gbm)

co_gbm <-
  h2o.gbm(
    x = x,
    y = y,
    training_frame = co_train,
    ntrees = co_best_ntrees,
    max_depth = co_best_max_depth,
    learn_rate = co_best_learn_rate,
    col_sample_rate = co_best_col_sample_rate,
    sample_rate = co_best_sample_rate,
    nfolds = nfolds,
    keep_cross_validation_predictions = TRUE,
    seed = 1
  )

# Save model when re estimate
model_path <- h2o.saveModel(object = co_gbm,
                            path = getwd(),
                            force = T)

mojo_destination <- h2o.save_mojo(object = co_gbm,
                                  path = getwd(),
                                  force = T)

co_varimp <- h2o.varimp(co_gbm)

co_gbm_perf <- h2o.performance(model = co_gbm,
                            newdata = co_data)

co_gbm_perf

# Look at the hyperparameters for the best model
print(co_gbm@model[["model_summary"]])

shap_values <-
  h2o.shap_summary_plot(model = co_gbm,
                        newdata =  co_data,
                        columns = co_varimp$variable)

#### Deep learning model ####

co_dlm <-
  h2o.deeplearning(
    x = x,
    y = y,
    seed = 1,
    training_frame = co_train,
    nfolds = nfolds,
    keep_cross_validation_predictions = T
  )

sem_co <-
  h2o.stackedEnsemble(
    x = x,
    y = y,
    training_frame = co_train,
    base_models = list(co_dlm, co_gbm)
  )

h2o.performance(sem_co, train = T)
h2o.performance(sem_co, co_test)

co_predict <-
  cbind(monthly_chargeoff_for_bt,
        estimated_charge_off_share =
          as.vector(h2o.predict(object = co_gbm,
                                newdata = co_data))) %>%
  as_tibble()

co_predict %>%
  pivot_longer(c(chargeoff_share, estimated_charge_off_share), 
               names_to = "real_estimated", values_to = "charge_off_share") %>%
  ggplot(aes(x = charge_off_share, color = real_estimated)) +
  geom_density()

co_predict %>%
  transmute(delta =
              chargeoff_share - estimated_charge_off_share) %>%
  ggplot(aes(x = delta)) +
  stat_ecdf(geom = "step", pad = FALSE)

co_predict %>%
  transmute(delta =
              chargeoff_share - estimated_charge_off_share) %$%
  quantile(delta, probs = seq(0.1,1,0.1))

