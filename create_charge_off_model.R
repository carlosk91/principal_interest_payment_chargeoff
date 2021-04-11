#### XGBoost Fits ####

# Identify predictors and response
y <- "chargeoff_share"
x <- setdiff(names(monthly_chargeoff_for_bt), y)
nfolds <- 5

## Boosting tree

# GBM hyperparameters
data <- as.h2o(monthly_chargeoff_for_bt %>%
                 filter(completion_percentage >= 3/original_term_to_maturity) %>%
                 select(all_of(c(y, x))))
ss <- h2o.splitFrame(data, seed = 1, ratios = 0.9)
train <- ss[[1]]
test <- ss[[2]]

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
gbm_grid <- h2o.grid(
  "gbm",
  x = x,
  y = y,
  grid_id = "gbm_grid",
  training_frame = train,
  validation_frame = test,
  ntrees = 100,
  seed = 1,
  hyper_params = gbm_params,
  search_criteria = search_criteria
)

# Get the grid results, sorted by validation MAE
gbm_gridperf <- h2o.getGrid(grid_id = "gbm_grid",
                            sort_by = "rmse")

print(gbm_gridperf)

# Grab the top GBM model, chosen by validation AUC
gbm <- h2o.getModel(gbm_gridperf@model_ids[[1]])

varimp <- h2o.varimp(gbm)

shap_values <-
  h2o.shap_summary_plot(model = gbm,
                        newdata =  data,
                        columns = x)

best_ntrees <- gbm@model[["model_summary"]]$number_of_trees
best_max_depth <- gbm@model[["model_summary"]]$max_depth
best_col_sample_rate <-
  as.numeric(gbm_gridperf@summary_table$col_sample_rate[1])
best_learn_rate <- gbm_gridperf@summary_table$col_learn_rate[1]
best_sample_rate <- as.numeric(gbm_gridperf@summary_table$sample_rate[1])

h2o.rm(gbm)

gbm <-
  h2o.gbm(
    x = x,
    y = y,
    training_frame = train,
    ntrees = best_ntrees,
    max_depth = best_max_depth,
    learn_rate = best_learn_rate,
    col_sample_rate = best_col_sample_rate,
    sample_rate = best_sample_rate,
    nfolds = nfolds,
    keep_cross_validation_predictions = TRUE,
    seed = 1
  )

# Save model when re estimate
model_path <- h2o.saveModel(object = gbm,
                            path = getwd(),
                            force = T)

mojo_destination <- h2o.save_mojo(object = gbm,
                                  path = getwd(),
                                  force = T)

varimp <- h2o.varimp(gbm)

gbm_perf <- h2o.performance(model = gbm,
                            newdata = data)
gbm_perf

# Look at the hyperparameters for the best model
print(gbm@model[["model_summary"]])

shap_values <-
  h2o.shap_summary_plot(model = gbm,
                        newdata =  data,
                        columns = varimp$variable)

#### Deep learning model ####

dlm <-
  h2o.deeplearning(
    x = x,
    y = y,
    seed = 1,
    training_frame = train,
    nfolds = nfolds,
    keep_cross_validation_predictions = T
  )

sem_co <-
  h2o.stackedEnsemble(
    x = x,
    y = y,
    training_frame = train,
    base_models = list(dlm, gbm)
  )


predict <-
  cbind(monthly_chargeoff_for_bt %>%
          filter(completion_percentage >= 3/original_term_to_maturity),
        estimated_charge_off_share =
          as.vector(h2o.predict(object = sem_co,
                                newdata = data))) %>%
  as_tibble() %>%
  group_by(vintage_year,
           vintage_month,
           original_term_to_maturity,
           credit_segment,
           vertical) %>%
  mutate(estimated_charge_off_share =
           case_when(
             completion_percentage < 3/original_term_to_maturity ~ 0,
             T ~ estimated_charge_off_share / sum(estimated_charge_off_share))) %>%
  ungroup()



predict %>%
  transmute(delta =
              chargeoff_share - estimated_charge_off_share) %>%
  ggplot(aes(x = delta)) +
  stat_ecdf(geom = "step", pad = FALSE)

predict %>%
  transmute(delta =
              chargeoff_share - estimated_charge_off_share) %$%
  quantile(delta, probs = seq(0.1,1,0.1))
