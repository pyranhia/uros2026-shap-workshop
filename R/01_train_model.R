# ============================================================
# 01_train_model.R
#
# Prepare the Adult dataset (income prediction) and train a gradient
# boosting model (LightGBM) with tidymodels.
#
# OPTIONAL: not needed during the tutorial, the trained model and the
# train/test sets are already in the repository. This script shows the full
# workflow from raw data to the fitted model, out of curiosity. To run it you
# need the `datapyranhia` package (Adult dataset), which is not part of the
# pinned environment:
#   install.packages("pak"); pak::pak("pyranhia/datapyranhia")
# Running it overwrites the three files below.
#
# Output: models/gbm_adult.rds (fitted tidymodels workflow),
#         data/processed/adult_train.rds, data/processed/adult_test.rds
# ============================================================

library(tidyverse)
library(tidymodels)
library(datapyranhia)
library(bonsai)
library(here)

# ------------------------------------------------------------
# 1. Load and split the data
# ------------------------------------------------------------

data(adult, package = "datapyranhia")

adult <- adult |>
  mutate(income = as.factor(income))

set.seed(123) # for reproducibility
adult_split <- initial_split(adult, prop = 0.8, strata = "income")

adult_train <- training(adult_split)
adult_test <- testing(adult_split)

set.seed(123)
cv_folds <- vfold_cv(adult_train, v = 3, repeats = 1, strata = "income")

# ------------------------------------------------------------
# 2. Model specification: gradient boosting (LightGBM engine)
# ------------------------------------------------------------

gbm_spec <- boost_tree(
  trees = tune(),
  learn_rate = tune(),
  tree_depth = tune()
) |>
  set_engine("lightgbm") |>
  set_mode("classification")

gbm_recipe <- recipe(income ~ ., data = adult_train)

gbm_wf <- workflow() |>
  add_recipe(gbm_recipe) |>
  add_model(gbm_spec)

# ------------------------------------------------------------
# 3. Hyperparameter tuning (grid search)
# ------------------------------------------------------------

tuning_grid <- crossing(
  trees = c(500, 1000, 1500),
  learn_rate = c(0.05, 0.1, 0.5),
  tree_depth = c(2, 5, 8)
)

set.seed(123)
tune_res <- tune_grid(
  object = gbm_wf,
  resamples = cv_folds,
  grid = tuning_grid,
  metrics = metric_set(accuracy),
  control = control_grid(verbose = TRUE)
)

autoplot(tune_res)

best_params <- select_best(tune_res, metric = "accuracy")

final_wf <- finalize_workflow(gbm_wf, best_params)

# ------------------------------------------------------------
# 4. Fit the final model on the full training set
# ------------------------------------------------------------

set.seed(123)
final_fit <- fit(final_wf, data = adult_train)

# ------------------------------------------------------------
# 5. Evaluate on the test set
# ------------------------------------------------------------

predictions <- predict(final_fit, adult_test) |>
  bind_cols(adult_test |> select(income)) |>
  mutate(
    income = factor(income, levels = c(">50K", "<=50K")),
    .pred_class = factor(.pred_class, levels = c(">50K", "<=50K"))
  )

accuracy(predictions, truth = income, estimate = .pred_class)

conf_mat(predictions, truth = income, estimate = .pred_class) |>
  autoplot(type = "heatmap") +
  coord_fixed()

precision(
  predictions,
  truth = income,
  estimate = .pred_class,
  event_level = "first"
)

recall(
  predictions,
  truth = income,
  estimate = .pred_class,
  event_level = "first"
)

# ------------------------------------------------------------
# 6. Save the fitted model for downstream scripts
#    (the exercises and the demo app read these files)
# ------------------------------------------------------------

saveRDS(final_fit, here("models", "gbm_adult.rds"))
saveRDS(adult_train, here("data", "processed", "adult_train.rds"))
saveRDS(adult_test, here("data", "processed", "adult_test.rds"))
