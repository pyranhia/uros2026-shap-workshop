# SHAP exercises: write your own code
# Same parts, same order and same exercises as the slides and the course notes
# (the hints are there). Fill in the slots marked <Your code here>; the pieces
# that are given are announced in the slides.
#   Part 0. Explainer and two contrasted people
#   Part 1. Break-down
#   Part 2. SHAP values by hand
#   Part 3. SHAP values with DALEX
#   Part 4. From local to global (precomputed SHAP values)
#   Part 5. TreeSHAP with shapviz
#
# The model is already trained (see 01_train_model.R): nothing to refit.
# Parts 0 to 4 are on the predicted-probability scale, Part 5 (shapviz) is on
# the log-odds scale: same idea, two computational routes.

library(tidymodels)
library(bonsai)
library(DALEXtra)
library(shapviz)
library(forcats)
library(here)

gbm_fit <- readRDS(here("models", "gbm_adult.rds"))
adult_test <- readRDS(here("data", "processed", "adult_test.rds"))


## Part 0. Warm-up: explainer and two contrasted people ----
#--------------------------------------------------------------------------#

# a) Build the DALEX explainer with explain_tidymodels(): the model and the
#    test set without the target, a random sample of 500 rows as reference
#    data to keep the computation fast (your numbers will differ slightly
#    from your neighbour's: the sample is random)

# Explainer: the model, and 500 random rows of adult_test without income
# <Your code here>

# b) Predict the probability of income >50K for the first 30 rows of the test
#    set, and keep the row number of one person with a LOW probability and
#    one with a HIGH probability

# Predicted probabilities of the first 30 rows, with their row number
# <Your code here>

# Row numbers of your two people
low_id <- NA # <- the row number of your LOW person
high_id <- NA # <- the row number of your HIGH person

# Given: your two people, used in all the following parts
my_low <- adult_test |> slice(low_id)
my_high <- adult_test |> slice(high_id)

# Their predicted probabilities
# <Your code here>

# c) Average prediction on the reference data: where do your two people
#    stand?
# <Your code here>


## Part 1. Break-down ----
#--------------------------------------------------------------------------#

# a) Compute and plot the break-down of my_low and my_high

# Break-down of my_low
# <Your code here>

# Plot it
# <Your code here>

# Same for my_high
# <Your code here>

# b) The same break-down of my_low, with the variables added in a random
#    order. Run it twice: do the contributions change?
vars <- setdiff(names(adult_test), "income") # given: the 12 predictors

# Break-down of my_low, with the variables in a random order
# <Your code here>

# Plot it
# <Your code here>


## Part 2. SHAP values by hand ----
#--------------------------------------------------------------------------#

# a) Complete the skeleton: 10 break-downs of my_low, each with a random
#    order, stacked in one data frame
bd_10_orders <- map(1:10, \(i) {
  predict_parts(
    explainer = explainer,
    new_observation = NULL, # <Your code here>
    type = "break_down",
    order = NULL # <Your code here>
  ) |>
    as_tibble()
}) |>
  list_rbind()

# b) Average the contribution of each variable over the 10 orders: your SHAP
#    values, computed by hand. Name the result shap_by_hand (Part 3 uses it)

# Keep the variables only: drop the rows whose variable_name is "intercept"
# or "" (the baseline and the prediction), then average the contribution of
# each variable
# <Your code here>

# Print the result
# <Your code here>


## Part 3. SHAP values with DALEX ----
#--------------------------------------------------------------------------#

# a) Compute the SHAP values of my_low and my_high (about 15 seconds each)
#    and plot their waterfalls, with max_features = 10

# SHAP values of my_low
# <Your code here>

# Waterfall of the 10 most important variables
# <Your code here>

# Same for my_high
# <Your code here>

# b) Compare with your values by hand: close? Why not identical?

# DALEX values: keep the rows with B == 0 (the averages over the orderings),
# and the variable name and contribution, renamed shap_dalex
# <Your code here>

# Join them with shap_by_hand and plot one against the other, with the
# diagonal and the variable names
# <Your code here>

# c) Bonus, additivity: average prediction + sum of the SHAP values =
#    prediction of my_low?

# Average prediction + sum of the SHAP values
# <Your code here>

# Prediction of my_low
# <Your code here>


## Part 4. From local to global ----
#--------------------------------------------------------------------------#

# Given: the SHAP values precomputed for 1000 observations (DALEX would take
# more than 24 hours): one row per variable and per observation (obs_id,
# variable_name, variable_value, contribution)
shap_summary <- readRDS(here("data", "processed", "shap_values.rds"))$shap_summary |>
  as_tibble()

# a) Global importance: the mean absolute SHAP value of each variable, as a
#    sorted bar chart

# Mean absolute SHAP value of each variable, sorted
# <Your code here>

# Bar chart
# <Your code here>

# b) The effect of age: the SHAP value of age against the value of age, one
#    point per observation

# SHAP values of age, with the value of age as a number
# <Your code here>

# One point per observation: SHAP value against age
# <Your code here>

# c) Bonus, a beeswarm: the four continuous variables on one plot. Complete
#    the skeleton, in three steps
shap_summary |>
  # 1. keep the four continuous variables: age, capital_gain, capital_loss,
  #    hours_per_week
  # <Your code here>
  # 2. rescale the values of each variable between 0 and 1 (value_scaled)
  # <Your code here>
  ggplot() +
  # 3. one jittered point per observation: x = contribution,
  #    colour = value_scaled
  geom_jitter(
    aes(y = variable_name), # <Your code here>
    height = 0.1,
    alpha = 0.5
  ) +
  scale_colour_gradient2(
    low = "#4575b4",
    mid = "#ffffbf",
    high = "#d73027",
    midpoint = 0.5
  )


## Part 5. TreeSHAP with shapviz ----
#--------------------------------------------------------------------------#

# a) Given: just run it. TreeSHAP values of every row of the test set, in one
#    call. How long does it take, compared with one DALEX waterfall?
X <- extract_recipe(gbm_fit) |>
  bake(new_data = adult_test, all_predictors())
X_pred <- bonsai:::prepare_df_lgbm(X)
lgb_booster <- extract_fit_engine(gbm_fit)
sv <- shapviz(lgb_booster, X_pred = X_pred, X = X)

# b) Beeswarm of the whole test set, with sv_importance(): same story as the
#    DALEX beeswarm?
# <Your code here>

# c) Dependence plot of sex, with sv_dependence(), then coloured by
#    relationship: what do you see?
#    Then draw the dependence plot of a variable of your choice

# Dependence plot of sex, without colour
# <Your code here>

# The same, coloured by relationship
# <Your code here>

# Dependence plot of a variable of your choice
# <Your code here>

# d) Bonus: the TreeSHAP waterfall of my_low
# <Your code here>
