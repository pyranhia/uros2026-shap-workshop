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

# Help: ?explain_tidymodels in the console, or F1 on a function name in RStudio

# a) Wrap the model and the test set in one DALEX object with
#    explain_tidymodels(). Use a random sample of 500 rows as reference data,
#    without the target income: it keeps the computation fast. Your numbers
#    will differ slightly from your neighbour's.
#    ref_data is the background DALEX compares with, not the people you will
#    explain.

# Explainer: the model, and 500 random rows of adult_test without income
# <Your code here>

# b) Predict the probability of income >50K for the first 30 rows of
#    adult_test, using predict() on the explainer you just created.
#    Keep the row number of one person with a low probability and one with a
#    high probability: low_id and high_id in the script.
#    The two people come from adult_test, not from ref_data: they don't need
#    to be in the reference data.

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

# c) Compute the average prediction on the reference data. Where do your two
#    people stand?
# <Your code here>

## Part 1. Break-down ----
#--------------------------------------------------------------------------#

# Help: ?predict_parts in the console, or F1 on a function name in RStudio
# Stuck, or done early? Explore other people in the app, tab Break-down:
# shiny-uros2026.pyranhia.eu (during the session only)

# a) Compute and plot the break-down of my_low and my_high. Which variables
#    push each prediction up, which push it down? As you expected?

# Break-down of my_low
# <Your code here>

# Plot it
# <Your code here>

# Same for my_high
# <Your code here>

# b) Compute the same break-down of my_low, with the variables added in a
#    random order. Run it twice: do the contributions change?
vars <- setdiff(names(adult_test), "income") # given: the 12 predictors

# Break-down of my_low, with the variables in a random order
# <Your code here>

# Plot it
# <Your code here>

## Part 2. SHAP values by hand ----
#--------------------------------------------------------------------------#

# Help: ?predict_parts in the console, or F1 on a function name in RStudio
# Stuck, or done early? Explore other people in the app:
# shiny-uros2026.pyranhia.eu (during the session only)

# a) Complete the skeleton in the script: 10 break-downs of my_low, each with
#    a random order, stacked in one data frame bd_10_orders.
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
#    values. Name the result shap_by_hand (Part 3 uses it).

# Keep the variables only: drop the rows whose variable_name is "intercept"
# or "" (the baseline and the prediction), then average the contribution of
# each variable
# <Your code here>

# Print the result
# <Your code here>

## Part 3. SHAP values with DALEX ----
#--------------------------------------------------------------------------#

# Help: ?predict_parts in the console, or F1 on a function name in RStudio
# Stuck, or done early? Explore other people in the app, tab SHAP waterfall:
# shiny-uros2026.pyranhia.eu (during the session only)

# a) Compute the SHAP values of my_low and my_high (about 15 seconds each)
#    and plot their waterfalls (max_features = 10).

# SHAP values of my_low
# <Your code here>

# Waterfall of the 10 most important variables
# <Your code here>

# Same for my_high
# <Your code here>

# b) Bonus: do DALEX and your hand computation agree on my_low? Keep the
#    DALEX SHAP values (rows with B == 0), join them with shap_by_hand, then
#    run the plot given in the script. Close? Why not identical?

# DALEX values: keep the rows with B == 0 (the averages over the orderings),
# and the variable name and contribution, renamed shap_dalex
# <Your code here>

# Join them with shap_by_hand, and name the result shap_compare
# <Your code here>

# Given: one point per variable, on the diagonal if both values are equal
shap_compare |>
  ggplot(aes(x = shap_dalex, y = shap_by_hand)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = variable_name)) +
  coord_fixed()

## Part 4. From local to global ----
#--------------------------------------------------------------------------#

# Help: ?scales::rescale in the console, or F1 on a function name in RStudio
# Stuck, or done early? Explore other people in the app, tab SHAP summary:
# shiny-uros2026.pyranhia.eu (during the session only)

# Given: the SHAP values precomputed for 1000 observations (DALEX would take
# more than 24 hours): one row per variable and per observation (obs_id,
# variable_name, variable_value, contribution)
shap_summary <- readRDS(here(
  "data",
  "processed",
  "shap_values.rds"
))$shap_summary |>
  as_tibble()

# a) Compute the mean absolute SHAP value of each variable and plot it as a
#    sorted bar chart. Same top variables as the permutation importance
#    (capital_gain, age)?

# Mean absolute SHAP value of each variable, sorted: name it shap_importance
# <Your code here>

# Given: the bar chart
shap_importance |>
  ggplot() +
  geom_col(aes(x = mean_abs, y = variable_name))

# b) Plot the SHAP value of age against the value of age, one point per
#    observation. Compare with the PDP of age: same shape? What does the
#    spread at a given age tell you?

# Given: the PDP of age, to compare with your plot
pdp_age <- model_profile(explainer, variables = "age")
plot(pdp_age)

# SHAP values of age, with the value of age as a number: name it shap_age
# <Your code here>

# Given: one point per observation, SHAP value against age
shap_age |>
  ggplot() +
  geom_point(aes(x = age, y = contribution), alpha = 0.3)

# c) Bonus: complete the skeleton in the script: the four continuous
#    variables on one plot, in two steps.
shap_summary |>
  # 1. keep the four continuous variables: age, capital_gain, capital_loss,
  #    hours_per_week
  # <Your code here>
  # 2. turn variable_value into a number (value), then rescale the values of
  #    each variable between 0 and 1 (value_scaled)
  # <Your code here>
  group_by(variable_name) |>
  mutate(value_scaled = scales::rescale(NULL)) |> # <Your code here>
  ungroup() |>
  # Given: one jittered point per observation
  ggplot() +
  geom_jitter(
    aes(x = contribution, y = variable_name, colour = value_scaled),
    height = 0.1,
    alpha = 0.5
  ) +
  scale_colour_viridis_c()


## Part 5. TreeSHAP with shapviz ----
#--------------------------------------------------------------------------#

# Help: ?sv_dependence in the console, or F1 on a function name in RStudio
# Stuck, or done early? Explore other people in the app, tab shapviz:
# shiny-uros2026.pyranhia.eu (during the session only)

# a) Run the code given in the script to compute sv, the TreeSHAP values of
#    every row of the test set. How long does it take, compared with one DALEX
#    waterfall?
X <- extract_recipe(gbm_fit) |>
  bake(new_data = adult_test, all_predictors())
X_pred <- bonsai:::prepare_df_lgbm(X)
lgb_booster <- extract_fit_engine(gbm_fit)
sv <- shapviz(lgb_booster, X_pred = X_pred, X = X)

# b) Plot the beeswarm of the whole test set with sv_importance(). Same story
#    as the DALEX beeswarm?
# <Your code here>

# c) Draw the dependence plot of sex with sv_dependence(), then colour the
#    points by relationship. What do you see?
#    Then go back to your beeswarm (Part 5 b): which variable catches your
#    eye, or makes you curious? Draw its dependence plot.

# Dependence plot of sex, without colour
# <Your code here>

# The same, coloured by relationship
# <Your code here>

# Dependence plot of a variable of your choice
# <Your code here>

# d) Bonus: plot the TreeSHAP waterfall of my_low with sv_waterfall(). Same
#    story as the DALEX waterfall?
# <Your code here>
