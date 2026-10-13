# SHAP exercises: write your own code
# Same progression and same exercises as the course notes (hints are in
# the notes, in the collapsible "Hint" blocks), on an observation of your
# own choosing:
#   1. break-down (one fixed order)
#   2. SHAP waterfall (one observation, averaged over all orders)
#   3. SHAP values of one observation, in the format of the SHAP summary
#   4. the fast route: TreeSHAP with shapviz
#
# The model is already trained (see 01_train_model.R): nothing to refit.
# Parts 1-3 use DALEX::predict_parts() (predicted-probability scale),
# Part 4 uses shapviz (log-odds scale): same idea, two computational
# routes. See "Going faster with shapviz and TreeSHAP" in the notes.

library(tidymodels)
library(bonsai)
library(DALEXtra)
library(shapviz)
library(here)


## Setup: load the model and the data, build the explainer ----
#--------------------------------------------------------------------------#

gbm_fit <- readRDS(here("models", "gbm_adult.rds"))
adult_test <- readRDS(here("data", "processed", "adult_test.rds"))

# Build the DALEX explainer (same as "Build your explainer" in the "Setup"
# section of the notes): no y, and a random sample of 500 rows as reference
# data to keep predict_parts() fast (your numbers will differ slightly from
# those of the notes: the sample is random)
# <Your code here>


## Part 1. Break-down ----
#--------------------------------------------------------------------------#

# a) Pick ONE observation (a row of adult_test) you find interesting
# <Your code here>

# b) Compute and plot its break-down
# <Your code here>


## Part 2. SHAP waterfall (individual) ----
#--------------------------------------------------------------------------#

# Compute and plot the SHAP waterfall of the same observation, and
# compare it with its break-down (takes about 15 seconds)
# <Your code here>


## Part 3. From one observation to the SHAP summary ----
#--------------------------------------------------------------------------#

# predict_parts(type = "shap") takes SEVERAL MINUTES per observation with the
# whole test set as reference, so the values for 1000 observations were
# precomputed (see the notes).

# Compute the SHAP values of ONE observation (your Part 1 one) and look
# at how the result is structured: the rows with B == 0 are the averaged
# contributions, the format stored in the precomputed summary
# <Your code here>


## Part 4. The fast route: TreeSHAP with shapviz ----
#--------------------------------------------------------------------------#

# Exact (not sampled) and vectorized, on the log-odds scale: don't expect
# the numbers to match Parts 1-3, only the general story.

# a) Build the sv object for the whole test set (X, X_pred, booster,
#    shapviz()). Beware: as.factor() on the test set is a trap, use the
#    fitted recipe
# <Your code here>

# b) Beeswarm of the whole test set (time it if you are curious)
# <Your code here>

# c) Dependence plot of a variable of your choice
# <Your code here>

# d) Bonus, check and explain: check that baseline + sum of SHAP values
#    reproduces the model's predictions, then draw the waterfall of your
#    Part 1 observation
# <Your code here>
