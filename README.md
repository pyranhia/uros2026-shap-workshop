# Opening the Black Box: Interpreting Machine Learning Models with SHAP Values in R

Hands-on tutorial given by Thelma Panaïotis ([Pyranhia](https://pyranhia.eu)) at **uRos2026** (Use of R in Official Statistics), Paris, 18 November 2026.

This repository contains what you need to code along during the session: the exercise script, the trained model, the data, a demo Shiny app and the pinned R environment. The slides, course notes and setup guide are on the tutorial website: [shap-uros2026.pyranhia.eu](https://shap-uros2026.pyranhia.eu).

**Prerequisites:** intermediate R, tidyverse, basic ML concepts (train/test split, model fitting). No prior knowledge of SHAP is required.

## Two ways to get ready

### Option A: on your machine

You need **R 4.6 or later** and RStudio. The install takes a few minutes: do it ahead of the session, on a good connection.

```bash
git clone https://github.com/pyranhia/uros2026-shap-workshop.git
```

Open `uros2026-shap-workshop.Rproj` in RStudio (**File > Open Project...**), then in the R Console:

```r
renv::restore()  # installs all packages at the pinned versions
```

You are done when you see `The library is already synchronized with the lockfile.`

Test it: run the setup part of `R/02_shap_exercises.R` without error, then open `app/shiny_demo/app.R` and click **Run App**.

If the install fails and you cannot sort it out quickly, use Option B.

### Option B: on the RStudio Server

Nothing to install. Usernames and passwords are handed out at the start of the session. Go to [rstudio.pyranhia.eu](https://rstudio.pyranhia.eu), sign in, and open `uros2026-shap-workshop.Rproj` from the **Files** tab: the repository and the environment are already there. Do not run the demo app from the server: use the dedicated instance, [shiny-uros2026.pyranhia.eu](https://shiny-uros2026.pyranhia.eu) (available during the session only).

## Repository content

```
├── R/
│   ├── 02_shap_exercises.R   # the hands-on exercises: start here
│   └── 01_train_model.R      # optional: how the model was trained (see below)
├── app/shiny_demo/           # demo Shiny app (Run App)
├── data/processed/           # train/test sets and precomputed SHAP values
├── models/                   # trained LightGBM model
├── assets/fonts/             # fonts used by the app
└── renv.lock                 # pinned R environment
```

## Optional: how the model was trained

`R/01_train_model.R` shows how the model and the train/test sets were made. It is there out of curiosity: you do **not** need it for the session, the trained model and the data are already in the repository. If you want to run it, you also need the `datapyranhia` package (the Adult dataset), which is hosted on GitHub and not part of the pinned environment:

```r
install.packages("pak")
pak::pak("pyranhia/datapyranhia")
```

Running the script overwrites `models/gbm_adult.rds` and the train/test sets.

## Data

The model is trained on the Adult income dataset (UCI Machine Learning Repository), available in the `datapyranhia` package. The train/test sets in `data/processed/` are derived from it. The dataset is licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

> Becker, B. & Kohavi, R. (1996). Adult [Dataset]. UCI Machine Learning Repository. <https://doi.org/10.24432/C5XW20>

## License

MIT, see [LICENSE](LICENSE).
