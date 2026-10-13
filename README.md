# Opening the Black Box: Interpreting Machine Learning Models with SHAP Values in R

Hands-on tutorial given by Thelma Panaïotis ([Pyranhia](https://pyranhia.eu)) at **uRos2026** (Use of R in Official Statistics), Paris, 18 November 2026.

This repository contains what you need to code along during the session: the exercise script, the trained model, the data, a demo Shiny app and the pinned R environment. The slides and course notes are shared separately.

**Prerequisites:** intermediate R, tidyverse, basic ML concepts (train/test split, model fitting). No prior knowledge of SHAP is required.

## Two ways to get ready

### Option A: on your machine

You need **R 4.6 or later** and RStudio. The install takes a few minutes: do it ahead of the session, on a good connection.

```bash
git clone https://github.com/pyranhia/uros2026-shap-workshop.git
```

Open `tutoriel_SHAP_uRos2026.Rproj` in RStudio (**File > Open Project...**), then in the R Console:

```r
install.packages("pak")
pak::pak("pyranhia/datapyranhia")  # data package, GitHub only (not on CRAN)
renv::restore()                    # all other packages, pinned versions
```

The order matters: `renv::restore()` alone cannot install `datapyranhia`. You are done when you see `The library is already synchronized with the lockfile.`

Test it: run the setup part of `R/02_shap_exercises.R` without error, then open `app/shiny_demo/app.R` and click **Run App**.

If `pak::pak()` fails with a GitHub rate-limit or authentication error, set up a [GitHub personal access token](https://happygitwithr.com/https-pat). If you cannot sort it out quickly, use Option B.

### Option B: on the RStudio Server

Nothing to install. Usernames and passwords are handed out at the start of the session. Go to [rstudio.pyranhia.eu](https://rstudio.pyranhia.eu), sign in, and open `tutoriel_SHAP_uRos2026.Rproj` from the **Files** tab: the repository and the environment are already there. Do not run the demo app from the server: a dedicated instance is used for it, its address will be given during the session.

## Repository content

```
├── R/
│   ├── 02_shap_exercises.R   # the hands-on exercises: start here
│   └── 01_train_model.R      # how the model and the train/test sets were made
├── app/shiny_demo/           # demo Shiny app (Run App)
├── data/processed/           # train/test sets and precomputed SHAP values
├── models/                   # trained LightGBM model
├── assets/fonts/             # fonts used by the app
└── renv.lock                 # pinned R environment
```

## Data

The model is trained on the Adult income dataset, shipped in the [datapyranhia](https://github.com/pyranhia/datapyranhia) package. The dataset is licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

> Becker, B. & Kohavi, R. (1996). Adult [Dataset]. UCI Machine Learning Repository. <https://doi.org/10.24432/C5XW20>

## License

MIT, see [LICENSE](LICENSE).
