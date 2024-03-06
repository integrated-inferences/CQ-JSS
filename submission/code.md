

```r
# R code for evaluated code chunks in CausalQueries.qmd
# "Making, Updating, and Querying Causal Models using CausalQueries"
# Till Tietz, Lily Medina, Georgiy Syunyaev, Macartan Humphreys
# Generated using: knitr::spin("code.R")
# 15 November 2023

## Set up
#####################################################################
```

```r
#|
library(tidyverse)
library(CausalQueries)
library(knitr)
library(rstan)
library(kableExtra)
library(tikzDevice)

options(kableExtra.latex.load_packages = FALSE)
options(mc.cores = parallel::detectCores())

set.seed(20231018)

## SECTION 2: Motivating example
#####################################################################

data("lipids_data")

lipids_data
```

```
##    event strategy count
## 1 Z0X0Y0      ZXY   158
## 2 Z1X0Y0      ZXY    52
## 3 Z0X1Y0      ZXY     0
## 4 Z1X1Y0      ZXY    23
## 5 Z0X0Y1      ZXY    14
## 6 Z1X0Y1      ZXY    12
## 7 Z0X1Y1      ZXY     0
## 8 Z1X1Y1      ZXY    78
```

```r
lipids_model <-
  make_model("Z -> X -> Y; X <-> Y") |>
  update_model(lipids_data, refresh = 0)
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
results <-
  lipids_model |>
  query_model(
    query = "Y[X=1] - Y[X=0]",
    given = c("All",  "X==0 & Y==0", "X[Z=1] > X[Z=0]"),
    using = "posteriors"
  )
```

```
## Error in is(model, "causal_model"): object 'lipids_model' not found
```

```r
results |>
  dplyr::select(query, given, mean, sd, starts_with("cred")) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_classic_2(latex_options = c("scale_down"))
```

```
## Error in dplyr::select(results, query, given, mean, sd, starts_with("cred")): object 'results' not found
```

```r
## SECTION 4: Statistical Model
#####################################################################
```

```r
with_pars <-
  lipids_model |>
  set_parameters(param_type = "prior_draw")
```

```
## Error in get_parameters(model): object 'lipids_model' not found
```

```r
with_pars$parameters_df |>
  dplyr::select(node,  nodal_type, param_set, 
                param_names, param_value, priors) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    longtable = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_classic_2(latex_options = c("hold_position"))
```

```
## Error in dplyr::select(with_pars$parameters_df, node, nodal_type, param_set, : object 'with_pars' not found
```

```r
## SECTION 5: Making models
#####################################################################
```

```r
model <- make_model("X -> M -> Y <- X")
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
make_model("X -> M -> Y <- X; Z -> Y") |>
  plot()
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
make_model("X -> M -> Y <- X; Z -> Y") |>
  plot(
    x_coord = 1:4,
    y_coord = c(1.5, 2, 1, 2),
    textcol = "white",
    textsize = 3,
    shape = 18,
    nodecol = "grey",
    nodesize = 12
  )
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
## Parameters data frame
#####################################################################
```

```r
latex_options = "HOLD_position"
make_model("X -> Y")$parameters_df |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_classic_2(latex_options = c("scale_down", "HOLD_position"))
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
## Interpreting nodal types
#####################################################################

interpretations <-
  make_model("X -> Y <- M; W -> Y") |>
  interpret_type()
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
interpretations$Y
```

```
## Error in eval(expr, envir, enclos): object 'interpretations' not found
```

```r
## Causal types


lipids_model$causal_types |> head()
```

```
## Error in head(lipids_model$causal_types): object 'lipids_model' not found
```

```r
make_model("X -> Y") |> get_parameter_matrix()
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
## Models with confounding
#####################################################################

model_restricted <-
  make_model("Z -> X -> Y; X <-> Y") |>
  set_restrictions("X[Z=1] < X[Z=0]")
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
confounded <- make_model("X -> Y ; X <-> Y")
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
confounded$parameters_df |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_classic_2(latex_options = c("scale_down", "HOLD_position"))
```

```
## Error in knitr::kable(confounded$parameters_df, digits = 2, booktabs = TRUE, : object 'confounded' not found
```

```r
get_parameter_matrix(confounded) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_classic_2(latex_options = c("scale_down"))
```

```
## Error in get_parameter_matrix(confounded): object 'confounded' not found
```

```r
## Degrees of freedom
#####################################################################


statements <- list(
  "X -> Y <- W",
  "X -> Y <- W; X <-> W",
  "X -> Y <- W; X <-> Y; W <-> Y",
  "X -> Y <- W; X <-> Y; W <-> Y; X <->W",
  "X -> W -> Y <- X",
  "X -> W -> Y <- X; W <-> Y",
  "X -> W -> Y <- X; X <-> W; W <-> Y",
  "X -> W -> Y <- X; X <-> W; W <-> Y; X <-> Y"
)

dof <- function(statement)
  make_model(statement, add_causal_types = FALSE)$parameters_df |>
  group_by(param_set) |>
  summarize(n  = n() - 1) |>
  pull(n) |>
  sum()

data.frame(Model = statements |> unlist(),
           dof = statements |> lapply(dof) |> unlist()) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = c("l", "c"),
    escape = TRUE,
    linesep = ""
  )
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
# get-priors

make_model("X -> Y") |> get_priors()
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
# set-priors-custom

make_model("X -> Y") |>
  set_priors(1:6) |>
  get_priors()
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
# label: set-priors-statement

make_model("X -> Y") |>
  set_priors(statement = "Y[X=1] > Y[X=0]", alphas = 3) |>
  get_priors()
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
# label: get-parameters


make_model("X -> Y") |>
  get_parameters()
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
# label: set-parameters

make_model("X -> Y") |>
  set_parameters(statement = "Y[X=1] > Y[X=0]", parameters = .5) |>
  get_parameters()
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
## Drawing and manipulating data


model <- make_model("X -> M -> Y")
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
sample_data_1 <-
  model |>
  make_data(n = 4)
```

```
## Error in get_parameters(model): object 'model' not found
```

```r
make_data(model, n = 3, param_type = "prior_draw")
```

```
## Error in minimum_components %in% names(model): object 'model' not found
```

```r
# Incompete data

sample_data_2 <-
  make_data(
    model,
    n = 8,
    nodes = list(c("X", "Y"), "M"),
    probs = list(1, .5),
    subsets = list(TRUE, "X==1 & Y==0"),
    verbose = FALSE
  )
```

```
## Error in get_parameters(model): object 'model' not found
```

```r
sample_data_2
```

```
## Error in eval(expr, envir, enclos): object 'sample_data_2' not found
```

```r
# Collapse data

sample_data_2 |> collapse_data(model)
```

```
## Error in collapse_data(sample_data_2, model): object 'model' not found
```

```r
## SECTION 6: Updating models
#####################################################################


# get_parmap

make_model("X -> Y") |>
  get_parmap() |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_classic_2()
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
# censored data

list(
  uncensored =
    make_model("X -> Y") |>
    update_model(
      data.frame(X = rep(0:1, 5), Y = rep(0:1, 5)),
      refresh = 0,
      iter = 3000
    ),
  censored =
    make_model("X -> Y") |>
    update_model(
      data.frame(X = rep(0:1, 5), Y = rep(0:1, 5)),
      censored_types = c("X1Y0",  "X0Y1"),
      refresh = 0,
      iter = 3000
    )
)  |>
  query_model(te("X", "Y"), using = "posteriors") |>
  dplyr::select(model, query, mean, sd) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_classic_2()
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
## SECTION 7: Querying models
#####################################################################


# realise_outcomes


make_model("X -> Y") |> realise_outcomes()
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
make_model("X -> Y") |> realise_outcomes(dos = list(X = 1))
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
# get_query_types

make_model("X -> Y")  |> get_query_types("Y==1")
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
make_model("X -> Y")  |> get_query_types("Y[X=1]==1")
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
make_model("X1 -> Y <- X2")  |>
  get_query_types("X1==1 & X2==1 & (Y[X1=1, X2=1] > Y[X1=0, X2=0])")
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
make_model("X -> Y")  |> get_query_types("Y[X=1] - Y[X=0]")
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
# posterior distributions

data  <- data.frame(X = rep(0:1, 50), Y = rep(0:1, 50))

model <-
  make_model("X -> Y") |>
  update_model(data, iter  = 4000, refresh = 0)
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
model$posterior_distribution |>
  ggplot(aes(Y.01 - Y.10)) + geom_histogram() + theme_bw()
```

```
## Error in ggplot(model$posterior_distribution, aes(Y.01 - Y.10)): object 'model' not found
```

```r
# case level queries

lipids_model |>
  query_model(query = "Y[X=1] - Y[X=0]",
              given = c("X==1 & Y==1 & Z==1"),
              using = "posteriors") |>
  dplyr::select(query, given, mean, sd, starts_with("cred")) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_classic_2(latex_options = c("scale_down", "hold_position"))
```

```
## Error in is(model, "causal_model"): object 'lipids_model' not found
```

```r
set.seed(1)

make_model("X -> M -> Y") |>
  update_model(data.frame(X = rep(0:1, 8), Y = rep(0:1, 8)),
               refresh = 0,
               iter = 10000) |>
  query_model(
    "Y[X=1] > Y[X=0]",
    given = "X==1 & Y==1 & M==1",
    using = "posteriors",
    case_level = c(TRUE, FALSE)
  ) |>
  dplyr::select(query, given, case_level, mean, sd) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    longtable = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_classic_2()
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
# batch queries
```

```r
models <- list(
  `1` = make_model("X -> Y")  |>
    update_model(data.frame(
      X = rep(0:1, 10),
      Y = rep(0:1, 10)
    ), refresh = 0),
  `2` = make_model("X -> Y")  |>  set_restrictions("Y[X=1] < Y[X=0]") |>
    update_model(data.frame(
      X = rep(0:1, 10),
      Y = rep(0:1, 10)
    ), refresh = 0)
)
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
query_model(
  models,
  query = list(ATE = "Y[X=1] - Y[X=0]",
               POS = "Y[X=1] > Y[X=0]"),
  given = c(TRUE,  "Y==1 & X==1"),
  case_level = c(FALSE, TRUE),
  using = c("priors", "posteriors"),
  expand_grid = TRUE
) |>
  dplyr::select(-starts_with("cred")) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    longtable = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_classic_2(latex_options = c("hold_position"))
```

```
## Error in is(model, "causal_model"): object 'models' not found
```

```r
## Appendix C: Benchmarks
#####################################################################

# effect of model complexity on run-time
options(mc.cores = parallel::detectCores())

model <- list(
  CausalQueries::make_model("X1 -> Y"),
  CausalQueries::make_model("X1 -> Y <- X2"),
  CausalQueries::make_model("X1 -> Y <- X2; X3 -> Y")
)
```

```
## Warning in data.frame(., stringsAsFactors = FALSE): restarting interrupted promise evaluation

## Warning in data.frame(., stringsAsFactors = FALSE): internal error -3 in R_decompress1
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
benchmark_model <- microbenchmark::microbenchmark(
  m1 = CausalQueries::update_model(model[[1]]),
  m2 = CausalQueries::update_model(model[[2]]),
  m3 = CausalQueries::update_model(model[[3]]),
  times = 5
)
```

```
## Error in collapse_data(., model): object 'model' not found
```

```r
summary(benchmark_model) |> select(expr, mean) |>
  kable(digits = 0)
```

```
## Error in h(simpleError(msg, call)): error in evaluating the argument 'object' in selecting a method for function 'summary': object 'benchmark_model' not found
```

```r
# effect of data size on run-time

model <- CausalQueries::make_model("X -> Y")
```

```
## Error in data.frame(., stringsAsFactors = FALSE): lazy-load database 'C:/Users/humphreys/AppData/Local/R/win-library/4.2/dagitty/R/dagitty.rdb' is corrupt
```

```r
data <- lapply(10 ^ c(1:5), function(n) {
  CausalQueries::make_data(model, n)
})
```

```
## Error in get_parameters(model): object 'model' not found
```

```r
benchmark_data <- microbenchmark::microbenchmark(
  d0 = CausalQueries::update_model(model, data[[1]]),
  d1 = CausalQueries::update_model(model, data[[2]]),
  d2 = CausalQueries::update_model(model, data[[3]]),
  d3 = CausalQueries::update_model(model, data[[4]]),
  d4 = CausalQueries::update_model(model, data[[5]]),
  times = 5
)
```

```
## Error in .subset2(x, i, exact = exact): subscript out of bounds
```

```r
summary(benchmark_data) |> select(expr, mean) |>
  kable(digits = 0)
```

```
## Error in h(simpleError(msg, call)): error in evaluating the argument 'object' in selecting a method for function 'summary': object 'benchmark_data' not found
```

