# Replication code for:
# "Making, Updating, and Querying Causal Models using CausalQueries"
# Till Tietz, Lily Medina, Georgiy Syunyaev, Macartan Humphreys
# Generated using: knitr::spin("code.R")
# 29 May 2024

## Set up
#####################################################################


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  preamble
#| include :  false

library(tidyverse)
library(CausalQueries)
library(microbenchmark)
library(parallel)
library(future)
library(future.apply)
library(knitr)
library(rstan)


options(kableExtra.latex.load_packages = FALSE)
library(kableExtra)

#| include :  true

options(mc.cores = parallel::detectCores())
set.seed(1, "L'Ecuyer-CMRG")
theme_set(theme_bw())


## 2. Motivating example
#####################################################################

#' Running example showing how to make, update, and query a model
#' with real data


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#| include :  true

data("lipids_data")

lipids_data

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


lipids_model <-
  make_model("Z -> X -> Y; X <-> Y") |>
  update_model(lipids_data)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

lipids_queries <-
  lipids_model  |>
  query_model(
    query = "Y[X=1] - Y[X=0]",
    given = c("All", "X==0 & Y==0", "X[Z=1] > X[Z=0]"),
    using = "posteriors"
  )


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

lipids_queries |>
  dplyr::select(query, given, mean, sd, starts_with("cred")) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_classic_2(latex_options = c("scale_down"))



## 4. Statistical model
#####################################################################


#' Nodal types and parameters for Lipids model.

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  tbl-lipidspar
#| tbl-cap :  "Nodal types and parameters for Lipids model."


with_pars <-
  lipids_model |>
  set_parameters(param_type = "prior_draw")

with_pars$parameters_df |>
  dplyr::select(node, nodal_type, param_set, param_names, param_value, priors) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    longtable = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_classic_2(latex_options = c("hold_position"))


## 5. Making models
#####################################################################

#'
## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' graphing models

#| results :  markup

model <- make_model("X -> M -> Y <- X")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  fig-plots

#| fig-cap :  "Examples of model graphs."
#| fig-subcap:#|   - "Without options"
#|   - "With options"
#| fig-pos :  'h'
#| layout-ncol :  2
#| results :  hold


lipids_model |> plot()

lipids_model |>
  plot(
    x_coord = 1:3,
    y_coord = 3:1,
    textcol = "white",
    textsize = 3,
    shape = 18,
    nodecol = "grey",
    nodesize = 12
  )


#' inspecting model elements

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  params-df


make_model("X -> Y") |>
  grab("parameters_df")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  lookup-types2

make_model("X -> Y <- M; W -> Y") |>
  interpret_type(nodes = "Y")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  causal-types


lipids_model |>
  grab("causal_types")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  get-param-matrix


make_model("X -> Y") |>
  grab("parameter_matrix")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


model <- make_model("X -> Y") |>
  set_parameter_matrix()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' setting restrictions

model_restricted <-
  lipids_model |>
  set_restrictions("X[Z=1] < X[Z=0]")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  set-restrictions1

model <-
  lipids_model |>
  set_restrictions(labels = list(X = "01", Y = c("00", "01", "11")), keep = TRUE)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  set-restrictions2

model <- lipids_model |>
  set_restrictions(labels = list(Y = "?0"))

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  set-restrictions3

model <- lipids_model |>
  set_restrictions(labels = list(Y = c('00', '11')), given = 'X.00')

#' degrees of freedom for models with confounding


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  tbl-dof


statements <- list(
  "X -> Y <- W",
  "X -> Y <- W; X <-> W",
  "X -> Y <- W; X <-> Y; W <-> Y",
  "X -> Y <- W; X <-> Y; W <-> Y; X <-> W",
  "X -> W -> Y <- X",
  "X -> W -> Y <- X; W <-> Y",
  "X -> W -> Y <- X; X <-> W; W <-> Y",
  "X -> W -> Y <- X; X <-> W; W <-> Y; X <-> Y"
)

dof <- function(statement) {
  make_model(statement, add_causal_types = FALSE) |>
    grab("parameters_df")  |>
    group_by(param_set) |>
    summarize(n  = n() - 1) |>
    pull(n) |>
    sum()
}


statements |>
  lapply(function(s)
    paste0("`", s, "`")) |>
  unlist() |>
  data.frame(Model = _, dof = unlist(lapply(statements, dof))) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = c("l", "c"),
    escape = TRUE,
    linesep = "",
    col.names = c("Model", "Degrees of freedom")
  )


#' setting priors

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  get-priors



lipids_model |>
  grab("prior_hyperparameters", "X")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  set-priors



model <- lipids_model |>
  set_priors(distribution = "jeffreys")


#' an example that generates a warning because parameter assignment
#' depends on knowledge of ordering of parameters

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  set-priors-custom


#| message :  FALSE

lipids_model |>
  set_priors(node = "X", alphas = 1:4) |>
  grab("prior_hyperparameters", "X")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  set-priors-statement



lipids_model |>
  set_priors(statement = "X[Z=1] > X[Z=0]", alphas = 3) |>
  grab("prior_hyperparameters", "X")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  set-priors-flat

query <-
  make_model("X -> Y") |>
  set_restrictions(decreasing("X", "Y")) |>
  query_model("Y[X=1] - Y[X=0]", using = "priors")


#' setting parameters


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  get-parameters



make_model("X -> Y") |>
  grab("parameters")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  set-parameters



make_model("X -> Y") |>
  set_parameters(statement = "Y[X=1] > Y[X=0]", parameters = .7) |>
  grab("parameters")


#' making data


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  make-data



sample_data_1 <-
  lipids_model |>
  make_data(n = 4)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  make-data-draw



lipids_model |>
  make_data(n = 3, param_type = "prior_draw")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  make-data-incomplete


#| message :  false

sample_data_2 <-
  lipids_model |>
  make_data(
    n = 8,
    nodes = list(c("Z", "Y"), "X"),
    probs = list(1, .5),
    subsets = list(TRUE, "Z==1 & Y==0")
  )

sample_data_2


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  collapse-data



sample_data_2 |>
  collapse_data(lipids_model)

## 6. Updating
#####################################################################


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




make_model("X -> Y") |>
  grab("parameter_mapping")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




data <- data.frame(X = rep(0:1, 5), Y = rep(0:1, 5))

list(
  uncensored =
    update_model(make_model("X -> Y"), data),
  censored =
    update_model(make_model("X -> Y"), data, censored_types = c("X1Y0", "X0Y1"))
) |>
  query_model("Y[X=1] - Y[X=0]", using = "posteriors") |>
  subset(select = c("model", "query", "mean", "sd"))



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



model <-
  make_model("X -> Y")  |>
  update_model()

posterior <- grab(model, "posterior_distribution")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



lipids_model <-
  lipids_model |>
  update_model(keep_fit = TRUE, keep_event_probabilities = TRUE)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




make_model("X -> Y")  |>
  update_model(keep_type_distribution = FALSE) |>
  grab("stan_summary")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




model <-
  make_model("X -> Y") |>
  update_model(refresh = 0, keep_fit = TRUE)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' accessing the stan fit



model |>
  grab("stan_fit")


## 7. Queries
#####################################################################


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  realise-outcomes


make_model("X -> Y") |>
  realise_outcomes()



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  realise-outcomes-do


make_model("X -> Y") |>
  realise_outcomes(dos = list(X = 1))


#' accessing causal types satisfying queries

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

make_model("X -> Y")  |>
  get_query_types("Y==1")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

make_model("X -> Y")  |>
  get_query_types("Y[X=1]==1")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



make_model("X1 -> Y <- X2")  |>
  get_query_types("X1==1 & X2==1 & (Y[X1=1, X2=1] > Y[X1=0, X2=0])")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
make_model("X -> Y") |>
  get_query_types("Y[X=1] - Y[X=0]")



#' posterior distribution illustration
## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data  <- data.frame(X = rep(0:1, 50), Y = rep(0:1, 50))

model <-
  make_model("X -> Y") |>
  update_model(data, iter  = 4000, refresh = 0)

model |>
  grab("posterior_distribution")  |>
  ggplot(aes(Y.01 - Y.10)) + geom_histogram()


#' querying functions
## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


queries <-
  make_model("X -> Y") |>
  query_distribution(
    query = list(increasing = "(Y[X=1] > Y[X=0])", ATE = "(Y[X=1] - Y[X=0])"),
    using = "priors"
  )



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  case-level-query




lipids_model |>
  query_model(query = "Y[X=1] - Y[X=0]",
              given = "X==1 & Y==1 & Z==1",
              using = "posteriors")  |>
  subset(select = c("query", "mean", "sd"))



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


make_model("X -> M -> Y") |>
  update_model(data.frame(X = rep(0:1, 8), Y = rep(0:1, 8)), iter = 4000) |>
  query_model(
    "Y[X=1] > Y[X=0]",
    given = "X==1 & Y==1 & M==1",
    using = "posteriors",
    case_level = c(TRUE, FALSE)
  )  |>
  subset(select = c("query", "case_level", "mean", "sd"))



#' batch queries

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  batch-query


models <- list(
  A = lipids_model |>
    update_model(data = lipids_data, refresh = 0),
  B = lipids_model |> set_restrictions("X[Z=1] < X[Z=0]") |>
    update_model(data = lipids_data, refresh = 0)
)

queries <-
  query_model(
    models,
    query = list(ATE = "Y[X=1] - Y[X=0]", POS = "Y[X=1] > Y[X=0]"),
    given = c(TRUE, "Y==1 & X==1"),
    case_level = c(FALSE, TRUE),
    using = c("priors", "posteriors"),
    expand_grid = TRUE
  )



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label :  tbl-batch-query
#| tbl-cap :  "Results for two queries on two models."


#| message :  false


queries |>
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


## Appendix
#####################################################################

#' Parallelization illustration
#'
## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(parallel)

options(mc.cores = parallel::detectCores())


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

chains <- 3
cores <- 4

future::plan(list(
  future::tweak(future::multisession, workers = floor(cores / (chains + 1))),
  future::tweak(future::multisession, workers = chains)
))

model <- make_model("X -> Y")
data <- list(data_1 = data.frame(X = 0:1, Y = 0:1),
             data_2 = data.frame(X = 0:1, Y = 1:0))

results <-
  future.apply::future_lapply(data, function(d) {
    update_model(
      model = model,
      data = d,
      chains = chains,
      refresh = 0
    )
  }, future.seed = TRUE)


#' Stan code

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#| results :  markup

CausalQueries:::stanmodels$simplexes


#' Benchmarking
#' Results depend on features of user systems

#'
#' effect of model complexity on run-time

models <- list(
  CausalQueries::make_model("X1 -> Y"),
  CausalQueries::make_model("X1 -> Y; X2 -> Y"),
  CausalQueries::make_model("X1 -> Y; X2 -> Y; X3 -> Y")
)

#' number of parameters in each model
lapply(models, function(m)
  m |> grab("parameters") |> length())

data <- expand_grid(X1 = 0:1, X2 = 0:1, X3 = 0:1, Y = 0:1) |>
  data.frame()

benchmark_model <- microbenchmark::microbenchmark(
  m1 = CausalQueries::update_model(
    models[[1]], data, keep_type_distribution = FALSE),
  m2 = CausalQueries::update_model(
    models[[2]], data, keep_type_distribution = FALSE),
  m3 = CausalQueries::update_model(
    models[[3]], data, keep_type_distribution = FALSE),
  times = 10
)
print(benchmark_model)

#' effect of data size on run-time
#'
model <- CausalQueries::make_model("X -> Y")

data <- 10 ^ c(1:5) |>
  lapply(function(n)
    CausalQueries::make_data(model, n))

benchmark_data <- microbenchmark::microbenchmark(
  d1 = CausalQueries::update_model(
    model, data[[1]], keep_type_distribution = FALSE),
  d2 = CausalQueries::update_model(
    model, data[[2]], keep_type_distribution = FALSE),
  d3 = CausalQueries::update_model(
    model, data[[3]], keep_type_distribution = FALSE),
  d4 = CausalQueries::update_model(
    model, data[[4]], keep_type_distribution = FALSE),
  d5 = CausalQueries::update_model(
    model, data[[5]], keep_type_distribution = FALSE),
  times = 10
)

print(benchmark_data)
