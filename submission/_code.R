# R code for evaluated code chunks in CausalQueries.qmd
# "Making, Updating, and Querying Causal Models using CausalQueries"
# Till Tietz, Lily Medina, Georgiy Syunyaev, Macartan Humphreys
# Generated using: knitr::spin("code.R")
# 15 November 2023

## Set up
#####################################################################

# ----------------------------------------------------------------------------------------------
#| label: preamble
#| warning: false
#| message: false
#| echo: true
#| eval: true
#|
library(tidyverse)
library(CausalQueries)
library(knitr)
library(rstan)
library(kableExtra)
library(tikzDevice)

options(kableExtra.latex.load_packages = FALSE)
options(mc.cores = parallel::detectCores())

set.seed(1, "L'Ecuyer-CMRG")

## SECTION 2: Motivating example
#####################################################################

data("lipids_data")

lipids_data

## ----------------------------------------------------------------------------------------------
#| label: tbl-lipids
#| tbl-cap: "Replication of \\citet{chickering_clinicians_1996}."



lipids_model <-
  make_model("Z -> X -> Y; X <-> Y") |>
  update_model(lipids_data, refresh = 0)


results <-
  lipids_model |>
  query_model(
    query = "Y[X=1] - Y[X=0]",
    given = c("All",  "X==0 & Y==0", "X[Z=1] > X[Z=0]"),
    using = "posteriors"
  )


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



## SECTION 4: Statistical Model
#####################################################################


## ----------------------------------------------------------------------------------------------
#| label: tbl-lipidspar
#| tbl-cap: "Nodal types and parameters for Lipids model."


with_pars <-
  lipids_model |>
  set_parameters(param_type = "prior_draw")

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

## SECTION 5: Making models
#####################################################################

## ----------------------------------------------------------------------------------------------
#| results: markup

model <- make_model("X -> M -> Y <- X")



## ----------------------------------------------------------------------------------------------
#| label: fig-plots
#| fig-cap: "Examples of model graphs."

make_model("X -> M -> Y <- X; Z -> Y") |>
  plot()

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

## Parameters data frame
#####################################################################

## ----------------------------------------------------------------------------------------------
#| label: tbl-params-df
#| tbl-cap: "Example of parameters data frame."

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


## Interpreting nodal types
#####################################################################

interpretations <-
  make_model("X -> Y <- M; W -> Y") |>
  interpret_type()

interpretations$Y


## Causal types


lipids_model$causal_types |> head()


## ----------------------------------------------------------------------------------------------
#| label: get-param-matrix

make_model("X -> Y") |> get_parameter_matrix()


## Models with confounding
#####################################################################

model_restricted <-
  make_model("Z -> X -> Y; X <-> Y") |>
  set_restrictions("X[Z=1] < X[Z=0]")


## ----------------------------------------------------------------------------------------------
#| label: tbl-confound-params-df
#| tbl-cap: "Parameters data frame for model with confounding."

confounded <- make_model("X -> Y ; X <-> Y")

confounded$parameters_df |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_classic_2(latex_options = c("scale_down", "HOLD_position"))


## ----------------------------------------------------------------------------------------------
#| label: tbl-confound-param-matrix
#| tbl-cap: "Parameter matrix for model with confounding."



get_parameter_matrix(confounded) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_classic_2(latex_options = c("scale_down"))


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


# get-priors

make_model("X -> Y") |> get_priors()


# set-priors-custom

make_model("X -> Y") |>
  set_priors(1:6) |>
  get_priors()


# label: set-priors-statement

make_model("X -> Y") |>
  set_priors(statement = "Y[X=1] > Y[X=0]", alphas = 3) |>
  get_priors()


# label: get-parameters


make_model("X -> Y") |>
  get_parameters()


# label: set-parameters

make_model("X -> Y") |>
  set_parameters(statement = "Y[X=1] > Y[X=0]", parameters = .5) |>
  get_parameters()


## Drawing and manipulating data


model <- make_model("X -> M -> Y")

sample_data_1 <-
  model |>
  make_data(n = 4)


make_data(model, n = 3, param_type = "prior_draw")

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

sample_data_2


# Collapse data

sample_data_2 |> collapse_data(model)


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


## SECTION 7: Querying models
#####################################################################


# realise_outcomes


make_model("X -> Y") |> realise_outcomes()


make_model("X -> Y") |> realise_outcomes(dos = list(X = 1))


# get_query_types

make_model("X -> Y")  |> get_query_types("Y==1")



make_model("X -> Y")  |> get_query_types("Y[X=1]==1")


make_model("X1 -> Y <- X2")  |>
  get_query_types("X1==1 & X2==1 & (Y[X1=1, X2=1] > Y[X1=0, X2=0])")



make_model("X -> Y")  |> get_query_types("Y[X=1] - Y[X=0]")

## ----------------------------------------------------------------------------------------------
#| label: fig-posterior-dist
#| fig-cap: 'Posterior on "Probability $Y$ is increasing in $X$".'
#| fig-pos: "t"
#| fig-align: center
#| out-width: "60%"

# posterior distributions

data  <- data.frame(X = rep(0:1, 50), Y = rep(0:1, 50))

model <-
  make_model("X -> Y") |>
  update_model(data, iter  = 4000, refresh = 0)


model$posterior_distribution |>
  ggplot(aes(Y.01 - Y.10)) + geom_histogram() + theme_bw()



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


## ----------------------------------------------------------------------------------------------
#| label: tbl-case-level
#| tbl-cap: "Results for a case level query."




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


# batch queries

## ----------------------------------------------------------------------------------------------
#| label: tbl-batch-query
#| tbl-cap: "Results for two queries on two models."
#| message: false


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

## Appendix C: Benchmarks
#####################################################################

# effect of model complexity on run-time
options(mc.cores = parallel::detectCores())

model <- list(
  CausalQueries::make_model("X1 -> Y"),
  CausalQueries::make_model("X1 -> Y <- X2"),
  CausalQueries::make_model("X1 -> Y <- X2; X3 -> Y")
)


## ----------------------------------------------------------------------------------------------
#| message: false
#| warning: false
benchmark_model <- microbenchmark::microbenchmark(
  m1 = CausalQueries::update_model(model[[1]]),
  m2 = CausalQueries::update_model(model[[2]]),
  m3 = CausalQueries::update_model(model[[3]]),
  times = 5
)

summary(benchmark_model) |> select(expr, mean) |>
  kable(digits = 0)

# effect of data size on run-time

model <- CausalQueries::make_model("X -> Y")

data <- lapply(10 ^ c(1:5), function(n) {
  CausalQueries::make_data(model, n)
})

## ----------------------------------------------------------------------------------------------
#| message: false
#| warning: false
benchmark_data <- microbenchmark::microbenchmark(
  d0 = CausalQueries::update_model(model, data[[1]]),
  d1 = CausalQueries::update_model(model, data[[2]]),
  d2 = CausalQueries::update_model(model, data[[3]]),
  d3 = CausalQueries::update_model(model, data[[4]]),
  d4 = CausalQueries::update_model(model, data[[5]]),
  times = 5
)

summary(benchmark_data) |> select(expr, mean) |>
  kable(digits = 0)