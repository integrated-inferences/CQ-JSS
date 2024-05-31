# R code for evaluated code chunks in CausalQueries.qmd
# "Making, Updating, and Querying Causal Models using CausalQueries"
# Till Tietz, Lily Medina, Georgiy Syunyaev, Macartan Humphreys
# Generated using: knitr::spin("code.R")
# 15 November 2023

## Set up
#####################################################################


## -----------------------------------------------------------------------------------------------
#| label: preamble
#| include: false

# knitr::purl("paper.qmd")
# knitr::spin("submission/code.R")
library(tidyverse)
library(CausalQueries)
library(microbenchmark)
library(parallel)
library(future)
library(future.apply)
library(knitr)
library(rstan)

library(tikzDevice)

options(kableExtra.latex.load_packages = FALSE)
library(kableExtra)
options(mc.cores = parallel::detectCores())


set.seed(1, "L'Ecuyer-CMRG")
theme_set(theme_bw())



## -----------------------------------------------------------------------------------------------
#| echo: true

data("lipids_data")

lipids_data



## -----------------------------------------------------------------------------------------------
#| echo: true
#| eval: true
#| purl: true

lipids_model <-  
  make_model("Z -> X -> Y; X <-> Y") |>
  update_model(lipids_data, refresh = 0)



## -----------------------------------------------------------------------------------------------
#| echo: true
#| eval: true
#| purl: true

lipids_queries <- 
  lipids_model  |>
  query_model(query = "Y[X=1] - Y[X=0]",
              given = c("All",  "X==0 & Y==0", "X[Z=1] > X[Z=0]"),
              using = "posteriors") 



## -----------------------------------------------------------------------------------------------
#| label: tbl-lipids
#| tbl-cap: "Replication of \\citet{chickering_clinicians_1996}."
#| 

lipids_queries |>
  dplyr::select(query, given, mean, sd, starts_with("cred")) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE, 
    linesep = "") |> 
  kableExtra::kable_classic_2(latex_options = c("scale_down"))



## -----------------------------------------------------------------------------------------------
#| echo: true
#| results: markup
#| include: false
#| purl: true

## # Calculation of parameter lengths cited in text
## 
 make_model("A -> B -> C -> D -> E") |>
   grab("parameters_df") |>
   nrow()
 
 make_model("A -> E <- B; C-> E <- D") |>
   grab("parameters_df") |>
   nrow()


## -----------------------------------------------------------------------------------------------
#| label: tbl-lipidspar
#| tbl-cap: "Nodal types and parameters for Lipids model."
#| 

with_pars <- 
  lipids_model |>
  set_parameters(param_type = "prior_draw") 

with_pars$parameters_df |>
  dplyr::select(node,  nodal_type, param_set, param_names, param_value, priors) |> 
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    longtable = TRUE,
    linesep = "") |> 
  kableExtra::kable_classic_2(latex_options = c("hold_position"))


## -----------------------------------------------------------------------------------------------
#| echo: true
#| results: markup

model <- make_model("X -> M -> Y <- X")


## -----------------------------------------------------------------------------------------------
#| label: fig-plots
#| echo: true
#| fig-cap: "Examples of model graphs."
#| fig-subcap:
#|   - "Without options"
#|   - "With options"
#| fig-pos: 'h'
#| layout-ncol: 2
#| results: hold
#| purl: true

lipids_model |> plot()

lipids_model |>
  plot(x_coord = 1:3,
       y_coord = c(3,2,1),
       textcol = "white",
       textsize = 3,
       shape = 18,
       nodecol = "grey",
       nodesize = 12)



## -----------------------------------------------------------------------------------------------
#| label: params-df
#| echo: true
#| eval: true
#| purl: true

make_model("X -> Y") |> 
  grab("parameters_df") 



## -----------------------------------------------------------------------------------------------
#| label: lookup-types2
#| echo: true

make_model("X -> Y <- M; W -> Y") |> 
  interpret_type(nodes = "Y")



## -----------------------------------------------------------------------------------------------
#| label: causal-types
#| echo: true
#| eval: true
#| purl: true

lipids_model |> 
  grab("causal_types")



## -----------------------------------------------------------------------------------------------
#| label: get-param-matrix
#| echo: true
#| eval: true
#| purl: true

make_model("X -> Y") |> 
  grab("parameter_matrix")



## -----------------------------------------------------------------------------------------------
 make_model("X -> Y") |>
   set_parameter_matrix()
 


## -----------------------------------------------------------------------------------------------
#| echo: true
#| eval: true

model_restricted <- 
  lipids_model |> 
  set_restrictions("X[Z=1] < X[Z=0]")


## -----------------------------------------------------------------------------------------------
#| label: set-restrictions1
#| echo: true
#| purl: true

 model <- lipids_model |>
   set_restrictions(labels = list(X = "01", Y = c("00", "01", "11")),
                    keep = TRUE)
 


## -----------------------------------------------------------------------------------------------
#| label: set-restrictions2
#| echo: true
#| purl: true

 model <- lipids_model |>
   set_restrictions(labels = list(Y = "?0"))


## -----------------------------------------------------------------------------------------------
#| label: set-restrictions3
#| echo: true
model <- lipids_model |>
   set_restrictions(labels = list(Y = c('00', '11')), given = 'X.00')


## -----------------------------------------------------------------------------------------------
#| label: tbl-dof
#| tbl-cap: "Number of different independent parameters (degrees of freedom) for different three-node models."
#| eval: true

statements <- list("X -> Y <- W", 
                   "X -> Y <- W; X <-> W", 
                   "X -> Y <- W; X <-> Y; W <-> Y",
                   "X -> Y <- W; X <-> Y; W <-> Y; X <-> W", 
                   "X -> W -> Y <- X",
                   "X -> W -> Y <- X; W <-> Y",
                   "X -> W -> Y <- X; X <-> W; W <-> Y", 
                   "X -> W -> Y <- X; X <-> W; W <-> Y; X <-> Y")

dof <- function(statement) {
  make_model(statement, add_causal_types = FALSE) |>
  grab("parameters_df")  |>
  group_by(param_set) |>
  summarize(n  = n() - 1) |>
  pull(n) |>
  sum()
}
  

statements |> 
  lapply(function(s) paste0("`", s, "`")) |> 
  unlist() |> 
  data.frame(
    Model = _,
    dof = unlist(lapply(statements, dof))) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = c("l", "c"),
    escape = TRUE, 
    linesep = "",
    col.names = c("Model", "Degrees of freedom")) 



## -----------------------------------------------------------------------------------------------
#| label: get-priors
#| echo: true
#| eval: true

lipids_model |> grab("prior_hyperparameters", "X") 



## -----------------------------------------------------------------------------------------------
#| label: set-priors
#| echo: true

 model <- lipids_model |>
   set_priors(distribution = "jeffreys")
 


## -----------------------------------------------------------------------------------------------
#| label: set-priors-custom
#| echo: true
#| eval: true
#| message: FALSE

lipids_model |> 
  set_priors(node = "X", alphas = 1:4) |> 
  grab("prior_hyperparameters", "X")



## -----------------------------------------------------------------------------------------------
#| label: set-priors-statement
#| echo: true
#| eval: true

lipids_model |>
  set_priors(statement = "X[Z=1] > X[Z=0]", alphas = 3) |>
  grab("prior_hyperparameters", "X")





## -----------------------------------------------------------------------------------------------
#| label: compare-flat-priors
#| echo: true
#| purl: true
 make_model("X -> Y") |>
   query_model("Y[X=1] > Y[X=0]", using = "priors")
 
 make_model("X -> M -> Y") |>
   query_model("Y[X=1] > Y[X=0]", using = "priors")


## -----------------------------------------------------------------------------------------------
#| label: get-parameters
#| echo: true
#| eval: true

make_model("X -> Y") |> 
  grab("parameters")


## -----------------------------------------------------------------------------------------------
#| label: set-parameters
#| echo: true
#| eval: true

make_model("X -> Y") |>
  set_parameters(statement = "Y[X=1] > Y[X=0]", parameters = .7) |>
  grab("parameters")



## -----------------------------------------------------------------------------------------------
#| label: make-data
#| echo: true
#| eval: true

sample_data_1 <- 
  lipids_model |> 
  make_data(n = 4)



## -----------------------------------------------------------------------------------------------
#| label: make-data-draw
#| echo: true
#| eval: true

lipids_model |>
  make_data(n = 3, param_type = "prior_draw")



## -----------------------------------------------------------------------------------------------
#| label: make-data-incomplete
#| echo: true
#| eval: true
#| message: false

sample_data_2 <-
  lipids_model |>
  make_data(n = 8,
            nodes = list(c("Z", "Y"), "X"),
            probs = list(1, .5),
            subsets = list(TRUE, "Z==1 & Y==0"),
            verbose = FALSE)

sample_data_2


## -----------------------------------------------------------------------------------------------
#| label: collapse-data
#| echo: true
#| eval: true

sample_data_2 |> collapse_data(lipids_model)


## -----------------------------------------------------------------------------------------------
#| echo: true
#| eval: true
#| purl: true

make_model("X -> Y") |> 
  grab("parameter_mapping") 



## -----------------------------------------------------------------------------------------------
#| label: update-model
#| echo: true

model <- update_model(model, data)




## -----------------------------------------------------------------------------------------------
#| label: tbl-censored
#| tbl-cap: "Posterior inferences taking account of censoring and not."
#| eval: true
#| purl: true

list(uncensored = 
       make_model("X -> Y") |> 
       update_model(data.frame(X = rep(0:1,5), Y = rep(0:1,5)),
                    refresh = 0, iter = 3000),
     censored = 
       make_model("X -> Y") |> 
       update_model(data.frame(X = rep(0:1,5), Y = rep(0:1,5)),
                    censored_types = c("X1Y0",  "X0Y1"),
                    refresh = 0, iter = 3000))  |>
  query_model(te("X", "Y"), using = "posteriors") |>
  dplyr::select(model, query, mean, sd) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE, 
    linesep = "") |> 
  kableExtra::kable_classic_2()


## -----------------------------------------------------------------------------------------------
#| echo: true
#| purl: true

 make_model("X -> Y")  |>
   update_model() |>
   grab("posterior_distribution")
 


## -----------------------------------------------------------------------------------------------
#| echo: true
#| purl: true

 lipids_model <-
   lipids_model |>
   update_model(keep_fit = TRUE,
                keep_event_probabilities = TRUE)
 


## -----------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| purl: true

make_model("X -> Y")  |> 
  update_model(keep_type_distribution = FALSE) |>
  grab("stan_summary") 



## -----------------------------------------------------------------------------------------------
model <- make_model("X -> Y") |> 
  update_model(refresh = 0, keep_fit = TRUE)


## -----------------------------------------------------------------------------------------------
model |> grab("stan_fit")


## -----------------------------------------------------------------------------------------------
np <- model |> grab("stan_fit") |> bayesplot::nuts_params()

model |> grab("stan_fit") |>
  bayesplot::mcmc_trace(pars = "lambdas[5]", np = np) +
  ylab("trace for fifth element of lambda")


## -----------------------------------------------------------------------------------------------
#| label: realise-outcomes
#| echo: true

make_model("X -> Y") |> realise_outcomes()


## -----------------------------------------------------------------------------------------------
#| label: realise-outcomes-do
#| echo: true

make_model("X -> Y") |> realise_outcomes(dos = list(X = 1))


## -----------------------------------------------------------------------------------------------
make_model("X -> Y")  |> get_query_types("Y==1")


## -----------------------------------------------------------------------------------------------
make_model("X -> Y")  |> get_query_types("Y[X=1]==1")


## -----------------------------------------------------------------------------------------------
#| eval: true
#| echo: true

make_model("X1 -> Y <- X2")  |>
  get_query_types("X1==1 & X2==1 & (Y[X1=1, X2=1] > Y[X1=0, X2=0])")




## -----------------------------------------------------------------------------------------------
make_model("X -> Y")  |> get_query_types("Y[X=1] - Y[X=0]")


## -----------------------------------------------------------------------------------------------
#| label: fig-posterior-dist
#| fig-cap: 'Posterior on "Probability $Y$ is increasing in $X$".'
#| fig-pos: "t"
#| fig-align: center
#| out-width: "60%"
#| purl: true

data  <- data.frame(X = rep(0:1, 50), Y = rep(0:1, 50))

model <- 
  make_model("X -> Y") |>
  update_model(data, iter  = 4000, refresh = 0)

model |> grab("posterior_distribution")  |> 
  ggplot(aes(Y.01 - Y.10)) + geom_histogram() 






## -----------------------------------------------------------------------------------------------
#| echo: true
#| eval: true

# "Results for a case level query"

make_model("X -> M -> Y") |>
  update_model(data.frame(X = rep(0:1, 8), Y = rep(0:1, 8)), iter = 4000) |>
  query_model("Y[X=1] > Y[X=0]", 
            given = "X==1 & Y==1 & M==1", 
            using = "posteriors",
            case_level = c(TRUE, FALSE)) 




## -----------------------------------------------------------------------------------------------
#| label: tbl-batch-query
#| tbl-cap: "Results for two queries on two models."
#| eval: true
#| message: false


queries |>
  dplyr::select(-starts_with("cred")) |> 
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    longtable = TRUE,
    linesep = "") |> 
  kableExtra::kable_classic_2(latex_options = c("hold_position"))






## -----------------------------------------------------------------------------------------------
#| 
#| results: markup
#| comment: ""

CausalQueries:::stanmodels$simplexes



## ----include = FALSE, eval = FALSE--------------------------------------------------------------
## 
## # effect of model complexity on run-time
## model <- list(
##   CausalQueries::make_model("X -> Y"),
##   CausalQueries::make_model("X1 -> Y <- X2"),
##   CausalQueries::make_model("X1 -> Y; X2 -> Y; X3 -> Y")
## )
## 
## data <- lapply(model, function(m) {
##   CausalQueries::make_data(m, 1000)
## })
## 
## options(mc.cores = parallel::detectCores())
## 
## benchmark_model <- microbenchmark::microbenchmark(
##   m1 = CausalQueries::update_model(model[[1]], data[[1]]),
##   m2 = CausalQueries::update_model(model[[2]], data[[2]]),
##   m3 = CausalQueries::update_model(model[[3]], data[[3]]),
##   times = 5
## )
## 
## # effect of data size on run-time
## model <- CausalQueries::make_model("X -> Y")
## 
## data <- lapply(10^c(1:5), function(n) {
##   CausalQueries::make_data(model, n)
## })
## 
## benchmark_data <- microbenchmark::microbenchmark(
##   d0 = CausalQueries::update_model(model, data[[1]]),
##   d1 = CausalQueries::update_model(model, data[[2]]),
##   d2 = CausalQueries::update_model(model, data[[3]]),
##   d3 = CausalQueries::update_model(model, data[[4]]),
##   times = 5
## )
## 

