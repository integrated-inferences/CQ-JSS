# R code for evaluated code chunks in CausalQueries.qmd
# "Making, Updating, and Querying Causal Models using CausalQueries"
# Till Tietz, Lily Medina, Georgiy Syunyaev, Macartan Humphreys
# Generated using: knitr::spin("code.R")
# 15 November 2023

## Set up
#####################################################################


## -----------------------------------------------------------------------------------------------
#| label: preamble

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


theme_set(theme_bw())
set.seed(1, "L'Ecuyer-CMRG")


/* SECTION 2: Motivating example */
#####################################################################


## -----------------------------------------------------------------------------------------------

data("lipids_data")

lipids_data



## -----------------------------------------------------------------------------------------------


lipids_model <-  
  make_model("Z -> X -> Y; X <-> Y") |>
  update_model(lipids_data, refresh = 0)



## -----------------------------------------------------------------------------------------------


lipids_queries <- 
  lipids_model  |>
  query_model(query = "Y[X=1] - Y[X=0]",
              given = c("All",  "X==0 & Y==0", "X[Z=1] > X[Z=0]"),
              using = "posteriors") 

lipids_queries


## -----------------------------------------------------------------------------------------------
#| label: tbl-lipids
#| tbl-cap: "Replication of \\citet{chickering_clinicians_1996}."

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

#| results: markup


## # Calculation of parameter lengths cited in text
## 
## make_model("A -> B -> C -> D -> E") |>
##   grab("parameters_df") |>
##   nrow()
## 
## make_model("A -> E <- B; C-> E <- D") |>
##   grab("parameters_df") |>
##   nrow()


# Table 3: Nodal types and parameters for Lipids model.

## -----------------------------------------------------------------------------------------------
#| label: tbl-lipidspar
#| tbl-cap: "Nodal types and parameters for Lipids model."

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

#| results: markup

model <- make_model("X -> M -> Y <- X")


## -----------------------------------------------------------------------------------------------
#| label: fig-plots

#| fig-cap: "Examples of model graphs."
#| fig-subcap:
#|   - "Without options"
#|   - "With options"
#| fig-pos: 'h'
#| layout-ncol: 2
#| results: hold


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




make_model("X -> Y") |> 
  grab("parameters_df") 



## -----------------------------------------------------------------------------------------------
#| label: lookup-types2


make_model("X -> Y <- M; W -> Y") |> 
  interpret_type(nodes = "Y")



## -----------------------------------------------------------------------------------------------
#| label: causal-types




lipids_model |> 
  grab("causal_types")



## -----------------------------------------------------------------------------------------------
#| label: get-param-matrix




make_model("X -> Y") |> 
  grab("parameter_matrix")



## -----------------------------------------------------------------------------------------------




## make_model("X -> Y") |>
##   set_parameter_matrix()
## 


## -----------------------------------------------------------------------------------------------



model_restricted <- 
  lipids_model |> 
  set_restrictions("X[Z=1] < X[Z=0]")


## -----------------------------------------------------------------------------------------------
#| label: set-restrictions1




## model <- lipids_model |>
##   set_restrictions(labels = list(X = "01", Y = c("00", "01", "11")),
##                    keep = TRUE)
## 


## -----------------------------------------------------------------------------------------------
#| label: set-restrictions2




## model <- lipids_model |>
##   set_restrictions(labels = list(Y = "?0"))


## -----------------------------------------------------------------------------------------------
#| label: set-restrictions3



## model <- lipids_model |>
##   set_restrictions(labels = list(Y = c('00', '11')), given = 'X.00')


## -----------------------------------------------------------------------------------------------
#| label: tbl-dof
#| tbl-cap: "Number of different independent parameters (degrees of freedom) for different three-node models."


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



lipids_model |> grab("prior_hyperparameters", "X") 



## -----------------------------------------------------------------------------------------------
#| label: set-priors




## model <- lipids_model |>
##   set_priors(distribution = "jeffreys")
## 


## -----------------------------------------------------------------------------------------------
#| label: set-priors-custom


#| message: FALSE

lipids_model |> 
  set_priors(node = "X", alphas = 1:4) |> 
  grab("prior_hyperparameters", "X")



## -----------------------------------------------------------------------------------------------
#| label: set-priors-statement



lipids_model |>
  set_priors(statement = "X[Z=1] > X[Z=0]", alphas = 3) |>
  grab("prior_hyperparameters", "X")





## -----------------------------------------------------------------------------------------------
#| label: compare-flat-priors



## make_model("X -> Y") |>
##   query_model("Y[X=1] > Y[X=0]", using = "priors")
## 
## make_model("X -> M -> Y") |>
##   query_model("Y[X=1] > Y[X=0]", using = "priors")


## -----------------------------------------------------------------------------------------------
#| label: get-parameters



make_model("X -> Y") |> 
  grab("prior_hyperparameters")


## -----------------------------------------------------------------------------------------------
#| label: set-parameters



make_model("X -> Y") |>
  set_parameters(statement = "Y[X=1] > Y[X=0]", parameters = .5) |>
  grab("prior_hyperparameters")



## -----------------------------------------------------------------------------------------------
#| label: make-data



sample_data_1 <- 
  lipids_model |> 
  make_data(n = 4)



## -----------------------------------------------------------------------------------------------
#| label: make-data-draw



lipids_model |>
  make_data(n = 3, param_type = "prior_draw")



## -----------------------------------------------------------------------------------------------
#| label: make-data-incomplete


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



sample_data_2 |> collapse_data(lipids_model)


## -----------------------------------------------------------------------------------------------




make_model("X -> Y") |> 
  grab("parameter_mapping") 



## -----------------------------------------------------------------------------------------------
#| label: update-model



## model <- update_model(model, data)




## -----------------------------------------------------------------------------------------------
#| label: tbl-censored
#| tbl-cap: "Posterior inferences taking account of censoring and not."



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




## lipids_model |>
##   grab("posterior_distribution")
## 


## -----------------------------------------------------------------------------------------------




## lipids_model <-
##   lipids_model |>
##   update_model(keep_fit = TRUE,
##                keep_event_probabilities = TRUE)
## 


## -----------------------------------------------------------------------------------------------




## lipids_model |>
##   grab("stan_summary")
## 


## -----------------------------------------------------------------------------------------------
#| label: realise-outcomes


make_model("X -> Y") |> realise_outcomes()


## -----------------------------------------------------------------------------------------------
#| label: realise-outcomes-do


make_model("X -> Y") |> realise_outcomes(dos = list(X = 1))


## -----------------------------------------------------------------------------------------------
make_model("X -> Y")  |> get_query_types("Y==1")


## -----------------------------------------------------------------------------------------------
make_model("X -> Y")  |> get_query_types("Y[X=1]==1")


## -----------------------------------------------------------------------------------------------



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


data  <- data.frame(X = rep(0:1, 50), Y = rep(0:1, 50))

model <- 
  make_model("X -> Y") |>
  update_model(data, iter  = 4000, refresh = 0)

model |> grab("posterior_distribution")  |> 
  ggplot(aes(Y.01 - Y.10)) + geom_histogram() 






## -----------------------------------------------------------------------------------------------
#| label: tbl-case-level-query
#| tbl-cap: "Case level query example."


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
    linesep = "") |> 
  kableExtra::kable_classic_2(latex_options = c("scale_down", "hold_position"))




## -----------------------------------------------------------------------------------------------
#| label: tbl-case-level
#| tbl-cap: "Results for a case level query."


make_model("X -> M -> Y") |>
  update_model(data.frame(X = rep(0:1, 8), Y = rep(0:1, 8)), iter = 4000) |>
  query_model("Y[X=1] > Y[X=0]", 
              given = "X==1 & Y==1 & M==1", 
              using = "posteriors",
              case_level = c(TRUE, FALSE)) 



## -----------------------------------------------------------------------------------------------
#| label: tbl-batch-query
#| tbl-cap: "Results for two queries on two models."

#| message: false


models <- list(
 `1` = make_model("X -> Y")  |> 
        update_model( data.frame(X = rep(0:1, 10), 
                                 Y = rep(0:1,10)), refresh = 0),
 `2` = make_model("X -> Y")  |>  set_restrictions("Y[X=1] < Y[X=0]") |>
        update_model( data.frame(X = rep(0:1, 10), 
                                 Y = rep(0:1,10)), refresh = 0))


query_model(
  models,
  query = list(ATE = "Y[X=1] - Y[X=0]", 
               POS = "Y[X=1] > Y[X=0]"),
  given = c(TRUE,  "Y==1 & X==1"),
  case_level = c(FALSE, TRUE),
  using = c("priors", "posteriors"),
  expand_grid = TRUE) |>
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

