params <-
list(run = FALSE)

## ----------------------------------------------------------------------------------------------
#| label: preamble
#| include: false

# knitr::purl("paper.qmd")

options(kableExtra.latex.load_packages = FALSE)

library(tidyverse)
library(CausalQueries)
library(knitr)
library(rstan)
library(DeclareDesign)
library(kableExtra)
library(tikzDevice)

set.seed(20231018)


## ----------------------------------------------------------------------------------------------
#| echo: true

data("lipids_data")

lipids_data



## ----------------------------------------------------------------------------------------------
#| echo: true
#| eval: false

## make_model("Z -> X -> Y; X <-> Y") |>
##   update_model(lipids_data, refresh = 0) |>
##   query_model(query = "Y[X=1] - Y[X=0]",
##               given = c("All",  "X==0 & Y==0", "X[Z=1] > X[Z=0]"),
##               using = "posteriors")


## ----------------------------------------------------------------------------------------------
#| label: tbl-lipids
#| tbl-cap: "Replication of \\citet{chickering_clinicians_1996}."
#| echo: false

if (params$run) {
  
  lipids_model <- 
    make_model("Z -> X -> Y; X <-> Y") |>
    update_model(lipids_data, refresh = 0) 
  
  lipids_model |> 
    readr::write_rds(x = _, file = "saved/lipids_model.rds")
     
  lipids_model |>
    query_model(
      query = "Y[X=1] - Y[X=0]",
      given = c("All",  "X==0 & Y==0", "X[Z=1] > X[Z=0]"),
      using = "posteriors") |>
    readr::write_rds(x = _, file = "saved/lipids_results.rds")

}

lipids_model <- read_rds("saved/lipids_model.rds")

results <- read_rds("saved/lipids_results.rds")

results |>
  dplyr::select(query, given, mean, sd, starts_with("cred")) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE, 
    linesep = "") |> 
  kableExtra::kable_classic_2(latex_options = c("scale_down"))



## ----------------------------------------------------------------------------------------------
#| echo: true
#| results: markup
#| eval: false
#| include: false

## make_model("A -> B -> C -> D -> E")$parameters_df |> nrow()
## 
## make_model("A -> E <- B; C->E<-D")$parameters_df |> nrow()


## ----------------------------------------------------------------------------------------------
#| label: tbl-lipidspar
#| tbl-cap: "Nodal types and parameters for Lipids model."
#| echo: false

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


## ----------------------------------------------------------------------------------------------
#| echo: true
#| results: markup

model <- make_model("X -> M -> Y <- X")


## ----------------------------------------------------------------------------------------------
#| echo: true
#| eval: false

## make_model("X -> M -> Y <- X; Z -> Y") |>
##   plot()
## 


## ----------------------------------------------------------------------------------------------
#| echo: true
#| eval: false

## make_model("X -> M -> Y <- X; Z -> Y") |>
##   plot(x_coord = 1:4,
##        y_coord = c(1.5,2,1,2),
##        textcol = "white",
##        textsize = 3,
##        shape = 18,
##        nodecol = "grey",
##        nodesize = 12)
## 


## ----------------------------------------------------------------------------------------------
#| label: fig-plots
#| echo: false
#| fig-cap: "Examples of model graphs."
#| fig-subcap:
#|   - "Without options"
#|   - "With options"
#| fig-pos: 'h'
#| layout-ncol: 2

make_model("X -> M -> Y <- X; Z -> Y") |>
  plot()

make_model("X -> M -> Y <- X; Z -> Y") |>
  plot(x_coord = 1:4,
       y_coord = c(1.5,2,1,2),
       textcol = "white",
       textsize = 3,
       shape = 18,
       nodecol = "grey",
       nodesize = 12)



## ----------------------------------------------------------------------------------------------
#| label: params-df
#| echo: true
#| eval: false

## make_model("X -> Y")$parameters_df


## ----------------------------------------------------------------------------------------------
#| label: tbl-params-df
#| echo: false
#| eval: true
#| tbl-cap: "Example of parameters data frame."

latex_options = "HOLD_position"
make_model("X -> Y")$parameters_df |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE, 
    linesep = "") |> 
  kableExtra::kable_classic_2(latex_options = c("scale_down", "HOLD_position"))


## ----------------------------------------------------------------------------------------------
#| label: lookup-types2
#| echo: true

interpretations <- 
  make_model("X -> Y <- M; W -> Y") |> 
  interpret_type()

interpretations$Y


## ----------------------------------------------------------------------------------------------
#| label: nodal-types
#| echo: true
#| eval: false

## make_model("X -> Y")$nodal_types


## ----------------------------------------------------------------------------------------------
#| label: nodal-types-nointerpret
#| echo: false
#| eval: false

## # make_model("X -> Y")$nodal_types |> `attr<-`("interpret", NULL)
## make_model("X -> Y")$nodal_types


## ----------------------------------------------------------------------------------------------
#| label: causal-types
#| echo: true

lipids_model$causal_types |> head()


## ----------------------------------------------------------------------------------------------
#| label: get-param-matrix
#| echo: true

make_model("X -> Y") |> get_parameter_matrix()


## ----------------------------------------------------------------------------------------------
#| echo: true
#| eval: false

## make_model("X -> Y") |> set_parameter_matrix()


## ----------------------------------------------------------------------------------------------
#| echo: true
#| eval: true

model_restricted <- 
  make_model("Z -> X -> Y; X <-> Y") |> 
  set_restrictions("X[Z=1] < X[Z=0]")


## ----------------------------------------------------------------------------------------------
#| label: set-restrictions1
#| echo: true
#| eval: false

## make_model("S -> C -> Y <- R <- X; X -> C -> R") |>
##   set_restrictions(labels = list(C = "1000", R = "0001", Y = "0001"),
##                    keep = TRUE)


## ----------------------------------------------------------------------------------------------
#| label: set-restrictions2
#| echo: true
#| eval: false

## make_model("X -> Y") |> set_restrictions(labels = list(Y = "?0"))


## ----------------------------------------------------------------------------------------------
#| label: set-restrictions3
#| echo: true
#| eval: false

## model <-
##   make_model("X -> Y -> Z; X <-> Z") |>
##   set_restrictions(labels = list(X = '0', Y = c('00', '11'), Z = '00'),
##                    given = c(NA, NA, 'X.1'))


## ----------------------------------------------------------------------------------------------
#| label: confound-params-df
#| echo: true
#| eval: false

## confounded <- make_model("X -> Y ; X <-> Y")
## 


## ----------------------------------------------------------------------------------------------
#| label: tbl-confound-params-df
#| tbl-cap: "Parameters data frame for model with confounding."
#| echo: false
#| eval: true

confounded <- make_model("X -> Y ; X <-> Y")

confounded$parameters_df |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE, 
    linesep = "") |> 
  kableExtra::kable_classic_2(latex_options = c("scale_down", "HOLD_position"))


## ----------------------------------------------------------------------------------------------
#| label: tbl-confound-param-matrix
#| tbl-cap: "Parameter matrix for model with confounding."
#| echo: false
#| eval: true

get_parameter_matrix(confounded) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE, 
    linesep = "") |> 
  kableExtra::kable_classic_2(latex_options = c("scale_down"))


## ----------------------------------------------------------------------------------------------
#| label: fig-confound
#| echo: false
#| fig-cap: "Graph of model with confounding."
#| fig-pos: 't'
#| fig-align: center
#| out-width: "60%"
#| eval: false

## make_model("A <- X -> B; A <-> X; B <-> X") |>
##   plot()


## ----------------------------------------------------------------------------------------------
#| label: tbl-dof
#| tbl-cap: "Number of different independent parameters (degrees of freedom) for different 3 node models."
#| echo: false
#| eval: true

statements <- list("X -> Y <- W", 
                   "X -> Y <- W; X <-> W", 
                   "X -> Y <- W; X <-> Y; W <-> Y",
                   "X -> Y <- W; X <-> Y; W <-> Y; X <->W", 
                   "X -> W -> Y <- X",
                   "X -> W -> Y <- X; W <-> Y",
                   "X -> W -> Y <- X; X <-> W; W <-> Y", 
                   "X -> W -> Y <- X; X <-> W; W <-> Y; X <-> Y")

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
    linesep = "") 



## ----------------------------------------------------------------------------------------------
#| label: get-priors
#| echo: true
#| eval: true

make_model("X -> Y") |> get_priors()


## ----------------------------------------------------------------------------------------------
#| label: set-priors
#| echo: true
#| eval: false

## make_model("X -> Y") |> set_priors(distribution = "jeffreys")


## ----------------------------------------------------------------------------------------------
#| label: set-priors-custom
#| echo: true
#| eval: true

make_model("X -> Y") |> 
  set_priors(1:6) |> 
  get_priors()


## ----------------------------------------------------------------------------------------------
#| label: set-priors-statement
#| echo: true
#| eval: true

make_model("X -> Y") |>
  set_priors(statement = "Y[X=1] > Y[X=0]", alphas = 3) |>
  get_priors()


## ----------------------------------------------------------------------------------------------
#| label: set-priors-nodal-type
#| echo: true
#| eval: false

## make_model("X -> Y") |>
##   set_priors(nodal_type = "01", alphas = 3) |>
##   get_priors()


## ----------------------------------------------------------------------------------------------
#| label: set-priors-param-names
#| echo: true
#| eval: false

## make_model("X -> Y") |>
##   set_priors(param_names = "Y.01", alphas = 3) |>
##   get_priors()


## ----------------------------------------------------------------------------------------------
#| label: set-priors-other
#| echo: true
#| eval: false

## model <- make_model("X -> M -> Y; X <-> Y")
## 
## model |>
##   set_priors(node = "Y",
##              nodal_type = c("01","11"),
##              given = "X.1",
##              alphas = c(3,2))
## 
## model |>
##   set_priors(
##     alter_at =
##       "node == 'Y' & nodal_type %in% c('01','11') & given == 'X.1'",
##     alphas = c(3,2))


## ----------------------------------------------------------------------------------------------
#| label: set-priors-flat
#| echo: true
#| eval: false

## make_model("X -> Y") |>
##   set_restrictions(decreasing("X", "Y")) |>
##   query_model("Y[X=1] - Y[X=0]", using = "priors")


## ----------------------------------------------------------------------------------------------
#| label: compare-flat-priors
#| echo: true
#| eval: false

## make_model("X -> Y") |>
##   query_model("Y[X=1] > Y[X=0]", using = "priors")
## 
## make_model("X -> M -> Y") |>
##   query_model("Y[X=1] > Y[X=0]", using = "priors")


## ----------------------------------------------------------------------------------------------
#| label: tbl-compare-flat-priors
#| tbl-cap: "Comparison between $X \\rightarrow Y$ vs $X \\rightarrow M \\rightarrow Y$ Models."
#| tbl-subcap:
#|    - "Average Causal Effect in $X \\rightarrow Y$ Model."
#|    - "Average Causal Effect in $X \\rightarrow M \\rightarrow Y$ Model."
#| echo: false
#| eval: false
#| include: false

## 
## make_model("X -> Y") |>
##   query_model("Y[X=1] > Y[X=0]", n_draws = 10000) |>
##   dplyr::select(query, given, mean, sd, starts_with("cred")) |>
##   knitr::kable(
##     digits = 2,
##     booktabs = TRUE,
##     align = "c",
##     escape = TRUE,
##     linesep = "") |>
##   kableExtra::kable_classic_2(latex_options = c("scale_down"))
## 
## make_model("X -> M -> Y") |>
##   query_model("Y[X=1] > Y[X=0]", n_draws = 10000) |>
##   dplyr::select(query, given, mean, sd, starts_with("cred")) |>
##   knitr::kable(
##     digits = 2,
##     booktabs = TRUE,
##     align = "c",
##     escape = TRUE,
##     linesep = "") |>
##   kableExtra::kable_classic_2(latex_options = c("scale_down"))


## ----------------------------------------------------------------------------------------------
#| label: get-parameters
#| echo: true
#| eval: true

make_model("X -> Y") |> 
  get_parameters()


## ----------------------------------------------------------------------------------------------
#| label: set-parameters
#| echo: true
#| eval: true

make_model("X -> Y") |>
  set_parameters(statement = "Y[X=1] > Y[X=0]", parameters = .5) |>
  get_parameters()


## ----------------------------------------------------------------------------------------------
#| label: make-data-model
#| eval: true

model <- make_model("X -> M -> Y") 



## ----------------------------------------------------------------------------------------------
#| label: make-data
#| echo: true
#| eval: true

sample_data_1 <- 
  model |> 
  make_data(n = 4)



## ----------------------------------------------------------------------------------------------
#| label: make-data-draw
#| echo: true
#| eval: true

make_data(model, n = 3, param_type = "prior_draw")



## ----------------------------------------------------------------------------------------------
#| label: make-data-incomplete
#| echo: true
#| eval: true
#| message: false

sample_data_2 <-
  make_data(model,
            n = 8,
            nodes = list(c("X", "Y"), "M"),
            probs = list(1, .5),
            subsets = list(TRUE, "X==1 & Y==0"),
            verbose = FALSE)

sample_data_2


## ----------------------------------------------------------------------------------------------
#| label: collapse-data
#| echo: true
#| eval: true

sample_data_2 |> collapse_data(model)


## ----------------------------------------------------------------------------------------------
#| label: prep-stan-data
#| echo: true
#| eval: false

## sample_data_2 |>
##   collapse_data(model = model) |>
##   CausalQueries:::prep_stan_data(model = model)


## ----------------------------------------------------------------------------------------------
#| echo: true
#| eval: false
## make_model("X -> Y") |>
##   get_parmap()


## ----------------------------------------------------------------------------------------------
#| label: tbl-parmap
#| tbl-cap: "Mapping from parameters to data types."
#| echo: false
#| eval: true

make_model("X -> Y") |> 
  get_parmap() |>   
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE, 
    linesep = "") |> 
  kableExtra::kable_classic_2()


## ----------------------------------------------------------------------------------------------
#| label: update-model
#| echo: true
#| eval: false

## update_model(model, data)


## ----------------------------------------------------------------------------------------------
#| echo: true
#| eval: false

## data <- data.frame(X = rep(0:1, 5), Y = rep(0:1, 5))
## 
## list(
##   uncensored =
##     make_model("X -> Y") |>
##     update_model(data),
##   censored =
##     make_model("X -> Y") |>
##     update_model(data, censored_types = c("X1Y0",  "X0Y1"))) |>
##   query_model(te("X", "Y"), using = "posteriors")


## ----------------------------------------------------------------------------------------------
#| label: tbl-censored
#| tbl-cap: "Posterior inferences taking account of censoring and not."
#| echo: false
#| eval: true

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


## ----------------------------------------------------------------------------------------------
#| label: realise-outcomes
#| echo: true

make_model("X -> Y") |> realise_outcomes()


## ----------------------------------------------------------------------------------------------
#| label: realise-outcomes-do
#| echo: true

make_model("X -> Y") |> realise_outcomes(dos = list(X = 1))


## ----------------------------------------------------------------------------------------------
make_model("X -> Y")  |> get_query_types("Y==1")


## ----------------------------------------------------------------------------------------------
make_model("X -> Y")  |> get_query_types("Y[X=1]==1")


## ----------------------------------------------------------------------------------------------
#| eval: true
#| echo: true

make_model("X1 -> Y <- X2")  |>
  get_query_types("X1==1 & X2==1 & (Y[X1=1, X2=1] > Y[X1=0, X2=0])")


## ----------------------------------------------------------------------------------------------
#| eval: false
#| echo: false

## make_model('X -> M -> Y <- X') |>
##   get_query_types(query = "Y[X=1, M=1] > Y[X=0, M=0]",
##                   map = "nodal_type")


## ----------------------------------------------------------------------------------------------
#| eval: false
#| echo: false

## model <- make_model('X -> Y')
## 
## query <- model |> get_query_types(query = "Y[X=1] > Y[X=0]")
## given <- model |> get_query_types(query = "Y==1 & X==1")
## 
## model$causal_types |>
##   mutate(query = query$types,
##          given = given$types)


## ----------------------------------------------------------------------------------------------
make_model("X -> Y")  |> get_query_types("Y[X=1] - Y[X=0]")


## ----------------------------------------------------------------------------------------------
#| eval: false

## data  <- data.frame(X = rep(0:1, 50), Y = rep(0:1, 50))
## 
## model <-
##   make_model("X -> Y") |>
##   update_model(data, iter  = 4000)
## 
## model$posterior_distribution  |>
##   ggplot(aes(Y.01 - Y.10)) + geom_histogram()


## ----------------------------------------------------------------------------------------------
#| label: fig-posterior-dist
#| fig-cap: 'Posterior on "Probability $Y$ is increasing in $X$".'
#| fig-pos: "t"
#| fig-align: center
#| out-width: "60%"
#| echo: false

if (params$run) {

  data  <- data.frame(X = rep(0:1, 50), Y = rep(0:1, 50))

  make_model("X -> Y") |>
    update_model(data, iter  = 4000, refresh = 0) |>
    write_rds("saved/app_2_illus.rds")

}

model <- read_rds("saved/app_2_illus.rds")

model$posterior_distribution |> 
  ggplot(aes(Y.01 - Y.10)) + geom_histogram() + theme_bw()


## ----------------------------------------------------------------------------------------------
#| eval: false
#| echo: true

## make_model("X -> Y") |>
##   query_distribution(
##     query = list(increasing = "(Y[X=1] > Y[X=0])"),
##     using = "priors")


## ----------------------------------------------------------------------------------------------
#| label: case-level-query
#| echo: true
#| eval: false

## lipids_model |>
##     query_model(query = "Y[X=1] - Y[X=0]",
##                 given = c("X==1 & Y==1 & Z==1"),
##                 using = "posteriors")


## ----------------------------------------------------------------------------------------------
#| label: tbl-case-level-query
#| tbl-cap: "Case level query example."
#| echo: false
#| eval: true

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


## ----------------------------------------------------------------------------------------------
#| echo: true
#| eval: false

## make_model("X -> M -> Y") |>
##   update_model(data.frame(X = rep(0:1, 8), Y = rep(0:1, 8)), iter = 10000) |>
##   query_model(
##     query = "Y[X=1] > Y[X=0]",
##     given = "X==1 & Y==1 & M==1",
##     using = "posteriors",
##     case_level = c(TRUE, FALSE))
## 


## ----------------------------------------------------------------------------------------------
#| label: tbl-case-level
#| tbl-cap: "Results for a case level query."
#| echo: false
#| eval: true

if (params$run) {
  
  set.seed(1)
 
  model <-
    make_model("X -> M -> Y") |>
    update_model(data.frame(X = rep(0:1, 8), Y = rep(0:1, 8)), iter = 10000) |>
    write_rds("saved/caselevel.rds")
  
}

read_rds("saved/caselevel.rds") |> 
  query_model("Y[X=1] > Y[X=0]", 
            given = "X==1 & Y==1 & M==1", 
            using = "posteriors",
            case_level = c(TRUE, FALSE)) |>
  dplyr::select(query, given, case_level, mean, sd) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE,
    longtable = TRUE,
    linesep = "") |> 
  kableExtra::kable_classic_2()


## ----------------------------------------------------------------------------------------------
#| label: batch-query
#| echo: true
#| eval: false

## models <- list(
##   `1` =
##     update_model(make_model("X -> Y"),
##                  data.frame(X = rep(0:1, 10), Y = rep(0:1,10)), refresh = 0),
##   `2` =
##     update_model(set_restrictions(make_model("X -> Y"), "Y[X=1] < Y[X=0]"),
##                  data.frame(X = rep(0:1, 10), Y = rep(0:1,10)), refresh = 0))
## 
## query_model(
##   models,
##   query = list(ATE = "Y[X=1] - Y[X=0]",
##                POS = "Y[X=1] > Y[X=0]"),
##   given = c(TRUE,  "Y==1 & X==1"),
##   case_level = c(FALSE, TRUE),
##   using = c("priors", "posteriors"),
##   expand_grid = TRUE)


## ----------------------------------------------------------------------------------------------
#| label: tbl-batch-query
#| tbl-cap: "Results for two queries on two models."
#| echo: false
#| eval: true
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


## ----------------------------------------------------------------------------------------------
#| eval: false

## library(parallel)
## 
## options(mc.cores = parallel::detectCores())


## ----------------------------------------------------------------------------------------------
#| echo: true
#| eval: false

## library(future)
## library(future.apply)
## 
## chains <- 3
## cores <- 8
## 
## future::plan(list(
##       future::tweak(future::multisession,
##                     workers = floor(cores/(chains + 1))),
##       future::tweak(future::multisession,
##                     workers = chains)
##     ))
## 
## model <- make_model("X -> Y")
## data <- list(data_1, data_2)
## 
## future.apply::future_lapply(data, function(d) {
##   update_model(
##     model = model,
##     data = d,
##     chains = chains,
##     refresh = 0
##   )
## })


## ----------------------------------------------------------------------------------------------
#| echo: false
#| results: markup
#| comment: ""
if (params$run) {
  
  make_model("X -> Y") |> 
    update_model(
      data = data,
      keep_fit = TRUE, 
      refresh = 0) |> 
    (\(.) .$stan_objects$stan_fit)() |>
    write_rds("saved/fit.rds")
  
}

cat(rstan::get_stancode(read_rds("saved/fit.rds")))


