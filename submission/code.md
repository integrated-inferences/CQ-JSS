

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
data("lipids_data")

lipids_data
```

```r
lipids_model <-  
  make_model("Z -> X -> Y; X <-> Y") |>
  update_model(lipids_data, refresh = 0)
```

```r
lipids_queries <- 
  lipids_model  |>
  query_model(query = "Y[X=1] - Y[X=0]",
              given = c("All",  "X==0 & Y==0", "X[Z=1] > X[Z=0]"),
              using = "posteriors") 
```

```r
lipids_queries |>
  dplyr::select(query, given, mean, sd, starts_with("cred")) |>
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    align = "c",
    escape = TRUE, 
    linesep = "") |> 
  kableExtra::kable_classic_2(latex_options = c("scale_down"))
```

<table class=" lightable-classic-2" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:center;"> query </th>
   <th style="text-align:center;"> given </th>
   <th style="text-align:center;"> mean </th>
   <th style="text-align:center;"> sd </th>
   <th style="text-align:center;"> cred.low </th>
   <th style="text-align:center;"> cred.high </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> Y[X=1] - Y[X=0] </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> 0.55 </td>
   <td style="text-align:center;"> 0.10 </td>
   <td style="text-align:center;"> 0.37 </td>
   <td style="text-align:center;"> 0.73 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y[X=1] - Y[X=0] </td>
   <td style="text-align:center;"> X==0 &amp; Y==0 </td>
   <td style="text-align:center;"> 0.64 </td>
   <td style="text-align:center;"> 0.15 </td>
   <td style="text-align:center;"> 0.37 </td>
   <td style="text-align:center;"> 0.89 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y[X=1] - Y[X=0] </td>
   <td style="text-align:center;"> X[Z=1] &gt; X[Z=0] </td>
   <td style="text-align:center;"> 0.70 </td>
   <td style="text-align:center;"> 0.05 </td>
   <td style="text-align:center;"> 0.59 </td>
   <td style="text-align:center;"> 0.80 </td>
  </tr>
</tbody>
</table>


```r
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
```

<table class=" lightable-classic-2" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:center;"> node </th>
   <th style="text-align:center;"> nodal_type </th>
   <th style="text-align:center;"> param_set </th>
   <th style="text-align:center;"> param_names </th>
   <th style="text-align:center;"> param_value </th>
   <th style="text-align:center;"> priors </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> Z </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> Z </td>
   <td style="text-align:center;"> Z.0 </td>
   <td style="text-align:center;"> 0.57 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Z </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> Z </td>
   <td style="text-align:center;"> Z.1 </td>
   <td style="text-align:center;"> 0.43 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 00 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X.00 </td>
   <td style="text-align:center;"> 0.24 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X.10 </td>
   <td style="text-align:center;"> 0.30 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 01 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X.01 </td>
   <td style="text-align:center;"> 0.20 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X.11 </td>
   <td style="text-align:center;"> 0.27 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 00 </td>
   <td style="text-align:center;"> Y.X.00 </td>
   <td style="text-align:center;"> Y.00_X.00 </td>
   <td style="text-align:center;"> 0.71 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;"> Y.X.00 </td>
   <td style="text-align:center;"> Y.10_X.00 </td>
   <td style="text-align:center;"> 0.19 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 01 </td>
   <td style="text-align:center;"> Y.X.00 </td>
   <td style="text-align:center;"> Y.01_X.00 </td>
   <td style="text-align:center;"> 0.00 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> Y.X.00 </td>
   <td style="text-align:center;"> Y.11_X.00 </td>
   <td style="text-align:center;"> 0.10 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 00 </td>
   <td style="text-align:center;"> Y.X.01 </td>
   <td style="text-align:center;"> Y.00_X.01 </td>
   <td style="text-align:center;"> 0.15 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;"> Y.X.01 </td>
   <td style="text-align:center;"> Y.10_X.01 </td>
   <td style="text-align:center;"> 0.40 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 01 </td>
   <td style="text-align:center;"> Y.X.01 </td>
   <td style="text-align:center;"> Y.01_X.01 </td>
   <td style="text-align:center;"> 0.39 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> Y.X.01 </td>
   <td style="text-align:center;"> Y.11_X.01 </td>
   <td style="text-align:center;"> 0.06 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 00 </td>
   <td style="text-align:center;"> Y.X.10 </td>
   <td style="text-align:center;"> Y.00_X.10 </td>
   <td style="text-align:center;"> 0.17 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;"> Y.X.10 </td>
   <td style="text-align:center;"> Y.10_X.10 </td>
   <td style="text-align:center;"> 0.65 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 01 </td>
   <td style="text-align:center;"> Y.X.10 </td>
   <td style="text-align:center;"> Y.01_X.10 </td>
   <td style="text-align:center;"> 0.14 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> Y.X.10 </td>
   <td style="text-align:center;"> Y.11_X.10 </td>
   <td style="text-align:center;"> 0.04 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 00 </td>
   <td style="text-align:center;"> Y.X.11 </td>
   <td style="text-align:center;"> Y.00_X.11 </td>
   <td style="text-align:center;"> 0.24 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;"> Y.X.11 </td>
   <td style="text-align:center;"> Y.10_X.11 </td>
   <td style="text-align:center;"> 0.71 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 01 </td>
   <td style="text-align:center;"> Y.X.11 </td>
   <td style="text-align:center;"> Y.01_X.11 </td>
   <td style="text-align:center;"> 0.04 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> Y.X.11 </td>
   <td style="text-align:center;"> Y.11_X.11 </td>
   <td style="text-align:center;"> 0.01 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
</tbody>
</table>

```r
model <- make_model("X -> M -> Y <- X")
```

```r
lipids_model |> plot()

lipids_model |>
  plot(x_coord = 1:3,
       y_coord = c(3,2,1),
       textcol = "white",
       textsize = 3,
       shape = 18,
       nodecol = "grey",
       nodesize = 12)
```

![Examples of model graphs.](figure/fig-plots-1.png)![Examples of model graphs.](figure/fig-plots-2.png)

```r
make_model("X -> Y") |> 
  grab("parameters_df") 
```

```
## Mapping of model parameters to nodal types: 
## 
## ----------------------------------------------------------------
## 
##  param_names: name of parameter
##  node: name of endogeneous node associated with the parameter
##  gen: partial causal ordering of the parameter's node
##  param_set: parameter groupings forming a simplex
##  given: if model has confounding gives conditioning nodal type
##  param_value: parameter values
##  priors: hyperparameters of the prior Dirichlet distribution 
## 
## ----------------------------------------------------------------
## 
##   param_names node gen param_set nodal_type given param_value priors
## 1         X.0    X   1         X          0              0.50      1
## 2         X.1    X   1         X          1              0.50      1
## 3        Y.00    Y   2         Y         00              0.25      1
## 4        Y.10    Y   2         Y         10              0.25      1
## 5        Y.01    Y   2         Y         01              0.25      1
## 6        Y.11    Y   2         Y         11              0.25      1
```

```r
make_model("X -> Y <- M; W -> Y") |> 
  interpret_type(nodes = "Y")
```

```
## $Y
##   node position     display            interpretation
## 1    Y        1 Y[*]******* Y | M = 0 & W = 0 & X = 0
## 2    Y        2 Y*[*]****** Y | M = 1 & W = 0 & X = 0
## 3    Y        3 Y**[*]***** Y | M = 0 & W = 1 & X = 0
## 4    Y        4 Y***[*]**** Y | M = 1 & W = 1 & X = 0
## 5    Y        5 Y****[*]*** Y | M = 0 & W = 0 & X = 1
## 6    Y        6 Y*****[*]** Y | M = 1 & W = 0 & X = 1
## 7    Y        7 Y******[*]* Y | M = 0 & W = 1 & X = 1
## 8    Y        8 Y*******[*] Y | M = 1 & W = 1 & X = 1
```

```r
lipids_model |> 
  grab("causal_types")
```

```
## 
## Causal Types: 
## cartesian product of nodal types
## 
## 
##  first 10 causal types: 
##            Z  X  Y
## Z0.X00.Y00 0 00 00
## Z1.X00.Y00 1 00 00
## Z0.X10.Y00 0 10 00
## Z1.X10.Y00 1 10 00
## Z0.X01.Y00 0 01 00
## Z1.X01.Y00 1 01 00
## Z0.X11.Y00 0 11 00
## Z1.X11.Y00 1 11 00
## Z0.X00.Y10 0 00 10
## Z1.X00.Y10 1 00 10
```

```r
make_model("X -> Y") |> 
  grab("parameter_matrix")
```

```
## 
## Rows are parameters, grouped in parameter sets
## 
## Columns are causal types
## 
## Cell entries indicate whether a parameter probability isused
## in the calculation of causal type probability
## 
## 
##  
##  param_set  (P)
## 
```

```r
 make_model("X -> Y") |>
   set_parameter_matrix()
```

```
## 
## Statement: 
## X -> Y
## 
## Number of types by node:
## X Y 
## 2 4 
## 
## Number of unit types: 8
```

```r
model_restricted <- 
  lipids_model |> 
  set_restrictions("X[Z=1] < X[Z=0]")
```

```r
 model <- lipids_model |>
   set_restrictions(labels = list(X = "01", Y = c("00", "01", "11")),
                    keep = TRUE)
```

```r
 model <- lipids_model |>
   set_restrictions(labels = list(Y = "?0"))
```

```r
model <- lipids_model |>
   set_restrictions(labels = list(Y = c('00', '11')), given = 'X.00')
```

```r
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
```



|Model                                         | Degrees of freedom |
|:---------------------------------------------|:------------------:|
|`X -> Y <- W`                                 |         17         |
|`X -> Y <- W; X <-> W`                        |         18         |
|`X -> Y <- W; X <-> Y; W <-> Y`               |         62         |
|`X -> Y <- W; X <-> Y; W <-> Y; X <-> W`      |         63         |
|`X -> W -> Y <- X`                            |         19         |
|`X -> W -> Y <- X; W <-> Y`                   |         64         |
|`X -> W -> Y <- X; X <-> W; W <-> Y`          |         67         |
|`X -> W -> Y <- X; X <-> W; W <-> Y; X <-> Y` |        127         |

```r
lipids_model |> grab("prior_hyperparameters", "X") 
```

```
##       Z.0       Z.1      X.00      X.10      X.01      X.11 Y.00_X.00 Y.10_X.00 Y.01_X.00 
##         1         1         1         1         1         1         1         1         1 
## Y.11_X.00 Y.00_X.01 Y.10_X.01 Y.01_X.01 Y.11_X.01 Y.00_X.10 Y.10_X.10 Y.01_X.10 Y.11_X.10 
##         1         1         1         1         1         1         1         1         1 
## Y.00_X.11 Y.10_X.11 Y.01_X.11 Y.11_X.11 
##         1         1         1         1
```

```r
 model <- lipids_model |>
   set_priors(distribution = "jeffreys")
```

```
## No specific parameters to alter values for specified. Altering all parameters.
```

```r
lipids_model |> 
  set_priors(node = "X", alphas = 1:4) |> 
  grab("prior_hyperparameters", "X")
```

```
## Warning in make_par_values(model = model, alter = "priors", x = alphas, : A specified condition
## matches multiple parameters. In these cases it is unclear which parameter value should be
## assigned to which parameter. Assignment thus defaults to the order in which parameters appear in
## 'parameters_df'. We advise checking that parameter assignment was carried out as you intended.
```

```
##       Z.0       Z.1      X.00      X.10      X.01      X.11 Y.00_X.00 Y.10_X.00 Y.01_X.00 
##         1         1         1         2         3         4         1         1         1 
## Y.11_X.00 Y.00_X.01 Y.10_X.01 Y.01_X.01 Y.11_X.01 Y.00_X.10 Y.10_X.10 Y.01_X.10 Y.11_X.10 
##         1         1         1         1         1         1         1         1         1 
## Y.00_X.11 Y.10_X.11 Y.01_X.11 Y.11_X.11 
##         1         1         1         1
```

```r
lipids_model |>
  set_priors(statement = "X[Z=1] > X[Z=0]", alphas = 3) |>
  grab("prior_hyperparameters", "X")
```

```
##       Z.0       Z.1      X.00      X.10      X.01      X.11 Y.00_X.00 Y.10_X.00 Y.01_X.00 
##         1         1         1         1         3         1         1         1         1 
## Y.11_X.00 Y.00_X.01 Y.10_X.01 Y.01_X.01 Y.11_X.01 Y.00_X.10 Y.10_X.10 Y.01_X.10 Y.11_X.10 
##         1         1         1         1         1         1         1         1         1 
## Y.00_X.11 Y.10_X.11 Y.01_X.11 Y.11_X.11 
##         1         1         1         1
```

```r
 make_model("X -> Y") |>
   query_model("Y[X=1] > Y[X=0]", using = "priors")
```

```
## 
## Causal queries generated by query_model (all at population level)
```

```r
 make_model("X -> M -> Y") |>
   query_model("Y[X=1] > Y[X=0]", using = "priors")
```

```
## 
## Causal queries generated by query_model (all at population level)
```

```r
make_model("X -> Y") |> 
  grab("parameters")
```

```
## Model parameters with associated probabilities: 
## 
## X.0 X.1 Y.00 Y.10 Y.01 Y.11
## 0.5 0.5 0.25 0.25 0.25 0.25
```

```r
make_model("X -> Y") |>
  set_parameters(statement = "Y[X=1] > Y[X=0]", parameters = .7) |>
  grab("parameters")
```

```
## Model parameters with associated probabilities: 
## 
## X.0 X.1 Y.00 Y.10 Y.01 Y.11
## 0.5 0.5 0.1 0.1 0.7 0.1
```

```r
sample_data_1 <- 
  lipids_model |> 
  make_data(n = 4)
```

```r
lipids_model |>
  make_data(n = 3, param_type = "prior_draw")
```

```r
sample_data_2 <-
  lipids_model |>
  make_data(n = 8,
            nodes = list(c("Z", "Y"), "X"),
            probs = list(1, .5),
            subsets = list(TRUE, "Z==1 & Y==0"),
            verbose = FALSE)

sample_data_2
```

```r
sample_data_2 |> collapse_data(lipids_model)
```

```r
make_model("X -> Y") |> 
  grab("parameter_mapping") 
```

```
##      X0Y0 X1Y0 X0Y1 X1Y1
## X.0     1    0    1    0
## X.1     0    1    0    1
## Y.00    1    1    0    0
## Y.10    0    1    1    0
## Y.01    1    0    0    1
## Y.11    0    0    1    1
## attr(,"map")
##      X0Y0 X1Y0 X0Y1 X1Y1
## X0Y0    1    0    0    0
## X1Y0    0    1    0    0
## X0Y1    0    0    1    0
## X1Y1    0    0    0    1
```

```r
model <- update_model(model, data)
```

```r
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
```

<table class=" lightable-classic-2" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:center;"> model </th>
   <th style="text-align:center;"> query </th>
   <th style="text-align:center;"> mean </th>
   <th style="text-align:center;"> sd </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> uncensored </td>
   <td style="text-align:center;"> (Y[X=1] - Y[X=0]) </td>
   <td style="text-align:center;"> 0.59 </td>
   <td style="text-align:center;"> 0.20 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> censored </td>
   <td style="text-align:center;"> (Y[X=1] - Y[X=0]) </td>
   <td style="text-align:center;"> 0.02 </td>
   <td style="text-align:center;"> 0.33 </td>
  </tr>
</tbody>
</table>

```r
 make_model("X -> Y")  |>
   update_model() |>
   grab("posterior_distribution")
```

```
## No data provided
```

```
## Summary statistics of model parameter posterior distributions:
## : 4000 rows (draws) by 6 cols (parameters)
## 
##      mean   sd
## X.0  0.50 0.29
## X.1  0.50 0.29
## Y.00 0.25 0.19
## Y.10 0.24 0.19
## Y.01 0.25 0.19
## Y.11 0.25 0.20
```

```r
 lipids_model <-
   lipids_model |>
   update_model(keep_fit = TRUE,
                keep_event_probabilities = TRUE)
```

```
## No data provided
```

```r
make_model("X -> Y")  |> 
  update_model(keep_type_distribution = FALSE) |>
  grab("stan_summary") 
```

```
## No data provided
```

```
## Inference for Stan model: simplexes.
## 4 chains, each with iter=2000; warmup=1000; thin=1; 
## post-warmup draws per chain=1000, total post-warmup draws=4000.
## 
##             mean se_mean   sd   2.5%   25%   50%   75% 97.5% n_eff Rhat
## X.0         0.51    0.01 0.29   0.02  0.26  0.51  0.76  0.98  3042    1
## X.1         0.49    0.01 0.29   0.02  0.24  0.49  0.74  0.98  3042    1
## Y.00        0.25    0.00 0.19   0.01  0.09  0.21  0.37  0.71  1959    1
## Y.10        0.25    0.00 0.20   0.01  0.09  0.20  0.37  0.72  4064    1
## Y.01        0.25    0.00 0.19   0.01  0.09  0.20  0.37  0.71  3730    1
## Y.11        0.25    0.00 0.20   0.01  0.09  0.20  0.37  0.72  5122    1
## lp__       -7.54    0.04 1.62 -11.46 -8.39 -7.16 -6.35 -5.45  1412    1
## 
## Samples were drawn using NUTS(diag_e) at Wed Apr 17 18:26:10 2024.
## For each parameter, n_eff is a crude measure of effective sample size,
## and Rhat is the potential scale reduction factor on split chains (at 
## convergence, Rhat=1).
```

```r
model <- make_model("X -> Y") |> 
  update_model(refresh = 0, keep_fit = TRUE)
```

```
## No data provided
```

```r
model |> grab("stan_fit")
```

```
## Inference for Stan model: simplexes.
## 4 chains, each with iter=2000; warmup=1000; thin=1; 
## post-warmup draws per chain=1000, total post-warmup draws=4000.
## 
##             mean se_mean   sd   2.5%   25%   50%   75% 97.5% n_eff Rhat
## lambdas[1]  0.50    0.01 0.29   0.03  0.26  0.51  0.75  0.97  2989    1
## lambdas[2]  0.50    0.01 0.29   0.03  0.25  0.49  0.74  0.97  2989    1
## lambdas[3]  0.25    0.00 0.19   0.01  0.09  0.21  0.37  0.71  1868    1
## lambdas[4]  0.25    0.00 0.19   0.01  0.09  0.21  0.37  0.71  4211    1
## lambdas[5]  0.25    0.00 0.20   0.01  0.09  0.20  0.37  0.71  4126    1
## lambdas[6]  0.25    0.00 0.19   0.01  0.09  0.20  0.37  0.71  4041    1
## types[1]    0.13    0.00 0.13   0.00  0.03  0.08  0.18  0.48  2259    1
## types[2]    0.13    0.00 0.13   0.00  0.03  0.08  0.18  0.49  2255    1
## types[3]    0.13    0.00 0.13   0.00  0.03  0.08  0.18  0.48  3468    1
## types[4]    0.12    0.00 0.13   0.00  0.03  0.08  0.18  0.48  3215    1
## types[5]    0.13    0.00 0.14   0.00  0.03  0.08  0.18  0.50  3515    1
## types[6]    0.12    0.00 0.13   0.00  0.02  0.08  0.18  0.48  3378    1
## types[7]    0.13    0.00 0.14   0.00  0.03  0.08  0.18  0.50  3255    1
## types[8]    0.12    0.00 0.13   0.00  0.03  0.08  0.18  0.49  3271    1
## lp__       -7.58    0.05 1.67 -11.74 -8.45 -7.24 -6.34 -5.42  1254    1
## 
## Samples were drawn using NUTS(diag_e) at Wed Apr 17 18:26:34 2024.
## For each parameter, n_eff is a crude measure of effective sample size,
## and Rhat is the potential scale reduction factor on split chains (at 
## convergence, Rhat=1).
```

```r
np <- model |> grab("stan_fit") |> bayesplot::nuts_params()

model |> grab("stan_fit") |>
  bayesplot::mcmc_trace(pars = "lambdas[5]", np = np) +
  ylab("trace for fifth element of lambda")
```

```
## No divergences to plot.
```

![plot of chunk unnamed-chunk-41](figure/unnamed-chunk-41-1.png)

```r
make_model("X -> Y") |> realise_outcomes()
```

```r
make_model("X -> Y") |> realise_outcomes(dos = list(X = 1))
```

```r
make_model("X -> Y")  |> get_query_types("Y==1")
```

```
## 
## Causal types satisfying query's condition(s)  
## 
##  query =  Y==1 
## 
## X0.Y10  X1.Y01
## X0.Y11  X1.Y11
## 
## 
##  Number of causal types that meet condition(s) =  4
##  Total number of causal types in model =  8
```

```r
make_model("X -> Y")  |> get_query_types("Y[X=1]==1")
```

```
## 
## Causal types satisfying query's condition(s)  
## 
##  query =  Y[X=1]==1 
## 
## X0.Y01  X1.Y01
## X0.Y11  X1.Y11
## 
## 
##  Number of causal types that meet condition(s) =  4
##  Total number of causal types in model =  8
```

```r
make_model("X1 -> Y <- X2")  |>
  get_query_types("X1==1 & X2==1 & (Y[X1=1, X2=1] > Y[X1=0, X2=0])")
```

```
## 
## Causal types satisfying query's condition(s)  
## 
##  query =  X1==1&X2==1&(Y[X1=1,X2=1]>Y[X1=0,X2=0]) 
## 
## X11.X21.Y0001  X11.X21.Y0101
## X11.X21.Y0011  X11.X21.Y0111
## 
## 
##  Number of causal types that meet condition(s) =  4
##  Total number of causal types in model =  64
```

```r
make_model("X -> Y")  |> get_query_types("Y[X=1] - Y[X=0]")
```

```
## X0.Y00 X1.Y00 X0.Y10 X1.Y10 X0.Y01 X1.Y01 X0.Y11 X1.Y11 
##      0      0     -1     -1      1      1      0      0
```

```r
data  <- data.frame(X = rep(0:1, 50), Y = rep(0:1, 50))

model <- 
  make_model("X -> Y") |>
  update_model(data, iter  = 4000, refresh = 0)

model |> grab("posterior_distribution")  |> 
  ggplot(aes(Y.01 - Y.10)) + geom_histogram() 
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<div class="figure" style="text-align: center">
<img src="figure/fig-posterior-dist-1.png" alt="Posterior on &quot;Probability $Y$ is increasing in $X$&quot;." width="60%" />
<p class="caption">Posterior on "Probability $Y$ is increasing in $X$".</p>
</div>

```r
# "Results for a case level query"

make_model("X -> M -> Y") |>
  update_model(data.frame(X = rep(0:1, 8), Y = rep(0:1, 8)), iter = 4000) |>
  query_model("Y[X=1] > Y[X=0]", 
            given = "X==1 & Y==1 & M==1", 
            using = "posteriors",
            case_level = c(TRUE, FALSE)) 
```

```
## 
## Causal queries generated by query_model
```

```r
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
```

<table class=" lightable-classic-2" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:center;"> model </th>
   <th style="text-align:center;"> query </th>
   <th style="text-align:center;"> given </th>
   <th style="text-align:center;"> using </th>
   <th style="text-align:center;"> case_level </th>
   <th style="text-align:center;"> mean </th>
   <th style="text-align:center;"> sd </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.00 </td>
   <td style="text-align:center;"> 0.20 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.00 </td>
   <td style="text-align:center;"> 0.22 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.56 </td>
   <td style="text-align:center;"> 0.10 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.56 </td>
   <td style="text-align:center;"> 0.10 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> 0.22 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> 0.25 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.95 </td>
   <td style="text-align:center;"> 0.04 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.95 </td>
   <td style="text-align:center;"> 0.04 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 0.12 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 0.13 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.61 </td>
   <td style="text-align:center;"> 0.10 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.61 </td>
   <td style="text-align:center;"> 0.10 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> 0.22 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> 0.25 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.95 </td>
   <td style="text-align:center;"> 0.04 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.95 </td>
   <td style="text-align:center;"> 0.04 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.00 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.00 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.56 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.56 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.95 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.95 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.61 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.61 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> A </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.95 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> B </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.95 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
</tbody>
</table>

```r
CausalQueries:::stanmodels$simplexes
```

```
## S4 class stanmodel 'simplexes' coded as follows:
## functions{
##   row_vector col_sums(matrix X) {
##     row_vector[cols(X)] s ;
##     s = rep_row_vector(1, rows(X)) * X ;
##     return s ;
##   }
## }
## data {
## int<lower=1> n_params;
## int<lower=1> n_paths;
## int<lower=1> n_types;
## int<lower=1> n_param_sets;
## int<lower=1> n_nodes;
## array[n_param_sets] int<lower=1> n_param_each;
## int<lower=1> n_data;
## int<lower=1> n_events;
## int<lower=1> n_strategies;
## int<lower=0, upper=1> keep_type_distribution;
## vector<lower=0>[n_params] lambdas_prior;
## array[n_param_sets] int<lower=1> l_starts;
## array[n_param_sets] int<lower=1> l_ends;
## array[n_nodes] int<lower=1> node_starts;
## array[n_nodes] int<lower=1> node_ends;
## array[n_strategies] int<lower=1> strategy_starts;
## array[n_strategies] int<lower=1> strategy_ends;
## matrix[n_params, n_types] P;
## matrix[n_params, n_paths] parmap;
## matrix[n_paths, n_data] map;
## matrix<lower=0,upper=1>[n_events,n_data] E;
## array[n_events] int<lower=0> Y;
## }
## parameters {
## vector<lower=0>[n_params - n_param_sets] gamma;
## }
## transformed parameters {
## vector<lower=0, upper=1>[n_params] lambdas;
## vector<lower=1>[n_param_sets] sum_gammas;
## matrix[n_params, n_paths] parlam;
## matrix[n_nodes, n_paths] parlam2;
## vector<lower=0, upper=1>[n_paths] w_0;
## vector<lower=0, upper=1>[n_data] w;
## vector<lower=0, upper=1>[n_events] w_full;
## // Cases in which a parameter set has only one value need special handling
## // they have no gamma components and sum_gamma needs to be made manually
## for (i in 1:n_param_sets) {
##   if (l_starts[i] >= l_ends[i]) {
##     sum_gammas[i] = 1;
##     // syntax here to return unity as a vector
##     lambdas[l_starts[i]] = lambdas_prior[1]/lambdas_prior[1];
##     }
##   else if (l_starts[i] < l_ends[i]) {
##     sum_gammas[i] =
##     1 + sum(gamma[(l_starts[i] - (i-1)):(l_ends[i] - i)]);
##     lambdas[l_starts[i]:l_ends[i]] =
##     append_row(1, gamma[(l_starts[i] - (i-1)):(l_ends[i] - i)]) /
##       sum_gammas[i];
##     }
##   }
## // Mapping from parameters to data types
## // (usual case): [n_par * n_data] * [n_par * n_data]
## parlam  = rep_matrix(lambdas, n_paths) .* parmap;
## // Sum probability over nodes on each path
## for (i in 1:n_nodes) {
##  parlam2[i,] = col_sums(parlam[(node_starts[i]):(node_ends[i]),]);
##  }
## // then take product  to get probability of data type on path
## for (i in 1:n_paths) {
##   w_0[i] = prod(parlam2[,i]);
##  }
##  // last (if confounding): map to n_data columns instead of n_paths
##  w = map'*w_0;
##   // Extend/reduce to cover all observed data types
##  w_full = E * w;
## }
## model {
## // Dirichlet distributions
## for (i in 1:n_param_sets) {
##   target += dirichlet_lpdf(lambdas[l_starts[i]:l_ends[i]]  |
##     lambdas_prior[l_starts[i] :l_ends[i]]);
##   target += -n_param_each[i] * log(sum_gammas[i]);
##  }
## // Multinomials
## // Note with censoring event_probabilities might not sum to 1
## for (i in 1:n_strategies) {
##   target += multinomial_lpmf(
##   Y[strategy_starts[i]:strategy_ends[i]] |
##     w_full[strategy_starts[i]:strategy_ends[i]]/
##      sum(w_full[strategy_starts[i]:strategy_ends[i]]));
##  }
## }
## // Option to export distribution of causal types
## generated quantities{
## vector[n_types] types;
## if (keep_type_distribution == 1){
## for (i in 1:n_types) {
##    types[i] = prod(P[, i].*lambdas + 1 - P[,i]);
## }}
##  if (keep_type_distribution == 0){
##     types = rep_vector(1, n_types);
##  }
## }
```


