

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
   <td style="text-align:center;"> 0.90 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y[X=1] - Y[X=0] </td>
   <td style="text-align:center;"> X[Z=1] &gt; X[Z=0] </td>
   <td style="text-align:center;"> 0.70 </td>
   <td style="text-align:center;"> 0.06 </td>
   <td style="text-align:center;"> 0.59 </td>
   <td style="text-align:center;"> 0.80 </td>
  </tr>
</tbody>
</table>

```r
## SECTION 4: Statistical Model
#####################################################################
```

```r
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
   <td style="text-align:center;"> 0.37 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Z </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> Z </td>
   <td style="text-align:center;"> Z.1 </td>
   <td style="text-align:center;"> 0.63 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 00 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X.00 </td>
   <td style="text-align:center;"> 0.02 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X.10 </td>
   <td style="text-align:center;"> 0.31 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 01 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X.01 </td>
   <td style="text-align:center;"> 0.06 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X.11 </td>
   <td style="text-align:center;"> 0.62 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 00 </td>
   <td style="text-align:center;"> Y.X.00 </td>
   <td style="text-align:center;"> Y.00_X.00 </td>
   <td style="text-align:center;"> 0.09 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;"> Y.X.00 </td>
   <td style="text-align:center;"> Y.10_X.00 </td>
   <td style="text-align:center;"> 0.42 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 01 </td>
   <td style="text-align:center;"> Y.X.00 </td>
   <td style="text-align:center;"> Y.01_X.00 </td>
   <td style="text-align:center;"> 0.19 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> Y.X.00 </td>
   <td style="text-align:center;"> Y.11_X.00 </td>
   <td style="text-align:center;"> 0.30 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 00 </td>
   <td style="text-align:center;"> Y.X.01 </td>
   <td style="text-align:center;"> Y.00_X.01 </td>
   <td style="text-align:center;"> 0.05 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;"> Y.X.01 </td>
   <td style="text-align:center;"> Y.10_X.01 </td>
   <td style="text-align:center;"> 0.41 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 01 </td>
   <td style="text-align:center;"> Y.X.01 </td>
   <td style="text-align:center;"> Y.01_X.01 </td>
   <td style="text-align:center;"> 0.13 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> Y.X.01 </td>
   <td style="text-align:center;"> Y.11_X.01 </td>
   <td style="text-align:center;"> 0.41 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 00 </td>
   <td style="text-align:center;"> Y.X.10 </td>
   <td style="text-align:center;"> Y.00_X.10 </td>
   <td style="text-align:center;"> 0.26 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;"> Y.X.10 </td>
   <td style="text-align:center;"> Y.10_X.10 </td>
   <td style="text-align:center;"> 0.07 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 01 </td>
   <td style="text-align:center;"> Y.X.10 </td>
   <td style="text-align:center;"> Y.01_X.10 </td>
   <td style="text-align:center;"> 0.11 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> Y.X.10 </td>
   <td style="text-align:center;"> Y.11_X.10 </td>
   <td style="text-align:center;"> 0.55 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 00 </td>
   <td style="text-align:center;"> Y.X.11 </td>
   <td style="text-align:center;"> Y.00_X.11 </td>
   <td style="text-align:center;"> 0.22 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;"> Y.X.11 </td>
   <td style="text-align:center;"> Y.10_X.11 </td>
   <td style="text-align:center;"> 0.06 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 01 </td>
   <td style="text-align:center;"> Y.X.11 </td>
   <td style="text-align:center;"> Y.01_X.11 </td>
   <td style="text-align:center;"> 0.54 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> Y.X.11 </td>
   <td style="text-align:center;"> Y.11_X.11 </td>
   <td style="text-align:center;"> 0.18 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
</tbody>
</table>

```r
## SECTION 5: Making models
#####################################################################
```

```r
model <- make_model("X -> M -> Y <- X")
```

```r
make_model("X -> M -> Y <- X; Z -> Y") |>
  plot()
```

![Examples of model graphs.](figure/fig-plots-1.png)

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

![Examples of model graphs.](figure/fig-plots-2.png)

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

<table class=" lightable-classic-2" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:center;"> param_names </th>
   <th style="text-align:center;"> node </th>
   <th style="text-align:center;"> gen </th>
   <th style="text-align:center;"> param_set </th>
   <th style="text-align:center;"> nodal_type </th>
   <th style="text-align:center;"> given </th>
   <th style="text-align:center;"> param_value </th>
   <th style="text-align:center;"> priors </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> X.0 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> X.1 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y.00 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 00 </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y.10 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y.01 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 01 </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y.11 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
</tbody>
</table>

```r
## Interpreting nodal types
#####################################################################

interpretations <-
  make_model("X -> Y <- M; W -> Y") |>
  interpret_type()

interpretations$Y
```

```
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
## Causal types


lipids_model$causal_types |> head()
```

```
##            Z  X  Y
## Z0.X00.Y00 0 00 00
## Z1.X00.Y00 1 00 00
## Z0.X10.Y00 0 10 00
## Z1.X10.Y00 1 10 00
## Z0.X01.Y00 0 01 00
## Z1.X01.Y00 1 01 00
```

```r
make_model("X -> Y") |> get_parameter_matrix()
```

```
## 
## Rows are parameters, grouped in parameter sets
## 
## Columns are causal types
## 
## Cell entries indicate whether a parameter probability is used
## in the calculation of causal type probability
## 
##      X0.Y00 X1.Y00 X0.Y10 X1.Y10 X0.Y01 X1.Y01 X0.Y11 X1.Y11
## X.0       1      0      1      0      1      0      1      0
## X.1       0      1      0      1      0      1      0      1
## Y.00      1      1      0      0      0      0      0      0
## Y.10      0      0      1      1      0      0      0      0
## Y.01      0      0      0      0      1      1      0      0
## Y.11      0      0      0      0      0      0      1      1
## 
##  
##  param_set  (P)
## 
```

```r
## Models with confounding
#####################################################################

model_restricted <-
  make_model("Z -> X -> Y; X <-> Y") |>
  set_restrictions("X[Z=1] < X[Z=0]")
```

```r
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
```

<table class=" lightable-classic-2" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:center;"> param_names </th>
   <th style="text-align:center;"> node </th>
   <th style="text-align:center;"> gen </th>
   <th style="text-align:center;"> param_set </th>
   <th style="text-align:center;"> nodal_type </th>
   <th style="text-align:center;"> given </th>
   <th style="text-align:center;"> param_value </th>
   <th style="text-align:center;"> priors </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> X.0 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> X.1 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y.00_X.0 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> Y.X.0 </td>
   <td style="text-align:center;"> 00 </td>
   <td style="text-align:center;"> X.0 </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y.10_X.0 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> Y.X.0 </td>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;"> X.0 </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y.01_X.0 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> Y.X.0 </td>
   <td style="text-align:center;"> 01 </td>
   <td style="text-align:center;"> X.0 </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y.11_X.0 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> Y.X.0 </td>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> X.0 </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y.00_X.1 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> Y.X.1 </td>
   <td style="text-align:center;"> 00 </td>
   <td style="text-align:center;"> X.1 </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y.10_X.1 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> Y.X.1 </td>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;"> X.1 </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y.01_X.1 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> Y.X.1 </td>
   <td style="text-align:center;"> 01 </td>
   <td style="text-align:center;"> X.1 </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y.11_X.1 </td>
   <td style="text-align:center;"> Y </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> Y.X.1 </td>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> X.1 </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
</tbody>
</table>

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

<table class=" lightable-classic-2" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> X0.Y00 </th>
   <th style="text-align:center;"> X1.Y00 </th>
   <th style="text-align:center;"> X0.Y10 </th>
   <th style="text-align:center;"> X1.Y10 </th>
   <th style="text-align:center;"> X0.Y01 </th>
   <th style="text-align:center;"> X1.Y01 </th>
   <th style="text-align:center;"> X0.Y11 </th>
   <th style="text-align:center;"> X1.Y11 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> X.0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> X.1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Y.00_X.0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Y.10_X.0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Y.01_X.0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Y.11_X.0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Y.00_X.1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Y.10_X.1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Y.01_X.1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Y.11_X.1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
</tbody>
</table>

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

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Model </th>
   <th style="text-align:center;"> dof </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> X -&gt; Y &lt;- W </td>
   <td style="text-align:center;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> X -&gt; Y &lt;- W; X &lt;-&gt; W </td>
   <td style="text-align:center;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> X -&gt; Y &lt;- W; X &lt;-&gt; Y; W &lt;-&gt; Y </td>
   <td style="text-align:center;"> 62 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> X -&gt; Y &lt;- W; X &lt;-&gt; Y; W &lt;-&gt; Y; X &lt;-&gt;W </td>
   <td style="text-align:center;"> 63 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> X -&gt; W -&gt; Y &lt;- X </td>
   <td style="text-align:center;"> 19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> X -&gt; W -&gt; Y &lt;- X; W &lt;-&gt; Y </td>
   <td style="text-align:center;"> 64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> X -&gt; W -&gt; Y &lt;- X; X &lt;-&gt; W; W &lt;-&gt; Y </td>
   <td style="text-align:center;"> 67 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> X -&gt; W -&gt; Y &lt;- X; X &lt;-&gt; W; W &lt;-&gt; Y; X &lt;-&gt; Y </td>
   <td style="text-align:center;"> 127 </td>
  </tr>
</tbody>
</table>

```r
# get-priors

make_model("X -> Y") |> get_priors()
```

```
##  X.0  X.1 Y.00 Y.10 Y.01 Y.11 
##    1    1    1    1    1    1
```

```r
# set-priors-custom

make_model("X -> Y") |>
  set_priors(1:6) |>
  get_priors()
```

```
##  X.0  X.1 Y.00 Y.10 Y.01 Y.11 
##    1    2    3    4    5    6
```

```r
# label: set-priors-statement

make_model("X -> Y") |>
  set_priors(statement = "Y[X=1] > Y[X=0]", alphas = 3) |>
  get_priors()
```

```
##  X.0  X.1 Y.00 Y.10 Y.01 Y.11 
##    1    1    1    1    3    1
```

```r
# label: get-parameters


make_model("X -> Y") |>
  get_parameters()
```

```
##  X.0  X.1 Y.00 Y.10 Y.01 Y.11 
## 0.50 0.50 0.25 0.25 0.25 0.25
```

```r
# label: set-parameters

make_model("X -> Y") |>
  set_parameters(statement = "Y[X=1] > Y[X=0]", parameters = .5) |>
  get_parameters()
```

```
##       X.0       X.1      Y.00      Y.10      Y.01      Y.11 
## 0.5000000 0.5000000 0.1666667 0.1666667 0.5000000 0.1666667
```

```r
## Drawing and manipulating data


model <- make_model("X -> M -> Y")

sample_data_1 <-
  model |>
  make_data(n = 4)


make_data(model, n = 3, param_type = "prior_draw")
```

```
##   X M Y
## 1 0 0 1
## 2 0 1 0
## 3 0 1 1
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

sample_data_2
```

```
##   X  M Y
## 1 0 NA 1
## 2 1  0 0
## 3 1 NA 0
## 4 1 NA 0
## 5 1 NA 0
## 6 1  1 0
## 7 1  1 0
## 8 1 NA 1
```

```r
# Collapse data

sample_data_2 |> collapse_data(model)
```

```
##     event strategy count
## 1  X0M0Y0      XMY     0
## 2  X1M0Y0      XMY     1
## 3  X0M1Y0      XMY     0
## 4  X1M1Y0      XMY     2
## 5  X0M0Y1      XMY     0
## 6  X1M0Y1      XMY     0
## 7  X0M1Y1      XMY     0
## 8  X1M1Y1      XMY     0
## 9    X0Y0       XY     0
## 10   X1Y0       XY     3
## 11   X0Y1       XY     1
## 12   X1Y1       XY     1
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

<table class=" lightable-classic-2" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> X0Y0 </th>
   <th style="text-align:center;"> X1Y0 </th>
   <th style="text-align:center;"> X0Y1 </th>
   <th style="text-align:center;"> X1Y1 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> X.0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> X.1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Y.00 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Y.10 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Y.01 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Y.11 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
</tbody>
</table>

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
   <td style="text-align:center;"> 0.01 </td>
   <td style="text-align:center;"> 0.32 </td>
  </tr>
</tbody>
</table>

```r
## SECTION 7: Querying models
#####################################################################


# realise_outcomes


make_model("X -> Y") |> realise_outcomes()
```

```
##      X Y
## 0.00 0 0
## 1.00 1 0
## 0.10 0 1
## 1.10 1 0
## 0.01 0 0
## 1.01 1 1
## 0.11 0 1
## 1.11 1 1
```

```r
make_model("X -> Y") |> realise_outcomes(dos = list(X = 1))
```

```
##      X Y
## 0.00 1 0
## 1.00 1 0
## 0.10 1 0
## 1.10 1 0
## 0.01 1 1
## 1.01 1 1
## 0.11 1 1
## 1.11 1 1
```

```r
# get_query_types

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
# posterior distributions

data  <- data.frame(X = rep(0:1, 50), Y = rep(0:1, 50))

model <-
  make_model("X -> Y") |>
  update_model(data, iter  = 4000, refresh = 0)


model$posterior_distribution |>
  ggplot(aes(Y.01 - Y.10)) + geom_histogram() + theme_bw()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<div class="figure" style="text-align: center">
<img src="figure/fig-posterior-dist-1.png" alt="Posterior on "Probability $Y$ is increasing in $X$"." width="60%" />
<p class="caption">Posterior on "Probability $Y$ is increasing in $X$".</p>
</div>

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
   <td style="text-align:center;"> X==1 &amp; Y==1 &amp; Z==1 </td>
   <td style="text-align:center;"> 0.95 </td>
   <td style="text-align:center;"> 0.04 </td>
   <td style="text-align:center;"> 0.86 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
</tbody>
</table>

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

<table class=" lightable-classic-2" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:center;"> query </th>
   <th style="text-align:center;"> given </th>
   <th style="text-align:center;"> case_level </th>
   <th style="text-align:center;"> mean </th>
   <th style="text-align:center;"> sd </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> Y[X=1] &gt; Y[X=0] </td>
   <td style="text-align:center;"> X==1 &amp; Y==1 &amp; M==1 </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.67 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Y[X=1] &gt; Y[X=0] </td>
   <td style="text-align:center;"> X==1 &amp; Y==1 &amp; M==1 </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.43 </td>
   <td style="text-align:center;"> 0.33 </td>
  </tr>
</tbody>
</table>

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
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> -0.01 </td>
   <td style="text-align:center;"> 0.31 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.33 </td>
   <td style="text-align:center;"> 0.24 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.76 </td>
   <td style="text-align:center;"> 0.13 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.83 </td>
   <td style="text-align:center;"> 0.11 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.49 </td>
   <td style="text-align:center;"> 0.29 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> 0.29 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.91 </td>
   <td style="text-align:center;"> 0.08 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.91 </td>
   <td style="text-align:center;"> 0.09 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.24 </td>
   <td style="text-align:center;"> 0.19 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.33 </td>
   <td style="text-align:center;"> 0.24 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.80 </td>
   <td style="text-align:center;"> 0.11 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.83 </td>
   <td style="text-align:center;"> 0.11 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.49 </td>
   <td style="text-align:center;"> 0.29 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> 0.29 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.91 </td>
   <td style="text-align:center;"> 0.08 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> FALSE </td>
   <td style="text-align:center;"> 0.91 </td>
   <td style="text-align:center;"> 0.09 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> -0.01 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.33 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.76 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.83 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.48 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.91 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> ATE </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.91 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.24 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.33 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.80 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> - </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.83 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.48 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> priors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.50 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.91 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> POS </td>
   <td style="text-align:center;"> Y==1 &amp; X==1 </td>
   <td style="text-align:center;"> posteriors </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> 0.91 </td>
   <td style="text-align:center;"> NA </td>
  </tr>
</tbody>
</table>

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

```r
benchmark_model <- microbenchmark::microbenchmark(
  m1 = CausalQueries::update_model(model[[1]]),
  m2 = CausalQueries::update_model(model[[2]]),
  m3 = CausalQueries::update_model(model[[3]]),
  times = 5
)

summary(benchmark_model) |> select(expr, mean) |>
  kable(digits = 0)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> expr </th>
   <th style="text-align:right;"> mean </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> m1 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m3 </td>
   <td style="text-align:right;"> 49 </td>
  </tr>
</tbody>
</table>

```r
# effect of data size on run-time

model <- CausalQueries::make_model("X -> Y")

data <- lapply(10 ^ c(1:5), function(n) {
  CausalQueries::make_data(model, n)
})
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

summary(benchmark_data) |> select(expr, mean) |>
  kable(digits = 0)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> expr </th>
   <th style="text-align:right;"> mean </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> d0 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> d1 </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> d2 </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> d3 </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> d4 </td>
   <td style="text-align:right;"> 23 </td>
  </tr>
</tbody>
</table>

