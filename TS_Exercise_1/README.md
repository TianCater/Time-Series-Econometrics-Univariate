README
================

``` r
library(readxl)
DF1 <- read_excel("data/TSexercise1data.xlsx")
```

``` r
pacman::p_load(dplyr,stats,fixest, tidyverse, huxtable, hrbrthemes, modelsummary, glue)
```

``` r
Spread <- DF1 %>% dplyr::select(dates)
y <- DF1%>% dplyr::select(y)
```

``` r
 ##For the Spread series

# ACF(Spread)
 
Spread_acf <- stats::acf(
x = Spread,
plot = T, # automatically create a plot
type = "correlation") # we want the standard AF
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#PACF(Spread)

Spread_pacf <- stats::acf(
x = Spread,
plot = T, 
type = "partial") 
```

![](README_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
##For the y series

# ACF(y)

y_acf <- stats::acf(
x = y,
plot = T, # automatically create a plot
type = "correlation") # we want the standard AF
```

![](README_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
# PACF(y)

y_pacf <- stats::acf(
x = y,
plot = T, 
type = "partial")
```

![](README_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->
