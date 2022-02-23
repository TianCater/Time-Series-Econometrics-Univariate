README
================

``` r
library(readxl)
DF1 <- read_excel("data/TSexercise1data.xlsx")

#load required packages

pacman::p_load(dplyr,stats,fixest, tidyverse, huxtable, hrbrthemes, modelsummary, glue,forecast)

#Isolate columns as numeric vectors/variables

Spread <- DF1 %>% dplyr::select(spread)
y <- DF1%>% dplyr::select(y)



#VITAL, mutate dates to :Type Date

DF1 <- DF1 %>%
   group_by(dates) %>%
   mutate(dates=as.Date(dates, format = "%Y.%m.%d"))

Date <- DF1 %>% dplyr::select(dates)
```

``` r
TS_spread <- ts(Spread,start=c(1984,1,1),end = c(2001,8,1),frequency = 12)

TS_y <-  ts(y,start=c(1984,1,1),end = c(2001,8,1),frequency = 12)
```

``` r
##Plot,ACF and PACF for {Spread}

forecast::tsdisplay(TS_spread)
```

![](README_files/figure-gfm/Plot%20ACF%20and%20PACF-1.png)<!-- -->

``` r
##Plot,ACF and PACF for {y}

forecast::tsdisplay(TS_y)
```

![](README_files/figure-gfm/Plot%20ACF%20and%20PACF-2.png)<!-- -->
