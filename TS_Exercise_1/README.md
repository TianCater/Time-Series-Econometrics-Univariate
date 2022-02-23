README
================

``` r
library(readxl)
DF1 <- read_excel("data/TSexercise1data.xlsx")

#load required packages

pacman::p_load(dplyr,stats,fixest, tidyverse, huxtable, hrbrthemes, modelsummary, glue)

#Isolate columns as numeric vectors/variables

Spread <- DF1 %>% dplyr::select(spread)
y <- DF1%>% dplyr::select(y)

#VITAL, mutate dates to :Type Date

DF1 <- DF1 %>%
   group_by(dates) %>%
   mutate(dates=as.Date(dates, format = "%Y.%m.%d"))
```

``` r
##{Spread} time path

DF1 %>% ggplot() + # creates the 'canvas'
theme_bw() + # choose on of many existing themes
geom_line(aes(x = dates, y = spread), size = 1, alpha = 0.9, color = "darkgreen") +
# creates the line on the canvas with aes() coordinates
geom_point(aes(x = dates, y = spread), size = 2, alpha = 0.9, color = "darkgreen") +
# similarly for points
scale_x_date(date_labels = "'%y", date_breaks = "year") +
# make x axis labels
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
# rotate x axis labels
labs(title = "Spread: 1 year and a 3 month US Treasury bill", y = "Spread", x = "Date")
```

![](README_files/figure-gfm/series%20plots-1.png)<!-- -->

``` r
##{y} time path

DF1 %>% ggplot() + # creates the 'canvas'
theme_bw() + # choose on of many existing themes
geom_line(aes(x = dates, y = y), size = 1, alpha = 0.9, color = "darkgreen") +
# creates the line on the canvas with aes() coordinates
geom_point(aes(x = dates, y = y), size = 2, alpha = 0.9, color = "darkgreen") +
# similarly for points
scale_x_date(date_labels = "'%y", date_breaks = "year") +
# make x axis labels
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
# rotate x axis labels
labs(title = "A simulated stationary ARMA(p,q) process (y)", y = "y", x = "Date")
```

![](README_files/figure-gfm/series%20plots-2.png)<!-- -->

``` r
 ##For the Spread series

pacman::p_load(stats)

# ACF(Spread)
 
Spread_acf <- stats::acf(
x = Spread,
plot = T, # automatically create a plot
type = "correlation") # we want the standard AF
```

![](README_files/figure-gfm/ACFs%20and%20PACFs-1.png)<!-- -->

``` r
#PACF(Spread)

Spread_pacf <- stats::acf(
x = Spread,
plot = T, 
type = "partial") 
```

![](README_files/figure-gfm/ACFs%20and%20PACFs-2.png)<!-- -->

``` r
##For the y series

#ACF(y)

y_acf <- stats::acf(
x = y,
plot = T, # automatically create a plot
type = "correlation") # we want the standard AF
```

![](README_files/figure-gfm/ACFs%20and%20PACFs-3.png)<!-- -->

``` r
#PACF(y)

y_pacf <- stats::acf(
x = y,
plot = T, 
type = "partial")
```

![](README_files/figure-gfm/ACFs%20and%20PACFs-4.png)<!-- -->
