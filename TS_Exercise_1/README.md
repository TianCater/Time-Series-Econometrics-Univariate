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

``` r
##{Spread}

###AR(1)

AR1 <- forecast::Arima(TS_spread,c(1,0,0))
plot(TS_spread)
lines(fitted(AR1), col='blue')
```

![](README_files/figure-gfm/Investigating%20and%20fitting%20different%20models%20for%20each%20process-1.png)<!-- -->

``` r
###summary
summary(AR1)
```

    ## Series: TS_spread 
    ## ARIMA(1,0,0) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1    mean
    ##       0.9045  0.1902
    ## s.e.  0.0284  0.0903
    ## 
    ## sigma^2 = 0.0173:  log likelihood = 129.41
    ## AIC=-252.83   AICc=-252.71   BIC=-242.76
    ## 
    ## Training set error measures:
    ##                        ME      RMSE        MAE  MPE MAPE      MASE      ACF1
    ## Training set 4.089987e-05 0.1308896 0.09606155 -Inf  Inf 0.2851761 0.2634687

``` r
###AR(2)
AR2 <- forecast::Arima(TS_spread,c(2,0,0))
plot(TS_spread)
lines(fitted(AR1), col='blue')
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
###summary
summary(AR2)
```

    ## Series: TS_spread 
    ## ARIMA(2,0,0) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1      ar2    mean
    ##       1.1638  -0.2856  0.1967
    ## s.e.  0.0655   0.0656  0.0692
    ## 
    ## sigma^2 = 0.01594:  log likelihood = 138.47
    ## AIC=-268.93   AICc=-268.74   BIC=-255.51
    ## 
    ## Training set error measures:
    ##                        ME     RMSE        MAE  MPE MAPE      MASE       ACF1
    ## Training set 2.736761e-05 0.125365 0.09296701 -Inf  Inf 0.2759893 0.06865052

``` r
###MA(2)

MA2 <- forecast::Arima(TS_spread,c(0,0,2))
plot(TS_spread)
lines(fitted(MA2), col='blue')
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
###summary
summary(MA2)
```

    ## Series: TS_spread 
    ## ARIMA(0,0,2) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ma1     ma2    mean
    ##       1.2316  0.5412  0.2019
    ## s.e.  0.0583  0.0431  0.0285
    ## 
    ## sigma^2 = 0.02285:  log likelihood = 100.4
    ## AIC=-192.79   AICc=-192.6   BIC=-179.37
    ## 
    ## Training set error measures:
    ##                         ME      RMSE       MAE  MPE MAPE      MASE      ACF1
    ## Training set -0.0001092165 0.1500871 0.1121802 -Inf  Inf 0.3330273 0.2487238

``` r
###ARMA(1,1)

ARMA11 <- forecast::Arima(TS_spread,c(1,0,1))
plot(TS_spread)
lines(fitted(ARMA11),col='blue')
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
###summary
summary(ARMA11)
```

    ## Series: TS_spread 
    ## ARIMA(1,0,1) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1     ma1    mean
    ##       0.8371  0.4199  0.1956
    ## s.e.  0.0397  0.0679  0.0718
    ## 
    ## sigma^2 = 0.01532:  log likelihood = 142.6
    ## AIC=-277.2   AICc=-277   BIC=-263.77
    ## 
    ## Training set error measures:
    ##                        ME      RMSE        MAE  MPE MAPE      MASE        ACF1
    ## Training set 3.823751e-05 0.1229113 0.09130108 -Inf  Inf 0.2710437 -0.01183017

``` r
###ARMA(2,1)

ARMA21 <- forecast::Arima(TS_spread,c(2,0,1))
plot(TS_spread)
lines(fitted(ARMA21),col='blue')
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
###summary
summary(ARMA21)
```

    ## Series: TS_spread 
    ## ARIMA(2,0,1) with non-zero mean 
    ## 
    ## Coefficients:
    ##         ar1     ar2     ma1    mean
    ##       0.754  0.0816  0.4866  0.1952
    ## s.e.  0.143  0.1358  0.1222  0.0743
    ## 
    ## sigma^2 = 0.01537:  log likelihood = 142.78
    ## AIC=-275.56   AICc=-275.27   BIC=-258.78
    ## 
    ## Training set error measures:
    ##                       ME      RMSE        MAE  MPE MAPE      MASE        ACF1
    ## Training set 7.86675e-06 0.1228064 0.09106651 -Inf  Inf 0.2703474 0.001325205

``` r
##{y}


###AR(1)
AR1_y <- forecast::Arima(TS_y,c(1,0,0))
plot(TS_y)
lines(fitted(AR1_y), col='blue')
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
###summary
summary(AR1_y)
```

    ## Series: TS_y 
    ## ARIMA(1,0,0) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1    mean
    ##       0.4941  0.5072
    ## s.e.  0.0599  0.0523
    ## 
    ## sigma^2 = 0.151:  log likelihood = -99.55
    ## AIC=205.09   AICc=205.21   BIC=215.16
    ## 
    ## Training set error measures:
    ##                        ME     RMSE       MAE      MPE     MAPE      MASE
    ## Training set 0.0006738706 0.386726 0.3163778 233.4678 349.1124 0.6370634
    ##                    ACF1
    ## Training set 0.02103549

``` r
###AR(2)
AR2_y <- forecast::Arima(TS_y,c(2,0,0))
plot(TS_y)
lines(fitted(AR2_y), col='blue')
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
###summary
summary(AR2_y)
```

    ## Series: TS_y 
    ## ARIMA(2,0,0) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1      ar2    mean
    ##       0.5154  -0.0416  0.5072
    ## s.e.  0.0698   0.0702  0.0502
    ## 
    ## sigma^2 = 0.1514:  log likelihood = -99.37
    ## AIC=206.74   AICc=206.94   BIC=220.17
    ## 
    ## Training set error measures:
    ##                        ME      RMSE       MAE      MPE     MAPE      MASE
    ## Training set 0.0006885428 0.3864018 0.3160035 235.0747 349.9293 0.6363098
    ##                     ACF1
    ## Training set 0.001986255

``` r
###MA(2)

MA2_y <- forecast::Arima(TS_y,c(0,0,2))
plot(TS_y)
lines(fitted(MA2_y), col='blue')
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
###summary
summary(MA2_y)
```

    ## Series: TS_y 
    ## ARIMA(0,0,2) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ma1     ma2    mean
    ##       0.5059  0.1895  0.5067
    ## s.e.  0.0667  0.0737  0.0450
    ## 
    ## sigma^2 = 0.1524:  log likelihood = -100.06
    ## AIC=208.12   AICc=208.32   BIC=221.55
    ## 
    ## Training set error measures:
    ##                        ME      RMSE       MAE      MPE     MAPE      MASE
    ## Training set 0.0006902326 0.3876738 0.3172841 238.9269 356.2071 0.6388884
    ##                    ACF1
    ## Training set 0.01438341

``` r
###ARMA(1,1)

ARMA11_y <- forecast::Arima(TS_y,c(1,0,1))
plot(TS_y)
lines(fitted(ARMA11_y),col='blue')
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
###summary
summary(ARMA11_y)
```

    ## Series: TS_y 
    ## ARIMA(1,0,1) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1     ma1    mean
    ##       0.4273  0.0894  0.5071
    ## s.e.  0.1284  0.1444  0.0503
    ## 
    ## sigma^2 = 0.1514:  log likelihood = -99.36
    ## AIC=206.72   AICc=206.91   BIC=220.14
    ## 
    ## Training set error measures:
    ##                        ME     RMSE       MAE      MPE     MAPE      MASE
    ## Training set 0.0007429472 0.386379 0.3159959 235.0554 349.7796 0.6362945
    ##                     ACF1
    ## Training set 0.000744146

``` r
###ARMA(2,1)

ARMA21_y <- forecast::Arima(TS_y,c(2,0,1))
plot(TS_y)
lines(fitted(ARMA21_y),col='blue')
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
###summary
summary(ARMA21_y)
```

    ## Series: TS_y 
    ## ARIMA(2,0,1) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1     ar2     ma1    mean
    ##       0.2319  0.0977  0.2848  0.5071
    ## s.e.  2.3134  1.1611  2.3074  0.0506
    ## 
    ## sigma^2 = 0.1521:  log likelihood = -99.35
    ## AIC=208.69   AICc=208.98   BIC=225.47
    ## 
    ## Training set error measures:
    ##                        ME      RMSE       MAE      MPE     MAPE      MASE
    ## Training set 0.0007307277 0.3863551 0.3159889 234.7491 349.2785 0.6362802
    ##                      ACF1
    ## Training set 0.0007207667

For the Spread process, the ARMA(1,1) model has the lowest AIC and BIC
values (-277.2 and -263.77, respectively)

Fir the y process, the AR(1) model has the lowest AIC and BIC values
(205.09 and 215.16, respectively)

We select these models and evaluate whether they are adequate, i.e is
congruent and parsimonious. If the models are adequate, we present the
estimated results of the best model and all necessary specification
tests.

``` r
pacman::p_load(lmtest)

ARMA11 <- forecast::Arima(TS_spread,c(1,0,1))

ARMA11
```

    ## Series: TS_spread 
    ## ARIMA(1,0,1) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1     ma1    mean
    ##       0.8371  0.4199  0.1956
    ## s.e.  0.0397  0.0679  0.0718
    ## 
    ## sigma^2 = 0.01532:  log likelihood = 142.6
    ## AIC=-277.2   AICc=-277   BIC=-263.77

``` r
coeftest(ARMA11)
```

    ## 
    ## z test of coefficients:
    ## 
    ##           Estimate Std. Error z value  Pr(>|z|)    
    ## ar1       0.837088   0.039725 21.0719 < 2.2e-16 ***
    ## ma1       0.419908   0.067912  6.1831 6.285e-10 ***
    ## intercept 0.195605   0.071788  2.7248  0.006435 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
Res1 <- forecast::checkresiduals(ARMA11)
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(1,0,1) with non-zero mean
    ## Q* = 23.483, df = 21, p-value = 0.3188
    ## 
    ## Model df: 3.   Total lags used: 24

``` r
pacman::p_load(tseries)

jarque.bera.test(residuals(ARMA11))
```

    ## 
    ##  Jarque Bera Test
    ## 
    ## data:  residuals(ARMA11)
    ## X-squared = 11.055, df = 2, p-value = 0.003976

``` r
AR1_y <- forecast::Arima(TS_y,c(1,0,0))

AR1_y
```

    ## Series: TS_y 
    ## ARIMA(1,0,0) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1    mean
    ##       0.4941  0.5072
    ## s.e.  0.0599  0.0523
    ## 
    ## sigma^2 = 0.151:  log likelihood = -99.55
    ## AIC=205.09   AICc=205.21   BIC=215.16

``` r
coeftest(AR1_y)
```

    ## 
    ## z test of coefficients:
    ## 
    ##           Estimate Std. Error z value  Pr(>|z|)    
    ## ar1       0.494074   0.059856  8.2544 < 2.2e-16 ***
    ## intercept 0.507176   0.052261  9.7047 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
Res2 <- forecast::checkresiduals(AR1_y)
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(1,0,0) with non-zero mean
    ## Q* = 23.821, df = 22, p-value = 0.3567
    ## 
    ## Model df: 2.   Total lags used: 24

``` r
pacman::p_load(tseries)

jarque.bera.test(residuals(AR1_y))
```

    ## 
    ##  Jarque Bera Test
    ## 
    ## data:  residuals(AR1_y)
    ## X-squared = 1.8101, df = 2, p-value = 0.4045
