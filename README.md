# Foreign Exchange Rate: Time Series Analysis.

### What is Exchange Rate? 

In *Finance*, exchange rate is the rate at which one currency is exchanged with another, which is equivalently saying that the value of one currency in comparison with other currency. Exchange rate is an important variable as it's value influences the decision of the traders, investors, importers, exporters, bankers, financiers, policy makers of a nation. 

*Forecasting the Exchange rate* is very important both at a individual level for the practitioners and Researchers and at ministry level for policy making. It is the avenue to understand the economic conditions of a country and taking measures to improve different factors to produce the maximum financial returns and profit. For all certain reasons, forecasting the *exchange rates* is also quiet a challenging task across the globe. 

In this report I shall be utilizing the time series concepts to do an analysis and predict the daily exchange rates of the *Indian Rupee (INR) against the United States Dollar (USD)*. I shall do the analysis comparatively on recent data i.e. *Daily exchange rates from January 2010 to December 2019*.


__A little history__ of *Foreign Exchange Rate*:  
The foreign exchange market in India is believed to have begun in 1978 when the government allowed banks
to trade foreign exchange with each other. Today, it is almost unnecessary to reiterate the observation that globalization and liberalization have significantly enhanced the scope for the foreign exchange market in India. The
Indian exchange rate is regime, as noted by [Goyal (2018)](https://www.researchgate.net/publication/327986460_Evaluating_India's_exchange_rate_regime_under_global_shocks) is a managed float, where the central bank allows markets to discover the equilibrium level but only intervenes to prevent excessive volatility.

**Research Objectives:** To Find out the perfect model for the Foreign Exchange Rate. I shall do so using the conventional method followed by a ACF, PACF analysis. 

## Information on dataset.

**Frequency :**	Monthly  
**Unit :**	INR/USD  
**Source :**	Organisation for Economic Co-operation and Development @CEIC.  
**Series ID : **279672802  
**SR Code :** SR5084711.  
**First Obs. Date :**	01-1957  
**Last Obs. Date :**	05-2020 

\pagebreak

# Step 1 : Loading the Data, Plotting the data, and Conclusion

```{r}
FER.data=readxl::read_xlsx("INForeignExchangeRateMonthlyAverageINR_USD.xlsx")
head(FER.data)
```

```{r}
tail(FER.data)
```

```{r}
# Monthly Seasonality (frequency set to 12 for monthly data)
FER <- ts(FER.data$FER_INR_USD, start = c(1957, 01),
          end = c(2019,12),frequency=12)
FER.window <- window(FER, start= c(2010))
FER.window
```

```{r}
plot(FER.window, main = "Foreign Exchange Rate from 2010 - 2019", ylab = "Amount in INR for 1 USD")
```

**Conclusion** : The graph clearly shows a *upward increasing trend* but it does not show any systematic seasonal peaks. Still to check if there is seasonality we shall apply a *seasonal Dummy LM test. (Method 1)*

\pagebreak

# Step 2 : Checking for seasonality

**a) Here we shall fit a dummy to find out if there is seasonality**  
**b) If seasonality exists we will use Ratio to MA to remove the seasonality**  
**c) then we shall check for non stochastic trend, and remove it finally.**  

_a) Checking the existence of seasonality using dummy indices_

```{r message= F}
# all libraries required, some dependencies are there.
library(caret)
library(lattice)
library(ggplot2)
library(mltools)
library(data.table)
```


```{r , message=FALSE, warning=FALSE}
library(zoo)
# Saving the months in  a vector
p <- month.abb[cycle(FER.window)]
# Changing the months into factors
p <- as.factor(p)
# fitting dummy using mltools package
# the dummy variable is a list
dmy <- one_hot(as.data.table(p))
# taking trend
t <- c(1: length(FER.window))
# Setting the dummies and creating a dataframe
m <- as.matrix(FER.window)
X <- cbind(as.data.frame(m),t, as.data.frame(dmy))
names(X)[1] <- "Y"
head(X, n= 12)
```

_Here we shall check if the coefficients of the seasonal dummies are significant. If any one of the coefficient is significant then we say that seasonality exists._  

**Note**: To remove multicolinearity we omit one of the dummies, thankfully R automatically does the trick for us.   

Here $$y_t = \beta_0+ \beta_1t+D_{1t}+\beta_2D_{2t}+.....+\beta_{12}D_{12t}$$ where $D_{i}$= Monthly dummies.  
Here for each coefficient $\beta_{i}$ the null and alternate hypothesis are as follows:  
$H_0{i}$ : $\beta_{i} = 0$  
$H_a{i}$ : $\beta_{i} \neq 0$  


```{r}
summary(lm(Y~.,data = X))
```

The lm summary shows that none of the *seasonal coefficients are significant*, so we do not have enough evidence to reject the null hypothesis. So we can consider that all the monthly coefficients are 0 showing that it has *got no seasonality.*

*2.c : Here we shall apply the* **Augmented Dickey Fuller(ADF)** test to check for any *Stochastic trend.* 

The equation is : $$\Delta y_t = \alpha+ \beta t+ \gamma y_{t-1} + \delta_1\Delta y_{t-1}+...$$    
Null Hypothesis: $H_0$ : $\gamma = 0$  
Alternate Hypothesis : $H_a$ : $\gamma < 0$  
Reject Null hypothesis at 5% level of significance if $test \ statistic \ value < tau 1$  value at *5%* significance level i.e. if the test statistic lies on the left side of the critical value at 5% level of significance.  

**Note:** Here the function will automatically choose the lag which takes the *lowest AIC*, and calculates accordingly.  

```{r}
library(urca)
# considering there is still a trend in the data, we take type as "trend"
# we provide a maximum lag and the function automatically the optimum lag according to AIC criterion
summary(ur.df(FER.window, type = "trend", lags = 15, selectlags = "AIC"))
```

This shows that the test statistic is $-2.996 > -3.43$, so the test statistic lies on the right side of the critical value, so we can say that our data has got a unit root, i.e. there is a stochastic trend which needs to be removed first.  

For that we take the first lag difference.

```{r}
FER.window.diff1 <- diff(FER.window)
```

Now again we shall check if there is still a stochastic trend. We will confirm the same using the *Augmented Dickey Fuller test*

```{r}
library(urca)
summary(ur.df(FER.window.diff1, type = "trend", lags = 15, selectlags = "AIC"))
```

This shows that the *test statistic* is $-5.9795 < -3.43$, Since it lies in the left side of the critical value, so we can *reject the null hypothesis*. Thus, we can say that our data has no unit root, i.e. stochastic trend is not there any more.  

Now we fit a linear trend to the data to check if there is any deterministic trend.  

```{r}
model_select <- function(y, x ,poly.deg)
{
  model <- lm(y~poly(x,poly.deg))
  return(summary(model))
    
}
t=seq.int(1,length(FER.window.diff1),1)
y <- FER.window.diff1
```

Assuming a linear trend we fit the following model: $$ y_t = a+ bt + u_t, \ \ where \ u_t \sim N(0,1)$$    
Now to determine if a linear trend is present, we test the following hypothesis.   
$H_0$ : $b=0$  
$H_a$ : $b \neq 0$  

```{r}
model_select(y,t,1)
```

Well this shows that *we fail to reject the null hypothesis* that is the coefficient $b = 0$, so here apparently there does not exist a deterministic trend as such in the data. 

We plot the difference data once more to see how it looks now. 

```{r}
plot(FER.window.diff1, main = "Foreign Exchange Rate from 2010 - 2019", 
     ylab = "Amount in INR for 1 USD")
```
Now that the process is stationary from a **Trend Stationary process**, we proceed forward to building the model. 

\pagebreak

# Step 3 : Sample Dividing Procedure. 

Now we shall divide the data into *In sample* and *Out Sample*, we shall keep 9.5 years for the training (in sample) and rest 6 months for forecasting (out sample). 

**In sample: Jan 2010 - Jun 2019**

```{r}
FER.train <-  window(FER.window, end = c(2019,06))
FER.train
```
**Out sample: July 2019- Dec 2019**

```{r}
FER.test <- window(FER.window, start= c(2019,07), end= c(2019,12))
FER.test
```

```{r}
FER.train.d1 <- window(FER.window.diff1, end = c(2019,06))
```

\pagebreak

# Step 4 : The ACF analysis. 

Here we shall make a table calculating all the acf, its test statistic and draw conclusion accordingly. We will be using a maximum lag of 19 for all the below calculations.

```{r}
max_lag <- 19
max_lag
```

```{r}
acf(FER.train.d1, lag.max = max_lag)
```

```{r}
rho <- acf(FER.train.d1, lag.max = max_lag, plot = F)
length(rho$acf)
```

**Ljung-Box test: **

**Null Hypothesis:** $H_0$: $\rho_1= ... =\rho_k=0$, at a given lag k.  
**Alternative Hypo: ** $H_1$: $\rho _i\neq 0 \ \forall \ i = 1,2,...k$  

**Test Statistic**: $$Q(k)= n(n+2)(\sum_{j=1}^k\frac{\hat e_j^2}{n-j}) \sim \chi^2_k$$  

*Note :* In statistical hypothesis testing, the __p-value__ or probability value is the probability of obtaining test results at least as extreme as the results actually observed, assuming that the null hypothesis is correct. So here, the P-value of the test is the probability that a __chi-square test statistic__ having the respective degrees of freedom is more extreme than the obtained test statistic.  

**Interpret results:** If the P-value is less than the significance level (0.05), we reject the null hypothesis.  

```{r}
k <- max_lag+1
Qt <- t(sapply(1:k,function(i) Box.test(FER.train.d1, lag = i, type = "Ljung-Box")))
Qt <- as.data.frame(Qt)
result <- NULL
for(i in 1:k){
  if(Qt$p.value[[i]] < 0.05){
  result[i] <- "reject"
  
} else {result[i] <- "accept"} 
}
acf_table <- data.frame(K= 1:k, 
                        SAMPLE_ACF = rho$acf, 
                        P_VALUES=as.numeric(Qt$p.value), 
                        TEST_STATISTIC = as.numeric(Qt$statistic), 
                        RESULT = as.vector(result))
acf_table
```

We see that the acfs are partly diminishing and partly significant but cuts off entirely after lag 11. So we can try and fit a *MA(11)* model. After looking at the PACF and deciding the same.

\pagebreak

# Step 5: Analysing PACF

```{r}
pacf(FER.train.d1, lag.max = max_lag)
```
**PACF**

**Null & Alternate Hypothesis:**  

$H_0$: $\phi_1= \phi_2=... =\phi_k=0$, at a given lag k.  
$H_1$: $\phi_i\neq 0 \ \forall \ i = 1,2,...k$  

**Test Statistic**: $$Q(k)= \sqrt n \hat \phi_k \sim N(0,1)$$   

**Interpret results:** Tne null Hypothesis follows a standard normal distribution, so at *5%* level of significant test statistic value is *1.96* $\implies$ if $|Q(k)| < 1.96$  we reject the null hypothesis else we shall accept.


```{r}
n <- length(FER.train)
phi <- pacf(FER.train.d1, lag.max = max_lag, plot = F)
Qt <- as.numeric(phi$acf*sqrt(n))
Qt <- abs(Qt)
result <- NULL
for(i in 1:max_lag){
  if(abs(Qt[i]) > 1.96){
  result[i] <- "reject"
  } else {result[i] <- "accept"} 
}
pacf_table <- data.frame(K= 1:max_lag, 
                         SAMPLE_PACF = phi$acf,  
                         TEST_STATISTIC = Qt, 
                         RESULT = as.vector(result))
pacf_table
```

This shows that the pacf cuts off entirely after lag 1. Suggesting a *AR(1)* model. Since both the models seems feasible, so we shall select the model depending on the min __AIC__ and __BIC__. 

\pagebreak

# Step 6 : Model fitting and Residual Analysis. 


**Model AR(1)** | **Model MA(11)**

```{r}
FER.model1 <- arima(FER.train, order = c(1,1,0))
FER.model2 <- arima(FER.train, order = c(0,1,11))
```

**AIC** and **BIC** of **AR(1) and MA(11)**

```{r}
bic1 <- BIC(FER.model1)
bic2 <- BIC(FER.model2)
```

```{r}
cat("AR(1) BIC : ", bic1, "\n", "MA(11) BIC : ", bic2, sep = "")
cat("\n","AR(1) AIC : ", FER.model1$aic, "\n", "MA(11) AIC : ", FER.model2$aic, sep = "")
```

As both AIC and BIC of model AR(1) is less than MA(11), so **we select AR(1) model as the final model**.  

```{r}
FER.model1
```

\pagebreak

Now we shall test the residuals to justify our model specification. 

```{r}
AR_1_resid <- na.omit(FER.model1$residuals)
acf(AR_1_resid, lag.max = max_lag, plot = T)
```

Again on the **residuals** we apply **Ljung Box Test**.

**Remember: Ljung-Box test: **

**Null Hypothesis:** $H_0$: $\rho_1= ... =\rho_k=0$, at a given lag k.  
**Alternative Hypo: ** $H_1$: $\rho _i\neq 0 \ \forall \ i = 1,2,...k$  

**Test Statistic**: $$Q(k)= n(n+2)(\sum_{j=1}^k\frac{\hat e_j^2}{n-j}) \sim \chi^2_{k-p-q}$$  
where p, q are the parameters of the model.  

*Note :* In statistical hypothesis testing, the __p-value__ or probability value is the probability of obtaining test results at least as extreme as the results actually observed, assuming that the null hypothesis is correct. So here, the P-value of the test is the probability that a __chi-square test statistic__ having the respective degrees of freedom is more extreme than the obtained test statistic. Also note that the degrees of freedom here is k-p-q, so the first two values comes out to be null. In the given code we have removed the first two values.   

**Interpret results:** If the P-value is less than the significance level (0.05), we reject the null hypothesis.  

```{r warning = F}
parameter <- sum(eval((FER.model1$call$order)))
resid_acf <- acf(AR_1_resid, lag.max = max_lag, plot = F)
Rt <- t(sapply(1:k,function(i) Box.test(AR_1_resid, lag = i, type = "Ljung-Box", fitdf = parameter)))
Rt <- as.data.frame(Rt)
result <- NULL
for(i in (parameter+1):k)
  {
  if(Rt$p.value[[i]] < 0.05){
  result[i] <- "reject"
  
} else {result[i] <- "accept"} 
}
residual_table <- cbind(K= 1:(k-parameter),na.omit(data.frame( 
                            RESIDUAL_ACF = resid_acf$acf, 
                             P_VALUES=as.numeric(Rt$p.value), 
                             TEST_STATISTIC = as.numeric(Rt$statistic), 
                             RESULT = as.vector(result))) 
                            )
residual_table
```

This shows that our model fit is *Good enough* as we fail to reject the null hypothesis showing that the residue is plain white noise present. This means that our model fitting is Alright.  

**Therefore, the correctly specified model is a AR(1) model given by:**  
$$y_t = 0.2928 + 0.0895 y_{t-1} + e_t  \ \ where \ \ e_t \sim WN$$  



\pagebreak

# Step 7 : Forecasting on the Out Sample.

Now we shall forecast 6 months using our correctly specified model and find the **RMSE** and **MAPE**. 

```{r}
library(astsa)
FER.pred <- sarima.for(FER.train, n.ahead = 6 , p = 1, d = 1, q= 0, P=0, D= 0, Q= 0, 12)
```

The plot shows the **training(in sample data)** in **black** and the **predicted(out sample data)** in **red**.


\pagebreak

Here is a table which shows the **actual** and **predicted values**.  
We see that the values are close enough.


```{r}
PRED_ACTtable <- data.frame(Predict = FER.pred$pred, Actual = FER.test)
PRED_ACTtable
```


```{r}
rmse_DL <- sqrt(mean((PRED_ACTtable$Predict- PRED_ACTtable$Actual)^2))
mape_DL <- mean(abs(PRED_ACTtable$Predict- PRED_ACTtable$Actual)/abs(PRED_ACTtable$Actual))*100
```

We display the **RMSE** and **MAPE** values of the forecast are as follows:  

```{r}
cat("RMSE : ", rmse_DL,"\n", "MAPE : ", mape_DL, sep = "")
```
This is the entire analysis of the *Foreign Exchange Rate.*

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# FER_Analysis
 Foreign Excahnge Rate in India, INR/USD time series modelling.
This repository contains a Pdf file, R notebook and a the data. 
The main idea is to model the Foreign Exchange Rate, and then test how good is the modelling. 
Its found that depending on the *AIC* and *BIC* of two seperate ARIMA models, ARIMA(0,1,11)
and ARIMA(1,1,0), the ARIMA(1,1,0) works better. 
And hence the prediction is done on ARIMA(1,1,0), which shows a nearly good prediction. 

Findings: 
**MODEL** : *ARIMA(1,1,0)*

Prediction **RMSE : 1.127**
Prediction **MAPE: 1.153**
