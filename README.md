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
