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
