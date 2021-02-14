---
title: "<center> Online Retail Data <br> Time Series Predictive Modeling </center>"
author: "<center> Kimberly Healy  |  healy.kim@gmx.us </center>"
---


```{css, echo=FALSE}
pre {
  max-height: 300px;
  overflow-y: auto;
}

pre[class] {
  max-height: 100px;
}
```


## Background
The data ([OnlineRetail.csv](https://archive.ics.uci.edu/ml/datasets/Online+Retail)) come from all transactions occurring between 01/12/2010 and 09/12/2011 for a UK-based online retail store. The company mainly sells unique gifts and the majority of customers are wholesalers. This report will use techniques to explore the data and use three different time series algorithms to model sales. The conclusion states which model(s) best predict sales and recommendations for further analysis.     
       
The following predictive models are used:        
**Model 1: Simple Moving Average**: a weighted average function that identifies the previous pattern in the data and uses that pattern to predict future data points.      
**Model 2: Exponential Smoothing**: similar to Simple Moving Average, but contains a smoothing constant which can be tweaked for more responsive or more stable models.      
**Model 3: Autoregressive Integrated Moving Average (ARIMA)**: unlilke the previous models, ARIMA considers the correlations among the previous values of the time series to predict future values.    
        
         
The following question is answered:     
  - **Which model best predicts company sales?**

***
## I. Load required libraries 
```{r lib, message = FALSE}
library(tidyverse)
library(bpa)
library(forecast)
library(ggplot2)
library(knitr)
library(kableExtra)
library(moments)
```
***
## II. Load raw data from Excel file
``` {r load}
retail_raw <- readxl::read_excel("OnlineRetail.xlsx")
retail <- retail_raw%>%
    mutate(across(where(is.character), as.factor))%>%
    mutate(Sales = (Quantity*UnitPrice))
retail$CustomerID<-as.factor(retail$CustomerID)
```

***
## III. Define Useful Functions

### (i) Moving Average with Prediction
Source: Quanghau (Robin) Qiu, Professor of Informational Science, Penn State Engineering
``` {r lkdsjk3al}

movavg_pred <- function(x, n, type=c("s", "e"), h=0) {
stopifnot(is.numeric(x), is.numeric(n), is.character(type),is.numeric(h))
if (length(n) != 1 || ceiling(n != floor(n)) || n <= 1)
stop("Window length 'n' must be a single integer greater 1.")
nx <- length(x)
if (n >= nx)
stop("Window length 'n' must be greater then length of time series.")
y <- numeric(nx)
if (type == "s") { # simple
for (k in 1:(n-1)) y[k] <- mean(x[1:k])
for (k in n:nx) y[k] <- mean(x[(k-n+1):k])

# adding h forecasting points based on the Moving Average equation
if (h>0) {
for (k in 1:h) {
y[nx+k] <- mean(x[(nx-n-1+k):(nx-1+k)])
x[nx+k] <- y[nx+k]
}
}
} else if (type == "e") { # exponential
a <- 2/(n+1)
y[1] <- x[1]
for (k in 2:nx) y[k] <- a*x[k] + (1-a)*y[k-1]

# adding h forecasting points based on the Exponential Smoothing equation
if (h>0) {
y[nx] <- x[nx]
for (k in 1:h) {
y[nx+k] <- a*x[nx+k-1] + (1-a)*y[nx+k-2]
x[nx+k] <- y[nx+k]
}
}
} else
stop("The type must be one of 's' or 'e'")
return(y)
}
```

The “n” input for the movavg_pred function used for the Simple Moving Average and Exponential Smoothing models indicates how many previous data points (previous months) are considered in the forecast. The larger to n, the more previous data points (months) are considered, and typically the higher accuracy of the forecast.   

***  
## IV. Data Exploration

### (i) Basic Exploration {.tabset}
    
#### Summary
The data set dimensions are 541,909 rows by 8 attributes.   
There are negative numbers for Quantity and UnitPrice. Sales is derived from Quantity and UnitPrice, so it's worth looking into.  
``` {r perform1}
dim(retail)
str(retail)
summary(retail)
```
    
***
    
#### NAs
There are 1,454 instances missing in Description and 136,534 instances missing in CustomerID. Keeping the missing values of these attributes will not have an impact on our predictive sales analysis. No action will be taken.     
``` {r perform2}
print(length(which(is.na(retail))))
print(colSums(is.na(retail)))
```
    
     
***

#### UnitPrice & Quantity
There are negative numbers for Quantity and UnitPrice. Sales is derived from Quantity and UnitPrice, so it’s worth looking into.     
    
- There are 2,515 instances where **UnitPrice is 0**: All of these instances indicate that the company ate the cost of a damaged, lost, or returned item; payment was by check; or an inventory adjustment needed to be made. Descriptions range from "wrongly coded" to "Wet pallet-thrown away" to "reverse 21/5/10 adjustment".    
- There are 2 instances where **UnitPrice is negative**: Both of these are adjustments for bad debt.     
- There are 9,288 instances where **Quantity is negative and UnitPrice is greater than 0**: The majority indicate return of product, and some indicate a transaction fee, discount, or commission. 

    
It is worth investigating the effect of any transaction that is not directly related to sales of product, such as various business expenses, in the data set.    

``` {r perform3}
nrow(retail%>%filter(UnitPrice == 0))
nrow(retail%>%filter(UnitPrice < 0))
nrow(retail%>%filter(Quantity <= 0 & UnitPrice > 0))
```
   
***
#### Biz Expenses

It is worth investigating the effect of any transaction that is not directly related to sales of product, such as various business expenses, in the data set.    
     
To investigate transactions in the data that are not related to sales, I used pattern analysis on the StockCode attribute. The output shows the 16 unique patterns of StockCode and the frequencies of the pattern. I extracted the StockCode values using match_pattern() - two examples are shown below - and found that there are three StockCode types in the data: product sales or returns, business expenses, or ambiguous whether it is a sale or expense.   
   
In a perfect world, sales, returns, and other expenses are separated into three distinct categories. **However, based on the information that I have, there is too much ambiguity surrounding the categorization of transactions.**       
   
**The best course of action will be to separate the analysis into sales that are positive and sales that are negative. Predicting money coming in (positive Sales attribute) and money going out (negative Sales attribute) is more informative than not separating them.**  


``` {r kls3}

retail$StockCode %>%
    as.character()%>%
    get_pattern %>%  
    table

retail$StockCode %>%
    as.character()%>%
    match_pattern(pattern = "AAAAAAAAA", unique_only=TRUE)

retail$StockCode %>%
    as.character()%>%
    match_pattern(pattern = "AAAAwAAAAAAA", unique_only=TRUE)




# divide the data set by positive and negative Sales.   
retail_pos<-retail%>%filter(Sales > 0)
retail_neg<-retail%>%filter(Sales <= 0)
```



***
   
     
### (ii) Sales Exploration  {.tabset}

#### Group by Week
Group positive and negative sales by week.
``` {r perform45}

retail_pos_Weekly <- retail_pos %>% 
  group_by(`Year-Week` = format(InvoiceDate, '%Y-%U'))%>% 
  summarise_if(is.numeric, sum)


retail_pos_Weekly%>%kable(digits = 2, format = "html", row.names = TRUE,caption = "Positive Sales by Week") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = F,
                font_size = 12,
                position = "left")%>%
  column_spec(column = 1:2, bold = TRUE)%>%
  scroll_box(width = "100%", height = "300px")



retail_neg_Weekly <- retail_neg %>% 
  group_by(`Year-Week` = format(InvoiceDate, '%Y-%U'))%>% 
  summarise_if(is.numeric, sum)


retail_neg_Weekly%>%kable(digits = 2, format = "html", row.names = TRUE,caption = "Negative Sales by Week") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = F,
                font_size = 12,
                position = "left")%>%
  column_spec(column = 1:2, bold = TRUE)%>%
  scroll_box(width = "100%", height = "300px")


```
   
***

#### Correlation
The correlation between positive and negative sales is moderately negative (r=-0.573). Meaning, as sales increase the higher chance of product breakage, returns, selling fees, etc., which result in negative sales.     
``` {r perf4orm4}
cor(retail_pos_Weekly$Sales, retail_neg_Weekly$Sales, method="pearson")
```
***

#### Visualizations

``` {r perf4morm4}

# all sales by week
ggplot(bind_rows(retail_pos_Weekly, retail_neg_Weekly), aes(x = Sales)) + 
    geom_histogram(aes(y = ..density..), fill = 'pink', alpha = 0.9) + 
    geom_density(colour = 'blue') + 
    xlab(expression(bold('Sales'))) + 
    ylab(expression(bold('Density'))) + 
    ggtitle("Distribution of Sales by Week")


# positive sales
ggplot(retail_pos_Weekly, aes(x = Sales)) + 
  geom_histogram(aes(y = ..density..), fill = 'green', alpha = 0.5) + 
  geom_density(colour = 'blue') + 
  xlab(expression(bold('Sales'))) + 
  ylab(expression(bold('Density'))) + 
  ggtitle("Distribution of Positive Sales")

# positive sales skewness & kurtosis
skewness(retail_pos_Weekly$Sales)
kurtosis(retail_pos_Weekly$Sales)


# negative sales
ggplot(retail_neg_Weekly, aes(x = Sales)) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + 
  xlab(expression(bold('Sales'))) + 
  ylab(expression(bold('Density'))) + 
  ggtitle("Distribution of Negative Sales")

# negative sales skewness & kurtosis
skewness(retail_neg_Weekly$Sales)
kurtosis(retail_neg_Weekly$Sales)

```
***



## V. Modeling & Evaluation

A trend or seasonal component is common for gift sales. A decomposition of the model for the observed, trend, seasonal, and random components cannot be done because the data set only contains 13 months (less than two seasonal cycles of data).


### (i) Positive Sales {.tabset}

#### Time Series Model
```{r lkslddfa}
Sales.ts_pos<-ts(retail_pos_Weekly$Sales,frequency=52)
plot(Sales.ts_pos)
```

#### Simple Moving Average
n=15 and 4 predictive points   
The next month of sales is predicted to total **$1,198,405**.
```{r lkds4la3}
Sales15_pos<-movavg_pred(retail_pos_Weekly$Sales,15,type="s",h=4)
sum(Sales15_pos[54:57])
```

n=30 and 4 predictive points   
The next month of sales is predicted to total **$938,841.1**.
```{r lkds4la}
Sales30_pos<-movavg_pred(retail_pos_Weekly$Sales,30,type="s",h=4)
sum(Sales30_pos[54:57])
```

n=50 and 4 predictive points   
The next month of sales is predicted to total **$801,493.8**.
```{r lkdslda}
Sales50_pos<-movavg_pred(retail_pos_Weekly$Sales,50,type="s",h=4)
sum(Sales50_pos[54:57])
```

***

#### Exponential Smoothing
n=15 and 4 predictive points   
The next months of sales is predicted to total **$1,634,202**.
```{r lkds4dla3}
Sales15e_pos<-movavg_pred(retail_pos_Weekly$Sales,15,type="e",h=4)
sum(Sales15e_pos[54:57])
```

n=30 and 4 predictive points   
The next months of sales is predicted to total **$1,545,821**.
```{r lkdas4la}
Sales30e_pos<-movavg_pred(retail_pos_Weekly$Sales,30,type="e",h=4)
sum(Sales30e_pos[54:57])
```

n=50 and 4 predictive points   
The next months of sales is predicted to total **$1,491,076**.
```{r lkds4lda}
Sales50e_pos<-movavg_pred(retail_pos_Weekly$Sales,50,type="e",h=4)
sum(Sales50e_pos[54:57])
```

***



#### ARIMA
Optimal parameters for the ARIMA model are found using the auto.arima() function. Looking at the tsdisplay() output, the function gave good parameters.   

The next months sales are predicted to total **$1,629,354**.
```{r lkds45dla3}
auto.arima(retail_pos_Weekly$Sales)
SalesARIMA_pos<-arima(Sales.ts_pos,order=c(0,1,1))
SalesARIMA_pos
tsdisplay(residuals(SalesARIMA_pos))
SalesARIMA_pos_forecast<-forecast(SalesARIMA_pos,h=4)
SalesARIMA_pos_forecast
plot(SalesARIMA_pos_forecast)
```

***

#### - EVALUATION - 
```{r jsklakla}
 plot(retail_pos_Weekly$Sales,type="l",col="1",main="Moving Averages",xlab="Weeks",ylab="Retail Sales")
 lines(Sales15_pos,col=2)
 lines(Sales30_pos,col=3)
 lines(Sales50_pos,col=4)
 lines(Sales15e_pos,col=5)
 lines(Sales30e_pos,col=6)
 lines(Sales50e_pos,col=7)
 legend(10,550000,c("Original Data","MV(15)","MV(30)","MV(50)","Exp(15)","Exp(30)","Exp(50)"),col=1:7,lty=1,lwd=1,box.col="gray",bg="white")
```

Model | Average Prediction
------------- | -------------
Simple Moving Average | 979,580
Exponential Smoothing | 1,557,033
ARIMA | 1,629,354


The predicted sales for the next 4 weeks are calculated using the exponential smoothing and simple moving average (using 15, 30, and 50 weeks in hindsight). The single moving averages with a higher window parameter produces a more stable model. Thus, the simple moving average model with 50 weeks in hindsight is more stable than the 30 and 15 week models.    
    
From the time series visualizations of each week (see above), there is debate whether which algorithm is best for this problem. The MV(50) model performs well at the beginning of the time frame due to having 50 weeks hindsight. MV(50) predicts the smallest amount of sales for the next 4 weeks. 
   
The Exp(15) model captures the trend and fluctuation of the sales data at the end of the time frame the best due to only considering 15 weeks hindsight. Exp(15) predicts the largest amount of sales for the next 4 weeks.    
    
When considering the original data, the sales spark at the very beginning of the time frame, then remain constant for several weeks, then rise again at the end of the time frame. This may be a seasonal trend but, our data set contains less than 2 seasonal cycles of data, so seasonality cannot be determined.   
    
When considering all the models, the average of the predictions shows that the Exponential Smoothing and ARIMA algorithms are agreeable in their predictions that the next 4 week’s sales data will be around $1.5 million. **We will choose either the Exponential Smoothing or ARIMA algorithm for future predictions. It is suggested that more data be collected for seasonality analysis, which will lead to better predictions**.  





***

### (ii) Negative Sales {.tabset}

#### Time Series Model
```{r lkslddf2a}
Sales.ts_neg<-ts(retail_neg_Weekly$Sales,frequency=52)
plot(Sales.ts_neg)
```

#### Simple Moving Average
n=15 and 4 predictive points   
The next month of negative sales is predicted to total **$-105,506.60**.
```{r lk22ds4la3}
Sales15_neg<-movavg_pred(retail_neg_Weekly$Sales,15,type="s",h=4)
sum(Sales15_neg[54:57])
```

n=30 and 4 predictive points   
The next month of negative sales is predicted to total **$-78,125.51**.
```{r lkds24la}
Sales30_neg<-movavg_pred(retail_neg_Weekly$Sales,30,type="s",h=4)
sum(Sales30_neg[54:57])
```

n=50 and 4 predictive points   
The next month of negative sales is predicted to total **$-67,694.15**.
```{r lkdslda2}
Sales50_neg<-movavg_pred(retail_neg_Weekly$Sales,50,type="s",h=4)
sum(Sales50_neg[54:57])
```

***

#### Exponential Smoothing
n=15 and 4 predictive points   
The next months of negative sales is predicted to total **$-441,156.30**.
```{r lkds4dla32}
Sales15e_neg<-movavg_pred(retail_neg_Weekly$Sales,15,type="e",h=4)
sum(Sales15e_neg[54:57])
```

n=30 and 4 predictive points   
The next months of negative sales is predicted to total **$-435,185.70**.
```{r l2kdas4la}
Sales30e_neg<-movavg_pred(retail_neg_Weekly$Sales,30,type="e",h=4)
sum(Sales30e_neg[54:57])
```

n=50 and 4 predictive points   
The next months of negative sales is predicted to total **$-432,179.90**.
```{r lkds4l2da}
Sales50e_neg<-movavg_pred(retail_neg_Weekly$Sales,50,type="e",h=4)
sum(Sales50e_neg[54:57])
```

***



#### ARIMA
Optimal parameters for the ARIMA model are found using the auto.arima() function. Looking at the tsdisplay() output, the function gave good parameters.   

The next months negative sales are predicted to total **$-101,155.39**.
```{r lkds452dla3}
auto.arima(retail_neg_Weekly$Sales)
SalesARIMA_neg<-arima(Sales.ts_neg,order=c(0,0,1))
SalesARIMA_neg
tsdisplay(residuals(SalesARIMA_neg))
SalesARIMA_neg_forecast<-forecast(SalesARIMA_neg,h=4)
SalesARIMA_neg_forecast
plot(SalesARIMA_neg_forecast)
```

***

#### - EVALUATION - 

```{r lkds4o52dla3}
plot(retail_neg_Weekly$Sales,type="l",col="1",main="Moving Averages",xlab="Weeks",ylab="Retail Sales")
lines(Sales15_neg,col=2)
lines(Sales30_neg,col=3)
lines(Sales50_neg,col=4)
lines(Sales15e_neg,col=5)
lines(Sales30e_neg,col=6)
lines(Sales50e_neg,col=7)
legend(10,-90000,c("Original Data","MV(15)","MV(30)","MV(50)","Exp(15)","Exp(30)","Exp(50)"),col=1:7,lty=1,lwd=1,box.col="gray",bg="white")
```

***

Model | Average Prediction
------------- | -------------
Simple Moving Average | -83,775.42
Exponential Smoothing | -436,173.97
ARIMA | -101,155.39


   
The predicted negative sales for the next 4 weeks are calculated using the exponential smoothing and simple moving average (using 15, 30, and 50 weeks in hindsight). The single moving averages with a higher window parameter produces a more stable model. Thus, the simple moving average model with 50 weeks in hindsight is more stable than the 30 and 15 week models.    
    
From the time series visualizations of each week (see above), there is debate whether which algorithm is best for this problem. The MV(50) model performs well at the beginning of the time frame due to having 50 weeks hindsight. MV(50) predicts the smallest amount of negative sales for the next 4 weeks.     
    
All exponential models capture the drastic increase in negative sales at the end of the time frame. Because of this, the exponential model predictions are drastically different than simple and ARIMA models. 
     
When considering the original data, the negative sales have a moderate increase at the beginning of the time frame, then remain constant for several weeks, then sharply increase again at the end of the time frame. The same pattern was followed with the positive sales.     
   
When considering all the models, the Exponential Smoothing model is the clear outlier. Simple and ARIMA algorithms are agreeable in their predictions that the next 4 week's negative sales data will be around $-92,500. This is likely more accurate unless the drastic negative sales continues into the new year. **We will choose either the Simple or ARIMA algorithm for future predictions. It is suggested that more data be collected for seasonality analysis, which will lead to better predictions**.  
