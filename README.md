
Online Retail Data
Time Series Predictive Modeling
Kimberly Healy | healy.kim@gmx.us

    Background
    I. Load required libraries
    II. Load raw data from Excel file
    III. Define Useful Functions
        (i) Moving Average with Prediction
    IV. Data Exploration
        (i) Basic Exploration
        (ii) Sales Exploration
    V. Modeling & Evaluation
        (i) Positive Sales
        (ii) Negative Sales

Background

The data (OnlineRetail.csv) come from all transactions occurring between 01/12/2010 and 09/12/2011 for a UK-based online retail store. The company mainly sells unique gifts and the majority of customers are wholesalers. This report will use techniques to explore the data and use three different time series algorithms to model sales. The conclusion states which model(s) best predict sales and recommendations for further analysis.

The following predictive models are used:
Model 1: Simple Moving Average: a weighted average function that identifies the previous pattern in the data and uses that pattern to predict future data points.
Model 2: Exponential Smoothing: similar to Simple Moving Average, but contains a smoothing constant which can be tweaked for more responsive or more stable models.
Model 3: Autoregressive Integrated Moving Average (ARIMA): unlilke the previous models, ARIMA considers the correlations among the previous values of the time series to predict future values.

The following question is answered:
- Which model best predicts company sales?
I. Load required libraries

library(tidyverse)
library(bpa)
library(forecast)
library(ggplot2)
library(knitr)
library(kableExtra)
library(moments)

II. Load raw data from Excel file

retail_raw <- readxl::read_excel("OnlineRetail.xlsx")
retail <- retail_raw%>%
    mutate(across(where(is.character), as.factor))%>%
    mutate(Sales = (Quantity*UnitPrice))
retail$CustomerID<-as.factor(retail$CustomerID)

III. Define Useful Functions
(i) Moving Average with Prediction

Source: Quanghau (Robin) Qiu, Professor of Informational Science, Penn State Engineering

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

The “n” input for the movavg_pred function used for the Simple Moving Average and Exponential Smoothing models indicates how many previous data points (previous months) are considered in the forecast. The larger to n, the more previous data points (months) are considered, and typically the higher accuracy of the forecast.
IV. Data Exploration
(i) Basic Exploration

    Summary
    NAs
    UnitPrice & Quantity
    Biz Expenses

The data set dimensions are 541,909 rows by 8 attributes.
There are negative numbers for Quantity and UnitPrice. Sales is derived from Quantity and UnitPrice, so it’s worth looking into.

dim(retail)

## [1] 541909      9

str(retail)

## tibble [541,909 × 9] (S3: tbl_df/tbl/data.frame)
##  $ InvoiceNo  : Factor w/ 25900 levels "536365","536366",..: 1 1 1 1 1 1 1 2 2 3 ...
##  $ StockCode  : Factor w/ 4070 levels "10002","10080",..: 3538 2795 3045 2986 2985 1663 801 1548 1547 3306 ...
##  $ Description: Factor w/ 4211 levels "?","? sold as sets?",..: 4014 4022 919 1947 2967 3221 1560 1685 1682 248 ...
##  $ Quantity   : num [1:541909] 6 6 8 6 6 2 6 6 6 32 ...
##  $ InvoiceDate: POSIXct[1:541909], format: "2010-12-01 08:26:00" "2010-12-01 08:26:00" ...
##  $ UnitPrice  : num [1:541909] 2.55 3.39 2.75 3.39 3.39 7.65 4.25 1.85 1.85 1.69 ...
##  $ CustomerID : Factor w/ 4372 levels "12346","12347",..: 4049 4049 4049 4049 4049 4049 4049 4049 4049 541 ...
##  $ Country    : Factor w/ 38 levels "Australia","Austria",..: 36 36 36 36 36 36 36 36 36 36 ...
##  $ Sales      : num [1:541909] 15.3 20.3 22 20.3 20.3 ...

summary(retail)

##    InvoiceNo        StockCode                                  Description    
##  573585 :  1114   85123A :  2313   WHITE HANGING HEART T-LIGHT HOLDER:  2369  
##  581219 :   749   22423  :  2203   REGENCY CAKESTAND 3 TIER          :  2200  
##  581492 :   731   85099B :  2159   JUMBO BAG RED RETROSPOT           :  2159  
##  580729 :   721   47566  :  1727   PARTY BUNTING                     :  1727  
##  558475 :   705   20725  :  1639   LUNCH BAG RED RETROSPOT           :  1638  
##  579777 :   687   84879  :  1502   (Other)                           :530362  
##  (Other):537202   (Other):530366   NA's                              :  1454  
##     Quantity          InvoiceDate                    UnitPrice        
##  Min.   :-80995.00   Min.   :2010-12-01 08:26:00   Min.   :-11062.06  
##  1st Qu.:     1.00   1st Qu.:2011-03-28 11:34:00   1st Qu.:     1.25  
##  Median :     3.00   Median :2011-07-19 17:17:00   Median :     2.08  
##  Mean   :     9.55   Mean   :2011-07-04 13:34:57   Mean   :     4.61  
##  3rd Qu.:    10.00   3rd Qu.:2011-10-19 11:27:00   3rd Qu.:     4.13  
##  Max.   : 80995.00   Max.   :2011-12-09 12:50:00   Max.   : 38970.00  
##                                                                       
##    CustomerID               Country           Sales           
##  17841  :  7983   United Kingdom:495478   Min.   :-168469.60  
##  14911  :  5903   Germany       :  9495   1st Qu.:      3.40  
##  14096  :  5128   France        :  8557   Median :      9.75  
##  12748  :  4642   EIRE          :  8196   Mean   :     17.99  
##  14606  :  2782   Spain         :  2533   3rd Qu.:     17.40  
##  (Other):380391   Netherlands   :  2371   Max.   : 168469.60  
##  NA's   :135080   (Other)       : 15279

(ii) Sales Exploration

    Group by Week
    Correlation
    Visualizations

Group positive and negative sales by week.

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

Positive Sales by Week 	Year-Week 	Quantity 	UnitPrice 	Sales
1 	2010-48 	74447 	30471.27 	153652.88
2 	2010-49 	127609 	92376.39 	344382.69
3 	2010-50 	113997 	49573.83 	225806.36
4 	2010-51 	43186 	27482.98 	99904.21
5 	2011-01 	68698 	21590.68 	117879.99
6 	2011-02 	93463 	35575.04 	201898.18
7 	2011-03 	143760 	34337.82 	212374.99
8 	2011-04 	65018 	29968.87 	128955.29
9 	2011-05 	70833 	29608.58 	129149.32
10 	2011-06 	50902 	22712.82 	104824.18
11 	2011-07 	78810 	24748.15 	139823.40
12 	2011-08 	85080 	28925.88 	148808.97
13 	2011-09 	69237 	28735.35 	132238.68
14 	2011-10 	71670 	32715.55 	141077.78
15 	2011-11 	76405 	31007.20 	143686.25
16 	2011-12 	87799 	33036.98 	162567.61
17 	2011-13 	105685 	35094.13 	195018.68
18 	2011-14 	68416 	34629.62 	131513.05
19 	2011-15 	84988 	26626.44 	146760.16
20 	2011-16 	89101 	32766.44 	154376.48
21 	2011-17 	47816 	17057.39 	79491.42
22 	2011-18 	71096 	31372.51 	135047.86
23 	2011-19 	115849 	43813.22 	220927.38
24 	2011-20 	100344 	34058.00 	201528.51
25 	2011-21 	92562 	34464.15 	182923.91
26 	2011-22 	49164 	22482.01 	100443.44
27 	2011-23 	101990 	38834.70 	233274.20
28 	2011-24 	97932 	28831.37 	180207.12
29 	2011-25 	86436 	35226.24 	151332.30
30 	2011-26 	77240 	24955.15 	140266.26
31 	2011-27 	91977 	38950.75 	177359.00
32 	2011-28 	74574 	27904.75 	125941.94
33 	2011-29 	103324 	42329.02 	192460.64
34 	2011-30 	103344 	27294.68 	176293.94
35 	2011-31 	118807 	27849.81 	196614.06
36 	2011-32 	100450 	44489.80 	192093.48
37 	2011-33 	95022 	20339.93 	167158.68
38 	2011-34 	98897 	25189.56 	168711.26
39 	2011-35 	75813 	37671.23 	147743.68
40 	2011-36 	100207 	32285.23 	174311.59
41 	2011-37 	133185 	39680.82 	249903.20
42 	2011-38 	159414 	48251.72 	323144.86
43 	2011-39 	131673 	40619.75 	231538.35
44 	2011-40 	172455 	49458.80 	319097.52
45 	2011-41 	116847 	45137.07 	219289.62
46 	2011-42 	154464 	49894.33 	286328.22
47 	2011-43 	138003 	52816.41 	238323.56
48 	2011-44 	156515 	55996.90 	293522.72
49 	2011-45 	186666 	68390.13 	378921.39
50 	2011-46 	187424 	73140.02 	387633.79
51 	2011-47 	165604 	75837.38 	330859.65
52 	2011-48 	155778 	73393.70 	320360.48
53 	2011-49 	258400 	81447.22 	528931.36

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

Negative Sales by Week 	Year-Week 	Quantity 	UnitPrice 	Sales
1 	2010-48 	-11780 	362.75 	-3189.58
2 	2010-49 	-3331 	55813.40 	-59952.63
3 	2010-50 	-1781 	3943.27 	-9030.58
4 	2010-51 	-119 	496.96 	-2616.33
5 	2011-01 	-3388 	35778.44 	-39869.86
6 	2011-02 	-3837 	842.43 	-2309.03
7 	2011-03 	-76804 	3396.88 	-81761.42
8 	2011-04 	6121 	2750.79 	-5988.97
9 	2011-05 	-1901 	577.66 	-2507.81
10 	2011-06 	-1548 	2538.81 	-4605.54
11 	2011-07 	-1394 	3672.06 	-4202.31
12 	2011-08 	-1596 	12731.63 	-15651.20
13 	2011-09 	-2138 	1351.21 	-1908.30
14 	2011-10 	-15358 	5222.11 	-6438.21
15 	2011-11 	-1101 	14241.35 	-16266.66
16 	2011-12 	-5806 	415.19 	-1657.15
17 	2011-13 	-2409 	4796.82 	-9415.09
18 	2011-14 	-6786 	9658.58 	-12400.61
19 	2011-15 	-2615 	1014.34 	-2640.81
20 	2011-16 	-8065 	2284.09 	-26210.93
21 	2011-17 	-1131 	443.71 	-2073.42
22 	2011-18 	-2687 	15617.45 	-16537.81
23 	2011-19 	-7039 	4208.30 	-6597.51
24 	2011-20 	-2354 	16284.87 	-17778.07
25 	2011-21 	-3010 	4070.49 	-5357.02
26 	2011-22 	-1259 	469.13 	-1822.97
27 	2011-23 	-1698 	40591.53 	-42320.84
28 	2011-24 	-32981 	2778.23 	-6066.77
29 	2011-25 	-8144 	15358.22 	-18060.75
30 	2011-26 	-4069 	1557.83 	-3780.79
31 	2011-27 	-1581 	905.74 	-2414.76
32 	2011-28 	-1660 	11534.33 	-13261.36
33 	2011-29 	-5068 	15724.03 	-19248.22
34 	2011-30 	-2025 	542.63 	-2489.25
35 	2011-31 	-6511 	12158.94 	-17415.62
36 	2011-32 	-280 	-16805.13 	-28362.52
37 	2011-33 	-3235 	2940.78 	-5832.82
38 	2011-34 	-6 	12737.32 	-15343.33
39 	2011-35 	-13331 	1333.77 	-10158.33
40 	2011-36 	-1568 	859.86 	-2245.10
41 	2011-37 	-4136 	17503.34 	-18407.44
42 	2011-38 	-4707 	4397.73 	-12506.73
43 	2011-39 	-2805 	1266.69 	-5092.78
44 	2011-40 	-12874 	8133.39 	-16929.59
45 	2011-41 	-13217 	4614.13 	-23238.18
46 	2011-42 	-2410 	8301.30 	-13262.97
47 	2011-43 	-7724 	10148.65 	-21924.24
48 	2011-44 	-18957 	10054.70 	-13623.07
49 	2011-45 	-6730 	16791.78 	-22969.07
50 	2011-46 	-7221 	5242.13 	-8608.01
51 	2011-47 	5172 	1698.90 	-8344.12
52 	2011-48 	-5206 	2216.72 	-4479.77
53 	2011-49 	-85838 	31785.94 	-203760.36
V. Modeling & Evaluation

A trend or seasonal component is common for gift sales. A decomposition of the model for the observed, trend, seasonal, and random components cannot be done because the data set only contains 13 months (less than two seasonal cycles of data).
(i) Positive Sales

    Time Series Model
    Simple Moving Average
    Exponential Smoothing
    ARIMA
    - EVALUATION -

Sales.ts_pos<-ts(retail_pos_Weekly$Sales,frequency=52)
plot(Sales.ts_pos)

(ii) Negative Sales

    Time Series Model
    Simple Moving Average
    Exponential Smoothing
    ARIMA
    - EVALUATION -

Sales.ts_neg<-ts(retail_neg_Weekly$Sales,frequency=52)
plot(Sales.ts_neg)

