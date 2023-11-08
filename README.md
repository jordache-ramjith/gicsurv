# gicsurv
 A shiny app for analyzing generalized interval-censoring survival data. A dataset can have exact times, right-censored times, interval-censored times, double-interval-censored times and any combination of the above.

## Purpose of the app

This app has been developed for interactive use of the methods presented in generalized interval-censored survival analysis (Ramjith \& Bousema, 2023). To this extent it allows users to analyze survival (time-to-event) data with 

1. exact times,
2. right-censored times,
3. interval-censored start times,
4. interval-censored event times,
5. double-interval-censored times (i.e. interval-censored start and event times).

All combinations of the above censoring (or none) can be present in the same dataset. The user can choose to estimate covariate effects, presented in terms of the hazard ratios, 95\% CI and p-values, as well as visualizing the baseline hazard and survival curves (with or without covariates).


## Import data

The `Import data` tab allows users to upload their own data for analysis in the app. Only `XLS/XLSX` files can be uploaded. Once the data is uploaded (never stored in the app), the user must select several parameters from the variables in their data: three time variables, a status variable and possibly covariates (not required). Further, the user can select the number of knots, where 10 (recommended) is the default option. Remember that more knots require a longer computational time. In the methodology for double-interval-censoring, time0 (= 0) is the time at last visit before the start event had occurred. And all other times are calculated as the time since this time. 

 - `time1`: The right boundary of the start/origin event interval. For data where the start time is known exactly, this can be set to zero.
 - `time2`: The last visit before the event of interest had occurred. For data where the event times are known exactly, `time2` and `time3` have the same values.
 - `time3`: This is the first visit where the event of interest was known to have occurred. For right-censored data, this can either be defined as `Inf` or set to the same values as `time2`.
 - `status`: right-censoring indicator. 1 if right-censored and 0 if not.
 
 
 
 
 
## Analysis, tables and plots
 
 
 Once the data has been imported and variables and parameters have been defined, the `Run analysis` should be clicked. If covariate(s) had been selected, then the user will automatically be taken to the `Covariate effects` tab where a summary table is shown. The table can be downloaded in a csv format by clicking the `Download the effects` button. The user can proceed to the `Baseline estimates and plots` tab. If no covariates were selected, then the user would be automatically directed to this tab after clicking `Run analysis`. In the tab there are options for the user to select that effect the resulting predictions table and plot that are displayed. 
 
 - `Maximum time in the plot`: this can be changed so that a longer/shorter time is visualized. Note this time can not exceed the maximum time in the uploaded data, else it will be reset to the data's maximum time.
 - `Number of simulations for confidence interval estimates`: The 95\% confidence intervals for the hazard, cumulative hazard and survival estimates are computed through simulating a number of parameters from a normal distribution given the mean as the parameters estimated in the model and the covariance matrix of the spline parameters. A larger number of simulations is associated with better precision surrounding the standard error of the estimates.
 - `Text for time axis`: to change the x-axis (time) labeling in the plot.
 - `File type`: before you download the plot, you may change the filetype to either png (default), pdf or jpeg. All plots are generated in 600 dpi.
 
 Thereafter the plot and/or the predicted data can be downloaded. The predicted data can easily be used to obtain the median survival time (and 95\% CI), or the user can use the downloaded data in any other program to make their own plot.
 
 Lastly, on the left panel of this page, we provide some example data that may be downloaded in excel format and can be used in the app or for a visual understanding of how to construct the data to be analyzed. 

To reference this app, please make use of the reference below.

 
## Reference

Ramjith, J. \& Bousema, T. A generalized interval-censoring survival regression model. Journal unknown. 2023.
