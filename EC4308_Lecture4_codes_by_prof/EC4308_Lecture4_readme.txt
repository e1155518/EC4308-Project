List of files to replicate results in Lecture 4:

EC4308_Lecture4_time_series_R.R - main script that produces the results. Relies on functions files below:

func-ar.R - file containing functions to compute AR forecasts, and use rolling window to generate results.
func-lasso.R - file containing functions to compute LASSO the Elastic Net forecasts using various information criteria, and use rolling window to generate results.
func-rlasso.R - file containing functions to compute LASSO and post-LASSO forecasts using the data-dependent rule of Chernozhukov et al. (via the rlasso() function),  and use rolling window to generate results.
func-rf.R - file containing functions to compute Random Forest forecasts, and use rolling window to generate results.
rawdata.RData - FRED-MD data vintage used in Medeiros et al. (2019)
Lecture4_results.RData - file containing all the results (useful since it may take a few hours to complete the Random Forest computation for all horizons)

