#Here we follow Medeiros et al. (2019) in defining four functions:

#One for forming forecasts using LASSO or Elastic Net model selected on BIC, which will be called
#on each iteration of the rolling window forecasting exercise.

#The second one for producing the series of h-step LASSO forecasts using rolling window.

#The third one will take the resulting coefficients from LASSO, seek out the nonzero ones and rerun OLS with selected predictors to produce post-LASSO forecast.
#The fourth one will call on the third one for producing the series of h-step post-LASSO forecasts using rolling window.

#Inputs for the function:

#1) Data matrix Y: includes all variables

#2) indice - index for dependent variable: 1 for CPI inflation, 2 for PCE inflation

#3) lag - the forecast horizon

#4) pl - set to TRUE if wnat to do post-LASSO or FALSE for vanilla LASSO.
#Timing: the rolling window function below passes the data up to period T to this function, and the resulting
#forecast is compared with y_{T+1}. For an h-step forecast the information set must therefore end at T-h+1,
#so the first step drops the last h-1 rows of the window. The principal components, the lags and the dummy are then
#built from data up to the forecast origin only.


runrlasso=function(Y,indice,lag,pl){
  
  Y=Y[1:(nrow(Y)-lag+1),] #drop the last h-1 rows so that the window ends at the forecast origin (nothing is dropped when lag=1)
  dum=Y[,ncol(Y)] # extract dummy from data
  Y=Y[,-ncol(Y)] #data without the dummy
  comp=princomp(scale(Y,scale=FALSE)) # compute principal components to add as predictors
  Y2=cbind(Y,comp$scores[,1:4]) #augment predictors by the first 4 principal components
  aux=embed(Y2,4+lag) #create 4 lags + forecast horizon shift (=lag option)
  y=aux[,indice] #  Y variable aligned/adjusted for missing data due do lags
  X=aux[,-c(1:(ncol(Y2)*lag))]   # lags of Y (predictors) corresponding to forecast horizon   
  
  X.out=tail(aux,1)[1:ncol(X)] #retrieve the last 4 observations of the window - these are the regressors for the h-step forecast (same construction for every horizon)
  dum=tail(dum,length(y)) #cut the dummy to size to account for lost observations due to lags
  
  
  #Here we replace the glmnet-based LASSO by rlasso:
  
  #If we want to use predict() with rlasso, we need to stuff things into a dataframe
  #type object first:
  
  #All the predictors:
  tempd=cbind(X,dum)
  #Append the test set at the end and set as dataframe:
  df=as.data.frame(rbind(tempd,c(X.out,0)))
  
  #Run (post-)lasso (default setting, heteroskedasticity-adjusted):
  model=rlasso(df[1:nrow(X),],y,post=pl) 
  
  
  pred=predict(model,newdata=df[nrow(X)+1,]) #generate the forecast (note that c(X.out,0) is the last row of the dataframe)
  
  return(list("model"=model,"pred"=pred)) #return the estimated model and h-step forecast
}


#This function will repeatedly call the previous function in the rolling window h-step forecasting

#Inputs for the function:

#1) Data matrix Y: includes all variables

#2) nprev - number of out-of-sample observations (at the end of the sample)

#3) indice - index for dependent variable: 1 for CPI inflation, 2 for PCE inflation

#4) lag - the forecast horizon

#5) pl - set to TRUE if wnat to do post-LASSO or FALSE for vanilla LASSO.



rlasso.rolling.window=function(Y,nprev,indice=1,lag=1,pl=FALSE){
  
  save.coef=matrix(NA,nprev,21-3+ncol(Y[,-1])*4 ) #blank matrix for coefficients at each iteration
  save.pred=matrix(NA,nprev,1) #blank for forecasts
  for(i in nprev:1){ #NB: backwards FOR loop: going from 180 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    lasso=runrlasso(Y.window,indice,lag,pl) #call the modified function to fit rlasso and predict
    save.coef[(1+nprev-i),]=lasso$model$coef #save estimated coefficients
    save.pred[(1+nprev-i),]=lasso$pred #save the forecast
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  #Some helpful stuff:
  real=Y[,indice] #get actual values
  #plot(real,type="l")
  #lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}


