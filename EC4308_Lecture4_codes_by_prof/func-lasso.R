#Here we follow Medeiros et al. (2019) in defining two functions:

#One for forming forecasts using LASSO or Elastic Net model selected on BIC, which will be called
#on each iteration of the rolling window forecasting exercise.

#The second one for producing the series of h-step LASSO forecasts using rolling window.


#Inputs for the function:

#1) Data matrix Y: includes all variables

#2) indice - index for dependent variable: 1 for CPI inflation, 2 for PCE inflation

#3) lag - the forecast horizon

#4) alpha - the alpha parameter determining whether we use LASSO (alpha=1) or ElNet (alpha=0.5)

#5) IC - information criterion used to select lambda. Can set to: "bic", "aic" or "aicc"
#Timing: the rolling window function below passes the data up to period T to this function, and the resulting
#forecast is compared with y_{T+1}. For an h-step forecast the information set must therefore end at T-h+1,
#so the first step drops the last h-1 rows of the window. The principal components, the lags and the dummy are then
#built from data up to the forecast origin only.


runlasso=function(Y,indice,lag,alpha=1,IC="bic"){
  
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

  
  #Here we use the glmnet wrapper written by the authors that does selection on IC:
  model=ic.glmnet(cbind(X,dum),y,crit=IC,alpha = alpha) #fit the LASSO/ElNet model selected on IC (glmnet standardizes the predictors internally)

  pred=predict(model,c(X.out,0)) #generate the forecast (note c(X.out,0) gives the last observations on X's and the dummy (the zero))
  
  return(list("model"=model,"pred"=pred)) #return the estimated model and h-step forecast
}


#This function will repeatedly call the previous function in the rolling window h-step forecasting

#Inputs for the function:

#1) Data matrix Y: includes all variables

#2) nprev - number of out-of-sample observations (at the end of the sample)

#3) indice - index for dependent variable: 1 for CPI inflation, 2 for PCE inflation

#4) lag - the forecast horizon


#5) alpha - the alpha parameter determining whether we use LASSO (alpha=1) or ElNet (alpha=0.5)

#6) IC - information criterion used to select lambda. Can set to: "bic", "aic" or "aicc"


lasso.rolling.window=function(Y,nprev,indice=1,lag=1,alpha=1,IC="bic"){
  
  save.coef=matrix(NA,nprev,21-3+ncol(Y[,-1])*4 ) #blank matrix for coefficients at each iteration
  save.pred=matrix(NA,nprev,1) #blank for forecasts
  for(i in nprev:1){ #NB: backwards FOR loop: going from 180 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    lasso=runlasso(Y.window,indice,lag,alpha,IC) #call the function to fit the LASSO/ElNET selected on IC and generate h-step forecast
    save.coef[(1+nprev-i),]=lasso$model$coef #save estimated coefficients
    save.pred[(1+nprev-i),]=lasso$pred #save the forecast
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  #Some helpful stuff:
  real=Y[,indice] #get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}



