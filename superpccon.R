superpccon <- function(file1, file2, nstep, prop, nfold, cv.order, sub){
  
  #Split the data file into training and testing data files
  if (is.null(sub)) {
    sub <- sample(nrow(file2), floor(nrow(file1) * prop))  #### sample size cannot be too small for choosing threshold
  } else sub = sub
  
  training <- file2[sub, ]
  testing <- file2[-sub, ]
  
  #Create training and testing clinical files
  training.ID<-training$bar_patient_barcode
  training.CI<-file1[file1$bcr_patient_barcode %in% c(training.ID),]
  
  testing.ID<-testing$bar_patient_barcode
  testing.CI<-file1[file1$bcr_patient_barcode %in% c(testing.ID),]
  
  #Create training data objects
  train.x<-training[, -ncol(training)]
  train.x.t<-as.matrix(t(train.x))
  rownames(train.x.t)<-NULL
  #colnames(train.x)<-NULL
  train.y<-training.CI$OS
  #train.censor<-training.CI$Status
  train.featurenames<-colnames(file2)
  train.data<-list(x=train.x.t,y=train.y,featurenames=train.featurenames)
  
  #Create testing data objects
  test.x<-testing[, -ncol(testing)]
  test.x.t<-t(test.x)
  rownames(test.x.t)<-NULL
  #colnames(test.x)<-NULL
  test.y<-testing.CI$OS
  #test.censor<-testing.CI$Status
  test.featurenames<-colnames(file2)
  test.data<-list(x=test.x.t,y=test.y,featurenames=test.featurenames)
  
  # train  the model for survival data.
  # This step just computes the  scores for each feature.
  train.obj<-superpc.train(train.data, type="regression")
  
  pcm_res <- pcm.cv3(train.data, train.obj$feature.scores, n.fold = nfold, n.step = nstep, cv.order = cv.order)
  pcm <- pcm_res[[1]]
  foldid <- cbind.data.frame(pcm_res[[2]], sub)
  
  #Compute supervised principal components, using scores from "object" - type="continuous"
  fit.cts<- superpc.predict(train.obj, train.data, test.data, threshold=pcm, n.components=2, prediction.type="continuous")
  #Compute supervised principal components, using scores from "object" - type="discrete"
  fit.groups<- superpc.predict(train.obj, train.data, test.data, threshold=pcm, n.components=1, prediction.type="discrete")
  
  #Fit predictive model using outcome of supervised principal components, via coxph for surival analysis
  res <- superpc.fit.to.outcome(train.obj, test.data, fit.cts$v.pred, print = F)
  pvalue <- res$coeftable[2, 4]
  
  pred_table <- data.frame(outcome = test.y, pred.contiuous = as.vector(fit.cts$v.pred[, 1]), pred.discrete = as.vector(fit.groups$v.pred), stringsAsFactors = FALSE)#km <- survfit(Surv(test.data$y,test.data$censoring.status)~fit.groups$v.pred)
  
  #Form reduced models to approximate the supervised principal component predictor.
  sink("sink-examp.txt")
  fit.red<- superpc.predict.red(train.obj, train.data, test.data, threshold=pcm, n.components=2)
  sink()
  
  #Return a list of the important predictors
  res_data <- superpc.listfeatures(test.data, train.obj, fit.red)
  
  results <- list(pvalue, pred_table, res_data, foldid)
  
  
  return(results)
}
