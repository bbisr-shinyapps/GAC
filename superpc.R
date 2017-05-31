superpc <- function(data1, data2, nstep, prop, nfold, cv.order, splitIndex){
  
  group <- ifelse(as.factor(as.vector(unlist(data1[1, -1])))=="HR", 1, 0)
  table(group)
  
  yy <- t(data2[, -1])
  colnames(yy) <- data1$X[-1]
  rownames(yy) <- colnames(data1)[-1]
  yy <- data.frame(cbind(yy, group)) # file[-c(1:2), -c(1:2)]
  
  #Split the data file into training and testing data files
  #set.seed(1234)
  if (is.null(splitIndex)) {
    splitIndex <- createDataPartition(yy[,"group"], p = prop, list = FALSE, times = 1)
  } else splitIndex = splitIndex

  trainDF <- yy[ splitIndex,]
  testDF  <- yy[-splitIndex,]
  
  est <- NULL
  var.names = colnames(trainDF)[-ncol(trainDF)]
  coef <- t(sapply(var.names,
                   
                   function(var) {
                     formula    <- as.formula(paste("group ~", var))
                     res.logist <- glm(formula, data = trainDF, family = binomial) 
                     est <- rbind(est, coef(res.logist))
                     
                     return(est)
                   }))
  absest = abs(coef[, 2])
  coef <- data.frame(cbind(coef, absest))
  
  data = list(x=t(trainDF[,-ncol(trainDF)]),y=trainDF[,ncol(trainDF)])
  cur.tt = abs(coef$absest)
  
  pcm_res <- pcm.cv(data, cur.tt, n.step = nstep, n.fold = nfold, cv.order = cv.order)
  pcm <- pcm_res[[1]]
  foldid <- cbind.data.frame(pcm_res[[2]], splitIndex)
  
  x.pcm <- t(trainDF[,(coef$absest>pcm)])
  svd.pcm <- svd(x.pcm)
  test.v.pcm <- as.matrix(testDF[,(coef$absest>pcm)]) %*% svd.pcm$u %*% diag(1/svd.pcm$d)
  sim.pcm.glm <- glm(testDF[,"group"]~test.v.pcm, family = binomial)
  pvalue.sim.pcm.glm <- as.vector(summary(sim.pcm.glm)$coefficients[-1, 4])
  ncom <- which(pvalue.sim.pcm.glm == min(pvalue.sim.pcm.glm))
  pvalue.ncom <- pvalue.sim.pcm.glm[as.numeric(ncom)]
  pred.contiuous <- test.v.pcm[, as.numeric(ncom)]
  pred.discrete <- ifelse(test.v.pcm[, as.numeric(ncom)]>=median(as.numeric(test.v.pcm[,as.numeric(ncom)])), "Predicted High Risk", "Predicted Low Risk")
  res_data <- data.frame(group=testDF[, "group"], pred.contiuous, pred.discrete, stringsAsFactors = FALSE)
  res_data$group_label <- ifelse(res_data$group == 1, "High Risk", "Low Risk")
  
  final <- data.frame(yy[,(coef$absest>pcm)])
  final$group <- yy[, ncol(yy)]
  var.names.yy = colnames(final)[-ncol(final)]
  
  res <- t(sapply(var.names.yy,
                  
                  function(var) {
                    
                    formula    <- as.formula(paste("group ~", var))
                    res.logist <- glm(formula, data = final, family = binomial)
                    est <- cbind(exp(cbind(OR = coef(res.logist), confint(res.logist))), pvalue = summary(res.logist)$coefficients[, 4])[2, ]
                    
                    return(est)
                  }))
  
   results <- list(pvalue.ncom, res_data, res, foldid)
  
  return(results)
}
