pcm.cv <- function(data, cur.tt, n.step, lower=quantile(cur.tt,.8),
                   upper=quantile(cur.tt,1-(5/nrow(data$x))), n.fold=10, cv.order) 
{
  #require(brglm)
  #set.seed(123456)
  n <- ncol(data$x)
  breaks <- round(seq(from=1,to=(n+1),length=(n.fold+1)))
  if (is.null(cv.order)) {
    cv.order <- sample(1:n)
  } else cv.order = cv.order
  th <- seq(from=lower, to=upper, length=n.step)
  best.w <- 0
  for (i in 1:n.step) {
    Sys.sleep(0.25)
    #cat(i)
    #cat("\n")
    cur.genes <- (cur.tt>th[i])
    w.vec <- rep(NA,n.fold)
    for (j in 1:n.fold) {
      cur.lo <- cv.order[(breaks[j]):(breaks[j+1]-1)]
      cur.svd <- svd(data$x[cur.genes,-cur.lo])
      cur.v <- t(data$x[cur.genes,cur.lo]) %*%
        cur.svd$u %*% diag(1/cur.svd$d)
      cur.glm <- glm(data$y[cur.lo]~cur.v[,1])
      w.vec[j] <- cur.glm$null.deviance - cur.glm$deviance
    }
    cur.w <- mean(w.vec)
    #print(cur.w)
    if (cur.w > best.w) {
      best.th <- th[i]
      best.w <- cur.w
    }
    
    
    # Increment the progress bar, and update the detail text.
    incProgress(1/n.step, detail = paste("Doing part", i, " Of ", n.step))
    
    # Pause for 0.1 seconds to simulate a long computation.
    Sys.sleep(0.25)
    
    #print(i)
  }
  res <- list(best.th, foldid = cv.order)
  return(res)
}
