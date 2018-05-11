#### Additional function form PLM: http://tarohmaru.web.fc2.com/R/plmFunctions.R


icc1 <- function(X, id){ 
  aov1 <- aov(X ~ -1 + factor(id))
  v.btwn <- var(coef(aov1))
  v.wthn <- sum(resid(aov1)^2)/aov1$df.residual
  return(v.btwn/(v.wthn + v.btwn))
} 


m.lines <- function(x, y, group){ 
  id <- unique(group)
  dat <-  na.omit(data.frame(x, y))
  for(i in 1:length(id)){ # 
    lines(dat[group==id[i], 1], dat[group==id[i], 2]) #
  }
}


logLik.plm <- function(...){
  x <- list(...)
  n <- length(x)
  out <- numeric(n)
  names(out) <- names(x)
  for(i in 1:n){
    ss <- sum(x[[i]]$residuals^2) / x[[i]]$df.residual
    N <- length(x[[i]]$residuals)
    out[i] <- -0.5 * N * log(2*pi*ss) - sum(x[[i]]$residuals^2) /(2 * ss)
  }
  return(out)
}

# AIC, BIC 
aic.plm <- function(... , digits=2){ 
  x <- list(...)
  n <- length(x)
  LL <- logLik.plm(...)
  N  <- aic <- bic <- adj.rsq <- n.par <- numeric(n)
  for(i in 1:n){
    n.par[i] <- length(x[[i]]$residuals) - x[[i]]$df.residual
    N[i] <- length(x[[i]]$residuals)
    adj.rsq[i] <- summary(x[[i]])$r.squared[2]
  }
  aic <- -2 * LL + 2 * n.par
  bic <- -2 * LL + log(N) * n.par
  out <- rbind(adj.rsq, LL, n.par, aic, bic)
  rownames(out) <- c("adjusted R squared", "Log Likelihood",
                     "n of parameters", "AIC", "BIC")
  m <- match.call(expand.dots = FALSE)
  colnames(out) <- sapply(m$..., paste)
  return(round(out, digits))
}

# plm() class 
p.cooks.disnance <- function(model.plm){  
  h <- X %*% solve(t(X) %*% X) %*% t(X) 
  h.ii <- diag(h) 
  resid <- model.plm$residuals 
  v <- sum(resid^2) / model.plm$df.residual 
  k <- length(coef(model.plm)) 
  h2 <- h.ii / (1 - h.ii)^2
  r2 <- resid^2/ (k * v)
  return(r2 * h2)
}

# plm() class 
p.vif <- function(model.plm){  
  X <- model.matrix(model.plm) 
  k <- ncol(X) 
  r2 <- numeric(ncol(X)) 
  names(r2) <- colnames(X)
  for(i in 1:k){
    r2[i] <- summary(lm(X[,i]~.,  data=as.data.frame(X[,-i])) )$r.squared
  }
  return(1/(1-r2))
}
