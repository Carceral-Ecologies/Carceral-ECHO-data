

pack <- function(...) {
  models <- list(...)
  
  sapply(models, function(m) {
    
    c(getME(m, "beta"), getME(m, "u"), getME(m, "theta"))
    
    
    
  })
  
  
}



pars <- pack(m_perfect, m_y1, m_y2, m_y3, m_y4)



### TODO

hat <- function(beta) {
  
  dim(pars) <- c(length(pars) / 5, 5)
  
  dim(beta) <- c(length(beta) / 5, 5)
  
  
  X <- getME(m_perfect, "X")
  Z <- getME(m_perfect, "Z")
  
  nb <- length(getME(m_perfect, "beta"))
  
  nb <- length(getME(m_perfect, "beta"))
  #beta <- pars[1:nb, ]
  u <- pars[seq(nb+1, nrow(pars) - 2), ]
  
  eta <- X %*% beta + Z %*% u
  
  eta[,1] <- plogis(eta[,1])
  
  eta[, -1] <- exp(eta[, -1])
  
  data.frame(as.matrix(X), Z=as.matrix(Z), eta=as.matrix(eta))
}


loglik.vzi <- function(beta, ...) {
  
  
  dim(pars) <- c(length(pars) / 5, 5)

  dim(beta) <- c(length(beta) / 5, 5)
  
    
  X <- getME(m_perfect, "X")
  Z <- getME(m_perfect, "Z")
  
  nb <- length(getME(m_perfect, "beta"))
  
  ys <- getME(m_perfect, "y")
  Y <- cbind(getME(m_y1, "y"),
             getME(m_y2, "y"),
             getME(m_y3, "y"),
             getME(m_y4, "y"))
  
  nb <- length(getME(m_perfect, "beta"))
  #beta <- pars[1:nb, ]
  u <- pars[seq(nb+1, nrow(pars) - 2), ]
  
  eta <- X %*% beta + Z %*% u
  
  phi <- plogis(eta[,1])
  
  mu <- as.matrix(exp(eta[, -1]))
  
  
  loglik0 <- log( phi + exp( log1p(-phi) - rowSums(mu) ) ) ## -mu = dpois(0, lambda = mu, log = TRUE)
  p <- dpois(Y, lambda=mu, log=TRUE)
  dim(p) <- dim(Y)
  loglik1 <- log1p(-phi) + rowSums(p)
  
  u1 <- sweep(pars[seq(nb+1, nb + 11), ],          2, FUN = dnorm, STATS =pars[nrow(pars) - 1, ], log=TRUE)
  u2 <- sweep(pars[seq(nb+12, nrow(pars) - 2), ],  2, FUN = dnorm, STATS =pars[nrow(pars), ],     log=TRUE)
  
  
  sum(loglik0[ys == 0], loglik1[ys == 1], u1, u2)
  
}

nb <- length(getME(m_perfect, "beta"))
beta <- pars[1:nb, ]


system.time({
  fit <- optim(beta, loglik.vzi, hessian = TRUE, control=list(maxit=50*1000, fnscale=-1), pars=pars)
})

system.time({
  fit <- optim(fit$par, loglik.vzi, hessian = TRUE, method="BFGS", control=list(maxit=50*1000, fnscale=-1), pars=pars)
})


# system.time({
#   H <- optimHess(beta, loglik.vzi, control=list(maxit=50*1000, fnscale=-1))
# })

system.time({
  Hp <- svd(fit$hessian)
})

rownames(fit$par) <- names(fixef(m_perfect))
colnames(fit$par) <- c("perft", "acute", "health", "montr", "notif")

se <- with(Hp, sapply(seq(length(beta)), function(i, r = u[i,] * v[i,] / (-2*d) , m=114) sum(r[1:m])))
table(sign(se))
dim(se) <- dim(fit$par)
dimnames(se) <- dimnames(fit$par)
round(se,4)


for(i in colnames(fit$par)) {
  message("**********************")  
  message("Submodel:", i)
  print(data.frame(estimate=fit$par[,i] |> round(4), se=se[,i] |> round(4)) |> transform(p = format.pval(Vectorize(pnorm)(estimate, sd=se, lower.tail=estimate < 0)*2)), digits=5 )
  cat("\n\n")
}
  

current_yr <- subset(df, FISCAL_YEAR == 2021)  

current_yr <- cbind(current_yr,
  pred=sapply(list(perft=m_perfect, acute=m_y1, health=m_y2, montr=m_y3, notif=m_y4), predict, newdata=current_yr, type='response', allow.new.levels=TRUE)
)
