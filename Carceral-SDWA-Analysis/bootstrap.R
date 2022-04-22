#source("modelb.R")

pack <- function(models) {
  
  sapply(models, function(m) {
    
    c(getME(m, "beta"), getME(m, "u"), getME(m, "theta"))
    
    
    
  })
  
  
}

bs_iter <- function(iters=8){
  
  m2 <- list2env(as.list(m), NULL)
  
  df2 <- df[sample(nrow(df), nrow(df), replace = TRUE),]
  
  
  
  
  for(j in seq(iters)) {
    
    message("**************************************")
    message("* Iteration ", j, " at ", as.character(Sys.time()) )
    
    if(j %in% c(10, 25, 45)){
      cont$optCtrl$maxfun <- cont$optCtrl$maxfun * 2
    }
    
    
    
    # E step
    
    Zold <- df2$Zhat
    
    preds <-  eapply(m2, predict, type='response', newdata=df2, allow.new.levels=TRUE)
    preds[["perfect"]] <- preds[["perfect"]] / (1 - preds[["perfect"]]) # prob to odds
    preds[names(preds) != 'perfect'] <- lapply(preds[names(preds) != 'perfect'], dpois, x=0)
    preds <- Reduce(`*`, preds)
    
    df2$Zhat[df2$i] <- (1 /  (1 + preds) )[df2$i]
    
    dZ = sum(abs(Zold - df2$Zhat))
    message("* dZ = ", dZ)
    message("**************************************")
    
    m2$perfect %<-% update(m2$perfect, start = getME(m2$perfect, c("theta","fixef")), data=df2, weights = ifelse(i, Zhat, 1), control=cont)
    
    # M step
    for(j in setdiff(ls(m2), "perfect"))
      m2[[j]] %<-% update(m2[[j]], start = getME(m2[[j]], c("theta","fixef")), data = df2, weights = ifelse(i, 1-Zhat, 1), control=cont)
    
    #summary(m_perfect)
    
    resolve(m2)
  }
  
  
  message("* Done at ", as.character(Sys.time()) )
  
  pack(as.list(m2)[c("perfect", "hhact", "monit", "notif")])
} 

require(future.apply)
bs_points <- future_lapply(1:100, function(i) bs_iter(5), future.seed=TRUE)

# 
# for(i in 1:100) {
#   bs_points[[i]] <- bs_iter(3)
# }



save.image("~/bs-0408.RDS",compress = TRUE)
