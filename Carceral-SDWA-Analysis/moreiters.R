for(j in 1:10) {
  
  message("**************************************")
  message("* Iteration ", j, " at ", as.character(Sys.time()) )
  
  # E step
  
  Zold <- df$Zhat
  
  df$Zhat[df$i] <- (
    1 /  (1 + pler(m_perfect) * dpois(0, lambda = pler(m_y1)) * dpois(0, lambda=pler(m_y2)) * dpois(0, lambda=pler(m_y3)) * dpois(0, lambda=pler(m_y4)))
  )[df$i]
  
  message("* dZ = ", mean(abs(Zold - df$Zhat)))
  message("**************************************")
  
  # M step
  
  m_y1 %<-% update(m_y1, start = getME(m_y1, c("theta","fixef")), data = df, weights = ifelse(i, 1-Zhat, 1))
  m_y2 %<-% update(m_y2, start = getME(m_y2, c("theta","fixef")), data = df, weights = ifelse(i, 1-Zhat, 1))
  m_y3 %<-% update(m_y3, start = getME(m_y3, c("theta","fixef")), data = df, weights = ifelse(i, 1-Zhat, 1))
  m_y4 %<-% update(m_y4, start = getME(m_y4, c("theta","fixef")), data = df, weights = ifelse(i, 1-Zhat, 1))
  
  m_perfect %<-% update(m_perfect, start = getME(m_perfect, c("theta","fixef")), weights = ifelse(i, Zhat, 1))
  #summary(m_perfect)
  
}
