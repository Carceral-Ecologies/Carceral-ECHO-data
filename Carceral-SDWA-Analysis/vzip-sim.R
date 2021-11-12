require(fabricatr)

set.seed(111111)
N=2000
df <- fabricate(N=N,
                X = rnorm(N),
                Z=rbinom(n=N, size=1, prob=plogis(X*2 + 3)),
                Y1=ifelse(Z, 0, rpois(n=N, lambda = exp(X*3 - 2))),
                Y2=ifelse(Z, 0, rpois(n=N, lambda = exp(X*-3 - 2))),
                Y3=ifelse(Z, 0, rpois(n=N, lambda = exp(X - 2)))
)

table(Z=df$Z, df$Y1 == 0) |> print()
table(Z=df$Z, df$Y2 == 0) |> print()
table(Z=df$Z, df$Y3 == 0) |> print()


df$i <- with(df, (Y1 == 0 & Y2 == 0 & Y3 == 0))

df$Zhat <- 0 + df$i

#repeat {}

w0 = .00001
w0 = 1

m_perfect_0 <- m_perfect <- glm(!i~X, df, family=binomial())  
m_y1_0 <- m_y1 <- glm(Y1~X, df, family=poisson(), weights=ifelse(i, w0, 1))
m_y2_0 <- m_y2 <- glm(Y2~X, df, family=poisson(), weights=ifelse(i, w0, 1))
m_y3_0 <- m_y3 <- glm(Y3~X, df, family=poisson(), weights=ifelse(i, w0, 1))


pler <- function(m) predict(m, type='response')

for(j in 1:10) {
  
  # E step
  
  df$Zhat[df$i] <- (
    1 /  (1 + pler(m_perfect) * dpois(0, lambda = pler(m_y1)) * dpois(0, lambda=pler(m_y2))* dpois(0, lambda=pler(m_y3)))
  )[df$i]
  
  # M step
  
  m_y1 <- glm(Y1~X, df, family=poisson(), weights = ifelse(i, 1 - Zhat, 1), start=coef(m_y1))
  #summary(m_y1)
  
  m_y2 <- glm(Y2~X, df, family=poisson(), weights = ifelse(i, 1 - Zhat, 1), start=coef(m_y2))
  #summary(m_y2)
  
  m_y3 <- glm(Y3~X, df, family=poisson(), weights = ifelse(i, 1 - Zhat, 1), start=coef(m_y3))
  
  
  m_perfect <- glm(!i~X, df, family=binomial(), weights = ifelse(i, Zhat, 1), start=coef(m_perfect))
  #summary(m_perfect)
  
}


cat("*****************************************************\n")
rbind( coef(summary(m_perfect_0)), coef(summary(m_perfect)) )  |> print()
cat("*****************************************************\n")
rbind( coef(summary(m_y1_0)), coef(summary(m_y1)) ) |> print()
cat("*****************************************************\n")
rbind( coef(summary(m_y2_0)), coef(summary(m_y2)) ) |> print()
cat("*****************************************************\n")
rbind( coef(summary(m_y3_0)), coef(summary(m_y3)) ) |> print()
cat("*****************************************************\n")
