# source("dataprep.R")
library(lme4)

df$TIME <- as.character(df$FISCAL_YEAR)|>as.integer() - 2010
df$TIME_gt2016 <- as.character(df$FISCAL_YEAR)|>as.integer() > 2016


f_visit <- update(base, VISITED~.-VISITED-FAC_PERCENT_MINORITY_na+TIME)

m_visit <- glmer(f_visit, df, family=binomial())
pars_visit <- getME(m_visit, c("theta","fixef"))
m_visit.restart <- update(m_visit, start=pars_visit)
summary(m_visit.restart)



f_visit_16 <- update(base, VISITED~(.-VISITED-FAC_PERCENT_MINORITY_na)*TIME_gt2016)

m_visit <- glmer(f_visit, df, family=binomial())
pars_visit <- getME(m_visit, c("theta","fixef"))
m_visit.restart <- update(m_visit, start=pars_visit)
summary(m_visit.restart)


# NB a few very large residuals
View(subset(cbind(model.frame(m_visit.restart), resid=resid(m_visit.restart, "pearson", scaled=TRUE)), abs(resid) > 3))


actual <- sapply(m0, getME, "y")[,c("hhact", 'monit', 'notif')]


v_predict <- function(X) {

  p <- cbind(#model.frame(m0$perfect), 
        hat(fit$par, X)[c("eta.1", "eta.2", "eta.3", "eta.4")]
  )
  
  (1 - p$eta.1) * data.frame(hhact=p$eta.2, monit=p$eta.3, notif=p$eta.4)
}

Xf <- model.frame(m0$perfect)
X <- getME(m0$perfect, "X")

data.frame(actual=actual, hat=v_predict()) |> head()

require(ggplot2)
require(dplyr)


Xf %>% group_by(FISCAL_YEAR) %>% summarise(rt=mean(VISITED),n=sum(VISITED))

ggplot() + geom_col(aes(x=FISCAL_YEAR,y=hhact), data= cbind(Xf, actual) %>% group_by(FISCAL_YEAR) %>%  summarise(hhact=sum(hhact)) ) + 
  geom_pointrange(aes(x=FISCAL_YEAR,y=hhact, ymin=lo, ymax=hi), col='red', size=1, data=
                  
      replicate(200, simplify = FALSE, cbind(Xf, 
                                             X %>% as.data.frame %>% mutate(VISITED=rbinom(length(VISITED), 1, 0.0295)) %>% as.matrix %>% v_predict() 
                                             ) %>% group_by(FISCAL_YEAR) %>%  summarise(hhact=sum(hhact)))       %>% 
        bind_rows(.id='replicate') %>% group_by(FISCAL_YEAR) %>% summarise(lo=quantile(hhact, .025), hi=quantile(hhact, .975), hhact=mean(hhact))
                  
                  
                  
                  ) +
  geom_pointrange(aes(x=FISCAL_YEAR,y=hhact, ymin=lo, ymax=hi), col='blue', size=1, data=
                    
                    replicate(200, simplify = FALSE, cbind(Xf, 
                                                           X %>% as.data.frame %>% mutate(VISITED=ave(VISITED, Xf$FISCAL_YEAR,FUN=sample)) %>% as.matrix %>% v_predict() 
                    ) %>% group_by(FISCAL_YEAR) %>%  summarise(hhact=sum(hhact)))       %>% 
                    bind_rows(.id='replicate') %>% group_by(FISCAL_YEAR) %>% summarise(lo=quantile(hhact, .025), hi=quantile(hhact, .975), hhact=mean(hhact))
  )  



ggplot() + geom_col(aes(x=FISCAL_YEAR,y=monit), data= cbind(Xf, actual) %>% group_by(FISCAL_YEAR) %>%  summarise(monit=sum(monit)) ) + 
  geom_pointrange(aes(x=FISCAL_YEAR,y=monit, ymin=lo, ymax=hi), col='red', size=1, data=
                    
                    replicate(200, simplify = FALSE, cbind(Xf, 
                                                           X %>% as.data.frame %>% mutate(VISITED=rbinom(length(VISITED), 1, 0.0295)) %>% as.matrix %>% v_predict() 
                    ) %>% group_by(FISCAL_YEAR) %>%  summarise(monit=sum(monit)))       %>% 
                    bind_rows(.id='replicate') %>% group_by(FISCAL_YEAR) %>% summarise(lo=quantile(monit, .025), hi=quantile(monit, .975), monit=mean(monit))
                  
                  
                  
  ) +
  geom_pointrange(aes(x=FISCAL_YEAR,y=monit, ymin=lo, ymax=hi), col='blue', size=1, data=
                    
                    replicate(200, simplify = FALSE, cbind(Xf, 
                                                           X %>% as.data.frame %>% mutate(VISITED=ave(VISITED, Xf$FISCAL_YEAR,FUN=sample)) %>% as.matrix %>% v_predict() 
                    ) %>% group_by(FISCAL_YEAR) %>%  summarise(monit=sum(monit)))       %>% 
                    bind_rows(.id='replicate') %>% group_by(FISCAL_YEAR) %>% summarise(lo=quantile(monit, .025), hi=quantile(monit, .975), monit=mean(monit))
  )  
