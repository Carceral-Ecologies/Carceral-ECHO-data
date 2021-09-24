### TODO dump data, move to notebook

# EPA Visits decreasing over time
visits %>% 
  count(FISCAL_YEAR, SANITARY_SURVEY) %>%  
  ggplot() + aes(x=FISCAL_YEAR, y = n, color=SANITARY_SURVEY) + 
  geom_point(size=3) + 
  geom_smooth(linetype='dashed') +
  scale_x_continuous(minor_breaks=2009:2021, breaks=seq(2010, 2020, 5), limits = c(2010, 2022)) + 
  scale_shape_discrete(guide=NULL) + 
  ggtitle("Site Visits per fiscal year")



fac_years_visited <- 
  
  sqldf("
      
with      hifld_visits as (

  select fiscal_year, HIFLD_FACILITYID, max(SANITARY_SURVEY) as SANITARY_SURVEY
  from visits 
  join frs_hifld  on (visits.registry_id = frs_hifld.FRS_ID)
  group by 1,2 
),
fac_years as (
  select years.fiscal_year, 
  facilities.*
  
  from facilities, years
)
select fac_years.*, 
  hifld_visits.HIFLD_FACILITYID is not null as visited,
  hifld_visits.SANITARY_SURVEY
  
from fac_years
left join hifld_visits on (hifld_visits.fiscal_year = fac_years.fiscal_year and hifld_visits.HIFLD_FACILITYID = fac_years.HIFLD_FACILITYID) 
")



count(fac_years_visited, fiscal_year, EPA_Region, visited) %>% subset(visited==1) %>%  ggplot() + aes(x=fiscal_year, y=n, color=EPA_Region) + geom_line()

fac_years_visited %>% ggplot() + 
  aes(x=fiscal_year - as.numeric(DOJ_YEAR_BUILT_2005), y=visited, color=EPA_Region) + geom_smooth() + xlab('age of building') + ylab("prob of visit")






df <- sqldf("
with viol_per as (
  select PWSID, FISCAL_YEAR, 1 as violation_id, MAX(health_based) as health_based, MAX(acute_health_based) as acute_health_based
  from violations group by 1,2,3

),
visits_per as (
  select distinct pwsid, fiscal_year
  from visits
)


select fiscal_year, facilities.*, violation_id is not null as violation, health_based, acute_health_based
from facilities
join frs_hifld using (HIFLD_FACILITYID)
join pws_reg on (FRS_ID = REGISTRY_ID)
join visits_per using (PWSID)
left join viol_per using (PWSID, FISCAL_YEAR)
      
      
")

df %>% group_by(fiscal_year, EPA_Region, violation) %>% summarise(n=length(violation)) %>% 
  ggplot() + aes(x = fiscal_year, y=n, fill=ifelse(violation, "No Violation", "Violation")) + 
  geom_col() + facet_wrap(~EPA_Region) + scale_fill_discrete("") + ggtitle("Visits over time (by EPA Region")


df %>% group_by(fiscal_year, EPA_Region, violation) %>% summarise(n=sum(as.numeric(HIFLD_POPULATION_2020), na.rm=TRUE)) %>% 
  ggplot() + aes(x = fiscal_year, y=n, fill=ifelse(violation, "No Violation", "Violation")) + 
  geom_col() + facet_wrap(~EPA_Region) + scale_fill_discrete("") + ggtitle("Population of Visited Facilities")

# 
# > sqldf("with t as (select hifld_facilityid, count(distinct pwsid) as n from frs_hifld join pws_reg on (pws_reg.registry_id = frs_hifld.frs_id) group by 1) select * from t join frs_hifld using (hifld_facilityid) join pws_reg on (frs_id = registry_id) where t.n > 1 -- select n, count(1) from t group by n")
# hifld_facilityid n       FRS_ID     PWSID  REGISTRY_ID
# 1         10002476 2 110005364658 WA5322330 110005364658
# 2         10002476 2 110005364658 WA5327510 110005364658
# 3         10002287 2 110021747822 NY6030008 110021747822
# 4         10002287 2 110021747822 NY6030009 110021747822
# 5         10000994 2 110060258858 WI7010058 110060258858
# 6         10000994 2 110060258858 WI7010105 110060258858
# 7         10001183 2 110068077191 ID4010141 110068077191
# 8         10001183 2 110068077191 ID4010240 110068077191



###
glm(violation~EPA_Region, df, family=binomial) %>% summary





str(violations)

violation_fix <- 
  glm(cbind(1, END_YEAR-BEGIN_YEAR) ~ ACUTE_HEALTH_BASED + HEALTH_BASED  + PUBLIC_NOTIF_OTHER, 
      violations, 
      family=binomial())


summary(violation_fix)


require(lme4)


violation_fix_lme <- 
  glmer(cbind(1, END_YEAR-BEGIN_YEAR) ~ 
          ACUTE_HEALTH_BASED + HEALTH_BASED  + PUBLIC_NOTIF_OTHER + I(BEGIN_YEAR - 2010) + (1 | EPA_REGION / STATE), 
        inner_join(violations, pws), subset = sample(length(BEGIN_YEAR)) < 6000, 
        family=binomial())
