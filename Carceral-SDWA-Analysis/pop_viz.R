##
# use df from model
require(ggplot2)
require(dplyr)
require(sqldf)



df %>% group_by(FISCAL_YEAR, i) %>% 
  summarise(pop = sum(POPULATION_SERVED_COUNT, na.rm = TRUE)) %>% 
  group_by(FISCAL_YEAR) %>% 
  summarise(pop_compliant = weighted.mean(i, pop, na.rm = TRUE)) %>% 
  ggplot() + aes(x=FISCAL_YEAR, y= pop_compliant, group=1) + ggtitle("% of population on compliant PWS (prisons)") + geom_point() + geom_line()
  


pws_comp_df <- sqldf("
      
with v as (
   select PWSID, FISCAL_YEAR, 0 as i
   from violations
   where ACUTE_HEALTH_BASED = 'Y' or HEALTH_BASED = 'Y' or MONITORING_REPORTING = 'Y' or PUBLIC_NOTIF_OTHER = 'Y'
   group by PWSID, FISCAL_YEAR
),
years as (
  select distinct FISCAL_YEAR from violations 
),
pws as (
  select distinct PWSID from violations
),
combo as (
  select FISCAL_YEAR, PWSID
  from years, pws
),
combo2 as (
  select FISCAL_YEAR, PWSID, POPULATION_SERVED_COUNT as pop, coalesce(i, 1) as i
  from combo left join pws_pop using (FISCAL_YEAR, PWSID)
  left join v using(FISCAL_YEAR, PWSID)
)
select FISCAL_YEAR, i, sum(pop) as pop
from combo2
group by 1,2
      
")

pws_comp_df %>% 
group_by(FISCAL_YEAR) %>% 
  summarise(pop_compliant = weighted.mean(i, pop, na.rm = TRUE)) %>% 
  ggplot() + aes(x=FISCAL_YEAR, y= pop_compliant, group=1) + ggtitle("% of population on compliant PWS (all PWS)") + geom_point() + geom_line()
