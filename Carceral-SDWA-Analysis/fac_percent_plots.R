# Note - do not use na-filled version

require(reshape2)
df %>% 
  select(FAC_PERCENT_MINORITY, MONITORING_V, HEALTH_V, ACUTE_V) %>% 
  melt("FAC_PERCENT_MINORITY", variable.name="type", value.name = "violations") %>% 
ggplot() + aes(x=FAC_PERCENT_MINORITY, y=violations, group=type, fill=type, color=type) + 
  stat_smooth( alpha=.5) +
  scale_y_log10("Violations (log scale)") +
  scale_color_discrete()
  


require(reshape2)
df %>% 
  select(FAC_PERCENT_MINORITY, EPA_Region, MONITORING_V, HEALTH_V, ACUTE_V) %>% 
  melt(c("FAC_PERCENT_MINORITY", "EPA_Region"), variable.name="type", value.name = "violations") %>% 
  ggplot() + aes(x=FAC_PERCENT_MINORITY, y=violations, group=type, fill=type, color=type) + 
  stat_smooth( alpha=.5) +
  #scale_y_log10("Violations (log scale)") +
  scale_color_discrete() + facet_wrap(~EPA_Region)


require(reshape2)
df %>% mutate(min_estimate=FAC_PERCENT_MINORITY/100 * HIFLD_POPULATION_2020) %>% 
  select(min_estimate, EPA_Region, MONITORING_V, HEALTH_V, ACUTE_V) %>% 
  melt(c("min_estimate", "EPA_Region"), variable.name="type", value.name = "violations") %>% 
  ggplot() + aes(x=min_estimate, y=violations, group=type, fill=type, color=type) + 
  stat_smooth( alpha=.5) +
  scale_x_log10("FAC_PERCENT_MINORITY * HIFLD_POPULATION_2020 (log scale)")+
  #scale_y_log10("Violations (log scale)") +
  scale_color_discrete() 
  #facet_wrap(~EPA_Region)
