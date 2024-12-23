source('codes/config.R')
rm(df1_commerce, df1_commerce_1, df1_commerce_l, df1_commerce_u)

sum.table <- summary(df1 %>% select(year, employment = emp_total, `log(employment)` = log_emp_total,
                       `treated year` = time, `(upper=1)` = level))

df1 %>% 
  select(year, employment = emp_total, `log(employment)` = log_emp_total,
         `treated year` = time, `(upper=1)` = level) %>% 
  summarise(across(everything(), sd, na.rm = TRUE))
print(xtable::xtable(as.table(sum.table), type = "latex"), file = "table.tex")

## CSDID on employment----
csdid <- att_gt('log_emp_total', tname = 'year', gname = 'time_notyet', idname = 'emd',
                control_group = 'notyettreated', data = df1, allow_unbalanced_panel = T)
csdid_upper <- att_gt('log_emp_total', tname = 'year', gname = 'time_notyet', idname = 'emd',
                      control_group = 'notyettreated', data = df1_upper, allow_unbalanced_panel = T)
csdid_lower <- att_gt('log_emp_total', tname = 'year', gname = 'time_notyet', idname = 'emd',
                      control_group = 'notyettreated', data = df1_lower, allow_unbalanced_panel = T)

aggte(csdid, type = 'simple', na.rm = T)
aggte(csdid_upper, type = 'simple', na.rm = T)
aggte(csdid_lower, type = 'simple', na.rm = T)

t1 <- rbind(aggte(csdid, type = 'simple', na.rm = T) %>% tidy(),
            aggte(csdid_upper, type = 'simple', na.rm = T) %>% tidy(),
            aggte(csdid_lower, type = 'simple', na.rm = T) %>% tidy()) %>% 
  mutate(type = c('total', 'upper', 'lower'))
tab<-xtable::xtable(t1, caption= "Callaway and Sant'Anna(2020)", 
            align=c("|c","|c","|c","|c","|c","|c","|c","|c|"))
print(tab,file="doc/t1.tex",append=F,table.placement = "h",
      caption.placement="bottom", hline.after=seq(from=-1,to=nrow(tab),by=1))

# texreg(l, file = 'tex/매출 regression(8월 15일부, 폐쇄 PC).tex',
#        stars = c(0.01, 0.05, 0.1), 
#        caption = 'Post-Covid Policy Effects(Revenue)', 
#        custom.header = list('Benchmark Specification' = 1:3, 'With Covariates' = 4:6),
#        digits = 4,
#        custom.note = 'Covariates include r&d cost.(all logged).',
#        include.model.names = FALSE,
#        include.adjrs = FALSE,
#        fontsize = 'tiny')

ggdid(aggte(csdid, type= 'dynamic', na.rm = T, min_e = -10, max_e = 15, alp = 0.1))
ggsave('figures/f3(csdid_total).png')
ggdid(aggte(csdid_upper, type= 'dynamic', na.rm = T, min_e = -10, max_e = 15, alp = 0.1))
ggsave('figures/f4(csdid_upper).png')
ggdid(aggte(csdid_lower, type= 'dynamic', na.rm = T, min_e = -10, max_e = 15, alp = 0.1))
ggsave('figures/f5(csdid_lower).png')


## CSDID on no. of business----
csdid <- att_gt('log_n', tname = 'year', gname = 'time_notyet', idname = 'emd',
                control_group = 'notyettreated', data = df1, allow_unbalanced_panel = T)
csdid_upper <- att_gt('log_n', tname = 'year', gname = 'time_notyet', idname = 'emd',
                      control_group = 'notyettreated', data = df1_upper, allow_unbalanced_panel = T)
csdid_lower <- att_gt('log_n', tname = 'year', gname = 'time_notyet', idname = 'emd',
                      control_group = 'notyettreated', data = df1_lower, allow_unbalanced_panel = T)
csdid

aggte(csdid, type = 'simple', na.rm = T)
aggte(csdid_upper, type = 'simple', na.rm = T)
aggte(csdid_lower, type = 'simple', na.rm = T)


ggdid(aggte(csdid, type= 'dynamic', na.rm = T, min_e = -10, max_e = 15))
ggsave('figures/f6(csdid_no_firms).png')
ggdid(aggte(csdid_upper, type= 'dynamic', na.rm = T, min_e = -10, max_e = 15))
ggsave('figures/f7(csdid_upper_no_firms).png')
ggdid(aggte(csdid_lower, type= 'dynamic', na.rm = T, min_e = -10, max_e = 15))
ggsave('figures/f8(csdid_lower_no_firms).png')

## TWFE on employment ----
df1 <- df1 %>% mutate(D = if_else(time_notyet<= year, 1, 0))
df1_upper <- df1_upper %>% mutate(D = if_else(time_notyet<= year, 1, 0))
df1_lower <- df1_lower %>% mutate(D = if_else(time_notyet<= year, 1, 0))


df1


twfe <- feols(log_emp_total ~ D|emd+year, data = df1)
twfe_upper <- feols(log_emp_total ~ D|emd+year, data = df1_upper)
twfe_lower <- feols(log_emp_total ~ D|emd+year, data = df1_lower)

screenreg(l = list(twfe, twfe_upper, twfe_lower), 
          custom.coef.names = c('ATT'),
          # custom.header = list('Two-Way Fixed Effects' = 1:3),
       stars = c(0.01, 0.05, 0.1), digits = 3, 
       custom.model.names = c('Total', 'Upper Stations', 'Under Stations'),
       include.proj = F) 

texreg(l = list(twfe, twfe_upper, twfe_lower), 'doc/t2.tex', 
       custom.coef.names =  c('ATT'),
       # custom.header = list('Two-Way Fixed Effects' = 1:3),
       stars = c(0.01, 0.05, 0.1), digits = 3, custom.model.names = c('Total', 'Upper Stations', 'Under Stations'),
       include.proj = F,
       caption = 'Two-Way Fixed Effects'
       )

# TWFE event-study designs ----
df1 <- df1 %>% mutate(
  Dm10 = if_else(year - time_notyet == -10, 1, 0),
  Dm9 = if_else(year - time_notyet == -9, 1, 0),
  Dm8 = if_else(year - time_notyet == -8, 1, 0),
  Dm7 = if_else(year - time_notyet == -7, 1, 0),
  Dm6 = if_else(year - time_notyet == -6, 1, 0),
  Dm5 = if_else(year - time_notyet == -5, 1, 0),
  Dm4 = if_else(year - time_notyet == -4, 1, 0),
  Dm3 = if_else(year - time_notyet == -3, 1, 0),
  Dm2 = if_else(year - time_notyet == -2, 1, 0),
  Dm1 = if_else(year - time_notyet == -1, 1, 0),
  Dp1 = if_else(year - time_notyet == 1, 1, 0),
  Dp2 = if_else(year - time_notyet == 2, 1, 0),
  Dp3 = if_else(year - time_notyet == 3, 1, 0),
  Dp4 = if_else(year - time_notyet == 4, 1, 0),
  Dp5 = if_else(year - time_notyet == 5, 1, 0),
  Dp6 = if_else(year - time_notyet == 6, 1, 0),
  Dp7 = if_else(year - time_notyet == 7, 1, 0),
  Dp8 = if_else(year - time_notyet == 8, 1, 0),
  Dp9 = if_else(year - time_notyet == 9, 1, 0),
  Dp10 = if_else(year - time_notyet == 10, 1, 0),
  Dp11 = if_else(year - time_notyet == 11, 1, 0),
  Dp12 = if_else(year - time_notyet == 12, 1, 0),
  Dp13 = if_else(year - time_notyet == 13, 1, 0),
  Dp14 = if_else(year - time_notyet == 14, 1, 0),
  Dp15 = if_else(year - time_notyet == 15, 1, 0))

df1_upper <- df1_upper %>% mutate(
  Dm10 = if_else(year - time_notyet == -10, 1, 0),
  Dm9 = if_else(year - time_notyet == -9, 1, 0),
  Dm8 = if_else(year - time_notyet == -8, 1, 0),
  Dm7 = if_else(year - time_notyet == -7, 1, 0),
  Dm6 = if_else(year - time_notyet == -6, 1, 0),
  Dm5 = if_else(year - time_notyet == -5, 1, 0),
  Dm4 = if_else(year - time_notyet == -4, 1, 0),
  Dm3 = if_else(year - time_notyet == -3, 1, 0),
  Dm2 = if_else(year - time_notyet == -2, 1, 0),
  Dm1 = if_else(year - time_notyet == -1, 1, 0),
  Dp1 = if_else(year - time_notyet == 1, 1, 0),
  Dp2 = if_else(year - time_notyet == 2, 1, 0),
  Dp3 = if_else(year - time_notyet == 3, 1, 0),
  Dp4 = if_else(year - time_notyet == 4, 1, 0),
  Dp5 = if_else(year - time_notyet == 5, 1, 0),
  Dp6 = if_else(year - time_notyet == 6, 1, 0),
  Dp7 = if_else(year - time_notyet == 7, 1, 0),
  Dp8 = if_else(year - time_notyet == 8, 1, 0),
  Dp9 = if_else(year - time_notyet == 9, 1, 0),
  Dp10 = if_else(year - time_notyet == 10, 1, 0),
  Dp11 = if_else(year - time_notyet == 11, 1, 0),
  Dp12 = if_else(year - time_notyet == 12, 1, 0),
  Dp13 = if_else(year - time_notyet == 13, 1, 0),
  Dp14 = if_else(year - time_notyet == 14, 1, 0),
  Dp15 = if_else(year - time_notyet == 15, 1, 0))

df1_lower <- df1_lower %>% mutate(
  Dm10 = if_else(year - time_notyet == -10, 1, 0),
  Dm9 = if_else(year - time_notyet == -9, 1, 0),
  Dm8 = if_else(year - time_notyet == -8, 1, 0),
  Dm7 = if_else(year - time_notyet == -7, 1, 0),
  Dm6 = if_else(year - time_notyet == -6, 1, 0),
  Dm5 = if_else(year - time_notyet == -5, 1, 0),
  Dm4 = if_else(year - time_notyet == -4, 1, 0),
  Dm3 = if_else(year - time_notyet == -3, 1, 0),
  Dm2 = if_else(year - time_notyet == -2, 1, 0),
  Dm1 = if_else(year - time_notyet == -1, 1, 0),
  Dp1 = if_else(year - time_notyet == 1, 1, 0),
  Dp2 = if_else(year - time_notyet == 2, 1, 0),
  Dp3 = if_else(year - time_notyet == 3, 1, 0),
  Dp4 = if_else(year - time_notyet == 4, 1, 0),
  Dp5 = if_else(year - time_notyet == 5, 1, 0),
  Dp6 = if_else(year - time_notyet == 6, 1, 0),
  Dp7 = if_else(year - time_notyet == 7, 1, 0),
  Dp8 = if_else(year - time_notyet == 8, 1, 0),
  Dp9 = if_else(year - time_notyet == 9, 1, 0),
  Dp10 = if_else(year - time_notyet == 10, 1, 0),
  Dp11 = if_else(year - time_notyet == 11, 1, 0),
  Dp12 = if_else(year - time_notyet == 12, 1, 0),
  Dp13 = if_else(year - time_notyet == 13, 1, 0),
  Dp14 = if_else(year - time_notyet == 14, 1, 0),
  Dp15 = if_else(year - time_notyet == 15, 1, 0))

event_twfe <- feols(log_emp_total ~ Dm10 + Dm9 + Dm8 + Dm7 + Dm6 + Dm5 + Dm4 + Dm3 + Dm2 + Dm1 + D + Dp1 + Dp2 + Dp3 + Dp4 + Dp5 + Dp6 + Dp7 + Dp8 + Dp9 + Dp10 + Dp11 + Dp12 + Dp13 + Dp14 + Dp15|year + emd, vcov = 'iid', data = df1)
event_twfe_upper <- feols(log_emp_total ~ Dm10 + Dm9 + Dm8 + Dm7 + Dm6 + Dm5 + Dm4 + Dm3 + Dm2 + Dm1 + D + Dp1 + Dp2 + Dp3 + Dp4 + Dp5 + Dp6 + Dp7 + Dp8 + Dp9 + Dp10 + Dp11 + Dp12 + Dp13 + Dp14 + Dp15|year + emd, vcov = 'iid', data = df1_upper)
event_twfe_lower <- feols(log_emp_total ~ Dm10 + Dm9 + Dm8 + Dm7 + Dm6 + Dm5 + Dm4 + Dm3 + Dm2 + Dm1 + D + Dp1 + Dp2 + Dp3 + Dp4 + Dp5 + Dp6 + Dp7 + Dp8 + Dp9 + Dp10 + Dp11 + Dp12 + Dp13 + Dp14 + Dp15|year + emd, vcov = 'iid', data = df1_lower)

tidy(event_twfe) %>% 
  mutate(term = str_extract(term, '\\w\\d*$'),
         term = case_when(
           str_detect(term, '^m') ~ as.numeric(str_remove(term, 'm'))*-1,
           str_detect(term, '^p') ~ as.numeric(str_remove(term, 'p'))*1,
           .default = 0
         )) %>% 
  mutate(post = if_else(term > 0, 'post', 'pre') %>% factor(levels = c('pre', 'post'))) %>% 
  ggplot() +
  geom_errorbar(aes(x = term, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error, color = post)) + 
  geom_point(aes(x = term, y = estimate, color = post))


tidy(event_twfe_upper) %>% 
  mutate(term = str_extract(term, '\\w\\d*$'),
         term = case_when(
           str_detect(term, '^m') ~ as.numeric(str_remove(term, 'm'))*-1,
           str_detect(term, '^p') ~ as.numeric(str_remove(term, 'p'))*1,
           .default = 0
         )) %>% 
  mutate(post = if_else(term > 0, 'post', 'pre') %>% factor(levels = c('pre', 'post'))) %>% 
  ggplot() +
  geom_errorbar(aes(x = term, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error, color = post)) + 
  geom_point(aes(x = term, y = estimate, color = post))

tidy(event_twfe_lower) %>% 
  mutate(term = str_extract(term, '\\w\\d*$'),
         term = case_when(
           str_detect(term, '^m') ~ as.numeric(str_remove(term, 'm'))*-1,
           str_detect(term, '^p') ~ as.numeric(str_remove(term, 'p'))*1,
           .default = 0
         )) %>% 
  mutate(post = if_else(term > 0, 'post', 'pre') %>% factor(levels = c('pre', 'post'))) %>% 
  ggplot() +
  geom_errorbar(aes(x = term, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error, color = post)) + 
  geom_point(aes(x = term, y = estimate, color = post))


## dCHDID on employment----
twowayfeweights( df1, 'log_emp_total', 'time', 'year', 'D')
twowayfeweights( df1_upper, 'log_emp_total', 'time', 'year', 'D')

df1 %>% names()
did_multiplegt(mode = 'dyn', df1, 'log_emp_total', 'time', 'year', 'D', graph_off = T)
did_multiplegt(mode = 'dyn', df1_upper, 'log_emp_total', 'time', 'year', 'D', graph_off = T)
did_multiplegt(mode = 'dyn', df1_lower, 'log_emp_total', 'time', 'year', 'D', graph_off = T)

did_multiplegt(mode = 'old', df1, 'log_emp_total', 'time', 'year', 'D')
did_multiplegt(mode = 'dyn', df1_upper, 'log_emp_total', 'time', 'year', 'D', graph_off = T)
did_multiplegt(mode = 'dyn', df1_lower, 'log_emp_total', 'time', 'year', 'D', graph_off = T)

df1
## SA on employment----
