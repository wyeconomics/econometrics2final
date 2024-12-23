source('codes/config.R')
## main code with emds with only one stations


## CSDID on employment----
csdid <- att_gt('log_emp_total', tname = 'year', gname = 'time_notyet', idname = 'emd',
                control_group = 'notyettreated', data = df2, allow_unbalanced_panel = T)
csdid_upper <- att_gt('log_emp_total', tname = 'year', gname = 'time_notyet', idname = 'emd',
                      control_group = 'notyettreated', data = df2_upper, allow_unbalanced_panel = T)
csdid_lower <- att_gt('log_emp_total', tname = 'year', gname = 'time_notyet', idname = 'emd',
                      control_group = 'notyettreated', data = df2_lower, allow_unbalanced_panel = T)
csdid

aggte(csdid, type = 'simple', na.rm = T)
aggte(csdid_upper, type = 'simple', na.rm = T)
aggte(csdid_lower, type = 'simple', na.rm = T)

t1 <- rbind(aggte(csdid, type = 'simple', na.rm = T) %>% tidy(),
            aggte(csdid_upper, type = 'simple', na.rm = T) %>% tidy(),
            aggte(csdid_lower, type = 'simple', na.rm = T) %>% tidy()) %>% 
  mutate(type = c('total', 'upper', 'lower'))
writeLines(kableExtra::kbl(t1, booktabs = T), 'doc/t1.tex')


ggdid(aggte(csdid, type= 'dynamic', na.rm = T, min_e = -10, max_e = 15))
# ggsave('figures/f3(csdid_total).png')
ggdid(aggte(csdid_upper, type= 'dynamic', na.rm = T, min_e = -10, max_e = 15))
# ggsave('figures/f4(csdid_upper).png')
ggdid(aggte(csdid_lower, type= 'dynamic', na.rm = T, min_e = -10, max_e = 15))
# ggsave('figures/f5(csdid_lower).png')


## CSDID on no. of business----
csdid <- att_gt('log_n', tname = 'year', gname = 'time_notyet', idname = 'emd',
                control_group = 'notyettreated', data = df2, allow_unbalanced_panel = T)
csdid_upper <- att_gt('log_n', tname = 'year', gname = 'time_notyet', idname = 'emd',
                      control_group = 'notyettreated', data = df2_upper, allow_unbalanced_panel = T)
csdid_lower <- att_gt('log_n', tname = 'year', gname = 'time_notyet', idname = 'emd',
                      control_group = 'notyettreated', data = df2_lower, allow_unbalanced_panel = T)
csdid

aggte(csdid, type = 'simple', na.rm = T)
aggte(csdid_upper, type = 'simple', na.rm = T)
aggte(csdid_lower, type = 'simple', na.rm = T)


t2 <- rbind(aggte(csdid, type = 'simple', na.rm = T) %>% tidy(),
            aggte(csdid_upper, type = 'simple', na.rm = T) %>% tidy(),
            aggte(csdid_lower, type = 'simple', na.rm = T) %>% tidy()) %>% 
  mutate(type = c('total', 'upper', 'lower'))
writeLines(kableExtra::kbl(t1, booktabs = T), 'doc/t2.tex')


ggdid(aggte(csdid, type= 'dynamic', na.rm = T, min_e = -10, max_e = 15))
ggsave('figures/f9(csdid2).png')
ggdid(aggte(csdid_upper, type= 'dynamic', na.rm = T, min_e = -10, max_e = 15))
ggsave('figures/f10(csdid2_upper).png')
ggdid(aggte(csdid_lower, type= 'dynamic', na.rm = T, min_e = -10, max_e = 15))
ggsave('figures/f10(csdid2_lower).png')

## TWFE on employment ----
df2 <- df2 %>% mutate(D = if_else(time_notyet<= year, 1, 0))
df2_upper <- df2_upper %>% mutate(D = if_else(time_notyet<= year, 1, 0))
df2_lower <- df2_lower %>% mutate(D = if_else(time_notyet<= year, 1, 0))


df1
twfe <- feols(log_emp_total ~ D|emd + year, data = df1)
twfe_upper <- feols(log_emp_total ~ D|emd + year, data = df1_upper)
twfe_lower <- feols(log_emp_total ~ D|emd + year, data = df1_lower)


# TWFE event-study designs ----
event_twfe <- feols(log_emp_total ~ i(year-time, ref = -1)|emd + year, data = df1)
event_twfe_upper <- feols(log_emp_total ~ i(year-time, ref = -1)|emd + year, data = df1_upper)
event_twfe_lower <- feols(log_emp_total ~ i(year-time, ref = -1)|emd + year, data = df1_lower)

tidy(event_twfe) %>% 
  mutate(term= as.numeric(str_extract(term, '-*\\d*$'))) %>% 
  filter(term %in% -10:15) %>% 
  mutate(post = if_else(term > 0, 'post', 'pre') %>% factor(levels = c('pre', 'post'))) %>% 
  ggplot() +
  geom_errorbar(aes(x = term, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error, color = post)) + 
  geom_point(aes(x = term, y = estimate, color = post))


tidy(event_twfe_upper) %>% 
  mutate(term= as.numeric(str_extract(term, '-*\\d*$'))) %>% 
  filter(term %in% -10:15) %>% 
  mutate(post = if_else(term > 0, 'post', 'pre') %>% factor(levels = c('pre', 'post'))) %>% 
  ggplot() +
  geom_errorbar(aes(x = term, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error, color = post)) + 
  geom_point(aes(x = term, y = estimate, color = post))


tidy(event_twfe_lower) %>% 
  mutate(term= as.numeric(str_extract(term, '-*\\d*$'))) %>% 
  filter(term %in% -10:15) %>% 
  mutate(post = if_else(term > 0, 'post', 'pre') %>% factor(levels = c('pre', 'post'))) %>% 
  ggplot() +
  geom_errorbar(aes(x = term, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error, color = post)) + 
  geom_point(aes(x = term, y = estimate, color = post))
