#Comparing states with and without minimum wage increases across a variety of
#PULSE survey outcomes:



mw_pulse_data <- pulse_data %>%
  select(week,est_st,hweight,pweight,tbirth_year,rrace,rhispanic,eeduc,
         expns_dif,childfood,curfoodsuf,evict,tmnthsbhnd,down,
         income,anywork) %>%
  mutate(yes_mw=case_when(
    est_st %in% mw_states$State.FIPS.Code ~ 1,
    !(est_st %in% mw_states$State.FIPS.Code) ~ 0))

#Distribution of INCOME in each group
pop_distribution <- mw_pulse_data %>% group_by(yes_mw,week) %>%
  summarize(
    tot_pop = sum(pweight,na.rm=TRUE)
  )

inc_distribution <-mw_pulse_data %>% group_by(yes_mw,income,week) %>%
  summarize(
    inc_group = sum(pweight,na.rm=TRUE)
  ) %>% left_join(pop_distribution, by=c('yes_mw','week')) %>%
  mutate(inc_share = inc_group / tot_pop) %>% filter(income>0)

#Average distribution
avg_inc_distribution <- inc_distribution %>% group_by(yes_mw, income) %>%
  summarize(
    share = mean(inc_share))

#States with MW bar chart
yes_mw_inc_dist<- avg_inc_distribution %>% filter(yes_mw == 1)
barplot(yes_mw_inc_dist$share, xlab="Income category", main = "States with mw increases")

#States with MW bar chart
no_mw_inc_dist<- avg_inc_distribution %>% filter(yes_mw == 0)
barplot(no_mw_inc_dist$share,xlab="Income category", main = "States with no mw increases")

#share household income below $50,000 per week (income category three and below)
sub_50_share <- inc_distribution %>% mutate(sub50 = case_when(
  income <=3 ~ 1,
  income > 3 ~ 0)) %>% group_by(sub50,week,yes_mw) %>% summarize(
    sub50_share = sum(inc_share)) %>% filter (sub50==1) %>%
  pivot_wider(names_from = yes_mw, values_from = sub50_share) %>%
  rename("No_mw_sub50_share"= "0", "Yes_mw_sub50_share"="1") %>%
  select(-sub50)

plot(sub_50_share$Yes_mw_sub50_share,type = "o", ylim=c(0.2,.45),col = "red", xlab = "Week", ylab = "Share below $50,000")
  lines(sub_50_share$No_mw_sub50_share, type = "o", col = "blue")

#general expenses
#EXPNS_DIF
#Food security
#children noe eating enough 1 often true 2 sometimes true CHILDFOOD
#CURFOODSUF 3 AND 4
#Housing insecurity
#Eviction in next two months EVICT
#Number of months behind on rent or mortgage: TMNTHSBHND
#mental health
#frequency of feeling depressed DOWN
#distribution of poverty INCOME
#1-8 1 less than 25000 3 
#pweight
#employment: ANYWORK 1 yes 

