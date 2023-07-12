#Crosswalking B Zipperer's state minimum wage increase data with 
#Pulse data week numbers

library(xlsx)

#import J Kandra pulse crosswalk
# how do we choose what the cutoff date is?
pulse_cross <- read.csv(here("data/date_xwalk.csv"))%>%
  rbind(c(42,"2022-01-26","2022-02-07"),
        c(43,"2022-03-02","2022-03-14"),
        c(44,"2022-03-30","2022-04-11"),
        c(45,"2022-04-27","2022-05-09"),
        c(46,"2022-06-01","2022-06-13"),
        c(47,"2022-06-29","2022-07-11"),
        c(48,"2022-07-27","2022-08-08"),
        c(49,"2022-09-14","2022-09-28"),
        c(50,"2022-10-05","2022-10-17"),
        c(51,"2022-11-02","2022-11-14"),
        c(52,"2022-12-09","2022-12-19"),
        c(53,"2023-01-04","2023-01-16"),
        c(54,"2023-02-01","2023-02-13"),
        c(55,"2023-03-01","2023-03-13"),
        c(56,"2023-03-29","2023-04-10"),
        c(57,"2023-04-26","2023-05-08"),
        c(58,"2023-06-07","2023-06-19")) %>%
  mutate(st_month=as.integer(format(as.Date(stdate,format="%Y-%m-%d"),"%m")),
         end_month=as.integer(format(as.Date(endate,format="%Y-%m-%d"),"%m")),
         st_year=as.integer(format(as.Date(stdate,format="%Y-%m-%d"),"%Y")),
         end_year=as.integer(format(as.Date(endate,format="%Y-%m-%d"),"%Y"))) %>%
  rename(year=end_year,month=end_month) %>% 
  write_csv(here("data/date_xwalk.csv"))
  

#import mw_state_changes
#need to clean so that it does not include current values of states that did not have an increase

#list of states with no increases in time period that nonetheless are in changes file
#also an issue where some CURRENT values are same as previous increase

no_chng_states <- c("Nebraska","West Virginia")

mw_changes <- read.xlsx(here("data/mw_state_changes.xlsx"),1) %>%
  filter(mw>7.25, year>=2020,year<=2023) %>%
  subset(!(State %in% no_chng_states))%>%
  select(-mw_healthinsurance,-mw_smallbusiness,-mw_youth,-source_2,-source_notes,-source) #-source

#List of states fips with increases in the period
mw_states <- mw_changes %>% distinct(State.FIPS.Code)

#List of state names
mw_states_names <- mw_changes %>% distinct(State)

#merge both: for every increase: need pulse week and state it took place in
mw_crosswalk <- merge(mw_changes, pulse_cross, by = c("year","month"),all.x=TRUE)


