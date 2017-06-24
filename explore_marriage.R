library(dplyr)
library(data.table)
library(magrittr)
library(ggplot2)
library(tidyr)

code_dir <- "MarriageSqueeze/Rcode/"
results_dir <- "MarriageSqueeze/SimResults/"
source(paste0(code_dir,"squeeze.r"))



## Here are what the columns mean in opop -----------------------------------
# pid - id number
# fem - sex indicator
# group - group indicator ?  - not used
# nev - (time of? age of?) next scheduled event 
# dob - date of birth
# mom - person id of mother
# pop - person id of father
# nesibm - id of next oldest sibling through mother
# nesibp - id of next oldest sibling through father
# lborn - id of last born child.
# marid - id of marriage in omar file.
# mstat - marriage status 1=single, 2=divorced, 3=widowed, 4=married.
# dod - date of death or 0 if still alive.
# fmult - fertility multiplier - beta distributed (?) random fertility multiplier to allow for
#         heterogenity. (ranges between 0 ans 2.4ish) 
# fmid - id of first marriage
# 

## omar column meanings -----------------------------------------
# mid - marriage id
# wpid - wife id
# hpid - husband id
# dstart - start date
# dend  - end date
# rend  - reason for end 2=divorce 3=death of one partner
# wprior - wife prior marriage id
# hprior - husband prior marriage

# date system - 

opop <- opop %>% rename(mid=fmid)

tot_pop <- select(opop, -Kdep) %>% left_join(omar)



m_pop <- filter(tot_pop, !fem, birthYr > 1900)
f_pop <- filter(tot_pop, fem==1, birthYr > 1900)

## create a list of the index number of all januaries.
# assume endmo is dec. 2028
endmo
firstmo <- min(m_pop$dob)
firstmo:endmo
januarys <- seq(firstmo,endmo+1,12)

# get population at start of year
get_pop <- function(pop,dob_e,dob_s, month){
  (pop %>% filter(dob < dob_e,dob > dob_s, (dod > month | dod==0)) %>% count())[[1]][1]
}
get_married_pop <- function(pop,dob_e,dob_s, month){
  tot_pop <- pop %>% filter(dob < dob_e,dob > dob_s, (dod > month | dod==0))
  mar_pop <- tot_pop %>% filter(!is.na(dstart),dstart < month, dend > month | dend ==0)
  return(count(mar_pop)[[1]][1])
}

get_total_pop <- function(pop, age_period_df){
  get_pop(pop, age_period_df$dob_e, age_period_df$dob_s, age_period_df$Month)
}

ages <- 10:50
periods <- januarys[29]:endmo

# create grid to aid mapping from dob to age.
ap_df <-expand.grid(Age=ages * 12,Month=januarys) %>% mutate(Age_end = Age+12)
#ap_df <-expand.grid(Age=ages,Month=januarys) %>% mutate(Age_end = Age+12)

ap_df <- ap_df %>% mutate(dob_e = Month - Age, dob_s =Month - Age_end)

# use this grid to count everyone who falls between the start and end date of each year.
# by iterating over it row-wise and applying the functions above.
# (there must be a better / more efficient way of doing this - using cut for example!)
ap_df_m <- ap_df
ap_df_m$Pop_m <- sapply(1:dim(ap_df)[1], function(i) get_total_pop(m_pop, ap_df[i,]))


ap_df_f <- ap_df
ap_df_f$Pop_f <- sapply(1:dim(ap_df)[1], function(i) get_total_pop(f_pop, ap_df[i,]))

ap_df_m <- ap_df_m %>% rename(Age_m=Age)
ap_df_f <- ap_df_f %>% rename(Age_f=Age)

# join males and females so that we have every combination of ages for each 
# observed period, in order to calculate availability of potential partners
ap_joint <- left_join(select(ap_df_m, -dob_e,-dob_s,-Age_end),
                      select(ap_df_f, -dob_e,-dob_s,-Age_end))

# get rid of zero counts
ap_joint <- filter(ap_joint, !(Pop_m==0 & Pop_f==0))

# find age last birthday and year
ap_joint <- ap_joint %>% mutate(Age_m_y = floor(Age_m/12), 
                                Age_f_y = floor(Age_f/12),
                                Year = 1702 + floor(Month/12))

# assume partners average 3-year age differences, 2 year sd. (I can't remember why)
ap_joint <- ap_joint %>% mutate(Age_diff= Age_m_y-Age_f_y,
                                Weight = dnorm(Age_diff,3, 2)) 

# calculate the ratio of pop size to prospective partners for each age and period
parts_m <- ap_joint %>% select(-Age_m, -Age_f, -Month,-Age_diff) %>% 
  group_by(Age_m_y,Year) %>% summarise(Prosp_partners=sum(Pop_f *Weight),
                                       Pop_m=first(Pop_m)) %>%
  mutate(Partner_ratio=Prosp_partners/Pop_m, Sex="Male")
  
parts_f <- ap_joint %>% select(-Age_m, -Age_f, -Month,-Age_diff) %>% 
  group_by(Age_f_y,Year) %>% summarise(Prosp_partners=sum(Pop_m *Weight),
                                       Pop_f=first(Pop_f)) %>%
  mutate(Partner_ratio=Prosp_partners/Pop_f, Sex="Female")



ggplot(parts_m %>% filter(Year==2004), aes(x=Age_m_y, y=Partner_ratio)) +
  geom_line()  

ggplot(parts_f %>% filter(Year==1994), aes(x=Age_f_y, y=Partner_ratio)) +
  geom_line()  


# at low and high ages, we get problems...
ggplot(parts_m, aes(x=Year, y=Age_m_y, fill=Partner_ratio)) +
  geom_tile() + scale_fill_gradient2(midpoint=1)

ggplot(parts_f, aes(x=Year, y=Age_f_y, fill=Partner_ratio)) +
  geom_tile() + scale_fill_gradient2(midpoint=1)

# build into single df

parts_m <- parts_m %>% rename(Age = Age_m_y, Pop = Pop_m)
parts_f <- parts_f %>% rename(Age = Age_f_y, Pop = Pop_f)

partner_df <- rbind(parts_m, parts_f)

partner_df <- partner_df %>% filter(Age > 17 & Age < 46, Year > 1950)

ggplot(filter(partner_df, Sex=="Male"), aes(x=Year, y=Age, fill=Partner_ratio)) +
  geom_tile() + scale_fill_gradient2(midpoint=1, na.value="transparent")

ggplot(filter(partner_df, Sex=="Female"), aes(x=Year, y=Age, fill=Partner_ratio)) +
  geom_tile() + scale_fill_gradient2(midpoint=1, na.value="transparent")

# save  for use in shiny
saveRDS(partner_df, file="Partner_availability/data/partner_df.Rdata")



















