library(tidyverse)
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
coins = read_csv('./aggreg_coins.csv')
og_coins = readr::read_csv('./aggreg_coins.csv')
head(coins)
coins = na.omit(coins)
coins$date_time = as.Date(coins$Date)
coins$weekday = weekdays.Date(coins$date_time)
require(Hmisc)
coins$d_i_m=monthDays(as.Date(coins$date_time))
coins %>% group_by(Name, date_time) %>% summarise (open = Open, close = Close)
coins
post = coins#[coins$date_time>'2015-12-31',]
post$day_in_mo = substr(post$date_time,9,10)
post$perc_in_mo_bod = (as.numeric(post$day_in_mo )-1) / as.numeric(post$d_i_m )
post
post$perc_in_mo_eod = as.numeric(post$day_in_mo)  / as.numeric(post$d_i_m)
post

#rollmean{zoo}
cal <- readr::read_csv('./calendar.csv')
cal$date = as.Date(cal$date)
cal

wip <- post
library(scales)
wip$day_ret = (wip$Close / wip$Open)-1

wip$date_time
cal$date

full_df <- merge(wip,cal,by.x='date_time',by.y='date')
full_df %>% ungroup %>% arrange(desc('date_time'),'Name')
sorted_df = arrange(full_df, Name, date_time ,by_group=F)
sorted_df
sorted_df$mo_yr = paste0(substr(sorted_df$date_time,6,7),"-",substr(sorted_df$date_time,1,4))
sorted_df

weekly_ret = group_by(sorted_df, Name, wk_num) %>% summarise(wk_ret_mean=mean(day_ret),wk_ret_sd=sd(day_ret))
weekly_ret

sorted_df
monthly_ret = group_by(sorted_df, Name, mo_yr) %>% summarise(mo_ret_mean=mean(day_ret),mo_ret_sd=sd(day_ret))
monthly_ret

sorted_df$yr = substr(sorted_df$date_time,1,4)
sorted_df$mo = substr(sorted_df$date_time,6,7)


sorted_df$qtr_yr = paste(ifelse(sorted_df$mo %in% c('01','02','03'), '01', ifelse(sorted_df$mo %in% c('04','05','06'), '02',ifelse(sorted_df$mo %in% c('07','08','09'), '03','04'))),'-',sorted_df$yr)

gsub(' ','',sorted_df$qtr_yr)

sorted_df
yearly_ret = group_by(sorted_df, Name, yr) %>% summarise(yr_ret_mean=mean(day_ret),yr_ret_sd=sd(day_ret))
yearly_ret

qtrly_ret = group_by(sorted_df, Name, qtr_yr) %>% summarise(qtr_ret_mean=mean(day_ret),qtr_ret_sd=sd(day_ret))
qtrly_ret

all_cols = sorted_df
all_cols

sorted_df

weekly_ret
sorted_df = left_join(sorted_df, weekly_ret, by=c('Name' = 'Name', 'wk_num' = 'wk_num'))


monthly_ret
sorted_df = left_join(sorted_df, monthly_ret, by=c('Name' = 'Name', 'mo_yr' = 'mo_yr'))


qtrly_ret
sorted_df = left_join(sorted_df,qtrly_ret, by = c('Name' ='Name' , 'qtr_yr' = 'qtr_yr'))

yearly_ret
sorted_df = left_join(sorted_df,yearly_ret, by = c('Name' ='Name' , 'yr' = 'yr'))

# sorted_df = sorted_df %>% rename(wk_ret_mean = norm_wk_ret_mean, wk_ret_sd = norm_wk_ret_sd, mo_ret_mean = norm_mo_ret_mean, mo_ret_sd = norm_mo_ret_sd, qtr_ret_mean = norm_qtr_ret_mean, qtr_ret_sd = norm_qtr_ret_sd, yr_ret_mean = norm_yr_ret_mean, yr_ret_sd = norm_yr_ret_sd)

sorted_df

#weekly_returns_normalized = ( returns - avg_ret_per_wk ) / norm_wk_ret_sd

#Normalizing daily returns to the week, month, quarter, and year

norm_df = sorted_df
# norm_df$day_ret_norm_to_wk = ((norm_df$day_ret - norm_df$wk_ret_mean) / norm_df$wk_ret_sd)
# norm_df$day_ret_norm_to_mo = ((norm_df$day_ret - norm_df$mo_ret_mean) / norm_df$mo_ret_sd)
# norm_df$day_ret_norm_to_qtr = ((norm_df$day_ret - norm_df$qtr_ret_mean) / norm_df$qtr_ret_sd)
# norm_df$day_ret_norm_to_yr = ((norm_df$day_ret - norm_df$yr_ret_mean) / norm_df$yr_ret_sd)
norm_df

norm_df$qtr = substr(norm_df$qtr_yr,1,2)
day_tbl = norm_df %>% group_by(Name, wk_day_n) %>% summarise(day_of_wk_ret =mean(day_ret))
day_tbl
# library(reshape2)
# melted_day = melt(day_tbl,id.vars=c('wk_day_n','Name') ,measure.vars='day_of_wk_ret')
# melted_day
all_Names_daily <- ggplot(day_tbl, aes(x=wk_day_n, y=day_of_wk_ret, col=Name)) + geom_line()
all_Names_daily
# melted %>% group_by(Name) %>% top_n(1,value)
day_tbl
ttl_by_day = day_tbl %>% group_by(wk_day_n) %>% summarise(agg_daily_return = mean(day_of_wk_ret, na.rm=T))
ttl_by_day
agg_daily <- ggplot(ttl_by_day, aes(x=wk_day_n, y=agg_daily_return)) +geom_line()
agg_daily


mo_tbl = norm_df %>% group_by(Name, mo, day_in_mo, qtr) %>% summarise(mo_ret_mean=mean(day_ret))
mo_tbl
monthly_tbl = mo_tbl %>% group_by(Name, day_in_mo) %>% summarise(mo_ret_mean = mean(mo_ret_mean))
monthly_tbl

monthly_coin = mo_tbl %>% group_by(Name, mo, day_in_mo, qtr) %>% summarise(month_avg_return = mean(mo_ret_mean))
monthly_coin
month_min = monthly_coin %>% group_by(Name, mo)%>%summarise(min = min(month_avg_return))
month_min
month_max = monthly_coin %>% group_by(Name, mo)%>%summarise(max = max(month_avg_return))
month_max

m  = norm_df %>% group_by(Name, mo, day_in_mo) %>% summarise(mo_ret_mean=mean(day_ret))
m1 <- m %>% group_by(Name, mo) %>% summarise(max = max(mo_ret_mean), min = min(mo_ret_mean))
m2 <- left_join(monthly_coin , m1, left_on=c(Name, mo), right_on=c(Name, mo))
m2$max_day = m2$month_avg_return==m2$max
m2$min_day = m2$month_avg_return==m2$min
m2
max_tbl = filter(m2, max_day==T)
max_tbl
min_tbl = filter(m2, min_day==T)
max_tbl%>% select(Name,mo,day_in_mo)
min_tbl


# library(reshape2)
# melted_mo = melt(norm_mo_tbl,id.vars=c('day_in_mo','Name') ,measure.vars='norm_mo')
# melted_mo
all_Names_monthly <- ggplot(mo_tbl,aes(x=day_in_mo, y=mo_ret_mean, group=Name)) + geom_line(aes(colour=Name))
all_Names_monthly + facet_wrap(~mo)

# all_Names_monthly
# melted_mo %>% group_by(Name) %>% top_n(1,value)

ttl_by_mo = mo_tbl %>% group_by(day_in_mo, mo) %>% summarise(agg_monthly_return = mean(mo_ret_mean, na.rm=T))
ttl_by_mo_by_day =mo_tbl %>% group_by(day_in_mo) %>% summarise(agg_monthly_return = mean(mo_ret_mean, na.rm=T))

agg_monthly <- ggplot(data = ttl_by_mo_by_day, aes(x=day_in_mo, y=agg_monthly_return)) +geom_line(aes(group = 1))
agg_monthly

norm_df$mo_day = substr(norm_df$date_time,6,10)
norm_df$first_qtr_day =paste(norm_df$yr,'-', ifelse(substr(norm_df$qtr_yr,1,2)=='01','01-01',ifelse(substr(norm_df$qtr_yr,1,2)=='02','04-01',ifelse(substr(norm_df$qtr_yr,1,2)=='03','07-01','10-01'))))
norm_df$first_qtr_day = gsub(" ", "",norm_df$first_qtr_day)
norm_df$qtr_yr = gsub(' ', '' , norm_df$qtr_yr)

as.Date(norm_df$first_qtr_day)
as.Date(norm_df$date_time)
norm_df$day_into_qtr = as.Date(norm_df$date_time) - as.Date(norm_df$first_qtr_day) +1
norm_df$qtr = substr(norm_df$qtr_yr,1,2)

qtr_tbl = norm_df %>% group_by(Name, qtr, day_into_qtr) %>% summarise(qtr_ret=mean(day_ret))

na.omit(qtr_tbl)
qtr_tbl

quarterly_tbl = qtr_tbl %>% group_by(Name, day_into_qtr) %>% summarise (qtr_ret_mean = mean(qtr_ret))

# library(reshape2)
# melted_qtr = melt(norm_qtr_tbl,id.vars=c('day_into_qtr','Name') ,measure.vars='norm_qtr')
# melted_qtr
all_Names_quarterly <- ggplot(qtr_tbl,aes(x=day_into_qtr, y=qtr_ret, group=Name)) + geom_line(aes(colour=Name))

all_Names_quarterly + facet_grid(~qtr)
# melted_qtr %>% group_by(Name) %>% top_n(1,value)

ttl_by_qtr = qtr_tbl %>% group_by(qtr, day_into_qtr) %>% summarise(agg_quarterly_return = mean(qtr_ret, na.rm=T))
ttl_by_qtr
agg_qtrly <- ggplot(data = ttl_by_qtr, aes(x=day_into_qtr, y=agg_quarterly_return)) +geom_line(aes(group = 1))
agg_qtrly + facet_wrap(~qtr)

norm_df
norm_df$first_yr_day = paste0(norm_df$yr,'-01-01')
norm_df$day_into_yr = as.Date(norm_df$date_time) - as.Date(norm_df$first_yr_day) +1

yr_tbl = norm_df %>% group_by(Name, day_into_yr) %>% summarise(yr_ret=mean(day_ret))
yr_tbl
na.omit(yr_tbl)
yr_tbl
# library(reshape2)
# melted_yr = melt(norm_yr_tbl,id.vars=c('day_into_yr','Name') ,measure.vars='norm_yr')
# melted_yr
all_Names_yearly <- ggplot(yr_tbl,aes(x=day_into_yr, y=yr_ret, group=Name)) + geom_line(aes(colour=Name))
all_Names_yearly

# melted_yr %>% group_by(Name) %>% top_n(1,value)

ttl_by_yr = yr_tbl %>% group_by(day_into_yr) %>% summarise(agg_yearly_return = mean(yr_ret, na.rm=T))
ttl_by_yr
agg_yrly <- ggplot(data = ttl_by_yr, aes(x=day_into_yr, y=agg_yearly_return)) +geom_line(aes(group = 1))
agg_yrly

library(heatmaply)
library(dplyr)
library(tidyr)
library(corrplot)
nasdaq = read_csv('./NASDAQ.csv')
nyse = read_csv('./NYSE.csv')
nasdaq
nasdaq$nasdaq_day_ret = nasdaq$Close / nasdaq$Open - 1
nyse$nyse_day_ret = nasdaq$Close / nasdaq$Open - 1
nyse$Name = 'NYSE'
nasdaq$Name = "NASDAQ"
nyse_tbl = nyse %>% select(Name, Date, nyse_day_ret)
nasdaq_tbl = nasdaq %>% select(Name, Date, nasdaq_day_ret)
nyse_tbl = nyse_tbl %>% rename(date_time = Date, day_ret = nyse_day_ret)
nyse_tbl
nasdaq_tbl = nasdaq_tbl %>% rename(date_time = Date, day_ret = nasdaq_day_ret)
nasdaq_tbl
norm_df


norm_df = bind_rows(norm_df,nyse_tbl)
norm_df = bind_rows(norm_df,nasdaq_tbl)
norm_df


norm_df
new_df = norm_df %>% dplyr::select(date_time, Name, day_ret)
new_df
new_mat = pivot_wider(new_df, names_from = 'Name', values_from = 'day_ret')
new_mat$date_time = as.numeric(new_mat$date_time)
new_mat
corr_mat = cor(new_mat, use="pairwise.complete.obs")
c1 <- corrplot(corr_mat, method='number', type='lower')
c1


norm_df
hi_in_mo = norm_df %>% group_by(Name, yr, mo) %>% summarise(highest_day_mo=max(day_ret),lowest_day_mo = min(day_ret))

hi_in_qtr = norm_df %>% group_by(Name, yr, qtr) %>% summarise(highest_day_qtr=max(day_ret),lowest_day_qtr = min(day_ret))

hi_in_yr = norm_df %>% group_by(Name, yr) %>% summarise(highest_day_yr=max(day_ret),lowest_day_yr = min(day_ret))

###
dow_max = day_tbl %>% group_by(Name) %>% summarise(max_dow = max(day_of_wk_ret))
dow_max
dow_min = day_tbl %>% group_by(Name) %>% summarise(min_dow = min(day_of_wk_ret))
dow_min
agg_max_min = left_join(day_tbl,dow_max, left_on ='Name',right_on='Name')
agg_max_min
agg_max_min = left_join(agg_max_min,dow_min, left_on ='Name',right_on='Name')

agg_max_min$max_day = agg_max_min$max_dow == agg_max_min$day_of_wk_ret
agg_max_min$min_day = agg_max_min$min_dow == agg_max_min$day_of_wk_ret

final_max_min = agg_max_min %>% select(Name, wk_day_n, day_of_wk_ret, max_dow,
                                       max_day, min_dow, min_day) %>%
                                     filter(min_day==T | max_day==T)
fin_day = final_max_min %>% select(Name, wk_day_n, day_of_wk_ret, max_day,
                                   min_day)
fin_day
###
monthly_max = monthly_tbl %>% group_by(Name)%>% summarise(max=max(mo_ret_mean))
monthly_max
monthly_min = monthly_tbl %>% group_by(Name)%>% summarise(min=min(mo_ret_mean))
monthly_min

mo_max_min = left_join(monthly_tbl, monthly_max, on='Name')
mo_max_min = left_join(mo_max_min, monthly_min, on='Name')
mo_max_min$max_day = mo_max_min$max == mo_max_min$mo_ret_mean
mo_max_min$min_day = mo_max_min$min == mo_max_min$mo_ret_mean
mo_max_min

final_mo = mo_max_min %>% select(Name, day_in_mo, mo_ret_mean, max_day, min_day) %>% filter(max_day==T | min_day==T)
final_mo
###
q_tbl = quarterly_tbl %>% group_by(Name) %>% summarise(max=max(qtr_ret_mean), min = min(qtr_ret_mean))
q_tbl
q_wip = left_join(quarterly_tbl,q_tbl, on='Name')
q_wip$q_max = q_wip$qtr_ret_mean==q_wip$max
q_wip$q_min = q_wip$qtr_ret_mean==q_wip$min
qtr_fin = q_wip %>% select(Name, day_into_qtr, qtr_ret_mean, q_max, q_min) %>% filter(q_max==T | q_min==T)
qtr_fin
###
yr_wip = yr_tbl %>% group_by(Name) %>% summarise(max =max(yr_ret), min = min(yr_ret))

yr_wip2 = left_join(yr_tbl, yr_wip, by='Name')
yr_wip2$max_day_y = yr_wip2$max == yr_wip2$yr_ret
yr_wip2$min_day_y = yr_wip2$min == yr_wip2$yr_ret

yr_fin = yr_wip2 %>% select(Name, day_into_yr, yr_ret, max_day_y, min_day_y)%>%filter(max_day_y==T | min_day_y==T)

yr_fin
###
coins

