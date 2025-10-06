library(tidyverse)
library(dplyr)


# global+5个sdi地区+21个地区的肺癌吸烟死亡标准化率EAPC -------------------------------------


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病死亡数据\\global+5个sdi地区+21个地区")

data <- read.csv("global+5个sdi地区+21个地区的肺癌吸烟死亡标准化率.csv",header = T)

data<-data[,-1]

colnames(data)
data <- data %>% mutate(location=location_name)


colnames(data)

unique(data$measure)
unique(data$location)
#EAPC 是通过回归模型估算出的指标在一段时间内的年均变化百分比，是对变化率的平均估计值。
EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Both") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)


head(EAPC) 

a <- EAPC %>% filter(location=='Global')
head(a)

a$y <- log(a$val)  #把val的数据进行对数转换，变成变量y
head(a$y)


#构建方程拟合
mod_simp_reg<-lm(y~year,data=a)
summary(mod_simp_reg)

summary(mod_simp_reg)[["coefficients"]] #提取Coefficients

summary(mod_simp_reg)[["coefficients"]][2,1] ##提取year的斜率

summary(mod_simp_reg)[["coefficients"]][2,2] #提取year的斜率的标准误

## 率的平均变化???  #拟合直线,即:Y=b+aX,其中,Y=Lg(死亡率),X为年份,则EAPC=(e的a次方-1)*100，得到的EAPC是一个百分率
(exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100  #e的a次方为exp(summary(mod_simp_reg)[["coefficients"]][2,1])




unique(data$measure)
## 可信区间 mean+-1.96*se
(exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

(exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

#建立循环语句计算每个国家的EAPC

EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Both") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)

EAPC_cal <- data.frame(location=unique(EAPC$location),
                       EAPC=rep(0,times=length(unique(EAPC$location))),
                       LCI=rep(0,times=length(unique(EAPC$location))),  #EAPC的95%下限
                       UCI=rep(0,times=length(unique(EAPC$location)))) #EAPC的95%上限

#通过循环语句计算每个国家的EAPC和95%的上下限

for (i in 1:length(unique(EAPC$location))){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,3] <- low
  EAPC_cal[i,4] <- high}


#保留2位小数
EAPC_cal <- EAPC_cal %>% mutate(EAPC=round(EAPC,2),
                                LCI=round(LCI,2),
                                UCI=round(UCI,2))


#把EAPC和95%的置信区间连接起来
EAPC_cal <- EAPC_cal %>% mutate(EAPC_CI = paste(EAPC, LCI,sep = '\n(')) %>% 
  mutate(EAPC_CI = paste(EAPC_CI, UCI,sep = ' to ')) %>% 
  mutate(EAPC_CI = paste0(EAPC_CI, ')'))
head(EAPC_cal)


location_order <- c("Global","High SDI" ,"High-middle SDI" , "Middle SDI","Low-middle SDI" ,"Low SDI"  
                    
                    ,"Central Asia"  ,    "Central Europe"   , "Eastern Europe"  ,  "Australasia"  
                    ,"High-income Asia Pacific"  ,"High-income North America","Southern Latin America" 
                    ,"Western Europe", "Andean Latin America"  ,"Caribbean" ,"Central Latin America", "Tropical Latin America","North Africa and Middle East"
                    ,"South Asia" ,"East Asia",  "Oceania","Southeast Asia" ,  "Central Sub-Saharan Africa"                         
                    , "Eastern Sub-Saharan Africa"     ,   "Southern Sub-Saharan Africa"   , "Western Sub-Saharan Africa" )

colnames(EAPC_cal)

EAPC_cal$location <- factor(EAPC_cal$location,
                        levels = location_order)


EAPC_cal <- EAPC_cal[order(EAPC_cal$location), ]


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\EAPC")

write.csv(EAPC_cal ,"global+5个sdi地区+21个地区的肺癌吸烟死亡标准化率EAPC.csv",row.names = F)









# global+5个sdi地区+21个地区的肺癌二手烟死亡标准化率EAPC ------------------------------------


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病死亡数据\\global+5个sdi地区+21个地区")

data <- read.csv("global+5个sdi地区+21个地区的肺癌二手烟死亡标准化率.csv",header = T)

data<-data[,-1]

colnames(data)
data <- data %>% mutate(location=location_name)


colnames(data)

unique(data$measure)

#EAPC 是通过回归模型估算出的指标在一段时间内的年均变化百分比，是对变化率的平均估计值。
EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Both") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)


head(EAPC) 

a <- EAPC %>% filter(location=='Global')
head(a)

a$y <- log(a$val)  #把val的数据进行对数转换，变成变量y
head(a$y)


#构建方程拟合
mod_simp_reg<-lm(y~year,data=a)
summary(mod_simp_reg)

summary(mod_simp_reg)[["coefficients"]] #提取Coefficients

summary(mod_simp_reg)[["coefficients"]][2,1] ##提取year的斜率

summary(mod_simp_reg)[["coefficients"]][2,2] #提取year的斜率的标准误

## 率的平均变化???  #拟合直线,即:Y=b+aX,其中,Y=Lg(死亡率),X为年份,则EAPC=(e的a次方-1)*100，得到的EAPC是一个百分率
(exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100  #e的a次方为exp(summary(mod_simp_reg)[["coefficients"]][2,1])




unique(data$measure)
## 可信区间 mean+-1.96*se
(exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

(exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

#建立循环语句计算每个国家的EAPC

EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Both") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)

EAPC_cal <- data.frame(location=unique(EAPC$location),
                       EAPC=rep(0,times=length(unique(EAPC$location))),
                       LCI=rep(0,times=length(unique(EAPC$location))),  #EAPC的95%下限
                       UCI=rep(0,times=length(unique(EAPC$location)))) #EAPC的95%上限

#通过循环语句计算每个国家的EAPC和95%的上下限

for (i in 1:length(unique(EAPC$location))){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,3] <- low
  EAPC_cal[i,4] <- high}


#保留2位小数
EAPC_cal <- EAPC_cal %>% mutate(EAPC=round(EAPC,2),
                                LCI=round(LCI,2),
                                UCI=round(UCI,2))


#把EAPC和95%的置信区间连接起来
EAPC_cal <- EAPC_cal %>% mutate(EAPC_CI = paste(EAPC, LCI,sep = '\n(')) %>% 
  mutate(EAPC_CI = paste(EAPC_CI, UCI,sep = ' to ')) %>% 
  mutate(EAPC_CI = paste0(EAPC_CI, ')'))
head(EAPC_cal)


location_order <- c("Global","High SDI" ,"High-middle SDI" , "Middle SDI","Low-middle SDI" ,"Low SDI"  
                    
                    ,"Central Asia"  ,    "Central Europe"   , "Eastern Europe"  ,  "Australasia"  
                    ,"High-income Asia Pacific"  ,"High-income North America","Southern Latin America" 
                    ,"Western Europe", "Andean Latin America"  ,"Caribbean" ,"Central Latin America", "Tropical Latin America","North Africa and Middle East"
                    ,"South Asia" ,"East Asia",  "Oceania","Southeast Asia" ,  "Central Sub-Saharan Africa"                         
                    , "Eastern Sub-Saharan Africa"     ,   "Southern Sub-Saharan Africa"   , "Western Sub-Saharan Africa" )

colnames(EAPC_cal)

EAPC_cal$location <- factor(EAPC_cal$location,
                            levels = location_order)


EAPC_cal <- EAPC_cal[order(EAPC_cal$location), ]


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\EAPC")

write.csv(EAPC_cal ,"global+5个sdi地区+21个地区的肺癌二手烟死亡标准化率EAPC.csv",row.names = F)







# global+5个sdi地区+21个地区的肺癌吸烟dalys标准化率 EAPC ---------------------------------


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病DALYS数据\\global+5个sdi地区+21个地区")

data <- read.csv("global+5个sdi地区+21个地区的肺癌吸烟dalys标准化率 .csv",header = T)

data<-data[,-1]

colnames(data)
data <- data %>% mutate(location=location_name)


colnames(data)

unique(data$measure)

#EAPC 是通过回归模型估算出的指标在一段时间内的年均变化百分比，是对变化率的平均估计值。
EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Both") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)


head(EAPC) 

a <- EAPC %>% filter(location=='Global')
head(a)

a$y <- log(a$val)  #把val的数据进行对数转换，变成变量y
head(a$y)


#构建方程拟合
mod_simp_reg<-lm(y~year,data=a)
summary(mod_simp_reg)

summary(mod_simp_reg)[["coefficients"]] #提取Coefficients

summary(mod_simp_reg)[["coefficients"]][2,1] ##提取year的斜率

summary(mod_simp_reg)[["coefficients"]][2,2] #提取year的斜率的标准误

## 率的平均变化???  #拟合直线,即:Y=b+aX,其中,Y=Lg(死亡率),X为年份,则EAPC=(e的a次方-1)*100，得到的EAPC是一个百分率
(exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100  #e的a次方为exp(summary(mod_simp_reg)[["coefficients"]][2,1])




unique(data$measure)
## 可信区间 mean+-1.96*se
(exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

(exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

#建立循环语句计算每个国家的EAPC

EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Both") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)

EAPC_cal <- data.frame(location=unique(EAPC$location),
                       EAPC=rep(0,times=length(unique(EAPC$location))),
                       LCI=rep(0,times=length(unique(EAPC$location))),  #EAPC的95%下限
                       UCI=rep(0,times=length(unique(EAPC$location)))) #EAPC的95%上限

#通过循环语句计算每个国家的EAPC和95%的上下限

for (i in 1:length(unique(EAPC$location))){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,3] <- low
  EAPC_cal[i,4] <- high}


#保留2位小数
EAPC_cal <- EAPC_cal %>% mutate(EAPC=round(EAPC,2),
                                LCI=round(LCI,2),
                                UCI=round(UCI,2))


#把EAPC和95%的置信区间连接起来
EAPC_cal <- EAPC_cal %>% mutate(EAPC_CI = paste(EAPC, LCI,sep = '\n(')) %>% 
  mutate(EAPC_CI = paste(EAPC_CI, UCI,sep = ' to ')) %>% 
  mutate(EAPC_CI = paste0(EAPC_CI, ')'))
head(EAPC_cal)


location_order <- c("Global","High SDI" ,"High-middle SDI" , "Middle SDI","Low-middle SDI" ,"Low SDI"  
                    
                    ,"Central Asia"  ,    "Central Europe"   , "Eastern Europe"  ,  "Australasia"  
                    ,"High-income Asia Pacific"  ,"High-income North America","Southern Latin America" 
                    ,"Western Europe", "Andean Latin America"  ,"Caribbean" ,"Central Latin America", "Tropical Latin America","North Africa and Middle East"
                    ,"South Asia" ,"East Asia",  "Oceania","Southeast Asia" ,  "Central Sub-Saharan Africa"                         
                    , "Eastern Sub-Saharan Africa"     ,   "Southern Sub-Saharan Africa"   , "Western Sub-Saharan Africa" )

colnames(EAPC_cal)

EAPC_cal$location <- factor(EAPC_cal$location,
                            levels = location_order)


EAPC_cal <- EAPC_cal[order(EAPC_cal$location), ]


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\EAPC")

write.csv(EAPC_cal ,"global+5个sdi地区+21个地区的肺癌吸烟dalys标准化率EAPC.csv",row.names = F)





# global+5个sdi地区+21个地区的肺癌二手烟dalys标准化率 EAPC --------------------------------


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病DALYS数据\\global+5个sdi地区+21个地区")

data <- read.csv("global+5个sdi地区+21个地区的肺癌二手烟dalys标准化率 .csv",header = T)

data<-data[,-1]

colnames(data)
data <- data %>% mutate(location=location_name)


colnames(data)

unique(data$measure)

#EAPC 是通过回归模型估算出的指标在一段时间内的年均变化百分比，是对变化率的平均估计值。
EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Both") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)


head(EAPC) 

a <- EAPC %>% filter(location=='Global')
head(a)

a$y <- log(a$val)  #把val的数据进行对数转换，变成变量y
head(a$y)


#构建方程拟合
mod_simp_reg<-lm(y~year,data=a)
summary(mod_simp_reg)

summary(mod_simp_reg)[["coefficients"]] #提取Coefficients

summary(mod_simp_reg)[["coefficients"]][2,1] ##提取year的斜率

summary(mod_simp_reg)[["coefficients"]][2,2] #提取year的斜率的标准误

## 率的平均变化???  #拟合直线,即:Y=b+aX,其中,Y=Lg(死亡率),X为年份,则EAPC=(e的a次方-1)*100，得到的EAPC是一个百分率
(exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100  #e的a次方为exp(summary(mod_simp_reg)[["coefficients"]][2,1])




unique(data$measure)
## 可信区间 mean+-1.96*se
(exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

(exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

#建立循环语句计算每个国家的EAPC

EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Both") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)

EAPC_cal <- data.frame(location=unique(EAPC$location),
                       EAPC=rep(0,times=length(unique(EAPC$location))),
                       LCI=rep(0,times=length(unique(EAPC$location))),  #EAPC的95%下限
                       UCI=rep(0,times=length(unique(EAPC$location)))) #EAPC的95%上限

#通过循环语句计算每个国家的EAPC和95%的上下限

for (i in 1:length(unique(EAPC$location))){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,3] <- low
  EAPC_cal[i,4] <- high}


#保留2位小数
EAPC_cal <- EAPC_cal %>% mutate(EAPC=round(EAPC,2),
                                LCI=round(LCI,2),
                                UCI=round(UCI,2))


#把EAPC和95%的置信区间连接起来
EAPC_cal <- EAPC_cal %>% mutate(EAPC_CI = paste(EAPC, LCI,sep = '\n(')) %>% 
  mutate(EAPC_CI = paste(EAPC_CI, UCI,sep = ' to ')) %>% 
  mutate(EAPC_CI = paste0(EAPC_CI, ')'))
head(EAPC_cal)


location_order <- c("Global","High SDI" ,"High-middle SDI" , "Middle SDI","Low-middle SDI" ,"Low SDI"  
                    
                    ,"Central Asia"  ,    "Central Europe"   , "Eastern Europe"  ,  "Australasia"  
                    ,"High-income Asia Pacific"  ,"High-income North America","Southern Latin America" 
                    ,"Western Europe", "Andean Latin America"  ,"Caribbean" ,"Central Latin America", "Tropical Latin America","North Africa and Middle East"
                    ,"South Asia" ,"East Asia",  "Oceania","Southeast Asia" ,  "Central Sub-Saharan Africa"                         
                    , "Eastern Sub-Saharan Africa"     ,   "Southern Sub-Saharan Africa"   , "Western Sub-Saharan Africa" )

colnames(EAPC_cal)

EAPC_cal$location <- factor(EAPC_cal$location,
                            levels = location_order)


EAPC_cal <- EAPC_cal[order(EAPC_cal$location), ]


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\EAPC")

write.csv(EAPC_cal ,"global+5个sdi地区+21个地区的肺癌二手烟dalys标准化率EAPC.csv",row.names = F)





# 204个国家肺癌吸烟死亡标准化率EAPC-------------------------------------------------------------------------

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病死亡数据\\204个国家\\吸烟")

data <- read.csv("肺癌的吸烟死亡标准化率（204个国家加5个sdi地区）.csv",header = T)

data<-data[,-1]

colnames(data)
data <- data %>% mutate(location=location_name)


colnames(data)

unique(data$measure)


year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data<- data %>% filter(year%in%year1)


unique(data$year)

unique(data$location_name)

#筛选出204个国家
data <- data %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data$location_name)

unique(data$year)

colnames(data)


#建立循环语句计算每个国家的EAPC

EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Both") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)

EAPC_cal <- data.frame(location=unique(EAPC$location),
                       EAPC=rep(0,times=length(unique(EAPC$location))),
                       LCI=rep(0,times=length(unique(EAPC$location))),  #EAPC的95%下限
                       UCI=rep(0,times=length(unique(EAPC$location)))) #EAPC的95%上限

#通过循环语句计算每个国家的EAPC和95%的上下限

for (i in 1:length(unique(EAPC$location))){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,3] <- low
  EAPC_cal[i,4] <- high}


#保留2位小数
EAPC_cal <- EAPC_cal %>% mutate(EAPC=round(EAPC,2),
                                LCI=round(LCI,2),
                                UCI=round(UCI,2))


#把EAPC和95%的置信区间连接起来
EAPC_cal <- EAPC_cal %>% mutate(EAPC_CI = paste(EAPC, LCI,sep = '\n(')) %>% 
  mutate(EAPC_CI = paste(EAPC_CI, UCI,sep = ' to ')) %>% 
  mutate(EAPC_CI = paste0(EAPC_CI, ')'))
head(EAPC_cal)




setwd("C:\\Users\\dell\\Desktop\\GBD数据\\EAPC")

write.csv(EAPC_cal ,"204个国家肺癌吸烟死亡标准化率EAPC.csv",row.names = F)




# 204个国家肺癌二手烟死亡标准化率EAPC ---------------------------------------------------

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病死亡数据\\204个国家\\二手烟")

data <- read.csv("肺癌的二手烟死亡标准化率（204个国家加5个sdi地区）.csv",header = T)

data<-data[,-1]

colnames(data)
data <- data %>% mutate(location=location_name)


colnames(data)

unique(data$measure)


year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data<- data %>% filter(year%in%year1)


unique(data$year)

unique(data$location_name)

#筛选出204个国家
data <- data %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                               "High SDI","High-middle SDI","Middle SDI" )))

unique(data$location_name)

unique(data$year)

colnames(data)


#建立循环语句计算每个国家的EAPC

EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Both") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)

EAPC_cal <- data.frame(location=unique(EAPC$location),
                       EAPC=rep(0,times=length(unique(EAPC$location))),
                       LCI=rep(0,times=length(unique(EAPC$location))),  #EAPC的95%下限
                       UCI=rep(0,times=length(unique(EAPC$location)))) #EAPC的95%上限

#通过循环语句计算每个国家的EAPC和95%的上下限

for (i in 1:length(unique(EAPC$location))){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,3] <- low
  EAPC_cal[i,4] <- high}


#保留2位小数
EAPC_cal <- EAPC_cal %>% mutate(EAPC=round(EAPC,2),
                                LCI=round(LCI,2),
                                UCI=round(UCI,2))


#把EAPC和95%的置信区间连接起来
EAPC_cal <- EAPC_cal %>% mutate(EAPC_CI = paste(EAPC, LCI,sep = '\n(')) %>% 
  mutate(EAPC_CI = paste(EAPC_CI, UCI,sep = ' to ')) %>% 
  mutate(EAPC_CI = paste0(EAPC_CI, ')'))
head(EAPC_cal)




setwd("C:\\Users\\dell\\Desktop\\GBD数据\\EAPC")

write.csv(EAPC_cal ,"204个国家肺癌的二手烟死亡标准化率EAPC.csv",row.names = F)




# 204个国家肺癌吸烟dalys标准化率 EAPC ------------------------------------------------

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病DALYS数据\\204个国家\\吸烟")

data <- read.csv("肺癌的吸烟DALYS标准化率（204个国家加5个sdi地区）.csv",header = T)

data<-data[,-1]

colnames(data)
data <- data %>% mutate(location=location_name)


colnames(data)

unique(data$measure)


year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data<- data %>% filter(year%in%year1)


unique(data$year)

unique(data$location_name)

#筛选出204个国家
data <- data %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                               "High SDI","High-middle SDI","Middle SDI" )))

unique(data$location_name)

unique(data$year)

colnames(data)


#建立循环语句计算每个国家的EAPC

EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Both") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)

EAPC_cal <- data.frame(location=unique(EAPC$location),
                       EAPC=rep(0,times=length(unique(EAPC$location))),
                       LCI=rep(0,times=length(unique(EAPC$location))),  #EAPC的95%下限
                       UCI=rep(0,times=length(unique(EAPC$location)))) #EAPC的95%上限

#通过循环语句计算每个国家的EAPC和95%的上下限

for (i in 1:length(unique(EAPC$location))){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,3] <- low
  EAPC_cal[i,4] <- high}


#保留2位小数
EAPC_cal <- EAPC_cal %>% mutate(EAPC=round(EAPC,2),
                                LCI=round(LCI,2),
                                UCI=round(UCI,2))


#把EAPC和95%的置信区间连接起来
EAPC_cal <- EAPC_cal %>% mutate(EAPC_CI = paste(EAPC, LCI,sep = '\n(')) %>% 
  mutate(EAPC_CI = paste(EAPC_CI, UCI,sep = ' to ')) %>% 
  mutate(EAPC_CI = paste0(EAPC_CI, ')'))
head(EAPC_cal)




setwd("C:\\Users\\dell\\Desktop\\GBD数据\\EAPC")

write.csv(EAPC_cal ,"204个国家肺癌的吸烟DALYS标准化率EAPC.csv",row.names = F)

# 204个国家肺癌二手烟dalys标准化率 EAPC -----------------------------------------------


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病DALYS数据\\204个国家\\二手烟")

data <- read.csv("肺癌的二手烟DALYS标准化率（204个国家加5个sdi地区）.csv",header = T)

data<-data[,-1]

colnames(data)
data <- data %>% mutate(location=location_name)


colnames(data)

unique(data$measure)


year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data<- data %>% filter(year%in%year1)


unique(data$year)

unique(data$location_name)

#筛选出204个国家
data <- data %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                               "High SDI","High-middle SDI","Middle SDI" )))

unique(data$location_name)

unique(data$year)

colnames(data)


#建立循环语句计算每个国家的EAPC

EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Both") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)

EAPC_cal <- data.frame(location=unique(EAPC$location),
                       EAPC=rep(0,times=length(unique(EAPC$location))),
                       LCI=rep(0,times=length(unique(EAPC$location))),  #EAPC的95%下限
                       UCI=rep(0,times=length(unique(EAPC$location)))) #EAPC的95%上限

#通过循环语句计算每个国家的EAPC和95%的上下限

for (i in 1:length(unique(EAPC$location))){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,3] <- low
  EAPC_cal[i,4] <- high}


#保留2位小数
EAPC_cal <- EAPC_cal %>% mutate(EAPC=round(EAPC,2),
                                LCI=round(LCI,2),
                                UCI=round(UCI,2))


#把EAPC和95%的置信区间连接起来
EAPC_cal <- EAPC_cal %>% mutate(EAPC_CI = paste(EAPC, LCI,sep = '\n(')) %>% 
  mutate(EAPC_CI = paste(EAPC_CI, UCI,sep = ' to ')) %>% 
  mutate(EAPC_CI = paste0(EAPC_CI, ')'))
head(EAPC_cal)




setwd("C:\\Users\\dell\\Desktop\\GBD数据\\EAPC")

write.csv(EAPC_cal ,"204个国家肺癌的二手烟DALYS标准化率EAPC.csv",row.names = F)



# global二手烟肺癌ASMR EAPC ------------------------------------------------------

#男
setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病死亡数据\\global+5个sdi地区+21个地区")

data <- read.csv("global+5个sdi地区+21个地区的肺癌二手烟死亡标准化率.csv",header = T)

data<-data[,-1]

colnames(data)
data <- data %>% mutate(location=location_name)


colnames(data)

unique(data$measure)
unique(data$sex_name)


#EAPC 是通过回归模型估算出的指标在一段时间内的年均变化百分比，是对变化率的平均估计值。
EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Male") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)


head(EAPC) 

a <- EAPC %>% filter(location=='Global')
head(a)

a$y <- log(a$val)  #把val的数据进行对数转换，变成变量y
head(a$y)


#构建方程拟合
mod_simp_reg<-lm(y~year,data=a)
summary(mod_simp_reg)

summary(mod_simp_reg)[["coefficients"]] #提取Coefficients

summary(mod_simp_reg)[["coefficients"]][2,1] ##提取year的斜率

summary(mod_simp_reg)[["coefficients"]][2,2] #提取year的斜率的标准误

## 率的平均变化???  #拟合直线,即:Y=b+aX,其中,Y=Lg(死亡率),X为年份,则EAPC=(e的a次方-1)*100，得到的EAPC是一个百分率
(exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100  #e的a次方为exp(summary(mod_simp_reg)[["coefficients"]][2,1])




unique(data$measure)
## 可信区间 mean+-1.96*se
(exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

(exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

#建立循环语句计算每个国家的EAPC

EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Male") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)

EAPC_cal <- data.frame(location=unique(EAPC$location),
                       EAPC=rep(0,times=length(unique(EAPC$location))),
                       LCI=rep(0,times=length(unique(EAPC$location))),  #EAPC的95%下限
                       UCI=rep(0,times=length(unique(EAPC$location)))) #EAPC的95%上限

#通过循环语句计算每个国家的EAPC和95%的上下限

for (i in 1:length(unique(EAPC$location))){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,3] <- low
  EAPC_cal[i,4] <- high}


#保留2位小数
EAPC_cal <- EAPC_cal %>% mutate(EAPC=round(EAPC,2),
                                LCI=round(LCI,2),
                                UCI=round(UCI,2))


#把EAPC和95%的置信区间连接起来
EAPC_cal <- EAPC_cal %>% mutate(EAPC_CI = paste(EAPC, LCI,sep = '\n(')) %>% 
  mutate(EAPC_CI = paste(EAPC_CI, UCI,sep = ' to ')) %>% 
  mutate(EAPC_CI = paste0(EAPC_CI, ')'))
head(EAPC_cal)


location_order <- c("Global","High SDI" ,"High-middle SDI" , "Middle SDI","Low-middle SDI" ,"Low SDI"  
                    
                    ,"Central Asia"  ,    "Central Europe"   , "Eastern Europe"  ,  "Australasia"  
                    ,"High-income Asia Pacific"  ,"High-income North America","Southern Latin America" 
                    ,"Western Europe", "Andean Latin America"  ,"Caribbean" ,"Central Latin America", "Tropical Latin America","North Africa and Middle East"
                    ,"South Asia" ,"East Asia",  "Oceania","Southeast Asia" ,  "Central Sub-Saharan Africa"                         
                    , "Eastern Sub-Saharan Africa"     ,   "Southern Sub-Saharan Africa"   , "Western Sub-Saharan Africa" )

colnames(EAPC_cal)

EAPC_cal$location <- factor(EAPC_cal$location,
                            levels = location_order)


EAPC_cal <- EAPC_cal[order(EAPC_cal$location), ]


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\EAPC")

write.csv(EAPC_cal ,"global+5个sdi地区+21个地区的肺癌二手烟死亡标准化率EAPC男.csv",row.names = F)







#女

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病死亡数据\\global+5个sdi地区+21个地区")

data <- read.csv("global+5个sdi地区+21个地区的肺癌二手烟死亡标准化率.csv",header = T)

data<-data[,-1]

colnames(data)
data <- data %>% mutate(location=location_name)


colnames(data)

unique(data$measure)
unique(data$sex_name)


#EAPC 是通过回归模型估算出的指标在一段时间内的年均变化百分比，是对变化率的平均估计值。
EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Female") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)


head(EAPC) 

a <- EAPC %>% filter(location=='Global')
head(a)

a$y <- log(a$val)  #把val的数据进行对数转换，变成变量y
head(a$y)


#构建方程拟合
mod_simp_reg<-lm(y~year,data=a)
summary(mod_simp_reg)

summary(mod_simp_reg)[["coefficients"]] #提取Coefficients

summary(mod_simp_reg)[["coefficients"]][2,1] ##提取year的斜率

summary(mod_simp_reg)[["coefficients"]][2,2] #提取year的斜率的标准误

## 率的平均变化???  #拟合直线,即:Y=b+aX,其中,Y=Lg(死亡率),X为年份,则EAPC=(e的a次方-1)*100，得到的EAPC是一个百分率
(exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100  #e的a次方为exp(summary(mod_simp_reg)[["coefficients"]][2,1])




unique(data$measure)
## 可信区间 mean+-1.96*se
(exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

(exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

#建立循环语句计算每个国家的EAPC

EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Female") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)

EAPC_cal <- data.frame(location=unique(EAPC$location),
                       EAPC=rep(0,times=length(unique(EAPC$location))),
                       LCI=rep(0,times=length(unique(EAPC$location))),  #EAPC的95%下限
                       UCI=rep(0,times=length(unique(EAPC$location)))) #EAPC的95%上限

#通过循环语句计算每个国家的EAPC和95%的上下限

for (i in 1:length(unique(EAPC$location))){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,3] <- low
  EAPC_cal[i,4] <- high}


#保留2位小数
EAPC_cal <- EAPC_cal %>% mutate(EAPC=round(EAPC,2),
                                LCI=round(LCI,2),
                                UCI=round(UCI,2))


#把EAPC和95%的置信区间连接起来
EAPC_cal <- EAPC_cal %>% mutate(EAPC_CI = paste(EAPC, LCI,sep = '\n(')) %>% 
  mutate(EAPC_CI = paste(EAPC_CI, UCI,sep = ' to ')) %>% 
  mutate(EAPC_CI = paste0(EAPC_CI, ')'))
head(EAPC_cal)


location_order <- c("Global","High SDI" ,"High-middle SDI" , "Middle SDI","Low-middle SDI" ,"Low SDI"  
                    
                    ,"Central Asia"  ,    "Central Europe"   , "Eastern Europe"  ,  "Australasia"  
                    ,"High-income Asia Pacific"  ,"High-income North America","Southern Latin America" 
                    ,"Western Europe", "Andean Latin America"  ,"Caribbean" ,"Central Latin America", "Tropical Latin America","North Africa and Middle East"
                    ,"South Asia" ,"East Asia",  "Oceania","Southeast Asia" ,  "Central Sub-Saharan Africa"                         
                    , "Eastern Sub-Saharan Africa"     ,   "Southern Sub-Saharan Africa"   , "Western Sub-Saharan Africa" )

colnames(EAPC_cal)

EAPC_cal$location <- factor(EAPC_cal$location,
                            levels = location_order)


EAPC_cal <- EAPC_cal[order(EAPC_cal$location), ]


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\EAPC")

write.csv(EAPC_cal ,"global+5个sdi地区+21个地区的肺癌二手烟死亡标准化率EAPC女.csv",row.names = F)









# global二手烟肺癌ASDR EAPC ----------------------------------------------------


#男

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病DALYS数据\\global+5个sdi地区+21个地区")

data <- read.csv("global+5个sdi地区+21个地区的肺癌二手烟dalys标准化率 .csv",header = T)

data<-data[,-1]

colnames(data)
data <- data %>% mutate(location=location_name)


colnames(data)

unique(data$measure)
unique(data$sex_name)
#EAPC 是通过回归模型估算出的指标在一段时间内的年均变化百分比，是对变化率的平均估计值。
EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Male") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)


head(EAPC) 

a <- EAPC %>% filter(location=='Global')
head(a)

a$y <- log(a$val)  #把val的数据进行对数转换，变成变量y
head(a$y)


#构建方程拟合
mod_simp_reg<-lm(y~year,data=a)
summary(mod_simp_reg)

summary(mod_simp_reg)[["coefficients"]] #提取Coefficients

summary(mod_simp_reg)[["coefficients"]][2,1] ##提取year的斜率

summary(mod_simp_reg)[["coefficients"]][2,2] #提取year的斜率的标准误

## 率的平均变化???  #拟合直线,即:Y=b+aX,其中,Y=Lg(死亡率),X为年份,则EAPC=(e的a次方-1)*100，得到的EAPC是一个百分率
(exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100  #e的a次方为exp(summary(mod_simp_reg)[["coefficients"]][2,1])




unique(data$measure)
## 可信区间 mean+-1.96*se
(exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

(exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

#建立循环语句计算每个国家的EAPC

EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Male") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)

EAPC_cal <- data.frame(location=unique(EAPC$location),
                       EAPC=rep(0,times=length(unique(EAPC$location))),
                       LCI=rep(0,times=length(unique(EAPC$location))),  #EAPC的95%下限
                       UCI=rep(0,times=length(unique(EAPC$location)))) #EAPC的95%上限

#通过循环语句计算每个国家的EAPC和95%的上下限

for (i in 1:length(unique(EAPC$location))){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,3] <- low
  EAPC_cal[i,4] <- high}


#保留2位小数
EAPC_cal <- EAPC_cal %>% mutate(EAPC=round(EAPC,2),
                                LCI=round(LCI,2),
                                UCI=round(UCI,2))


#把EAPC和95%的置信区间连接起来
EAPC_cal <- EAPC_cal %>% mutate(EAPC_CI = paste(EAPC, LCI,sep = '\n(')) %>% 
  mutate(EAPC_CI = paste(EAPC_CI, UCI,sep = ' to ')) %>% 
  mutate(EAPC_CI = paste0(EAPC_CI, ')'))
head(EAPC_cal)


location_order <- c("Global","High SDI" ,"High-middle SDI" , "Middle SDI","Low-middle SDI" ,"Low SDI"  
                    
                    ,"Central Asia"  ,    "Central Europe"   , "Eastern Europe"  ,  "Australasia"  
                    ,"High-income Asia Pacific"  ,"High-income North America","Southern Latin America" 
                    ,"Western Europe", "Andean Latin America"  ,"Caribbean" ,"Central Latin America", "Tropical Latin America","North Africa and Middle East"
                    ,"South Asia" ,"East Asia",  "Oceania","Southeast Asia" ,  "Central Sub-Saharan Africa"                         
                    , "Eastern Sub-Saharan Africa"     ,   "Southern Sub-Saharan Africa"   , "Western Sub-Saharan Africa" )

colnames(EAPC_cal)

EAPC_cal$location <- factor(EAPC_cal$location,
                            levels = location_order)


EAPC_cal <- EAPC_cal[order(EAPC_cal$location), ]


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\EAPC")

write.csv(EAPC_cal ,"global+5个sdi地区+21个地区的肺癌二手烟dalys标准化率EAPC男.csv",row.names = F)



#女



setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病DALYS数据\\global+5个sdi地区+21个地区")

data <- read.csv("global+5个sdi地区+21个地区的肺癌二手烟dalys标准化率 .csv",header = T)

data<-data[,-1]

colnames(data)
data <- data %>% mutate(location=location_name)


colnames(data)

unique(data$measure)
unique(data$sex_name)
#EAPC 是通过回归模型估算出的指标在一段时间内的年均变化百分比，是对变化率的平均估计值。
EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Female") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)


head(EAPC) 

a <- EAPC %>% filter(location=='Global')
head(a)

a$y <- log(a$val)  #把val的数据进行对数转换，变成变量y
head(a$y)


#构建方程拟合
mod_simp_reg<-lm(y~year,data=a)
summary(mod_simp_reg)

summary(mod_simp_reg)[["coefficients"]] #提取Coefficients

summary(mod_simp_reg)[["coefficients"]][2,1] ##提取year的斜率

summary(mod_simp_reg)[["coefficients"]][2,2] #提取year的斜率的标准误

## 率的平均变化???  #拟合直线,即:Y=b+aX,其中,Y=Lg(死亡率),X为年份,则EAPC=(e的a次方-1)*100，得到的EAPC是一个百分率
(exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100  #e的a次方为exp(summary(mod_simp_reg)[["coefficients"]][2,1])




unique(data$measure)
## 可信区间 mean+-1.96*se
(exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

(exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100

#建立循环语句计算每个国家的EAPC

EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Female") %>% 
  filter(metric_name == 'Rate') %>%
  select(13, 9, 10) %>% arrange(location,year)

EAPC_cal <- data.frame(location=unique(EAPC$location),
                       EAPC=rep(0,times=length(unique(EAPC$location))),
                       LCI=rep(0,times=length(unique(EAPC$location))),  #EAPC的95%下限
                       UCI=rep(0,times=length(unique(EAPC$location)))) #EAPC的95%上限

#通过循环语句计算每个国家的EAPC和95%的上下限

for (i in 1:length(unique(EAPC$location))){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,3] <- low
  EAPC_cal[i,4] <- high}


#保留2位小数
EAPC_cal <- EAPC_cal %>% mutate(EAPC=round(EAPC,2),
                                LCI=round(LCI,2),
                                UCI=round(UCI,2))


#把EAPC和95%的置信区间连接起来
EAPC_cal <- EAPC_cal %>% mutate(EAPC_CI = paste(EAPC, LCI,sep = '\n(')) %>% 
  mutate(EAPC_CI = paste(EAPC_CI, UCI,sep = ' to ')) %>% 
  mutate(EAPC_CI = paste0(EAPC_CI, ')'))
head(EAPC_cal)


location_order <- c("Global","High SDI" ,"High-middle SDI" , "Middle SDI","Low-middle SDI" ,"Low SDI"  
                    
                    ,"Central Asia"  ,    "Central Europe"   , "Eastern Europe"  ,  "Australasia"  
                    ,"High-income Asia Pacific"  ,"High-income North America","Southern Latin America" 
                    ,"Western Europe", "Andean Latin America"  ,"Caribbean" ,"Central Latin America", "Tropical Latin America","North Africa and Middle East"
                    ,"South Asia" ,"East Asia",  "Oceania","Southeast Asia" ,  "Central Sub-Saharan Africa"                         
                    , "Eastern Sub-Saharan Africa"     ,   "Southern Sub-Saharan Africa"   , "Western Sub-Saharan Africa" )

colnames(EAPC_cal)

EAPC_cal$location <- factor(EAPC_cal$location,
                            levels = location_order)


EAPC_cal <- EAPC_cal[order(EAPC_cal$location), ]


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\EAPC")

write.csv(EAPC_cal ,"global+5个sdi地区+21个地区的肺癌二手烟dalys标准化率EAPC女.csv",row.names = F)







