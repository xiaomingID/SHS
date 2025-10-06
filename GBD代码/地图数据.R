
library(reshape)
library(ggplot2)
library(ggrepel)
library(data.table)
library(tidyverse)


# #提取204个国家肺癌二手烟死亡标准化率 ----------------------------------------------------


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病死亡数据\\204个国家\\二手烟")
data1.1 <- fread("肺癌的二手烟死亡标准化率（204个国家加5个sdi地区）.csv")

year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data1.1<- data1.1 %>% filter(year%in%year1)

data1.1 <- data1.1[,-1]

unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出204个国家2021年ASMR(总)


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\地图")
colnames(data1.1)
unique(data1.1$sex_name)

data1.2 <- data1.1 %>% filter(year=="2021") %>% 
                      filter(sex_name=="Both")

unique(data1.2$year)


write.csv(data1.2 ,"筛选出204个国家2021年ASMR(总).csv",row.names = F)


#筛选出204个国家2021年ASMR(男)

data1.3<- data1.1 %>% filter(year=="2021") %>% 
                         filter(sex_name=="Male")

write.csv(data1.3 ,"筛选出204个国家2021年ASMR(男).csv",row.names = F)

#筛选出204个国家2021年ASMR(女)

data1.4<- data1.1 %>% filter(year=="2021") %>% 
              filter(sex_name=="Female")

write.csv(data1.4 ,"筛选出204个国家2021年ASMR(女).csv",row.names = F)


# 提取204个国家肺癌二手烟dalys标准化率  -------------------------------------------------


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病DALYS数据\\204个国家\\二手烟")


data2.1 <- fread("肺癌的二手烟DALYS标准化率（204个国家加5个sdi地区）.csv")

year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data2.1<- data2.1 %>% filter(year%in%year1)

data2.1 <- data2.1[,-1]

unique(data2.1$year)




unique(data2.1$location_name)

#筛选出204个国家
data2.1 <- data2.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data2.1$location_name)

unique(data2.1$year)

#筛选出204个国家2021年ASDR(总)


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\地图")
colnames(data2.1)
unique(data2.1$sex_name)

data2.2 <- data2.1 %>% filter(year=="2021") %>% 
  filter(sex_name=="Both")

unique(data2.2$year)


write.csv(data2.2 ,"筛选出204个国家2021年ASDR(总).csv",row.names = F)


#筛选出204个国家2021年ASMR(男)

data2.3<- data2.1 %>% filter(year=="2021") %>% 
  filter(sex_name=="Male")

write.csv(data2.3 ,"筛选出204个国家2021年ASDR(男).csv",row.names = F)

#筛选出204个国家2021年ASMR(女)

data2.4<- data2.1 %>% filter(year=="2021") %>% 
  filter(sex_name=="Female")

write.csv(data2.4 ,"筛选出204个国家2021年ASDR(女).csv",row.names = F)














# 提取204个国家肺癌二手烟死亡标准化率EAPC -------------------------------------------------

#筛选出男的ASMR的EAPC
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
unique(data$sex_name)

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




setwd("C:\\Users\\dell\\Desktop\\GBD数据\\地图")

write.csv(EAPC_cal ,"204个国家肺癌的二手烟死亡标准化率EAPC（男）.csv",row.names = F)


#筛选出女的ASMR的EAPC
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
unique(data$sex_name)

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




setwd("C:\\Users\\dell\\Desktop\\GBD数据\\地图")

write.csv(EAPC_cal ,"204个国家肺癌的二手烟死亡标准化率EAPC（女）.csv",row.names = F)





# 提取204个国家肺癌二手烟dalys标准化率EAPC ----------------------------------------------

#提取男的ASDR的EAPC

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

unique(data$sex_name)
#建立循环语句计算每个国家的EAPC

EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Male") %>% 
  filter(metric_name == 'Rate') %>%
  select(1,6,4,13, 9, 10) %>% arrange(location,year)

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

write.csv(EAPC_cal ,"204个国家肺癌的二手烟DALYS标准化率EAPC(男）.csv",row.names = F)





#提取女的ASDR的EAPC

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

unique(data$sex_name)
#建立循环语句计算每个国家的EAPC

EAPC <- data %>%
  filter(age_name == 'Age-standardized') %>%
  filter(sex_name=="Female") %>% 
  filter(metric_name == 'Rate') %>%
  select(1,6,4,13, 9, 10) %>% arrange(location,year)

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

write.csv(EAPC_cal ,"204个国家肺癌的二手烟DALYS标准化率EAPC(女）.csv",row.names = F)








# 世界地图204个国家2021年ASDR(总)------------------------------------------------------------------

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\地图\\世界国家")

map<- read_sf("世界国家.shp") #读取文件

colnames(map)


# 设定地图系统的坐标参考系 ------------------------------------------------------------

map <- st_set_crs(map,4326) #WGS-84坐标的EPSG为4326

ggplot(data = map)+
  geom_sf()




setwd("C:\\Users\\dell\\Desktop\\GBD数据\\地图")

data1<- fread("筛选出204个国家2021年ASDR(总).csv",header = T)

colnames(data1)
unique(data1$location_name)

#location中它有三列,分别为 location,location2,location3。
#其中 location 与 GBD 的 location 完全一 致;
#location2 与 map 数据的 FENAME 列完全一致;
#location3 与 map 数据 的 NAME 列完全一致。
location <- read.csv("location.csv")



# 数据的合并连接 -----------------------------------------------------------------

#合并 GBD 数据与 location 数据

data1 <- data1 %>%   
  rename(  
    location = location_name)  



data1.1 <- left_join(data1,location,by="location") #合并后会多出location2和location3
colnames(data1.1)
unique(data1.1$location2)

unique(data1.1$location)
#再通过 location#再通过 location2/location3 与 map 数据合并

data1.2 <- left_join(map,data1.1,by=c("NAME"="location3")) #带有地理坐标的map要放在前面

colnames(data1.2)

unique(data1.2$NAME)

unique(data1.2$measure_name)

unique(data1.2$year)

unique(data1.2$sex_name)

unique(data1.2$age_name)


# 绘制属性地图 ------------------------------------------------------------------
library(ggsci)
fig1 <- data1.2 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(year==2021 ) %>% 
  filter(sex_name=="Both") %>% 
  ggplot()+
  geom_sf(aes(group=NAME,fill=val),color=alpha("white",0.2))+  
  scale_fill_distiller(palette="Spectral",# 色盘 
                       name="ASDR") +  
  theme_minimal() +      # 简洁主题  
  labs(  
    title = "世界地图204个国家二手烟肺癌2021年ASDR",  
    fill = "ASDR",  
    caption = "数据来源：全球健康数据"  
  ) +  
  theme(  
    legend.position = "right",  
    plot.title = element_text(hjust = 0.5)  
  )  
fig1



# 颜色变换 --------------------------------------------------------------------



ggsave("世界地图204个国家二手烟肺癌2021年ASDR总.JPEG",width = 8,height=10,dpi = 300) #使用ggsave()函数保存，图片质量好





