


library(reshape)
library(ggplot2)
library(ggrepel)
library(data.table)
library(tidyverse)


# global+21个地区肺癌吸烟死亡标准化率相关性分析 ---------------------------------------------

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\吸烟")

data <- fread("肺癌global+21个地区吸烟死亡标准化率.csv")

year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data<- data %>% filter(year%in%year1)



unique(data$year)


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性")

order_SDI <- read.csv('order_SDI.csv',header = F)

SDI_2021<-fread("SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$year)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'

#筛选变量

colnames(data)

unique(data$sex_name)
unique(data$age_name)
unique(data$year)

data <- data %>% filter(sex_name=="Both") %>% select(-1)

colnames(data)
data_ASMR <- data[,c(3,9,10)] ###选择location year val

names(data_ASMR)[3] <- 'ASMR'
names(data_ASMR)[1] <- 'location'



### 合并SDI与ASR数据
data_ASMR_SDI <- merge(data_ASMR,SDI_2021,by=c('location','year'))

unique(data_ASMR_SDI$location)

unique(data_ASMR_SDI$year)

data_ASMR_SDI$location <- factor(data_ASMR_SDI$location, 
                                levels=order_SDI$V1, 
                                ordered=TRUE) ## location图例按照我们的顺序排列

str(data_ASMR_SDI$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\吸烟")

write.csv(data_ASMR_SDI,"肺癌global+21个地区吸烟死亡标准化率+SDI.csv") 

getwd()


##开始作图，主变量为ASMR以及SDI,图形的颜色和形状根据不同区域来调整即可
### 同时以所有数据画出拟合曲线

a <- cor.test(as.numeric(data_ASMR_SDI$ASMR),as.numeric(data_ASMR_SDI$SDI),method = "pearson")

fig1 <- ggplot(data_ASMR_SDI, aes(SDI,ASMR)) + geom_point(aes(color = location, shape= location))+
  scale_shape_manual(values = 1:22) + 
  annotate("text",x=0.25,y=max(data_ASMR_SDI$ASMR), #定义添加的文本和其位置
           label=paste("R=",round ( a[["estimate"]] ,3),sep = ""),
           size=4,color="black" )+
  annotate("text",x=0.25,y=max(data_ASMR_SDI$ASMR)*0.95,
           label=paste("p=",round ( a[["p.value"]] ,3),sep = ""),
           size=4,color="black")+
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) 

fig1



# #提取204个国家肺癌吸烟死亡标准化率，相关性分析 ---------------------------------------------------------



setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病死亡数据\\204个国家\\吸烟")


data1.1 <- fread("肺癌的吸烟死亡标准化率（204个国家加5个sdi地区）.csv")

year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data1.1<- data1.1 %>% filter(year%in%year1)



unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出2021年的sdi


SDI_2021<-fread("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$Location)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')

SDI_2021 <- SDI_2021[SDI_2021$year== 2021,] ###只取2021年的数据


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'


#获取肺癌吸烟死亡标准化率2021年数据

colnames(data1.1)
unique(data1.1$year)

data2.1 <- data1.1 %>% filter(year=="2021") %>% 
                       filter(sex_name=="Both")

colnames(data2.1)

data2.1 <- data2.1[,c(4,10,11)]

names(data2.1)[3] <- 'ASMR'


### 调整疾病数据里的location，使其与SDI数据中location的命名一致
data2.1$location[data2.1$location == "Democratic People's Republic of Korea"] = 'North Korea'
data2.1$location[data2.1$location == 'Russian Federation'] = 'Russia'
data2.1$location[data2.1$location == 'United Kingdom'] = 'UK'
data2.1$location[data2.1$location == "Iran (Islamic Republic of)"] = 'Iran'
data2.1$location[data2.1$location == "Taiwan (Province of China)"] = 'Taiwan (province of China)'
data2.1$location[data2.1$location == "Republic of Korea"] = 'South Korea'
data2.1$location[data2.1$location == "United Republic of Tanzania"] = 'Tanzania'
data2.1$location[data2.1$location == "C么te d'Ivoire"] = 'Saint Helena'
data2.1$location[data2.1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
data2.1$location[data2.1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'

data2.1$location[data2.1$location == "Republic of Moldova"] = 'Moldova'

data2.1$location[data2.1$location == "Lao People's Democratic Republic"] = 'Laos'
data2.1$location[data2.1$location == "Syrian Arab Republic"] = 'Syria'

data2.1$location[data2.1$location == "Brunei Darussalam"] = 'Brunei'
data2.1$location[data2.1$location == "Gambia"] = 'The Gambia'
data2.1$location[data2.1$location == "United States of America"] = 'USA'
data2.1$location[data2.1$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
data2.1$location[data2.1$location == "Bahamas"] = 'The Bahamas'
data2.1$location[data2.1$location == "United States Virgin Islands"] = 'Virgin Islands'
data2.1$location[data2.1$location == "Macedonia"] = 'North Macedonia'
data2.1$location[data2.1$location == 'Democratic Republic of the Congo'] = 'DR Congo'
data2.1$location[data2.1$location == 'Congo'] = 'Congo (Brazzaville)'
data2.1$location[data2.1$location == 'Sao Tome and Principe'] = 'São Tomé and Príncipe'


unique(data2.1$location)

data2.1 <- data2.1 %>% select(4,2,3)

#合并疾病和sdi数据


data2.1_ASMR_SDI_2021 <- merge(data2.1,SDI_2021,by=c('location','year'))

unique(data2.1$location)

unique(data2.1_ASMR_SDI_2021$location)

setdiff(data2.1$location,data2.1_ASMR_SDI_2021$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\吸烟")

write.csv(data2.1_ASMR_SDI_2021,"204个国家肺癌吸烟死亡标准化率+SDI(2021).csv",row.names = F)


#作图

b <- cor.test(as.numeric(data2.1_ASMR_SDI_2021$ASMR),as.numeric(data2.1_ASMR_SDI_2021$SDI),method = "pearson")


fig2 <- ggplot(data2.1_ASMR_SDI_2021, aes(SDI,ASMR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  labs(title = NULL,x=NULL,y=NULL)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 1,size = 9,color = "black"),
        axis.text.y = element_text(vjust = 1,size = 9,color = "black"),
        title = element_text(size = 12,hjust = 0.5))+theme(legend.position="none")+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR), #定义添加的文本和其位置
           label=paste("R=",round ( b[["estimate"]] ,3),sep = ""),
           size=4,color="black" )+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR)*0.95,
           label=paste("p=",round ( b[["p.value"]] ,3),sep = ""),
           size=4,color="black")+
  xlab("SDI")+
  ylab("Age-standardized prevalence rate （per 100000 populations)")

fig2
















# global+21个地区肺癌二手烟死亡标准化率相关性分析 --------------------------------------------

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟")

data <- fread("肺癌global+21个地区二手烟死亡标准化率.csv")

year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data<- data %>% filter(year%in%year1)



unique(data$year)


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性")

order_SDI <- read.csv('order_SDI.csv',header = F)

SDI_2021<-fread("SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$year)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'

#筛选变量

colnames(data)

unique(data$sex_name)
unique(data$age_name)
unique(data$year)

data <- data %>% filter(sex_name=="Both") %>% select(-1)

colnames(data)
data_ASMR <- data[,c(3,9,10)] ###选择location year val

names(data_ASMR)[3] <- 'ASMR'
names(data_ASMR)[1] <- 'location'



### 合并SDI与ASR数据
data_ASMR_SDI <- merge(data_ASMR,SDI_2021,by=c('location','year'))

unique(data_ASMR_SDI$location)

unique(data_ASMR_SDI$year)

data_ASMR_SDI$location <- factor(data_ASMR_SDI$location, 
                                 levels=order_SDI$V1, 
                                 ordered=TRUE) ## location图例按照我们的顺序排列

str(data_ASMR_SDI$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟")

write.csv(data_ASMR_SDI,"肺癌global+21个地区二手烟死亡标准化率+SDI.csv") 

getwd()


##开始作图，主变量为ASMR以及SDI,图形的颜色和形状根据不同区域来调整即可
### 同时以所有数据画出拟合曲线



a <- cor.test(as.numeric(data_ASMR_SDI$ASMR),as.numeric(data_ASMR_SDI$SDI),method = "pearson")

fig1 <- ggplot(data_ASMR_SDI, aes(SDI,ASMR)) +
  theme_bw()+
  geom_point(aes(color = location, shape= location))+
  scale_shape_manual(values = 1:22) + 
  scale_x_continuous(limits = c(0,1),    #设置x轴的尺度在 0-60000 之间,间隔为 5 年
                     breaks = seq(0,1,by=0.25) )+
  annotate("text",x=0.05,y=max(data_ASMR_SDI$ASMR), #定义添加的文本和其位置
           label=paste("R=",round ( a[["estimate"]] ,3),sep = ""),
           size=4,color="black" )+
  annotate("text",x=0.05,y=max(data_ASMR_SDI$ASMR)*0.95,
           label=paste("p<0.01"),
           size=4,color="black")+
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5)

fig1

#label=paste("p=",round ( a[["p.value"]] ,4),sep = ""),

ggsave("global+21个地区肺癌二手烟死亡标准化率SDI相关性分析.JPEG",width = 8,height=10,dpi = 300) #使用ggsave()函数保存，图片质量好




# 提取204个国家肺癌二手烟死亡标准化率，相关性分析 -----------------------------------------------


#筛选出总的
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



unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出2021年的sdi


SDI_2021<-fread("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$Location)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')

SDI_2021 <- SDI_2021[SDI_2021$year== 2021,] ###只取2021年的数据


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'


#获取肺癌二手烟死亡标准化率2021年数据

colnames(data1.1)
unique(data1.1$year)

data2.1 <- data1.1 %>% filter(year=="2021") %>% 
  filter(sex_name=="Both")

colnames(data2.1)

data2.1 <- data2.1[,c(4,10,11)]

names(data2.1)[3] <- 'ASMR'


### 调整疾病数据里的location，使其与SDI数据中location的命名一致
data2.1$location[data2.1$location == "Democratic People's Republic of Korea"] = 'North Korea'
data2.1$location[data2.1$location == 'Russian Federation'] = 'Russia'
data2.1$location[data2.1$location == 'United Kingdom'] = 'UK'
data2.1$location[data2.1$location == "Iran (Islamic Republic of)"] = 'Iran'
data2.1$location[data2.1$location == "Taiwan (Province of China)"] = 'Taiwan (province of China)'
data2.1$location[data2.1$location == "Republic of Korea"] = 'South Korea'
data2.1$location[data2.1$location == "United Republic of Tanzania"] = 'Tanzania'
data2.1$location[data2.1$location == "C么te d'Ivoire"] = 'Saint Helena'
data2.1$location[data2.1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
data2.1$location[data2.1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'

data2.1$location[data2.1$location == "Republic of Moldova"] = 'Moldova'

data2.1$location[data2.1$location == "Lao People's Democratic Republic"] = 'Laos'
data2.1$location[data2.1$location == "Syrian Arab Republic"] = 'Syria'

data2.1$location[data2.1$location == "Brunei Darussalam"] = 'Brunei'
data2.1$location[data2.1$location == "Gambia"] = 'The Gambia'
data2.1$location[data2.1$location == "United States of America"] = 'USA'
data2.1$location[data2.1$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
data2.1$location[data2.1$location == "Bahamas"] = 'The Bahamas'
data2.1$location[data2.1$location == "United States Virgin Islands"] = 'Virgin Islands'
data2.1$location[data2.1$location == "Macedonia"] = 'North Macedonia'
data2.1$location[data2.1$location == 'Democratic Republic of the Congo'] = 'DR Congo'
data2.1$location[data2.1$location == 'Congo'] = 'Congo (Brazzaville)'
data2.1$location[data2.1$location == 'Sao Tome and Principe'] = 'São Tomé and Príncipe'


unique(data2.1$location)

data2.1 <- data2.1 %>% select(4,2,3)

#合并疾病和sdi数据


data2.1_ASMR_SDI_2021 <- merge(data2.1,SDI_2021,by=c('location','year'))

unique(data2.1$location)

unique(data2.1_ASMR_SDI_2021$location)

setdiff(data2.1$location,data2.1_ASMR_SDI_2021$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\2021")

write.csv(data2.1_ASMR_SDI_2021,"204个国家肺癌二手烟死亡标准化率+SDI(2021).csv",row.names = F)


#作图

b <- cor.test(as.numeric(data2.1_ASMR_SDI_2021$ASMR),as.numeric(data2.1_ASMR_SDI_2021$SDI),method = "pearson")


fig2 <- ggplot(data2.1_ASMR_SDI_2021, aes(SDI,ASMR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  labs(title = NULL,x=NULL,y=NULL)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 1,size = 15,color = "black"),
        axis.title.x = element_text(size = 17),  # X轴标题样式  
        axis.text.y = element_text(vjust = 1,size = 15,color = "black"),
        axis.title.y = element_text(size = 17),  # Y轴标题样式  
        title = element_text(size = 12,hjust = 0.5))+theme(legend.position="none")+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR), #定义添加的文本和其位置
           label=paste("R=",round ( b[["estimate"]] ,3),sep = ""),
           size=8,color="black" )+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR)*0.95,
           label=paste("p<0.01"),
           size=8,color="black")+
  xlab("SDI")+
  ylab("ASMR(per 100,000)")

fig2
setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\2021")
ggsave("204个国家肺癌二手烟死亡标准化率SDI相关性分析2021.JPEG",width = 12,height=8,dpi = 1000) #使用ggsave()函数保存，图片质量好

#"Age-standardized prevalence rate （per 100000 populations)"





#筛选出男的
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



unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出2021年的sdi


SDI_2021<-fread("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$Location)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')

SDI_2021 <- SDI_2021[SDI_2021$year== 2021,] ###只取2021年的数据


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'


#获取肺癌吸烟死亡标准化率2021年数据

colnames(data1.1)
unique(data1.1$year)

unique(data1.1$sex_name)

data2.1 <- data1.1 %>% filter(year=="2021") %>% 
  filter(sex_name=="Male")

colnames(data2.1)

data2.1 <- data2.1[,c(4,10,11)]

names(data2.1)[3] <- 'ASMR'


### 调整疾病数据里的location，使其与SDI数据中location的命名一致
data2.1$location[data2.1$location == "Democratic People's Republic of Korea"] = 'North Korea'
data2.1$location[data2.1$location == 'Russian Federation'] = 'Russia'
data2.1$location[data2.1$location == 'United Kingdom'] = 'UK'
data2.1$location[data2.1$location == "Iran (Islamic Republic of)"] = 'Iran'
data2.1$location[data2.1$location == "Taiwan (Province of China)"] = 'Taiwan (province of China)'
data2.1$location[data2.1$location == "Republic of Korea"] = 'South Korea'
data2.1$location[data2.1$location == "United Republic of Tanzania"] = 'Tanzania'
data2.1$location[data2.1$location == "C么te d'Ivoire"] = 'Saint Helena'
data2.1$location[data2.1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
data2.1$location[data2.1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'

data2.1$location[data2.1$location == "Republic of Moldova"] = 'Moldova'

data2.1$location[data2.1$location == "Lao People's Democratic Republic"] = 'Laos'
data2.1$location[data2.1$location == "Syrian Arab Republic"] = 'Syria'

data2.1$location[data2.1$location == "Brunei Darussalam"] = 'Brunei'
data2.1$location[data2.1$location == "Gambia"] = 'The Gambia'
data2.1$location[data2.1$location == "United States of America"] = 'USA'
data2.1$location[data2.1$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
data2.1$location[data2.1$location == "Bahamas"] = 'The Bahamas'
data2.1$location[data2.1$location == "United States Virgin Islands"] = 'Virgin Islands'
data2.1$location[data2.1$location == "Macedonia"] = 'North Macedonia'
data2.1$location[data2.1$location == 'Democratic Republic of the Congo'] = 'DR Congo'
data2.1$location[data2.1$location == 'Congo'] = 'Congo (Brazzaville)'
data2.1$location[data2.1$location == 'Sao Tome and Principe'] = 'São Tomé and Príncipe'


unique(data2.1$location)

data2.1 <- data2.1 %>% select(4,2,3)

#合并疾病和sdi数据


data2.1_ASMR_SDI_2021 <- merge(data2.1,SDI_2021,by=c('location','year'))

unique(data2.1$location)

unique(data2.1_ASMR_SDI_2021$location)

setdiff(data2.1$location,data2.1_ASMR_SDI_2021$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\2021")


write.csv(data2.1_ASMR_SDI_2021,"204个国家肺癌二手烟死亡标准化率+SDI(2021男).csv",row.names = F)


#作图

b <- cor.test(as.numeric(data2.1_ASMR_SDI_2021$ASMR),as.numeric(data2.1_ASMR_SDI_2021$SDI),method = "pearson")


fig2 <- ggplot(data2.1_ASMR_SDI_2021, aes(SDI,ASMR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  labs(title = NULL,x=NULL,y=NULL)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 1,size = 15,color = "black"),
        axis.title.x = element_text(size = 17),  # X轴标题样式  
        axis.text.y = element_text(vjust = 1,size = 15,color = "black"),
        axis.title.y = element_text(size = 17),  # X轴标题样式  
        title = element_text(size = 12,hjust = 0.5))+theme(legend.position="none")+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR), #定义添加的文本和其位置
           label=paste("R=",round ( b[["estimate"]] ,3),sep = ""),
           size=8,color="black" )+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR)*0.95,
           label=paste("p<0.01"),
           size=8,color="black")+
  xlab("SDI")+
  ylab("ASMR(per 100,000)")

fig2
setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\2021")

ggsave("204个国家肺癌二手烟死亡标准化率男SDI相关性分析2021.JPEG",width = 12,height=8,dpi = 1000) #使用ggsave()函数保存，图片质量好





#筛选出女的


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



unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出2021年的sdi


SDI_2021<-fread("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$Location)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')

SDI_2021 <- SDI_2021[SDI_2021$year== 2021,] ###只取2021年的数据


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'


#获取肺癌吸烟死亡标准化率2021年数据

colnames(data1.1)
unique(data1.1$year)

unique(data1.1$sex_name)

data2.1 <- data1.1 %>% filter(year=="2021") %>% 
  filter(sex_name=="Female")

colnames(data2.1)

data2.1 <- data2.1[,c(4,10,11)]

names(data2.1)[3] <- 'ASMR'


### 调整疾病数据里的location，使其与SDI数据中location的命名一致
data2.1$location[data2.1$location == "Democratic People's Republic of Korea"] = 'North Korea'
data2.1$location[data2.1$location == 'Russian Federation'] = 'Russia'
data2.1$location[data2.1$location == 'United Kingdom'] = 'UK'
data2.1$location[data2.1$location == "Iran (Islamic Republic of)"] = 'Iran'
data2.1$location[data2.1$location == "Taiwan (Province of China)"] = 'Taiwan (province of China)'
data2.1$location[data2.1$location == "Republic of Korea"] = 'South Korea'
data2.1$location[data2.1$location == "United Republic of Tanzania"] = 'Tanzania'
data2.1$location[data2.1$location == "C么te d'Ivoire"] = 'Saint Helena'
data2.1$location[data2.1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
data2.1$location[data2.1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'

data2.1$location[data2.1$location == "Republic of Moldova"] = 'Moldova'

data2.1$location[data2.1$location == "Lao People's Democratic Republic"] = 'Laos'
data2.1$location[data2.1$location == "Syrian Arab Republic"] = 'Syria'

data2.1$location[data2.1$location == "Brunei Darussalam"] = 'Brunei'
data2.1$location[data2.1$location == "Gambia"] = 'The Gambia'
data2.1$location[data2.1$location == "United States of America"] = 'USA'
data2.1$location[data2.1$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
data2.1$location[data2.1$location == "Bahamas"] = 'The Bahamas'
data2.1$location[data2.1$location == "United States Virgin Islands"] = 'Virgin Islands'
data2.1$location[data2.1$location == "Macedonia"] = 'North Macedonia'
data2.1$location[data2.1$location == 'Democratic Republic of the Congo'] = 'DR Congo'
data2.1$location[data2.1$location == 'Congo'] = 'Congo (Brazzaville)'
data2.1$location[data2.1$location == 'Sao Tome and Principe'] = 'São Tomé and Príncipe'


unique(data2.1$location)

data2.1 <- data2.1 %>% select(4,2,3)

#合并疾病和sdi数据


data2.1_ASMR_SDI_2021 <- merge(data2.1,SDI_2021,by=c('location','year'))

unique(data2.1$location)

unique(data2.1_ASMR_SDI_2021$location)

setdiff(data2.1$location,data2.1_ASMR_SDI_2021$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\2021")

write.csv(data2.1_ASMR_SDI_2021,"204个国家肺癌二手烟死亡标准化率+SDI(2021女).csv",row.names = F)


#作图

b <- cor.test(as.numeric(data2.1_ASMR_SDI_2021$ASMR),as.numeric(data2.1_ASMR_SDI_2021$SDI),method = "pearson")


fig2 <- ggplot(data2.1_ASMR_SDI_2021, aes(SDI,ASMR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  labs(title = NULL,x=NULL,y=NULL)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 1,size = 15,color = "black"),
        axis.title.x = element_text(size = 17),  # X轴标题样式  
        axis.text.y = element_text(vjust = 1,size = 15,color = "black"),
        axis.title.y = element_text(size = 17),  # X轴标题样式  
        title = element_text(size = 12,hjust = 0.5))+theme(legend.position="none")+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR), #定义添加的文本和其位置
           label=paste("R=",round ( b[["estimate"]] ,3),sep = ""),
           size=8,color="black" )+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR)*0.95,
           label=paste("p<0.01"),
           size=8,color="black")+
  xlab("SDI")+
  ylab("ASMR(per 100,000)")

fig2
setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\2021")

ggsave("204个国家肺癌二手烟死亡标准化率女SDI相关性分析2021.JPEG",width = 12,height=8,dpi = 1000) #使用ggsave()函数保存，图片质量好








# global+21个地区肺癌吸烟dalys标准化率相关性分析 ------------------------------------------

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\吸烟")

data <- fread("肺癌global+21个地区吸烟dalys标准化率.csv")

year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data<- data %>% filter(year%in%year1)



unique(data$year)


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性")

order_SDI <- read.csv('order_SDI.csv',header = F)

SDI_2021<-fread("SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$year)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'

#筛选变量

colnames(data)

unique(data$sex_name)
unique(data$age_name)
unique(data$year)

data <- data %>% filter(sex_name=="Both") %>% select(-1)

colnames(data)
data_ASMR <- data[,c(3,9,10)] ###选择location year val

names(data_ASMR)[3] <- 'ASDR'
names(data_ASMR)[1] <- 'location'



### 合并SDI与ASR数据
data_ASMR_SDI <- merge(data_ASMR,SDI_2021,by=c('location','year'))

unique(data_ASMR_SDI$location)

unique(data_ASMR_SDI$year)

data_ASMR_SDI$location <- factor(data_ASMR_SDI$location, 
                                 levels=order_SDI$V1, 
                                 ordered=TRUE) ## location图例按照我们的顺序排列

str(data_ASMR_SDI$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\吸烟")

write.csv(data_ASMR_SDI,"肺癌global+21个地区吸烟dalys标准化率+SDI.csv") 

getwd()


##开始作图，主变量为ASMR以及SDI,图形的颜色和形状根据不同区域来调整即可
### 同时以所有数据画出拟合曲线

a <- cor.test(as.numeric(data_ASMR_SDI$ASMR),as.numeric(data_ASMR_SDI$SDI),method = "pearson")

fig1 <- ggplot(data_ASMR_SDI, aes(SDI,ASMR)) + geom_point(aes(color = location, shape= location))+
  scale_shape_manual(values = 1:22) + 
  annotate("text",x=0.25,y=max(data_ASMR_SDI$ASMR), #定义添加的文本和其位置
           label=paste("R=",round ( a[["estimate"]] ,3),sep = ""),
           size=4,color="black" )+
  annotate("text",x=0.25,y=max(data_ASMR_SDI$ASMR)*0.95,
           label=paste("p=",round ( a[["p.value"]] ,3),sep = ""),
           size=4,color="black")+
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) 

fig1




# 提取204个国家肺癌吸烟dalys标准化率，相关性分析 ---------------------------------------------


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病DALYS数据\\204个国家\\吸烟")


data1.1 <- fread("肺癌的吸烟DALYS标准化率（204个国家加5个sdi地区）.csv")

year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data1.1<- data1.1 %>% filter(year%in%year1)



unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出2021年的sdi


SDI_2021<-fread("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$Location)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')

SDI_2021 <- SDI_2021[SDI_2021$year== 2021,] ###只取2021年的数据


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'


#获取肺癌吸烟死亡标准化率2021年数据

colnames(data1.1)
unique(data1.1$year)

data2.1 <- data1.1 %>% filter(year=="2021") %>% 
  filter(sex_name=="Both")

colnames(data2.1)

data2.1 <- data2.1[,c(4,10,11)]

names(data2.1)[3] <- 'ASDR'


### 调整疾病数据里的location，使其与SDI数据中location的命名一致
data2.1$location[data2.1$location == "Democratic People's Republic of Korea"] = 'North Korea'
data2.1$location[data2.1$location == 'Russian Federation'] = 'Russia'
data2.1$location[data2.1$location == 'United Kingdom'] = 'UK'
data2.1$location[data2.1$location == "Iran (Islamic Republic of)"] = 'Iran'
data2.1$location[data2.1$location == "Taiwan (Province of China)"] = 'Taiwan (province of China)'
data2.1$location[data2.1$location == "Republic of Korea"] = 'South Korea'
data2.1$location[data2.1$location == "United Republic of Tanzania"] = 'Tanzania'
data2.1$location[data2.1$location == "C么te d'Ivoire"] = 'Saint Helena'
data2.1$location[data2.1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
data2.1$location[data2.1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'

data2.1$location[data2.1$location == "Republic of Moldova"] = 'Moldova'

data2.1$location[data2.1$location == "Lao People's Democratic Republic"] = 'Laos'
data2.1$location[data2.1$location == "Syrian Arab Republic"] = 'Syria'

data2.1$location[data2.1$location == "Brunei Darussalam"] = 'Brunei'
data2.1$location[data2.1$location == "Gambia"] = 'The Gambia'
data2.1$location[data2.1$location == "United States of America"] = 'USA'
data2.1$location[data2.1$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
data2.1$location[data2.1$location == "Bahamas"] = 'The Bahamas'
data2.1$location[data2.1$location == "United States Virgin Islands"] = 'Virgin Islands'
data2.1$location[data2.1$location == "Macedonia"] = 'North Macedonia'
data2.1$location[data2.1$location == 'Democratic Republic of the Congo'] = 'DR Congo'
data2.1$location[data2.1$location == 'Congo'] = 'Congo (Brazzaville)'
data2.1$location[data2.1$location == 'Sao Tome and Principe'] = 'São Tomé and Príncipe'


unique(data2.1$location)

data2.1 <- data2.1 %>% select(4,2,3)

#合并疾病和sdi数据


data2.1_ASMR_SDI_2021 <- merge(data2.1,SDI_2021,by=c('location','year'))

unique(data2.1$location)

unique(data2.1_ASMR_SDI_2021$location)

setdiff(data2.1$location,data2.1_ASMR_SDI_2021$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\吸烟")

write.csv(data2.1_ASMR_SDI_2021,"204个国家肺癌吸烟dalys标准化率+SDI(2021).csv",row.names = F)


#作图

b <- cor.test(as.numeric(data2.1_ASMR_SDI_2021$ASMR),as.numeric(data2.1_ASMR_SDI_2021$SDI),method = "pearson")


fig2 <- ggplot(data2.1_ASMR_SDI_2021, aes(SDI,ASMR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  labs(title = NULL,x=NULL,y=NULL)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 1,size = 9,color = "black"),
        axis.text.y = element_text(vjust = 1,size = 9,color = "black"),
        title = element_text(size = 12,hjust = 0.5))+theme(legend.position="none")+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR), #定义添加的文本和其位置
           label=paste("R=",round ( b[["estimate"]] ,3),sep = ""),
           size=4,color="black" )+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR)*0.95,
           label=paste("p=",round ( b[["p.value"]] ,3),sep = ""),
           size=4,color="black")+
  xlab("SDI")+
  ylab("Age-standardized prevalence rate （per 100000 populations)")

fig2







# global+21个地区肺癌二手烟dalys标准化率相关性分析 -----------------------------------------

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟")

data <- fread("肺癌global+21个地区二手烟dalys标准化率.csv")

year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data<- data %>% filter(year%in%year1)



unique(data$year)


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性")

order_SDI <- read.csv('order_SDI.csv',header = F)

SDI_2021<-fread("SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$year)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'

#筛选变量

colnames(data)

unique(data$sex_name)
unique(data$age_name)
unique(data$year)

data <- data %>% filter(sex_name=="Both") %>% select(-1)

colnames(data)
data_ASMR <- data[,c(3,9,10)] ###选择location year val

names(data_ASMR)[3] <- 'ASDR'
names(data_ASMR)[1] <- 'location'



### 合并SDI与ASR数据
data_ASMR_SDI <- merge(data_ASMR,SDI_2021,by=c('location','year'))

unique(data_ASMR_SDI$location)

unique(data_ASMR_SDI$year)

data_ASMR_SDI$location <- factor(data_ASMR_SDI$location, 
                                 levels=order_SDI$V1, 
                                 ordered=TRUE) ## location图例按照我们的顺序排列

str(data_ASMR_SDI$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟")

write.csv(data_ASMR_SDI,"肺癌global+21个地区二手烟dalys标准化率+SDI.csv") 

getwd()


##开始作图，主变量为ASMR以及SDI,图形的颜色和形状根据不同区域来调整即可
### 同时以所有数据画出拟合曲线

a <- cor.test(as.numeric(data_ASMR_SDI$ASDR),as.numeric(data_ASMR_SDI$SDI),method = "pearson")

fig1 <- ggplot(data_ASMR_SDI, aes(SDI,ASDR)) + geom_point(aes(color = location, shape= location))+
  theme_bw()+
  scale_shape_manual(values = 1:22) + 
  annotate("text",x=0.25,y=max(data_ASMR_SDI$ASDR), #定义添加的文本和其位置
           label=paste("R=",round ( a[["estimate"]] ,3),sep = ""),
           size=4,color="black" )+
  annotate("text",x=0.25,y=max(data_ASMR_SDI$ASDR)*0.95,
           label=paste("p<0.01"),
           size=4,color="black")+
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) 

fig1


ggsave("肺癌global+21个地区二手烟dalys标准化率SDI相关性分析.JPEG",width = 8,height=10,dpi = 300) #使用ggsave()函数保存，图片质量好



# 提取204个国家肺癌二手烟dalys标准化率，相关性分析 --------------------------------------------

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病DALYS数据\\204个国家\\二手烟")


data1.1 <- fread("肺癌的二手烟DALYS标准化率（204个国家加5个sdi地区）.csv")

year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data1.1<- data1.1 %>% filter(year%in%year1)



unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出2021年的sdi


SDI_2021<-fread("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$Location)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')

SDI_2021 <- SDI_2021[SDI_2021$year== 2021,] ###只取2021年的数据


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'


#获取肺癌吸烟死亡标准化率2021年数据

colnames(data1.1)
unique(data1.1$year)

data2.1 <- data1.1 %>% filter(year=="2021") %>% 
  filter(sex_name=="Both")

colnames(data2.1)

data2.1 <- data2.1[,c(4,10,11)]

names(data2.1)[3] <- 'ASDR'


### 调整疾病数据里的location，使其与SDI数据中location的命名一致
data2.1$location[data2.1$location == "Democratic People's Republic of Korea"] = 'North Korea'
data2.1$location[data2.1$location == 'Russian Federation'] = 'Russia'
data2.1$location[data2.1$location == 'United Kingdom'] = 'UK'
data2.1$location[data2.1$location == "Iran (Islamic Republic of)"] = 'Iran'
data2.1$location[data2.1$location == "Taiwan (Province of China)"] = 'Taiwan (province of China)'
data2.1$location[data2.1$location == "Republic of Korea"] = 'South Korea'
data2.1$location[data2.1$location == "United Republic of Tanzania"] = 'Tanzania'
data2.1$location[data2.1$location == "C么te d'Ivoire"] = 'Saint Helena'
data2.1$location[data2.1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
data2.1$location[data2.1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'

data2.1$location[data2.1$location == "Republic of Moldova"] = 'Moldova'

data2.1$location[data2.1$location == "Lao People's Democratic Republic"] = 'Laos'
data2.1$location[data2.1$location == "Syrian Arab Republic"] = 'Syria'

data2.1$location[data2.1$location == "Brunei Darussalam"] = 'Brunei'
data2.1$location[data2.1$location == "Gambia"] = 'The Gambia'
data2.1$location[data2.1$location == "United States of America"] = 'USA'
data2.1$location[data2.1$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
data2.1$location[data2.1$location == "Bahamas"] = 'The Bahamas'
data2.1$location[data2.1$location == "United States Virgin Islands"] = 'Virgin Islands'
data2.1$location[data2.1$location == "Macedonia"] = 'North Macedonia'
data2.1$location[data2.1$location == 'Democratic Republic of the Congo'] = 'DR Congo'
data2.1$location[data2.1$location == 'Congo'] = 'Congo (Brazzaville)'
data2.1$location[data2.1$location == 'Sao Tome and Principe'] = 'São Tomé and Príncipe'


unique(data2.1$location)

data2.1 <- data2.1 %>% select(4,2,3)

#合并疾病和sdi数据


data2.1_ASMR_SDI_2021 <- merge(data2.1,SDI_2021,by=c('location','year'))

unique(data2.1$location)

unique(data2.1_ASMR_SDI_2021$location)

setdiff(data2.1$location,data2.1_ASMR_SDI_2021$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\2021")

write.csv(data2.1_ASMR_SDI_2021,"204个国家肺癌二手烟dalys标准化率+SDI(2021).csv",row.names = F)


#作图

b <- cor.test(as.numeric(data2.1_ASMR_SDI_2021$ASDR),as.numeric(data2.1_ASMR_SDI_2021$SDI),method = "pearson")


fig2 <- ggplot(data2.1_ASMR_SDI_2021, aes(SDI,ASDR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  labs(title = NULL,x=NULL,y=NULL)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 1,size = 15,color = "black"),
        axis.title.x = element_text(size = 17),  # Y轴标题样式  
        axis.text.y = element_text(vjust = 1,size = 15,color = "black"),
        axis.title.y = element_text(size = 17),  # Y轴标题样式  
        title = element_text(size = 12,hjust = 0.5))+theme(legend.position="none")+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASDR), #定义添加的文本和其位置
           label=paste("R=",round ( b[["estimate"]] ,3),sep = ""),
           size=8,color="black" )+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASDR)*0.95,
           label=paste("p<0.01"),
           size=8,color="black")+
  xlab("SDI")+
  ylab("ASDR(per 100,000)")

fig2

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\2021")

ggsave("204个国家肺癌二手烟dalys标准化率SDI相关性分析2021.JPEG",width = 12,height=8,dpi = 1000) #使用ggsave()函数保存，图片质量好





#筛选出男的ASDR



setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病DALYS数据\\204个国家\\二手烟")


data1.1 <- fread("肺癌的二手烟DALYS标准化率（204个国家加5个sdi地区）.csv")

year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data1.1<- data1.1 %>% filter(year%in%year1)



unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出2021年的sdi


SDI_2021<-fread("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$Location)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')

SDI_2021 <- SDI_2021[SDI_2021$year== 2021,] ###只取2021年的数据


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'


#获取肺癌吸烟死亡标准化率2021年数据

colnames(data1.1)
unique(data1.1$year)

unique(data1.1$sex_name)


data2.1 <- data1.1 %>% filter(year=="2021") %>% 
  filter(sex_name=="Male")

colnames(data2.1)

data2.1 <- data2.1[,c(4,10,11)]

names(data2.1)[3] <- 'ASDR'


### 调整疾病数据里的location，使其与SDI数据中location的命名一致
data2.1$location[data2.1$location == "Democratic People's Republic of Korea"] = 'North Korea'
data2.1$location[data2.1$location == 'Russian Federation'] = 'Russia'
data2.1$location[data2.1$location == 'United Kingdom'] = 'UK'
data2.1$location[data2.1$location == "Iran (Islamic Republic of)"] = 'Iran'
data2.1$location[data2.1$location == "Taiwan (Province of China)"] = 'Taiwan (province of China)'
data2.1$location[data2.1$location == "Republic of Korea"] = 'South Korea'
data2.1$location[data2.1$location == "United Republic of Tanzania"] = 'Tanzania'
data2.1$location[data2.1$location == "C么te d'Ivoire"] = 'Saint Helena'
data2.1$location[data2.1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
data2.1$location[data2.1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'

data2.1$location[data2.1$location == "Republic of Moldova"] = 'Moldova'

data2.1$location[data2.1$location == "Lao People's Democratic Republic"] = 'Laos'
data2.1$location[data2.1$location == "Syrian Arab Republic"] = 'Syria'

data2.1$location[data2.1$location == "Brunei Darussalam"] = 'Brunei'
data2.1$location[data2.1$location == "Gambia"] = 'The Gambia'
data2.1$location[data2.1$location == "United States of America"] = 'USA'
data2.1$location[data2.1$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
data2.1$location[data2.1$location == "Bahamas"] = 'The Bahamas'
data2.1$location[data2.1$location == "United States Virgin Islands"] = 'Virgin Islands'
data2.1$location[data2.1$location == "Macedonia"] = 'North Macedonia'
data2.1$location[data2.1$location == 'Democratic Republic of the Congo'] = 'DR Congo'
data2.1$location[data2.1$location == 'Congo'] = 'Congo (Brazzaville)'
data2.1$location[data2.1$location == 'Sao Tome and Principe'] = 'São Tomé and Príncipe'


unique(data2.1$location)

data2.1 <- data2.1 %>% select(4,2,3)

#合并疾病和sdi数据


data2.1_ASMR_SDI_2021 <- merge(data2.1,SDI_2021,by=c('location','year'))

unique(data2.1$location)

unique(data2.1_ASMR_SDI_2021$location)

setdiff(data2.1$location,data2.1_ASMR_SDI_2021$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\2021")

write.csv(data2.1_ASMR_SDI_2021,"204个国家肺癌二手烟dalys标准化率+SDI(2021男).csv",row.names = F)


#作图

b <- cor.test(as.numeric(data2.1_ASMR_SDI_2021$ASDR),as.numeric(data2.1_ASMR_SDI_2021$SDI),method = "pearson")


fig2 <- ggplot(data2.1_ASMR_SDI_2021, aes(SDI,ASDR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  labs(title = NULL,x=NULL,y=NULL)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 1,size = 15,color = "black"),
        axis.title.x = element_text(size = 17),  # X轴标题样式  
        axis.text.y = element_text(vjust = 1,size = 15,color = "black"),
        axis.title.y = element_text(size = 17),  # X轴标题样式  
        title = element_text(size = 12,hjust = 0.5))+theme(legend.position="none")+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASDR), #定义添加的文本和其位置
           label=paste("R=",round ( b[["estimate"]] ,3),sep = ""),
           size=8,color="black" )+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASDR)*0.95,
           label=paste("p<0.01"),
           size=8,color="black")+
  xlab("SDI")+
  ylab("ASDR(per 100,000)")

fig2

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\2021")

ggsave("204个国家肺癌二手烟dalys标准化率男SDI相关性分析2021.JPEG",width = 12,height=8,dpi = 1000) #使用ggsave()函数保存，图片质量好



#筛选出女的



setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病DALYS数据\\204个国家\\二手烟")


data1.1 <- fread("肺癌的二手烟DALYS标准化率（204个国家加5个sdi地区）.csv")

year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data1.1<- data1.1 %>% filter(year%in%year1)



unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出2021年的sdi


SDI_2021<-fread("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$Location)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')

SDI_2021 <- SDI_2021[SDI_2021$year== 2021,] ###只取2021年的数据


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'


#获取肺癌吸烟死亡标准化率2021年数据

colnames(data1.1)
unique(data1.1$year)

unique(data1.1$sex_name)


data2.1 <- data1.1 %>% filter(year=="2021") %>% 
  filter(sex_name=="Female")

colnames(data2.1)

data2.1 <- data2.1[,c(4,10,11)]

names(data2.1)[3] <- 'ASDR'


### 调整疾病数据里的location，使其与SDI数据中location的命名一致
data2.1$location[data2.1$location == "Democratic People's Republic of Korea"] = 'North Korea'
data2.1$location[data2.1$location == 'Russian Federation'] = 'Russia'
data2.1$location[data2.1$location == 'United Kingdom'] = 'UK'
data2.1$location[data2.1$location == "Iran (Islamic Republic of)"] = 'Iran'
data2.1$location[data2.1$location == "Taiwan (Province of China)"] = 'Taiwan (province of China)'
data2.1$location[data2.1$location == "Republic of Korea"] = 'South Korea'
data2.1$location[data2.1$location == "United Republic of Tanzania"] = 'Tanzania'
data2.1$location[data2.1$location == "C么te d'Ivoire"] = 'Saint Helena'
data2.1$location[data2.1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
data2.1$location[data2.1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'

data2.1$location[data2.1$location == "Republic of Moldova"] = 'Moldova'

data2.1$location[data2.1$location == "Lao People's Democratic Republic"] = 'Laos'
data2.1$location[data2.1$location == "Syrian Arab Republic"] = 'Syria'

data2.1$location[data2.1$location == "Brunei Darussalam"] = 'Brunei'
data2.1$location[data2.1$location == "Gambia"] = 'The Gambia'
data2.1$location[data2.1$location == "United States of America"] = 'USA'
data2.1$location[data2.1$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
data2.1$location[data2.1$location == "Bahamas"] = 'The Bahamas'
data2.1$location[data2.1$location == "United States Virgin Islands"] = 'Virgin Islands'
data2.1$location[data2.1$location == "Macedonia"] = 'North Macedonia'
data2.1$location[data2.1$location == 'Democratic Republic of the Congo'] = 'DR Congo'
data2.1$location[data2.1$location == 'Congo'] = 'Congo (Brazzaville)'
data2.1$location[data2.1$location == 'Sao Tome and Principe'] = 'São Tomé and Príncipe'


unique(data2.1$location)

data2.1 <- data2.1 %>% select(4,2,3)

#合并疾病和sdi数据


data2.1_ASMR_SDI_2021 <- merge(data2.1,SDI_2021,by=c('location','year'))

unique(data2.1$location)

unique(data2.1_ASMR_SDI_2021$location)

setdiff(data2.1$location,data2.1_ASMR_SDI_2021$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\2021")

write.csv(data2.1_ASMR_SDI_2021,"204个国家肺癌二手烟dalys标准化率+SDI(2021女).csv",row.names = F)


#作图

b <- cor.test(as.numeric(data2.1_ASMR_SDI_2021$ASDR),as.numeric(data2.1_ASMR_SDI_2021$SDI),method = "pearson")


fig2 <- ggplot(data2.1_ASMR_SDI_2021, aes(SDI,ASDR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  labs(title = NULL,x=NULL,y=NULL)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 1,size = 15,color = "black"),
        axis.title.x = element_text(size = 17),  # X轴标题样式  
        axis.text.y = element_text(vjust = 1,size = 15,color = "black"),
        axis.title.y = element_text(size = 17),  # X轴标题样式  
        title = element_text(size = 12,hjust = 0.5))+theme(legend.position="none")+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASDR), #定义添加的文本和其位置
           label=paste("R=0.400"),
           size=8,color="black" )+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASDR)*0.95,
           label=paste("p<0.01"),
           size=8,color="black")+
  xlab("SDI")+
  ylab("ASDR(per 100,000)")

fig2

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\2021")

ggsave("204个国家肺癌二手烟dalys标准化率女SDI相关性分析2021.JPEG",width = 12,height=8,dpi =1000) #使用ggsave()函数保存，图片质量好









# 提取204个国家肺癌二手烟标准化死亡率，相关性分析1992 -------------------------------------------

#筛选出总的
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



unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出1992年的sdi


SDI_2021<-fread("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$Location)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')

SDI_2021 <- SDI_2021[SDI_2021$year== 1992,] ###只取1992年的数据


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'


#获取肺癌二手烟死亡标准化率1992年数据

colnames(data1.1)
unique(data1.1$year)

data2.1 <- data1.1 %>% filter(year=="1992") %>% 
  filter(sex_name=="Both")

colnames(data2.1)

data2.1 <- data2.1[,c(4,10,11)]

names(data2.1)[3] <- 'ASMR'


### 调整疾病数据里的location，使其与SDI数据中location的命名一致
data2.1$location[data2.1$location == "Democratic People's Republic of Korea"] = 'North Korea'
data2.1$location[data2.1$location == 'Russian Federation'] = 'Russia'
data2.1$location[data2.1$location == 'United Kingdom'] = 'UK'
data2.1$location[data2.1$location == "Iran (Islamic Republic of)"] = 'Iran'
data2.1$location[data2.1$location == "Taiwan (Province of China)"] = 'Taiwan (province of China)'
data2.1$location[data2.1$location == "Republic of Korea"] = 'South Korea'
data2.1$location[data2.1$location == "United Republic of Tanzania"] = 'Tanzania'
data2.1$location[data2.1$location == "C么te d'Ivoire"] = 'Saint Helena'
data2.1$location[data2.1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
data2.1$location[data2.1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'

data2.1$location[data2.1$location == "Republic of Moldova"] = 'Moldova'

data2.1$location[data2.1$location == "Lao People's Democratic Republic"] = 'Laos'
data2.1$location[data2.1$location == "Syrian Arab Republic"] = 'Syria'

data2.1$location[data2.1$location == "Brunei Darussalam"] = 'Brunei'
data2.1$location[data2.1$location == "Gambia"] = 'The Gambia'
data2.1$location[data2.1$location == "United States of America"] = 'USA'
data2.1$location[data2.1$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
data2.1$location[data2.1$location == "Bahamas"] = 'The Bahamas'
data2.1$location[data2.1$location == "United States Virgin Islands"] = 'Virgin Islands'
data2.1$location[data2.1$location == "Macedonia"] = 'North Macedonia'
data2.1$location[data2.1$location == 'Democratic Republic of the Congo'] = 'DR Congo'
data2.1$location[data2.1$location == 'Congo'] = 'Congo (Brazzaville)'
data2.1$location[data2.1$location == 'Sao Tome and Principe'] = 'São Tomé and Príncipe'


unique(data2.1$location)

data2.1 <- data2.1 %>% select(4,2,3)

#合并疾病和sdi数据


data2.1_ASMR_SDI_2021 <- merge(data2.1,SDI_2021,by=c('location','year'))

unique(data2.1$location)

unique(data2.1_ASMR_SDI_2021$location)

setdiff(data2.1$location,data2.1_ASMR_SDI_2021$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\1992")

write.csv(data2.1_ASMR_SDI_2021,"204个国家肺癌二手烟死亡标准化率+SDI(1992).csv",row.names = F)


#作图

b <- cor.test(as.numeric(data2.1_ASMR_SDI_2021$ASMR),as.numeric(data2.1_ASMR_SDI_2021$SDI),method = "pearson")


fig2 <- ggplot(data2.1_ASMR_SDI_2021, aes(SDI,ASMR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  labs(title = NULL,x=NULL,y=NULL)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 1,size = 9,color = "black"),
        axis.text.y = element_text(vjust = 1,size = 9,color = "black"),
        title = element_text(size = 12,hjust = 0.5))+theme(legend.position="none")+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR), #定义添加的文本和其位置
           label=paste("R=",round ( b[["estimate"]] ,3),sep = ""),
           size=4,color="black" )+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR)*0.95,
           label=paste("p<0.01"),
           size=4,color="black")+
  xlab("SDI")+
  ylab("ASMR")

fig2

ggsave("204个国家肺癌二手烟死亡标准化率SDI相关性分析1992.JPEG",width = 8,height=10,dpi = 300) #使用ggsave()函数保存，图片质量好

#"Age-standardized prevalence rate （per 100000 populations)"





#筛选出男的
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



unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出1992年的sdi


SDI_2021<-fread("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$Location)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')

SDI_2021 <- SDI_2021[SDI_2021$year== 1992,] ###只取2021年的数据


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'


#获取肺癌吸烟死亡标准化率1992年数据

colnames(data1.1)
unique(data1.1$year)

unique(data1.1$sex_name)

data2.1 <- data1.1 %>% filter(year=="1992") %>% 
  filter(sex_name=="Male")

colnames(data2.1)

data2.1 <- data2.1[,c(4,10,11)]

names(data2.1)[3] <- 'ASMR'


### 调整疾病数据里的location，使其与SDI数据中location的命名一致
data2.1$location[data2.1$location == "Democratic People's Republic of Korea"] = 'North Korea'
data2.1$location[data2.1$location == 'Russian Federation'] = 'Russia'
data2.1$location[data2.1$location == 'United Kingdom'] = 'UK'
data2.1$location[data2.1$location == "Iran (Islamic Republic of)"] = 'Iran'
data2.1$location[data2.1$location == "Taiwan (Province of China)"] = 'Taiwan (province of China)'
data2.1$location[data2.1$location == "Republic of Korea"] = 'South Korea'
data2.1$location[data2.1$location == "United Republic of Tanzania"] = 'Tanzania'
data2.1$location[data2.1$location == "C么te d'Ivoire"] = 'Saint Helena'
data2.1$location[data2.1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
data2.1$location[data2.1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'

data2.1$location[data2.1$location == "Republic of Moldova"] = 'Moldova'

data2.1$location[data2.1$location == "Lao People's Democratic Republic"] = 'Laos'
data2.1$location[data2.1$location == "Syrian Arab Republic"] = 'Syria'

data2.1$location[data2.1$location == "Brunei Darussalam"] = 'Brunei'
data2.1$location[data2.1$location == "Gambia"] = 'The Gambia'
data2.1$location[data2.1$location == "United States of America"] = 'USA'
data2.1$location[data2.1$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
data2.1$location[data2.1$location == "Bahamas"] = 'The Bahamas'
data2.1$location[data2.1$location == "United States Virgin Islands"] = 'Virgin Islands'
data2.1$location[data2.1$location == "Macedonia"] = 'North Macedonia'
data2.1$location[data2.1$location == 'Democratic Republic of the Congo'] = 'DR Congo'
data2.1$location[data2.1$location == 'Congo'] = 'Congo (Brazzaville)'
data2.1$location[data2.1$location == 'Sao Tome and Principe'] = 'São Tomé and Príncipe'


unique(data2.1$location)

data2.1 <- data2.1 %>% select(4,2,3)

#合并疾病和sdi数据


data2.1_ASMR_SDI_2021 <- merge(data2.1,SDI_2021,by=c('location','year'))

unique(data2.1$location)

unique(data2.1_ASMR_SDI_2021$location)

setdiff(data2.1$location,data2.1_ASMR_SDI_2021$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\1992")

write.csv(data2.1_ASMR_SDI_2021,"204个国家肺癌二手烟死亡标准化率+SDI(1992男).csv",row.names = F)


#作图

b <- cor.test(as.numeric(data2.1_ASMR_SDI_2021$ASMR),as.numeric(data2.1_ASMR_SDI_2021$SDI),method = "pearson")


fig2 <- ggplot(data2.1_ASMR_SDI_2021, aes(SDI,ASMR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  labs(title = NULL,x=NULL,y=NULL)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 1,size = 9,color = "black"),
        axis.text.y = element_text(vjust = 1,size = 9,color = "black"),
        title = element_text(size = 12,hjust = 0.5))+theme(legend.position="none")+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR), #定义添加的文本和其位置
           label=paste("R=",round ( b[["estimate"]] ,3),sep = ""),
           size=4,color="black" )+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR)*0.95,
           label=paste("p<0.01"),
           size=4,color="black")+
  xlab("SDI")+
  ylab("ASMR")

fig2

ggsave("204个国家肺癌二手烟死亡标准化率男SDI相关性分析1992.JPEG",width = 8,height=10,dpi = 300) #使用ggsave()函数保存，图片质量好





#筛选出女的


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



unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出1992年的sdi


SDI_2021<-fread("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$Location)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')

SDI_2021 <- SDI_2021[SDI_2021$year== 1992,] ###只取2021年的数据


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'


#获取肺癌吸烟死亡标准化率1992年数据

colnames(data1.1)
unique(data1.1$year)

unique(data1.1$sex_name)

data2.1 <- data1.1 %>% filter(year=="1992") %>% 
  filter(sex_name=="Female")

colnames(data2.1)

data2.1 <- data2.1[,c(4,10,11)]

names(data2.1)[3] <- 'ASMR'


### 调整疾病数据里的location，使其与SDI数据中location的命名一致
data2.1$location[data2.1$location == "Democratic People's Republic of Korea"] = 'North Korea'
data2.1$location[data2.1$location == 'Russian Federation'] = 'Russia'
data2.1$location[data2.1$location == 'United Kingdom'] = 'UK'
data2.1$location[data2.1$location == "Iran (Islamic Republic of)"] = 'Iran'
data2.1$location[data2.1$location == "Taiwan (Province of China)"] = 'Taiwan (province of China)'
data2.1$location[data2.1$location == "Republic of Korea"] = 'South Korea'
data2.1$location[data2.1$location == "United Republic of Tanzania"] = 'Tanzania'
data2.1$location[data2.1$location == "C么te d'Ivoire"] = 'Saint Helena'
data2.1$location[data2.1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
data2.1$location[data2.1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'

data2.1$location[data2.1$location == "Republic of Moldova"] = 'Moldova'

data2.1$location[data2.1$location == "Lao People's Democratic Republic"] = 'Laos'
data2.1$location[data2.1$location == "Syrian Arab Republic"] = 'Syria'

data2.1$location[data2.1$location == "Brunei Darussalam"] = 'Brunei'
data2.1$location[data2.1$location == "Gambia"] = 'The Gambia'
data2.1$location[data2.1$location == "United States of America"] = 'USA'
data2.1$location[data2.1$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
data2.1$location[data2.1$location == "Bahamas"] = 'The Bahamas'
data2.1$location[data2.1$location == "United States Virgin Islands"] = 'Virgin Islands'
data2.1$location[data2.1$location == "Macedonia"] = 'North Macedonia'
data2.1$location[data2.1$location == 'Democratic Republic of the Congo'] = 'DR Congo'
data2.1$location[data2.1$location == 'Congo'] = 'Congo (Brazzaville)'
data2.1$location[data2.1$location == 'Sao Tome and Principe'] = 'São Tomé and Príncipe'


unique(data2.1$location)

data2.1 <- data2.1 %>% select(4,2,3)

#合并疾病和sdi数据


data2.1_ASMR_SDI_2021 <- merge(data2.1,SDI_2021,by=c('location','year'))

unique(data2.1$location)

unique(data2.1_ASMR_SDI_2021$location)

setdiff(data2.1$location,data2.1_ASMR_SDI_2021$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\1992")

write.csv(data2.1_ASMR_SDI_2021,"204个国家肺癌二手烟死亡标准化率+SDI(1992女).csv",row.names = F)


#作图

b <- cor.test(as.numeric(data2.1_ASMR_SDI_2021$ASMR),as.numeric(data2.1_ASMR_SDI_2021$SDI),method = "pearson")


fig2 <- ggplot(data2.1_ASMR_SDI_2021, aes(SDI,ASMR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  labs(title = NULL,x=NULL,y=NULL)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 1,size = 9,color = "black"),
        axis.text.y = element_text(vjust = 1,size = 9,color = "black"),
        title = element_text(size = 12,hjust = 0.5))+theme(legend.position="none")+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR), #定义添加的文本和其位置
           label=paste("R=",round ( b[["estimate"]] ,3),sep = ""),
           size=4,color="black" )+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASMR)*0.95,
           label=paste("p<0.01"),
           size=4,color="black")+
  xlab("SDI")+
  ylab("ASMR")

fig2

ggsave("204个国家肺癌二手烟死亡标准化率女SDI相关性分析1992.JPEG",width = 8,height=10,dpi = 300) #使用ggsave()函数保存，图片质量好















# 提取204个国家肺癌二手烟dalys标准化率，相关性分析1992 --------------------------------------------

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病DALYS数据\\204个国家\\二手烟")


data1.1 <- fread("肺癌的二手烟DALYS标准化率（204个国家加5个sdi地区）.csv")

year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data1.1<- data1.1 %>% filter(year%in%year1)



unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出1992年的sdi


SDI_2021<-fread("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$Location)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')

SDI_2021 <- SDI_2021[SDI_2021$year== 1992,] ###只取2021年的数据


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'


#获取肺癌吸烟死亡标准化率1992年数据

colnames(data1.1)
unique(data1.1$year)

data2.1 <- data1.1 %>% filter(year=="1992") %>% 
  filter(sex_name=="Both")

colnames(data2.1)

data2.1 <- data2.1[,c(4,10,11)]

names(data2.1)[3] <- 'ASDR'


### 调整疾病数据里的location，使其与SDI数据中location的命名一致
data2.1$location[data2.1$location == "Democratic People's Republic of Korea"] = 'North Korea'
data2.1$location[data2.1$location == 'Russian Federation'] = 'Russia'
data2.1$location[data2.1$location == 'United Kingdom'] = 'UK'
data2.1$location[data2.1$location == "Iran (Islamic Republic of)"] = 'Iran'
data2.1$location[data2.1$location == "Taiwan (Province of China)"] = 'Taiwan (province of China)'
data2.1$location[data2.1$location == "Republic of Korea"] = 'South Korea'
data2.1$location[data2.1$location == "United Republic of Tanzania"] = 'Tanzania'
data2.1$location[data2.1$location == "C么te d'Ivoire"] = 'Saint Helena'
data2.1$location[data2.1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
data2.1$location[data2.1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'

data2.1$location[data2.1$location == "Republic of Moldova"] = 'Moldova'

data2.1$location[data2.1$location == "Lao People's Democratic Republic"] = 'Laos'
data2.1$location[data2.1$location == "Syrian Arab Republic"] = 'Syria'

data2.1$location[data2.1$location == "Brunei Darussalam"] = 'Brunei'
data2.1$location[data2.1$location == "Gambia"] = 'The Gambia'
data2.1$location[data2.1$location == "United States of America"] = 'USA'
data2.1$location[data2.1$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
data2.1$location[data2.1$location == "Bahamas"] = 'The Bahamas'
data2.1$location[data2.1$location == "United States Virgin Islands"] = 'Virgin Islands'
data2.1$location[data2.1$location == "Macedonia"] = 'North Macedonia'
data2.1$location[data2.1$location == 'Democratic Republic of the Congo'] = 'DR Congo'
data2.1$location[data2.1$location == 'Congo'] = 'Congo (Brazzaville)'
data2.1$location[data2.1$location == 'Sao Tome and Principe'] = 'São Tomé and Príncipe'


unique(data2.1$location)

data2.1 <- data2.1 %>% select(4,2,3)

#合并疾病和sdi数据


data2.1_ASMR_SDI_2021 <- merge(data2.1,SDI_2021,by=c('location','year'))

unique(data2.1$location)

unique(data2.1_ASMR_SDI_2021$location)

setdiff(data2.1$location,data2.1_ASMR_SDI_2021$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\1992")

write.csv(data2.1_ASMR_SDI_2021,"204个国家肺癌二手烟dalys标准化率+SDI(1992).csv",row.names = F)


#作图

b <- cor.test(as.numeric(data2.1_ASMR_SDI_2021$ASDR),as.numeric(data2.1_ASMR_SDI_2021$SDI),method = "pearson")


fig2 <- ggplot(data2.1_ASMR_SDI_2021, aes(SDI,ASDR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  labs(title = NULL,x=NULL,y=NULL)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 1,size = 9,color = "black"),
        axis.text.y = element_text(vjust = 1,size = 9,color = "black"),
        title = element_text(size = 12,hjust = 0.5))+theme(legend.position="none")+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASDR), #定义添加的文本和其位置
           label=paste("R=",round ( b[["estimate"]] ,3),sep = ""),
           size=4,color="black" )+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASDR)*0.95,
           label=paste("p<0.01"),
           size=4,color="black")+
  xlab("SDI")+
  ylab("ASDR")

fig2
ggsave("204个国家肺癌二手烟dalys标准化率SDI相关性分析1992.JPEG",width = 8,height=10,dpi = 300) #使用ggsave()函数保存，图片质量好





#筛选出男的ASDR



setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病DALYS数据\\204个国家\\二手烟")


data1.1 <- fread("肺癌的二手烟DALYS标准化率（204个国家加5个sdi地区）.csv")

year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data1.1<- data1.1 %>% filter(year%in%year1)



unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出1992年的sdi


SDI_2021<-fread("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$Location)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')

SDI_2021 <- SDI_2021[SDI_2021$year== 1992,] ###只取2021年的数据


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'


#获取肺癌吸烟死亡标准化率1992年数据

colnames(data1.1)
unique(data1.1$year)

unique(data1.1$sex_name)


data2.1 <- data1.1 %>% filter(year=="1992") %>% 
  filter(sex_name=="Male")

colnames(data2.1)

data2.1 <- data2.1[,c(4,10,11)]

names(data2.1)[3] <- 'ASDR'


### 调整疾病数据里的location，使其与SDI数据中location的命名一致
data2.1$location[data2.1$location == "Democratic People's Republic of Korea"] = 'North Korea'
data2.1$location[data2.1$location == 'Russian Federation'] = 'Russia'
data2.1$location[data2.1$location == 'United Kingdom'] = 'UK'
data2.1$location[data2.1$location == "Iran (Islamic Republic of)"] = 'Iran'
data2.1$location[data2.1$location == "Taiwan (Province of China)"] = 'Taiwan (province of China)'
data2.1$location[data2.1$location == "Republic of Korea"] = 'South Korea'
data2.1$location[data2.1$location == "United Republic of Tanzania"] = 'Tanzania'
data2.1$location[data2.1$location == "C么te d'Ivoire"] = 'Saint Helena'
data2.1$location[data2.1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
data2.1$location[data2.1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'

data2.1$location[data2.1$location == "Republic of Moldova"] = 'Moldova'

data2.1$location[data2.1$location == "Lao People's Democratic Republic"] = 'Laos'
data2.1$location[data2.1$location == "Syrian Arab Republic"] = 'Syria'

data2.1$location[data2.1$location == "Brunei Darussalam"] = 'Brunei'
data2.1$location[data2.1$location == "Gambia"] = 'The Gambia'
data2.1$location[data2.1$location == "United States of America"] = 'USA'
data2.1$location[data2.1$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
data2.1$location[data2.1$location == "Bahamas"] = 'The Bahamas'
data2.1$location[data2.1$location == "United States Virgin Islands"] = 'Virgin Islands'
data2.1$location[data2.1$location == "Macedonia"] = 'North Macedonia'
data2.1$location[data2.1$location == 'Democratic Republic of the Congo'] = 'DR Congo'
data2.1$location[data2.1$location == 'Congo'] = 'Congo (Brazzaville)'
data2.1$location[data2.1$location == 'Sao Tome and Principe'] = 'São Tomé and Príncipe'


unique(data2.1$location)

data2.1 <- data2.1 %>% select(4,2,3)

#合并疾病和sdi数据


data2.1_ASMR_SDI_2021 <- merge(data2.1,SDI_2021,by=c('location','year'))

unique(data2.1$location)

unique(data2.1_ASMR_SDI_2021$location)

setdiff(data2.1$location,data2.1_ASMR_SDI_2021$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\1992")

write.csv(data2.1_ASMR_SDI_2021,"204个国家肺癌二手烟dalys标准化率+SDI(1992男).csv",row.names = F)


#作图

b <- cor.test(as.numeric(data2.1_ASMR_SDI_2021$ASDR),as.numeric(data2.1_ASMR_SDI_2021$SDI),method = "pearson")


fig2 <- ggplot(data2.1_ASMR_SDI_2021, aes(SDI,ASDR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  labs(title = NULL,x=NULL,y=NULL)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 1,size = 9,color = "black"),
        axis.text.y = element_text(vjust = 1,size = 9,color = "black"),
        title = element_text(size = 12,hjust = 0.5))+theme(legend.position="none")+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASDR), #定义添加的文本和其位置
           label=paste("R=",round ( b[["estimate"]] ,3),sep = ""),
           size=4,color="black" )+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASDR)*0.95,
           label=paste("p<0.01"),
           size=4,color="black")+
  xlab("SDI")+
  ylab("ASDR")

fig2
ggsave("204个国家肺癌二手烟dalys标准化率男SDI相关性分析1992.JPEG",width = 8,height=10,dpi = 300) #使用ggsave()函数保存，图片质量好



#筛选出女的



setwd("C:\\Users\\dell\\Desktop\\GBD数据\\疾病DALYS数据\\204个国家\\二手烟")


data1.1 <- fread("肺癌的二手烟DALYS标准化率（204个国家加5个sdi地区）.csv")

year1 <- c( "1992", "1993", "1994",
            "1995", "1996", "1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004",
            "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", 
            "2015", "2016", "2017", "2018", "2019",
            "2020", "2021" )

data1.1<- data1.1 %>% filter(year%in%year1)



unique(data1.1$year)




unique(data1.1$location_name)

#筛选出204个国家
data1.1 <- data1.1 %>% filter(!(location_name %in% c("Global", "Low SDI","Low-middle SDI",
                                                     "High SDI","High-middle SDI","Middle SDI" )))

unique(data1.1$location_name)

unique(data1.1$year)

#筛选出1992年的sdi


SDI_2021<-fread("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\SDI_2021.csv",header = T) #注意year为1990-2021

SDI_2021<-SDI_2021[,-1]

unique(SDI_2021$Location)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))

names(SDI_2021) <- c('location','year','SDI')

SDI_2021 <- SDI_2021[SDI_2021$year== 1992,] ###只取2021年的数据


#改变量名，SDI与GBD保持一致
SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'


#获取肺癌吸烟死亡标准化率1992年数据

colnames(data1.1)
unique(data1.1$year)

unique(data1.1$sex_name)


data2.1 <- data1.1 %>% filter(year=="1992") %>% 
  filter(sex_name=="Female")

colnames(data2.1)

data2.1 <- data2.1[,c(4,10,11)]

names(data2.1)[3] <- 'ASDR'


### 调整疾病数据里的location，使其与SDI数据中location的命名一致
data2.1$location[data2.1$location == "Democratic People's Republic of Korea"] = 'North Korea'
data2.1$location[data2.1$location == 'Russian Federation'] = 'Russia'
data2.1$location[data2.1$location == 'United Kingdom'] = 'UK'
data2.1$location[data2.1$location == "Iran (Islamic Republic of)"] = 'Iran'
data2.1$location[data2.1$location == "Taiwan (Province of China)"] = 'Taiwan (province of China)'
data2.1$location[data2.1$location == "Republic of Korea"] = 'South Korea'
data2.1$location[data2.1$location == "United Republic of Tanzania"] = 'Tanzania'
data2.1$location[data2.1$location == "C么te d'Ivoire"] = 'Saint Helena'
data2.1$location[data2.1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
data2.1$location[data2.1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'

data2.1$location[data2.1$location == "Republic of Moldova"] = 'Moldova'

data2.1$location[data2.1$location == "Lao People's Democratic Republic"] = 'Laos'
data2.1$location[data2.1$location == "Syrian Arab Republic"] = 'Syria'

data2.1$location[data2.1$location == "Brunei Darussalam"] = 'Brunei'
data2.1$location[data2.1$location == "Gambia"] = 'The Gambia'
data2.1$location[data2.1$location == "United States of America"] = 'USA'
data2.1$location[data2.1$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
data2.1$location[data2.1$location == "Bahamas"] = 'The Bahamas'
data2.1$location[data2.1$location == "United States Virgin Islands"] = 'Virgin Islands'
data2.1$location[data2.1$location == "Macedonia"] = 'North Macedonia'
data2.1$location[data2.1$location == 'Democratic Republic of the Congo'] = 'DR Congo'
data2.1$location[data2.1$location == 'Congo'] = 'Congo (Brazzaville)'
data2.1$location[data2.1$location == 'Sao Tome and Principe'] = 'São Tomé and Príncipe'


unique(data2.1$location)

data2.1 <- data2.1 %>% select(4,2,3)

#合并疾病和sdi数据


data2.1_ASMR_SDI_2021 <- merge(data2.1,SDI_2021,by=c('location','year'))

unique(data2.1$location)

unique(data2.1_ASMR_SDI_2021$location)

setdiff(data2.1$location,data2.1_ASMR_SDI_2021$location)

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\1992")

write.csv(data2.1_ASMR_SDI_2021,"204个国家肺癌二手烟dalys标准化率+SDI(1992女).csv",row.names = F)


#作图

b <- cor.test(as.numeric(data2.1_ASMR_SDI_2021$ASDR),as.numeric(data2.1_ASMR_SDI_2021$SDI),method = "pearson")


fig2 <- ggplot(data2.1_ASMR_SDI_2021, aes(SDI,ASDR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  labs(title = NULL,x=NULL,y=NULL)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 1,size = 9,color = "black"),
        axis.text.y = element_text(vjust = 1,size = 9,color = "black"),
        title = element_text(size = 12,hjust = 0.5))+theme(legend.position="none")+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASDR), #定义添加的文本和其位置
           label=paste("R=",round ( b[["estimate"]] ,3),sep = ""),
           size=4,color="black" )+
  annotate("text",x=0.10,y=max(data2.1_ASMR_SDI_2021$ASDR)*0.95,
           label=paste("p<0.01"),
           size=4,color="black")+
  xlab("SDI")+
  ylab("ASDR")

fig2
ggsave("204个国家肺癌二手烟dalys标准化率女SDI相关性分析1992.JPEG",width = 8,height=10,dpi = 300) #使用ggsave()函数保存，图片质量好













# ASMR/ASDR总组合 ------------------------------------------------------------------


library(ggplot2)  
library(gridExtra)  
library(grid)  
library(cowplot)  
library(magick)  
library(magick)  



library(magick)  
library(ggplot2)  
library(patchwork)  

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\SDI图片")
# 读取图片  
p1 <- image_read("204个国家肺癌二手烟死亡标准化率SDI相关性分析2021.JPEG") %>%   
  image_ggplot() +   
  theme(  
    plot.margin = margin(0, 0, 0, 0),  
    aspect.ratio = 0.6
  )  
# 设置高度为宽度的0.4，使图片更扁平  
  


p2 <- image_read("204个国家肺癌二手烟dalys标准化率SDI相关性分析2021.JPEG") %>%   
  image_ggplot() +   
  theme(  
    plot.margin = margin(0, 0, 0, 0),  
    aspect.ratio = 0.6
  )  

# 添加标签  
p1_labeled <- p1 + annotate("text", x = -Inf, y = Inf, label = "A",   
                            hjust = -0.5, vjust = 1.5,   
                            size = 5, fontface = "bold")  
p2_labeled <- p2 + annotate("text", x = -Inf, y = Inf, label = "B",   
                            hjust = -0.5, vjust = 1.5,   
                            size = 5, fontface = "bold")  

# 组合图片  
combined_plot <- p1_labeled / p2_labeled +  
  plot_layout(  
    heights = c(1, 1)  # 确保两张图片高度相等  
  ) &   
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))  



# 保存图片  
ggsave("ASMR、ASDR总（A为ASMR、B为ASDR）.JPEG",   
       combined_plot,  
       width = 10,     # 设置适当的宽度  
       height = 12,     # 设置适当的高度  
       dpi = 1000)








# ASMR男女组合 ----------------------------------------------------------------
setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\SDI图片")
# 读取图片  
p3 <- image_read("204个国家肺癌二手烟死亡标准化率男SDI相关性分析2021.JPEG") %>%   
  image_ggplot() +   
  theme(  
    plot.margin = margin(0, 0, 0, 0),  
    aspect.ratio = 0.6
  )  
# 设置高度为宽度的0.4，使图片更扁平  



p4 <- image_read("204个国家肺癌二手烟死亡标准化率女SDI相关性分析2021.JPEG") %>%   
  image_ggplot() +   
  theme(  
    plot.margin = margin(0, 0, 0, 0),  
    aspect.ratio = 0.6
  )  

# 添加标签  
p3_labeled <- p3 + annotate("text", x = -Inf, y = Inf, label = "A",   
                            hjust = -0.5, vjust = 1.5,   
                            size = 5, fontface = "bold")  
p4_labeled <- p4 + annotate("text", x = -Inf, y = Inf, label = "B",   
                            hjust = -0.5, vjust = 1.5,   
                            size = 5, fontface = "bold")  

# 组合图片  
combined_plot <- p3_labeled / p4_labeled +  
  plot_layout(  
    heights = c(1, 1)  # 确保两张图片高度相等  
  ) &   
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))  


# 保存图片  
ggsave("ASMR男女（A为男、B为女）.JPEG",   
       combined_plot,  
       width = 10,     # 设置适当的宽度  
       height = 12,     # 设置适当的高度  
       dpi = 1000)






# ASDR男女组合 ----------------------------------------------------------------

setwd("C:\\Users\\dell\\Desktop\\GBD数据\\SDI相关性\\二手烟\\SDI图片")
# 读取图片  
p5 <- image_read("204个国家肺癌二手烟dalys标准化率男SDI相关性分析2021.JPEG") %>%   
  image_ggplot() +   
  theme(  
    plot.margin = margin(0, 0, 0, 0),  
    aspect.ratio = 0.6
  )  
# 设置高度为宽度的0.4，使图片更扁平  



p6 <- image_read("204个国家肺癌二手烟dalys标准化率女SDI相关性分析2021.JPEG") %>%   
  image_ggplot() +   
  theme(  
    plot.margin = margin(0, 0, 0, 0),  
    aspect.ratio = 0.6
  )  

# 添加标签  
p5_labeled <- p5 + annotate("text", x = -Inf, y = Inf, label = "A",   
                            hjust = -0.5, vjust = 1.5,   
                            size = 5, fontface = "bold")  
p6_labeled <- p6 + annotate("text", x = -Inf, y = Inf, label = "B",   
                            hjust = -0.5, vjust = 1.5,   
                            size = 5, fontface = "bold")  

# 组合图片  
combined_plot <- p5_labeled / p6_labeled +  
  plot_layout(  
    heights = c(1, 1)  # 确保两张图片高度相等  
  ) &   
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))  



# 保存图片  
ggsave("ASDR男女（A为男、B为女）.JPEG",   
       combined_plot,  
       width = 10,     # 设置适当的宽度  
       height = 12,     # 设置适当的高度  
       dpi = 1000)





