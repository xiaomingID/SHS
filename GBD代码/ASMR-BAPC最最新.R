setwd("C:\\Users\\dell\\Desktop\\GBD数据\\BAPC最新")

library(tidyverse)
library(BAPC)
library(INLA)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(epitools)
library(reshape2)


# 提取both疾病数据 --------------------------------------------------------------------

data <- read.csv("global+5个sdi地区的肺癌肺癌二手烟死亡人数.csv")

colnames(data)

unique(data$age_name)


unique(data$year)
year1 <- c("1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001",
           "2002", "2003", "2004", "2005", "2006", "2007",
           "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021") 

# 发病数据需要的年龄分层
age1 <- c(
  "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
  "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
  "75-79 years","80-84 years","85-89 years","90-94 years","95+ years")   ###15个年龄组

#### 调取标准人口百分比用
ages_2 <- c("<1 year","1 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
            "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
            "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
            "90 to 94", "95 plus")
####  预测的年龄结构
ages_3 <- c("0 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
            "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
            "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
            "90 to 94", "95 plus")

# 提取标准年龄结构数据age_stand
age_stand <- read.csv("标准人口比例.csv") #表示的是各年龄结构所占的百分比
sum(age_stand$std_population)
colnames(age_stand)



str(age_stand)


#把提取的标准年龄结构数据中的年龄结构转化成与ages_3一样
wstand <- c(age_stand$std_population[1:6] %>% as.numeric() %>% sum(),
            age_stand$std_population[7:25] %>% as.numeric())/sum(age_stand$std_population[1:25])


wstand
sum(wstand)

#提取global的发病人口数据
unique(data$age_name)

unique(data$location_name)


unique(data$sex_name)


#提取both的发病人数

data_in_both<- subset(data,
                      (data$age_name %in% age1 ) &
                        (data$year%in% year1)&
                        data$sex_name=="Both"&
                        data$location_name=='Global'&
                        data$metric_name== 'Number' &
                        data$measure_name=='Deaths')
unique(data_in_both$age_name)


unique(data_in_both$year)

#通过 gsub() 去除了 age_name 列中的 " years" 字符串，使得年龄组更简洁
data_in_both$age_name<-gsub(" years","",data_in_both$age_name)

unique(data_in_both$age_name)
#将 age_name 列因子化，并按指定的年龄段顺序排列，确保在后续分析或绘图中，年龄组按正确的逻辑顺序（从小到大）展示。
data_in_both$age_name <- factor(data_in_both$age_name, levels = c( "25-29" ,"30-34" ,"35-39" ,"40-44", "45-49" ,
                                                                   "50-54" ,"55-59" ,"60-64" ,"65-69" ,"70-74" ,
                                                                   "75-79" ,"80-84","85-89","90-94","95+"))



# 提取数据Measure_name,age_name,year,val
data_in_both <- data_in_both[,c("measure_name", "age_name","year","val")]

#长转宽，具体作用是将 year作为行标签，将 age_name 作为列标签，并且提取 val 列的数值作为表格中的值
data_in_both_n <- reshape2::dcast(data=data_in_both, year ~ age_name, value.var="val")
rownames(data_in_both_n) <- data_in_both_n$year
data_in_both_n <- data_in_both_n[,-1]

data_in_both_n[,'0 to 4'] <- 0
data_in_both_n[,'5 to 9'] <- 0
data_in_both_n[,'10 to 14'] <- 0
data_in_both_n[,'15 to 19'] <- 0

data_in_both_n[,'20 to 24'] <- 0

data_in_both_n <-data_in_both_n[,c(16:20,1:15)]
#对数据进行取整（BAPC要求整数数据）
data_in_both_n <- apply(data_in_both_n,c(1,2),round) %>% as.data.frame #c(1, 2)：指定了操作的维度：1 代表按行操作，2 代表按列操作


# 提取both人口数据 ------------------------------------------------------------------




age2 <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
          "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
          "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
          "75-79 years","80-84 years","85-89 years","90-94 years","95+ years") 


dirname <- dir("GBD_Population") #读取文件名

file <- paste0(getwd(),"/GBD_Population/",dirname) #添加路径名，方便读取文件
var_name <- c("location_name","sex_name","year","age_name","val")  #提取需要的变量

GBD_population  <-  as.data.frame(matrix(nrow=0,ncol=length(var_name))) #创建空的数据集

names(GBD_population)=var_name #根据var_name对列名进行命名


library(data.table)
#通过循环语句将 file 中的每个元素（假设 file 是文件路径的集合），从中读取数据文件，转换为数据框，并对数据进行筛选和处理
for (a in file) {
  mydata <- fread(a) %>% as.data.frame() %>% select(all_of(var_name)) %>%
    filter(age_name %in% age2 & location_name %in% 'Global') 
  GBD_population <- rbind(GBD_population,mydata)}

colnames(GBD_population)

GBD_population<- GBD_population %>% filter(GBD_population$year%in%year1)

unique(GBD_population$year)
unique(GBD_population$age_name)

GBD_population$age_name<-gsub(" years","",GBD_population$age_name)

unique(GBD_population$sex_name)
#提取1992~2021年真实的both人口数据

GBD_Both_population<- subset(GBD_population,GBD_population$sex_name =="Both")

unique(GBD_Both_population$age_name)



# 提取2022-2046年both预测人口学数据 -------------------------------------------------
prediction_var_name <- c("location_name", "sex", "year_id", "age_group_name", "val")


GBD_population_prediction_both <- fread("2022-2100全球预测人口学数据总.csv") %>%
  dplyr::select(prediction_var_name) %>%
  dplyr::filter(location_name %in% 'Global' & year_id %in% 2022:2046
                & sex %in% "Both"  )

unique(GBD_population_prediction_both$age_group_name)


write.csv(GBD_population_prediction_both,"2022-2046全球预测人口学数据总.csv",row.names = F)

#names(GBD_population_prediction) <- var_name #把GBD_population_prediction中的变量名设置成和GBD_population一样

unique(GBD_population_prediction_both$age_group_name)


## 由于预测人口数据没有<1的年龄数据，因此我们需要进行转换，将Early Neonatal,Late Neonatal, Post Neonatal,合并成<1的年龄数据
## 筛选`Early Neonatal`,`Late Neonatal`, `Post Neonatal`数据并根据location_name,sex,year_id 进行分组后对3个年龄的val值相加得到<1 year的人口学数据
## 同时加上1-4岁的数据，组成<5岁的人口数据
GBD_5year <- GBD_population_prediction_both %>% 
  filter(age_group_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal","1 to 4")) %>%
  group_by(location_name,sex,year_id) %>% 
  summarise(val=sum(val)) %>%
  mutate(age_group_name="<5")


## 再将 "Early Neonatal","Late Neonatal", "Post Neonatal","1 to 4"数据去除,加上<5 year的人口学数据
GBD_population_prediction_both <- GBD_population_prediction_both %>% filter(!(age_group_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal","All Ages","1 to 4"))) %>%
  rbind(GBD_5year) 

names(GBD_population_prediction_both)[names(GBD_population_prediction_both) == 'age_group_name'] <- 'age_name'

GBD_population_prediction_both$age_name<-gsub(" to ","-",GBD_population_prediction_both$age_name)
GBD_population_prediction_both$age_name<-gsub(" plus","+",GBD_population_prediction_both$age_name)



unique(GBD_population_prediction_both$age_name)

colnames(GBD_population_prediction_both)<-var_name

unique(data_in_both$age_name) #疾病数据

unique(GBD_Both_population$age_name) #人口学数据
unique(GBD_population_prediction_both$age_name) #预测人口




##合并both人口学数据1992-2046
GBD_both <- rbind(GBD_Both_population, GBD_population_prediction_both)
GBD_both$age_name<-factor(GBD_both$age_name, levels = c("<5","5-9","10-14" ,"15-19" ,"20-24",
                                              "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                              "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                              "90-94", "95+"))
unique(GBD_both$age_name)



# 整理人口学数据变成BAPC能够识别的数据形式

GBD_Global_Both <- subset(GBD_both,location_name=="Global" & sex_name=="Both")

GBD_Global_Both$age_name<-factor(GBD_Global_Both$age_name, levels =c("<5","5-9","10-14" ,"15-19" ,"20-24",
                                                                     "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                                                     "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                                                     "90-94", "95+"))
GBD_Global_Both_n <- reshape2::dcast(data=GBD_Global_Both, year~age_name, value.var=c("val")) %>% as.data.frame()



#改行名
rownames(GBD_Global_Both_n) <- GBD_Global_Both_n$year

GBD_Global_Both_n <- GBD_Global_Both_n[,-1]

GBD_Global_Both_n <- apply(GBD_Global_Both_n, c(1,2), as.numeric) %>% as.data.frame()
GBD_Global_Both_n <- apply(GBD_Global_Both_n, c(1,2), round) %>% as.data.frame()

# 补充没有发病人数数据的年份(2022~2046年25年)
data_both <- matrix(data=NA, nrow=2046-2021, ncol=ncol(GBD_Global_Both_n)) %>% as.data.frame()
rownames(data_both) <- seq(2022, 2046, 1)
colnames(data_both) <- names(data_in_both_n)

#合并1992~2046年的发病数据，其中1992~2021年为真实的发病人数，2022~2046年为预测的发病人数
data_both_n <- rbind(data_in_both_n, data_both)
data_both_n <- apply(data_both_n, c(1,2), as.numeric) %>% as.data.frame()
data_both_n <- apply(data_both_n, c(1,2), round) %>% as.data.frame()


















# 提取male疾病数据 --------------------------------------------------------------
unique(data$sex_name)
data_in_male<- subset(data,
                      (data$age_name %in% age1 ) &
                        (data$year%in% year1)&
                        data$sex_name=="Male"&
                        data$location_name=='Global'&
                        data$metric_name== 'Number' &
                        data$measure_name=='Deaths')
unique(data_in_male$age_name)


unique(data_in_male$year)

#通过 gsub() 去除了 age_name 列中的 " years" 字符串，使得年龄组更简洁
data_in_male$age_name<-gsub(" years","",data_in_male$age_name)

unique(data_in_male$age_name)
#将 age_name 列因子化，并按指定的年龄段顺序排列，确保在后续分析或绘图中，年龄组按正确的逻辑顺序（从小到大）展示。
data_in_male$age_name <- factor(data_in_male$age_name, levels = c( "25-29" ,"30-34" ,"35-39" ,"40-44", "45-49" ,
                                                                   "50-54" ,"55-59" ,"60-64" ,"65-69" ,"70-74" ,
                                                                   "75-79" ,"80-84","85-89","90-94","95+"))



# 提取数据Measure_name,age_name,year,val
data_in_male <- data_in_male[,c("measure_name", "age_name","year","val")]

#长转宽，具体作用是将 year作为行标签，将 age_name 作为列标签，并且提取 val 列的数值作为表格中的值
data_in_male_n <- reshape2::dcast(data=data_in_male, year ~ age_name, value.var="val")
rownames(data_in_male_n) <- data_in_male_n$year
data_in_male_n <- data_in_male_n[,-1]

data_in_male_n[,'0 to 4'] <- 0
data_in_male_n[,'5 to 9'] <- 0
data_in_male_n[,'10 to 14'] <- 0
data_in_male_n[,'15 to 19'] <- 0

data_in_male_n[,'20 to 24'] <- 0

data_in_male_n <-data_in_male_n[,c(16:20,1:15)]
#对数据进行取整（BAPC要求整数数据）
data_in_male_n <- apply(data_in_male_n,c(1,2),round) %>% as.data.frame #c(1, 2)：指定了操作的维度：1 代表按行操作，2 代表按列操作


# 提取male人口数据 --------------------------------------------------------------


#提取1992~2021年真实的Male人口数据

GBD_male_population<- subset(GBD_population,GBD_population$sex_name =="Male")

unique(GBD_male_population$age_name)

# 提取2022-2046年male预测人口数据 --------------------------------------------------


prediction_var_name <- c("location_name", "sex", "year_id", "age_group_name", "val")


GBD_population_prediction_male <- fread("IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.csv") %>%
  dplyr::select(prediction_var_name) %>%
  dplyr::filter(location_name %in% 'Global' & year_id %in% 2022:2046
                & sex %in% "Male" )

unique(GBD_population_prediction_male$age_group_name)


#write.csv(GBD_population_prediction_male,"2022-2046全球预测人口学数据男.csv",row.names = F)

#names(GBD_population_prediction) <- var_name #把GBD_population_prediction中的变量名设置成和GBD_population一样

unique(GBD_population_prediction_male$age_group_name)


## 由于预测人口数据没有<1的年龄数据，因此我们需要进行转换，将Early Neonatal,Late Neonatal, Post Neonatal,合并成<1的年龄数据
## 筛选`Early Neonatal`,`Late Neonatal`, `Post Neonatal`数据并根据location_name,sex,year_id 进行分组后对3个年龄的val值相加得到<1 year的人口学数据
## 同时加上1-4岁的数据，组成<5岁的人口数据
GBD_5year_male <- GBD_population_prediction_male %>% 
  filter(age_group_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal","1 to 4")) %>%
  group_by(location_name,sex,year_id) %>% 
  summarise(val=sum(val)) %>%
  mutate(age_group_name="<5")


## 再将 "Early Neonatal","Late Neonatal", "Post Neonatal","1 to 4"数据去除,加上<5 year的人口学数据
GBD_population_prediction_male <- GBD_population_prediction_male %>% filter(!(age_group_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal","All Ages","1 to 4"))) %>%
  rbind(GBD_5year_male) 

names(GBD_population_prediction_male)[names(GBD_population_prediction_male) == 'age_group_name'] <- 'age_name'

GBD_population_prediction_male$age_name<-gsub(" to ","-",GBD_population_prediction_male$age_name)
GBD_population_prediction_male$age_name<-gsub(" plus","+",GBD_population_prediction_male$age_name)



unique(GBD_population_prediction_male$age_name)

colnames(GBD_population_prediction_male)<-var_name

unique(data_in_male$age_name) #疾病数据

unique(GBD_male_population$age_name) #人口学数据
unique(GBD_population_prediction_male$age_name) #预测人口




##合并both人口学数据1992-2046
GBD_male <- rbind(GBD_male_population, GBD_population_prediction_male)
GBD_male$age_name<-factor(GBD_male$age_name, levels = c("<5","5-9","10-14" ,"15-19" ,"20-24",
                                                        "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                                        "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                                        "90-94", "95+"))
unique(GBD_male$age_name)



# 整理人口学数据变成BAPC能够识别的数据形式

GBD_Global_male <- subset(GBD_male,location_name=="Global" & sex_name=="Male")

GBD_Global_male$age_name<-factor(GBD_Global_male$age_name, levels =c("<5","5-9","10-14" ,"15-19" ,"20-24",
                                                                     "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                                                     "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                                                     "90-94", "95+"))
GBD_Global_male_n <- reshape2::dcast(data=GBD_Global_male, year~age_name, value.var=c("val")) %>% as.data.frame()



#改行名
rownames(GBD_Global_male_n) <- GBD_Global_male_n$year

GBD_Global_male_n <- GBD_Global_male_n[,-1]

GBD_Global_male_n <- apply(GBD_Global_male_n, c(1,2), as.numeric) %>% as.data.frame()
GBD_Global_male_n <- apply(GBD_Global_male_n, c(1,2), round) %>% as.data.frame()

# 补充没有发病人数数据的年份(2022~2046年25年)
data_male <- matrix(data=NA, nrow=2046-2021, ncol=ncol(GBD_Global_male_n)) %>% as.data.frame()
rownames(data_male) <- seq(2022, 2046, 1)
colnames(data_male) <- names(data_in_male_n)

#合并1992~2046年的发病数据，其中1992~2021年为真实的发病人数，2022~2046年为预测的发病人数
data_male_n <- rbind(data_in_male_n, data_male)
data_male_n <- apply(data_male_n, c(1,2), as.numeric) %>% as.data.frame()
data_male_n <- apply(data_male_n, c(1,2), round) %>% as.data.frame()



























# 提取female疾病数据 ------------------------------------------------------------

unique(data$sex_name)
data_in_female<- subset(data,
                      (data$age_name %in% age1 ) &
                        (data$year%in% year1)&
                        data$sex_name=="Female"&
                        data$location_name=='Global'&
                        data$metric_name== 'Number' &
                        data$measure_name=='Deaths')
unique(data_in_female$age_name)


unique(data_in_female$year)

#通过 gsub() 去除了 age_name 列中的 " years" 字符串，使得年龄组更简洁
data_in_female$age_name<-gsub(" years","",data_in_female$age_name)

unique(data_in_female$age_name)
#将 age_name 列因子化，并按指定的年龄段顺序排列，确保在后续分析或绘图中，年龄组按正确的逻辑顺序（从小到大）展示。
data_in_female$age_name <- factor(data_in_female$age_name, levels = c( "25-29" ,"30-34" ,"35-39" ,"40-44", "45-49" ,
                                                                   "50-54" ,"55-59" ,"60-64" ,"65-69" ,"70-74" ,
                                                                   "75-79" ,"80-84","85-89","90-94","95+"))



# 提取数据Measure_name,age_name,year,val
data_in_female <- data_in_female[,c("measure_name", "age_name","year","val")]

#长转宽，具体作用是将 year作为行标签，将 age_name 作为列标签，并且提取 val 列的数值作为表格中的值
data_in_female_n <- reshape2::dcast(data=data_in_female, year ~ age_name, value.var="val")
rownames(data_in_female_n) <- data_in_female_n$year
data_in_female_n <- data_in_female_n[,-1]

data_in_female_n[,'0 to 4'] <- 0
data_in_female_n[,'5 to 9'] <- 0
data_in_female_n[,'10 to 14'] <- 0
data_in_female_n[,'15 to 19'] <- 0

data_in_female_n[,'20 to 24'] <- 0

data_in_female_n <-data_in_female_n[,c(16:20,1:15)]
#对数据进行取整（BAPC要求整数数据）
data_in_female_n <- apply(data_in_female_n,c(1,2),round) %>% as.data.frame #c(1, 2)：指定了操作的维度：1 代表按行操作，2 代表按列操作


# 提取female人口数据 ------------------------------------------------------------

#提取1992~2021年真实的female人口数据

GBD_female_population<- subset(GBD_population,GBD_population$sex_name =="Female")

unique(GBD_female_population$age_name)

# 提取2022-2046年female预测人口数据 ------------------------------------------------


age2 <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
          "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
          "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
          "75-79 years","80-84 years","85-89 years","90-94 years","95+ years") 


dirname <- dir("GBD_Population") #读取文件名

file <- paste0(getwd(),"/GBD_Population/",dirname) #添加路径名，方便读取文件
var_name <- c("location_name","sex_name","year","age_name","val")  #提取需要的变量

GBD_population  <-  as.data.frame(matrix(nrow=0,ncol=length(var_name))) #创建空的数据集

names(GBD_population)=var_name #根据var_name对列名进行命名


library(data.table)
#通过循环语句将 file 中的每个元素（假设 file 是文件路径的集合），从中读取数据文件，转换为数据框，并对数据进行筛选和处理
for (a in file) {
  mydata <- fread(a) %>% as.data.frame() %>% select(all_of(var_name)) %>%
    filter(age_name %in% age2 & location_name %in% 'Global') 
  GBD_population <- rbind(GBD_population,mydata)}

colnames(GBD_population)

GBD_population<- GBD_population %>% filter(GBD_population$year%in%year1)

unique(GBD_population$year)
unique(GBD_population$age_name)

GBD_population$age_name<-gsub(" years","",GBD_population$age_name)

unique(GBD_population$sex_name)
#提取1992~2021年真实的female人口数据

prediction_var_name <- c("location_name", "sex", "year_id", "age_group_name", "val")



GBD_population_prediction_female <- fread("IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.csv") %>%
  dplyr::select(prediction_var_name) %>%
  dplyr::filter(location_name %in% 'Global' & year_id %in% 2022:2046
                & sex %in% "Female"  )

#write.csv(GBD_population_prediction_female,"2022-2046全球预测人口学数据女.csv",row.names = F)



#names(GBD_population_prediction) <- var_name #把GBD_population_prediction中的变量名设置成和GBD_population一样

unique(GBD_population_prediction_female$age_group_name)


## 由于预测人口数据没有<1的年龄数据，因此我们需要进行转换，将Early Neonatal,Late Neonatal, Post Neonatal,合并成<1的年龄数据
## 筛选`Early Neonatal`,`Late Neonatal`, `Post Neonatal`数据并根据location_name,sex,year_id 进行分组后对3个年龄的val值相加得到<1 year的人口学数据
## 同时加上1-4岁的数据，组成<5岁的人口数据
GBD_5year_female <- GBD_population_prediction_female %>% 
  filter(age_group_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal","1 to 4")) %>%
  group_by(location_name,sex,year_id) %>% 
  summarise(val=sum(val)) %>%
  mutate(age_group_name="<5")


## 再将 "Early Neonatal","Late Neonatal", "Post Neonatal","1 to 4"数据去除,加上<5 year的人口学数据
GBD_population_prediction_female <- GBD_population_prediction_female %>% filter(!(age_group_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal","All Ages","1 to 4"))) %>%
  rbind(GBD_5year_female) 

names(GBD_population_prediction_female)[names(GBD_population_prediction_female) == 'age_group_name'] <- 'age_name'

GBD_population_prediction_female$age_name<-gsub(" to ","-",GBD_population_prediction_female$age_name)
GBD_population_prediction_female$age_name<-gsub(" plus","+",GBD_population_prediction_female$age_name)



unique(GBD_population_prediction_female$age_name)

colnames(GBD_population_prediction_female)<-var_name

unique(data_in_female$age_name) #疾病数据

unique(GBD_female_population$age_name) #人口学数据
unique(GBD_population_prediction_female$age_name) #预测人口




##合并female人口学数据1992-2046
GBD_female <- rbind(GBD_female_population, GBD_population_prediction_female)
GBD_female$age_name<-factor(GBD_female$age_name, levels = c("<5","5-9","10-14" ,"15-19" ,"20-24",
                                                        "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                                        "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                                        "90-94", "95+"))
unique(GBD_female$age_name)



# 整理人口学数据变成BAPC能够识别的数据形式

GBD_Global_female <- subset(GBD_female,location_name=="Global" & sex_name=="Female")

GBD_Global_female$age_name<-factor(GBD_Global_female$age_name, levels =c("<5","5-9","10-14" ,"15-19" ,"20-24",
                                                                     "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                                                     "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                                                     "90-94", "95+"))
GBD_Global_female_n <- reshape2::dcast(data=GBD_Global_female, year~age_name, value.var=c("val")) %>% as.data.frame()



#改行名
rownames(GBD_Global_female_n) <- GBD_Global_female_n$year

GBD_Global_female_n <- GBD_Global_female_n[,-1]

GBD_Global_female_n <- apply(GBD_Global_female_n, c(1,2), as.numeric) %>% as.data.frame()
GBD_Global_female_n <- apply(GBD_Global_female_n, c(1,2), round) %>% as.data.frame()

# 补充没有发病人数数据的年份(2022~2046年25年)
data_female <- matrix(data=NA, nrow=2046-2021, ncol=ncol(GBD_Global_female_n)) %>% as.data.frame()
rownames(data_female) <- seq(2022, 2046, 1)
colnames(data_female) <- names(data_in_female_n)

#合并1992~2046年的发病数据，其中1992~2021年为真实的发病人数，2022~2046年为预测的发病人数
data_female_n <- rbind(data_in_female_n, data_female)
data_female_n <- apply(data_female_n, c(1,2), as.numeric) %>% as.data.frame()
data_female_n <- apply(data_female_n, c(1,2), round) %>% as.data.frame()





























require(INLA)
library(INLA)
library(devtools)
library(inlabru)
library(BAPC)
library(INLA)
library(nordpred)
library(reshape)
library(data.table)
library(tidyr)
library(tidyverse)
library(epitools)
library(ggplot2)






# ###BAPC模型拟合 -------------------------------------------------------------


###BAPC模型拟合
Male_death <- APCList(data_male_n, GBD_Global_male_n, gf = 5)
Male_bapc_result <- BAPC(Male_death, predict = list(npredict = 25, retro = T),
                         secondDiff = FALSE, stdweight = wstand ,verbose = F)

Female_death <- APCList(data_female_n, GBD_Global_female_n, gf = 5)
Female_bapc_result <- BAPC(Female_death, predict = list(npredict = 25, retro = T),
                           secondDiff = FALSE, stdweight = wstand, verbose = F)

Both_death <- APCList(data_both_n, GBD_Global_Both_n, gf = 5)
Both_bapc_result <- BAPC(Both_death, predict = list(npredict = 25, retro = T),
                         secondDiff = FALSE, stdweight = wstand,, verbose = F)




# ### 标准患病率——agestd.rate函数 ------------------------------------------------


### 标准死亡率——agestd.rate函数
Male_ASMR <- agestd.rate(x = Male_bapc_result) %>% as.data.frame()
Male_ASMR$mean <- Male_ASMR$mean*100000
Male_ASMR$year <- rownames(Male_ASMR)

Female_ASMR <- agestd.rate(x =Female_bapc_result) %>% as.data.frame()
Female_ASMR$mean <- Female_ASMR$mean*100000
Female_ASMR$year <- rownames(Female_ASMR)

Both_ASMR <- agestd.rate(x =Both_bapc_result) %>% as.data.frame()
Both_ASMR$mean <- Both_ASMR$mean*100000
Both_ASMR$year <- rownames(Both_ASMR)


# ### 用到qapc函数将0.025以及0.975写入到BAPC结果中 -------------------------------------


### 用到qapc函数将0.025以及0.975写入到BAPC结果中
Male_bapc_result <- qapc(Male_bapc_result,percentiles=c(0.025,0.975))
Female_bapc_result <- qapc(Female_bapc_result,percentiles=c(0.025,0.975))
Both_bapc_result <- qapc(Both_bapc_result,percentiles=c(0.025,0.975))



# 整理 ----------------------------------------------------------------------



### 获取ASR以及95%CrI
Male_ASMR <- agestd.rate(x = Male_bapc_result) %>% as.data.frame()*10^5
Female_ASMR <- agestd.rate(x = Female_bapc_result) %>% as.data.frame()*10^5
Both_ASMR <- agestd.rate(x = Both_bapc_result) %>% as.data.frame()*10^5
###创建时间列
Male_ASMR$year <- 1992:2046
Female_ASMR$year <- 1992:2046
Both_ASMR$year <- 1992:2046
###创建性别列
Male_ASMR$sex <- 'Male'
Female_ASMR$sex <- 'Female'
Both_ASMR$sex <- 'Both'



# 画图 ----------------------------------------------------------------------

###合并成一个数据集
ASMR <- Male_ASMR %>% 
  rbind(Female_ASMR) %>% 
  rbind(Both_ASMR) 



names(ASMR)[names(ASMR) == "mean"] <- "ASMR"  
names(ASMR)[names(ASMR) == "0.025Q"] <- "lower"  
names(ASMR)[names(ASMR) == "0.975Q"] <- "upper"

###绘图
p <- ggplot() +
  geom_line(data=subset(ASMR,year %in% 1992:2021),aes(year,ASMR,color='Global',linetype='Observed')) + #绘制1990-2021年直线
  geom_line(data=subset(ASMR,year %in% 2021:2046),aes(year,ASMR,linetype="Predicted"),color="#565bab") + #绘制2021-2040年直线
  geom_ribbon(data=subset(ASMR,year %in% 2021:2046),aes(x=year,ymin=lower,ymax=upper),fill='#565bab',alpha=.5) + #绘制2021-2040年区间条带
  geom_vline(xintercept = 2021,linetype=2)+ #绘制2021竖虚线
  scale_y_continuous(breaks=seq(0,2.5,0.2))+ #调整y轴值标签
  scale_linetype_manual(name="line",
                        values=c('Observed'=1,
                                 "Predicted"=2 #手动更改线形状，并更改命名
                        )) +
  scale_color_manual(name="group",
                     values=c('Global'='#565bab'
                     )) + #手动改更改线条颜色，并更改命名
  facet_wrap(~sex) + #分组绘图，以性别为依据
  labs(
       y = 'ASMR (per 100,000 )') #增加图标题和更改y轴名称


p







p <- ggplot() +  
  geom_line(data=subset(ASMR,year %in% 1992:2021),aes(year,ASMR,color='Global',linetype='Observed')) +  
  geom_line(data=subset(ASMR,year %in% 2021:2046),aes(year,ASMR,linetype="Predicted"),color="#565bab") +  
  geom_ribbon(data=subset(ASMR,year %in% 2021:2046),aes(x=year,ymin=lower,ymax=upper),fill='#565bab',alpha=.5) +  
  geom_vline(xintercept = 2021,linetype=2)+  
  scale_y_continuous(breaks=seq(0,2.5,0.2))+  
  scale_linetype_manual(name="line",  
                        values=c('Observed'=1,  
                                 "Predicted"=2  
                        )) +  
  scale_color_manual(name="group",  
                     values=c('Global'='#565bab'  
                     )) +  
  facet_wrap(~sex) +   
  labs(y = 'ASMR (per 100,000 )') +  
  # 添加以下代码来设置标签  
  theme(strip.text = element_text(size = 12)) +  # 调整分面标题文字大小  
  facet_wrap(~sex, labeller = as_labeller(c(  
    'Both' = 'A. Both',  
    'Female' = 'B. Female',  
    'Male' = 'C. Male'  
  )))













p <- ggplot() +  
  geom_line(data=subset(ASMR,year %in% 1992:2021),aes(year,ASMR,color='Global',linetype='Observed')) +  
  geom_line(data=subset(ASMR,year %in% 2021:2046),aes(year,ASMR,linetype="Predicted"),color="#565bab") +  
  geom_ribbon(data=subset(ASMR,year %in% 2021:2046),aes(x=year,ymin=lower,ymax=upper),fill='#565bab',alpha=.5) +  
  geom_vline(xintercept = 2021,linetype=2)+  
  scale_y_continuous(breaks=seq(0,2.5,0.2))+  
  scale_linetype_manual(name="line",  
                        values=c('Observed'=1,  
                                 "Predicted"=2  
                        )) +  
  scale_color_manual(name="group",  
                     values=c('Global'='#565bab'  
                     )) +  
  facet_wrap(~sex) +   
  labs(x = 'Year',  # 添加x轴标签  
       y = 'ASMR (per 100,000)') +  
  theme(  
    # 分面标题设置  
    strip.text = element_text(size = 12),  # 分面标题大小和粗细  
    
    # 坐标轴标题设置  
    axis.title.x = element_text(size = 15),  # X轴标题大小  
    axis.title.y = element_text(size = 15),  # Y轴标题大小  
    
    # 坐标轴刻度标签设置  
    axis.text.x = element_text(size = 14),  # X轴刻度标签大小  
    axis.text.y = element_text(size = 14),  # Y轴刻度标签大小  
    
    # 图例设置  
    legend.title = element_text(size = 14),  # 图例标题大小  
    legend.text = element_text(size = 12),    # 图例文本大小  
    legend.position = "right",               # 图例位置  
    legend.box = "vertical"                  # 图例排列方式  
  ) +  
  facet_wrap(~sex, labeller = as_labeller(c(  
    'Both' = 'A. Both',  
    'Female' = 'B. Female',  
    'Male' = 'C. Male'  
  )))












ggsave("ASMR1.JPEG",width = 12,height=8,dpi = 1000) #使用ggsave()函数保存，图片质量好

getwd()


write.csv(Both_ASMR,"global肺癌二手烟bothASMR预测数据.csv",row.names = F)

write.csv(Male_ASMR,"global肺癌二手烟maleASMR预测数据.csv",row.names = F)

write.csv(Female_ASMR,"global肺癌二手烟femaleASMR预测数据.csv",row.names = F)
write.csv(ASMR,"global肺癌二手烟ASMR预测数据总.csv",row.names = F)

