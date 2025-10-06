library(sf)
library(maps)
library(patchwork)
library(dplyr)
library(ggplot2)
library(reshape)
library(ggplot2)
library(ggrepel)
library(data.table)
library(tidyverse)
###读取绘制世界地图的经纬度等数据
world <- map_data('world')

# 读取男ASMR数据 ---------------------------------------------------------------

world <- map_data('world')
setwd("C:\\Users\\dell\\Desktop\\GBD数据\\地图\\2")

data1<- fread("筛选出204个国家2021年ASMR(男).csv",header = T)

colnames(data1)
unique(data1$location_name)


map1 <- data1 %>%
  mutate(val2 = cut(val, breaks = c(0,0.26,0.75,1.40,2.25,3.00,100000000),
                    labels = c("<0.26","0.26~0.75","0.75~1.40","1.40~2.25","2.25~3.00",
                               ">3.00"),  
                    include.lowest = T,right = T)) #建立新列，连续变量转换成分类变量，包含最小值所在区间且包括左端点而不包括右端点

names(map1)[names(map1) == "location_name"] <- "location"  

###地区名称转换，方便之后数据集合并
map1$location[map1$location == 'United States of America'] = 'USA'
map1$location[map1$location == 'Russian Federation'] = 'Russia'
map1$location[map1$location == 'United Kingdom'] = 'UK'
map1$location[map1$location == 'Congo'] = 'Republic of Congo'
map1$location[map1$location == "Iran (Islamic Republic of)"] = 'Iran'
map1$location[map1$location == "Democratic People's Republic of Korea"] = 'North Korea'
map1$location[map1$location == "Taiwan (Province of China)"] = 'Taiwan'
map1$location[map1$location == "Republic of Korea"] = 'South Korea'
map1$location[map1$location == "United Republic of Tanzania"] = 'Tanzania'
map1$location[map1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
map1$location[map1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
map1$location[map1$location == "Czechia"] = 'Czech Republic'
map1$location[map1$location == "Republic of Moldova"] = 'Moldova'
map1$location[map1$location == "Viet Nam"] = 'Vietnam'
map1$location[map1$location == "Lao People's Democratic Republic"] = 'Laos'
map1$location[map1$location == "Syrian Arab Republic"] = 'Syria'
map1$location[map1$location == "North Macedonia"] = 'Macedonia'
map1$location[map1$location == "Micronesia (Federated States of)"] = 'Micronesia'
map1$location[map1$location == "Macedonia"] = 'North Macedonia'
map1$location[map1$location == "Trinidad and Tobago"] = 'Trinidad'


a <- map1[map1$location == "Trinidad",]

a$location <- 'Tobago'
map1 <- rbind(map1,a)
map1$location[map1$location == "Cabo Verde"] = 'Cape Verde'

map1$location[map1$location == "United States Virgin Islands"] = 'Virgin Islands'
map1$location[map1$location == "Antigua and Barbuda"] = 'Antigu'
a <- map1[map1$location == "Antigu",]
a$location <- 'Barbuda'
map1 <- rbind(map1,a)
map1$location[map1$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
a <- map1[map1$location == "Saint Kitts",]
a$location <- 'Nevis'
map1 <- rbind(map1,a)
map1$location[map1$location == "Côte d'Ivoire"] = 'Ivory Coast'
map1$location[map1$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
a <- map1[map1$location == "Saint Vincent",]
a$location <- 'Grenadines'
map1 <- rbind(map1,a)
map1$location[map1$location == "Eswatini"] = 'Swaziland'
map1$location[map1$location == "Brunei Darussalam"] = 'Brunei'

#合并数据集
map1 <- full_join(world,map1,by = c('region'='location')) %>%
  filter(val != "NA")


###绘图

fig1 <- map1 %>%  
  ggplot()+  
  geom_polygon(aes(x = long, y = lat, group = group, fill=val2),  
               colour="black", size=0.5) + #绘制地图  
  theme_void()+ #主题设置  
  scale_fill_manual(name = "ASMR(per 100,000)",  # 在这里添加图例标题  
                    values=c("#019e73","#56b4e8","#f0e53e",  
                             "#e79e01","#d65e00","#fe0000","#00468BFF")) + #手动添加颜色  
  theme(legend.position = c(0.15,0.23), #设置图例位置  
        legend.title = element_text(color="black", size=22), #设置图例标题样式  
        legend.text = element_text(color="black",  
                                   size = 22,  
        ), #设置图例文字  
        plot.title =  element_blank(), #取消图标题  
        panel.grid=element_blank(), #取消图网格线  
        axis.title.x = element_blank(), #取消x轴标题  
        axis.text.x = element_blank(), #取消x轴标签  
        axis.ticks.x = element_blank(), #取消x轴刻度标  
        axis.title.y = element_blank(), #取消y轴标题  
        axis.text.y = element_blank(), #取消y轴标签  
        axis.ticks.y = element_blank(), #取消y轴刻度标  
  )

fig1

ggsave("ASMR男.JPEG",width = 12, height = 7,dpi = 1000) #使用ggsave()函数保存，图片质量好










# 读取男ASDR数据 ---------------------------------------------------------------

world <- map_data('world')


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\地图\\2")

data1<- fread("筛选出204个国家2021年ASDR(男).csv",header = T)

colnames(data1)
unique(data1$location_name)


map1 <- data1 %>%
  mutate(val2 = cut(val, breaks = c(0,6.50,18.00,35.50,55.50,70.00,100000000),
                    labels = c("<6.50","6.50~18.00","18.00~35.50","35.50~55.50","55.50~70.00",
                               ">70.00"),  
                    include.lowest = T,right = T)) #建立新列，连续变量转换成分类变量，包含最小值所在区间且包括左端点而不包括右端点

names(map1)[names(map1) == "location_name"] <- "location"  

###地区名称转换，方便之后数据集合并
map1$location[map1$location == 'United States of America'] = 'USA'
map1$location[map1$location == 'Russian Federation'] = 'Russia'
map1$location[map1$location == 'United Kingdom'] = 'UK'
map1$location[map1$location == 'Congo'] = 'Republic of Congo'
map1$location[map1$location == "Iran (Islamic Republic of)"] = 'Iran'
map1$location[map1$location == "Democratic People's Republic of Korea"] = 'North Korea'
map1$location[map1$location == "Taiwan (Province of China)"] = 'Taiwan'
map1$location[map1$location == "Republic of Korea"] = 'South Korea'
map1$location[map1$location == "United Republic of Tanzania"] = 'Tanzania'
map1$location[map1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
map1$location[map1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
map1$location[map1$location == "Czechia"] = 'Czech Republic'
map1$location[map1$location == "Republic of Moldova"] = 'Moldova'
map1$location[map1$location == "Viet Nam"] = 'Vietnam'
map1$location[map1$location == "Lao People's Democratic Republic"] = 'Laos'
map1$location[map1$location == "Syrian Arab Republic"] = 'Syria'
map1$location[map1$location == "North Macedonia"] = 'Macedonia'
map1$location[map1$location == "Micronesia (Federated States of)"] = 'Micronesia'
map1$location[map1$location == "Macedonia"] = 'North Macedonia'
map1$location[map1$location == "Trinidad and Tobago"] = 'Trinidad'


a <- map1[map1$location == "Trinidad",]

a$location <- 'Tobago'
map1 <- rbind(map1,a)
map1$location[map1$location == "Cabo Verde"] = 'Cape Verde'

map1$location[map1$location == "United States Virgin Islands"] = 'Virgin Islands'
map1$location[map1$location == "Antigua and Barbuda"] = 'Antigu'
a <- map1[map1$location == "Antigu",]
a$location <- 'Barbuda'
map1 <- rbind(map1,a)
map1$location[map1$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
a <- map1[map1$location == "Saint Kitts",]
a$location <- 'Nevis'
map1 <- rbind(map1,a)
map1$location[map1$location == "Côte d'Ivoire"] = 'Ivory Coast'
map1$location[map1$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
a <- map1[map1$location == "Saint Vincent",]
a$location <- 'Grenadines'
map1 <- rbind(map1,a)
map1$location[map1$location == "Eswatini"] = 'Swaziland'
map1$location[map1$location == "Brunei Darussalam"] = 'Brunei'

#合并数据集
map1 <- full_join(world,map1,by = c('region'='location')) %>%
  filter(val != "NA")


###绘图

fig1 <- map1 %>%  
  ggplot()+  
  geom_polygon(aes(x = long, y = lat, group = group, fill=val2),  
               colour="black", size=0.5) + #绘制地图  
  theme_void()+ #主题设置  
  scale_fill_manual(name = "ASDR(per 100,000)",  # 在这里添加图例标题  
                    values=c("#019e73","#56b4e8","#f0e53e",  
                             "#e79e01","#d65e00","#fe0000","#00468BFF")) + #手动添加颜色  
  theme(legend.position = c(0.15,0.23), #设置图例位置  
        legend.title = element_text(color="black", size=22), #设置图例标题样式  
        legend.text = element_text(color="black",  
                                   size = 22,  
        ), #设置图例文字  
        plot.title =  element_blank(), #取消图标题  
        panel.grid=element_blank(), #取消图网格线  
        axis.title.x = element_blank(), #取消x轴标题  
        axis.text.x = element_blank(), #取消x轴标签  
        axis.ticks.x = element_blank(), #取消x轴刻度标  
        axis.title.y = element_blank(), #取消y轴标题  
        axis.text.y = element_blank(), #取消y轴标签  
        axis.ticks.y = element_blank(), #取消y轴刻度标  
  )

fig1

ggsave("ASDR男.JPEG",width = 12, height = 7,dpi = 1000) #使用ggsave()函数保存，图片质量好








 # 读取EAPC in ASMR ----------------------------------------------------------

world <- map_data('world')
setwd("C:\\Users\\dell\\Desktop\\GBD数据\\地图\\2")

data1<- fread("204个国家肺癌的二手烟死亡标准化率EAPC（男）.csv",header = T)

colnames(data1)
unique(data1$location_name)


map1 <- data1 %>%
  mutate(val2 = cut(EAPC, breaks = c(-6,-2.50,-1.20,0,0.40,1.00,100000000),
                    labels = c("<-2.50","-2.50~-1.20","-1.20~0.00","0.00~0.40","0.40~1.00",
                               ">1.00"),  
                    include.lowest = T,right = T)) #建立新列，连续变量转换成分类变量，包含最小值所在区间且包括左端点而不包括右端点

names(map1)[names(map1) == "location_name"] <- "location"  

###地区名称转换，方便之后数据集合并
map1$location[map1$location == 'United States of America'] = 'USA'
map1$location[map1$location == 'Russian Federation'] = 'Russia'
map1$location[map1$location == 'United Kingdom'] = 'UK'
map1$location[map1$location == 'Congo'] = 'Republic of Congo'
map1$location[map1$location == "Iran (Islamic Republic of)"] = 'Iran'
map1$location[map1$location == "Democratic People's Republic of Korea"] = 'North Korea'
map1$location[map1$location == "Taiwan (Province of China)"] = 'Taiwan'
map1$location[map1$location == "Republic of Korea"] = 'South Korea'
map1$location[map1$location == "United Republic of Tanzania"] = 'Tanzania'
map1$location[map1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
map1$location[map1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
map1$location[map1$location == "Czechia"] = 'Czech Republic'
map1$location[map1$location == "Republic of Moldova"] = 'Moldova'
map1$location[map1$location == "Viet Nam"] = 'Vietnam'
map1$location[map1$location == "Lao People's Democratic Republic"] = 'Laos'
map1$location[map1$location == "Syrian Arab Republic"] = 'Syria'
map1$location[map1$location == "North Macedonia"] = 'Macedonia'
map1$location[map1$location == "Micronesia (Federated States of)"] = 'Micronesia'
map1$location[map1$location == "Macedonia"] = 'North Macedonia'
map1$location[map1$location == "Trinidad and Tobago"] = 'Trinidad'


a <- map1[map1$location == "Trinidad",]

a$location <- 'Tobago'
map1 <- rbind(map1,a)
map1$location[map1$location == "Cabo Verde"] = 'Cape Verde'

map1$location[map1$location == "United States Virgin Islands"] = 'Virgin Islands'
map1$location[map1$location == "Antigua and Barbuda"] = 'Antigu'
a <- map1[map1$location == "Antigu",]
a$location <- 'Barbuda'
map1 <- rbind(map1,a)
map1$location[map1$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
a <- map1[map1$location == "Saint Kitts",]
a$location <- 'Nevis'
map1 <- rbind(map1,a)
map1$location[map1$location == "Côte d'Ivoire"] = 'Ivory Coast'
map1$location[map1$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
a <- map1[map1$location == "Saint Vincent",]
a$location <- 'Grenadines'
map1 <- rbind(map1,a)
map1$location[map1$location == "Eswatini"] = 'Swaziland'
map1$location[map1$location == "Brunei Darussalam"] = 'Brunei'

#合并数据集
map1 <- full_join(world,map1,by = c('region'='location')) %>%
  filter(EAPC != "NA")


###绘图

fig1 <- map1 %>%  
  ggplot()+  
  geom_polygon(aes(x = long, y = lat, group = group, fill=val2),  
               colour="black", size=0.5) + #绘制地图  
  theme_void()+ #主题设置  
  scale_fill_manual(name = "EAPC in ASMR",  # 在这里添加图例标题  
                    values=c("#019e73","#56b4e8","#f0e53e",  
                             "#e79e01","#d65e00","#fe0000","#00468BFF")) + #手动添加颜色  
  theme(legend.position = c(0.15,0.23), #设置图例位置  
        legend.title = element_text(color="black", size=22), #设置图例标题样式  
        legend.text = element_text(color="black",  
                                   size = 22,  
        ), #设置图例文字  
        plot.title =  element_blank(), #取消图标题  
        panel.grid=element_blank(), #取消图网格线  
        axis.title.x = element_blank(), #取消x轴标题  
        axis.text.x = element_blank(), #取消x轴标签  
        axis.ticks.x = element_blank(), #取消x轴刻度标  
        axis.title.y = element_blank(), #取消y轴标题  
        axis.text.y = element_blank(), #取消y轴标签  
        axis.ticks.y = element_blank(), #取消y轴刻度标  
  )

fig1

ggsave("EAPC in ASMR男.JPEG",width = 12, height = 7,dpi = 1000) #使用ggsave()函数保存，图片质量好






# 读取EAPC in ASDR ----------------------------------------------------------

world <- map_data('world')
setwd("C:\\Users\\dell\\Desktop\\GBD数据\\地图\\2")

data1<- fread("204个国家肺癌的二手烟DALYS标准化率EAPC(男）.csv",header = T)

colnames(data1)
unique(data1$location_name)


map1 <- data1 %>%
  mutate(val2 = cut(EAPC, breaks = c(-6,-2.65,-1.40,0,0.25,1.00,100000000),
                    labels = c("<-2.65","-2.65~-1.40","-1.40~0.00","0.00~0.25","0.25~1.00",
                               ">1.00"),  
                    include.lowest = T,right = T)) #建立新列，连续变量转换成分类变量，包含最小值所在区间且包括左端点而不包括右端点

names(map1)[names(map1) == "location_name"] <- "location"  

###地区名称转换，方便之后数据集合并
map1$location[map1$location == 'United States of America'] = 'USA'
map1$location[map1$location == 'Russian Federation'] = 'Russia'
map1$location[map1$location == 'United Kingdom'] = 'UK'
map1$location[map1$location == 'Congo'] = 'Republic of Congo'
map1$location[map1$location == "Iran (Islamic Republic of)"] = 'Iran'
map1$location[map1$location == "Democratic People's Republic of Korea"] = 'North Korea'
map1$location[map1$location == "Taiwan (Province of China)"] = 'Taiwan'
map1$location[map1$location == "Republic of Korea"] = 'South Korea'
map1$location[map1$location == "United Republic of Tanzania"] = 'Tanzania'
map1$location[map1$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
map1$location[map1$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
map1$location[map1$location == "Czechia"] = 'Czech Republic'
map1$location[map1$location == "Republic of Moldova"] = 'Moldova'
map1$location[map1$location == "Viet Nam"] = 'Vietnam'
map1$location[map1$location == "Lao People's Democratic Republic"] = 'Laos'
map1$location[map1$location == "Syrian Arab Republic"] = 'Syria'
map1$location[map1$location == "North Macedonia"] = 'Macedonia'
map1$location[map1$location == "Micronesia (Federated States of)"] = 'Micronesia'
map1$location[map1$location == "Macedonia"] = 'North Macedonia'
map1$location[map1$location == "Trinidad and Tobago"] = 'Trinidad'


a <- map1[map1$location == "Trinidad",]

a$location <- 'Tobago'
map1 <- rbind(map1,a)
map1$location[map1$location == "Cabo Verde"] = 'Cape Verde'

map1$location[map1$location == "United States Virgin Islands"] = 'Virgin Islands'
map1$location[map1$location == "Antigua and Barbuda"] = 'Antigu'
a <- map1[map1$location == "Antigu",]
a$location <- 'Barbuda'
map1 <- rbind(map1,a)
map1$location[map1$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
a <- map1[map1$location == "Saint Kitts",]
a$location <- 'Nevis'
map1 <- rbind(map1,a)
map1$location[map1$location == "Côte d'Ivoire"] = 'Ivory Coast'
map1$location[map1$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
a <- map1[map1$location == "Saint Vincent",]
a$location <- 'Grenadines'
map1 <- rbind(map1,a)
map1$location[map1$location == "Eswatini"] = 'Swaziland'
map1$location[map1$location == "Brunei Darussalam"] = 'Brunei'

#合并数据集
map1 <- full_join(world,map1,by = c('region'='location')) %>%
  filter(EAPC != "NA")


###绘图

fig1 <- map1 %>%  
  ggplot()+  
  geom_polygon(aes(x = long, y = lat, group = group, fill=val2),  
               colour="black", size=0.5) + #绘制地图  
  theme_void()+ #主题设置  
  scale_fill_manual(name = "EAPC in ASDR",  # 在这里添加图例标题  
                    values=c("#019e73","#56b4e8","#f0e53e",  
                             "#e79e01","#d65e00","#fe0000","#00468BFF")) + #手动添加颜色  
  theme(legend.position = c(0.15,0.23), #设置图例位置  
        legend.title = element_text(color="black", size=22), #设置图例标题样式  
        legend.text = element_text(color="black",  
                                   size = 22,  
        ), #设置图例文字  
        plot.title =  element_blank(), #取消图标题  
        panel.grid=element_blank(), #取消图网格线  
        axis.title.x = element_blank(), #取消x轴标题  
        axis.text.x = element_blank(), #取消x轴标签  
        axis.ticks.x = element_blank(), #取消x轴刻度标  
        axis.title.y = element_blank(), #取消y轴标题  
        axis.text.y = element_blank(), #取消y轴标签  
        axis.ticks.y = element_blank(), #取消y轴刻度标  
  )

fig1

ggsave("EAPC in ASDR男.JPEG",width = 12, height = 7,dpi = 1000) #使用ggsave()函数保存，图片质量好











# 组合 ----------------------------------------------------------------------


setwd("C:\\Users\\dell\\Desktop\\GBD数据\\地图\\2")
# 加载包  
library(magick)  

# 读取图片  
# 将下面的路径替换为你的图片实际路径  

# 首先查看当前工作目录  
getwd()  

# 查看目录中的文件  


# 读取图片  
fig1<- image_read("ASMR男.JPEG")%>% image_ggplot()  
print(fig1)
fig2<- image_read("ASDR男.JPEG")%>% image_ggplot()  
print(fig2)
fig3<- image_read("ASMR女.JPEG")%>% image_ggplot()  
print(fig3)
fig4<- image_read("ASDR女.JPEG")%>% image_ggplot()  
print(fig4)
# 如果还没安装patchwork包，先安装  
# install.packages("patchwork")  

library(patchwork)  

# 组合图片  
combined_plot <- (fig1 + fig2) / (fig3 + fig4) + # 使用 / 表示换行，+ 表示并列  
  plot_annotation(tag_levels = 'A') +  # 自动添加 A, B, C, D 标签  
  plot_layout(guides = 'collect')  # 收集并统一图例位置  

# 调整标签样式  
combined_plot <- combined_plot &   
  theme(plot.tag = element_text(size = 12, face = "bold"))  # 设置标签字体大小和样式  

# 显示图片  


ggsave("ASMR和ASDR组合图片男女（AB为男，CD为女）.JPEG",width = 12, height = 7,dpi = 1000) #使用ggsave()函数保存，图片质量好










