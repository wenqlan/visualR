library(ggplot2)
library(dplyr)
#绘制面积图
cases <- read.table("~/Documents/post-one/zuotu/Supplementary%20Dataset%20S1%20MDPPF%20Brazil.txt",header=TRUE) #原始数据
#对原始数据各列进行处理
cases$people <- log(cases$people)
cases$year <- factor(cases$year)
cases$month_cont.f <- factor(cases$month_cont)
cases$week_cont.f <- factor(cases$week_cont)
cases$area <- factor(cases$area)
cases$period <- factor(cases$period)
cases$period_lag <- factor(cases$period_lag)
week_centered<-(cases$week_cont)-97
week_centered_lag<-(cases$week_cont)-100
cases$pre_range <- factor(cases$pre_range)

getOption("na.action")
options(na.action = na.fail) #如果遇到缺失值，R 将会停止执行并返回一个错误
getOption("digits")
options(digits=5) #显示最多 5 位小数
levels(cases$area) <- c("Control", "Intervention", "Buffer") #area 列转换为一个因子变量，级别被设置为 "Control"、"Intervention" 和 "Buffer"

epicases<-subset(cases,week_cont.f %in% c(1:25, 159:190))#筛选出在 1-25 周（2016 年流行病期间）和 159-190 周（2019 年流行病期间）的数据并存储
data_aggregated <- epicases %>%
  group_by(week_cont.f, area) %>%
  summarize(sum_dengue = sum(dengue),
            sum_people = sum(exp(people)))#计算登革热病例数和人口总和
write.csv(data_aggregated, file = "~/Documents/post-one/zuotu/data_aggregated.csv", row.names = FALSE)#计算登革热病例数和人口总和后的数据
data_aggregated <- read.csv("~/Documents/post-one/zuotu/data_aggregated.csv")
data_aggregated$week_cont.f <- factor(data_aggregated$week_cont.f)#将整数类型数据转换为因子类型
data_aggregated$area <- factor(data_aggregated$area, levels = c("Control", "Intervention", "Buffer"))#将 area列转换为因子且保持正确的水平和顺序
data_aggregated$ratio <- (data_aggregated$sum_dengue / data_aggregated$sum_people)*10000#计算每万人发病率
g2<-ggplot(data= data_aggregated) +
  geom_area(mapping = aes(x = week_cont.f, y = ratio, group = area,  fill = area), alpha=0.6)+
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "160", "165", "170", "175", "180", "185", "190"), labels = c("1", "5", "10", "15", "20", "25", "160", "165", "170", "175", "180", "185", "190")) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("Control" = "firebrick1", "Buffer" = "deepskyblue3", "Intervention" = "green3")) +
  geom_vline(xintercept = 26,color="grey",linetype="dashed",linewidth=1) +
  labs(x = "Week (1-25: 2016 epidemic; 160-190: 2019 epidemic)", y = "Dengue incidence (confirmed cases per 10000)") +
  guides(fill = guide_legend(title = "Area"))#画图
ggsave(filename = '~/Documents/post-one/zuotu/Area Plot.pdf', width = 30, height = 20, units = "cm", dpi = 400)


