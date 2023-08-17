library(tidyverse)
library(reshape2)
df <- read.csv("D:/Desktop/数据.csv")
colnames(df) <- c("对比组","参照组")
df$`移动后参照组` <- df$`参照组`
df$`移动后参照组` <- df$`移动后参照组`+4000
gd1_long1<-melt(df,
                measure.vars = c("对比组","参照组","移动后参照组"),#用于聚合的变量,
                variable.name='year',
                value.name='value')

p1 <- ggplot(gd1_long1[gd1_long1$value<30000,],aes(x=value,   
                  # fill填充颜色，根据变量名赋值
              colour=year,
              linetype=year))+ # colour图形边界颜色，根据变量名赋值
  geom_density(alpha=0.2,        # 填充颜色透明度
               size=0.75,
               adjust = 1.5# 线条粗细 # 线条类型1是实线，2是虚线
  )+
  scale_color_brewer(palette = "Set1")+
  theme_bw()+
  theme(legend.position = "top"
        )+
  labs(shape=NULL,color=NULL,fill=NULL,linetype=NULL)+
  geom_vline(xintercept = 10000,linetype=1,size=0.5)+
  geom_vline(xintercept = 12000,linetype=2,size=0.5)+
  geom_vline(xintercept = 15000,linetype=3,size=0.5)+
  geom_vline(xintercept = 18000,linetype=4,size=0.5)+
  annotate("text", x = 9500 , y = 0.000065,label = "A")+
  annotate("text", x = 9500 , y = 0.000045,label = "B")+
  annotate("text", x = 9500 , y = 0.000034,label = "C")+
  annotate("text", x = 11500 , y = 0.00004,label = "A1")+
  annotate("text", x = 11500 , y = 0.00003,label = "B1")+
  annotate("text", x = 11500 , y = 0.00002,label = "C1")+
  annotate("text", x = 14500 , y = 0.00003,label = "D")+
  annotate("text", x = 14500 , y = 0.00002,label = "E")+
  annotate("text", x = 14500 , y = 0.00001,label = "F")+
  annotate("text", x = 17500 , y = 0.000018,label = "D1")+
  annotate("text", x = 17500 , y = 0.00001,label = "E1")+
  annotate("text", x = 17500 , y = 0.000005,label = "F1")

p1
ggsave("D:/Desktop/p1.png",p1,dpi = 300,height = 5,width = 8)
