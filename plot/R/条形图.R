library(tidyverse)

df <- read.csv('D:/master_study/fills/r/domestic/柱状图.csv')
df_long <- reshape2::melt(
  df,id.vars=c('年份','省份'),
  measure.vars=c('基础部门','融合部门','替代部门'),
  variable.name='部门',
  value.name = '数字技术使用量')
df_long$年份 <- as.numeric(df_long$年份)
df_long$x <- rep(1,nrow(df_long))
df_long$x[df_long$省份=='广东'] <- 2
df_long$x[df_long$省份=='江苏'] <- 3
df_long$x[df_long$省份=='山东'] <- 4
df_long$x[df_long$省份=='上海'] <- 5
df_long$x[df_long$省份=='浙江'] <- 6

t0 <- ggplot()+
  geom_bar(data=df_long[df_long$年份==2012,],
           aes(x=x,
               y=数字技术使用量,fill=部门,
               color=as.factor(年份),
               group=as.factor(年份)),
    stat = 'identity',
    position = "stack",
    width=0.3)+
  geom_bar(data=df_long[df_long$年份==2017,],
           aes(x=x+0.3+0.1,
               y=数字技术使用量,fill=部门,
               color=as.factor(年份),
               group=as.factor(年份)),
           stat = 'identity',
           position = "stack",
           width=0.3)+
  scale_x_continuous(breaks = c(1.2,2.2,3.2,4.3,5.2,6.2),
                     labels = c('北京','广东','江苏',
                                '山东','上海','浙江'))+
  #scale_fill_brewer(palette = 'Set1')+
  labs(fill=NULL,color=NULL)+theme_bw()+
  scale_color_manual(values=c('black','red'))+
  xlab('地区')+ylab('数字化改革的产业规模')
t0
ggsave('D:/master_study/fills/r/domestic/数字化改革的产业规模.png',t0,width = 6,height = 4,dpi=500)


t1 <- ggplot(data=df_long,
             aes(x=x,y=数字技术使用量,fill=部门))+
  geom_bar(stat = 'identity',
           position = "stack",
           width=0.3)+
  facet_wrap(年份~.)+
  scale_fill_brewer(palette = 'Set1')+
  scale_x_continuous(breaks = c(1.2,2.2,3.2,4.3,5.2,6.2),
                     labels = c('北京','广东','江苏',
                                '山东','上海','浙江'))+
  #scale_fill_brewer(palette = 'Set1')+
  labs(fill=NULL,color=NULL)+
  theme_bw()+
  xlab('地区')+ylab('数字化改革的产业规模')
t1
ggsave(
  'D:/master_study/fills/r/domestic/数字化改革的产业规模2.png',
  t1,width = 6,height = 4,dpi=500)
