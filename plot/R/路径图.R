library(tidyverse)
CHN_data <- read.csv(
  'D:/master_study/fills/r/rdata/GVCin5R6S/CHN.csv'
  )

kj <- read.csv(
  'D:/Desktop/高技术产业全球价值链/空间指标.csv'
  )

kj <- kj[kj$country=='CHN',]

CHN_R <- (CHN_data$RVGVCPt+CHN_data$RYGVCPt)/2
CHN_G <- (CHN_data$GVGVCPt+CHN_data$GYGVCPt)/2

CHN_RG <- CHN_G/CHN_R

kj$shete1 <- 1/kj$shete

kj2 <- kj[,c(4,5,7)] %>% scale(center = F,scale = T)
kj3 <- apply(kj2, 1, mean)

df <- data.frame(year=2000:2018,RG=CHN_RG[1:19],net=kj3)

ggplot(data = df[1:15,],aes(x=RG,y=net,color=year))+
  geom_point(size=3,shape=19)+
  #facet_wrap(.~year,nrow = 2)+
  scale_color_gradient(low = 'blue',high = 'red')+
  ggrepel::geom_text_repel(aes(label=year))+
  theme_bw()+
  geom_path()+
  theme(strip.text.x = element_text(size=13),
        strip.text.y = element_text(size=12, face="bold"),
        strip.background =
          element_rect(colour="black",
                       fill = 'white'))+
  xlim(0.7,1.3)+geom_vline(xintercept = 1)+
  geom_hline(yintercept = 0.5)





x <- c(0.8,1.05,1.225)
y <- c(0.25,0.125,0.46)
xend <- c(1.05,1.225,1.1)
yend <- c(0.125,0.46,0.895)

pl_df <- data.frame(x=x,y=y,xend=xend,yend=yend)
pl_df$period <- c('2000-2008','2008-2014','2014-2018')

ggplot()+xlim(0.7,1.3)+geom_vline(xintercept = 1)+
  ylim(0,1)+
  geom_hline(yintercept = 0.5)+
  theme_bw()+
  geom_segment(data=pl_df,aes(x=x,y=y,xend=xend,yend=yend,color=period),
             #angle = 30,
             size=2,
             arrow = arrow())+
  scale_color_brewer(palette = 'Set1')+
  labs(x='区域化/全球化GVCs参与度指数',y='GVCs国家网络空间指数')