p2 <- ggplot(data = data_plot2,
  aes(x=index,y=value,fill=index))+
  geom_violin(scale = 'width')+geom_boxplot(width=0.1)+
  xlab("year")+ylab("value")+
  labs(shape=NULL,color=NULL,fill=NULL)+
  theme_bw()

p2