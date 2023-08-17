p3 <- ggplot(data = data_plot4,aes(x=value,color=index,
                                   linetype=index))+
  geom_density(size=1)+theme_bw()+
  labs(shape=NULL,color=NULL,fill=NULL,linetype=NULL)+
  xlab("value")+
  scale_color_manual(values = c("blue",
                                "green",
                                "purple",
                                "red",
                                'yellow',
                                'orange',
                                'black'))

p3