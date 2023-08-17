p4 <- ggplot(data = data_plot2,aes(sample=value,
                                   color=index
                                   ))+
  geom_qq(size=0.7)+geom_qq_line()+
  theme_bw()+
  facet_wrap(index~.,scales = 'free')+
  labs(shape=NULL,color=NULL,
       fill=NULL,linetype=NULL)+
  scale_color_brewer(palette = 'Set1')
  
p4