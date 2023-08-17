library(corrplot)
ma=read.csv('D:\\master_study\\fills\\r\\相关性分析.csv',header=T)
row.names(ma) <- ma[,1]
ma <- ma[,-1]
str(ma)
ma <- as.matrix(ma)
corrplot(ma)#图一

corrplot(ma,method = 'shade',shade.col = NA,tl.col = 'black',tl.srt = 45)#图二

col <- colorRampPalette(c('#BB4444','#EE9988','#FFFFFF','#77AADD','#4477AA'))
corrplot(ma,method = 'shade',shade.col = NA,tl.col = 'black',tl.srt = 45,
         col=col(200),addCoef.col = 'black',cl.pos = 'no',order = 'AOE')#


library(ggplot2)
library(RColorBrewer)
library(reshape2)
mydata1 <- melt(ma)
colnames(mydata1) <- c('var1','var2','value')
ggplot(mydata1,aes(x=var1,y=var2,fill=value,label=value))+
  geom_tile(colour='black')+
  geom_text(size=3,colour='white')+
  coord_equal()+
  scale_fill_gradientn(colours = c(brewer.pal(
    7,'Set1'
  )[2],'white',brewer.pal(7,'Set1')[1]),na.value = NA)

mydata1$absvalue <- abs(mydata1$value)
ggplot(mydata1,aes(x=var1,y=var2))+
  geom_point(aes(size=absvalue,fill=value),shape=21,colour='black')+
  scale_fill_gradientn(colours = c(brewer.pal(
    7,'Set1'
  )[2],'white',brewer.pal(7,'Set1')[1]),na.value = NA)+
  scale_size_area(max_size = 12,guide=F)
