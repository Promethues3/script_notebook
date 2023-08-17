x=read.csv("qianxiang(1).csv")
y=x[,2:3]#前向数据计算
A3=matrix(0,nrow=660,ncol=3)#全部
A2=matrix(0,nrow=660,ncol=3)#服务业
A1=matrix(0,nrow=660,ncol=3)#制造业
for (i in 1:660){
		a=1+(i-1)*56
		b=56*i	
        	B=y[a:b,]
		t11=B[11,1]+B[12,1]+B[17,1]+B[18,1]+B[19,1]+B[20,1]+B[21,1]
		t12=B[11,2]+B[12,2]+B[17,2]+B[18,2]+B[19,2]+B[20,2]+B[21,2]
		A1[i,1]=t11;A1[i,2]=t12;A1[i,3]=t12/t11
		t21=B[39,1]+B[40,1]+B[47,1]
		t22=B[39,2]+B[40,2]+B[47,2]
		A2[i,1]=t21;A2[i,2]=t22;A2[i,3]=t22/t21
		t31=B[11,1]+B[12,1]+B[17,1]+B[18,1]+B[19,1]+B[20,1]+B[21,1]+B[39,1]+B[40,1]+B[47,1]
		t32=B[11,2]+B[12,2]+B[17,2]+B[18,2]+B[19,2]+B[20,2]+B[21,2]+B[39,2]+B[40,2]+B[47,2]
	      A3[i,2]=t32;A3[i,3]=t32/t31;A3[i,1]=t31
}
x2=read.csv("后向.csv")
x2
y1=x2[,2:3]
C3=matrix(0,nrow=660,ncol=3)#全部
C2=matrix(0,nrow=660,ncol=3)#服务业
C1=matrix(0,nrow=660,ncol=3)#制造业
for (i in 1:660){
		a=1+(i-1)*56
		b=56*i	
        	B=y1[a:b,]
		t11=B[11,1]+B[12,1]+B[17,1]+B[18,1]+B[19,1]+B[20,1]+B[21,1]
		t12=B[11,2]+B[12,2]+B[17,2]+B[18,2]+B[19,2]+B[20,2]+B[21,2]
		C1[i,1]=t11;C1[i,2]=t12;C1[i,3]=t12/t11
		t21=B[39,1]+B[40,1]+B[47,1]
		t22=B[39,2]+B[40,2]+B[47,2]
		C2[i,1]=t21;C2[i,2]=t22;C2[i,3]=t22/t21
		t31=B[11,1]+B[12,1]+B[17,1]+B[18,1]+B[19,1]+B[20,1]+B[21,1]+B[39,1]+B[40,1]+B[47,1]
		t32=B[11,2]+B[12,2]+B[17,2]+B[18,2]+B[19,2]+B[20,2]+B[21,2]+B[39,2]+B[40,2]+B[47,2]
	      C3[i,2]=t32;C3[i,3]=t32/t31;C3[i,1]=t31
}
z=read.csv("国家.csv",head=F)
library(ggplot2)
library(ggrepel)
data=data.frame(country=c(z[1:43,]),高技术制造业前向参与度=c(A1[573:615,3]),高技术服务业前向参与度=c(A2[573:615,3]),高技术产业前向参与度=c(A3[573:615,3]),高技术制造业后向参与度=c(C1[573:615,3]),高技术服务业后向参与度=c(C2[573:615,3]),高技术产业后向参与度=c(C3[573:615,3]))
#制造业画图#需要加载ggplot2,ggrepel两个包
ggplot(data) +
     geom_point(aes(高技术制造业前向参与度, 高技术制造业后向参与度),color = 'red') +
     geom_text_repel(aes(高技术制造业前向参与度, 高技术制造业后向参与度, label = country)) +
     theme_classic(base_size = 16)+
	geom_abline(intercept=0,slope=1 )+
	xlim(0,0.7)+
	ylim(0,0.7)
#服务业画图
ggplot(data) +
     geom_point(aes(高技术服务业前向参与度, 高技术服务业后向参与度),color = 'red') +
     geom_text_repel(aes(高技术服务业前向参与度, 高技术服务业后向参与度, label = country)) +
     theme_classic(base_size = 16)+
	geom_abline(intercept=0,slope=1 )+
	xlim(0,0.7)+
	ylim(0,0.7)
#高技术产业画图
ggplot(data) +
     geom_point(aes(高技术产业前向参与度, 高技术产业后向参与度),color = 'red') +
     geom_text_repel(aes(高技术产业前向参与度, 高技术产业后向参与度, label = country)) +
     theme_classic(base_size = 16)+
	geom_abline(intercept=0,slope=1 )+
	xlim(0,0.7)+
	ylim(0,0.7)

