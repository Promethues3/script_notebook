#####################************************************######################
#                                                                             #
#                    Created by LiBin     Date:18/12/2020                     #
#                        Email:1499237580@zufe.edu.cn                         #
#                Zhejiang Univercity of Finance & Economics                   #
#                                                                             #
#                                                                             #
#####################************************************######################
###############################第二章#############################
library(gcookbook)
#2.1绘制一个普通的散点图
plot(mtcars$wt,mtcars$mpg)
x=mtcars$wt
y=mtcars$mpg
library(ggplot2)
#散点图
#利用ggplot2辑程包来绘制散点图
qplot(x,y)
#如果两个变量在同一个数据框dataframe里可以使用如下函数
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()

#2.2折线图
pressure
plot(pressure$temperature,pressure$pressure,type = "l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col='red')
points(pressure$temperature,pressure$pressure/2,col='red')
#利用ggplot2辑程包绘制折线图
library(ggplot2)
qplot(pressure$temperature,pressure$pressure,geom = "line")
qplot(temperature,pressure,data=pressure,geom = 'line')
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()


#2.3绘制条形图
str(BOD$Time)
barplot(BOD$demand,names.arg = BOD$Time)
#利用table函数汇总某一变量的特征频数
table(mtcars$cyl)
barplot(table(mtcars$cyl))
#利用ggplot2辑程包绘制条形图
library(ggplot2)
ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat = 'identity')
qplot(factor(cyl),data = mtcars)
ggplot(mtcars,aes(factor(cyl)))+geom_bar()


#2.4绘制直方图
hist(mtcars$mpg)
hist(mtcars$mpg,breaks = 10)
#ggplot2绘制直方图
library(ggplot2)
qplot(mpg,data = mtcars,binwidth=4)
qplot(mtcars$mpg)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 4)


#2.5绘制箱线图
plot(ToothGrowth$supp,ToothGrowth$len)
#利用ggplot2绘制箱线图
library(ggplot2)
qplot(ToothGrowth$supp,ToothGrowth$len,geom = 'boxplot')
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
#使用interaction()函数将分组变量组合在一起也可以绘制基于多分组变量的箱线图
qplot(interaction(supp,dose),len,data = ToothGrowth,geom = 'boxplot')
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()



#2.6绘制函数图像
curve(x^2+2*x,from=0,to=20)
#建立一个封装函数
myfun=function(xvar){1/(1+exp(-xvar+10))}
curve(myfun(x),from = 0,to=20)
ggplot(data.frame(x=c(0,20)),aes(x=x))+stat_function(fun=myfun,geom = 'line')



###################################第三章##################################
#3.1绘制简单的条形图
#有一个包含了两列数据 的数据框，其中一列数据表示条形在x轴上的位置，
#另一列表示每个条形在y轴上对应的高度，基于此，如何绘制条形图?


#使用ggplot()函数和geom_bar(stat='identity')绘制条形图
library(ggplot2)
library(gcookbook)
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat = 'identity')
#当x是连续型（数值型）变量时，ggplot不是只在实际值处绘制条形，而将在x轴上介于
#最大值和最小值之间所有可能的取值处绘图
#因此我们也可以使用factor()函数将连续性变量转化为离散型变量。
BOD
ggplot(BOD,aes(x=Time,y=demand))+geom_bar(stat = 'identity')
ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat = 'identity')
#我们可以在geom_bar函数中利用fill参数对填充颜色进行调整，利用colour参数对
#边框颜色进行调整
ggplot(BOD,aes(x=factor(Time),y=demand))+
  geom_bar(stat = 'identity',fill='lightblue',colour='black')


#3.2绘制簇状条形图
#将分类变量映射到fill参数当中，并运行命令geom_bar(position='dodge')
library(gcookbook)#调用数据
library(ggplot2)
cabbage_exp
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(position = 'dodge',stat = 'identity')
#在geom_bar中调整参数colour，还可以通过scale_fill_brewer()
#或者scale_fill_manual()函数对图形颜色进行设置
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(position = 'dodge',stat = 'identity',colour='black')+
  scale_fill_brewer(palette = 'Pastell')


#3.3绘制频数条形图
#使用geom_bar()函数，同时不要有任何变量映射到y参数
ggplot(diamonds,aes(x=cut))+geom_bar()#等价于使用geom_bar(stat='bin')
#当x轴对应的是连续性变量时，我们会得到一张直方图
ggplot(diamonds,aes(x=carat))+geom_bar()#此时效果和geom_histogram()效果相同



#3.4条形图的着色
#将合适的变量映射到填充颜色fill即可。
library(gcookbook)#调用内部数据
upc=subset(uspopchange,rank(Change)>40)
upc
library(ggplot2)
ggplot(upc,aes(x=Abb,y=Change,fill=Region))+geom_bar(stat = 'identity')
#条形图的默认颜色不太吸引眼球，因此，可能需要借助函数scale_fill_brewer()
#或scale_fill_manual()重新设定颜色。我们通过把参数指定为colour='black'
#将条形的边框线设定为黑色
ggplot(upc,aes(x=Abb,y=Change,fill=Region))+
  geom_bar(stat = 'identity',colour='black')+
  scale_fill_manual(values = c('#FFDDEE','#443322'))+
  xlab('State')



#3.5对正负条形图分别着色
library(gcookbook)#调用内部数据
library(ggplot2)
csub=subset(climate,Source=='Berkeley'&Year>=1900)
csub$pos=csub$Anomaly10y>=0
#在数据中加入变量pos，并且映射到fill变量中。将参数设定position='identity'.
ggplot(csub,aes(x=Year,y=Anomaly10y,fill=pos))+
  geom_bar(stat = 'identity',position = 'identity',colour='black')+
  scale_fill_manual(values = c('#998800','#334455'))



#3.6调整条形宽度和条形间距
#通过设定geom_bar()函数的参数width可以使条形变得更宽或者更窄。
#该参数的默认值为0.9；更大的值回是绘制的条形更宽，反之则更窄。
library(gcookbook)#调用内部数据
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat = 'identity')
#让条形更窄一些
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat = 'identity',width = 0.5)
#宽些的条形图(条形图的最大宽度是1)
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat = 'identity',width = 1)
#簇状条形图默认组内的条形间距为0.如果希望增加组内条形的间距，
#则可以通过将width设定得更小一些，并令position_dodge得取值大于width
#更窄得簇状条形图可运行：
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity',width = 0.5,position = 'dodge')
#添加条形组距可运行:
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity',width = 0.5,position=position_dodge(0.7))



#3.7绘制堆积条形图
#使用geom_bar()函数，并映射一个变量给填充色参数(fill)即可
library(gcookbook)#为了使用数据
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity')
#弄清楚图形对应得数据结构有助于理解图形得绘制过程。
#上述数据集中Date对应三个水平，Cultivar变量对应于两个水平，
#两个变量不同水平的组合又分别与一个Weight变量相对应：
cabbage_exp
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity')+
  guides(fill=guide_legend(reverse = TRUE))
#如果想改变堆叠的顺序，可以使用order=desc()
library(plyr)#为了使用desc()函数
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar,order=desc(Cultivar)))+
  geom_bar(stat = 'identity')




#3.8绘制百分比堆积条形图
#首先要通过plyr包中的ddply()函数和transform()函数将每组条形对应的数据标准化
#为100%格式，之后，针对计算的得到的结果绘制堆积条形图即可
library(gcookbook)#为了使用数据
library(plyr)
library(ggplot2)
#以Date为切割变量，对每组数据进行transform()
ce=ddply(cabbage_exp,'Date',transform,percent_weight=Weight/sum(Weight)*100)
ggplot(ce,aes(x=Date,y=percent_weight,fill=Cultivar))+
  geom_bar(stat = 'identity')




#3.9添加数据标签
library(gcookbook)#为了使用数据
#在条形图顶端下方
ggplot(cabbage_exp,aes(x=interaction(Date,Weight),y=Cultivar))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=Weight),vjust=1.5,colour='white')
#在条形图的上方
ggplot(cabbage_exp,aes(x=interaction(Date,Weight),y=Cultivar))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=Cultivar),vjust=-0.2)
#将y轴的上限放大
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=Weight),vjust=-0.2)+
  ylim(0,max(cabbage_exp$Weight)*1.5)
#y轴自动调整
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+
  geom_bar(stat = 'identity')+
  geom_text(aes(y=Weight+0.1,label=Weight))
#向堆积条形图添加数据标签之前，先要对每组条形对应的数据进行累计求和。
#我们可以使用plyr包中的arrange()函数完成上述操作，
#plyr包是一个随ggplot2包加载的软件包
library(plyr)
library(ggplot2)
ce=arrange(cabbage_exp,Date,Cultivar)
#计算累计和
ce=ddply(ce,"Date",transform,label_y=cumsum(Weight))
ce
ggplot(ce,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity',colour='black')+
  guides(fill=guide_legend(reverse = F))+
  geom_text(aes(y=label_y,label=Weight),vjust=1.5,colour='white')+
  scale_fill_brewer(palette = "Pastell")




#3.10绘制Cleveland点图
#最简单的绘制点图的方法是直接运行geom_point()
library(ggplot2)
library(gcookbook)#为了使用数据
tophit=tophitters2001[1:25,]#取前25行的数据
ggplot(tophit,aes(x=avg,y=reorder(name,avg)))+geom_point()



##############################第四章##########################
#4.1 绘制简单的折线图
#运行ggplot()和geom_line()函数，并分别指定一个变量映射给x和y
library(ggplot2)
ggplot(BOD,aes(x=Time,y=demand))+geom_line()
#通过factor()函数将Time转化为分类变量，当x是因子型变量时必须使用命令
#aes(group=1)
BOD1=BOD
BOD1$Time=factor(BOD1$Time)
ggplot(BOD1,aes(x=Time,y=demand,group=1))+geom_line()
#将y轴设为从0开始
ggplot(BOD1,aes(x=Time,y=demand,group=1))+geom_line()+
  ylim(0,max(BOD1$demand))
#or
ggplot(BOD1,aes(x=Time,y=demand,group=1))+geom_line()+
  expand_limits(y=0)


#4.2向折线图添加数据标记
#在代码后加geom_point()
ggplot(BOD1,aes(x=Time,y=demand,group=1))+geom_line()+
  geom_point()



#4.3绘制多重折线图
#方法：在分别设定给x和y的基础上，再将另一个变量映射给颜色(colour)
#或者线形(linetype)的supp变量
library(plyr)
tg=ddply(ToothGrowth,c('supp','dose'),summarise,length=mean(len))
library(ggplot2)
ggplot(tg,aes(x=dose,y=length,colour=supp))+geom_line()
ggplot(tg,aes(x=dose,y=length,linetype=supp))+geom_line()
#当x作为分类变量的时候
ggplot(tg,aes(x=factor(dose),y=length,colour=supp,group=supp))+geom_line()
#如果折线图上有数据标记，也可以就将分组变量映射给有数据标记的属性，诸如shape和fill
ggplot(tg,aes(x=dose,y=length,fill=supp))+geom_line()+geom_point(size=4,shape=21)
ggplot(tg,aes(x=dose,y=length,shape=supp))+geom_line()+geom_point(size=4)



#4.6 绘制面积图
#运用geom_area()
sunspot.year
sunspotyear=data.frame(Year=as.numeric(time(sunspot.year)),
                       Sunspots=as.numeric(sunspot.year))
ggplot(sunspotyear,aes(x=Year,y=Sunspots))+geom_area()
#设置线的与填充的颜色,透明度alpha
ggplot(sunspotyear,aes(x=Year,y=Sunspots))+
  geom_area(colour='black',fill='lightblue',alpha=0.2)




#4.7绘制面积图
library(gcookbook)
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup,order=desc(AgeGroup)))+
  geom_area(colour='black',alpha=.4)+
  scale_fill_brewer(palette = 'Blues')



#4.8绘制百分比堆积图
library(plyr)
library(gcookbook)
uspopage_prop=ddply(uspopage,'Year',transform,
                    Percent=(Thousands/sum(Thousands))*100)
ggplot(uspopage_prop,aes(x=Year,y=Percent,fill=AgeGroup))+
  geom_area(colour='black',alpha=.4)+
  scale_fill_brewer(palette = 'Blues')




#4.9添加置信域
#方法：运行geom_ribbon(),然后分别映射一个变量给ymin和ymax。
#climate数据集中的Anomaly10y变量表示了各年温度相对于1950-1980平均水平变异
#(以摄氏度衡量)的10年移动平均。变量Unc10y表示其95%置信水平下的置信区间。我们令
#ymax和ymin分别设定为Anomaly10y加减Unc10y：
library(gcookbook)#为了使用数据
library(ggplot2)
clim=subset(climate,Source=='Berkeley',
            select = c('Year','Anomaly10y','Unc10y'))
ggplot(clim,aes(x=Year,y=Anomaly10y))+
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y,ymax=Anomaly10y+Unc10y),alpha=.2)+
  geom_line()
#除了使用阴影区域，我们还可以使用虚线来表示置信域的上下边界
ggplot(clim,aes(x=Year,y=Anomaly10y))+
  geom_line(aes(x=Year,y=Anomaly10y-Unc10y),colour='grey50',linetype='dotted')+
  geom_line(aes(x=Year,y=Anomaly10y+Unc10y),colour='grey50',linetype='dotted')+
  geom_line()




##################################第五章##################################
#5.1绘制基本的散点图
#方法：运行geom_point()函数，分别映射变量x和y,heightweight是个多列数据集
#接下来的例子我们只用到其中两列。
library(gcookbook)#为了使用数据
library(ggplot2)
heightweight[,c('ageYear','heightIn')]
ggplot(heightweight,aes(x=ageYear,y=heightIn))+
  geom_point()
#通过设定点型(shape)参数可以在散点图中绘制默认值以外的点型。空心圆（点型21）,
#实心圆(点型16)
ggplot(heightweight,aes(x=ageYear,y=heightIn))+
  geom_point(shape=21)
#大小(size)参数可以控制途中点的大小。系统默认的大小(size)值为2，接下来
#我们将其设定为大小size=1.5，且将点型设为19以修正16（抗锯齿）
ggplot(heightweight,aes(x=ageYear,y=heightIn))+
  geom_point(shape=19,size=1.5)
ggplot(heightweight,aes(x=ageYear,y=heightIn))+
  geom_point(shape=16,size=1.5)



#5.2 使用点型和颜色属性，并基于某变量对数据进行分组
library(gcookbook)#为了使用数据
library(ggplot2)
#列出要用的三个列
heightweight[,c('sex','ageYear','heightIn')]
ggplot(heightweight,aes(x=ageYear,y=heightIn,shape=sex,colour=sex))+
  geom_point()
ggplot(heightweight,aes(x=ageYear,y=heightIn,colour=sex))+
  geom_point()




#5.3 使用不同于默认设置的点型
#方法通过指定geom_point()函数中的点型shape参数可以设定散点图中所有数据点的点型
library(gcookbook)#为了使用数据
library(ggplot2)
ggplot(heightweight,aes(x=ageYear,y=heightIn))+geom_point(shape=3)
#如果已将分组变量映射给shape，则可以调用scale_shape_manual()函数来修改点型：
ggplot(heightweight,aes(x=ageYear,y=heightIn,shape=sex))+
  geom_point(size=3)+
  scale_shape_manual(values = c(1,4))
#点型1-20的点的颜色，包括实心区域的颜色都可由colour参数来控制。对于点型21-25
#而言，边框线和实心区域的颜色则分别由colour和fill参数控制。
#我们可以用点型和填充色属性分别表示两个不同的变量。但是这一过程不太直接，
#我们需要选择一个同时具有colour和fill属性的点型及一个包括NA和其它颜色的调色板
#（NA会生成一个空心的形状）。

#以heightweight数据集为例，同时在数据集中增加一个用来标识儿童体重是否超过100磅
#的列
hw=heightweight
hw$weightGroup=cut(hw$weightLb,breaks = c(-Inf,100,Inf),
                   labels = c('<100','>100'))
#使具有颜色和填充色的点型对应空值和填充色的颜色
ggplot(hw,aes(x=ageYear,y=heightIn,shape=sex,fill=weightGroup))+
  geom_point(size=3)+
  scale_shape_manual(values = c(21,24))+
  scale_fill_manual(values = c(NA,'black'),
                    guide=guide_legend(override.aes = list(shape=21)))


#5.4 将连续型变量映射到点的颜色或大小属性上
#方法：将连续型变量映射到size或者colour属性上即可。heightweight数据集有很多列
#下面的例子中只用其中四列
library(gcookbook)#为了使用数据
library(ggplot2)
heightweight[,c('sex','ageYear','heightIn','weightLb')]
#在5.1节中的散点图刻画了两个连续型变量ageYear和heightIn的关系，
#如果想要表示第三个连续性变量WeightLb,必须将其映射给其他的图形属性
ggplot(heightweight,aes(x=ageYear,y=heightIn,size=weightLb))+
  geom_point()
ggplot(heightweight,aes(x=ageYear,y=heightIn,colour=weightLb))+
  geom_point()
#由于人类对于位置的差异较为敏感，所以将较为重要的变量映射给x和y，而对于颜色和大小
#人类并不敏感，为了增加观感上的差异使用21-25号点型
ggplot(heightweight,aes(x=ageYear,y=heightIn,fill=weightLb))+
  geom_point(size=2.5,shape=21)+
  scale_fill_gradient(low='white',high = 'black')
#使用guide_legend()函数以离散的图例代表色阶
ggplot(heightweight,aes(x=ageYear,y=heightIn,fill=weightLb))+
  geom_point(size=2.5,shape=21)+
  scale_fill_gradient(low='white',high = 'black',
                      breaks=seq(70,170,by=20),guide=guide_legend())
#当我们把一个连续型变量映射给某个图形属性之后，这并不妨碍我们同时将分类变量映射
#给其他图形属性
ggplot(heightweight,aes(x=ageYear,y=heightIn,size=weightLb,colour=sex))+
  geom_point(alpha=.4)+
  scale_size_area()+
  scale_color_brewer(palette = 'Set1')



#5.5 处理图形重叠
#如果图形的重叠程度比较低，我们可以通过使用较小的数据点或者使用不会遮盖其他数据
#点的点型（例如1号空心圆）来避免数据重叠
library(ggplot2)
sp=ggplot(diamonds,aes(x=carat,y=price))
sp+geom_point()
#设定alph参数，将点的透明度调整为半透明
sp+geom_point(alpha=0.1)#90%的透明度
sp+geom_point(alpha=0.01)#99%的透明度
#另一个方法是将数据分箱(bin)并以矩形来表示，同时将数据点的密度映射为矩形的
#填充色调用stat_bin2d()函数对数据进行分箱。默认情况下stat_bin2d()函数分别在
#x轴和y轴方向上将数据分割为30个组，总计900个箱子。
sp+stat_bin2d()
sp+stat_bin2d(bins = 50)+
  scale_fill_gradient(low = 'lightblue',high = 'red',limits=c(0,6000))
#如果不想将数据分箱并以矩形表示的话，可以调用stat_binhex()函数使用六边形代替
#先运行install.package('hexbin')m命令安装hexbin包
#install.packages('hexbin')
library(hexbin)
sp+stat_binhex()+
  scale_fill_gradient(low = 'lightblue',high = 'red',limits=c(0,8000))
sp+stat_binhex()+
  scale_fill_gradient(low = 'lightblue',high = 'red',
                      breaks=c(0,250,500,1000,2000,4000,6000),
                      limits=c(0,6000))
#当散点图的其中一个数据轴或者两个数据轴都对应离散数据时，也会出现图形重叠的情况
#这时候调用position_jitter()函数给数据点增加随机扰动。默认情况下，该函数在每个
#方向上添加随机扰动值为数据点最小精度的40%,可以通过width和height参数对该值进行调整
sp1=ggplot(ChickWeight,aes(x=Time,y=weight))
sp1+geom_point()
sp1+geom_point(position = 'jitter')
#当数据集对应于一个离散型数据轴和一个连续型数据轴时，箱线图可能时一种较好的展示
#方式，箱线图所表现的信息于散点图略有不同，因为它很难反映出离散坐标轴上每个数据
#点数量的信息。

#对于ChickWeights数据集，其对应的x轴本质上是离散的，但其被储存为数值型向量，因此，
#ggplot()函数不知该如何分组，如果不告诉ggplot()函数就会绘制以下图形。
sp1+geom_boxplot()
#通过aes(group=)可以告诉ggplot如何对数据分组
sp1+geom_boxplot(aes(group=Time))




#5.6 添加回归模型拟合线
#方法：运行stat_smooth()函数并设定method=lm即可向散点图中添加线性回归拟合线，这
#将调用lm()函数对数据拟合线性模型。首先，我们将基本绘图对象储存在对象sp中
library(ggplot2)
library(gcookbook)#为了使用数据
sp=ggplot(heightweight,aes(x=ageYear,y=heightIn))
sp+geom_point()+stat_smooth(method = lm)
#默认情况下，stat_smooth()函数会将默认添加95%的置信域，也可在level参数中进行修改
#设定参数se=FALSE时，系统将不会对回归拟合线添加置信域

#99%的置信域
sp+geom_point()+stat_smooth(method = lm,level=0.99)
#没有置信域
sp+geom_point()+stat_smooth(method = lm,se=F)
#拟合线默认是蓝色的，也可以在通过设定参数colour进行调整，还可以调整线形(linetype)
#大小(size)
sp+geom_point(colour='gray60')+stat_smooth(method = lm,se=F,colour='black')

#如果在stat_smooth()中未设定模型的类型，则默认为loss(局部加权多项式)
sp+geom_point()+stat_smooth()

#另一种常见的模型是Logistic回归。Logistic回归对heightweight数据集不适用，但对MASS
#包中的biopsy数据拟合效果良好，在进行模型拟合时要线进行数据预处理，我们必须将具有
#两个水平benign和malignan的因子型变量转化为具有0和1取值的向量
library(MASS)#为了使用数据
b=biopsy
b$classn[b$class=='benign']=0
b$classn[b$class=='malignant']=1
b
#由于本例数据点重叠比较严重，我们将数据点设为半透明(alpha=0.4)，点型为(shape=21)
#令stat_smooth()函数使用选项为family=binomial的glm()函数向散点图添加logistic回归
ggplot(b,aes(x=V1,y=classn))+
  geom_point(position = position_jitter(width = 0.3,height = 0.06),
             alpha=0.4,shape=21,size=1.5)+
  stat_smooth(method = glm,family=binomial)

#对不同的因子水平进行拟合，将分组变量映射给colour或者shape属性
sps=ggplot(heightweight,aes(x=ageYear,y=heightIn,colour=sex))+
  geom_point()+
  scale_color_brewer(palette = 'Set1')
sps+geom_smooth()



#5.7 根据已有的模型向散点图添加拟合线
#有时候我们想建立自己的模型，再将模型拟合线添加到散点图上，这样做可以使所用的
#模型图中保持一致。
#本例中，先使用lm()函数建立以ageYear为预测变量对heightIn进行预测的一个预测模型。
#然后调用predict()函数计算预测变量ageYear各取值对应的heightIn变量的预测值：
library(gcookbook)#为了使用数据
library(ggplot2)
model=lm(heightIn~ageYear+I(ageYear^2),heightweight)
model

#创建一个包含变量ageYear的列，并对其进行插值
xmin=min(heightweight$ageYear)
xmax=max(heightweight$ageYear)
predicted=data.frame(ageYear=seq(xmin,xmax,length.out = 100))
predicted$heightIn=predict(model,predicted)
predicted
#散点图和拟合曲线使用不同的两组数据
sp=ggplot(heightweight,aes(x=ageYear,y=heightIn))+
  geom_point(colour='grey40')
sp+geom_line(data=predicted,size=1)

#predictvals()函数可以简化向散点图添加模型拟合线的过程。使用时，只需向其传递一个
#模型作为参数，该函数就会自动查询变量名、预测变量范围、并返回一个包含预测变量和
#模型预测值的数据框。将该数据框。将该数据框传递给geom_line()函数即可绘制模型拟合线
predictvals=function(model,xvar,yvar,xrang=NULL,samples=100,...){
  #yvar是xvar和模型预测的变量
  #xrang是x轴的范围，当值为NULL时，等于模型对象中提取x轴范围；当设定为包含两个
  #数字的向量时，两个数字分别对应x轴范围的上下限
  #sample：x轴上包含的样本数量
  #...：可传递给predict()函数的其他参数
  if (is.null(xrang)) {
    if (any(class(model)%in%c('lm','glm')))
      xrange=range(model$model[[xvar]])
    else if (any(class(model)%in%'loess'))
      xrange=range(model$x)
  }
  newdata=data.frame(x=seq(xrange[1],xrange[2],length.out = samples))
  names(newdata)=xvar
  newdata[[yvar]]=predict(model,newdata = newdata,...)
  newdata
}

#调用lm()函数和loess()函数对数据集建立线性模型和LOWSS模型
modlinear=lm(heightIn~ageYear,heightweight)
modloess=loess(heightIn~ageYear,heightweight)
#针对两个模型分别调用predictedvals()函数，并将得到的结果传递给geom_line():
lm_predicted=predictvals(modlinear,'ageYear','heightIn')
loess_predicted=predictvals(modloess,'ageYear','heightIn')
sp+geom_line(data=lm_predicted,colour='red',size=.8)+
  geom_line(data = loess_predicted,colour='blue',size=.8)

#对于非线性链接函数的glm()模型，需要将predictvals()函数的参数设定为
#type="response"。这样做的原因在于，默认情况下该函数返回的预测结果是项的，
#而不是基于响应变量(y)的。

#以MASS包中的biopsy数据为例演示以下上述过程。与5.6节中的一样，下面用变量V1来预测
#变量class。Logistic模型对应的值须是介于0到1之间的数值，这里变量class是因子型变量
#因而，要先将变量class的取值转化为0和1
library(ggplot2)
library(MASS)#为了使用数据
b=biopsy
b$classn[b$class=='benign']=0
b$classn[b$class=='malignant']=1
#下面建立回归模型
fitlogistic=glm(classn~V1,b,family = binomial)
#获取预测值
glm_predicted=predictvals(fitlogistic,"V1","classn",type="response")

ggplot(b,aes(x=V1,y=classn))+
  geom_point(position = position_jitter(width = .3,height = .08),alpha=0.4
             ,shape=21,size=1.5)+
  geom_line(data=glm_predicted,colour="#1177FF",size=1)




#5.8 添加来自多个模型的拟合线
#方法：使用上文提到的predictvals()函数和来自plyr包的dlply()及ldply()函数即可。
#根据变量sex的水平对heightweight数据集进行分组，调用lm()函数对每组数据分别建立
#线性模型，并将模型结果存放在一个列表内。随后通过下面定义的make_model()函数
#建立模型。调用该函数时，向其输入一个数据框作为参数，该函数会返回一个lm()对象
#也可以根据数据集自定义模型：
make_model=function(data){
  lm(heightIn~ageYear,data)
}
library(gcookbook)#为了使用数据
library(plyr)
models=dlply(heightweight,"sex",.fun = make_model)
models
#得到模型对象后，配和使用ldply()函数和predictvals()函数即可获得两个模型对应的预测值
predvals=ldply(models,predictvals,xvar="ageYear",yvar='heightIn')
predvals
#再将散点图与模型拟合线叠加
library(ggplot2)
ggplot(heightweight,aes(x=ageYear,y=heightIn,colour=sex))+
  geom_point()+geom_line(data=predvals)
#让拟合线向外延伸，添加一个xrange参数
predvals=ldply(models,.fun = predictvals,xvar='ageYear',yvar='heightIn'
               ,xrange=range(heightweight$ageYear))

ggplot(heightweight,aes(x=ageYear,y=heightIn,colour=sex))+
  geom_point()+geom_line(data = predvals)



#5.9 向散点图添加模型系数
library(gcookbook)#为了使用数据
library(ggplot2)
model=lm(heightIn~ageYear,heightweight)
summary(model)
#结果表明模型的r^2值是0.4249。我们创建一个图形，并调用annotate()函数向其手动添加
#文本
pred=predictvals(model,'ageYear','heightIn')
sp=ggplot(heightweight,aes(x=ageYear,y=heightIn))+
  geom_point()+geom_line(data=pred)
sp+annotate("text",label="r^2=0.42",x=16.5,y=52)#纯文本
sp+annotate("text",label="r^2==0.42",parse=TRUE,x=16.5,y=52)#R的数学表达式语法

#ggplot2中的文本对象不能直接以表达式对象作为输入，其参数通常是一个字符串，接受
#字符串后，通过parse(text="a+b")函数将其转化为公式。
#使用数学公式作为注释时，必须使用正确的语法才能保证系统输出一个合法的R的数学表达
#式对象。把公式封装在expression()内部，检验其输出结果可以辅助判断R表达式的有效性
#本例中==是公式中表达等号的合法字符，而=是非法字符。
expression(r^2==0.42)#合法字符
#expression(r^2=0.42)#非法字符
#还可以自动提取模型对象的值并创建一个引用这些值的公式。
epn=as.character(as.expression(
  substitute(italic(y)==a+b*italic(x)*","~~italic(r)^2~'='~r2,
             list(a=format(coef(model)[1],digits = 3),
                  b=format(coef(model)[2],digits = 3),
                  r2=format(summary(model)$r.squared,digits = 2)))
))
epn
parse(text = epn)#解析并返回一个表达式
sp+annotate('text',label=epn,parse=TRUE,x=Inf,y=-Inf,hjust=1.1,vjust=-.5)



#5.10 向散点图添加边际地毯
#调用geom_rug()函数即可，下面以faithful数据集为例，该数据包含两列关于“老忠实喷泉”
#的信息：其中，一列是变量eruptiongs.记录的是喷泉每次喷发的时长；另一列是waiting
#记录的是喷泉在两次喷发之间的间隔
library(ggplot2)
ggplot(faithful,aes(x=eruptions,y=waiting))+geom_point()+geom_rug()
#边际地毯线严重重叠，可以设定size减小线宽并增加扰动
ggplot(faithful,aes(x=eruptions,y=waiting))+geom_point()+
  geom_rug(position = 'jitter',size=.2)



#5.11 向散点图添加标签
#方法：调用annotate()函数或者geom_text()函数可以为一个或几个数据点添加标签。
#下面以countries数据集为例，对各国医疗保健支出与每千新生婴儿死亡率之间的关系
#进化可视化。为了方便操作，选取人均支出大于2000美元的国家的数据子集进行分析。
library(ggplot2)
library(gcookbook)#为了使用数据
pp=subset(countries,Year==2009&healthexp>2000)
#先将基本散点图对象保存在sp中，再向其添加其他元素。要手动注释，可调用annotate()
#可能要尝试多次才能调整到合适位置。
sp=ggplot(pp,aes(x=healthexp,y=infmortality))+
  geom_point()
sp+annotate('text',x=4350,y=5.4,label='Canada')+
  annotate('text',x=7400,y=6.8,label='USA')
#要根据数据集自动添加标签，可以使用geom_text()函数，此时只需将一个因子型变量
#映射给label属性即可，为了避免数据点过于拥挤，我们使用略小一点的字号。默认size=5
sp+geom_text(aes(label=Name),size=4)
#系统自动放置注释时会将其中心置于x坐标和y坐标的位置。不过，我们可以对文本位置
#进行上下左右调整，或者两者兼做。
#设定vjust=0时，标签文本基线会与数据点对齐；设定vjust=1时，标签文本的顶部会与
#数据点对齐。但有时候，这些还不够，我们还可以通过增加或者减少vjust参数值来调高
#或者调低文本标签的位置；也可以通过y的映射增加或减少一个值的相同效果
sp+geom_text(aes(label=Name),size=4,vjust=0)
sp+geom_text(aes(y=infmortality+.1,label=Name),size=4,vjust=0)
#有时候，有必要根据数据点的位置令注释左对齐或右对齐。要左对齐，可设置hjust=0;
#要右对齐，可设置hjust=1。
sp+geom_text(aes(label=Name),size=4,hjust=0)
sp+geom_text(aes(x=healthexp+100,label=Name),size=4,hjust=0)
#如果只想给为数不多的几个点添加标签，但希望R自动设置标签位置的话，可以给数据框
#增加一个只包含拟使用的标签新列。一个可行的方案是：首先，将所用数据复制给一个
#副本，并将列Name复制为Name1：
cdat=subset(countries,Year==2009&healthexp>2000)
cdat$Name1=cdat$Name
#接下来，用%in%运算符找出绘图时希望保有的标签所处位置。本操作将返回一个逻辑向量
#该向量标识了cdat$Name1中哪些元素出现在第二个向量中，其中第二个向量指定的是
#我们希望标示出来的国家的名字：
idx=cdat$Name1%in%c('Canada','Ireland','United Kingdom','United States',
                    'New Zealand','Iceland','Japan','Luxembourg','Netherlands',
                    'Switzerland'
                    )
idx
cdat$Name1[!idx]=NA
ggplot(cdat,aes(x=healthexp,y=infmortality))+
  geom_point()+
  geom_text(aes(x=healthexp+100,label=Name1),size=4,hjust=0)+
  xlim(2000,10000)




#5.12 绘制气泡图
#方法：调用geom_points()和scale_size_area()函数即可绘制气泡图。下面以countries
#数据集为例：
library(gcookbook)#为了使用数据
library(ggplot2)
cdat=subset(countries,Year==2009&
              Name%in%c('Canada','Ireland','United Kingdom','United States',
                        'New Zealand','Iceland','Japan','Luxembourg','Netherlands',
                        'Switzerland'))
cdat
p=ggplot(cdat,aes(x=healthexp,y=infmortality,size=GDP))+
  geom_point(shape=21,colour='black',fill='cornsilk')
#将GDP映射给半径
p
p+scale_size_area(max_size = 15)

#本例中气泡图实际上是散点图，但气泡图也有其他方法。比如，当x轴和y轴皆为分类变量
#时，气泡图可以用来表示网格上的变量值
hec=HairEyeColor[,,'Male']+HairEyeColor[,,'Female']#对男性组和女性组求和
hec
#转化为长格式
library(reshape2)
hec=melt(hec,value.name = 'count')
hec

ggplot(hec,aes(x=Eye,y=Hair))+
  geom_point(aes(size=count),shape=21,colour='black',fill='cornsilk')+
  scale_size_area(max_size = 20,guide=FALSE)+
  geom_text(aes(y=as.numeric(Hair)-sqrt(count)/22,label=count),vjust=1,
            colour='grey60',size=4)



#5.13 绘制散点图矩阵
#方法：散点图矩阵是一种对多个变量两两之间关系进行可视化的有效方法。调用R基础绘图
#系统中的pairs()函数可以绘制散点图矩阵。
#这里以countries数据集为例。我们从countries数据集中选取2009年的数据且保留几个
#相关列：
library(gcookbook)#为了使用数据
library(ggplot2)
c2009=subset(countries,Year==2009,
             select = c(Name,GDP,laborrate,healthexp,infmortality))
c2009
#我们用第2——5列数据绘制散点图矩阵——对变量Name绘图的意义不大，而且会出现奇怪的结果
plot(c2009[,2:5])



#############################第六章 描述数据分布######################

#6.1 绘制简单直方图
#方法：运行geom_histogram()函数并映射一个连续型变量到参数X
library(ggplot2)
ggplot(faithful,aes(x=waiting))+geom_histogram()

#geom_histogram()函数只需要数据框的其中一列或者一个单独的数据向量作为参数。
#以faithful数据集为例，该数据集包含了两列描述老忠实喷泉的信息：第一列eruptio，
#描述老忠实喷泉每次喷发的时长；第二列waiting，描述两次喷发之间的间隔

faithful

#为了快速查看未包含在数据框中的直方图，可以在运行上述命令时，将数据框参数设定为
#NULL，同时，向ggplot()函数传递一个向量作为参数。

#将变量保存为一个基本的向量
w=faithful$waiting
ggplot(NULL,aes(x=w))+geom_histogram()

#还可以通过祖居(binwidth)参数来调整数据的分组数目，或者将数据切分为指定的分组
#数目。直方图默认填充颜色是黑色且没有边框线，这使得我们难以看清各个条形对应的
#变量值，因此，可以调整一下直方图的颜色设置

#设定组距为5
ggplot(faithful,aes(x=waiting))+
  geom_histogram(binwidth = 5,fill='white',colour='black')

#将x的取值切分为15组
binsize=diff(range(faithful$waiting))/15
ggplot(faithful,aes(x=waiting))+
  geom_histogram(binwidth = binsize,fill='white',colour='black')

#6.2 基于分组数据绘制分组直方图
#方法：运行geom_histogram()函数并使用分面绘图即可

library(MASS)#为了使用数据
#以smoke变量为分面变量
library(ggplot2)
ggplot(birthwt,aes(x=bwt))+geom_histogram(fill='white',colour='black')+
  facet_grid(smoke~.)

#分面的标签有一个问题，即分面的标签只有0和1，且没有指明这个标签是smoke的取值。
#想要修改标签，我们需要修改因子水平的名称。首先列出现有的因子水平，然后依照相同
#的顺序向它们赋予新的名字：

birthwt1=birthwt#复制一个副本
#将smoke转化为因子
birthwt1$smoke=as.factor(birthwt1$smoke)
levels(birthwt1$smoke)

library(plyr)#为了使用revalue()函数
birthwt1$smoke=revalue(birthwt1$smoke,c('0'='no smoke','1'='smoke'))
ggplot(birthwt1,aes(x=bwt))+geom_histogram(fill='white',colour='black')+
  facet_grid(smoke~.)

#分面绘图时，各分面对应的y轴标度是相同的。当各组数据包含的样本数目不同时，
#可能会难以比较分组数据的分布形状。我们看看按照race对出生体重进行分组并分面绘图

ggplot(birthwt,aes(x=bwt))+geom_histogram(fill='white',colour='black')+
  facet_grid(race~.)
#设置参数scales='free'可以单独设定各个分面的y轴坐标。注意：这种设置只适用于y轴
#标度，x轴的标度仍然是固定得到，因为各个分面的直方图是依照x轴对齐的。
ggplot(birthwt,aes(x=bwt))+geom_histogram(fill='white',colour='black')+
  facet_grid(race~.,scales = 'free')
#分组绘图时的另一种做法，可以考虑把分组变量映射给fill，此处分组变量必须是
#因子型或者字符型
#的向量。对于birthwt数据集，变量smoke是合适的分组变量，所以我们先将smoke的变量
#类型变为字符型
birthwt1$smoke=as.character(birthwt1$smoke)
ggplot(birthwt1,aes(x=bwt,fill=smoke))+
  geom_histogram(position = 'identity',alpha=0.4)



#6.3 绘制密度曲线
#方法：运行geom_density()函数，并映射一个连续型变量到x
library(ggplot2)
ggplot(faithful,aes(x=waiting))+geom_density()
#想快速绘制一个未在数据框中的数据的直方图，可以在ggplot()将数据框设定为NULL，
#同时向ggplot()函数中传递一个包含所需数据的向量作为参数
w=faithful$waiting
ggplot(NULL,aes(x=w))+geom_density()
#曲线的光滑程度取决于和函数的带宽：带宽越大，曲线越光滑。带宽可以通过adjust
#参数进行设置，默认值为1
ggplot(faithful,aes(x=waiting))+
  geom_line(stat = 'density',adjust=.25,colour='red')+
  geom_line(stat = 'density')+
  geom_line(stat = 'density',adjust=2,colour='blue')
#x轴的范围是可以手动设定的，同时设置alpha=.2使填充色的透明度为80%
ggplot(faithful,aes(x=waiting))+
  geom_density(fill='blue',alpha=.2)+
  xlim(35,105)
#或者使用geom_density()绘制一个蓝色的区域，并在顶端加一条实线
ggplot(faithful,aes(x=waiting))+
  geom_density(fill='blue',alpha=.2,colour=NA)+
  geom_line(stat = 'density')+
  xlim(35,105)
#将密度图叠加在直方图上，可以对观测值的理论分布和实际分布进行比较。由于密度
#曲线对应的y轴比较小，曲线会很难看清。通过设置y=..density..可以减小直方图的
#标度以使其与密度曲线的标度相匹配。
ggplot(faithful,aes(x=waiting,y=..density..))+
  geom_histogram(fill='cornsilk',colour='gray60',size=.2)+
  geom_line(stat = 'density')+
  xlim(35,105)



#6.4 基于分组数据绘制分组密度曲线
#方法：使用geom_density()，函数将分组变量映射给colour或fill等图形属性即可，分组
#变量必须是因子型或者字符串向量。数据集birthwt对应的最佳分组变量smoke被储存为
#数值型，我们先将其转化为因子型
library(MASS)#为了使用数据
library(ggplot2)
birthwt1=birthwt
birthwt1$smoke=as.factor(birthwt1$smoke)
ggplot(birthwt1,aes(x=bwt,fill=smoke))+geom_density()
ggplot(birthwt1,aes(x=bwt,fill=smoke))+geom_density(alpha=.3)
#另一种对分组数据分布进行可视化的方法是分面，可以令各个分面水平或竖直对齐。
ggplot(birthwt1,aes(x=bwt))+geom_density()+facet_grid(smoke~.)
#分面的另一个问题是，分面标签是变量smoke的取值。想要修改标签，就需要修改因子水平
#的名称。
levels(birthwt1$smoke)
library(plyr)#为了使用revalue函数
birthwt1$smoke <- revalue(birthwt1$smoke,c('0'='No smoke','1'='Smoke'))
ggplot(birthwt1,aes(x=bwt))+geom_density()+facet_grid(smoke~.)
#如果将直方图和核密度曲线绘制在一张图上，最佳的方法就是利用分面，因为两个直方图
#绘制在一张图里不好解释。操作时，需设置y=..density..，这样系统会将直方图的y轴
#标度降低到和密度曲线相同。
ggplot(birthwt1,aes(x=bwt,y=..density..))+
  geom_histogram(binwidth = 200,fill='lightblue',colour='gray60',size=.2)+
  geom_density()+facet_grid(smoke~.)



#6.5 绘制频数多边形
#方法：使用geom_freqpoly()函数即可
ggplot(faithful,aes(x=waiting))+geom_freqpoly()
#频数多边形看起来和核密度曲线类似，但核密度曲线只是一种估计
#与直方图类似，可以通过binwidth参数控制频数多边形的组距
ggplot(faithful,aes(x=waiting))+geom_freqpoly(binwidth=4)
#也可以通过直接设定组距将x轴范围切分为特定数目的组
#将组数设为15
binsize <- diff(range(faithful$waiting))/15
ggplot(faithful,aes(x=waiting))+geom_freqpoly(binwidth=binsize)



#6.6 绘制基本箱线图
#方法：使用geom_boxplot()函数，分别映射一个连续型变量和一个离散型变量到y和x
library(MASS)#为了使用数据
library(ggplot2)
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot()
#设置参数width可以修改箱线图的宽度
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot(width=.5)
#如果图中异常值较多且图形有重叠的话，可以通过设置outlier.size和outlier.shape参数
#修改异常点的大小和点型。异常点默认大小为2点形16
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot(outlier.size = 1.5,outlier.shape = 21)
#在绘制单组数据的箱线图时，必须给x参数映射一个特定的取值，否则，ggplot()函数不知道
#箱线图对应得到x轴坐标。
ggplot(birthwt,aes(x=1,y=bwt))+geom_boxplot(width=.5)+
  scale_x_continuous(breaks = NULL)+
  theme((axis.title.x=element_blank()))




#6.7 向箱线图添加槽口
#如何向箱线图添加槽口以比较各组数据的中位数是否有差异
#方法：使用geom_boxplot()函数设定参数notch=TRUE 
library(ggplot2)
library(MASS)#为了使用数据
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot(notch = TRUE)




#6.8 向箱线图添加均值
#方法：使用stat_summary()函数。箱线图中常以钻石状来表示点型23且填充色为白色的点
library(ggplot2)
library(MASS)#为了使用数据
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot()+
  stat_summary(fun = 'mean',geom = 'point',shape=23,fill='white',size=3)




#6.9 绘制小提琴图
#方法geom_violin()函数即可
library(gcookbook)#为了使用数据
p <- ggplot(heightweight,aes(x=sex,y=heightIn))
p+geom_violin()
