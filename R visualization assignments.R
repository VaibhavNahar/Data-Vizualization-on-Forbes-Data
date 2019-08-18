setwd("C:\\Users\\Windows 10\\Desktop\\jigsaw academy vaibhav")
datasets::Orange
data(Orange)
library(ggplot2)
P = ggplot(Orange,aes(x=age,y=circumference))
P+geom_point(aes(color=Tree))
str( Orange)
library(sp)
data(meuse)


# ------non graded casestudy------

F = read.csv("dataF.csv",sep=',',header=TRUE)
str(F)
summary(F)
round(2.108,digits = 2)
library(dplyr)
tech = F%>%filter(Industry=="Technology",)
remove(p)
p = ggplot(tech,aes(Company.Advertising,Brand.Revenue))
p+geom_point(aes(color=Brand,size=Brand.Value)) + xlab("Company Advertising in Billions") + ylab("Brand Value is billions") + scale_size(range = c(2,4), breaks = c(30,60,100),name="Brand Value") +ggtitle("Technology") +geom_text(aes(label=Brand),vjust=1,hjust=1)+guides(color=FALSE)+theme_light()+theme(legend.key=element_rect(fill = "light blue", color = "black"))

lux = F%>%filter(Industry=="Luxury",)
q = ggplot(lux,aes(Company.Advertising,Brand.Revenue))
q+geom_point(aes(color=Brand,size=Brand.Value))+xlab("Company Adverstising In Billions")+ylab("Brand Revenue in Billions")+ggtitle("Luxury")+scale_size(range = c(1,3),breaks = c(10,28.1),name="Brand Value")+geom_text(aes(label=Brand),vjust=1,hjust=0.5)+theme_light()+theme(legend.key = element_rect(fill = "green",color = "brown"))+guides(color=FALSE)+scale_x_continuous(breaks=seq(0,6,0.1))

Fin = F%>%filter(Industry=="Financial Services",)
f = ggplot(Fin,aes(Company.Advertising,Brand.Revenue))
f+geom_point(aes(color=Brand,size=Brand.Value))+xlab("Company Advertising")+ylab("brand Value")+ggtitle("Financial Services")+scale_size(range = c(1,3),breaks = c(8,12,24),name="Brand value")+geom_text(aes(label=Brand),hjust=0.8,vjust=0.8)+theme_light()+theme(legend.key = element_rect(fill = "red",colour = "yellow"))+guides(color=FALSE)+scale_x_continuous(breaks = seq(0,4,0.5))+scale_y_continuous(breaks = seq(0,100,10))
  

     



#-------------Graded assignbment---------#
getwd()

library(RCurl)
library(XML)

forbes<-readHTMLTable("The World's Most Valuable Brands List - Forbes.html")

class(forbes)
str(forbes)
forbes

forbestable<-forbes$the_list

#Convert forbestable to DataFrame
forbestable<-as.data.frame(forbestable)

class(forbestable)
str(forbestable)

write.csv(forbestable,"forbes.csv",row.names = F)
forbestable<-read.csv("forbes.csv",header=T,as.is=T)

#Converting Millions to Billions:
index<-which(grepl("M",forbestable$Company.Advertising))
forbestable$Company.Advertising[index]<-forbestable$Company.Advertising[index]/1000

forbestable
forbestable$Rank<-gsub("#","",forbestable$Rank)
forbestable$`Brand Value`<-gsub("[$]","",forbestable$`Brand Value`)
forbestable$`Brand Value`<-gsub("B","",forbestable$`Brand Value`)
forbestable$`1-Yr Value Change`<-gsub("%","",forbestable$`1-Yr Value Change`)
forbestable$`Brand Revenue`<-gsub("[$]","",forbestable$`Brand Revenue`)
forbestable$`Brand Revenue`<-gsub("B","",forbestable$`Brand Revenue`)
forbestable$`Company Advertising`<-gsub("[$]","",forbestable$`Company Advertising`)
str(forbestable)
forbestable$`Brand Value`<-as.numeric(forbestable$`Brand Value`)
forbestable$`1-Yr Value Change`<-as.numeric(forbestable$`1-Yr Value Change`)
forbestable$`Brand Revenue`<-as.numeric(forbestable$`Brand Revenue`)
write.csv(forbestable,"forbes.csv",row.names = F)
forbestable<-read.csv("forbes.csv",header=T,as.is=T)
forbestable$Industry<-as.factor(forbestable$Industry)

library(dplyr)
tech<-filter(forbestable,Industry=="Technology")
lux<-filter(forbestable,Industry=="Luxury")
auto<-filter(forbestable,Industry=="Automotive")
fs<-filter(forbestable,Industry=="Financial Services")

library(ggplot2)

#------ Technology Scatter Plot --------#

Tech<-ggplot(tech,aes(x=Company.Advertising,y=Brand.Revenue,label=Brand))

Tech + geom_point(aes(size=Brand.Value,color=Brand)) + geom_text() + xlab("Company Advertising in Billions of $") + 
  ylab("Brand Revenue in Billions of $") + ggtitle("Technology")

Tech + geom_point(aes(size=Brand.Value,color=Brand)) + geom_text() + xlab("Company Advertising in Billions of $") + 
  ylab("Brand Revenue in Billions of $") + ggtitle("Technology") + 
  scale_size_continuous(name="Brand Value $(Billions)",breaks=c(30,60,100))

Tech + geom_point(aes(size=Brand.Value,color=Brand)) + geom_text() + xlab("Company Advertising in Billions of $") + 
  ylab("Brand Revenue in Billions of $") + ggtitle("Technology") + 
  scale_size_continuous(name="Brand Value $(Billions)",breaks=c(30,60,100))+guides(color=F)

Tech + geom_point(aes(size=Brand.Value,color=Brand)) + geom_text() + xlab("Company Advertising in Billions of $") + 
  ylab("Brand Revenue in Billions of $") + ggtitle("Technology") + 
  scale_size_continuous(name="Brand Value $(Billions)",breaks=c(30,60,100)) + guides(color=F) + 
  theme(panel.grid.major=element_line("grey")) + theme_bw() + 
  theme(panel.border=element_rect(color="grey",fill=NA)) + theme(plot.title=element_text(size=25)) + 
  theme(plot.title=element_text(face="bold"))

Tech + geom_point(aes(size=Brand.Value,color=Brand)) + geom_text() + xlab("Company Advertising in Billions of $") + 
  ylab("Brand Revenue in Billions of $") + ggtitle("Technology") + 
  scale_size_continuous(name="Brand Value $(Billions)",breaks=c(30,60,100)) + guides(color=F) + 
  theme(panel.grid.major=element_line("grey")) + theme_bw() + 
  theme(panel.border=element_rect(color="grey",fill=NA)) + theme(plot.title=element_text(size=25)) + 
  theme(plot.title=element_text(face="bold")) + theme(panel.grid.minor=element_line("grey"))


#------ Luxury Scatter Plot -------#

ggplot(lux,aes(x=Company.Advertising,y=Brand.Revenue,label=Brand))+geom_point(aes(size=Brand.Value,color=Brand))

ggplot(lux,aes(x=Company.Advertising,y=Brand.Revenue,label=Brand))+geom_point(aes(size=Brand.Value,color=Brand))+
  geom_text(aes(color=factor(Brand),size=Brand.Value))+labs(title="Luxury",x="Company Advertising in Billions of $",y="Brand Revenue in Billions of $")+
  scale_size_continuous(name="Brand Value $ (Billions)",breaks=c(10.0,28.1))+guides(color=F)

ggplot(lux,aes(x=Company.Advertising,y=Brand.Revenue,label=Brand))+geom_point(aes(size=Brand.Value*50,color=Brand))+
  geom_text(aes(color=factor(Brand),size=Brand.Value*50))+labs(title="Luxury",x="Company Advertising in Billions of $",y="Brand Revenue in Billions of $")+
  scale_size_continuous(name="Brand Value $ (Billions)",breaks=c(10.0,28.1))+guides(color=F)+scale_x_continuous(breaks=seq(0,5,0.10))+
  theme(panel.grid.major=element_line("grey"))+theme_bw()+theme(panel.border=element_rect(color="grey",fill=NA))+theme(plot.title=element_text(size=25))+
  theme(plot.title=element_text(face="bold"))+theme(panel.grid.minor=element_line("grey"))


# ------- Financial Scatter Plot -------- #

ggplot(fs,aes(x=Company.Advertising,y=Brand.Revenue,label=Brand))+geom_point(aes(size=Brand.Value,color=Brand))+
  geom_text(aes(color=factor(Brand),size=Brand.Value))+labs(title="Financial",x="Company Advertising in Billions of $",y="Brand Revenue in Billions of $")+
  scale_size_continuous(name="Brand Value $ (Billions)",breaks=c(7.0,12.0,23.4))+guides(color=F)+scale_x_continuous(breaks=seq(0.6,3.5,0.1))+
  scale_y_discrete(breaks=seq(0,100,10))+theme(panel.grid.major=element_line("grey"))+theme_bw()+theme(panel.border=element_rect(color="grey",fill=NA))+
  theme(plot.title=element_text(size=25))+theme(plot.title=element_text(face="bold"))+theme(panel.grid.minor=element_line("grey"))


# -------- Automotive Scatter Plot --------- #
ggplot(auto,aes(x=Company.Advertising,y=Brand.Revenue,label=Brand))+geom_point(aes(size=Brand.Value,color=Brand))+
  geom_text(aes(color=factor(Brand),size=Brand.Value))+labs(title="Automotive",x="Company Advertising in Billions of $",y="Brand Revenue in Billions of $")+
  scale_size_continuous(name="Brand Value $ (Billions)",breaks=c(6.2,20,37.8))+guides(color=F)+scale_x_continuous(breaks=seq(0.8,5.5,0.1))+
  scale_y_discrete(breaks=seq(40,170,10))+theme(panel.grid.major=element_line("grey"))+theme_bw()+theme(panel.border=element_rect(color="grey",fill=NA))+
  theme(plot.title=element_text(size=25))+theme(plot.title=element_text(face="bold"))+theme(panel.grid.minor=element_line("grey"))




F