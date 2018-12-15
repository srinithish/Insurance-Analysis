library(dtplyr)
library(data.table)
library(dplyr)
library(corrplot)
library(psych)
library(ggplot2)
library(MASS)
library(forcats)

insurance_Original = fread(choose.files())
str(insurance_Original)
cormatrix = cor(insurance_Original[,c(1,3,4,7)])
corplot = corrplot(cormatrix)
pairs.panels(insurance_Original)

#draw histograms for each to find the distributions 
# Classes ‘data.table’ and 'data.frame':	1338 obs. of  7 variables:
#   $ age     : int  19 18 28 33 32 31 46 37 37 60 ...
# $ sex     : chr  "female" "male" "male" "male" ...
# $ bmi     : num  27.9 33.8 33 22.7 28.9 ...
# $ children: int  0 1 3 0 0 0 1 3 2 0 ...
# $ smoker  : chr  "yes" "no" "no" "no" ...
# $ region  : chr  "southwest" "southeast" "southeast" "northwest" ...
# $ charges : num  16885 1726 4449 21984 3867 ...
# - attr(*, ".internal.selfref")=<externalptr> 
#

str(insuranceMutated)
# age            sex                 bmi           children        smoker         
# Min.   :18.00   Length:1338        Min.   :15.96   Min.   :0.000   Length:1338       
# 1st Qu.:27.00   Class :character   1st Qu.:26.30   1st Qu.:0.000   Class :character  
# Median :39.00   Mode  :character   Median :30.40   Median :1.000   Mode  :character  
# Mean   :39.21                      Mean   :30.66   Mean   :1.095                     
# 3rd Qu.:51.00                      3rd Qu.:34.69   3rd Qu.:2.000                     
# Max.   :64.00                      Max.   :53.13   Max.   :5.000                     
# region             charges      ChildrenBin          logCharges    
# Length:1338        Min.   : 1122   Length:1338        Min.   : 7.023  
# Class :character   1st Qu.: 4740   Class :character   1st Qu.: 8.464  
# Mode  :character   Median : 9382   Mode  :character   Median : 9.147  
# Mean   :13270                      Mean   : 9.099  
# 3rd Qu.:16640                      3rd Qu.: 9.720  
# Max.   :63770                      Max.   :11.063  


summary(insuranceMutated)
table(insurance_Original$children)
plotCanvas = ggplot(data = insuranceMutated)

##creating factor for bmi

plotCanvas+geom_histogram(mapping = aes(x=bmi),binwidth = 2)

insuranceMutated$bmiClass = cut(insuranceMutated$bmi,breaks = c(0,16,17,18.5,24.9,29.9,34.9,39.9,60),
    labels = c("SevereM","ModerateM","MildM","Normal","OverWeight","Obese1","Obese2","Obese3"))

insuranceMutated = insuranceMutated %>% mutate(bmiCollapsedClass = fct_collapse(bmiClass,
                                                            underWeight = c("SevereM","ModerateM","MildM"),
                                                            normal = c("Normal"),
                                                            OverWeight = c("OverWeight","Obese1","Obese2","Obese3")
                                                            )) %>% mutate(children = as.factor(children))

## creting factor for childeer
insuranceMutated = insurance_Original
childrenDIst  = ggplot(data = insuranceMutated) +
                geom_histogram(mapping = aes(x=insuranceMutated$children))+ 
                labs(x = "Number of children") 

insuranceMutated$ChildrenBin = ifelse(insuranceMutated$children == 0,"NoChildren",
                                      ifelse((insurance_Original$children > 0 & insurance_Original$children <=2),"1to2Children",
                                             "3to5 Children")
                                      
                                      )

table(insuranceMutated$ChildrenBin)
table(insuranceMutated[,c("sex","smoker")])


###age distribuition plot
agePlot  =  ggplot(data=insuranceMutated)+geom_histogram(binwidth = 10,mapping = aes(x=age),color = "blue",fill="orange") 
table(insuranceMutated$age)
?geom_bar
braeakpoints = seq(15,65,10)
table(cut(insuranceMutated$age,breaks = braeakpoints))
?geom_histogram
agePlot$data

###check charges against age distribution within males and females

p4  = ggplot(data = insuranceMutated)+geom_jitter(mapping = aes(x=age,y=charges,color = smoker,shape=bmiCollapsedClass),height = 4,width = 4,alpha = 1/3)+
  facet_wrap(~sex)
ggsave("ChargesVsAge_Smoker_bmi.jpg",p4)

#charges distribution within regions and bifurcated by sex 
p5 = ggplot(data = insuranceMutated) + geom_area(mapping = aes(x=charges),stat = "bin")+facet_wrap(~sex+region)

ggsave(filename = "f_m_s_ns_cahrges_age.jpg",plot = p4,dpi = 1000)

####bmki vs charges

p6 = ggplot(data = insuranceMutated)+ geom_jitter(mapping = aes(x=bmi,y=charges,color=smoker),width = 3,alpha=1/2)

###check about children vs charges distribution
p7 = plotCanvas + geom_boxplot(mapping = aes(x=as.factor(ChildrenBin),y=charges))

# chrges distribution 

plotCanvas+geom_histogram(mapping = aes(x=log(charges)),binwidth = 0.1)



insuranceMutated$logCharges = log(insuranceMutated$charges)

linearModel = lm(logCharges~region+sex+age+smoker+bmiCollapsedClass+children,data = insuranceMutated)

summary(linearModel)
stepAIC(linearModel,direction = "both")
residualsLM =  as.data.frame(linearModel$residuals)
colnames(residualsLM) = c("res")
ggplot()+geom_histogram(mapping = aes(x=res),data = residualsLM)
colnames(insuranceMutated)
