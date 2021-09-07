library(dplyr)
library(ggplot2)
library(mice)
library(purrr)
library(corrplot)
library(psych)
library(caTools)
library(readxl)
library(ModelMetrics)
library(openxlsx)
library(writexl)
set.seed(123)

data_4=read.csv("https://raw.githubusercontent.com/sruthireddy1482/MA317_GRP17/main/LifeExpectancyData1.csv")
View(data_4)

map(data_4, ~sum(is.na(.)))

#imputing all the missing values using mice
impute= mice(data_4[,5:23],method = "cart")
print(impute)
#impute$imp$NY.ADJ.NNTY.KD.ZG
new_data_4=complete(impute,3)
new_data_4

#replacing missing columns with imputed data
data_4[,5:23]=new_data_4[]


#replacing 1 missing value of life expectency using mean of the column 
data_4$SP.DYN.LE00.IN[is.na(data_4$SP.DYN.LE00.IN)]<-mean(data_4$SP.DYN.LE00.IN,na.rm=TRUE)

sum(is.na(data_4))


M<-cor(data_4[,4:23])
corrplot(M,method = "color",number.cex=0.5,type="upper")

#checking correlation b/w mortality rate male and female
corr.test(data_4$SP.DYN.AMRT.FE,data_4$SP.DYN.AMRT.MA)
#the value is 0.93 - highly correlated so removing male variable from the data
#checking correlation b/w mortality rate female and infant
corr.test(data_4$SP.DYN.AMRT.FE,data_4$SP.DYN.IMRT.IN)
#the value is 0.87 - highly correlated so removing infant variable from the data
#checking correlation b/w gdp per capita and gnp per capita
corr.test(data_4$NY.GDP.PCAP.PP.CD,data_4$NY.GNP.PCAP.PP.CD)
#the value is 0.99 - highly correlated so removing gnp per capita from the data
#checking correlation b/w access to electricity and birth rate crude
corr.test(data_4$EG.ELC.ACCS.ZS,data_4$SP.DYN.CBRT.IN)
#the value is -0.83 - highly correlated so removing birth rate crude from the data
final_data_4=data_4[,-c(18,9,22,21)]
View(final_data_4)

#building multiple linear regressor model

model=lm(SP.DYN.LE00.IN~EG.ELC.ACCS.ZS+SE.PRM.UNER.ZS+SE.XPD.PRIM.ZS+SE.ADT.LITR.ZS+
           SP.POP.GROW+SE.PRM.CMPT.ZS+SH.XPD.CHEX.GD.ZS+SH.XPD.CHEX.PC.CD+
           SP.DYN.AMRT.FE+NY.GDP.PCAP.PP.CD+NY.ADJ.NNTY.KD+SL.EMP.TOTL.SP.ZS+
           SP.POP.TOTL+SL.UEM.TOTL.NE.ZS+NY.ADJ.NNTY.KD.ZG+NY.GDP.MKTP.KD.ZG,
         data=final_data_4)
summary(model)
anova(model)

#removing least significant variable one each time and check the adjusted r square value
model1=lm(SP.DYN.LE00.IN~EG.ELC.ACCS.ZS+SE.PRM.UNER.ZS+SE.XPD.PRIM.ZS+SE.ADT.LITR.ZS+
           SE.PRM.CMPT.ZS+SH.XPD.CHEX.GD.ZS+SH.XPD.CHEX.PC.CD+
           SP.DYN.AMRT.FE+SL.EMP.TOTL.SP.ZS+
           NY.ADJ.NNTY.KD.ZG,
         data=final_data_4)
summary(model1)
anova(model1)

#summary(model1)$coefficient
#confint(model1)

# Hence we have removed all the less signifiant variables from the model 

#4b
split_4= sample.split(final_data_4$SP.DYN.LE00.IN, SplitRatio = 0.8)
training_set_4=subset(final_data_4,split_4==TRUE)
test_set_4=subset(final_data_4,split_4==FALSE)

#fitting multiple linear regressor to the training set
regressor_4=lm(SP.DYN.LE00.IN~EG.ELC.ACCS.ZS+SE.PRM.UNER.ZS+SE.XPD.PRIM.ZS+SE.ADT.LITR.ZS+
               SE.PRM.CMPT.ZS+SH.XPD.CHEX.GD.ZS+SH.XPD.CHEX.PC.CD+
               SP.DYN.AMRT.FE+SL.EMP.TOTL.SP.ZS+
               NY.ADJ.NNTY.KD.ZG,
             data=training_set_4)
summary(regressor_4)

#predicting on test set

y_pred_4= predict(regressor_4,newdata = test_set_4)
y_pred_4

#evaluating the model
rmse(test_set_4$SP.DYN.LE00.IN,y_pred_4)

#4c
set.seed(123)
unseen_data_4=read.xlsx("https://github.com/sruthireddy1482/MA317_GRP17/raw/main/LifeExpectancyData2.xlsx")
unseen_data_4[, 4:22] <- sapply(unseen_data_4[,4:22], as.numeric)

#impputing missing values using mice
map(unseen_data_4, ~sum(is.na(.)))
impute1_4= mice(unseen_data_4[,4:22],method = "cart")
print(impute1_4)

new_data1_4=complete(impute1_4,3)
new_data1_4
unseen_data_4[,4:22]=new_data1_4[]

unseen_data_4$SE.XPD.PRIM.ZS[is.na(unseen_data_4$SE.XPD.PRIM.ZS)]<-mean(unseen_data_4$SE.XPD.PRIM.ZS,na.rm=TRUE)
unseen_data_4$NY.GNP.PCAP.PP.CD[is.na(unseen_data_4$NY.GNP.PCAP.PP.CD)]<-mean(unseen_data_4$NY.GNP.PCAP.PP.CD,na.rm=TRUE)
unseen_data_4$SE.ADT.LITR.ZS[is.na(unseen_data_4$SE.ADT.LITR.ZS)]<-mean(unseen_data_4$SE.ADT.LITR.ZS,na.rm=TRUE)
unseen_data_4$SE.PRM.CMPT.ZS[is.na(unseen_data_4$SE.PRM.CMPT.ZS)]<-mean(unseen_data_4$SE.PRM.CMPT.ZS,na.rm=TRUE)

map(unseen_data_4, ~sum(is.na(.)))
sum(is.na(unseen_data_4))

View(unseen_data_4)
result_4=unseen_data_4%>%
  select(c("Country.Name","Country.Code","EG.ELC.ACCS.ZS","NY.ADJ.NNTY.KD.ZG",
           "NY.ADJ.NNTY.KD","SE.PRM.UNER.ZS","SE.XPD.PRIM.ZS","SE.ADT.LITR.ZS",
           "SP.POP.GROW","SP.POP.TOTL","SE.PRM.CMPT.ZS","SH.XPD.CHEX.GD.ZS",
           "SH.XPD.CHEX.PC.CD","SL.UEM.TOTL.NE.ZS","SP.DYN.AMRT.FE",
           "NY.GDP.MKTP.KD.ZG","NY.GDP.PCAP.PP.CD","SL.EMP.TOTL.SP.ZS"
           ))

#removing all unsignificant variables in unseen data

life_expectancy_4= predict(regressor_4,newdata = result_4)
life_expectancy_4
final_excel_4=cbind(result_4,life_expectancy_4)
#write_xlsx(final_excel_4,"D:\\MA317\\Datapredict_4.xlsx")
