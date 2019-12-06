#import data set 

library(psych)
library(reshape2)
library(MVA)
wide_data<-dcast(maria, id~var,value.var = "value") 
View(wide_data)
mydata<-wide_data
View(mydata)
#checking for missing values and checking skewness and kurtosis
require(lsr)
summary(mydata)
describe(mydata)
str(mydata)
#we can see that there are many missing values in our data set and we have to deal with them
#we want to replace the missing values with the mean
#creating a copy of data set
mydata1=mydata
#I can also see it in summary
mean(mydata1$Q5_alien,na.rm = TRUE) #calculating the mean for Q5 variable without the missing value
mean(mydata1$Q6_inferior,na.rm = TRUE) 
mean(mydata1$Q8_Support,na.rm = TRUE)
mean(mydata1$Q9_Nerd,na.rm = TRUE)

#replacing NA with the mean for each variable
mydata1$Q5_alien[is.na(mydata1$Q5_alien)]=2.97
mydata1$Q6_inferior[is.na(mydata1$Q6_inferior)]=2.98
mydata1$Q8_Support[is.na(mydata1$Q8_Support)]=2.02
mydata1$Q9_Nerd[is.na(mydata1$Q9_Nerd)]=3.06
summary(mydata1)


#before running the pca analysis I want to exclude age and sex from the pca
data_final=mydata1[,c("Q1_cry", "Q2_help", "Q3_breathe","Q4_freeze", "Q5_alien",
                           "Q6_inferior", "Q7_weep", "Q8_Support", "Q9_Nerd")]



#reliability test
alpha(data_final)

str(data_final)

cor(data_final)

data_pca<- princomp(data_final,cor=TRUE)
data_pca
print(summary(data_pca), loadings = TRUE)
windows() 
plot(data_pca$sdev^2, xlab = "Component number",
     ylab = "Component variance", type = "l" )

plot(log(data_pca$sdev^2), xlab = "Component number",
     ylab = "log(Component variance)", type="l")



#barplot with percentage
windows()
pca_var=data_pca$sdev^2
pca_var_per= round(pca_var/sum(pca_var)*100,1)
barplot(pca_var_per,
        xlab= "Principal Component", 
        ylab = "Percentage variation")
windows()
xlim=range(data_pca$scores[,1])
plot(data_pca$scores, xlim=xlim, ylim = xlim,main = "Plot of scores", xlab = "Component 1"
     , ylab = "Component 2")

#plots
windows()
biplot(data_pca,col=c("grey", "black"), xlab= "Component 1(50.85%)",
       ylab= "Component 2 (13.10%)")

###to check for differences regarding age and sex##is not completely finished
#first extract the PC scores for the three dominant components extracting from PCA
PC1=data_pca$scores[,1]
PC2=data_pca$scores[,2]
PC3=data_pca$scores[,3]
#tranform the numerical variables of sex and age to factors for ANOVA analysis
mydata2$sex=factor(c(0,1))


levels(mydata1$sex)
#one way ANOVA
aov( formula = PC1 ~ sex, data = mydata1 )
my.anova <- aov( PC1 ~ sex, mydata1 )
summary(my.anova)
aov( formula = sex ~ PC2, data = mydata1 )
my.anova2 <- aov( sex ~ PC2, mydata1 )
summary(my.anova2)
aov( formula = age ~ PC1, data = mydata1)
my.anova3 <- aov( age ~ PC1, mydata1 )
summary(my.anova3)
etaSquared( x = my.anova )

