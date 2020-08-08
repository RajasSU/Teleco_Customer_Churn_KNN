#########################################################################################
############################### Telco Churn Analysis#####################################
#########################################################################################

rm(list=ls()); gc()


usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage('ggplot2')
# For Accuracy Calculation
usePackage('caret')
usePackage('cowplot')
usePackage('e1071')
usePackage('rpart.plot')
usePackage('rattle')
usePackage('randomForest')
usePackage('caTools')
usePackage('descr')
usePackage('rpart')
usePackage('RColorBrewer')
usePackage('knitr')
usePackage('tidyr')
usePackage('plyr')
usePackage('dplyr')
usePackage('ROCR')
usePackage('corrplot')
usePackage('gridExtra')
usePackage('ggthemes')
usePackage('stringr')
usePackage('ggcorrplot')

#Data Preprocessing

#Data Extraction
churn <- read.csv(file="E:/CSUF ISDS/574 Project/TelcoChurn.csv", header=TRUE)

str(churn) #structure of our data frame

dim(churn) # To check dimension of the data-set

#summary before cleaning

summary(churn) #statistical information about the data-set

#Removing the missing values
sapply(churn, function(x) sum(is.na(x)))
#############################################################################
#Visualizing NAs in the columns:

options(repr.plot.width = 6, repr.plot.height = 4) #
missing_data <- churn %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+  xlab('variables')+
  coord_flip()+ 
  theme_bw()

churn <- churn[complete.cases(churn), ]

#Changing "No internet service" to "No" for six columns:
cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

#Changing "No phone service" to "No" for column "MultipleLines"
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

min(churn$tenure) #1
max(churn$tenure) #72

#grouping tenure into five tenure groups:
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}



churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)

#Change the values in column "SeniorCitizen" from 0 or 1 to "No" or "Yes".
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))


#Removing the columns we do not need for the analysis
churn$customerID <- NULL
churn$tenure <- NULL

#Exploratory data analysis and feature selection:

theme1 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),legend.position="none")
theme2 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position="none")

options(repr.plot.width = 6, repr.plot.height = 4)
churn %>% 
  group_by(Churn) %>% 
  dplyr::summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("#FC4E07", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percent")+
  ggtitle("Churn Percent")  

ggplot(churn) + geom_bar(aes(x = gender, fill = Churn), position = "dodge")

options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(churn, aes(x=gender,fill=Churn))+ geom_bar()+ theme1, 
          ggplot(churn, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(churn, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(churn, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(churn, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(churn, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")


options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(churn, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+ theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(churn, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

plot_grid(ggplot(churn, aes(x=StreamingMovies,fill=Churn))+ 
            geom_bar(position = 'fill')+ theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(churn, aes(x=Contract,fill=Churn))+ 
            geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn, aes(x=PaperlessBilling,fill=Churn))+ 
            geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(churn, aes(x=PaymentMethod,fill=Churn))+
            geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")


#Correlation between numeric variables

ggplot(churn, aes(y= MonthlyCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

ggplot(churn, aes(y= TotalCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")


options(repr.plot.width =6, repr.plot.height = 4)
telco_cor <- round(cor(churn[,c("MonthlyCharges", "TotalCharges")]), 1)

ggcorrplot(telco_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))


numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

#The Monthly Charges and Total Charges are correlated. 
#So Total Charges removed from the model
churn$TotalCharges <- NULL

###########################Bar plots of demographic data############################### 
#Gender plot
p1 <- ggplot(churn, aes(x = gender)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Senior citizen plot
p2 <- ggplot(churn, aes(x = SeniorCitizen)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Partner plot
p3 <- ggplot(churn, aes(x = Partner)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Dependents plot
p4 <- ggplot(churn, aes(x = Dependents)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Plot demographic data within a grid
grid.arrange(p1, p2, p3, p4, ncol=2)

###########################Bar plots of various services############################### 


#Phone service plot
p5 <- ggplot(churn, aes(x = PhoneService)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Multiple phone lines plot
p6 <- ggplot(churn, aes(x = MultipleLines)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Internet service plot
p7 <- ggplot(churn, aes(x = InternetService)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Online security service plot
p8 <- ggplot(churn, aes(x = OnlineSecurity)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Online backup service plot
p9 <- ggplot(churn, aes(x = OnlineBackup)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Device Protection service plot
p10 <- ggplot(churn, aes(x = DeviceProtection)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Tech Support service plot
p11 <- ggplot(churn, aes(x = TechSupport)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Streaming TV service plot
p12 <- ggplot(churn, aes(x = StreamingTV)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Streaming Movies service plot
p13 <- ggplot(churn, aes(x = StreamingMovies)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Plot service data within a grid
grid.arrange(p5, p6, p7,
             p8, p9, p10,
             p11, p12, p13,
             ncol=3)


#######################remaining categorical variables bar plot###########################################
#Contract status plot
p14 <- ggplot(churn, aes(x = Contract)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Paperless billing plot
p15 <- ggplot(churn, aes(x = PaperlessBilling)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Payment method plot
p16 <- ggplot(churn, aes(x = PaymentMethod)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Plot contract data within a grid
grid.arrange(p14, p15, p16, ncol=1)


########################distributions of the quantitative variables###########################################


#Monthly charges histogram
p18 <- ggplot(churn, aes(x = MonthlyCharges)) +
  geom_histogram(binwidth = 5) +
  labs(x = "Dollars (binwidth = 5)",
       title = "Monthly charges Distribtion")

#Plot quantitative data within a grid
grid.arrange(p18, ncol=1)

###############################main outcome variable of interest#####################################

p20 <- ggplot(churn, aes(x = Churn)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
p20

#------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------
#################### KNN Algorithm Implementation #################################################

str(churn)
churn$churn_number <- 0
churn$churn_number[churn$Churn == 'Yes'] <- 1

churn$Churn <- NULL

#covert categorical variables to factor
churn$gender <- as.factor(churn$gender)
churn$SeniorCitizen <- as.factor(churn$SeniorCitizen)
churn$Partner <- as.factor(churn$Partner)
churn$Dependents <- as.factor(churn$Dependents)
churn$PhoneService <- as.factor(churn$PhoneService)
churn$MultipleLines <- as.factor(churn$MultipleLines)
churn$InternetService <- as.factor(churn$InternetService)
churn$OnlineSecurity <- as.factor(churn$OnlineSecurity)
churn$OnlineBackup <- as.factor(churn$OnlineBackup)
churn$DeviceProtection <- as.factor(churn$DeviceProtection)
churn$TechSupport <- as.factor(churn$TechSupport)
churn$StreamingTV <- as.factor(churn$StreamingTV)
churn$StreamingMovies <- as.factor(churn$StreamingMovies)
churn$Contract <- as.factor(churn$Contract)
churn$PaperlessBilling <- as.factor(churn$PaperlessBilling)
churn$PaymentMethod <- as.factor(churn$PaymentMethod)


#Creating Dummies
trainDummy <- dummyVars("~.", data = churn, fullRank = F)


train <- as.data.frame(predict(trainDummy,churn))
colnames(train)

#Coverting target variable to a factor
train$churn_number <- as.factor(ifelse(train$churn_number == 1,'yes','no'))


#split the data into training set and testing set
set.seed(123)
split <- sample(2,nrow(train), replace = T,prob = c(0.70,0.30))
trainDF<- train[split ==1,]
testDF <- train[split ==2,]

dim(trainDF)
dim(testDF)

colnames(trainDF)
colnames(testDF)

#check whether any NA value exists or not
anyNA(churn)
anyNA(trainDF)
anyNA(testDF)
#Defining the training controls for multiple models
fitControl <- trainControl(
method = "repeatedcv",
number = 10,
savePredictions = 'final',
classProbs = T)

#fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,savePredictions = 'final',classProbs = T)

#######Choose the significant variables in training and testing set#################################
training_set <- trainDF[,c(1,2,3,4,5,6,7,8,11,13,15,17,18,19,23,24,28,29,30,31,33,34,36,37,38,39,40,41,42,43)]
testing_set <- testDF[,c(1,2,3,4,5,6,7,8,11,13,15,17,18,19,23,24,28,29,30,31,33,34,36,37,38,39,40,41,42,43)]


# Considering all variables
#training_set <- trainDF
#testing_set <- testDF

colnames(training_set)
colnames(testing_set)


#2--Training the knn model

#model_knn<-train(training_set,training_set[,outcomeName],
#          method='knn',trControl=fitControl,preProcess = c("center", "scale"),tuneLength=3)

model_knn <-train(churn_number~. , data = training_set,method = "knn",
                  trControl = fitControl,
                  preProcess = c("center","scale"),
                  tuneLength = 20)

?train

#plot accuracy vs K Value graph
plot(model_knn)

#Best K
model_knn


#Predicting using knn model

pred_knn <-predict( model_knn, testing_set,"prob" )[,2]

#- Area Under Curve
plot(performance(prediction(pred_knn, testing_set$churn_number),
                 "tpr", "fpr"))

testing_set[,'churn_number'] <- as.factor(testing_set[,'churn_number'])

testing_set[,'churn_number']<-ifelse(testing_set$churn_number == "yes", 1,0)

################### use probability cut off : 0.3 ###########################################
pred_knn = ifelse(pred_knn > 0.3, 1,0)

#Checking the accuracy of the KNN model

#Accuracy
mean(pred_knn == testing_set$churn_number)

misClasificError <- mean(pred_knn != testing_set$churn_number)
print(paste('KNN Accuracy',1-misClasificError))
#"KNN Accuracy 0.760056791292002"

#table  

table(testing_set$churn_number, pred_knn)

#- confusion matrix
confusionMatrix(factor(pred_knn),factor(testing_set$churn_number))
  
################### use probability cut off : 0.4 ###########################################
pred_knn = ifelse(pred_knn > 0.4, 1,0)

#Checking the accuracy of the KNN model

mean(pred_knn == testing_set$churn_number)

misClasificError <- mean(pred_knn != testing_set$churn_number)
print(paste('KNN Accuracy',1-misClasificError))

#table  
?table
table(testing_set$churn_number, pred_knn  > 0.4)

#- confusion matrix
confusionMatrix(factor(pred_knn),factor(testing_set$churn_number))


################### use probability cut off : 0.5 ###########################################
pred_knn = ifelse(pred_knn > 0.5, 1,0)

#Checking the accuracy of the KNN model

mean(pred_knn == testing_set$churn_number)

misClasificError <- mean(pred_knn != testing_set$churn_number)
print(paste('KNN Accuracy',1-misClasificError))
#"KNN Accuracy 0.760056791292002"

#table  

table(testing_set$churn_number, pred_knn  > 0.5)

#- confusion matrix
confusionMatrix(factor(pred_knn),factor(testing_set$churn_number))



