
data <- read.csv("diabetes.csv")



summary(data)
data <- data[ , c(2:22,1)]
colnames(data)[22] <- "Diabetes"

apply(data, MARGIN = 2, FUN = function(x) sum(is.na(x)))
# nema Na vr



###################### Conversion ####################




### Diabetes ####
table(data$Diabetes)
data$Diabetes <- as.factor(data$Diabetes)
levels(data$Diabetes) <- c("No","Pre", "Yes")
table(data$Diabetes)

# 253.680obs.
# No    Pre    Yes 
# 213703   4631  35346 







#################### YES / NO ##########################
y_n_vars <- c(1:3,5:13,17)

data[,y_n_vars] <- apply(X = data[,y_n_vars], 
                         MARGIN = 2, 
                         FUN = function(x) ifelse(test = x==1, yes = "Yes", no = "No") )

data[,y_n_vars] <- lapply(data[,y_n_vars] , factor)



###################### GenHlth #############################

table(data$GenHlth)

data$GenHlth <- as.factor(data$GenHlth)
levels(data$GenHlth) <- c("excellent", "very good", "good", "fair", "poor")

table(data$GenHlth)


####################### Sex ##############################

table(data$Sex)

data$Sex <- ifelse(test = data$Sex == 0 , yes = "F", no = "M")
data$Sex <- as.factor(data$Sex)

table(data$Sex)

######################## Age ######################

table(data$Age)

data$Age <- as.factor(data$Age)
levels(data$Age) <- c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 to 79","80 or older")

table(data$Age)



######################## Education ###################

table(data$Education)

data$Education <- as.factor(data$Education)
levels(data$Education) <- c("Never attended school or only kindergarten", "Grades 1 - 8 (Elementary)", "Grades 9 - 11 (Some high school)", "Grade 12 or GED (High school graduate)", "College 1 year to 3 years (Some college or technical school)","College 4 years or more (College graduate)")

table(data$Education)

######################### Income ######################3

table(data$Income)

data$Income <- as.factor(data$Income)
levels(data$Income) <- c("Less than $10,000", "Less than $15,000", "Less than $20,000", "Less than $25,000", "Less than $35,000", "Less than $50,000", "Less than $75,000","$75,000 or more")

table(data$Income)




summary(data)



######################################################################

### HighBP ###
ggplot(data = data,
       mapping = aes(x= HighBP)) +
  geom_bar(position = "dodge")+
  theme_bw()

### HighChol ###
ggplot(data = data,
       mapping = aes(x= HighChol)) +
  geom_bar(position = "dodge")+
  theme_bw()

### CholCheck ###  ///
ggplot(data = data,
       mapping = aes(x= CholCheck)) +
  geom_bar(position = "dodge")+
  theme_bw()

### Smoker ###
ggplot(data = data,
       mapping = aes(x= Smoker)) +
  geom_bar(position = "dodge")+
  theme_bw()


### Stroke ###  ///
ggplot(data = data,
       mapping = aes(x= Stroke)) +
  geom_bar(position = "dodge")+
  theme_bw()

### HeartDiseaseorAttack ###  ///
ggplot(data = data,
       mapping = aes(x= HeartDiseaseorAttack)) +
  geom_bar(position = "dodge")+
  theme_bw()

### PhysActivity ###  /
ggplot(data = data,
       mapping = aes(x= PhysActivity)) +
  geom_bar(position = "dodge")+
  theme_bw()


### Fruits ###  
ggplot(data = data,
       mapping = aes(x= Fruits)) +
  geom_bar(position = "dodge")+
  theme_bw()

### Veggies ###  /
ggplot(data = data,
       mapping = aes(x= Veggies)) +
  geom_bar(position = "dodge")+
  theme_bw()

### HvyAlcoholConsump ###  ///
ggplot(data = data,
       mapping = aes(x= HvyAlcoholConsump)) +
  geom_bar(position = "dodge")+
  theme_bw()

### AnyHealthcare ###  ///
ggplot(data = data,
       mapping = aes(x= AnyHealthcare)) +
  geom_bar(position = "dodge")+
  theme_bw()

### DiffWalk ###  //
ggplot(data = data,
       mapping = aes(x= DiffWalk)) +
  geom_bar(position = "dodge")+
  theme_bw()




                    ### Reducing Factor Levels ###



library(ggplot2)


####################### GenHlth #######################
ggplot(data = data,
       mapping = aes(x= GenHlth)) +
  geom_bar(position = "dodge")+
  theme_bw()

table(data$Age)

levels(data$GenHlth) <- c("excellent", "very good", "good", "fair", "poor")


levels(data$GenHlth) <- c("excellent", "very good", "good", "fair/poor", "fair/poor")

ggplot(data = data,
       mapping = aes(x= GenHlth)) +
  geom_bar(position = "dodge")+
  theme_bw()




######################## Age #############################
ggplot(data = data,
       mapping = aes(x= Age)) +
  geom_bar(position = "dodge")+
  theme_bw()

table(data$Age)


levels(data$Age) <- c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 to 79","80 or older")


levels(data$Age) <- c("18 to 34", "18 to 34", "18 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 to 79","80 or older")
levels(data$Age) <- c("18 to 34", "35 to 39", "40 to 44", "45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 or older","75 or older")
levels(data$Age) <- c("18 to 34", "35 to 44", "35 to 44", "45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 or older","75 or older")


ggplot(data = data,
       mapping = aes(x= Age)) +
  geom_bar(position = "dodge")+
  theme_bw()



########################## Education ####################
ggplot(data = data,
       mapping = aes(x= Education)) +
  geom_bar(position = "dodge")+
  theme_bw()

table(data$Education)

levels(data$Education) <- c("Never attended school or only kindergarten", "Grades 1 - 8 (Elementary)", "Grades 9 - 11 (Some high school)", "Grade 12 or GED (High school graduate)", "College 1 year to 3 years (Some college or technical school)","College 4 years or more (College graduate)")


levels(data$Education) <- c("High school graduate or less", "High school graduate or less", "High school graduate or less", "High school graduate or less", "College 1 year to 3 years (Some college or technical school)","College 4 years or more (College graduate)")
levels(data$Education) <- c("High school graduate or less", "College or technical school", "College 4 years or more")


ggplot(data = data,
       mapping = aes(x= Education)) +
  geom_bar(position = "dodge")+
  theme_bw()



########################## Income ######################
ggplot(data = data,
       mapping = aes(x= Income)) +
  geom_bar(position = "dodge")+
  theme_bw()

table(data$Income)

levels(data$Income) <- c("Less than $10,000", "Less than $15,000", "Less than $20,000", "Less than $25,000", "Less than $35,000", "Less than $50,000", "Less than $75,000","$75,000 or more")


levels(data$Income) <- c("Less than $10,000", "Less than $15,000", "Less than $20,000", "Less than $25,000", "Less than $35,000", "Between $50,000 and $75,000", "Between $50,000 and $75,000","$75,000 or more")
levels(data$Income) <- c("Less than $35,000", "Less than $35,000", "Less than $35,000", "Less than $35,000", "Less than $35,000", "Between $50,000 and $75,000","$75,000 or more")


ggplot(data = data,
       mapping = aes(x= Income)) +
  geom_bar(position = "dodge")+
  theme_bw()


######################################################################
                    ### Feature importance ###

library(ggplot2)

#### HighBP ###  
ggplot(data,
       mapping = aes(x=HighBP, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()
# veci broj dijabeticara sa viskom pritiksom
# zavise



### HIghChol ###
ggplot(data,
       mapping = aes(x=HighChol, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()
# veci broj dijabeticara sa holesterolom
# zavise



### CholCheck ###    
# no - cholesterol check in 5 years 
# yes - cholesterol check in 5 years
ggplot(data,
       mapping = aes(x=CholCheck, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()
# zavise



### BMI ###     Body Mass Index: 12 - 98
ggplot(data,
       mapping = aes(x=BMI, fill=Diabetes))+
  geom_density(alpha=0.7) +
  theme_light()
# veci BMI za Yes
# zavise



### Smoker ###    Have you smoked at least 100 cigarettes in your entire life? [Note: 5 packs = 100 cigarettes]
ggplot(data,
       mapping = aes(x=Smoker, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()
# --
chisq.test(data$Diabetes, data$Smoker)
#  p < 0.05  


### Stroke ###
ggplot(data,
       mapping = aes(x=Stroke, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()

chisq.test(data$Diabetes, data$Stroke)
#  p < 0.05 -



### HeartDiseaseorAttack ###   coronary  heart disease (CHD) or myocardial infarction (MI)
ggplot(data,
       mapping = aes(x=HeartDiseaseorAttack, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()

chisq.test(data$Diabetes, data$HeartDiseaseorAttack)
#  p < 0.05  -



### PhysActivity ###  physical activity in past 30 days - not including job
ggplot(data,
       mapping = aes(x=PhysActivity, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()
# zavise
chisq.test(data$Diabetes, data$PhysActivity)
#  p < 0.05 



### Fruits ###   Consume Fruit 1 or more times per day
ggplot(data,
       mapping = aes(x=Fruits, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()

chisq.test(data$Diabetes, data$Fruits)
#  p < 0.05  zavise



### Veggies ###    Consume Vegetables 1 or more times per day
ggplot(data,
       mapping = aes(x=Veggies, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()

chisq.test(data$Diabetes, data$Veggies)
#  p < 0.05  zavise



### HvyAlcoholConsump ###  Heavy drinkers (adult men having more than 14 drinks per week and adult women having more than 7 drinks per week)
ggplot(data,
       mapping = aes(x=HvyAlcoholConsump, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()
# ne zavise

chisq.test(data$Diabetes, data$HvyAlcoholConsump)
#  p < 0.05?



### AnyHealthcare ###   Have any kind of health care coverage, including health insurance, prepaid plans such as HMO, etc.
ggplot(data,
       mapping = aes(x=AnyHealthcare, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()
# -

chisq.test(data$Diabetes, data$AnyHealthcare)
#  p < 0.05  



### NoDocbcCost ###   Was there a time in the past 12 months when you needed to see a doctor but could not because of cost?
ggplot(data,
       mapping = aes(x=NoDocbcCost, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()
# - 
chisq.test(data$Diabetes, data$NoDocbcCost)
#  p < 0.05  



### GenHlth ###   Would you say that in general your health is: scale 1-5 1 = excellent 2 = very good 3 = good 4 = fair 5 = poor
ggplot(data,
       mapping = aes(x=GenHlth, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()
# zavisee



### MentHlth ###  Now thinking about your mental health, which includes stress, depression, and problems with emotions, for how many days during the past 30 days was your mental health not good?
ggplot(data,
       mapping = aes(x=MentHlth, fill=Diabetes))+
  geom_density(alpha=0.7) +
  theme_light()

kruskal.test(Diabetes ~ MentHlth, data = data)
# p < 0.05 zavise



### PhysHlth ###  Now thinking about your physical health, which includes physical illness and injury, for how many days during the past 30 days was your physical health not good?
ggplot(data,
       mapping = aes(x=PhysHlth, fill=Diabetes))+
  geom_density(alpha=0.7) +
  theme_light()

kruskal.test(Diabetes ~ PhysHlth, data = data)
# p < 0.05 zavise



### DiffWalk ###   Do you have serious difficulty walking or climbing stairs?
ggplot(data,
       mapping = aes(x=DiffWalk, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()

chisq.test(data$Diabetes, data$DiffWalk)
#  p < 0.05  zavise



### Sex ###
ggplot(data, 
       mapping = aes(x=Sex, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()
# ne zavise
chisq.test(data$Diabetes, data$Sex)
#  p < 0.05? 



### Age ###
ggplot(data, 
       mapping = aes(x=Age, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()
# zavise



### Education ###
ggplot(data, 
       mapping = aes(x=Education, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()

chisq.test(data$Diabetes, data$Education)
# p < 0.05 zavise



### Income ###
ggplot(data, 
       mapping = aes(x=Income, fill=Diabetes))+
  geom_bar(position = "dodge") +
  theme_light()

chisq.test(data$Diabetes, data$Income)
# p < 0.05 zavise


