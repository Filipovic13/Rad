
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
levels(data$Age) <- c("18 to 34", "35 to 39", "40 to 44", "45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 to older","75 to older")
levels(data$Age) <- c("18 to 34", "35 to 44", "35 to 44", "45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 to older","75 to older")


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


