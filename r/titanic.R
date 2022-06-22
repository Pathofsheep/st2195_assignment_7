#Practice assignment 7

#libraries

if (!("ggplot2" %in% installed.packages()))
{
  install.packages("ggplot2")
}
if (!("dplyr" %in% installed.packages()))
{
  install.packages("dplyr")
}
if (!("vioplot" %in% installed.packages()))
{
  install.packages("vioplot")
}

library("ggplot2")
library("dplyr")
#library("vioplot")

#gsm     = read.csv("N:\\Uni_Bigdata\\gender_submission.csv", sep = ',') 
#test    = read.csv("N:\\Uni_Bigdata\\test.csv", quote = '"', sep = ',') 
#train   = read.csv("N:\\Uni_Bigdata\\train.csv", quote = '"', sep = ',') 
titanic   = read.csv("N:\\Uni_Bigdata\\titanic.csv", quote = '"', sep = ',') 

#Part1

#Gender
bc1 = ggplot(titanic, aes(x = Sex)) + 
  geom_bar(fill='red') + 
  labs(title = "Gender barplot", x = "gender")
bc1

#Ticketclass (Assuming Pclass is the Ticketclass)
bc2 = ggplot(titanic, aes(x = Pclass)) + 
  geom_bar() + 
  labs(title = "Ticketclass barplot", x = "Ticketclass")
bc2

#Survived, changed 0 and 1 to yes/no
bc3 = ggplot(titanic, aes(x = Survived)) + 
  geom_bar() + 
  labs(title = "Survival barplot", x = "Survival") +
  #scale_x_continuous(breaks = 0:1, labels = c("no", "yes"))
  # breaks to set breakpoints
  # label to change 0/1 to no/yes
  # reverse to have Yes in front
  scale_x_reverse(breaks = 0:1, labels = c("no", "yes"))
bc3

#Part2

#Age
sum(is.na(titanic$Age)) #a few NAs here ...
# Remove them via subset fx
hg1 = ggplot(subset(titanic,!is.na(titanic$Age)), aes(x = Age), ) + 
  geom_histogram(binwidth = 5, fill = "steelblue") + 
  labs(title = "Age histogram")
hg1

#Age per Ticketclass 
bp1 = ggplot(subset(titanic,!is.na(titanic$Age)), aes(x = Age, y = as.factor(Pclass), group =Pclass)) + 
  geom_boxplot() + 
  labs(title = "Age/Ticketclass Boxplot", y = "Ticketclass")
bp1

#Age per Survival
bp2 = ggplot(subset(titanic,!is.na(titanic$Age)), aes(x = Age, y = Survived, group =Survived)) + 
  geom_boxplot(fill="steelblue") + 
  labs(title = "Age/Survival Boxplot", y = "Survival") + 
  scale_y_continuous(breaks = 0:1, labels = c("no", "yes"))
bp2

#Part3

#Histogram Travel fare
hg2 = ggplot(titanic, aes(x = Fare)) + 
  geom_histogram(binwidth = 10) + 
  #Removing 3 extreme outliers for better scale
  scale_x_continuous(breaks = seq(0, 300, by = 50),limits = c(-5,295)) +
  labs(title = "Travel Fare histogram")
hg2


#Table Travel fare
# Vector to order the table output
table(ifelse(titanic$Fare > 0,"Paid","Didn't Pay"))[c("Paid","Didn't Pay")]

#Appearently there were 10 people  from a 'guarantee group', 
#overseeing the titanic's smooth voyage with a free ticket.
#Others were meant to travel with the ship Philadelphia, 
#but due to scheduling problems Philadelphia's voyage was cancelled.

#Part4

#Histogram family size / Ticketclass
hgx = ggplot(titanic, aes(x=SibSp, fill=as.factor(Pclass))) +
  geom_histogram(alpha = 0.9, binwidth = 1) + 
  labs(title= "Scatterplot Family Size/Ticketclass", x = "Family Size", fill = "Ticketclass") +
  scale_x_continuous(breaks = c(0:8)) +
  theme_linedraw()
hgx

#Part5

#Stacked bar charts of survival differing between gender and ticketclass

#Check if data on plot is correct ... 
(titanic$Survived[titanic$Sex == "female" & titanic$Pclass == 1])

# Labels for Ticketclass missing, how to put it on top?

hgx2 = ggplot(titanic,aes(x = Survived, fill = as.factor(Sex))) +
  geom_histogram(binwidth = 1) + 
  labs(title = "Histogram: Survival count over Gender / Ticketclass", x = "Survived", y = "Count", fill = "Gender") + 
  scale_x_reverse(breaks = 0:1, labels = c("no", "yes"), position = "bottom") +
  scale_fill_discrete(labels = c("female", "male")) +
  scale_y_continuous(breaks = seq(0, 400, by = 50)) +
  theme_minimal()

#Facet_grid to make the graph for each pclass
hgx2 + facet_grid(~Pclass)

#Part6

#Violin plot Survival over Age / Gender

df = subset(titanic,!is.na(titanic$Age))
length(df$Survived[df$Survived == 1 & df$Sex == "female" & between(df$Age,0,4)])

vp1 = ggplot(subset(titanic,!is.na(titanic$Age)),aes(x = as.factor(Survived), y = Age, fill = as.factor(Sex))) +
  geom_violin() +
  scale_x_discrete(breaks = 0:1, labels = c("no", "yes")) +
  labs(title = "Violin Plot Survival over Age / Gender", x = "Survival", fill = "Gender") +
  scale_y_continuous(breaks = seq(0,80, by = 5)) +
  theme_bw()

#vioplot(Age ~ as.factor(Sex), data=subset(titanic,!is.na(titanic$Age)),col = titanic$Survived+2)

vp1

#Violin plot Survival over Age / Ticket Class

length(df$Survived[df$Survived == 0 & df$Pclass == 2 & between(df$Age,0,15)])

vp2 = ggplot(subset(titanic,!is.na(titanic$Age)),aes(x = as.factor(Survived), y = Age, fill = as.factor(Pclass))) +
  geom_violin() +
  scale_x_discrete(breaks = 0:1, labels = c("no", "yes")) +
  labs(title = "Violin Plot Survival over Age / Ticketclass", x = "Survival", fill = "Ticketclass") +
  scale_y_continuous(breaks = seq(0,80, by = 5)) +
  theme_bw()

vp2

