
#libraries

if (!("ggplot2" %in% installed.packages()))
{
  install.packages("ggplot2")
}

if (!("dplyr" %in% installed.packages()))
{
  install.packages("dplyr")
}
library("ggplot2")
library("dplyr")

#create histogram and add title, axis labels

my_hist = ggplot(diamonds) + geom_histogram(binwidth = 0.5) + aes(x = carat)

#add title, change labels, use theme
my_hist + labs(title = "Some Diamonds!", x = "carat", y="count") + theme_bw()
my_hist

#Kernel density plot
my_kd = ggplot(diamonds, aes(x = carat)) + geom_density() + 
  labs(title="Weight of Dmnds",x = "Carat", y="Density")
my_kd

#Scatterplot
my_sc = ggplot(diamonds,aes(x = carat, y=price, colour= cut)) + geom_point(alpha=0.5) +
  labs(x="Carat",y="Price")
my_sc

#Frequency for each cut in the data
ggplot(diamonds) + geom_bar(aes(x=cut,fill=cut,colour=carat))

#carat vs price for ideal cut
ggplot(diamonds %>% filter(cut=="Ideal")) + 
  geom_point(aes(x=carat,y=price),alpha=0.5) + 
  labs(title= "Diamonds!",x="carat",y="Price")

#carat vs price per cut
my_sc + facet_grid(~ cut) + theme_linedraw()

