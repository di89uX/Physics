library(ggplot2)
data <- read.csv("charge_2.csv")
View(data)

#Plot the Histogram
ggplot(data, aes(x= Q)) + geom_histogram(binwidth = 1e-19, fill ="skyblue", color="black") + labs(title = "Histogram of Raw charges in Millikan's oil drop Experiment", x ="Charge(Q)",y="Frequency(n)") 

#Density plot
ggplot(data, aes(x=Q))+
  geom_density(fill="blue")+
  ggtitle("Density Plot of corrected charge (Qc)")+
  xlab("Q (C)")+
  ylab("Density")


