install.packages("pracma")
library(pracma)
library(ggplot2)

#Load the CSV
data <- read.csv("charge_2.csv")

#Plot Histogram for Raw Charge (Q)
ggplot(data, aes(x =Q))+
  geom_histogram(binwidth = 1e-19, fill="skyblue",color = "black")+
  ggtitle("Histogram of corrected Charge (Qc)")+
  xlab("Charge (Coulombs)")+
  ylab("Frequency")

#Extract Q values
charge_data <- na.omit(data$Q)

#Create a histogram
hist_data <- hist(charge_data, breaks=20,plot = TRUE)

#Detect peaks
peaks <- findpeaks(hist_data$counts)

#Display the peak values
peak_values <- hist_data$mids[peaks[,2]]
print(peak_values)

#Find the smallest peak value
smallest_peak <- min(peak_values)
print(paste("The smallest distinct peak value is: ", smallest_peak))

#Fit the Linear model

data$Q_2_3 <- (data$Q)^(2/3)
data$Qc_2_3 <- (data$Qc)^(2/3)

#Scatter plot
ggplot(data, aes(x=Qc_2_3, y=Q_2_3))+
  geom_point(color ="blue")+
  ggtitle("The graph of Q^(2/3) vs Qc^(2/3)")+
  xlab("Qc^(2/3)")+
  ylab("Q^(2/3)")
#Fit a linear model
model <- lm(Q_2_3 ~ Qc_2_3, data = data)

#Add Regression line to the Plot

ggplot(data, aes(x = Qc_2_3, y = Q_2_3))+
  geom_point(color = "blue")+
  geom_smooth(method= "lm", se = FALSE, color ="red")+
  ggtitle("Q^(2/3) vs Qc^(2/3) with Fitted Line")+
  xlab("Qc^(2/3)")+
  ylab("Q^(2/3)")

print(summary(model))
