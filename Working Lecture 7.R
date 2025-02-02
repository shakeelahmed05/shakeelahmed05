library(ggplot2)##For Ploting
library(gridExtra)##Combining Plots and Tables
# Example data frame

dat2 <- data.frame(
  Index = c(1, 2, 3, 4), ## This provides an order to the data
  label = c("Age", "SEX",  "Income", "EDU"),
  OR = c(1.00, 2.00, 3.00, 0.50),
  LL = c(0.25, 0.90, 2.25, 0.2),
  UL = c(1.75, 3.10, 3.75, 0.8),
  CI = c("0.25, 1.75", "0.90, 3.10", "2.25, 3.75", "0.20, 0.80")
)
dat2


plot2 <- ggplot(dat2, aes(x=OR, y=label, shape=label), color="black") +
  #Add dot plot and error bars
  geom_errorbar(aes(xmin = LL, xmax = UL), width = 0.20) +
  geom_point(size = 2.5) + 
  ggtitle("Odds ratio with 95%CI") +
  #Add a line above graph
  geom_hline(yintercept=4.6, size=2) + 
  #Add a reference dashed line at 20
  geom_vline(xintercept = 1, linetype = "longdash") + 
  labs(x="OR and 95% CI", y = "Variable") +
  scale_x_continuous(breaks=seq(-2,8,2), limits=c(-1,5), expand=c(0,0) ) +
  scale_shape_manual(values=c(15,16,17,18)) +
  theme_gray(base_size=14) +
  theme(legend.position = "none", #Remove legend
        plot.title = element_text(hjust =0.5),## Sets title to center
        axis.line.x = element_line(size = 0.7), # Sets axis line to 0.7 cm.
        axis.ticks.length=unit(0.3,"cm"),# Sets tick mark length to 0.3 cm.
        axis.text.y  = element_blank(), #Remove axis text
        axis.line.y = element_blank(), #Remove axis line
        axis.ticks.y = element_blank(), #Remove axis tick marks
        axis.title.y  = element_blank() #Remove axis title marks
  )

plot2
##############################################
tab0 <- ggplot(data=dat2) +
  geom_text(aes(y=Index, x=1, label= paste0(label)), vjust=0) +
  #Add a line above graph
  geom_hline(yintercept=4.6, size=2) + 
  ggtitle("Index") +
  xlab("  ") +
  theme_classic(base_size=14) +
  theme(
    axis.line.y = element_blank(), ##Remove Y axis line
    axis.line.x = element_line(color = "white"), #Makes the x-axis line white
    axis.text.y  = element_blank(), ##Remove Y axis text
    axis.ticks.y  = element_blank(), ##Remove Y axis ticks
    axis.ticks.x = element_line(color = "white"), #Makes the x-axis tick white
    axis.ticks.length=unit(0.3,"cm"), ## Sets tick mark length to 0.3 cm.
    axis.title.y  = element_blank(),
    axis.text.x = element_text(color="white"), ##Makes the x-axis text white
    plot.title = element_text(hjust =0.5) ## Centers the plot title
  )

tab0
###########
tab1 <- ggplot(data=dat2) +
  geom_text(aes(y=label, x=1, label= paste0(round(OR, digits=2))), vjust=0) +
  #Add a line above graph
  geom_hline(yintercept=4.6, size=2) + 
  ggtitle("OR") +
  xlab("  ") +
  theme_classic(base_size=14) +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "white"),
    axis.text.y  = element_blank(),
    axis.ticks.y  = element_blank(),
    axis.ticks.x = element_line(color = "white"),
    axis.ticks.length=unit(0.3,"cm"),
    axis.title.y  = element_blank(),
    axis.text.x = element_text(color="white"),
    plot.title = element_text(hjust =0.5)
  )

tab1
tab2 <- ggplot(data=dat2) +
  geom_text(aes(y=label, x=1, label= paste0("(",round(LL, digits=2),"; ", round(UL, digits=2),")")), vjust=0) +
  #Add a line above graph
  geom_hline(yintercept=4.6, size=2) + 
  ggtitle("90% CI") +
  xlab("  ") +
  theme_classic(base_size=14) +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "white"),
    axis.text.y  = element_blank(),
    axis.ticks.y  = element_blank(),
    axis.ticks.x = element_line(color = "white"),
    axis.ticks.length=unit(0.3,"cm"),
    axis.title.y  = element_blank(),
    axis.text.x = element_text(color="white"),
    plot.title = element_text(hjust =0.5)
  )

tab2


#Put the individual components of the forest plot together
fplt <- grid.arrange(tab0, plot2, tab1, tab2, widths=c(1,4,1,2))




### FOREST PLOT WITH A MULTIPLE BAR CHART
dat1 <- data.frame(
  Index = c(1, 2, 3, 4,5,6,7,8), ## This provides an order to the data
  label = c("Age", "Age","SEX", "SEX", "Income", "Income", "EDU", "EDU"),
  Prob = c(0.3, 0.4, 0.3, 0.50, 0.3, 0.12, 0.33, 0.50),
  GROUP= c("A", "B", "A", "B", "A", "B", "A", "B"),
  LL = c(0.25, 0.90, 2.25, 0.2, 0.25, 0.90, 2.25, 0.2),
  UL = c(1.75, 3.10, 3.75, 0.8, 1.75, 3.10, 3.75, 0.8)
)

plot1 <-ggplot(dat=dat1, aes(x=factor(label), y=Prob, fill=factor(GROUP))) +
  #Creates a bar graph
  geom_bar(stat="identity", width=0.5, position=position_dodge()) +
  #Flips the axes to create horizontal bars
  coord_flip() +
  ggtitle("Class Proportions") +
  #Creates a horizontal line on top of the graph
  geom_vline(xintercept=4.6, size=2) + 
  labs(y = "Proportion of Events", x = "Variable", fill = "Group") +
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1), expand=c(0,0) ) +
  scale_fill_manual(values=c("#E0EEEE", "#5F9EA0")) + 
  theme_classic(base_size=14) +
  theme(
    #Colors the y-axis line white to make it invisible and setting the length of ticks
    axis.ticks.length=unit(0.3,"cm"),
    axis.line.y = element_line(colour = "white"),
    axis.line.x = element_line(size = 0.6),
    #Use this to control the legend position
    plot.title = element_text(hjust =0.5),
    legend.position=c(0.8, 0.1),
    legend.title=element_blank()
  )

plot1
plt <- grid.arrange(plot1, plot2, tab1, tab2, widths=c(4,4,1,2))
