#made by sawan
Project <- data.frame(
Excercise <- c(18,8,6,7,12,7,4,3,5,5,0,0,0,0,12,0,0,14,14,14,0,8,14,8,16,7,9,4,13,5),
Weight <- c(65,60,75,70,72,69,54,80,68,60,65,88,85,80,75,85,70,60,86,85,92,60,70,68,68,88,70,75,71,74),
Height <- c(168,155,174,168,174,168,161,177,180,177,164,180,177,171,180,174,158,174,183,168,183,171,189,177,180,177,174,180,189,178))
ggpairs(data==Project, columns=1:3, title="Project data")
fit_1 <- lm(Weight ~ Excercise, data = Project)
#made by sawan
summary(fit_1)
ggplot(data = Project, aes(x = Excercise, y = Weight)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")
fit_2 <- lm(Weight ~ Excercise + Height, data = Project)
summary(fit_2)
a <-lm(Weight~Excercise, data = Project)$coefficients[2]
cat("Slope ", a)
sprintf("Equation: y= %s(x) + 76.4547",a)

predict(fit_2, data.frame(Height = 174, Excercise = 0))
#made by sawan


      