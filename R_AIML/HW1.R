data(airquality)
summary(airquality)
sum(is.na(airquality))
pairs(airquality)

help(boxplot)
boxplot(Ozone ~ interaction(Month), data = subset(airquality, !is.na(Ozone)))

boxplot(Ozone ~ interaction(Day), data = subset(airquality, !is.na(Ozone)))

library(plotly)
library(scatterplot3d)

scatterplot3d(airquality$Temp, airquality$Wind, airquality$Ozone,
              xlab = "Temperature", ylab = "Wind Speed", zlab = "Ozone Concentration",
              main = "Ozone Concentrations by Temperature and Wind Speed")

scatter_plot <- ggplot(airquality, aes(x = Temp, y = Wind, z = Ozone)) + 
  geom_point(aes(color = Ozone)) + 
  scale_color_gradient(low = "blue", high = "red") + 
  labs(title = "Ozone Concentrations by Temperature and Wind Speed",
       x = "Temperature", y = "Wind Speed", z = "Ozone Concentration")

ggplotly(scatter_plot, tooltip = c("Ozone", "Temp", "Wind"))

is.na(airquality$Ozone)
ozone_by_month=table(airquality$Month, is.na(airquality$Ozone))
ozone_by_month_pct <- prop.table(ozone_by_month, margin = 1) * 100
airquality$Solar.R
