library(ggplot2)
library(ggthemes)
data <- read.csv("Hg.emissions.csv")

#Shorten a few of the names in the Sector factor
data$Sector <- as.character(data$Sector)
data$Sector[data$Sector ==
              "Non-ferrous Metals Production"] <- "Non-ferrous Metals"
data$Sector[data$Sector ==
              "Iron and Steel Production"] <- "Iron and Steel"
data$Sector[data$Sector ==
              "Consumer Product Waste"] <- "Consumer Products"
data$Sector[data$Sector ==
              "Chlor-alkali Production"] <- "Chlor-alkali"
data$Sector <- as.factor(data$Sector)

#remove the sectors with the lowest emissions
top8 <- data[data$Sector != "Cremation" &
             data$Sector != "Oil and Gas Combustion" &
             data$Sector != "Waste Incineration",]

p1 <- ggplot(top8, aes(y = Emission.Est..kg.,
                      x = reorder(Sector, Emission.Est..kg., sum))) +
       geom_bar(stat = "identity", fill = "Steelblue4")+ 
       coord_flip()+
  theme_wsj()+
       theme(axis.title = element_text(colour="black"))+
       theme(title=element_text(family="sans", face = "italic", size=rel(1), vjust = 0))+
       theme(axis.title.y = element_blank())+
       theme(axis.text.y = element_text(colour = "black", hjust = 1),
             axis.text.x = element_text(colour = "black"))+
       theme(panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank())+
       theme(panel.grid.major.x = element_line(colour = "gray20"))+
       scale_y_continuous("Mercury Emissions (metric tons)",
                          breaks = c(100000,200000,300000,400000,500000,600000,
                                     700000),
                          labels = c("100", "200", "300", "400", "500",
                                     "600", "700"))
