# First source hg.emissions.bysec.T
# Creates a vertical bar chart of Hg emissions by country for the
# top 8 mercury emitting counrties. Bar areas colored by sector.

# Subset data to remove all countries except top 8 highest emitters
cbe <- with (data, reorder(Country.Name, Emission.Est..kg., sum))
cbe <- rev(levels(cbe))
top8cbe <- cbe[1:8]
newdata <- data[data$Country.Name %in% top8cbe,]

#Replace the really long name for China with a much shorter one
newdata$Country.Name <- as.character(newdata$Country.Name)
newdata$Country.Name[newdata$Country.Name ==
  "China (and Hong Kong if not separately identified)"] <- "China"
newdata$Country.Name <- as.factor(newdata$Country.Name)

#Put the minor emissions sectors in an "other" catagory
newdata$Sector <- as.character(newdata$Sector)
newdata$Sector[!newdata$Sector %in% c("ASGM", "Cement Production",
                                      "Coal Combustion",
                                      "Non-ferrous Metals", 
                                      "Consumer Products")]<- "Other"
newdata$Sector <- as.factor(newdata$Sector)

#reorder sector so bar fills stack correctly
newdata$Sector <- factor(newdata$Sector,
                         levels = c("ASGM",
                                    "Coal Combustion",
                                    "Non-ferrous Metals", 
                                    "Cement Production",
                                    "Consumer Products",
                                    "Other"), ordered = TRUE)

p2 <- ggplot(newdata, aes(y = Emission.Est..kg.,
  x = reorder(Country.Name, Emission.Est..kg., sum),
  fill = Sector, order = Sector)) +
  geom_bar(stat = "identity")+
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
  theme(legend.direction="vertical")+
  theme(legend.position = c(0.75, 0.3))+
  theme(legend.background = element_blank())+
  theme(legend.key = element_blank())+
  scale_y_continuous("Mercury Emissions (metric tons)",
                     breaks = c(100000,200000,300000,400000,500000,600000),
                     labels = c("100", "200", "300", "400", "500",
                                "600"))
  
#p2+scale_fill_few("medium")