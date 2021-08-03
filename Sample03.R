# Ricca Callis
# EN 605.662 Data Visualization
# Project 4 - Data Exploration and Design
# 3 Required Data Sets:
#   Data Set 1: https://www.kaggle.com/spscientist/students-performance-in-exams 
#   Data Set 2: https://www.kaggle.com/kimjihoo/coronavirusdataset?select=Case.csv 
#   Data Set 3: https://www.kaggle.com/karangadiya/fifa19 

# Project Instructions:

# I. Purpose:
#   Recently, a number of libraries have been released to help organizations 
#   develop new visualizations and illustration tools. Some of the popular libraries include D3, 
#   Chart.js, Plot.ly, Highcharts, Bokeh, ggplot, matplotlib, ProtoVIS, R Shiny, NVD3, etc… 
#   As data scientists, it is important for us to have a basic understanding of those libraries 
#   and their capabilities. The purpose of this assignment is to get familiar with open source 
#   libraries by developing three sample visualizations using any libraries for JavaScript, R, or Python.

# II. Task
#   1. Analyze 3 data sets. The data should have 3 or more variables and 100 or more rows.
#   2. Develop 3 different visualizations by leveraging the library of your choice to illustrate
#      the datasets under consideration.
#   3. File structure: students should structure their project the following way:
#       o your_lastname_project04/
#           	Paper: your_lastname_project04.pdf
#                 • Introduction: What the project is about
#                 • Dataset: explain and provide links to the sources
#                 • Approach: explain which libraries you selected
#                 • Visualization #1: Explain, provide screenshot, and justify
#                 • Visualization #2: Explain, provide screenshot, and justify
#                 • Visualization #3: Explain, provide screenshot, and justify
#                 • Conclusion
#                 • References
#           	src/
#                 • Sample01/
#                     o Index1.html or Sample1.py or Sample1.R, etc…
#                     o Data1.csv
#                     o Screenshot_sample01.jpg
#                 • Sample02/
#                     o Index2.html or Sample2.py or Sample2.R, etc…
#                     o Data2.csv
#                     o Screenshot_sample02.jpg
#                 • Sample03/
#                     o Index3.html or Sample3.py or Sample3.R, etc…
#                     o Data3.csv
#                     o Screenshot_sample03.jpg
#          Requeriments.txt: List dependencies (e.g. R, Shiny R, Python, matplotlib, D3, etc…). 
# IV. What to submit
#     • A .zip file with the file structure shown above
#     • A paper describing the projects, the datasets that were chosen, the thee visualizations or
#       dashboards that were developed (including screenshot), and explanation of what was
#       updated from any sample code that was used.
#     • Submit document through Blackboard. Please use the following file format: your_lastname_project04.zip

# Load standard libraries
library(readr)
library(ggplot2)
library(readxl)
library (MPV)
library (olsrr)
library("ggpubr")
library("Hmisc")
library(caret)
library (lattice)
library(leaps)
library(MASS)
library(alr3)
library(readxl)
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(ggthemes)
library(viridis)
library("plotly")
library(corrplot)
library(maps)
library(alr3)
library(rms)
library(ppcor)
library(ggthemes)
library(data.table)
library(ISLR)
library(tibble)
library(aod)
library(tidyverse)
library(psych)
library(pastecs)
library(summarytools)
library(magrittr)
library(scales)
library(sf)
library(lubridate)
library(ggraph)
library(igraph)
library(dplyr)
library(reshape)
library(tidygraph)
library(ggthemes)
library(ggExtra)
library(cowplot)
library(maps)
library(highcharter)
install.packages("imputeTS")
library(imputeTS)
library(corrplot)
# Set  working directory
#setwd("/Users/riccacallis/Desktop/JHU/Data Science/Data Visualization/Project 4")

# DATA SET 3
# Data on FIFA 19 Complete Player
# https://www.kaggle.com/karangadiya/fifa19
# Read csv file
FIFA19 <- read_csv("/Users/riccacallis/Desktop/JHU/Data Science/Data Visualization/Project 4/FIFA19.csv")

# Look at the first six rows
head(FIFA19)
# Look at all data & attach it
View(FIFA19)
attach(FIFA19)
str(FIFA19)

# Check for missing data/null values
apply(is.na(FIFA19[,]),2,sum)
# Lots of missing values

cat("Complete Cases No Null ", sum(complete.cases(FIFA19)))
cat('Total Missing Values -> ', sum(is.na(FIFA19)))
cat('Missing Value Rate -> ', mean(is.na(FIFA19)))
cat("Highest Null Val", sum(!complete.cases(FIFA19)), 'Jersey.Number')

list_na <- colnames(FIFA19)[ apply(FIFA19, 2, anyNA)]
cat('Columns with null values -> ', list_na)

nulls <- sapply(FIFA19, function(x) sum(is.na(x)))
cat('Null Value Count for each Column -> ')
nulls

median_missing <- apply(FIFA19[,colnames(FIFA19) %in% list_na],
                        2,
                        median,
                        na.rm =  TRUE)

cat('Median values for Missing rows in each Columns-> ', median_missing)                           
#data.frame(median_missing) 

mean_missing <- apply(FIFA19[,colnames(FIFA19) %in% list_na],
                      2,
                      mean,
                      na.rm =  TRUE)

cat('Mean values for Missing rows in each Columns-> ', mean_missing)                             

cat('Using imputeTS to fill NA values with Mean') 
mean_fill <- FIFA19 %>%
  na_mean()
FIFA19 <- mean_fill 

cat('Missing Valus After filling With Mean', sum(is.na(FIFA19)))

dim(FIFA19)                                            

# Drop those players
drop_FIFA19 <- FIFA19 %>%                       
  na.omit()
FIFA19 <- drop_FIFA19
FIFA19 <- mean_fill 
dim(FIFA19)
# We Just Removed ', (18207 - 18147), 'Players'

# Descriptive Statistics
# Summary Statistics
summary(FIFA19) # N, class, min, 1Q, Median, Mean, 3Q, Max each variable
describe(FIFA19) # N, mean, sd, median, min, max, range, skew, kurtosis
stat.desc(FIFA19) # null, min, max, rang, sum, median, mean, SE mean, CI mean, var, std, coef var

# Overall Rate
summary(FIFA19$Overall) # Min (46), Q1 (62), Median (66), Mean (66.24), Q3 (71), Max (94)
# HighChart Histogram: Overall
hchart(FIFA19$Overall, name = 'Overall Rate FIFA 2019')
#Plotly Histogram: Overall
OverallDistribution <- plot_ly(FIFA19, x = ~Overall)%>%
  add_histogram(name="Plotly Distribution of FIFA 2019 Player Overall Rate")
overall_hist<-function(method="FD"){
  h<-hist(FIFA19$Overall, breaks = method, plot = FALSE)
  plot_ly(x=h$mids, y=h$counts) %>% add_bars(name=method)}
OverallDistribution
subplot(OverallDistribution, overall_hist(), overall_hist("Sturges"), overall_hist("Scott"), nrows=4, shareX=TRUE)
# Determine summary stats for Overall
overall_FIFA19 <- data.frame(version=" ",x=FIFA19$Overall)   
Overall_IQR <- overall_FIFA19 %>%
  select(x)%>%
  filter(!is.na(x))%>%
  summarise(median(x),mean(x), sd = sd(x), IQR = IQR(x), Q1 = quantile(x, probs = 0.25), Q3 = quantile(x, probs = 0.75))
#QQ norm: Overall
qqnorm(FIFA19$Overall, pch = 1, frame = FALSE)
qqline(FIFA19$Overall, col = "#b8cff5", lwd = 2) 

# Potential Rate
summary(FIFA19$Potential) # Min (48), Q1 (67), Median (71), Mean (71.31), Q3 (75), Max (95)
# HighChart Distribution: Potential Rate
hchart(FIFA19$Potential, name = 'Potential Rate FIFA19')
#Plotly Histogram: Potential
PotentialDistribution <- plot_ly(FIFA19, x = ~Potential)%>%
  add_histogram(name="Plotly Distribution of FIFA 2019 Player Potential Rate")
potential_hist<-function(method="FD"){
  h2<-hist(FIFA19$Potential, breaks = method, plot = FALSE)
  plot_ly(x=h2$mids, y=h2$counts) %>% add_bars(name=method)}
PotentialDistribution
subplot(PotentialDistribution, potential_hist(), potential_hist("Sturges"), potential_hist("Scott"), nrows=4, shareX=TRUE)
# Determine summary stats for Potential
potential_FIFA19 <- data.frame(version=" ",x=FIFA19$Potential)  
Potential_IQR <- potential_FIFA19 %>%
  select(x)%>%
  filter(!is.na(x))%>%
  summarise(median(x),mean(x), sd = sd(x), IQR = IQR(x), Q1 = quantile(x, probs = 0.25), Q3 = quantile(x, probs = 0.75))
# QQ Norm: Potential
qqnorm(FIFA19$Potential, pch = 1, frame = FALSE)
qqline(FIFA19$Potential, col = "#b8cff5", lwd = 2)  

# Age
summary(FIFA19$Age) # Min (16), Q1 (21), Median (25), Mean (25.12), Q3 (28), Max (45)   
# HighChart Histogram: Age
hchart(FIFA19$Age, name = 'Age FIFA 2019') 
#Plotly Histogram: Age
AgeDistribution <- plot_ly(FIFA19, x = ~Age)%>%
  add_histogram(name="Plotly Distribution of FIFA 2019 Player Ages")
age_hist<-function(method="FD"){
  h3<-hist(FIFA19$Age, breaks = method, plot = FALSE)
  plot_ly(x=h3$mids, y=h3$counts) %>% add_bars(name=method)}
AgeDistribution
subplot(AgeDistribution, age_hist(), age_hist("Sturges"), age_hist("Scott"), nrows=4, shareX=TRUE)
# Overall by Age
# HighChart
hcboxplot(x = FIFA19$Overall, var = FIFA19$Age) %>%
  hc_chart(type = "column") %>%
  hc_yAxis(title = list(text = "Overall Rate"),
           labels = list(format = "{value}%")) %>%
  hc_xAxis(title = list(text = "Age"),
           labels = list(format = "{value}"))    

# Potential by Age
hcboxplot(x = FIFA19$Potential, var = FIFA19$Age) %>%
  hc_chart(type = "column") %>%
  hc_yAxis(title = list(text = "Potential Rate"),
           labels = list(format = "{value}")) %>%
  hc_xAxis(title = list(text = "Age"),
           labels = list(format = "{value}")) 


# Wage
summary(FIFA19$Wage)
#Max = 100; if we set All values it would be messy We cant Compare them with Boxplots  
# Wage & Overall
hcboxplot(x = FIFA19$Overall, var = FIFA19$Wage) %>%
  hc_chart(type = "column") %>%
  hc_yAxis(title = list(text = "Overall Rate"),
           labels = list(format = "{value}")) %>%
  hc_xAxis(title = list(text = "Wage"),
           labels = list(format = "{value}"), max = 100) 
# Wage & Potential
hcboxplot(x = FIFA19$Potential, var = FIFA19$Wage) %>%
  hc_chart(type = "column") %>%
  hc_yAxis(title = list(text = "Potential Rate"),
           labels = list(format = "{value}")) %>%
  hc_xAxis(title = list(text = "Wage"),
           labels = list(format = "{value}"), max = 100)   

hchart(FIFA19$Wage, name = 'Wage') 

qqnorm(FIFA19$Wage, pch = 1, frame = FALSE)
qqline(FIFA19$Wage, col = "purple", lwd = 2)

# Value
summary(FIFA19$Value)
# Max = 100 if we set All values it would be messy We cant Compare them with Boxplots
# Value by Overall Rate
hcboxplot(x = FIFA19$Overall, var = FIFA19$Value) %>%
  hc_chart(type = "column") %>%
  hc_yAxis(title = list(text = "Overall Rate"),
           labels = list(format = "{value}")) %>%
  hc_xAxis(title = list(text = "Value"),
           labels = list(format = "{value}"), max = 100)  
# Value by Potential Rate
hcboxplot(x = FIFA19$Potential, var = FIFA19$Value) %>%
  hc_chart(type = "column") %>%
  hc_yAxis(title = list(text = "Potential Rate"),
           labels = list(format = "{value}")) %>%
  hc_xAxis(title = list(text = "Value"),
           labels = list(format = "{value}"), max = 100) 

hchart(FIFA19$Value, name = 'Value')

qqnorm(FIFA19$Value, pch = 1, frame = FALSE)
qqline(FIFA19$Value, col = "purple", lwd = 2)    

# Top-Valued Players
topValuedPlayers <- FIFA19 %>%
  select(Name, Nationality, Overall, Value, Wage)%>%
  group_by(Value)%>%
  arrange(desc(Value))%>%
  head(30)
data.frame(topValuedPlayers)   

require(scales)
players_plot <- FIFA19 %>%
  group_by(Value)%>%
  arrange(desc(Value))%>%
  head(30)%>%
  ggplot(mapping = aes(x = Name, y = Value, color = Nationality, fill = Nationality, alpha = Value, size=Value))+
  geom_bar(stat='identity')+
  coord_polar()+
  theme_minimal()+
  labs(x = 'Name', y ='Value', title ='Highest Valued Players')+
  theme(plot.title = element_text(hjust=0.5),legend.position ='bottom')+
  scale_y_continuous(labels = comma)

players_plot
# Error: Discrete value supplied to continuous scale; not sure how to fix this

# Top Overall Scored Players
summary(FIFA19$Overall)                           
topOverallPlayers <- FIFA19 %>%
  select(Name, Nationality, Overall, Value, Wage)%>%
  group_by(Overall)%>%
  arrange(desc(Overall))%>%
  head(30)
data.frame(topOverallPlayers)
head(topOverallPlayers)

require(scales)
players_plot_overall <- FIFA19 %>%
  group_by(Overall)%>%
  arrange(desc(Overall))%>%
  head(30)%>%
  ggplot(mapping = aes(x = Name, y = Overall, color = Nationality, alpha = Overall, size=Overall))+
  geom_point()+
  coord_polar()+
  theme_minimal()+
  labs(x = 'Name', y ='Overall', title ='Overall Top Players')+
  theme(plot.title = element_text(hjust=0.5),legend.position ='bottom')+
  scale_y_continuous(labels = comma)

players_plot_overall

# Replot with multiple user selections  
playerOverallHighlight <- highlight_key(topOverallPlayers)
ggPlayerOverall <- ggplot(playerOverallHighlight) +
  geom_point(mapping = aes(x = Name, y = Overall, color = Nationality, alpha = Overall, size=Overall))+
  theme_minimal()+
  labs(x = 'Name', y ='Overall', title ='Overall Top Players')+
  theme(plot.title = element_text(hjust=0.5),legend.position ='bottom')+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(labels = comma)
widgetSelections <- bscols(
  widths = c(12,12,12),
  filter_checkbox("id", "Select Overall", playerOverallHighlight, ~Overall, inline=TRUE),
  filter_select("id", "Select Nationality", playerOverallHighlight, ~Nationality),
  ggplotly(ggPlayerOverall)
)
bscols(widgetSelections)

# Top Potential Players
summary(FIFA19$Potential) 
topPotentialPlayers <- FIFA19 %>%
  select(Name,Nationality,Potential, Value, Wage)%>%
  group_by(Potential)%>%
  arrange(desc(Potential))%>%
  head(30)
data.frame(topPotentialPlayers) 

require(scales)
players_plot_Potential <- FIFA19 %>%
  group_by(Overall)%>%
  arrange(desc(Overall))%>%
  head(30)%>%
  ggplot(mapping = aes(x = Name, y = Potential, color = Nationality, alpha = Potential, size=Potential))+
  geom_point()+
  coord_flip()+
  theme_minimal()+
  labs(x = 'Name', y ='Potential', title =' Top Potential Players')+
  theme(plot.title = element_text(hjust=0.5),legend.position ='bottom')+
  scale_y_continuous(labels = comma)

players_plot_Potential

# Replot with multiple user selections  
playerPotentialHighlight <- highlight_key(topPotentialPlayers)
ggPlayerPotential <- ggplot(playerPotentialHighlight) +
  geom_point(mapping = aes(x = Name, y = Potential, color = Nationality, alpha = Potential, size=Potential))+
  coord_flip()+
  theme_minimal()+
  labs(x = 'Name', y ='Potential', title ='Top Potential Players')+
  theme(plot.title = element_text(hjust=0.5),legend.position ='bottom')+
  #theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(labels = comma)
widgetSelection <- bscols(
  widths = c(12,12,12),
  filter_checkbox("id", "Select Potential", playerPotentialHighlight, ~Potential, inline=TRUE),
  filter_select("id", "Select Nationality", playerPotentialHighlight, ~Nationality),
  ggplotly(ggPlayerPotential)
)
bscols(widgetSelection)

# Average Value WorldWide
options(scipen=999) #to prevent scientific notation
value_data = FIFA19 %>% 
  select(Name, Nationality, Value) %>%
  group_by(Nationality) %>% 
  summarise(Count = n(),
            Avg_Value = mean(Value)) %>% 
  filter(Count > 50)

value_data %>%
  select(Nationality, Avg_Value)%>%                       
  arrange(desc(Avg_Value))   

# IGNORE CANT GET TO WORK
# worldmap = map_data("world")
# merged_data <- merge(x = worldmap, y = value_data, by.x = "region", by.y = "Nationality", all.x = TRUE) %>% arrange(order)
# ggplot(data = merged_data, aes(x = long, y = lat, group = group)) +
#   geom_polygon(aes(fill = Avg_Value)) +
#   labs(fill='Average Value')
# # Didnt get map to work

# Try a different map
# Nationality & Number of Players
options(repr.plot.width = 12, repr.plot.height = 8)

world_map <- map_data("world")

numofplayers <- world_map %>% 
  mutate(region = as.character(region)) %>% 
  left_join((FIFA19 %>% mutate(Nationality = as.character(Nationality),
                               Nationality = if_else(Nationality %in% "England", 
                                                     "UK", Nationality)) %>%
               #filter(League == "Bundesliga") %>%
               count(Nationality, name = "Number of Player") %>%
               rename(region = Nationality) %>%
               mutate(region = as.character(region))), by = "region")

ggplot(numofplayers, aes(long, lat, group = group))+
  geom_polygon(aes(fill = `Number of Player` ), color = "white", show.legend = TRUE)+
  scale_fill_viridis_c(option = "C")+
  theme_void()+
  labs(fill = "Number of Player",
       title = "Number of Player with ggplot2")


# Heat Map for FIFA19 Correlations
# Create correlation matrix
corr3 <- cor(dplyr::select_if(FIFA19, is.numeric))
corr3
# Quick plotly heatmap
corrPlotly<- plot_ly(colors='RdBu') %>%
  add_heatmap(x=rownames(corr3), y=colnames(corr3), z=corr3)%>%
  colorbar(limits=c(-1,1))
corrPlotly
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(corr3)
# Visualize the lower triangle of the correlation matrix
# Barring the no significant coefficient
corr.plot <- ggcorrplot(
  corr3, hc.order = TRUE, type = "lower", outline.col = "white",
  p.mat = p.mat
)
corr.plot
# Make interactive
ggplotly(corr.plot)

# Need to reduce number of columns & make user interactive
corrHighlight <- highlight_key(corr3)
corrFilterPlot <- ggplot(corrHighlight) +
  geom_
geom_tile(mapping=(aes(x=rownames(corr3), y=colnames(corr3))))+
  ggtitle('Correlation Heat Map')
selectSkills <- bscols(
  filter_checkbox("id", "Select a Variable", corrHighlight, colnames(corr3)),
  ggplotly(corrFilterPlot)
)
bscols(selectSkills)



# Create player position classes
# Find unique positions
unique(FIFA19$Position)
# [1] "RF"  "ST"  "LW"  "GK"  "RCM" "LF"  "RS"  "RCB" "LCM" "CB"  "LDM" "CAM" "CDM" "LS" 
# [15] "LCB" "RM"  "LAM" "LM"  "LB"  "RDM" "RW"  "CM"  "RB"  "RAM" "CF"  "RWB" "LWB" NA 
# Separate into classes: defence, midfielder, goal keeper, forward

defence <- c("CB", "RB", "LB", "LWB", "RWB", "LCB", "RCB")
midfielder <- c("CM", "CDM","CAM","LM","RM", "LAM", "RAM", "LCM", "RCM", "LDM", "RDM")

FIFA19 %<>% mutate(Class = if_else(Position %in% "GK", "Goal Keeper",
                                   if_else(Position %in% defence, "Defender",
                                           if_else(Position %in% midfielder, "Midfielder", "Forward"))))

rm(defence, midfielder)
# [15] "LCB" "RM"  "LAM" "LM"  "LB"  "RDM" "RW"  "CM"  "RB"  "RAM" "CF"  "RWB" "LWB" NA 

# Relationship between position and wage
p<-FIFA19 %>% 
  ggplot(aes(Position, `International Reputation` , color = Class))+
  geom_jitter()+
  theme_minimal()+
  theme(
    legend.position = "top"
  )+
  labs(y = "International Reputation")
fig <- ggplotly(p)
fig
# Make interactive
# Select filter
positionHighlight <- highlight_key(FIFA19)
ggPosition<-ggplot(positionHighlight) + 
  geom_point(mapping = aes(x = Position, `International Reputation`,fill = Class), stat = 'Identity',position = 'dodge') + 
  geom_jitter()+
  theme_minimal()+
  ggtitle('Effect of Position on International Reputation')
selectPosition <- bscols(
  filter_select("id", "Select a Position", positionHighlight, ~Position),
  ggplotly(ggPosition),
)
bscols(selectPosition)