library(ggplot2)
library(zoo)
library(stringr)
library(scales)
library(ggridges)

# reads first 2500 rows of data
dataset <- read.csv(file="/Users/mertyenilmez/Desktop/eCheckbook_Commodities_Others_paymentsnew4444 - Copy comma seperated.csv", header=TRUE, sep=",", nrow=2500)
dataset <- na.omit(dataset) # deals with NA values

### 1. image: Fund Amounts Based on Date - Linear Regression ###

# creates a single data value with combine year and month columns  
DATE <- as.yearmon(as.character(paste(str_sub(dataset$CAL_YEAR, start= -2), dataset$CAL_MONTH, sep="")), "%y%m")

# turns not formatted numbers into currency format
FUND <- as.numeric(gsub(",", ".", gsub("\\.", "", dataset$AMOUNT)))

# linear regression
ggplot(dataset, aes(x= DATE, y=FUND, color=FUND, size=FUND)) + 
  geom_point(shape="$")+ geom_smooth(method=lm) + # turns dots into dollar signs
  labs(title="Fund Amounts Based on Date") +  # title of image
  scale_colour_gradient(low = "#70ad4c", high = "#213216",) + # dollar green colour range
  scale_x_continuous(breaks = 2008:2020) + # x-axis sections on graph
  guides(size = FALSE) + # hides size legend
  theme_bw() + # theme
  xlab("Years") + # x-axis label
  ylab("Fund Amount (US$)") + # y-axis label
  theme(legend.background = element_rect(fill="#CBA580")) # changes legend background color


### 2. image: Last 5 years Miscellaneous Fund Amounts of Austin Energy and Austin Water Departments ###

# reads data
dataset <- read.csv(file="/Users/mertyenilmez/Desktop/eCheckbook_Commodities_Others_paymentsnew4444 - Copy comma seperated.csv", header=TRUE, sep=",")

# restrict data with chosen parameters
dataset <- subset(dataset,DEPT_NM=='Austin Water' | DEPT_NM== 'Austin Energy')
dataset <- subset(dataset,DIV_NM=='Miscellaneous')
dataset <- subset(dataset,CAL_YEAR>'2017')

FUND <- as.numeric(gsub(",", ".", gsub("\\.", "", dataset$AMOUNT)))

programName <- dataset$DIV_NM

# box plot
ggplot(dataset, aes(x=dataset$DEPT_NM, y=FUND)) +
  geom_point(aes(fill=programName), size=1, shape=21, colour="grey15",
             position=position_jitter(width=0.3, height=0.1)) +
  geom_boxplot(outlier.colour=NA, fill=NA, colour="grey15") +
  labs(title="Last 5 Years Miscellaneous Fund Amounts of Austin Energy and Austin Water Departments") + 
  xlab("Departments") +
  ylab("Fund Amount (US$)") + 
  theme(legend.position="bottom") + # arranges legend positions
  theme(legend.background = element_rect(fill="#d0e5c4"))


### 3. image: Total Fund Distributions of 4 Departments - Ridgeline Plot ###

# reads data
dataset <- read.csv(file="/Users/mertyenilmez/Desktop/eCheckbook_Commodities_Others_paymentsnew4444 - Copy comma seperated.csv", header=TRUE, sep=",")

dataset <- subset(dataset,LGL_NM=='DELL MARKETING LP' | LGL_NM== 'MOTOROLA SOLUTIONS INC'| LGL_NM== 'HAMILTON ELECTRIC WORKS INC' | LGL_NM== 'TEXAS ELECTRIC COOPERATIVES')

FUND <- as.numeric(gsub(",", ".", gsub("\\.", "", dataset$AMOUNT)))

# ridgeline plot
ggplot(dataset, aes(x = FUND, y = dataset$LGL_NM, fill = dataset$LGL_NM)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") +
  xlab("Fund Amount (US$)") +
  ylab("Departments") +
  labs(title="Fund Distributions of 4 Departments")


### 4. image: Fund Distributions of DELL MARKETING LP's Object Names - Bar Plot ###

dataset <- read.csv(file="/Users/mertyenilmez/Desktop/eCheckbook_Commodities_Others_paymentsnew4444 - Copy comma seperated.csv", header=TRUE, sep=",")
dataset <- subset(dataset,LGL_NM=='DELL MARKETING LP')
ObjectNames <- dataset$OBJ_NM

FUND <- as.numeric(gsub(",", ".", gsub("\\.", "", dataset$AMOUNT)))

# bar plot
p<-ggplot(data=dataset, aes(x=ObjectNames, y=FUND, fill=ObjectNames)) +
  geom_bar(stat="identity", position = 'dodge') + 
  coord_flip() + # makes bar plot horizontal
  theme_minimal() +
  xlab("Fund Amount (US$)") +
  ylab("Object Names") +
  labs(title="Fund Distributions of DELL MARKETING LP's Object Names") +
  scale_fill_brewer(palette="Paired") + # uses Paired color palette
  theme(legend.position="bottom")
p



