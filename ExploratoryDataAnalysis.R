##################################################
### Exploratory Data Analysis                   ##

##################################################
# Written by Bala Anthony Xavier Pasala

##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("D:/DataAnalyis/EDA")

##################################################
### Install Libraries                           ##
##################################################

#Install dplyr and lattice packages

if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")

if(!require(lattice)){install.packages("lattice")}
library("lattice")

##################################################
### Read in Data from study file                ##
##################################################

StudyFile_BP <- read.csv("AS01.csv", header = TRUE, sep = ",")
colnames(StudyFile_BP)[2] <- "EDN_BP"
colnames(StudyFile_BP)[4] <- "Prov_BP"
colnames(StudyFile_BP)[5] <- "Electors_BP"
colnames(StudyFile_BP)[14] <- "LPV_BP"
colnames(StudyFile_BP)[22] <- "Male_BP"
colnames(StudyFile_BP)[23] <- "Female_BP"
colnames(StudyFile_BP)[29] <- "Own_fin_BP"
colnames(StudyFile_BP)[30] <- "LiB_BP"
colnames(StudyFile_BP)[34] <- "Trude_BP"
colnames(StudyFile_BP)[40] <- "WinParty_BP"
str(StudyFile_BP)


##################################################
###   1. Summarizing data                       ##
##################################################
# 1.1 Total votes for liberal party in each province
Table1_BP <- aggregate(StudyFile_BP$LPV_BP, by=list(StudyFile_BP$Prov_BP), FUN=sum, na.rm=TRUE)
Table1_BP

#1.2 Weighted mean for feelings on Trudeau(leaders_2 a.k.a Tru) in Manitoba with weight
# as number of electors

ProvFeelTru_BP <- StudyFile_BP %>%
  select(EDN_BP,Electors_BP,Prov_BP,Trude_BP)  %>% 
  group_by(Prov_BP) %>%
  summarise(wFeelTru_BP = weighted.mean(Trude_BP,w=Electors_BP))
wMeanTruMB_BP <- ProvFeelTru_BP[ProvFeelTru_BP$Prov_BP == "MB", c("wFeelTru_BP")]
wMeanTruMB_BP

# 1.3 What is the standard deviation of the answer to the question "And
# what about the performance of the economy during the past four
# years? Has it improved, stayed the same, or got worse?" for electoral
# districts that were won by your assigned political party

#Electoral districts won by liberal party
EdnLibWon_BP <- StudyFile_BP[StudyFile_BP$WinParty_BP == "LIB", c("EDN_BP", "WinParty_BP", "ecc_sat")]
names(EdnLibWon_BP)[3]<- "EcnmyPerf_BP"
# head(EdnLibWon_BP)
# tail(EdnLibWon_BP)
#calculate standard deviation ignoring NA's.
EdnLibWonEcoPerfSd_BP = sd(EdnLibWon_BP$EcnmyPerf_BP, na.rm = TRUE)
EdnLibWonEcoPerfSd_BP

# 1.4 The study file has total number of females and total males answering
# the LPP2015 in each electoral district. What is the 68th percentile
# (quantile) of number of females across your study file?

FemQuntl_68_pc_BP <- quantile(StudyFile_BP$Female_BP,c(.68), na.rm = TRUE)
FemQuntl_68_pc_BP

# 1.5 What is the mean absolute deviation of turnout for electoral districts
# in Alberta?

#Collect TurnOut data that belongs to Alberta
TurnOutStudyAlberta_BP <- StudyFile_BP[StudyFile_BP$Prov_BP == "AB",c("EDN_BP","Prov_BP","TO")]
colnames(TurnOutStudyAlberta_BP)[3]<- "TurnOut_BP"
#Calculate mean absolute deviation ignoring NA's
MadTOAlb_BP <- mad(TurnOutStudyAlberta_BP$TurnOut_BP, na.rm = TRUE)
MadTOAlb_BP

# 2.1. Summary table
# a. Create a table that shows Number of Electoral Districts
# (Ridings) won by each party (columns) in each province
# (rows). Each cell in the table should be the percentage of each
# row.


Table_EDN_Party_BP <- table(StudyFile_BP$Prov_BP, StudyFile_BP$WinParty_BP)
Table_EDN_Party_BP
PropTable_BP<-prop.table(Table_EDN_Party_BP,1)
PropTable_BP

# 2.1.b. In what province did liberal party win the highest percentage of
# ridings?
colnames(PropTable_BP)[4] <- "LIB_BP"
PropTable_BP[,"LIB_BP"]


# 2.2 Bar Chart
# a. Create a bar chart showing the number of Males and Females
# by which party won the electoral district. NOTE - Exclude the
# three territories from this analysis (Nunavut, Northwest
# Territories and Yukon). 

str(StudyFile_BP)
unique(StudyFile_BP$Prov_BP)
NewStudyFile_BP <- StudyFile_BP[!(StudyFile_BP$Prov_BP %in% c("NU", "NT","YT")), ]

barchart(WinParty_BP ~ Male_BP + Female_BP,
         data=NewStudyFile_BP, beside=TRUE, main="Gender Breakdown by Winning party_BP",
         xlab="Gender_BP", ylab="Win Party_BP", 
         auto.key=list(space='bottom'))

#2.3 Histogram
# Create a histogram showing the distribution of the answer to the question "How do you feel about the party leaders?" 
# according to the LPP2015. The leader here considered is Trudeau. 
histogram( ~ Trude_BP, dat=StudyFile_BP, breaks=10, type = "count",
           main="Trudeau Count of Support_BP")
histogram( ~ Trude_BP, dat=StudyFile_BP, breaks=20, type = "count",
           main="Trudeau Count of Support_BP")

#2.4 Box Chart
# Create a sequence of box plots showing the distribution of the answer to the question "How do you feel about the political parties?" 
# for liberal party divided by Electoral Districts won by each party. NOTE - Exclude the three territories from this analysis (Nunavut, Northwest Territories and Yukon) 

bwplot(NewStudyFile_BP$LiB_BP ~ NewStudyFile_BP$WinParty_BP, data=StudyFile_BP, 
       main="Distribution of Liberal Party Support by WinningParty_BP",
       xlab="Winning Party_BP",  pch = '|')

#2.5 Scatter Plots
# a.Create a histogram for the answer to the question: "How do you feel about the 
# political parties?" for liberal party
histogram( ~ LiB_BP, dat=StudyFile_BP, breaks=20, type = "count",
           main="Support for Liberal Party_BP")
# b.Create a histogram for the answer to the question: "How do
# you feel about the party leaders?" for Trudeau
histogram( ~ Trude_BP, dat=StudyFile_BP, breaks=20, type = "count",
           main="Count of Trudeau Support_BP")
# c. Create a scatter plot showing the relationship between the
# answer to the question: "How do you feel about the political
# parties?" for liberal party and "How do you feel about
# the party leaders?" for Trudeau.
xyplot(LiB_BP ~ Trude_BP, data=StudyFile_BP, color="violet", pch=20,
       main="Liberal Party by Trudeau Support_BP")

