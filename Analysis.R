##this R script is analysing data collected on Dwarf Mongooses
#comparing pup responses to sentinel calls vs close calls 
#in terms of nearest neighbour distance (NND) and vigilence behaviour
##To cite packages used in publications use -> citation("package_name") <- to get a 
#citation + a BibTex enrty for LaTeX
#Wes Anderson themes colour palettes: names(wes_palettes)

##load libraries
library(tidyverse)
library("dplyr")  
library(ggplot2)
library(ggpubr)
library(wesanderson) #for colour palettes 

# wes anderson themed colour palettes#####
#print all wes anderson themed palettes in the wesanderson library
palettes <- c("BottleRocket1", "BottleRocket2", "Rushmore1", "Royal1", "Royal2", "Zissou1", "Darjeeling1", "Darjeeling2", "Chevalier1", "FantasticFox1", "Moonrise1", "Moonrise2", "Moonrise3", "Cavalcanti1", "GrandBudapest1", "GrandBudapest2", "IsleofDogs1", "IsleofDogs2", "FrenchDispatch", "AsteroidCity2", "AsteroidCity2", "AsteroidCity3")

for (palette in palettes) {
  print(wes_palette(c(palette)))
}

#to choose specific colours of a palette, either call the colour names directly or generate your own palette only with specific colours
# Generate the AsteroidCity1 and 3 palette
asteroid_city1_palette <- wes_palette("AsteroidCity1")
asteroid_city3_palette <- wes_palette("AsteroidCity3")

# Select the third and fourth colour of AsteroidCity3
selected_colours2 <- asteroid_city3_palette[c(3, 4)]

# to use the first two colours in a wespalette with ggplot use line "scale_fill_manual(values=wes_palette(n=2, name="Chevalier1"),"
# to use colours chosen above in ggplot use line "scale_fill_manual((values = selected_colours6),"
# for baseR plots use "col =  selected_colours9"

# ####

##import data
nnd <- read.csv(file = "data/NND_DATA.csv", stringsAsFactors = FALSE)
vigi <- read.csv(file = "data/VIGI_DATA.csv", stringsAsFactors = FALSE)

#data frame for first move

Movement <- c("Closer", "Away")
Sentinel_Call <- c(3, 6)
Close_Call <- c(6, 3)
df <- data.frame(Movement, Sentinel_Call, Close_Call)

barplot(cbind(df$`Sentinel_Call`, df$`Close_Call`) ~ df$Movement,
        beside = TRUE,
        main="First movement response to call type",
        xlab = "First movement response from nearest neighbour to call type",
        ylab = "Number of occurences",
        #col = wes_palette(n=2, name="Chevalier1"),
        col =  selected_colours9, #fill colours with wes anderson palette
        legend.text = c("Close Call", "Sentinel Call"),
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

##analyse normalility visually with Q-Q plots and densitiy plots ####
#Q-Q plots
a <- ggqqplot(nnd$PER_MIN)
b <- ggqqplot(nnd$AV_DIST)
c <- ggqqplot(vigi$REL_TIME_VIGI)
d <- ggqqplot(vigi$RATE_LOOKUPS)
e <- ggqqplot(vigi$MEAN_DUR)

#arrange plots into 2 figures
ggarrange(a, b, c, d, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
ggqqplot(vigi$MEAN_DUR)

#density plots
A <- ggdensity(nnd$PER_MIN, 
          main = "Density plot",
          xlab = "Mean NND")
B <- ggdensity(nnd$AV_DIST, 
          main = "Density plot",
          xlab = "Mean NND in first min")
C <- ggdensity(vigi$REL_TIME_VIGI, 
          main = "Density plot",
          xlab = "Relative time spent vigilent")
D <- ggdensity(vigi$RATE_LOOKUPS, 
          main = "Density plot",
          xlab = "Lookup rate")
E <- ggdensity(vigi$MEAN_DUR, 
          main = "Density plot",
          xlab = "Mean duration of vigilent bout")

#arrange plots into 2 figures
ggarrange(A, B, C, D,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
ggdensity(vigi$MEAN_DUR, 
          main = "Density plot",
          xlab = "Mean duration of vigilent bout")

##analyse normality using Shapiro Wilk test 
#for this study it is better to stick to visual observation because formal tests are overly sensitive
#The null hypothesis of these tests is that “sample distribution is normal”. 
#If the test is significant, the distribution is non-normal.
shapiro.test(nnd$PER_MIN)
shapiro.test(vigi$REL_TIME_VIGI)

#From the output, the p-value > 0.05 implying that the distribution of the data are not 
#significantly different from normal distribution. In other words, we can assume the normality.
shapiro.test(nnd$AV_DIST)
shapiro.test(vigi$RATE_LOOKUPS)
shapiro.test(vigi$MEAN_DUR)

#boxplots

#for fitting several plots in one pic
#par(mfrow=c(2,3)) 

#plots

p<-ggplot(nnd, aes(x=AV_DIST,y=CALL_TYPE)) +
  geom_boxplot(notch = FALSE)+
  geom_point()+
  geom_line(aes(group=in_group_id))
p

axislabels <- c("Close Call", "Sentinel Call")
boxplot(AV_DIST~CALL_TYPE,data=nnd, 
        main="Mean nearest neighbour distance",
        ylab="Mean distance [m]", 
        xlab="Call type", 
        col=wes_palette(n=2, name="Chevalier1"),
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
        names=axislabels)

boxplot(PER_MIN~CALL_TYPE,data=nnd,
        main="Mean nearest neighbour distance in first minute",
        ylab="Mean distance [m]", 
        xlab="Call type", 
        col=wes_palette(n=2, name="Chevalier1"),
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
        names=axislabels)

boxplot(REL_TIME_VIGI~CALL_TYPE,data=vigi,
        main="Relative time spent vigilent",
        ylab="Relative time [s/s]", 
        xlab="Call type", 
        col=wes_palette(n=2, name="Chevalier1"),
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
        names=axislabels)

boxplot(RATE_LOOKUPS~CALL_TYPE,data=vigi,
        main="Vigilence rate",
        ylab="Mean rate [nr./s]", 
        xlab="Call type", 
        col=wes_palette(n=2, name="Chevalier1"),
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
        names=axislabels)

boxplot(MEAN_DUR~CALL_TYPE,data=vigi,
        main="Mean vigilence bout duration",
        ylab="Mean duration [s]", 
        xlab="Call type", 
        col=wes_palette(n=2, name="Chevalier1"),
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
        names=axislabels)

boxplot(FIRST_HEADUP~CALL_TYPE,data=vigi,
        main="First vigilance bout after start of experiment",
        ylab="Average start time [s]", 
        xlab="Call type", 
        col=wes_palette(n=2, name="Chevalier1"),
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
        names=axislabels)
par(mfrow=c(1,1))

# ####

############data analysis ####
#paired t-test for normally distributed data, wilcoxon for non-normally distributed data
#as seen using shapiro wilk test

##data and analysis for NND
#subset data for sentinel and close calls, nnd (to analyse with paired tests)
nnd_s <- nnd %>% filter(CALL_TYPE=='SENTINEL')
nnd_c <- nnd %>% filter(CALL_TYPE=='CLOSE')

#wilcoxon for relative NND 
mean_dist_s <-  as.vector(nnd_s$AV_DIST)
mean_dist_c <-  as.vector(nnd_c$AV_DIST)

t.test(mean_dist_s, mean_dist_c, paired = TRUE, alternative = "two.sided")
wilcox.test(mean_dist_s, mean_dist_c, alternative = "two.sided")

#wilcoxon for relative NND in first minute
mean_dist_1min_s <-  as.vector(nnd_s$PER_MIN)
mean_dist_1min_c <-  as.vector(nnd_c$PER_MIN)

wilcox.test(mean_dist_1min_s, mean_dist_1min_c, alternative = "two.sided")

# McNemar's chi^2 for first move
m <- matrix(c(6, 3, 3, 6), nrow = 2, dimnames = list(c("SENTINEL","CLOSE"), c("Away","Closer")))
print(m)

mcnemar.test(m)

##data and analysis for vigilence
#subset data for sentinel and close calls, vigilence data
vigi_s <- vigi %>% filter(CALL_TYPE=='SENTINEL')
vigi_c <- vigi %>% filter(CALL_TYPE=='CLOSE')

#wilcoxon for time spent vigilent (total and relative) -> I think only relative makes sense here
time_vigi_s <-  as.vector(vigi_s$TOTAL_TIME_VIGILENT)
time_vigi_c <-  as.vector(vigi_c$TOTAL_TIME_VIGILENT)
rel_time_vigi_s <-  as.vector(vigi_s$REL_TIME_VIGI)
rel_time_vigi_c <-  as.vector(vigi_c$REL_TIME_VIGI)

wilcox.test(time_vigi_s, time_vigi_c, alternative = "two.sided")
wilcox.test(rel_time_vigi_s, rel_time_vigi_c, alternative = "two.sided")

#wilcoxon for look up rate
lookup_rate_s <-  as.vector(vigi_s$RATE_LOOKUPS)
lookup_rate_c <-  as.vector(vigi_c$RATE_LOOKUPS)

t.test(lookup_rate_s, lookup_rate_c, paired = TRUE, alternative = "two.sided")
wilcox.test(lookup_rate_s, lookup_rate_c, alternative = "two.sided")

#wilcoxon for mean bout duration
boutdur_s <-  as.vector(vigi_s$MEAN_DUR)
boutdur_c <-  as.vector(vigi_c$MEAN_DUR)

t.test(boutdur_s, boutdur_c, paired = TRUE, alternative = "two.sided")
wilcox.test(boutdur_s, boutdur_c, alternative = "two.sided")

#wilcoxon for first head up
firstheadup_s <-  as.vector(vigi_s$FIRST_HEADUP)
firstheadup_c <-  as.vector(vigi_c$FIRST_HEADUP)

wilcox.test(firstheadup_s, firstheadup_c, alternative = "two.sided")

# ####


################################################### new plots with lines between ids

nnd2 <- nnd %>% #subset nnd data to create lines between IDs
  group_by(CALL_TYPE)  %>% 
  arrange(CALL_TYPE) %>% mutate(in_group_id=row_number()) # this adds a column to identidy the same IDs

vigi2 <- vigi %>% #subset nnd data to create lines between IDs
  group_by(CALL_TYPE)  %>% 
  arrange(CALL_TYPE) %>% mutate(in_group_id=row_number()) # this adds a column to identidy the same IDs

p1 <- ggplot(nnd2, aes(x=CALL_TYPE,y=AV_DIST, fill=CALL_TYPE)) +
  geom_boxplot(notch = FALSE)+
  geom_point()+
  geom_line(aes(group=in_group_id)) +
  #ggtitle("Mean nearest neighbour distance") +
  ylab("Mean distance [m]") +
  xlab("Call type") + 
  scale_fill_manual(values=wes_palette(n=2, name="AsteroidCity1"), breaks=c("Close Call", "Sentinel Call"), labels=c("Close Call", "Sentinel Call")) + #fill colours with wes anderson palette
  theme(axis.text=element_text(size=13, colour = 'black'), axis.title=element_text(size=15, colour = 'black'), plot.title=element_text(size=15, colour = 'black')) +   #Adjust axis label size  guides(fill='none')     
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(fill='none') +    #remove legend
  scale_x_discrete(labels=c("Close Call", "Sentinel Call"))

p2 <- ggplot(nnd2, aes(x=CALL_TYPE,y=PER_MIN, fill=CALL_TYPE)) +
  geom_boxplot(notch = FALSE)+
  geom_point()+
  geom_line(aes(group=in_group_id)) +
  #ggtitle("Mean nearest neighbour distance in first minute") +
  ylab("Mean distance [m]") +
  xlab("Call type") + 
  scale_fill_manual(values=wes_palette(n=2, name="AsteroidCity1"), breaks=c("Close Call", "Sentinel Call"), labels=c("Close Call", "Sentinel Call")) + #fill colours with wes anderson palette
  theme(axis.text=element_text(size=13, colour = 'black'), axis.title=element_text(size=15, colour = 'black'), plot.title=element_text(size=15, colour = 'black')) +   #Adjust axis label size  guides(fill='none')     
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(fill='none') +    #remove legend
  scale_x_discrete(labels=c("Close Call", "Sentinel Call"))

p5 <- ggplot(vigi2, aes(x=CALL_TYPE,y=REL_TIME_VIGI, fill=CALL_TYPE)) +
  geom_boxplot(notch = FALSE)+
  geom_point()+
  geom_line(aes(group=in_group_id)) +
  #ggtitle("Relative time spent vigilent") +
  ylab("Proportion of time") +
  xlab("Call type") + 
  scale_fill_manual(values=wes_palette(n=2, name="AsteroidCity1"), breaks=c("Close Call", "Sentinel Call"), labels=c("Close Call", "Sentinel Call")) + #fill colours with wes anderson palette
  theme(axis.text=element_text(size=13, colour = 'black'), axis.title=element_text(size=15, colour = 'black'), plot.title=element_text(size=15, colour = 'black')) +   #Adjust axis label size  guides(fill='none')     
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(fill='none') +    #remove legend
  scale_x_discrete(labels=c("Close Call", "Sentinel Call"))

p4 <- ggplot(vigi2, aes(x=CALL_TYPE,y=RATE_LOOKUPS, fill=CALL_TYPE)) +
  geom_boxplot(notch = FALSE)+
  geom_point()+
  geom_line(aes(group=in_group_id)) +
 # ggtitle("Vigilence rate") +
  ylab("Mean rate [nr./s]") +
  xlab("Call type") + 
  scale_fill_manual(values=wes_palette(n=2, name="AsteroidCity1"), breaks=c("Close Call", "Sentinel Call"), labels=c("Close Call", "Sentinel Call")) + #fill colours with wes anderson palette
  theme(axis.text=element_text(size=13, colour = 'black'), axis.title=element_text(size=15, colour = 'black'), plot.title=element_text(size=15, colour = 'black')) +   #Adjust axis label size  guides(fill='none')     
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(fill='none') +    #remove legend
  scale_x_discrete(labels=c("Close Call", "Sentinel Call"))

p6 <- ggplot(vigi2, aes(x=CALL_TYPE,y=MEAN_DUR, fill=CALL_TYPE)) +
  geom_boxplot(notch = FALSE)+
  geom_point()+
  geom_line(aes(group=in_group_id)) +
 # ggtitle("Vigilence bout duration") +
  ylab("Mean duration [s]") +
  xlab("Call type") + 
  scale_fill_manual(values=wes_palette(n=2, name="AsteroidCity1"), breaks=c("Close Call", "Sentinel Call"), labels=c("Close Call", "Sentinel Call")) + #fill colours with wes anderson palette
  theme(axis.text=element_text(size=13, colour = 'black'), axis.title=element_text(size=15, colour = 'black'), plot.title=element_text(size=15, colour = 'black')) +   #Adjust axis label size  guides(fill='none')     
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(fill='none') +    #remove legend
  scale_x_discrete(labels=c("Close Call", "Sentinel Call"))

p7 <- ggplot(vigi2, aes(x=CALL_TYPE,y=FIRST_HEADUP, fill=CALL_TYPE)) +
  geom_boxplot(notch = FALSE)+
  geom_point()+
  geom_line(aes(group=in_group_id)) +
 # ggtitle("First vigilance bout after start of experiment") +
  ylab("Mean start time [s]") +
  xlab("Call type") + 
  scale_fill_manual(values=wes_palette(n=2, name="AsteroidCity1"), breaks=c("Close Call", "Sentinel Call"), labels=c("Close Call", "Sentinel Call")) + #fill colours with wes anderson palette
  theme(axis.text=element_text(size=13, colour = 'black'), axis.title=element_text(size=15, colour = 'black'), plot.title=element_text(size=15, colour = 'black')) +   #Adjust axis label size  guides(fill='none')     
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(fill='none') +    #remove legend
  scale_x_discrete(labels=c("Close Call", "Sentinel Call"))

#arrange plots in one figure with several panels + plot 2

ggarrange(p1, p2,
          labels = c("A", "B"),
          ncol = 2, nrow = 1) +
  xlab("Call type")

#new dataframe for first movement response so that call type is on x axis
Call_names <- c("Sentinel Call", "Close Call")

Call_type <- c("Sentinel_Call", "Close_Call")
Closer <- c(3, 6)
Away <- c(6, 3)
df2 <- data.frame(Call_type, Away, Closer)

barplot(cbind(df2$`Closer`, df2$`Away`) ~ df2$Call_type,
        names.arg = Call_names,
        beside = TRUE,
        #main="First movement response to call type",
        xlab = "First movement response to call type",
        ylab = "Number of occurences",
        col =  selected_colours2,
        legend.text = c("Closer", "Away"),
        cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.names=1.3)

ggarrange(p4, p5,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

ggarrange(p6, p7,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

