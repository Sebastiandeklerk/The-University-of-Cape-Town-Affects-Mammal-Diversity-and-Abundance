###########################################################################################
# NB Information
###########################################################################################

# This r script will run your code to analyse your data
# Anything written after a hash tag, like this sentence, does not get run (it's just text)
# Multiple hash tags between sentence, like the title above, indicate different sections
# This helps you to keep track of your working
# r works with manipulating your data using functions- a bit like a cheesy hacker film guru

# if you have any questions, either email me (woodgate34@gmail.com) or message the Whatsapp group

###########################################################################################
# Load packages 
###########################################################################################

# First clean your environment

# This basically resets r back to a clean slate
rm(list=ls())

# Load required packages

# r largely works through installed packages
# packages are basically little bits opf instructions given to r that allow us to run different analyses
# These packages get installed, and then  everytime you boot up r again you have to tell r to use the package

# First, install the ones we will be using:
# Run these one time, and once installed you can delete these lines
install.packages("tidyverse") # This makes basic data organizing easier
install.packages("dplyr")     # This makes basic data organizing easier
install.packages("vegan")     # This package contains all the diversity analyses we will be using
install.packages("readxl")    # This package allows us to read in excel files
install.packages("iNEXT")     # This package contains the functions to run species accumulation curves
install.packages("gridExtra") # This package helps make pretty graphs
install.packages("ggplot2")   # This package helps make pretty graphs
install.packages("rstatix")   # This will be used to make statistical comparisons
install.packages("ggpubr")

# Now load the packages
library("tidyverse")
library("dplyr")
library("vegan")
library("readxl")
library("iNEXT")
library("gridExtra")
library("ggplot2")
library("rstatix")
library("ggpubr")

###########################################################################################
# Set workdrive
###########################################################################################

# This tell r where to look for your data
# replace C:\\Users\\User\\Pictures\\Biodiversity_survey with where you have saved it
# Copy the address of the folder to get your drive
# NB NB you need double spaces, otherwise it will not reset the workdrive and give you an error NB NB

# For me (Zoe):
setwd("C:/Users/user/Downloads/BIO2015F Project")

# LEts see if it has the right place
list.files()

###########################################################################################
# Load data
###########################################################################################

# Now that we have set the workdrive, we can tell r to load in the data
# The arrows tell r to save the output of the function/work
data <- read_excel("UCT_Biodiversity_Data.xlsx", sheet = "Export to r")

# look at the data
view(data)
head(data)

# make sure the data resembles what you excel sheet looks like :)

# We need to rename the columns to make life easier later (the gap between the words causes issues)
# We do this using the dplyr package and the rename_all function
data <- data %>% dplyr::rename_all(make.names)

# look at the data
# Each space should be a full stop now
view(data)
head(data)

###########################################################################################
# Overall diversity & sample based species accumulation 
###########################################################################################

# here we will use the iNEXT package
# iNEXt is a very cool package, but it has some strict requirements for how the data must look

# Fist, lets remove the station column from our dataset
overall.data <- data[-c(1)]

# Next, make an empty list to store our data
# A list is another way of storing data
# Lists are very similar to real life flip folders in design

#Lets make a blank list:
overall <- list()

# Now, we must sum all of the observations of each species (using the function colSums), and then insert it in the list
overall[[1]]<- colSums(overall.data[1:8])

# Name this overall, as it is the pooled stations
names(overall) <- "Overall"

# Plug this list into iNEXT
out.overall <- iNEXT(overall, datatype="abundance")

# now look at the output
out.overall
# It's a bit chaotic! But we want to mainly look at $AsyEst 
# $AsvEst lists the observed diversity, asymptotic estimates, estimated bootstrap standard error and 95% confidence intervals for 
# Hill numbers 
# We are not focused on these values for now.

# You may want to see if your camera project has sufficient survey effort to capture all species within the study site. 
# To do this we can produce sample based species accumulation curves across the site as a whole.
# Species accumulation curves plot the increase in species richness as we add individuals or survey units.
# If the curve plataues, flattens, then that suggests you have sampled the majority of the species in your survey area.
p1 <- ggiNEXT(out.overall, type=1)+ theme_classic() 
p2 <- ggiNEXT(out.overall, type=2)+ theme_classic() 
ggarrange(p1, p2, nrow = 1, labels = c("a)","b)"))
# The left plot a) is the Sample-size-based rarefaction (solid line segment) 
# and extrapolation (dotted line segments) sampling curves with 95% confidence intervals (shaded areas)

# The right plot b) is the sample completeness curve
# this plot suggests that we have likely sampled all the species that will be detected,
# however the left plot has yet to 

###########################################################################################
# Land use specific sample based species accumulation
###########################################################################################

# What about looking at each land use?
# Lets create two datasets
# Each dataset is the overall.data split into 2 based on if the camera is inside or outside UCT

#TMNP
TMNP.data <-  dplyr::select((data  %>%
                               filter(Land.use == "TMNP")), -c(1,10))
#UCT
UCT.data  <-  dplyr::select((data  %>%
                               filter(Land.use == "UCT")), -c(1,10))

# As above, lets create a new empty list
# Lets name this one land.type
land.type <- list()

# Only sum the data for each relvent strata
land.type[[1]] <- colSums(TMNP.data)
land.type[[2]] <- colSums(UCT.data)

# Give them names which corrospond to the land use type
names(land.type) <- c("TMNP", "UCT")

# Again, like before, plug into the iNEXT function
out.land.type <- iNEXT(land.type, datatype="abundance")

# Lets look at the results
out.land.type 
# Again don't spoend too much focus on this
# it's going to be used in the plotting mainly

# once more lets look at some plots
# these are the same as prior, just spit between the land use types
p1 <- ggiNEXT(out.land.type , type=1)+ theme_classic() 
p2 <- ggiNEXT(out.land.type , type=2)+ theme_classic() 
ggarrange(p1, p2, nrow = 1, labels = c("a)","b)"))
# What do you notice? And why do you think it is?

# One issue with species richness assessments is that they weight all species equally
# ie rare species hold incredibly sway over results
#  Consequently, you might want to estimate species diversity.

###########################################################################################
# Diversity measures (between land uses)
###########################################################################################

# First lets prep the data
# Sum all species events by land use type
# We need to use dplyr again for this
diversity.overall <- data %>%
  group_by(Land.use) %>%
  dplyr::summarise_each(funs(sum))
# ignore the warnings, rather look at your list

view(diversity.overall)
# Ignore the first two columns
# What patterns do you see? ETC what are caracal doing?
# What descriptive statements can you make?

# Now lets calculate some bstandard diversity indicies for each land use type
#keep a separate list for the name of the land use type
land.use.overall <- select(diversity.overall, c(1))
#Remove the station & land use columns
diversity.overall<- select(diversity.overall, -c(1,2))

# Lets calculate some diversity!

# Observed Station specific species richness 
richness.overall <- specnumber(diversity.overall)

# Station specific Shannon diversity (H)
shannon.overall <- diversity(diversity.overall,index="shannon")

# Station specific Shannon's equitability (EH)
even.overall <- shannon.overall/log(richness.overall)

# Station specific effective number of species (ENS)
ens.overall <- exp(shannon.overall)

# bind these values together along with the land use names
# this will allow us to compare the results between land use types
indicies.overall <- as.data.frame(cbind(land.use.overall ,richness.overall,shannon.overall,even.overall,ens.overall))
indicies.overall[is.na(indicies.overall)] <- 0

# Lets look at the result
indicies.overall 

#These values can be put into a table

###########################################################################################
# Diversity measures (station-specific)
###########################################################################################

# lets repeat this exercise, but looking at some station specific values of diversity
# These are camera trap specific

# Rename the data for the diversity analysis
diversity <- data 
#keep a separate list for the name of the land use type
land.use <- select(diversity, c(10))
#Remove the station & land use columns
diversity <- select(diversity, -c(1,10))

# Lets calculate some diversity!

# Observed Station specific species richness 
richness <- specnumber(diversity)

# Station specific Shannon diversity (H)
shannon <- diversity(diversity,index="shannon")

# Station specific Shannon's equitability (EH)
even <- shannon/log(richness)

# Station specific effective number of species (ENS)
ens <- exp(shannon)

# bind these values together along with the land use names
# this will allow us to compare the results between land use types
indicies <- as.data.frame(cbind(land.use ,richness,shannon,even,ens))
indicies[is.na(indicies)] <- 0

# Observed Station specific species richness
richness_matrix <- indicies %>%
  select(Land.use,richness) %>%
  group_by(Land.use) %>%
  summarise(N = length(richness), 
            mean = mean(richness), 
            sd = sd(richness),
            min = min(richness),
            max = max(richness), 
            se = sd/sqrt(N))

# Look at the result
richness_matrix

# Station specific Shannon diversity (H)
diversity_matrix <- indicies %>%
  select(Land.use ,shannon) %>%
  group_by(Land.use ) %>%
  summarise(N = length(shannon), 
            mean = mean(shannon), 
            sd = sd(shannon),
            min = min(shannon),
            max = max(shannon), 
            se = sd/sqrt(N))

# Look at the result
diversity_matrix

# Station specific Shannon's equitability (EH)
even_matrix <- indicies %>%
  select(Land.use ,even) %>%
  group_by(Land.use ) %>%
  summarise(N = length(even), 
            mean = mean(even), 
            sd = sd(even),
            min = min(even),
            max = max(even), 
            se = sd/sqrt(N))

# Look at the result
even_matrix


# Station specific effective number of species (ENS)
ens_matrix <- indicies %>%
  select(Land.use ,ens) %>%
  group_by(Land.use ) %>%
  summarise(N = length(ens), 
            mean = mean(ens), 
            sd = sd(ens),
            min = min(ens),
            max = max(ens), 
            se = sd/sqrt(N))

# Look at the result
ens_matrix

###########################################################################################
# Testing significance
###########################################################################################
# Lets see if station-specific effective number of species significantly differ?

# We first run a kruskal wallis test
# nonparametric approach to the one-way ANOVA

# kruskal wallace test
# looking at statiion specific ENS between land uses
res.kruskal <- indicies  %>% kruskal_test(ens ~ Land.use )
summary(res.kruskal)

# is it significantly different?
# Lets do a post-hoc wilcox test

wilcox <- indicies   %>%
  wilcox_test(ens ~ Land.use , paired=F, conf.level = 0.95, p.adjust.method = "bonferroni")
wilcox

# plot the results as part of box plot
# if it is significantly different it will be shown on the plot
box <- wilcox  %>% add_xy_position(x = "Land.use ")
ggboxplot(indicies, x = "Land.use", y = "ens",order = c("TMNP", "UCT"),  
          ylab = expression(paste("Station-specific effective number of species (ENS)")), xlab = "Land use",
          color = "black ", fill = "gray") +
  stat_pvalue_manual(box , hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal),
    caption = get_pwc_label(wilcox)
  )
# What does this show?


