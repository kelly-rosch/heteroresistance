
rm(list = ls()) # clear out previous data 
dev.off()       # close all previous plots
cat("\014")     # clear console of previous output

library("eeptools")
library("tidyr")
library("dplyr")
library("MetBrewer")

#------------------------------------------------------------

setwd("/Users/me/Documents/Doerr Lab/Andy manuscript/R data")
load("AllClists.RData")

#------------------------------------------------------------
# making a column with the full strain names so they appear this way on the X axis
data$full_strain_name <- with(data, ifelse(strain == 'WT', 'WT','Δ3'))


#-----------------------------------------------------------
# getting a sense of the data here


# How many cells in each strain/concentration/position?
table(data$strain, data$drug_conc)
table(data$strain, data$position)

#How many cells in each concentration/position for d3?
d3 <- filter(data, strain == "d3")
table(d3$drug_conc, d3$position)

#How many cells in each concentration/position for WT?
WT <- filter(data, strain == "WT")
table(WT$drug_conc, WT$position)

# what percentage of cells divided over the entire course of the TL?
# "margin = 1" makes it so that each row adds up to 1
# "margin = 2" would make each column add up to 1
prop.table(table(data$strain, data$division), margin = 1)
# this shows that most cells didn't divide
# probably biased by all the cells born later in the TL

# what percentage of the initial population divided?
Initial_pop <- filter(data, birthframe == 1)
prop.table(table(Initial_pop$strain, Initial_pop$division), margin = 1)
# this shows that most of d3 divided
# probably just because there are so few d3 cells that divided at high conc

# what percentage of cells divided, grouped by drug concentration?
d3_initial_pop <- filter(d3, birthframe == 1)
d3_initial_pop_division <- as.data.frame(prop.table(table(d3_initial_pop$drug_conc, d3_initial_pop$division), margin = 1))
colnames(d3_initial_pop_division)[1] <- 'drug_conc'
colnames(d3_initial_pop_division)[2] <- 'division'
colnames(d3_initial_pop_division)[3] <- 'proportion'
d3_initial_pop_division$strain <- "Δ3"

WT_initial_pop <- filter(WT, birthframe == 1)
WT_initial_pop_division <- as.data.frame(prop.table(table(WT_initial_pop$drug_conc, WT_initial_pop$division), margin = 1))
colnames(WT_initial_pop_division)[1] <- 'drug_conc'
colnames(WT_initial_pop_division)[2] <- 'division'
colnames(WT_initial_pop_division)[3] <- 'proportion'
WT_initial_pop_division$strain <- "WT"

initial_pop_division <- rbind(d3_initial_pop_division, WT_initial_pop_division)


#-------------------------------------------------------------
# graphing proportion of initial population that divided
# grouping forst by drug concentration and then by strain
ggplot(initial_pop_division, aes(x = strain, y = proportion, fill = division)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~drug_conc, strip.position = "bottom", ncol = 4) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  scale_fill_manual(values = met.brewer("Juarez", 2)) +
  ylab("Proportion of dividing cells") +
  guides(color = guide_legend(title = "Division observed"))


# grouping first by strain and then by drug concentration
ggplot(initial_pop_division, aes(x = drug_conc, y = proportion, fill = division)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~strain, strip.position = "bottom", ncol = 4) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  scale_fill_manual(values = met.brewer("Juarez", 2)) +
  ylab("Proportion of dividing cells") +
  xlab("Drug concentration (ug/mL)")


# -------------------------------------------------------------
# SuperSegger claims that some cells divided at concentration 200 
# I looked at the TL images and saw that this wasn't the case
# SuperSegger likely got confused when the focus changed halfway during the TL
# or when the cells changed from phase dark to phase light

# My solution is to manually exclude cells that were flagged as having divided

corrected_data <- filter(data, drug_conc == 0 | drug_conc == 10 | drug_conc == 20 |
                         drug_conc == 200 & division == FALSE)

# checking that this correction worked
table(corrected_data$drug_conc, corrected_data$division)


#-------------------------------------------------------------
# preparing corrected data

#How many cells in each concentration/position for d3?
c_d3 <- filter(corrected_data, strain == "d3")
table(c_d3$drug_conc, c_d3$position)

#How many cells in each concentration/position for WT?
c_WT <- filter(corrected_data, strain == "WT")
table(c_WT$drug_conc, c_WT$position)

# what percentage of the initial population divided?
c_initial_pop <- filter(corrected_data, birthframe == 1)
prop.table(table(c_initial_pop$strain, c_initial_pop$division), margin = 1)

# what percentage of cells divided, grouped by drug concentration?
c_d3_initial_pop <- filter(c_d3, birthframe == 1)
c_d3_initial_pop_division <- as.data.frame(prop.table(table(c_d3_initial_pop$drug_conc, c_d3_initial_pop$division), margin = 1))
colnames(c_d3_initial_pop_division)[1] <- 'drug_conc'
colnames(c_d3_initial_pop_division)[2] <- 'division'
colnames(c_d3_initial_pop_division)[3] <- 'proportion'
c_d3_initial_pop_division$strain <- "Δ3"

c_WT_initial_pop <- filter(c_WT, birthframe == 1)
c_WT_initial_pop_division <- as.data.frame(prop.table(table(c_WT_initial_pop$drug_conc, c_WT_initial_pop$division), margin = 1))
colnames(c_WT_initial_pop_division)[1] <- 'drug_conc'
colnames(c_WT_initial_pop_division)[2] <- 'division'
colnames(c_WT_initial_pop_division)[3] <- 'proportion'
c_WT_initial_pop_division$strain <- "WT"

c_initial_pop_division <- rbind(c_d3_initial_pop_division, c_WT_initial_pop_division)


#-------------------------------------------------------------
# graphing CORRECTED proportion of initial population that divided
# grouping first by drug concentration and then by strain
ggplot(c_initial_pop_division, aes(x = strain, y = proportion, fill = division)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~drug_conc, strip.position = "bottom", ncol = 4) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  scale_fill_manual(values = met.brewer("Juarez", 2)) +
  ylab("Proportion of dividing cells") +
  guides(color = guide_legend(title = "Division observed"))


# grouping first by strain and then by drug concentration
ggplot(c_initial_pop_division, aes(x = drug_conc, y = proportion, fill = division)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~strain, strip.position = "bottom", ncol = 4) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  scale_fill_manual(values = met.brewer("Juarez", 2)) +
  ylab("Proportion of dividing cells") +
  xlab("Drug concentration (ug/mL)")





