# Braden Judson
# November 2022
# Spawner biosample data for Osoyoos Sockeye
# Most data from Margot Stockwell's files

# Libraries
library(dplyr); library(ggplot2)

setwd("~/oso_primary/data/spawners")      # Set wd.

'%ni%' <- Negate('%in%')                  # Custom "not in" operator.

dat <- read.csv("oso_spawner_valsonly_nov3.2022.csv") %>% # Read in raw data.
  filter(!is.na(year))                                    # Remove empty rows.

dim(dat)                            # 32558 x 15.
print(lapply(dat, class))           # Check class of each column. Mostly characters.

dat$year <- as.factor(dat$year)     # Year as factor.
unique(dat$year)                    # 20 levels, 2000 to 2019. 

dat %>%                             
  count(year) %>%                   # Visualize sample sizes by year.
  ggplot() +                        # Pipe into ggplot.
  geom_col(aes(x = year, y = n)) +  # Sample size by year.
  theme(axis.text.x = element_text(angle = 60)) + # Fix axis labs.
  ylab("Sample size (n)")           # Change y lab.

# Standardize each variable. Lots of noise. 

# 1. Conservation units. 
unique(dat$conservation_unit)     # All Osoyoos SOX - good!

# 2. Dates.
unique(dat$date)                  # Numerical excel format. Fix.

dat$ymd <- as.Date(dat$date, origin = "1899-12-30")       # Dates now yyyy/mm/dd.
dat$doy <- as.numeric(strftime(dat$ymd, format = "%j"))   # Also add day of year (doy).
class(dat$ymd)                                            # Class "Date".
dat <- dat[,colnames(dat) %ni% "date"]                    # Remove Excel-formatted dates.
summary(dat$ymd)                                          # Checks out (2000 - Nov 2019).

dates_df <- dat %>%                                       # Isolate dates.
  select(c("year", "doy"))                                # DOY is the most important variable.                       

(year_summ <- aggregate(. ~ year, data = dates_df,        # Observations by year.
                       FUN = function(x)                  # Which years more representative than others.
                         c(dev = round(sd(x),2),          # standard deviation - how variable were sample dates?
                           spread = max(x)-min(x),        # Days between last and first sampling day.
                           days = length(unique(x)))) %>% # Unique days sampled on.
   as.data.frame(.))                                      # Save as a DF and print.


# 3. Locations.

unique(dat$location)    # Whopping 82 factors - tons of redundancy. Messy.

dat$location <- tolower(dat$location) %>%               # Set locations to all lowercase.
  { gsub(pattern = ".*:\\s", replacement = "", .) %>%   # Remove "reach" info - anything before : and a space afterwards.
  gsub(pattern = "-", replacement = "to", .) %>%        # Make 2015 - 2016 and 2015 to 2016 the same.
      na_if(., "n/a") %>%                               # Turn character n/a into an actual NA.
      gsub(pattern = "; radio tagged", "", .) %>%       # Trim unnecessary text from location identifiers.
      gsub(pattern = "; pit tag # 3d9.1c2c5054ff", "", .) %>%   # Again.
      gsub(pattern = "; pit tag # 3d9.1c2c508be2", "", .) %>%   # Again.
      gsub(pattern = "; 1 otolith", "", .) %>%                  # Again
      sub(x = .,                                                # Below is ugly fix to consolidating similar strings.
          "ok prov park to vds 17|ok falls to vds 17|okanagan provincial park to vds 17|ok falls prov park to vds 17|ok falls prov. park to vds 17",
          "ok falls to vds 17") %>% 
      sub(x = .,
          "hwy bridge to boat launch|hwy 97 bridge crossing to boat launch ",
          "hwy 97 bridge to boat launch")}

unique(na.omit(dat$location))      # 42 factors now, excluding no data.


########## LOCATION QUESTIONS ##################################################
# Is direction important? ******************************************************
#    E.g., Is VDS 17 to VDS 16 the same as VDS 16 to VDS 17? *******************
# Needs review from somebody who knows system - not sure how to consolidate ****
################################################################################


# 4. Sampling.

unique(dat$Sampling); length(unique(dat$Sampling)) # Three factors. Assuming NA is same as unknown.

dat$Sampling <- gsub(dat$Sampling, pattern = "^$", replacement = "Unknown") %>% 
  as.factor() # Set as factor (instead of character string).

unique(dat$Sampling); # Two factors, Unknown and DP.
summary(dat$Sampling) # 29825 dead pitch (91.6%) and 2733 unknown (8.4% total).


# 5. Sex

unique(dat$sex)   # 16 factors with a lot of redundancy. 
# Unknown as: U, -, 0, ?, *, (blank), and unk. Consolidate below.

dat$sex <- tolower(dat$sex) %>%                                   # Set all lowercase.
  { gsub(pattern = "^\\?|^\\-{1}?|^u{1}$|^unk{1}$|\\*|0|^$",      # Combine redundant "unknowns".
         replacement = "unknown", .) %>%                          # Replace.
      gsub(pattern = "m jack", "jack", .) }                       # Consolidate jacks (all male).

unique(dat$sex)   # Looking much better, but there are Kokanee in the dataset.

dat_sox <- filter(dat, !grepl("ko", sex))  # Dataset now entirely Sockeye.
nrow(dat) - nrow(dat_sox)                  # Only five Kokanee removed. 
dat_sox$sex <- as.factor(dat_sox$sex)      # Sex as factor. Do this last to allow grepl above.

summary(dat_sox$sex)                       # Four levels. M, F, Unknown and 14 jacks.

########## FISH SEX QUESTIONS ##################################################
# What to do with Jacks? Remove, or keep as a separate factor? *****************
# Discard or retain fish of unknown sex? ***************************************
# Nov 7, from SA: What about Jills? Great point - quantify? ********************
################################################################################

# 6. Fish length (fork and POH)

dat_sox$fork_length_mm <- as.numeric(dat_sox$fork_length_mm)  # Convert to numbers. Sets text to NAs.
dat_sox$fork_length_mm[dat_sox$fork_length_mm == 0] <- NA     # Replace 0s with NAs.
summary(dat_sox$fork_length_mm)                               # Reasonable summary stats.
hist(dat_sox$fork_length_mm)                                  # No obvious errors.

dat_sox %>%                                   # Fish from 2016, 2017 and 2018 seem small.
  group_by(year) %>%                          # Group by year.
  summarise_at(vars(POH_mm, fork_length_mm),  # Mean lengths by each sample year.
               ~mean(., na.rm = T)) %>%       # Remove NAs from mean estim.
  print() %>%                                 # Print summary values.
  as.data.frame() %>%                         # Organize as DF (for ggplot).
  ggplot(.) +                                 # Read into ggplot.
  geom_col(aes(y = fork_length_mm, x = year)) + # Columns are length, bars are years.
  theme(axis.text.x = element_text(angle = 70)) # Angle text otherwise tough to read.

dat_sox$POH_mm <- as.numeric(dat_sox$POH_mm)          # Same as FL, but for POH ests.
dat_sox$POH_mm[dat_sox$POH_mm == 0] <- NA
summary(dat_sox$POH_mm)                               # Lowest values appear erroneous.
hist(dat_sox$POH_mm)                                  # Low value stands out. 
sort(dat_sox$POH_mm, decreasing = F)[1:10]            # POH of 29 and 41 clearly wrong.

sum(dat_sox$POH_mm < 100, na.rm = T)                  # Two fish below 10cm.

na.omit(dat_sox[dat_sox$POH_mm < 100, ])              # Given FL and mass, clearly an error. Must be 290mm.
dat_sox$POH_mm[dat_sox$POH_mm == 29] <- 290           # Now POH is 290mm. 

dat_sox <- subset(dat_sox,                                      # Remove 41mm fish. No FL or mass data to reinforce.
                  dat_sox$POH_mm != 41 | is.na(dat_sox$POH_mm)) # Remove POH of 41, but keep all POHs of NA.

summary(dat_sox$POH_mm)                               # Seems much better.
hist(dat_sox$POH_mm)                                  # No obvious visual outliers. 

sum(!is.na(dat_sox$fork_length_mm & dat_sox$POH_mm))  #~4k fish with both FL and POH numbers.
dat_sox[,c("fork_length_mm", "POH_mm")] <- round(dat_sox[,c("fork_length_mm", "POH_mm")],3)

lm(data = dat_sox, fork_length_mm ~ POH_mm) %>%       # adjR2 = 95.6 (p < 0.001; n = 4218).
  summary(.)                                          # intercept = 18.25, POH = 1.18.

# Note that in Hyatt et al. 2017 states that among parent pops of OSO SOX the relationship
# is: FL = 1.29*POH - 1.08 (r2 = 0.96, n = 541 b/w 2013 and 2018).
# Also note the data here are in mm, whereas Hyatt et al. uses cm.

FL_reg <- (dat_sox$POH_mm) * 1.18 + 18.25                # Estimate FL based off POHs.

dat_sox$FL_reg <- ifelse(!is.na(dat_sox$fork_length_mm), # If fork length are available, use those.
                         dat_sox$fork_length_mm, FL_reg) # If not, used regression-based estimates.
hist(dat_sox$FL_reg)                                     # Distribution of FLs.

sum(!is.na(dat_sox$fork_length_mm) & !is.na(dat_sox$POH_mm)) # ~4k have both POH + FL, but use FL.
sum(is.na(dat_sox$fork_length_mm) & !is.na(dat_sox$POH_mm))  # ~14k use regression-estimated FLs.
sum(!is.na(dat_sox$fork_length_mm) & is.na(dat_sox$POH_mm))  # ~12k raw FLs are the only data.


(ghist_age <- ggplot(data = dat_sox[dat_sox$age %ni% c("3", "4", "5", "Unknown"),], # Remove unwanted "Total Ages".
       aes(x = FL_reg, fill = age)) +                                           # FL hist, coloured by Euro Age.
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 5) +            # Histogram, transparency, smaller bins.
  scale_fill_brewer(palette = "Set1") +                                         # Color palette.
  ylab("Count") + xlab("Fork length (mm)") +                                    # Tidier labels.
  theme_bw() +                                                                  # Preset theme.
  theme(plot.background = element_blank(),                                      # Remove background panel lines.
        panel.grid = element_blank(),                                           # Remove grid lines.
        legend.position = c(0.1, 0.70),                                         # Put legend inside plot.
        legend.title = element_blank()) +                                       # Remove legend title.
    ggtitle("Aged fish (n = 6713)") +                                           # Plot title and sample size.
  geom_hline(yintercept = 0))                                                   # Add base line geom.

(ghist_un <- ggplot(data = dat_sox[dat_sox$age %in% c("Unknown"),],             # Remove unwanted "Total Ages".
       aes(x = FL_reg, fill = age)) +                                           # See comments on above plot.
  geom_histogram(alpha = 0.5, binwidth = 5) +
  scale_fill_brewer(palette = "Dark2") +
  ylab("Count") + xlab("Fork length (mm)") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
    ggtitle("Unknown age (n = 25645)") +
  geom_hline(yintercept = 0))

library(gridExtra)                                                              # Necessary for grid arrangement step.

grid.arrange(ghist_age, ghist_un, ncol = 2)                                     # Place plots side-by-side.

# Experimenting with Jack/Jill identification.
jacks1 <- dat_sox[dat_sox$age == 1.1,] 
jacks2 <- dat_sox[dat_sox$FL_reg < 350,] 
summary(jacks1$age)
summary(jacks1$FL_reg)
hist(as.numeric(jacks2$age))



########## LENGTH QUESTIONS ####################################################
# Convert all lengths to POH? Use regression? Relationship might vary by year **
# ~13% of fish have both FL and POH values *************************************
# Check my math on the FL ~ POH regression *************************************
# Check reasonable sizes - 150mm for a spawner? seems small ********************
# See comments on 2016-2017 fish sizes *****************************************
################################################################################



# 7. Age

unique(dat_sox$age)   # 21 factors with a lot of messiness. 

dat_sox$age <- dat_sox$age %>%  
  {gsub(pattern = "0|^n{1}$|ns|nr|No sample|NR|n\\\a|n/a|^$|na|\\#REF!|\\#N/A", 
        replacement = "Unknown", .) %>%    # Replace redundant "Unknown" symbols.
      gsub(pattern = "1F", "1", .) }      

dat_sox$age[is.na(dat_sox$age)] <- "Unknown"                 # Replace NAs with a factor for Unknown.

dat_sox <- dat_sox %>%                                       # Assign years-at-sea (salt age).
  mutate(salt_age = as.numeric(substr(x = dat_sox$age,       # Make numeric, remove characters.
                                      start = 3, stop = 3))) # Extract from European Age values.
hist(dat_sox$salt_age)                                       # 2 years at sea most common by far.
dat_sox$age <- as.factor(dat_sox$age)                        # European ages as factors.
summary(dat_sox$age)                                         # Non-decimal values are TOTAL AGES. 


########## AGE QUESTIONS #######################################################
# NOTE: In 2018 lots of ages are entered only as TOTAL AGE in both the Total Age
# and European Age columns. ****************************************************
# What about total ages? Remove? Can't make sense of them **********************
# Only relevant in 2018 for about ~430 fish ************************************
################################################################################


# 8. Thermal mark / Origin.

unique(dat_sox$thermal_mark)    # Needs consolidating / tidying. 23 Factors.

dat_sox$thermal_mark <- dat_sox$thermal_mark %>%                              # Convert into origins (H, N, U).
  {gsub(pattern = "UNK|lost|0|n/a|^$|NS", replacement = "Unknown", .) %>%     # Unknown origins.
      gsub(pattern = "^N,\\S+|^N{1}$", replacement = "Natural", .) %>%        # Natural origin (i.e. Wild fish).
      gsub(pattern = "^Y.+|^Y$|^[[:digit:]].+", replacement = "Hatchery") %>% # Hatchery origin fish.
      as.factor(.)}                                                           # Set as factor.

summary(dat_sox$thermal_mark) # Summary of origin/thermal mark factor.
# 4.6% of dead pitch fish from hatchery.
# 35.1% of dead pitch fish wild.
# Remaining ~60.3% of fish unknown origin (unresolvable otoliths, or not sampled at all).


########## ORIGIN QUESTIONS ####################################################
# Thermal mark = 0 - does this mean no (i.e., natural origin) or unknown? ******
# There aren't any with 1.... **************************************************
# Same with NS = not sampled? not sure *****************************************
################################################################################


# 9. Weight.

dat_sox$weight_g <- as.numeric(dat_sox$weight_g)  # Convert to numeric.
summary(dat_sox$weight_g); hist(dat_sox$weight_g) # Zeroes still in data.

dat_sox$weight_g[dat_sox$weight_g == 0] <- NA     # Replace zeroes with NAs.
summary(dat_sox$weight_g); hist(dat_sox$weight_g) # No super obvious outliers. 

sort(dat_sox$weight_g, decreasing = F)[1:20]      # Lowest weights.

########## WEIGHT QUESTIONS ####################################################
# Are 56.8 and 75.4g reasonable weights for spawners? **************************
# 56.8 is a male - maybe a jack? 75.4 is female though. ************************
# Nov7, SA: What about Jills? How to account for these fish? *******************
################################################################################


# 10. Fecundity.

summary(as.numeric(dat_sox$fecundity))     # Heavily skewed.
sum(is.na(as.numeric(dat_sox$fecundity)))  # 32359.
nrow(dat_sox)                              # 32552.
# Fecundity data missing for >99% of fish, skewed - remove. 
# I think these fecundity were estimated from dead pitch? Not useful for this.

dat_sox <- dat_sox[,colnames(dat_sox) %ni% "fecundity"] # Remove.

brd_fec <- read.csv("oso_sox_fecundity.csv",            # M. Stockwell's broodstock fecundity data. 
                    stringsAsFactors = T)[,c(1:8)]      # Strings as factors.
brd_fec$year <- as.factor(brd_fec$year)                 # Year as factor. 
summary(brd_fec$year)                                   # Distribution of fecundity samples.
dim(brd_fec)                                            # 413 x 8.

brd_fec$salt_age <- as.numeric(substr(x = brd_fec$age,     # Factor for years at sea (salt age).
                                      start = 3, stop = 3))# Number after decimal in Euro style.

brd_fec$salt_age <- as.numeric(brd_fec$salt_age)
brd_fec$eggs <- as.numeric(brd_fec$eggs)                # Eggs (i.e., fecundity) as number.
brd_fec$fork_l_cm <- as.numeric(brd_fec$fork_l_cm)*10   # FL as number, convert to mm.

summary(aov(eggs ~ year, data = brd_fec))               # Significant.
summary(aov(fork_l_cm ~ year, data = brd_fec))          # Significant.

cor(brd_fec[,colnames(brd_fec) %in% c("salt_age",       # Make a correlation matrix. 
                                      "eggs",           # Three numerical values. 
                                      "fork_l_cm")],    # Returns r (Pearson's CC).
    use = "complete.obs")                               # Ignores NAs for age.

# Fork length and salt age highly related, r > 0.8.
# Shouldn't use both in the same regression model.

ggplot(data = brd_fec) +                      # Visualize length ~ fecundity. 
  geom_point(aes(x = fork_l_cm, y = eggs))    # Exponential or linear.

summary(lm(eggs ~ fork_l_cm, data = brd_fec)) # Adj R2 = 0.542, p < 0.0001, DF1 = 411.
summary(lm(eggs ~ salt_age, data = brd_fec))  # Adj R2 = 0.339, p < 0.0001, DF1 = 351.
summary(lm(eggs ~ I(fork_l_cm^3), data = brd_fec))  # Adj R2 = 0.529, p < 0.001, DF = 411. 
summary(lm(log10(eggs) ~ I(log10(fork_l_cm)), data = brd_fec))  # Adj R2 = 0.573, p < 0.001, DF = 411.

# FL better predictor of fecundity than salt age. 
# Cubic and log perform similarly well (although log is better).
# If you need a refresher in antilog-ing an equation (like me), see Ben Bolker's
# comments here: https://stackoverflow.com/questions/46394106/is-there-a-way-to-get-the-antilog-in-r


dat_sox$eggs_lm <- case_when(                                 # Estimate fecundity based on LM.
  dat_sox$sex == "f" ~ (9.6984 * dat_sox$FL_reg) - 2139.0770, # For females, compute estimates.
  dat_sox$sex != "f" ~ NA_real_                               # For males, no estimates.
)

dat_sox$eggs_log <- case_when(                                # Using log-LM, estimate fecundity.
  dat_sox$sex == "f" ~ (dat_sox$FL_reg^1.98349)*(10^-1.9265), # For females only, use equation.
  dat_sox$sex != "f" ~ NA_real_                               # Exclude males from estimate.
)

hist(dat_sox$eggs_lm, main = 'Linear model',         # Visualize distribution of estimates.
     xlab = "Fecundity estimate"); box()             # Obtained from regular linear model.

hist(dat_sox$logeggs, main = "Log linear model",     # Visualize distribution of estimates. 
     xlab = "Fecundity estimate"); box()             # Obtained from log linear model (no negative values!).

sort(dat_sox$logeggs, decreasing = T)[1:20]          # Not sure if these larger values (>6k) need addressing.                


########## FECUNDITY QUESTIONS #################################################
# Are there more data? Surely >2016 exists somewhere ***************************
# Same as FL regression, check methods (AO) ************************************
# Up to ~230mm FL, fecundity estimates are NEGATIVE - check ********************
# APPLY TO FEMALES ONLY ********************************************************
# Account of AOV results - add year as a factor? Separate regressions? *********
# Redo LMs - convert to same units (mm). ***************************************
# Build side-by-side histograms of estimated and actual fecundity **************
################################################################################





################################################################################
################################################################################
################################################################################

# Before writing good copy:
  # Answer all questions above.
  # Figure out fecundity estimates (female only!).
  # Get annual averages of relevant biostats and cbind with 
  # data on dates (spread, stdev, etc). Use for regression weights?

# write.csv(dat_sox, "OSO_SOX_spawner_data_tidy.csv", row.names = F)
