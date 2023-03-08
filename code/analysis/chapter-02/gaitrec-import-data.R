# ======================================================================= #
# Plot the mean and covariance of the vgrf data for a group of
# healthy subjects in the gait data
# ======================================================================= #


# 1) Load Packages -----------------------------------------------------------
library(readr) # CRAN v1.3.1
library(tidyverse) # CRAN v1.3.0
library(fda) # CRAN v5.1.9

# File paths --------------------------------------------------------------
data_path <- here::here("data")


# 2) Plot settings --------------------------------------------------------
theme_set(new = theme_bw())
theme_update(axis.text = element_text(size = 10),
             axis.title = element_text(size = 12),
             strip.text = element_text(size = 12),
             axis.ticks = element_blank(),
             plot.title = element_text(size = 12, hjust = 0.5),
             plot.subtitle = element_text(size = 14, hjust = 0.5))

# Note: Skip to line 240 and read in the RDS files (commented out)
# if you just want to make the plots.

# 3) Read in Data ------------------------------------------------------------
# Exploratory analysis of the GaitRec data:
# https://www.nature.com/articles/s41597-020-0481-z

# Download:
# Force = vertical
# Leg: Left
# Processing: Smoothed and Normalized
# This is a big data set, it might take a while:
GRF_F_V_PRO_left <- read_csv("https://springernature.figshare.com/ndownloader/files/22063191")
# And associated MetaDeta, e.g., session and subject information:
GRF_metadata <- read_csv("https://ndownloader.figshare.com/files/22062960")
#.. you might have to wait.. it Will download!



# 4) Exploratory data analysis -----------------------------------------------
# Aim: become familiar eith the structure of the meta data
# to pick out a nice subset to demonstrate exploratory techniques.

head(GRF_metadata)
# We can get all info from here: https://www.nature.com/articles/s41597-020-0481-z/tables/4
#Identifiers
#SUBJECT_ID 	integer 	— 	Unique identifier of a subject
#SESSION_ID 	integer 	— 	Unique identifier of a session
#Labels
#CLASS_LABEL 	string 	— 	Annotated class labels
#CLASS_LABEL_DETAILED 	string 	— 	Annotated class labels for subclasses
#Subject Metadata
#SEX 	binary 	— 	female=0, male=1
#AGE 	integer 	years 	Age at recording date
#HEIGHT 	integer 	centimeter 	Body height in centimeters
#BODY_WEIGHT 	double 	𝑘𝑔𝑚𝑠2
#Body weight in Newton
#BODY_MASS 	double 	kg 	Body mass
#SHOE_SIZE 	double 	EU 	Shoe size in the Continental European System
#AFFECTED_SIDE 	integer — left=0, right=1, both=2
#Trial Metadata
#SHOD_CONDITION 	integer 	— 	barefoot & socks=0, normal shoe=1, orthopedic shoe=2
#ORTHOPEDIC_INSOLE 	binary 	— 	without insole=0, with insole=1
#SPEED 	integer 	— 	slow=1, self-selected=2, fast=3 walking speed
#READMISSION 	integer 	— 	indicates the number of re-admission=0 … n
#SESSION_TYPE 	integer 	— 	initial measurement=1, control measurement=2, initial measurement after readmission=3
#SESSION_DATE 	string 	— 	date of recording session in the format “DD-MM-YYYY”
#Train-Test Split Information
#TRAIN 	binary 	— 	is part (=1) or is not part (=0) of TRAIN
#TRAIN_BALANCED 	binary 	— 	is part (=1) or is not part (=0) of TRAIN_BALANCED
# TEST 	binary 	— 	is part (=1) or is not part (=0) of TEST

# Skim the meta data:
skimr::skim(GRF_metadata)

# How many different subjects?
GRF_metadata  %>%
  summarise(n_distinct(SUBJECT_ID))
#  2295 (!!!! big)

# How many different subjects in the healthy class (HC)?
GRF_metadata %>% filter(CLASS_LABEL == "HC") %>%
  summarise(n_distinct(SUBJECT_ID))
# 211

# How many sessions do each of them have?
GRF_metadata %>% filter(CLASS_LABEL == "HC") %>%
  group_by(SUBJECT_ID) %>%
  summarise(n_sesh = n_distinct(SESSION_ID)) %>%
  summarise(min(n_sesh), max(n_sesh), median(n_sesh))

# can look on a bar chart too..
GRF_metadata %>% filter(CLASS_LABEL == "HC") %>%
  group_by(SUBJECT_ID) %>%
  summarise(n_sesh = n_distinct(SESSION_ID)) %>%
  ggplot(aes(x = n_sesh)) +
  geom_bar()

# Are these all on the same day and under separate conditions?
GRF_metadata %>% filter(CLASS_LABEL == "HC") %>%
  group_by(SUBJECT_ID) %>%
  summarise(n_sesh = n_distinct(SESSION_DATE)) %>%
  summarise(min(n_sesh), max(n_sesh), median(n_sesh))
# Answer, a clear yes:.


# do the SESSION TYPE's differ?
GRF_metadata %>% filter(CLASS_LABEL == "HC") %>%
  summarise( min(SESSION_TYPE), max(SESSION_TYPE))
# No, they are all coded 1



# 5) Select a subset of the sessions for Exploratory Analysis  ---------------------

# Here is the subset we will take
# try and make it  as standard as possible
# eg. healthy controls, self-selected speed, wearing normal shoes.....
GRF_metadata %>% filter(CLASS_LABEL == "HC") %>%
  group_by(SUBJECT_ID) %>% # we are going to select one session per subject
  # filter(SESSION_TYPE == 1) %>% 
  filter(SPEED == 2) %>% # we'll go with self selected speed
  # filter(SESSION_DATE == min(SESSION_DATE)) %>% # make sure it's their first session 
  filter(SHOD_CONDITION == 1) %>% # all shod in normal shoe
  ungroup() %>% 
  summarise(n_distinct(SESSION_ID)) # check how man subjects this is
# 208
# Nice sample size for demonstration.

# Make sure it is one session per subject
# i.e we get all 1's here.
GRF_metadata %>% filter(CLASS_LABEL == "HC") %>%
  group_by(SUBJECT_ID) %>% 
  # filter(SESSION_TYPE == 1) %>%
  filter(SPEED == 2) %>%
  #filter(SESSION_DATE == min(SESSION_DATE)) %>% 
  filter(SHOD_CONDITION == 1) %>%
  count() %>%
  pull
# all good!!!

selected_sessions <- GRF_metadata %>% filter(CLASS_LABEL == "HC") %>%
  group_by(SUBJECT_ID) %>% 
  # filter(SESSION_TYPE == 1) %>%
  filter(SPEED == 2) %>%
  #filter(SESSION_DATE == min(SESSION_DATE)) %>% 
  filter(SHOD_CONDITION == 1) %>%
  pull(SESSION_ID) # Choose the session ID variable.

# make sure every element is a unique session ID
stopifnot(length(unique(selected_sessions)) == length(selected_sessions)) 

# save selected sessions as an RDS obj for later.
saveRDS(object = selected_sessions, 
        file = file.path(data_path, "gaitrec-selected-sessions.rds"))


# 6) Subset the GRF data -----------------------------------------------------

# This is processed data (normalised to [0,1])
head(GRF_F_V_PRO_left)

# We will subset it to look at only the session we have selected above
# Let's see how many trials per session/subject: 
GRF_F_V_PRO_left %>%
  filter(SESSION_ID %in% selected_sessions) %>%
  group_by(SESSION_ID) %>%
  summarise(num_trials = n_distinct(TRIAL_ID)) %>%
  summarise(min(num_trials), max(num_trials), median(num_trials))

# min max median
#  4  16 10

# the data are already smoothed ('filtered')
# think 35 basis functions and a tiny smoothing parameter will do.
# we will use cubic spline, penalize 2nd derivative with a tiny penalty
bsplines_35 <- create.bspline.basis(rangeval = c(0, 100), nbasis = 35, norder = 4)


# 7) Register and Average and represent the data as an FD object ----------

# we want an iid sample for mean, covar and pca.
# we will average all trials within a session to have 1 per subj

# Going to loop through every subject, and get an average
# store the coefficients of the average for each subject in this matrix
coefficients_average <- matrix(data = NA, nrow = bsplines_35$nbasis, ncol = length(selected_sessions))

# Start loop
for(sesh in 1:length(selected_sessions)){ 
  print(paste("session", sesh, "of", length(selected_sessions)))
  sesh_id <- selected_sessions[sesh] # loop through each session
  GRF_data <- GRF_F_V_PRO_left %>% filter(SESSION_ID == sesh_id) # subset GRF data to contain session
  n_trials <- nrow(GRF_data) # number of trials
  trial_id <- GRF_data$TRIAL_ID # ID of the trials
  grf_fd <- smooth.basis(argvals = 0:100, # smooth the data with the 35 basis functions
                         y = t(GRF_data[, -c(1:3)]), # and a tiny roughness penalty (10^-10)
                         fdParobj = fdPar(fdobj = bsplines_35, Lfdobj = 2, lambda = 10^-10))$fd
  # Store the coefficients of the average of the trials for this session.
  coefficients_average[, sesh] <- mean.fd(grf_fd)$coefs
}

# create an fd object from the averaged profiles:
average_profiles <- fd(coef = coefficients_average, basisobj = bsplines_35)
average_profiles$fdnames$reps <- selected_sessions # add session names
average_profiles$fdnames$funs <- "grf"

saveRDS(object = average_profiles,
        file = file.path(data_path, "gaitrec-healthy.rds"))
