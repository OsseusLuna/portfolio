### Alan Luna
### COSC 6510 Dr. Bialkowski
### Public Trust in Tech Companies

## Part 1: Data Management
################################################################################

## Remove all objects from workspace
rm(list=ls())

## Import Pew Research Data Set - Wave 127
## Haven allows reading spss files
library(haven)
data<-read_sav("Documents/School/Fall2025/COSC6510/Project/Pew/Pew.sav")

## Part 1: Data Management
################################################################################
## Part 1a. Variable management
################################################################################

## Renaming variables to make them more comprehensive and shorten the name
## How effective privacy policies are
names(data)[names(data) == "PPEFF_W127"] <- "ppeff"
## Feeling in control
names(data)[names(data) == "PRIVFEEL_a_W127"] <- "priv_control"
## Feeling confident
names(data)[names(data) == "PRIVFEEL_b_W127"] <- "priv_confident"
## Feeling informed
names(data)[names(data) == "PRIVFEEL_c_W127"] <- "priv_informed"
## Feeling Concerned
names(data)[names(data) == "PRIVFEEL_d_W127"] <- "priv_concerned"
## Feeling frustrated
names(data)[names(data) == "PRIVFEEL_e_W127"] <- "priv_frustrated"
## Age Category
names(data)[names(data) == "F_AGECAT"] <- "age"
## Gender
names(data)[names(data) == "F_GENDER"] <- "gender"
## Education
names(data)[names(data) == "F_EDUCCAT2"] <- "edu"
## Political Ideology
names(data)[names(data) == "F_PARTYSUMIDEO_FINAL"] <- "poli_ideo"
## Household Income
names(data)[names(data) == "F_INC_SDT1"] <- "income"
## Frequency of Internet Use
names(data)[names(data) == "F_INTFREQ"] <- "int_freq"
## Own a smartphone
names(data)[names(data) == "SMARTPHONE_W127"] <- "smartphone"
## Digital Habits - Manage Privacy Online - Decline cookies or tracking
names(data)[names(data) == "DIGHABT2_a_W127"] <- "dig_habit_tracking"
## Digital Habits - Manage Privacy Online - Use messaging apps that
## encrypt private communications
names(data)[names(data) == "DIGHABT2_c_W127"] <- "dig_habit_encrypt"
## Digital Habits - Manage Privacy Online - Use a browser that does not
## track what you are doing
names(data)[names(data) == "DIGHABT2_d_W127"] <- "dig_habit_browser"
## Digital Habits - Manage Privacy Online - Change your social media
## privacy setting
names(data)[names(data) == "DIGHABT2_e_W127"] <- "dig_habit_socialmedia"
## Privacy policies "just something to click through"
names(data)[names(data) == "PPUSE_W127"] <- "priv_policy_att"
## Privacy laws literacy
names(data)[names(data) == "PRIVACYREG_W127"] <- "priv_lit"
## Does the US have a national privacy law?
names(data)[names(data) == "DKQ10_W127"] <- "nati_privlaw"
## How worried are you about companies selling your identity or peronsal info?
names(data)[names(data) == "PRIVWRY_b_W127"] <- "comp_selling"
## How worried are you about law enforcement monitoring what you do online?
names(data)[names(data) == "PRIVWRY_c_W127"] <- "law_monitoring"
## How concerned are you that companies are selling your data
names(data)[names(data) == "CONCERNCO_W127"] <- "conc_co"
## How concerned are you that law enforcment is monitoring you
names(data)[names(data) == "CONCERNGOV_W127"] <- "conc_gov"
## How much do you trust social media leaders to NOT sell your personal
## data without your consent
names(data)[names(data) == "SMLEAD_c_W127"] <- "trust_smlead"
## How much do you trust companies to make respnsible decisions about how they
## use AI in their products
names(data)[names(data) == "AIPRIV3_W127"] <- "trust_ai"
## How much do you trust that leaders of social media companies will take
## responsibilities when they misuse or compromise personal data
names(data)[names(data) == "SMLEAD_a_W127"] <- "trust_smlead2"
## How much do you trust that leaders of social media companies will notify
## the government if they misuse or compromise users' personal data
names(data)[names(data) == "SMLEAD_b_W127"] <- "trust_smlead3"

## Pew Research Dataset is an SPSS file, and by default all the values are
## labeled, I need to convert all variables I use into numeric to proceed
data$ppeff <- as.numeric(data$ppeff)
data$priv_control <- as.numeric(data$priv_control)
data$priv_confident <- as.numeric(data$priv_confident)
data$priv_informed <- as.numeric(data$priv_informed)
data$priv_concerned <- as.numeric(data$priv_concerned)
data$priv_frustrated <- as.numeric(data$priv_frustrated)
data$age <- as.numeric(data$age)
data$gender <- as.numeric(data$gender)
data$edu <- as.numeric(data$edu)
data$poli_ideo <- as.numeric(data$poli_ideo)
data$income <- as.numeric(data$income)
data$int_freq <- as.numeric(data$int_freq)
data$smartphone <- as.numeric(data$smartphone)
data$dig_habit_tracking <- as.numeric(data$dig_habit_tracking)
data$dig_habit_encrypt <- as.numeric(data$dig_habit_encrypt)
data$dig_habit_browser <- as.numeric(data$dig_habit_browser)
data$dig_habit_socialmedia <- as.numeric(data$dig_habit_socialmedia)
data$priv_policy_att <- as.numeric(data$priv_policy_att)
data$priv_lit <- as.numeric(data$priv_lit)
data$nati_privlaw <- as.numeric(data$nati_privlaw)
data$comp_selling <- as.numeric(data$comp_selling)
data$law_monitoring <- as.numeric(data$law_monitoring)
data$conc_co <- as.numeric(data$conc_co)
data$conc_gov <- as.numeric(data$conc_gov)
data$trust_smlead <- as.numeric(data$trust_smlead)
data$trust_ai <- as.numeric(data$trust_ai)
data$trust_smlead2 <- as.numeric(data$trust_smlead2)
data$trust_smlead3 <- as.numeric(data$trust_smlead3)

## Part 1b. Data Cleaning
################################################################################

var <- c("ppeff", "priv_control", "priv_confident", "priv_informed",
         "priv_concerned", "priv_frustrated", "age", "gender", "edu",
         "poli_ideo", "income", "int_freq", "smartphone", "dig_habit_tracking", 
         "dig_habit_encrypt", "dig_habit_browser", "dig_habit_socialmedia",
         "priv_policy_att", "priv_lit", "nati_privlaw", "comp_selling",
         "law_monitoring", "conc_co", "conc_gov", "trust_smlead", "trust_ai",
         "trust_smlead2", "trust_smlead3")

## Looking for missing variables
is.na(var)

## Realized that does not work since its a grouping of multiple variables
is.na(data$ppeff)

## Counting missing values
sum(is.na(data$ppeff))

## sum(is.na()) worked however there are some values that are coded as 99
table(data$ppeff)

## After reviewing Pew Research Center code book, some variables have answers
## such as 99, 98, or 97 that are "refused" or "don't know" answers. 

## Inspecting each variable for how they code the non answer variables while
## including missing values
table(data$ppeff, useNA = "ifany")

## After observing "99" used as missing value, it gets replaced as "NA"
data$ppeff[data$ppeff == 99] <- NA

## Repeat for other variables
table(data$priv_control, useNA = "ifany")
data$priv_control[data$priv_control == 99] <- NA      

table(data$priv_confident, useNA = "ifany")
data$priv_confident[data$priv_confident == 99] <- NA

table(data$priv_informed, useNA = "ifany")
data$priv_informed[data$priv_informed == 99] <- NA

table(data$priv_concerned, useNA = "ifany")
data$priv_concerned[data$priv_concerned == 99] <- NA

table(data$priv_frustrated, useNA = "ifany")
data$priv_frustrated[data$priv_frustrated == 99] <- NA

table(data$age, useNA = "ifany")
data$age[data$age == 99] <- NA

table(data$gender, useNA = "ifany")
data$gender[data$gender == 99] <- NA

table(data$edu, useNA = "ifany")
data$edu[data$edu == 99] <- NA

## 9 used instead of 99
table(data$poli_ideo, useNA = "ifany")
data$poli_ideo[data$poli_ideo == 9] <- NA

table(data$income, useNA = "ifany")
data$income[data$income == 99] <- NA

table(data$int_freq, useNA = "ifany")
data$int_freq[data$int_freq == 99] <- NA

table(data$smartphone, useNA = "ifany")
data$smartphone[data$smartphone == 99] <- NA

table(data$dig_habit_tracking, useNA = "ifany")
data$dig_habit_tracking[data$dig_habit_tracking == 99] <- NA

table(data$dig_habit_encrypt, useNA = "ifany")
data$dig_habit_encrypt[data$dig_habit_encrypt == 99] <- NA

table(data$dig_habit_browser, useNA = "ifany")
data$dig_habit_browser[data$dig_habit_browser == 99] <- NA

table(data$dig_habit_socialmedia, useNA = "ifany")
data$dig_habit_socialmedia[data$dig_habit_socialmedia == 99] <- NA

table(data$priv_policy_att, useNA = "ifany")
data$priv_policy_att[data$priv_policy_att == 99] <- NA

table(data$priv_lit, useNA = "ifany")
data$priv_lit[data$priv_lit == 99] <- NA

table(data$nati_privlaw, useNA = "ifany")
data$nati_privlaw[data$nati_privlaw == 99] <- NA

table(data$comp_selling, useNA = "ifany")
data$comp_selling[data$comp_selling == 99] <- NA

table(data$law_monitoring, useNA = "ifany")
data$law_monitoring[data$law_monitoring == 99] <- NA

table(data$conc_co, useNA = "ifany")
data$conc_co[data$conc_co == 99] <- NA

table(data$conc_gov, useNA = "ifany")
data$conc_gov[data$conc_gov == 99] <- NA

table(data$trust_ai, useNA = "ifany")
data$trust_ai[data$trust_ai == 99] <- NA

## Since 5 is an answer of "Not Sure", it is transformed to NA
table(data$trust_smlead, useNA = "ifany")
data$trust_smlead[data$trust_smlead %in% c(5, 99)] <- NA

table(data$trust_smlead2, useNA = "ifany")
data$trust_smlead2[data$trust_smlead2 %in% c(5, 99)] <- NA

table(data$trust_smlead3, useNA = "ifany")
data$trust_smlead3[data$trust_smlead3 %in% c(5, 99)] <- NA

## Ommitting NA values instead of using imputation since a composite score will
## be created and can distort standardization of the scores since some of the NA
## values come from "Refused" or "Dont know" Answers
cdata <- data[complete.cases(data[, var]), ]

## Part 1c. Data Recoding
################################################################################

## Recoding certain categorical variables for easier visualization down the line

## Age categories
cdata$age_cat <- factor(cdata$age,
                       levels = c(1,2,3,4),
                       labels = c("18-29","30-49","50-64","65+")
)
## Education categories
cdata$edu_cat <- factor(cdata$edu,
                       levels = c(1,2,3,4,5,6),
                       labels = c("Less than HS","High school","Some College",
                                  "Associate's","Bachelor's","Postgraduate"))
## Internet Frequency
cdata$int_freq_cat <- factor(cdata$int_freq,
                            levels = c(1,2,3,4,5),
                            labels = c("Almost constantly","Several times a day",
                                       "About once a day","Several times a week",
                                       "Less Often"))
## Smartphone ownership
cdata$smartphone_cat <- factor(cdata$smartphone,
                              levels = c(1,2),
                              labels = c("Owns a smartphone", "Does not own"))
## Gender
cdata$gender_cat <- factor(cdata$gender,
                      levels = c(1,2,3),
                      labels = c("Male", "Female", "Other"))

## Political Ideology
cdata$poli_cat <- factor(cdata$poli_ideo,
                         levels = c(1,2,3,4),
                         labels = c("Conservative","Moderate Liberal Republican",
                                    "Moderate Conservative Democrat",
                                    "Liberal"))

## Part 1d. Data refinement
################################################################################
## Creating a reverse code function for the questions that require it
reverse_code <- function(x) {
  max_val <- max(x, na.rm = TRUE)
  max_val + 1 - x
}

## Reverse coding variables that require it for data analysis
cdata$comp_selling <- reverse_code(cdata$comp_selling)
cdata$law_monitoring <- reverse_code(cdata$law_monitoring)
cdata$conc_co <- reverse_code(cdata$conc_co)
cdata$conc_gov <- reverse_code(cdata$conc_gov)
cdata$priv_lit <- reverse_code(cdata$priv_lit)

## Part 1e. Composite score creation
################################################################################

## Create composite mistrust score, since there is no dedicated trust variable
## created from 4 different trust based questions
trust_vars <- c("trust_smlead", "trust_ai", "trust_smlead2", "trust_smlead3")

## Subset 
trust_items <- cdata[, trust_vars]
## Standardize items since likert scales are different, as some are 1-4/1-5
trust_items_z <- scale(trust_items)
## Creating mistrust composite score from z-scored items 
cdata$trust_z <- rowMeans(trust_items_z)
## Non-standardized
cdata$trust <-rowMeans(trust_items)

## Create privacy concerns composite score since there is no direct concern
## variable, a group of 4 concern based questions were used.
conc_vars <- c("comp_selling", "law_monitoring", "conc_co", "conc_gov")
## Subset
conc_items <- cdata[, conc_vars]
## Standardize
conc_items_z <- scale(conc_items)
cdata$conc_z <- rowMeans(conc_items_z)
## Non-standardized
cdata$conc <- rowMeans(conc_items)

## Cronbach's Alpha to find internal consistency
library(psych)

## Misrust composite score internal consistency
alpha_trust <- psych::alpha(trust_items_z)
alpha_trust
## Percieved privacy concern composite score
alpha_conc <- psych::alpha(conc_items_z)
alpha_conc

## Exploratory Factor Analysis 
## Digital Mistrust Parallel Analysis
fa.parallel(trust_items_z,
            fm = "ml",
            fa = "fa",
            main = "Parallel Analysis for Digital Mistrust Items",)
abline(h = 1, lty = 2)
         
## Factor loading using olbimin rotation
efa_trust_1f <- fa(trust_items_z,
                   nfactors = 1,
                   fm = "ml",
                   rotate = "oblimin")
efa_trust_1f
efa_trust_2f <-fa(trust_items_z,
                  nfactors = 2,
                  fm = "ml",
                  rotate = "oblimin")
efa_trust_2f
## Only loads on one factor
## Digital Privacy Concerns
fa.parallel(conc_items_z,
            fm = "ml",
            fa = "fa")
efa_conc_1f <- fa(conc_items_z,
                   nfactors = 1,
                   fm = "ml",
                   rotate = "oblimin")
efa_conc_1f
efa_conc_2f <-fa(conc_items_z,
                  nfactors = 2,
                  fm = "ml",
                  rotate = "oblimin")
efa_conc_2f

## Part 1f. Descriptive statistics
################################################################################
## Showing descriptive statistics for some key variables
summary(cdata$trust_z) ## z standardized
summary(cdata$conc_z) ## z standardized
summary(cdata$ppeff)  
summary(cdata$priv_lit)

## Part 1g. Exploratory Visualizations
################################################################################
library(ggplot2)

## Distribution of Digital Mistrust (Trust composite score) - z score
## Histogram
ggplot(cdata, aes(x = trust_z)) +
  geom_histogram(binwidth = 0.25, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Digital Mistrust (trust_z)",
       x = "Mistrust (standardized)", y = "Count")

## Since standardization created a different max and minimum score than the
## likert scale, the x axis must be changed to not appear skewed.
ggplot(cdata, aes(x = trust_z)) +
  geom_histogram(binwidth = 0.25, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Digital Mistrust",
       x = "Mistrust (standardized)", y = "Count") +
  scale_x_continuous(limits = c(-2, 2))

## Distribution of digital mistrust - likert scale
## Histogram
ggplot(cdata, aes(x = trust)) +
  geom_histogram(binwidth = 0.25, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Digital Mistrust (trust_z)",
       x = "Mistrust", y = "Count")

## Distribution of privacy concerns (privacy concerns composite score) - z score
## Histogram
ggplot(cdata, aes(x = conc_z)) +
  geom_histogram(binwidth = 0.25, fill = "firebrick", color = "white") +
  labs(title = "Distribution of Privacy Concerns",
       x = "Concerns (standardized)", y = "Count") +
  scale_x_continuous(limits = c(-2, 2))

## Distribution of privacy concerns (privacy concerns composite score) - likert
## Histogram
ggplot(cdata, aes(x = conc)) +
  geom_histogram(binwidth = 0.25, fill = "firebrick", color = "white") +
  labs(title = "Distribution of Privacy Concerns (conc_z)",
       x = "Concerns", y = "Count") 

## Relationship between privacy concern and mistrust
## Bivariate scatterplot
ggplot(cdata, aes(x = conc_z, y = trust_z))+
  geom_point(alpha = 0.3, color = "darkgreen") + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Digital Privacy Concerns vs Digital Mistrust",
       x = "Privacy Concern",
       y = "Digital Mistrust") +
  scale_x_continuous(limits = c(-2, 2))

## Privacy literacy vs mistrust
## Bivariate scatterplot
ggplot(cdata, aes(x = priv_lit, y = trust_z)) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Privacy Literacy vs Digital Mistrust",
       x = "Privacy Literacy (1–4 Likert)",
       y = "Digital Mistrust (z-score)") +
  scale_x_continuous(limits = c(1,4))

## Perceived Privacy effectiveness  literacy vs mistrust
## Bivariate scatterplot
ggplot(cdata, aes(x = ppeff, y = trust_z)) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Perceived Effectiveness vs Digital Mistrust",
       x = "Privacy Literacy (1–4 Likert)",
       y = "Digital Mistrust (z-score)") +
  scale_x_continuous(limits = c(1,4))

## Mistrust by age group - z score
ggplot(cdata, aes(x = age_cat, y = trust_z, fill = age_cat)) +
  geom_boxplot() +
  labs(title = "Digital Mistrust Across Age Groups",
       x = "Age Category", y = "Digital Mistrust") +
  theme(legend.position = "none")

## Mistrust by age group - likert
ggplot(cdata, aes(x = age_cat, y = trust, fill = age_cat)) +
  geom_boxplot() +
  labs(title = "Digital Mistrust Across Age Groups",
       x = "Age Category", y = "Digital Mistrust") +
  theme(legend.position = "none")

## Overlaying a normal curve to histogram
ggplot(cdata, aes(x = trust_z)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 0.25, 
                 fill = "lightblue", 
                 color = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(cdata$trust_z), 
                            sd   = sd(cdata$trust_z)),
                color = "darkblue", 
                size = 1.2) +
  scale_x_continuous(limits = c(-3, 3)) +
  labs(title = "Bell Curve of Digital Mistrust (trust_z)",
       x = "Digital Mistrust (z-score)",
       y = "Density")

## Trend line applied to histogram
ggplot(cdata, aes(x = trust)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.25,
                 fill = "lightblue",
                 color = "white") +
  geom_density(color = "darkblue", size = 1.3, adjust = 1.2) +
  labs(title = "Digital Mistrust (trust_z): Histogram with Trend Line",
       x = "Digital Mistrust (z-score)",
       y = "Density")


## Part 2. Analytical Techniques
################################################################################
## Part 2a. Applied Probability
## Since trust_z is a z-score, 0 represents the sample average mistrust
## High mistrust is then scored as greater than 0 for trust_z or above average
cdata$high_mistrust <- ifelse(cdata$trust_z > 0,1,0)

## Probability that a randomly selected adult has a high digital mistrust
prob_high_mistrust <- mean(cdata$high_mistrust == 1)
prob_high_mistrust

## Probability that a randomly selected adult own a smartphone
prob_smartphone <- mean(cdata$smartphone == 1)
prob_smartphone 

## P(High mistrust AND owns smartphone)
joint_highm_smart <- mean(
  cdata$high_mistrust == 1 & cdata$smartphone == 1)
joint_highm_smart

## P(High mistrust|Owns smartphone)
prob_highm_g_smart <- joint_highm_smart / prob_smartphone
prob_highm_g_smart

## Conditional probability by privacy literacy level
## Split privacy into "low" and high" using the median
med_priv_lit <- median(cdata$priv_lit)
cdata$priv_lit_high <- ifelse(cdata$priv_lit > med_priv_lit, 1 ,0)

## P(high mistrust|high privacy literacy)
prob_highm_g_highlit <- mean(
  cdata$high_mistrust == 1 & cdata$priv_lit_high == 1
) / mean(cdata$priv_lit_high == 1)
prob_highm_g_highlit

## P(high mistrust|low privacy literacy)
prob_highm_g_lowlit <- mean(
  cdata$high_mistrust == 1 & cdata$priv_lit_high == 0
) / mean(cdata$priv_lit_high == 0)
prob_highm_g_lowlit

## Conditional probability using digital habits: declining cookies/tracking
cdata$decline_track_y <- ifelse(cdata$dig_habit_tracking == 1, 1, 0)

## P(high mistrust|declines tracking)
prob_highm_g_decline <- mean(
  cdata$high_mistrust == 1 & cdata$decline_track_y == 1
) / mean(cdata$decline_track_y == 1)
prob_highm_g_decline

## P(high mistrust|does not decline tracking)
prob_highm_g_notdecline <- mean(
  cdata$high_mistrust == 1 & cdata$decline_track_y == 0
) / mean(cdata$decline_track_y == 0)
prob_highm_g_notdecline

## Conditional probability by digital privacy concerns

## Defining high privacy concern = above average concern since z-score
cdata$high_conc <- ifelse(cdata$conc_z > 0,1,0)

## P(high mistrust|high privacy concern)
prob_highm_g_highc <- mean(
  cdata$high_mistrust == 1 & cdata$high_conc == 1
) / mean(cdata$high_conc == 1)
prob_highm_g_highc

## P(high mistrust|low privacy concern)
prob_highm_g_lowc <- mean(
  cdata$high_mistrust == 1 & cdata$high_conc == 0
) / mean(cdata$high_conc == 0)
prob_highm_g_lowc

## Part 2b. Correlation matrix 
################################################################################
## Combine correlation variables
corr_vars <- c("trust_z",      
               "conc_z",       
               "ppeff",        
               "priv_lit",     
               "dig_habit_tracking",
               "dig_habit_encrypt",
               "dig_habit_browser",
               "dig_habit_socialmedia",
               "age", "edu", "income", "int_freq")

corr_data <- cdata[, corr_vars]
corr_matrix <- cor(corr_data)

## Round for easier viewing
round(corr_matrix, 2)
corr_matrix

## Part 2c. Linear regression 
################################################################################
## Model digital mistrust as a function of privacy concern, literacy, perceived
## policy effectiveness, digital habits and demographics

model_mistrust <- lm(
  trust_z ~ conc_z + priv_lit + ppeff + dig_habit_tracking + dig_habit_encrypt +
    dig_habit_browser + dig_habit_socialmedia + age_cat + edu_cat + income + 
    int_freq + gender_cat + poli_cat + smartphone_cat,
  data = cdata
)

## Individual significance (t-tests, p-values)
summary(model_mistrust)

## Joint significance (ANOVA)
anova(model_mistrust)

## Confidence intervals for coefficients
confint(model_mistrust)

## Part 2d. Group comparisons 
################################################################################

## ANOVA: Digital mistrust by age category
mis_anova_age <- aov(trust_z ~ age_cat, data = cdata)
summary(mis_anova_age)

## ANOVA: Digital mistrust by educational category
mis_anova_edu <- aov(trust_z ~ edu_cat, data = cdata)
summary(mis_anova_edu)

## ANOVA: Digital mistrust by gender category
mis_anova_gender <- aov(trust_z ~ gender_cat, data = cdata)
summary(mis_anova_gender)

## ANOVA: Privacy concerns by age category
conc_anova_age <- aov(conc_z ~ age_cat, data = cdata)
summary(conc_anova_age)

## ANOVA: Privacy concerns by educational category
conc_anova_edu <- aov(conc_z ~ edu_cat, data = cdata)
summary(conc_anova_edu)

## ANOVA: Privacy concerns by gender
conc_anova_gender <- aov(conc_z ~ gender_cat, data = cdata)
summary(conc_anova_gender)

## Part 2e. Logistic Regression
################################################################################
## Using the high_mistrust indicator used for applied probability as a binary
## outcome
## Convert to factor for glm with binomial family
cdata$high_mistrust_fac <- factor(
  cdata$high_mistrust,
  levels = c(0, 1),
  labels = c("Lower mistrust", "Higher mistrust")
)

logit_mistrust <- glm(
  high_mistrust_fac ~ conc_z + priv_lit + ppeff +
    dig_habit_tracking + dig_habit_encrypt +
    dig_habit_browser + dig_habit_socialmedia +
    age + edu + income + int_freq +
    gender + poli_ideo + smartphone,
  data   = cdata,
  family = binomial(link = "logit")
)

summary(logit_mistrust)

## Convert logistic coefficients to odds ratios for easier interpretation
odds_ratios <- exp(coef(logit_mistrust))
odds_ci      <- exp(confint(logit_mistrust))

odds_ratios
odds_ci

## Predicted probability of high mistrust for each respondent
cdata$pred_high_mistrust <- predict(logit_mistrust, type = "response")

## 0.5 cutoff classification
cdata$pred_class <- ifelse(cdata$pred_high_mistrust >= 0.5,
                           "Higher mistrust", "Lower mistrust")

## Confusion matrix: observed vs predicted high mistrust
table(
  Observed  = cdata$high_mistrust_fac,
  Predicted = cdata$pred_class
)

## Part 2e. k-fold Cross validation
################################################################################
## Linear regression model 
library(caret)

## Psuedorandom number generator 
set.seed(123)
## Setting up cross validation
cv_control <- trainControl(method = "cv", number = 10)

##
linear_cv_trust <- train(trust_z ~ conc_z + priv_lit + ppeff +
                           dig_habit_tracking + dig_habit_encrypt +
                           dig_habit_browser + dig_habit_socialmedia +
                           age + edu + income + int_freq +
                           gender + poli_ideo + smartphone,
                         data = cdata,
                         method = "lm",
                         trControl = cv_control
)
linear_cv_trust

