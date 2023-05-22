# load packages -----------------------------------------------------------

library("stargazer")
library("here")
library("xlsx")
library("readxl")
library("ggplot2")
library("patchwork")
library("sjPlot")
library("stringr")

# create outputs folder ---------------------------------------------------

main_dir <- here::here()
sub_dir <- "Outputs"
ifelse(!dir.exists(file.path(main_dir, sub_dir)), dir.create(file.path(main_dir, sub_dir)), F)
rm("main_dir", "sub_dir")

# load dataset ------------------------------------------------------------

df <- read.xlsx(here::here("cleaned dataset.xlsx"), 1,
                as.data.frame = T, header = T)

df <- subset(df, select = -c(1))


# explore vars and generate summary stats ---------------------------------

min(df$num_animals[df$species == "chickens"])
mean(df$num_animals[df$species == "chickens"])
max(df$num_animals[df$species == "chickens"])

min(df$num_animals[df$species == "goats"])
mean(df$num_animals[df$species == "goats"])
max(df$num_animals[df$species == "goats"])

min(df$num_animals[df$species == "cows"])
mean(df$num_animals[df$species == "cows"])
max(df$num_animals[df$species == "cows"])

count(df$species == "chickens")
count(df$species == "goats")
count(df$species == "cows")

table(df$disease_last6month[df$species == "chickens"])
table(df$disease_last6month[df$species == "goats"])
table(df$disease_last6month[df$species == "cows"])

table(df$nontherapeutic_abx[df$species == "chickens"])
table(df$nontherapeutic_abx[df$species == "goats"])
table(df$nontherapeutic_abx[df$species == "cows"])

table(df$nontherapeutic_abx[df$species == "chickens"])
table(df$nontherapeutic_abx[df$species == "goats"])
table(df$nontherapeutic_abx[df$species == "cows"])

table(df$prof_diagn_treat[df$species == "chickens"])
table(df$prof_diagn_treat[df$species == "goats"])
table(df$prof_diagn_treat[df$species == "cows"])

# do simple correlations --------------------------------------------------

dfchickens <- df[df$species == "chickens",]
dfchickens <- subset(dfchickens, select = -c(18))

dfgoats <- df[df$species == "goats",]
dfgoats <- subset(dfgoats, select = -c(18))

dfcows <- df[df$species == "cows",]
dfcows <- subset(dfcows, select = -c(18))

dfcorr <- subset(df, select = -c(18))

tab_corr(dfchickens, triangle = "lower",
         title = "Chickens",
         file = "Outputs/corrchickens.doc")

tab_corr(dfgoats, triangle = "lower",
         title = "Goats",
         file = "Outputs/corrgoats.doc")

tab_corr(dfcows, triangle = "lower",
         title = "Cows",
         file = "Outputs/corrcows.doc")

tab_corr(dfcorr, triangle = "lower",
         title = "Whole Sample",
         file = "Outputs/corrwholesample.doc")

# effect of AMU habit on disease (last month) -----------------------------

reg1 <- glm(disease_lastmonth ~ prophylactic_abx + fattening_amu_attitude + num_animals,
                    family = "binomial", data = subset(df, species == "chickens"))

reg2 <- glm(disease_lastmonth ~ prophylactic_abx + fattening_amu_attitude + num_animals,
            family = "binomial", data = subset(df, species == "goats"))

reg3 <- glm(disease_lastmonth ~ prophylactic_abx + fattening_amu_attitude + num_animals,
            family = "binomial", data = subset(df, species == "cows"))

reg4 <- glm(disease_lastmonth ~ prophylactic_abx + fattening_amu_attitude + num_animals,
            family = "binomial", data = df)


# effect of AMU habit on disease (last 6 months) --------------------------

reg5 <- glm(disease_last6month ~ prophylactic_abx + fattening_amu_attitude + num_animals,
            family = "binomial", data = subset(df, species == "chickens"))

reg6 <- glm(disease_last6month ~ prophylactic_abx + fattening_amu_attitude + num_animals,
            family = "binomial", data = subset(df, species == "goats"))

reg7 <- glm(disease_last6month ~ prophylactic_abx + fattening_amu_attitude + num_animals,
            family = "binomial", data = subset(df, species == "cows"))

reg8 <- glm(disease_last6month ~ prophylactic_abx + fattening_amu_attitude + num_animals,
            family = "binomial", data = df)

# effect of AMU habit on disease (last year) ------------------------------

reg9 <- glm(disease_lastyear ~ prophylactic_abx + fattening_amu_attitude + num_animals,
            family = "binomial", data = subset(df, species == "chickens"))

reg10 <- glm(disease_lastyear ~ prophylactic_abx + fattening_amu_attitude + num_animals,
            family = "binomial", data = subset(df, species == "goats"))

reg11 <- glm(disease_lastyear ~ prophylactic_abx + fattening_amu_attitude + num_animals,
            family = "binomial", data = subset(df, species == "cows"))

reg12 <- glm(disease_lastyear ~ prophylactic_abx + fattening_amu_attitude + num_animals,
            family = "binomial", data = df)

# effect of AMU habit on disease (all time) -------------------------------

reg13 <- glm(disease_alltime ~ prophylactic_abx + fattening_amu_attitude + num_animals,
            family = "binomial", data = subset(df, species == "chickens"))

reg14 <- glm(disease_alltime ~ prophylactic_abx + fattening_amu_attitude + num_animals,
             family = "binomial", data = subset(df, species == "goats"))

reg15 <- glm(disease_alltime ~ prophylactic_abx + fattening_amu_attitude + num_animals,
             family = "binomial", data = subset(df, species == "cows"))

reg16 <- glm(disease_alltime ~ prophylactic_abx + fattening_amu_attitude + num_animals,
             family = "binomial", data = df)

# Decide which regressions to display -------------------------------------

stargazer(reg1, reg2, reg3, reg4, type = 'text') 
#'cows and whole sample found positive link bt proph amu and disease

stargazer(reg5, reg6, reg7, reg8, type = 'text')
#'goats, cows and whole sample found positive link bt proph amu and disease

stargazer(reg9, reg10, reg11, reg12, type = 'text')
#'goats, cows and whole sample found positive link bt proph amu and disease

stargazer(reg13, reg14, reg15, reg16, type = 'text')
#'goats, cows and whole sample found positive link bt proph amu and disease
#'belief that abx can be used for fattening associated with lower disease incidence in goats

#conclusion: display all time results, and state which trends were present in other specifications




# education and health provider --> disease (last month) ------------------

reg17 <- glm(disease_lastmonth ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
            family = "binomial", data = subset(df, species == "chickens"))

reg18 <- glm(disease_lastmonth ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
            family = "binomial", data = subset(df, species == "goats"))

reg19 <- glm(disease_lastmonth ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
            family = "binomial", data = subset(df, species == "cows"))

reg20 <- glm(disease_lastmonth ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
            family = "binomial", data = df)

# education and health provider --> disease (last 6 months) ---------------

reg21 <- glm(disease_last6month ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
             family = "binomial", data = subset(df, species == "chickens"))

reg22 <- glm(disease_last6month ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
             family = "binomial", data = subset(df, species == "goats"))

reg23 <- glm(disease_last6month ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
             family = "binomial", data = subset(df, species == "cows"))

reg24 <- glm(disease_last6month ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
             family = "binomial", data = df)

# education and health provider --> disease (last year) -------------------

reg25 <- glm(disease_lastyear ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
             family = "binomial", data = subset(df, species == "chickens"))

reg26 <- glm(disease_lastyear ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
             family = "binomial", data = subset(df, species == "goats"))

reg27 <- glm(disease_lastyear ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
             family = "binomial", data = subset(df, species == "cows"))

reg28 <- glm(disease_lastyear ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
             family = "binomial", data = df)

# education and health provider --> disease (all time) --------------------

reg29 <- glm(disease_alltime ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
             family = "binomial", data = subset(df, species == "chickens"))

reg30 <- glm(disease_alltime ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
             family = "binomial", data = subset(df, species == "goats"))

reg31 <- glm(disease_alltime ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
             family = "binomial", data = subset(df, species == "cows"))

reg32 <- glm(disease_alltime ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals,
             family = "binomial", data = df)


# decide which regressions to display -------------------------------------

stargazer(reg17, reg18, reg19, reg20, type = 'text',
          covariate.labels = c("formal education level", "goes primarily to community animal health worker",
                               "goes primarily to public vet", "goes primarily to unqualified private vet",
                               "goes primarily to qualified private vet", "number of animals"))
#' no main covariate significant

stargazer(reg21, reg22, reg23, reg24, type = 'text',
          covariate.labels = c("formal education level", "goes primarily to community animal health worker",
                               "goes primarily to public vet", "goes primarily to unqualified private vet",
                               "goes primarily to qualified private vet", "number of animals"))
#' public vet assiociated with lower disease for cows and whole sample. Negative coefficient, but
#' not significant, for chickens and goats

stargazer(reg21, reg22, reg23, reg24, type = 'text',
          covariate.labels = c("formal education level", "goes primarily to community animal health worker",
                               "goes primarily to public vet", "goes primarily to unqualified private vet",
                               "goes primarily to qualified private vet", "number of animals")) 
#' public vet assiociated with lower disease for cows and whole sample. Negative coefficient, but
#' not significant, for chickens and goats

stargazer(reg25, reg26, reg27, reg28, type = 'text',
          covariate.labels = c("formal education level", "goes primarily to community animal health worker",
                               "goes primarily to public vet", "goes primarily to unqualified private vet",
                               "goes primarily to qualified private vet", "number of animals")) 
#' no main covariate significant

stargazer(reg29, reg30, reg31, reg32, type = 'text',
          covariate.labels = c("formal education level", "goes primarily to community animal health worker",
                               "goes primarily to public vet", "goes primarily to unqualified private vet",
                               "goes primarily to qualified private vet", "number of animals")) 
#' no main covariate significant

#' conclusion: use last 6 month and state the results for the other timeframes.
#' probably best to use last 6 months for other regression as well, for consistency 
#' and comparability



# effect of education + service provider on nontherapeutic amu ------------

reg33 <- glm(nontherapeutic_abx ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + prof_diagn_treat + num_animals,
             family = "binomial", data = subset(df, species == "chickens"))

reg34 <- glm(nontherapeutic_abx ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + prof_diagn_treat + num_animals,
             family = "binomial", data = subset(df, species == "goats"))

reg35 <- glm(nontherapeutic_abx ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + prof_diagn_treat + num_animals,
             family = "binomial", data = subset(df, species == "cows"))

reg36 <- glm(nontherapeutic_abx ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + prof_diagn_treat + num_animals,
             family = "binomial", data = df)

stargazer(reg33, reg34, reg35, reg36, type = 'text',
          covariate.labels = c("formal education level", "goes primarily to community animal health worker",
                               "goes primarily to public vet", "goes primarily to unqualified private vet",
                               "goes primarily to qualified private vet", 
                               "professional provides diagnosis and treatment", "number of animals")) 
#' chicken farmers who went to community health workers seemed more likely to use abx 
#' nontherapeutically. In the sample as a whole, farmers who went to a public vet were
#' less likely to do so. Chicken farmers who went to a professional (of any kind) for
#' diagnosis and treatment were less likely to do this, but goat farmers (and the 
#' sample as a whole) were more likely to



# saving outputs ----------------------------------------------------------

#effect of AMU habits on disease
stargazer(reg5, reg6, reg7, reg8, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'text', out = "Outputs/AMU habits and disease.csv",
          # title = "Table 3: Effect of Antibiotic Use Habits on Likelihood of Disease in Last 6 Months (Odds Ratio)",
          column.labels = c("Chickens and other Poultry", "Goats and Sheep", "Cattle", "Whole Sample"),
          covariate.labels = c("uses antibiotics prophylactically",
                               "believes that antibiotics can be used for fattening",
                               "number of animals in flock / herd"),
          dep.var.labels = "Likelihood of Disease in Last 6 Months",
          style = "qje"
          )

stargazer(reg5, reg6, reg7, reg8, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'html', out = "Outputs/AMU habits and disease.html",
          # title = "Table 3: Effect of Antibiotic Use Habits on Likelihood of Disease in Last 6 Months (Odds Ratio)",
          column.labels = c("Chickens and other Poultry", "Goats and Sheep", "Cattle", "Whole Sample"),
          covariate.labels = c("uses antibiotics prophylactically",
                               "believes that antibiotics can be used for fattening",
                               "number of animals in flock / herd"),
          dep.var.labels = "Likelihood of Disease in Last 6 Months",
          style = "qje"
)

#effect of education and service provider on disease
stargazer(reg21, reg22, reg23, reg24, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'text', out = "Outputs/education, health service provider, and disease.csv",
          # title = "Table 4: Effect of Education Level and Primary Animal Health Service Provider
          # on Likelihood of Disease in Last 6 Months (Odds Ratio)",
          column.labels = c("Chickens and other Poultry", "Goats and Sheep", "Cattle", "Whole Sample"),
          covariate.labels = c("formal education level score", "goes primarily to community animal health worker",
                               "goes primarily to public vet", "goes primarily to unqualified private vet",
                               "goes primarily to qualified private vet", "number of animals in flock / herd"),
          dep.var.labels = "Likelihood of Disease in Last 6 Months",
          style = "qje"
)

stargazer(reg21, reg22, reg23, reg24, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'html', out = "Outputs/education, health service provider, and disease.html",
          # title = "Table 4: Effect of Education Level and Primary Animal Health Service Provider
          # on Likelihood of Disease in Last 6 Months (Odds Ratio)",
          column.labels = c("Chickens and other Poultry", "Goats and Sheep", "Cattle", "Whole Sample"),
          covariate.labels = c("formal education level score", "goes primarily to community animal health worker",
                               "goes primarily to public vet", "goes primarily to unqualified private vet",
                               "goes primarily to qualified private vet", "number of animals in flock / herd"),
          dep.var.labels = "Likelihood of Disease in Last 6 Months",
          style = "qje"
)

#effect of education and service provider on nontherapeutic amu
stargazer(reg33, reg34, reg35, reg36, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'text', out = "Outputs/education, health service provider, and nontherapeutic amu.csv",
          # title = "Table 5: Effect of Education Level and Primary Animal Health Service Provider
          # on Likelihood of Using Antibiotics Nontherapeutically",
          column.labels = c("Chickens and other Poultry", "Goats and Sheep", "Cattle", "Whole Sample"),
          covariate.labels = c("formal education level score", "goes primarily to community animal health worker",
                               "goes primarily to public vet", "goes primarily to unqualified private vet",
                               "goes primarily to qualified private vet", 
                               "professional provides diagnosis and treatment", "number of animals in flock / herd"),
          dep.var.labels = "Likelihood of Using Antibiotics Nontherapeutically",
          style = "qje"
)

stargazer(reg33, reg34, reg35, reg36, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'html', out = "Outputs/education, health service provider, and nontherapeutic amu.html",
          # title = "Table 5: Effect of Education Level and Primary Animal Health Service Provider
          # on Likelihood of Using Antibiotics Nontherapeutically",
          column.labels = c("Chickens and other Poultry", "Goats and Sheep", "Cattle", "Whole Sample"),
          covariate.labels = c("formal education level score", "goes primarily to community animal health worker",
                               "goes primarily to public vet", "goes primarily to unqualified private vet",
                               "goes primarily to qualified private vet", 
                               "professional provides diagnosis and treatment", "number of animals in flock / herd"),
          dep.var.labels = "Likelihood of Using Antibiotics Nontherapeutically",
          style = "qje"
)


# additional robustness checks --------------------------------------------

#does public vet access reduce disease independently of the effect on nontherapeutic AMU?
reg37 <- glm(disease_last6month ~ prophylactic_abx + public_vet + num_animals,
             family = "binomial", data = subset(df, species == "chickens"))

reg38 <- glm(disease_last6month ~ prophylactic_abx + public_vet + num_animals,
             family = "binomial", data = subset(df, species == "goats"))

reg39 <- glm(disease_last6month ~ prophylactic_abx + public_vet + num_animals,
             family = "binomial", data = subset(df, species == "cows"))

reg40 <- glm(disease_last6month ~ prophylactic_abx + public_vet + num_animals,
             family = "binomial", data = df)

stargazer(reg37, reg38, reg39, reg40, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'html', out = "Outputs/robustness.html",
          # title = "Effect of public vet use and prophylactic antibiotic use on likelihood of disease",
          column.labels = c("Chickens and other Poultry", "Goats and Sheep", "Cattle", "Whole Sample"),
          covariate.labels = c("uses antibiotics prophylactically",
                               "goes primarily to public vet",
                               "number of animals in flock / herd"),
          dep.var.labels = "Likelihood of Disease in Last 6 Months",
          style = "qje"
)

stargazer(reg37, reg38, reg39, reg40, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'text', out = "Outputs/robustness.csv",
          # title = "Effect of public vet use and prophylactic antibiotic use on likelihood of disease",
          column.labels = c("Chickens and other Poultry", "Goats and Sheep", "Cattle", "Whole Sample"),
          covariate.labels = c("uses antibiotics prophylactically",
                               "goes primarily to public vet",
                               "number of animals in flock / herd"),
          dep.var.labels = "Likelihood of Disease in Last 6 Months",
          style = "qje"
)

#redoing main results controlling for the species
# reg 8, reg 24, reg 36
#create a dummy for each species
df$chickens = 0
df$goats = 0
df$cows = 0
df$chickens[df$species == "chickens"] <- 1
df$goats[df$species == "goats"] <- 1
df$cows[df$species == "cows"] <- 1

reg8r <- glm(disease_last6month ~ prophylactic_abx + fattening_amu_attitude + num_animals +
               chickens + goats + cows,
            family = "binomial", data = df)

reg24r <- glm(disease_last6month ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + num_animals +
                chickens + goats + cows,
             family = "binomial", data = df)

reg36r <- glm(nontherapeutic_abx ~ education + comm_animal_health__worker + public_vet +
               unqual_priv_vet + qual_priv_vet + prof_diagn_treat + num_animals +
                chickens + goats + cows,
             family = "binomial", data = df)

stargazer(reg8, reg8r, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'html', out = "Outputs/AMU habits and disease robust.html",
          covariate.labels = c("uses antibiotics prophylactically",
                               "believes that antibiotics can be used for fattening",
                               "number of animals in flock / herd",
                               "chicken farm",
                               "goat or sheep farm",
                               "cow farm"),
          dep.var.labels = "Likelihood of Disease in Last 6 Months",
          style = "qje"
)

stargazer(reg8, reg8r, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'text', out = "Outputs/AMU habits and disease robust.csv",
          covariate.labels = c("uses antibiotics prophylactically",
                               "believes that antibiotics can be used for fattening",
                               "number of animals in flock / herd",
                               "chicken farm",
                               "goat or sheep farm",
                               "cow farm"),
          dep.var.labels = "Likelihood of Disease in Last 6 Months",
          style = "qje"
)

stargazer(reg24, reg24r, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'text', out = "Outputs/education, health service provider, and disease robust.csv",
          covariate.labels = c("formal education level score", "goes primarily to community animal health worker",
                               "goes primarily to public vet", "goes primarily to unqualified private vet",
                               "goes primarily to qualified private vet", "number of animals in flock / herd",
                               "chicken farm",
                               "goat or sheep farm",
                               "cow farm"),
          dep.var.labels = "Likelihood of Disease in Last 6 Months",
          style = "qje"
)

stargazer(reg24, reg24r, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'html', out = "Outputs/education, health service provider, and disease robust.html",
          covariate.labels = c("formal education level score", "goes primarily to community animal health worker",
                               "goes primarily to public vet", "goes primarily to unqualified private vet",
                               "goes primarily to qualified private vet", "number of animals in flock / herd",
                               "chicken farm",
                               "goat or sheep farm",
                               "cow farm"),
          dep.var.labels = "Likelihood of Disease in Last 6 Months",
          style = "qje"
)

stargazer(reg36, reg36r, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'html', out = "Outputs/education, health service provider, and nontherapeutic amu robust.html",
          covariate.labels = c("formal education level score", "goes primarily to community animal health worker",
                               "goes primarily to public vet", "goes primarily to unqualified private vet",
                               "goes primarily to qualified private vet", 
                               "professional provides diagnosis and treatment", "number of animals in flock / herd",
                               "chicken farm",
                               "goat or sheep farm",
                               "cow farm"),
          dep.var.labels = "Likelihood of Using Antibiotics Nontherapeutically",
          style = "qje"
)

stargazer(reg36, reg36r, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'text', out = "Outputs/education, health service provider, and nontherapeutic amu robust.csv",
          covariate.labels = c("formal education level score", "goes primarily to community animal health worker",
                               "goes primarily to public vet", "goes primarily to unqualified private vet",
                               "goes primarily to qualified private vet", 
                               "professional provides diagnosis and treatment", "number of animals in flock / herd",
                               "chicken farm",
                               "goat or sheep farm",
                               "cow farm"),
          dep.var.labels = "Likelihood of Using Antibiotics Nontherapeutically",
          style = "qje"
)

#redoing first robustness check controlling for species

reg40r <- glm(disease_last6month ~ prophylactic_abx + public_vet + num_animals +
                chickens + goats + cows,
              family = "binomial", data = df)

stargazer(reg40, reg40r, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'html', out = "Outputs/public vet and prophylactic AMU on disease robust.html",
          covariate.labels = c("uses antibiotics prophylactically", "goes primarily to public vet",
                               "number of animals in flock / herd",
                               "chicken farm",
                               "goat or sheep farm",
                               "cow farm"),
          dep.var.labels = "Likelihood of Disease in Last 6 Months",
          style = "qje"
)

stargazer(reg40, reg40r, 
          apply.coef = exp,
          t.auto = F,
          p.auto = F,
          report = "vct*",
          type = 'text', out = "Outputs/public vet and prophylactic AMU on disease robust.csv",
          covariate.labels = c("uses antibiotics prophylactically", "goes primarily to public vet",
                               "number of animals in flock / herd",
                               "chicken farm",
                               "goat or sheep farm",
                               "cow farm"),
          dep.var.labels = "Likelihood of Disease in Last 6 Months",
          style = "qje"
)