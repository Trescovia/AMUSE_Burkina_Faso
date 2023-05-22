# Load Packages -----------------------------------------------------------
library("haven")
library("here")
library("plyr")
library("labelled")
library("xlsx")


# load dataset ------------------------------------------------------------

df <- read.xlsx(here::here("Main dataset.xlsx"), "Sheet1")


# check number of farms with each species ---------------------------------

#216 total farms

dfbroiler <- df[df$q16a == "Volaille" & !is.na(df$q16a) | df$q16b == "Volaille" & !is.na(df$q16b) | 
                  df$q16c == "Volaille" & !is.na(df$q16c) | df$q16d == "Volaille" & !is.na(df$q16d) |
                  df$q16e == "Volaille" & !is.na(df$q16e),] #all 216


dflayer <- df[df$q16a == "Ovins" & !is.na(df$q16a) | df$q16b == "Ovins" & !is.na(df$q16b) | 
                  df$q16c == "Ovins" & !is.na(df$q16c) | df$q16d == "Ovins" & !is.na(df$q16d) |
                  df$q16e == "Ovins" & !is.na(df$q16e),] #54

dfpig <- df[df$q16a == "Porcins" & !is.na(df$q16a) | df$q16b == "Porcins" & !is.na(df$q16b) | 
                df$q16c == "Porcins" & !is.na(df$q16c) | df$q16d == "Porcins" & !is.na(df$q16d) |
                df$q16e == "Porcins" & !is.na(df$q16e),] #only 6

dfcow <- df[df$q16a == "Bovins" & !is.na(df$q16a) | df$q16b == "Bovins" & !is.na(df$q16b) | 
                df$q16c == "Bovins" & !is.na(df$q16c) | df$q16d == "Bovins" & !is.na(df$q16d) |
                df$q16e == "Bovins" & !is.na(df$q16e),] #49

dfrabbit <- df[df$q16a == "Lapin" & !is.na(df$q16a) | df$q16b == "Lapin" & !is.na(df$q16b) | 
              df$q16c == "Lapin" & !is.na(df$q16c) | df$q16d == "Lapin" & !is.na(df$q16d) |
              df$q16e == "Lapin" & !is.na(df$q16e),] #2

dfgoat <- df[df$q16a == "Caprins" & !is.na(df$q16a) | df$q16b == "Caprins" & !is.na(df$q16b) | 
                 df$q16c == "Caprins" & !is.na(df$q16c) | df$q16d == "Caprins" & !is.na(df$q16d) |
                 df$q16e == "Caprins" & !is.na(df$q16e),] #20

dfhorsedonkey <- df[df$q16a == "Equins ou Asins" & !is.na(df$q16a) | df$q16b == "Equins ou Asins" & !is.na(df$q16b) | 
               df$q16c == "Equins ou Asins" & !is.na(df$q16c) | df$q16d == "Equins ou Asins" & !is.na(df$q16d) |
               df$q16e == "Equins ou Asins" & !is.na(df$q16e),] #6

dfchickens <- df[df$q16a == "Volaille" & !is.na(df$q16a) | df$q16a == "Ovins" & !is.na(df$q16a) |
                   df$q16b == "Volaille" & !is.na(df$q16b) | df$q16b == "Ovins" & !is.na(df$q16b) |
                   df$q16c == "Volaille" & !is.na(df$q16c) | df$q16c == "Ovins" & !is.na(df$q16c) |
                   df$q16d == "Volaille" & !is.na(df$q16d) | df$q16d == "Ovins" & !is.na(df$q16d) |
                   df$q16e == "Volaille" & !is.na(df$q16e) | df$q16e == "Ovins" & !is.na(df$q16e),] #trivially, all 216

#No camels

#' ecause chickens were the only species present on more than 50 farms, we will not be able to do
#' specific analysis on each species. Instead, we will do a) just chickens and b) all species together


# create a var for flock / herd size --------------------------------------
df$num_chickens <- 0
df$num_cows <- 0
df$num_goats <- 0
df$num_horsedonkey <- 0
df$num_pigs <- 0 
df$num_rabbits <- 0

df$num_cows <- df$q17a + df$q17b + df$q17c + df$q17d
df$num_chickens <- df$q33a + df$q33b + df$q33c + df$q33autre
df$num_pigs <- df$q48a + df$q48b + df$q48c + df$q48d
df$num_goats <- df$q61a + df$q61b + df$q61c + df$q61d + df$q61e + df$q61f
df$num_horsedonkey <- df$q94a + df$q94b + df$q94c
df$num_rabbits <- df$q109a + df$q109b + df$q109c

#' because of the small number of farms with pigs, rabbits and horses / donkeys,
#' we will just include goats/sheep, chickens and cows from here on


# create a var for having each species ------------------------------------
df$has_cows <- 0
df$has_chickens <- 0
df$has_goats <- 0
df$has_pigs <- 0
df$has_rabbits <- 0
df$has_horsedonkey <- 0

df$has_cows[df$num_cows > 0 & !is.na(df$num_cows)] <- 1 #49 was correct
df$has_goats[df$num_goats > 0 & !is.na(df$num_goats)] <- 1 #actually 59, not 20
df$has_chickens[df$num_chickens > 0 & !is.na(df$num_chickens)] <- 1 # actually 212, not 216
df$has_pigs[df$num_pigs > 0 & !is.na(df$num_pigs)] <- 1 #actually 5, not 6
df$has_rabbits[df$num_rabbits > 0 & !is.na(df$num_rabbits)] <- 1 #2 is correct
df$has_horsedonkey[df$num_horsedonkey > 0 & !is.na(df$horsedonkey)] <- 1 #apparently none had?

rm(dfbroiler, dflayer, dfchickens, dfcow, dfgoat, dfhorsedonkey, dfpig, dfrabbit)


# create variable for recent disease (by species) -------------------------
# 27 for cows, 42 for chickens, 72 for goats


#cows
df$disease_lastmonth_cows <- NA
df$disease_lastmonth_cows[df$has_cows == 1] <- 0
df$disease_lastmonth_cows[df$q27 == "Il y a moins d�un (1) mois"] <- 1
#5 in last month

df$disease_last6month_cows <- NA
df$disease_last6month_cows[df$has_cows == 1] <- 0
df$disease_last6month_cows[df$q27 == "Il y a moins d�un (1) mois" 
                           | df$q27 == "Il y a 1-6 mois"] <- 1
#14 in last 6 months

df$disease_lastyear_cows <- NA
df$disease_lastyear_cows[df$has_cows == 1] <- 0
df$disease_lastyear_cows[df$q27 == "Il y a moins d�un (1) mois" 
                         | df$q27 == "Il y a 1-6 mois" 
                         | df$q27 == "Il y a7-12 mois"] <- 1
#17 in last year

df$disease_alltime_cows <- NA
df$disease_alltime_cows[df$has_cows == 1] <- 0
df$disease_alltime_cows[df$q27 == "Il y a moins d�un (1) mois" 
                        | df$q27 == "Il y a 1-6 mois" 
                        | df$q27 == "Il y a7-12 mois"
                        | df$q27 == "Il y a plus de douze (12) mois"] <- 1
#24 ever, 25 never



#chickens
df$disease_lastmonth_chickens <- NA
df$disease_lastmonth_chickens[df$has_chickens == 1] <- 0
df$disease_lastmonth_chickens[df$q42 == "Il y a moins d�un (1) mois"] <- 1
#72 in last month

df$disease_last6month_chickens <- NA
df$disease_last6month_chickens[df$has_chickens == 1] <- 0
df$disease_last6month_chickens[df$q42 == "Il y a moins d�un (1) mois" 
                           | df$q42 == "Il y a 1-6 mois"] <- 1
#186 in last 6 months

df$disease_lastyear_chickens <- NA
df$disease_lastyear_chickens[df$has_chickens == 1] <- 0
df$disease_lastyear_chickens[df$q42 == "Il y a moins d�un (1) mois" 
                         | df$q42 == "Il y a 1-6 mois" 
                         | df$q42 == "Il y a7-12 mois"] <- 1
#193 in last year

df$disease_alltime_chickens <- NA
df$disease_alltime_chickens[df$has_chickens == 1] <- 0
df$disease_alltime_chickens[df$q42 == "Il y a moins d�un (1) mois" 
                        | df$q42 == "Il y a 1-6 mois" 
                        | df$q42 == "Il y a7-12 mois"
                        | df$q42 == "Il y a plus de douze (12) mois"] <- 1
#197 ever,19 never


#goats
df$disease_lastmonth_goats <- NA
df$disease_lastmonth_goats[df$has_goats == 1] <- 0
df$disease_lastmonth_goats[df$q72 == "Il y a moins d�un (1) mois"] <- 1
#3 in last month

df$disease_last6month_goats <- NA
df$disease_last6month_goats[df$has_goats == 1] <- 0
df$disease_last6month_goats[df$q72 == "Il y a moins d�un (1) mois" 
                               | df$q72 == "Il y a 1-6 mois"] <- 1
#10 in last 6 months

df$disease_lastyear_goats <- NA
df$disease_lastyear_goats[df$has_goats == 1] <- 0
df$disease_lastyear_goats[df$q72 == "Il y a moins d�un (1) mois" 
                             | df$q72 == "Il y a 1-6 mois" 
                             | df$q72 == "Il y a7-12 mois"] <- 1
#15 in last year

df$disease_alltime_goats <- NA
df$disease_alltime_goats[df$has_goats == 1] <- 0
df$disease_alltime_goats[df$q72 == "Il y a moins d�un (1) mois" 
                            | df$q72 == "Il y a 1-6 mois" 
                            | df$q72 == "Il y a7-12 mois"
                            | df$q72 == "Il y a plus de douze (12) mois"] <- 1
#21 ever,38 never

#the difference in likelihood of sickness between species might make comparison difficult

# make a var for use of drugs intended for humans in animals --------------
df$human_drugs_farmwide <- 0
df$human_drugs_farmwide[df$q144 == "Oui"] <- 1

#literally only one farm - not suitable for use in this analysis
#looking at species-specific vars (df$q23medichum2, df$q38medichum2, df$q52medichum2, df$q68medichum2, df$q114medichum2) confirms this


# make a var for use of nontherapeutic antibiotics ------------------------

#goats

df$nontherapeutic_abx_goats <- NA
df$nontherapeutic_abx_goats[df$has_goats == 1] <- 0
#68

df$nontherapeutic_abx_goats[
  df$q68amin2 == "Pr�vention" | df$q68amin2 == "Embouche" |
    df$q68autreanti3 == "Pr�vention" | df$q68autreanti3 == "Embouche" |
    df$q68fluo2 == "Pr�vention" | df$q68fluo2 == "Embouche" |
    df$q68macr2a == "Pr�vention" | df$q68macr2a == "Embouche" |
    df$q68macr2b == "Pr�vention" | df$q68macr2b == "Embouche" |
    df$q68sulph2 == "Pr�vention" | df$q68sulph2== "Embouche" |
    df$q68tetra2a == "Pr�vention" | df$q68tetra2a == "Embouche" |
    df$q68tetra2b == "Pr�vention" | df$q68tetra2b == "Embouche" |
    df$q68penc2a == "Pr�vention" | df$q68penc2a == "Embouche" |
    df$q68penc2b == "Pr�vention" | df$q68penc2b == "Embouche" 
] <- 1 #13/59


#cows
df$nontherapeutic_abx_cows <- NA
df$nontherapeutic_abx_cows[df$has_cows == 1] <- 0
#23

df$nontherapeutic_abx_cows[
  df$q23amin2 == "Pr�vention" | df$q23amin2 == "Embouche" |
    df$q23autreanti3 == "Pr�vention" | df$q23autreanti3 == "Embouche" |
    df$q23fluo2 == "Pr�vention" | df$q23fluo2 == "Embouche" |
    df$q23macr2a == "Pr�vention" | df$q23macr2a == "Embouche" |
    df$q23macr2b == "Pr�vention" | df$q23macr2b == "Embouche" |
    df$q23sulph2 == "Pr�vention" | df$q23sulph2== "Embouche" |
    df$q23tetra2a == "Pr�vention" | df$q23tetra2a == "Embouche" |
    df$q23tetra2b == "Pr�vention" | df$q23tetra2b == "Embouche" |
    df$q23penc2a == "Pr�vention" | df$q23penc2a == "Embouche" |
    df$q23penc2b == "Pr�vention" | df$q23penc2b == "Embouche" 
] <- 1 #10/49


#chickens
df$nontherapeutic_abx_chickens <- NA
df$nontherapeutic_abx_chickens[df$has_chickens == 1] <- 0
#38

df$nontherapeutic_abx_chickens[
  df$q38amin2a == "Pr�vention" | df$q38amin2a == "Embouche" |
    df$q38amin2b == "Pr�vention" | df$q38amin2b == "Embouche" |
    df$q38autreanti3 == "Pr�vention" | df$q38autreanti3 == "Embouche" |
    df$q38fluo2a == "Pr�vention" | df$q38fluo2a == "Embouche" |
    df$q38fluo2b == "Pr�vention" | df$q38fluo2b == "Embouche" |
    df$q38macr2a == "Pr�vention" | df$q38macr2a == "Embouche" |
    df$q38macr2b == "Pr�vention" | df$q38macr2b == "Embouche" |
    df$q38sulph2a == "Pr�vention" | df$q38sulph2a == "Embouche" |
    df$q38sulph2b == "Pr�vention" | df$q38sulph2b == "Embouche" |
    df$q38tetra2a == "Pr�vention" | df$q38tetra2a == "Embouche" |
    df$q38tetra2b == "Pr�vention" | df$q38tetra2b == "Embouche" |
    df$q38tetra2c == "Pr�vention" | df$q38tetra2c == "Embouche" |
    df$q38penc2a == "Pr�vention" | df$q38penc2a == "Embouche" |
    df$q38penc2b == "Pr�vention" | df$q38penc2b == "Embouche" 
] <- 1 #183 / 214



# Make a var for use of prophylactic antibiotics --------------------------

#goats

df$prophylactic_abx_goats <- NA
df$prophylactic_abx_goats[df$has_goats == 1] <- 0
#68

df$prophylactic_abx_goats[
  df$q68amin2 == "Pr�vention" |
    df$q68autreanti3 == "Pr�vention" |
    df$q68fluo2 == "Pr�vention" |
    df$q68macr2a == "Pr�vention" |
    df$q68macr2b == "Pr�vention" |
    df$q68sulph2 == "Pr�vention" |
    df$q68tetra2a == "Pr�vention" |
    df$q68tetra2b == "Pr�vention" |
    df$q68penc2a == "Pr�vention" |
    df$q68penc2b == "Pr�vention" 
] <- 1 #13/59


#cows
df$prophylactic_abx_cows <- NA
df$prophylactic_abx_cows[df$has_cows == 1] <- 0
#23

df$prophylactic_abx_cows[
  df$q23amin2 == "Pr�vention" |
    df$q23autreanti3 == "Pr�vention" |
    df$q23fluo2 == "Pr�vention" |
    df$q23macr2a == "Pr�vention" |
    df$q23macr2b == "Pr�vention" |
    df$q23sulph2 == "Pr�vention" |
    df$q23tetra2a == "Pr�vention" |
    df$q23tetra2b == "Pr�vention" |
    df$q23penc2a == "Pr�vention" |
    df$q23penc2b == "Pr�vention"
] <- 1 #10/49


#chickens
df$prophylactic_abx_chickens <- NA
df$prophylactic_abx_chickens[df$has_chickens == 1] <- 0
#38

df$prophylactic_abx_chickens[
  df$q38amin2a == "Pr�vention" |
    df$q38amin2b == "Pr�vention" |
    df$q38autreanti3 == "Pr�vention" |
    df$q38fluo2a == "Pr�vention" |
    df$q38fluo2b == "Pr�vention" |
    df$q38macr2a == "Pr�vention" |
    df$q38macr2b == "Pr�vention" |
    df$q38sulph2a == "Pr�vention" |
    df$q38sulph2b == "Pr�vention" |
    df$q38tetra2a == "Pr�vention" |
    df$q38tetra2b == "Pr�vention" |
    df$q38tetra2c == "Pr�vention" |
    df$q38penc2a == "Pr�vention" |
    df$q38penc2b == "Pr�vention"
] <- 1 #183/214

#note that for all three animals, the 'prophylactic use' var is the same as the 'nontherapeutic use' var


# create by-species vars for frequency of vaccination ---------------------

#goats
#68
df$frequency_vaccination_goats <- NA
df$frequency_vaccination_goats[df$has_goats == 1] <- 0

#' after exploring the var, it seems like 28/59 had vaccinated in the past 4 weeks.
#' Of those, all 28 used vaccines preventatively, and 11 of those also used them 
#' for treatment. This makes it a bit difficult, because there could be endogeneity 
#' between disease and vaccination. We could look at just those who vaccinated for
#' prevention only, but this might introduce bias because this might just show us 
#' the number of farms who happened to have disease in the last 4 weeks
#' If we had data on vaccination in the past year, this would be a different story
#' For this reason, we might instead look at attitudes towards vaccination and 
#' antibiotic use, using questions 136 and 137


# make vars for believing in nontherapeutic vacc and amu ------------------

df$fattening_vacc_attitude <- 0
df$fattening_vacc_attitude[df$q136c == "Oui"] <- 1

df$fattening_amu_attitude <- 0
df$fattening_amu_attitude[df$q137c == "Oui"] <- 1

df$prophylactic_amu_attitude <- 0
df$prophylactic_amu_attitude[df$q137b == "Oui"] <- 1


# make a var for exposure to campaigns ------------------------------------

df$campaigns <- 0
df$campaigns[df$q134d == "Oui"] <- 1
#unfortunately, only 7 farms took part in campaigns, so we won't be able to use this variable :/


# make a var for education ------------------------------------------------

#' there are vars for formal education (q149), informal education (q149nonformelle), and 
#' professional education (q149professionnelle)
#' only two had professional education, and only four had informal education (arabic or franco-arabic school)
#' because of the low number of people having informal and professional education, we will look only
#' at the years of formal education (as a factor variable)

df$education <- as.numeric(0)
df$education[df$q149 == "Alphab�tisation"] <- 1
df$education[df$q149 == "Niveau primaire (CP1-CM2)"] <- 2
df$education[df$q149 == "Niveau secondaire (6e-Te)"] <- 3
df$education[df$q149 == "Niveau universitaire ()"] <- 4
df$education[df$q149 == "Formation professionnelle (Pr�cisez) __"] <- NA
df$education[df$q149 == "Education non-formelle durant_ann�es"] <- NA

#' note that we are assigning a score to their education level and treating this 
#' score as a numeric rather than a factor. Note also that there were six repondents
#' who did not report having formal education but did report having informal education
#' or a professional qualification. Unfortunately, because these cannot be directly
#' compared to specific amounts of formal education, we did not assign them a score.
#' We did not simply give them a score of zero, because it was unclear whether or not
#' these respondents had another level of formal education (e.g. literacy, primary), 
#' and since one of them reported being a trained teacher it seemed unwise to treat
#' these six people as having zero education


# make a var for professionals providing animal health services -----------

#' it seems like every single respondent went to some kind of specialist for
#' animal health services, which is obviously great but also mean that we can't 
#' separate farms based only on that. There were five specific types of specialist
#' mentioned, and four with significant numbers of respondents going to them
#' probably create four variables, one for each of those categories

df$public_vet <- 0
df$public_vet[df$q134a == "V�t�rinaire public" ] <- 1

df$comm_animal_health__worker <- 0
df$comm_animal_health__worker[df$q134a == "Travailleur Communautaire en sant� animale" ] <- 1

df$qual_priv_vet <- 0
df$qual_priv_vet[df$q134a == "V�t�rinaire priv� (qualifi�)" ] <- 1

df$unqual_priv_vet <- 0
df$unqual_priv_vet[df$q134a == "V�t�rinaire priv� (qualification inconnue)" ] <- 1


# create a var for going to a vet for diagnosis and treatment -------------
#q30 for cows, q45 for chickens, q75 for goats

#' 21 / 49 cow farmers went to a professional (community animal health worker,
#' private or public vet). The figure for chickens was 136 / 216, and the figure
#' for goats was 16 / 59

df$prof_diagn_treat_cows <- NA
df$prof_diagn_treat_cows[df$has_cows == 1] <- 0
df$prof_diagn_treat_cows[df$q75 == "Travailleur communautaire en sant� animale" 
                         | df$q30 == "V�t�rinaire priv�" 
                         | df$q30 == "V�t�rinaire public"] <- 1

df$prof_diagn_treat_chickens <- NA
df$prof_diagn_treat_chickens[df$has_chickens == 1] <- 0
df$prof_diagn_treat_chickens[df$q45 == "Travailleur communautaire en sant� animale" 
                         | df$q45 == "V�t�rinaire priv�" 
                         | df$q45 == "V�t�rinaire public"] <- 1

df$prof_diagn_treat_goats <- NA
df$prof_diagn_treat_goats[df$has_goats == 1] <- 0
df$prof_diagn_treat_goats[df$q75 == "Travailleur communautaire en sant� animale" 
                         | df$q75 == "V�t�rinaire priv�" 
                         | df$q75 == "V�t�rinaire public"] <- 1



# summary stats of variables collected at the farm level ------------------

table(df$fattening_amu_attitude)

table(df$education)

table(df$comm_animal_health__worker)
table(df$public_vet)
table(df$unqual_priv_vet)
table(df$qual_priv_vet)

# create separate datasets ------------------------------------------------

dfgoats <- df[df$has_goats == 1,]
dfchickens <- df[df$has_chickens == 1,]
dfcows <- df[df$has_cows == 1,]

# select vars -------------------------------------------------------------

dfchickens <- dfchickens[,c("num_chickens", "disease_lastmonth_chickens",
                            "disease_last6month_chickens", "disease_lastyear_chickens",
                            "disease_alltime_chickens", "human_drugs_farmwide",
                            "nontherapeutic_abx_chickens", "prophylactic_abx_chickens",
                            "fattening_vacc_attitude", "fattening_amu_attitude",
                            "prophylactic_amu_attitude", "education",
                            "public_vet", "comm_animal_health__worker",
                            "qual_priv_vet", "unqual_priv_vet",
                            "prof_diagn_treat_chickens")]

dfgoats <- dfgoats[,c("num_goats", "disease_lastmonth_goats",
                            "disease_last6month_goats", "disease_lastyear_goats",
                            "disease_alltime_goats", "human_drugs_farmwide",
                            "nontherapeutic_abx_goats", "prophylactic_abx_goats",
                            "fattening_vacc_attitude", "fattening_amu_attitude",
                            "prophylactic_amu_attitude", "education",
                            "public_vet", "comm_animal_health__worker",
                            "qual_priv_vet", "unqual_priv_vet",
                            "prof_diagn_treat_goats")]

dfcows <- dfcows[,c("num_cows", "disease_lastmonth_cows",
                            "disease_last6month_cows", "disease_lastyear_cows",
                            "disease_alltime_cows", "human_drugs_farmwide",
                            "nontherapeutic_abx_cows", "prophylactic_abx_cows",
                            "fattening_vacc_attitude", "fattening_amu_attitude",
                            "prophylactic_amu_attitude", "education",
                            "public_vet", "comm_animal_health__worker",
                            "qual_priv_vet", "unqual_priv_vet",
                            "prof_diagn_treat_cows")]

# rename species-specific vars in indiv dataframes ------------------------

dfchickens <- plyr::rename(dfchickens,
                       c("num_chickens" = "num_animals",
                         "disease_lastmonth_chickens" = "disease_lastmonth",
                         "disease_last6month_chickens" = "disease_last6month",
                         "disease_lastyear_chickens" = "disease_lastyear",
                         "disease_alltime_chickens" = "disease_alltime",
                         "nontherapeutic_abx_chickens" = "nontherapeutic_abx",
                         "prophylactic_abx_chickens" = "prophylactic_abx",
                         "prof_diagn_treat_chickens" = "prof_diagn_treat"
                         ))

dfcows <- plyr::rename(dfcows,
                           c("num_cows" = "num_animals",
                             "disease_lastmonth_cows" = "disease_lastmonth",
                             "disease_last6month_cows" = "disease_last6month",
                             "disease_lastyear_cows" = "disease_lastyear",
                             "disease_alltime_cows" = "disease_alltime",
                             "nontherapeutic_abx_cows" = "nontherapeutic_abx",
                             "prophylactic_abx_cows" = "prophylactic_abx",
                             "prof_diagn_treat_cows" = "prof_diagn_treat"
                           ))

dfgoats <- plyr::rename(dfgoats,
                           c("num_goats" = "num_animals",
                             "disease_lastmonth_goats" = "disease_lastmonth",
                             "disease_last6month_goats" = "disease_last6month",
                             "disease_lastyear_goats" = "disease_lastyear",
                             "disease_alltime_goats" = "disease_alltime",
                             "nontherapeutic_abx_goats" = "nontherapeutic_abx",
                             "prophylactic_abx_goats" = "prophylactic_abx",
                             "prof_diagn_treat_goats" = "prof_diagn_treat"
                           ))


# add species marker ------------------------------------------------------

dfchickens$species <- "chickens"
dfgoats$species <- "goats"
dfcows$species <- "cows"

# recombine dataframes into master, and save ------------------------------

df <- rbind(dfchickens, dfcows, dfgoats)
write.xlsx(df, "cleaned dataset.xlsx", replace)





