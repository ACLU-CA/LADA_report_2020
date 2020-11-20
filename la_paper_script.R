################################################################################
# Introduction to this script
################################################################################

# This script runs all the analysis for data shown in the paper. It requires
# the object "data" which is produced by the script la_data.R
# That script is run in the first line of code here, or could be run separately
# and the first line be skipped here. In either case, the `data` dataframe there
# is required for this script.

# To find how a specific statistic was calculated, you can search this script
# for the statistic or words around the stat in the paper. Quotes from the paper
# are indicated by four hashes ####. Sections of the paper are separated with
# an entire line of hashes, the title of the section, and another line of hashes.
# Subsections are separated with the subsection name followed by a line of hashes.
# Single hashes refer to a comment about the code, not the paper


################################################################################
# Setup
################################################################################

# run the script which creates the dataframe with all 2017-2018 charges
source("la_data.R")

# load gridExtra
if (!require("pacman")) install.packages("pacman")
pacman::p_load(gridExtra) # for plots



# define a function that gives you a frequency table (count()), sorted by
# most frequent, and displays the percent of the total for each item
count2 <- function(..., sort = T) {
  count(..., sort = sort) %>%
    mutate(pct = n/sum(n)*100)
}


################################################################################
# Executive Summary
################################################################################

#### "Data provided by the LADA’s office reveals that of the 510,996 charges the LADA's office prosecuted in 2017 and 2018," 
nrow(data) # each row of the dataset is a charge, so number of rows is number of charges

#### "almost 60% were misdemeanors, "
nrow(data %>% 
       filter(charge_level == "M")) / # filter for only misdemeanors
  nrow(data) # divide by total number of un-filtered rows

#### "60.5% were charges that are minor enough that the ACLU recommends DAs..."
nrow(data %>% 
       filter(low_level == T)) / # filter for ACLU's "pre-plea diversion / decline to charge" list
  nrow(data)# divide by total number of un-filtered rows

#### "only 9.6% were serious or violent felonies"
nrow(data %>% 
       filter(svf == T)) / # filter for serious / violent felonies
  nrow(data)# divide by total number of un-filtered rows

#### "less than 1% of people prosecuted by her office in 2017-18 had any of their charges diverted"

diversion <- data %>% # create dataframe where
  group_by(person_id) %>% # each person is a row
  summarise(diverted = sum(result_description == "Diversion", na.rm = T), # and we calculate the # of charges that were filed against that person that were diverted
            num_charges = n()) %>% # also calculate the number of charges that person faced 
  mutate(pct_crgs_diverted = diverted/num_charges) # then calculate the % of charges diverted

# now can calculate how many people had at least one charge (>0) diverted
nrow(diversion %>% # take the diversion dataframe created above
       filter(diverted > 0))/ # filter for anyone who had more than zero charges diverted
  length(unique(alldata$person_id)) # divide by the total number of unique people

rm(diversion) # remove object

################################################################################
# Methodology
################################################################################

#### "charge city (99.8% of which was missing)"
nrow(data %>% filter(is.na(charge_city))) / nrow(data)

#### "zip code (40.6% of which was missing)"
nrow(data %>% filter(is.na(minor_zip_code))) / nrow(data)


################################################################################
# Charging Decisions
################################################################################

#### "charge city (99.8% of which was missing)"
nrow(data %>% filter(is.na(charge_city))) / nrow(data)

#### "zip code (40.6% of which was missing)"
nrow(data %>% filter(is.na(minor_zip_code))) / nrow(data)

#### "60% of the charges brought by the office are misdemeanor charges, while only 33% are 
#### felonies, of which a mere 9.1% are classified as serious or violent felonies"

#### "Figure 1: Frequency of different charge types"

charge_types <- data %>% 
  mutate(felony = ifelse(charge_level == "F", T, F), # create felony dummy
         misdemeanor = ifelse(charge_level == "M", T, F), # create misdo dummy
         infraction = ifelse(charge_level == "I", T, F)) %>% # create infraction dummy
  summarise(`Decline to charge or \n pre-plea diversion  ` = (sum(low_level)/n()), # calculate % of charges that are on ACLU's "decline to charge / pre-plea diversion" list
            `Felonies` = (sum(felony, na.rm = T)/n()), # calculate % of charges that are felonies
            `Misdemeanors` = (sum(misdemeanor, na.rm = T)/n()), # calculate % of charges that are misdos
            `Infractions` = (sum(infraction, na.rm = T)/n()), # calculate % of charges that are infractions
            n = n() # calculate total # of charges
  ) %>%
  pivot_longer(cols = -n, names_to = "type", values_to = "pct") %>% # make dataframe long instead of wide
  mutate(n = n*pct, # calculate the number of charges for each category
         # create groups for coloring chart. "B" will be grey, "A" will be blue
         group = case_when(type == "Serious or Violent Felonies \n (adults only)" | 
                             type ==  "Decline to charge or \n pre-plea diversion  " ~ "B",
                           TRUE ~ "A")) %>%
  select(type, pct, group, n) %>%
  # serious and violent felonies should only include adults:
  rbind(data %>%
          filter(data_part == "adult_misdemeanors" | data_part == "adult_felonies") %>% # filter for only adults
          summarise(`Serious or Violent Felonies \n (adults only)` = (sum(svf)/n()), # calculate % SVF
                    n = n() # calculate total number of charges against adults
          ) %>%
          pivot_longer(cols = -n, names_to = "type", values_to = "pct") %>% # make long instead of wide
          mutate(n = n*pct, # calculate # of SVF charges against adults
                 # create groups for coloring chart. "B" will be grey, "A" will be blue
                 group = case_when(type == "Serious or Violent Felonies \n (adults only)" | 
                                     type ==  "Decline to charge or \n pre-plea diversion  " ~ "B",
                                   TRUE ~ "A"))%>%
          select(type, pct, group, n))

# create factors to order the barsin the chart
charge_types$type <- 
  factor(charge_types$type,levels = 
           c("Felonies", "Misdemeanors", "Infractions", "Serious or Violent Felonies \n (adults only)", "Decline to charge or \n pre-plea diversion  ", "n"))

# select colors
group.colors <- c(A = "#80d8f0", B = "grey")

# plot
ggplot(charge_types, 
       aes(y=pct*100, 
           x=type, 
           fill = group)) + 
  geom_bar(stat="identity", show.legend = F) +
  ggtitle("Frequency of charge types") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=group.colors) +
  ylab("Percent of All Charges") +
  xlab("") +
  geom_text(
    aes(label= paste0(sprintf("%0.1f", round(pct*100, digits = 1)), "%", " \n ", "(", format(n, big.mark = ","), " charges) ") ), 
    position=position_dodge(width=0.9), 
    vjust=.5)

rm(list = c("charge_types", "group.colors")) # remove objects from workspace



#### "Figure 1: Frequency of different charge types. All charges are either felonies, 
#### misdemeanors, or infractions. All serious or violent felonies are felonies, 
#### while 73.5% of  of the “Decline to charge or pre-plea diversion” charges are
#### misdemeanors, 22.3% are felonies, and 4.2% are infractions."

count2(data %>% filter(low_level == T), # filter for only the “Decline to charge or pre-plea diversion” charges
       charge_level) # get frequency table of charge levels



#### "Figure 2: The ten charges most frequently prosecuted by the LADA’s office 
#### on the ACLU’s “Decline to Charge / Pre-plea Diversion” list."

data %>% 
  filter(low_level == T) %>% # filter for only charges on the list
  count(offense_description, sort = T) %>% # get frequency table of the charges, sorted most to least
  mutate(pct_chrgs = n/nrow(data)*100) %>% # determine what percent of all charges each is
  head(10) # only view top 10


#### "51% of all charges brought by the LADA’s office in 2017-18 were dismissed"
nrow(data %>% # determine number of rows (which equals the number of charges)
       filter(result_description == "Dismissed")) / # for only those charges that were dismissed
  nrow(data) # divide by total # of charges (rows)

#### "(49% of felonies, 51% of misdemeanors, and 64% of infractions were dismissed."
data %>%
  group_by(charge_level) %>% # do following actions separately for felony, misdo, and infractions
  count2(result_description) %>% # frequency table of the charge result (dismissed, convicted, etc.)
  filter(result_description == "Dismissed") # only look at charges that are dismissed


#### "Over 23% of people in all cases had all their charges dismissed"
dismissals <- # create dataframe where each person within a case is a row
  data %>% 
  mutate(dismissed = ifelse(result_description == "Dismissed", 1, 0)) %>% # create "dismissed" dummy
  group_by(person_id) %>% # group by person
  summarise(pct_dismissed = sum(dismissed/n())) # calculate, for each person, the % of charges dismissed

length(unique((dismissals %>% filter(pct_dismissed == 1))$person_id)) / # number of people with all charges dismissed
  length(unique(data$person_id)) # divided by total number of people

#### "and 59% had more than half of their charges dismissed." 
length(unique((dismissals %>% filter(pct_dismissed >= 0.5))$person_id)) / # number of people with at least 50% charges dismissed
  length(unique(data$person_id)) # divided by total number of people


#### "This means that as many as 61,699 legally innocent people had their lives disrupted by being brought into criminal 
#### proceedings by the LA District Attorney’s office, only to have all of their charges dropped"
dismissals %>%
  filter(pct_dismissed ==1) %>% # filter for only those who have all charges dropped
  pull(person_id) %>% # pull the person_id column
  length() # tells us how many people there are


#### "A full 156,954 people in the LADA data we analyzed had at least half of their charges dropped."
length(unique((dismissals %>% # number of unique person_ids that...
                 filter(pct_dismissed >= 0.5))$person_id)) # ...have a dismissal percentage of at least 50%

rm(dismissals) # remove object from workspace


#### "Most frequently dismissed charges"
#### "Figure 3: Top 10 most frequently dismissed charges, out of the top 50 most-prosecuted charges"
data %>% 
  group_by(code_section_m_f) %>% # grouping by charge
  count(result_description) %>% # get frequency table for the results (convicted, dismissed, etc) for each charge
  mutate(pct = n/sum(n)) %>% # create a percent column, to see what % are convicted, dismissed, etc.
  filter(result_description == "Dismissed") %>% # only look at dismissed charges
  ungroup() %>%
  filter(code_section_m_f %in% # only include top 50 most common charges
           (head(data %>% count(code_section_m_f) %>% arrange(-n), 50))$code_section_m_f) %>%
  arrange(-pct) %>% # sort most dismissed to least dismissed
  head(10) # only view top 10




#### "Case-level severity"
#### "Figure 4: Case-level severity. Although figure 1 shows a similar graph..."

case_types_data <- data %>%
  group_by(case_id) %>% # for each case...
  slice(1) %>% # ... take the first row (since all rows within a case have the same severity-level)
  ungroup() %>%
  mutate(felony = ifelse(case_level == "F", T, F), # create dummy for felony-level
         misdemeanor = ifelse(case_level == "M", T, F)) %>% # dummy for misdo-level
  summarise(
    `Felonies` = (sum(felony, na.rm = T)/n()), # determine what percent are felonies
    `Misdemeanors` = (sum(misdemeanor, na.rm = T)/n()), # and what % are misdos
    n = n() # total number of cases
  ) %>%
  pivot_longer(cols = everything(), # make dataframe long instead of wide
               names_to = "type", 
               values_to = "pct")

# plot
ggplot((case_types_data) %>% filter(type != "n"), 
       aes(y=pct*100, 
           x=type,
           fill = "#80d8f0")) + 
  geom_bar(stat="identity", show.legend = F) +
  ggtitle("Case-level severity") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.4)) +
  scale_fill_manual(values="#80d8f0") +
  ylab("Percent of All Cases") +
  xlab("") +
  geom_text(
    aes(label= paste0(sprintf("%0.1f", round(pct*100, digits = 1)), "%", " \n ", "(", format(pct*253075, big.mark = ","), " cases) ") ), 
    position=position_dodge(width=0.9), 
    vjust=.5)

rm(case_types_data) # remove object from workspace


#### "The office prosecuted 173,740 misdemeanor cases, 
data %>% 
  group_by(case_id) %>%
  slice(1) %>% # take the first row (since all rows within a case have the same severity-level)
  filter(case_level == "M") %>% # filter for only misdemeanor cases
  nrow() # determine the # of cases
#### representing as many as 179,552 people."
data %>% 
  group_by(case_id, defendant_no) %>%
  slice(1) %>% # within a case, get one row for each defendant
  filter(case_level == "M") %>% # filter for defendants in misdemeanor cases only
  nrow() # find number of defendants


#### "The three most common misdemeanor charges prosecuted by the LA District 
#### Attorney’s office are possession of drug paraphernalia (HSC 11364), driving 
#### with a suspended license (VC 14601.1(a)), and possession of a controlled 
#### substance (HSC 11377)"

data %>%
  filter(charge_level == "M") %>% # filter for only misdemeanor charges
  count2(offense_description) %>% # get frequency table of charges
  head(3) # look at top 3

#### "These three charges represent almost 25% of all misdemeanors charged by the LADA’s office."

8.48+8.19+7.77 # using the `pct` column from above


#### "53% of the time, one or more of these three charges are brought as the only 
#### charge against someone..."
#### "Another 27% of the time, only one other charge is brought against a defendant,"
data %>% 
  filter(person_id %in% (data %>% # filter for people charged with these crimes
                           filter(code_section_m_f == "M_HS_11364" | 
                                    code_section_m_f == "M_VC_14601.1(A)" |
                                    code_section_m_f == "M_HS_11377"))$person_id) %>% 
  mutate(top3 = case_when(code_section_m_f == "M_HS_11364" ~ 1, # create column identifying if the charge...
                          code_section_m_f == "M_VC_14601.1(A)" ~ 1, # ...is one of the three 
                          code_section_m_f == "M_HS_11377" ~ 1, # ...charges in question
                          TRUE ~ 0)) %>% # if that row is not, mark it as zero
  group_by(person_id) %>% # within each individual
  summarise(pct_of_charges_that_are_one_of_these_3_charges = sum(top3, na.rm = T)/n()) %>% # what percent of their charges are one of those three?
  count2(pct_of_charges_that_are_one_of_these_3_charges) %>% # get a frequency table
  select(-n) %>% # remove an unnecessary column
  head(2) # only show top 2 (100% of charges are those charges, and 50% of charges are one or more of those)




# Personal & Professional Perspectives
#################################################


#### "The LADA’s office diverted less than 1% of all charges in the data they provided"
data %>%
  count2(result_description)


#### "The combined trespassing-related charges are the 23rd most prosecuted 
#### charge by the District Attorney office, with 5,262 charges prosecuted in 2017-18"

data %>%
  count2(offense_description) %>% # frequency of charges, sorted most to least
  slice(22:23) # what are the 22nd and 23rd most-prosecuted charges, and how many charges are there?
  # 22nd most-prosecuted charge was prosecuted 5,282 times
  # 23rd most-proseuted charge was prosecuted 5,070 times

data %>% # determine how many tresspassing-related charges there are
  filter(grepl("tres", offense_description)) %>% # filter for charges that include "tres" in them
  nrow() # how many charges are there?

  # since the sum of all the trespassing charges was 5,262, we know it fits
  # between the 22nd and 23rd most-prosecuted charge, making it the 23rd-most
  # prosecuted charge


#### "The data the LADA’s office provided show that 78.1% of criminal threat and 
#### 88.2% of assault charges are brought as felonies"
data %>% 
  filter(grepl("422", code_section)) %>% # filter for "criminal threat" (PC 422)
  count2(charge_level) # get frequency table of charge level

data %>% 
  filter(code_section == "PC_245(A)(4)") %>% # filter for assault charges
  count2(charge_level) # get frequency table of charge level

#### "Overall, more than 54% of wobbler charges were brought as felonies by the LADA’s 
#### office in 2017-18...  if the LADA’s office charged the 97,742 wobblers they 
#### prosecuted as felonies"
data %>%
  filter(wobbler == T) %>% # filter for only wobblers
  count2(charge_level)


####  "most if not all of the 23% of people who have all their charges dismissed"
dismissals <- # create dataframe where each person within a case is a row
  data %>% 
     mutate(dismissed = ifelse(result_description == "Dismissed", 1, 0)) %>% # create "dismissed" dummy
     group_by(person_id) %>% # group by person
     summarise(pct_dismissed = sum(dismissed/n())) # calculate, for each person, the % of charges dismissed

length(unique((dismissals %>% filter(pct_dismissed == 1))$person_id)) / # number of people with all charges dismissed
  length(unique(data$person_id)) # divided by total number of people


#### "people who have all their charges dismissed (61,699 in 2017-18)"
length(unique((dismissals %>% filter(pct_dismissed == 1))$person_id))



# Recommendations
########################

#### "Declining to charge just misdemeanor and infraction-level paraphernalia possession 
#### HSC 11364), driving without a license or with a suspended license (VC 12500(a) 
#### and VC 14601.1(a)), and possession of a controlled substance (HSC 11377) charges 
#### would eliminate 102,910 charges."
#### "This represents 20% of all charges filed by the LA District Attorney's office in 2017-18,"
data %>%
  filter(charge_level != "F") %>% # remove felony charges
  filter(grepl("11364", code_section) | # use pattern matching to find all HS 11364 charges
           grepl("12500", code_section) | # use pattern matching to find all VC 12500(a) charges
           code_section_m_f == "M_VC_14601.1(A)" | # pattern matching catches some charges we don't want for this charge, so use exact matching
           grepl("11377", code_section)) %>% # use pattern matching to find all HS 11377 charges
  summarise(num_charges = n(), # find total number of charges
            pct_of_all_charges = n() / nrow(data)) # calculate what percent of all chartes, these three are


#### "and over 50% of these charges are dismissed anyway."
data %>%
  filter(charge_level != "F") %>%# remove felony charges
  filter(grepl("11364", code_section) | # use pattern matching to find all HS 11364 charges
           grepl("12500", code_section) | # use pattern matching to find all VC 12500(a) charges
           code_section_m_f == "M_VC_14601.1(A)" | # pattern matching catches some charges we don't want, so use exact matching
           grepl("11377", code_section)) %>% # use pattern matching to find all HS 11377 charges
  count2(result_description) %>% # get frequency table of the result for each charge
  head(5) # limit to top 5


#### "It would also entirely remove from the justice system 48,260 people for 
#### whom those charges were the only ones they faced in 2017-18"
data %>%
  filter(person_id %in% (data %>% # filter for only people who were charged with those specific charges
                           filter(charge_level != "F") %>%
                           filter(grepl("11364", code_section) | 
                                    grepl("12500", code_section) |
                                    code_section_m_f == "M_VC_14601.1(A)" |
                                    grepl("11377", code_section)))$person_id) %>%
  mutate(charges = case_when(grepl("11364", code_section) ~ 1, # create dummy for if a charge against someone was one of the ones in question
                             grepl("12500", code_section) ~ 1,
                             code_section_m_f == "M_VC_14601.1(A)" ~ 1,
                             grepl("11377", code_section) ~ 1,
                             TRUE ~ 0 # if the charge isn't one of the ones in question, give it a 0
                             )) %>%
  group_by(person_id) %>% # within each individual...
  summarise(pct_these_charges = sum(charges)/n()) %>% # ...find the % of the charges filed against them that are the charges in question
  filter(pct_these_charges == 1) %>% # filter for only people who are only charged with the charges in question, and no other charges
  nrow() # count the number of people


#### "60.5% of the total charges filed by the DA,"
data %>%
  count2(low_level) # frequency table of whether charges are on the ACLU "decline to charge" list

#### "48.5% of which are currently dismissed anyway."
data %>%
  filter(low_level == T) %>% # filter for only charges that are on the ACLU "decline to charge" list
  count2(result_description) # frequency table for the result of those charges

#### "23% of all people charged by the LADA’s office had all of their charges dismissed"
dismissals <- # create dataframe where each person within a case is a row
  data %>% 
     mutate(dismissed = ifelse(result_description == "Dismissed", 1, 0)) %>% # create "dismissed" dummy
     group_by(person_id) %>% # group by person
     summarise(pct_dismissed = sum(dismissed/n())) # calculate, for each person, the % of charges dismissed

dismissals %>%
  filter(pct_dismissed == 1) %>% # filter for only people who have all charges dismissed
  pull(person_id) %>% # get all the person_id's
  length() %>% # count how many there are
  `/` (length(unique(data$person_id))) # divide by the total unique # of person_ids in the dataset


#### "Figure 5: Charge-level outcomes, showing over 50% of all charges are dismissed..."

# charge-level coutcomes

outcomes <-  data %>%
  # recode Held To Answer to be on two lines & all others (excluding dismissed, convicted, HTA & diversion) to "Other"
  mutate(result_simple = case_when(result_description == "Dismissed" ~ "Dismissed",
                                   result_description == "Convicted" ~ "Convicted",
                                   result_description == "Held To Answer in Superior Court" ~ "Held To Answer \n in Superior Court",
                                   result_description == "Diversion" ~ "Diversion",
                                   TRUE ~ "Other")) %>%
  count2(result_simple) # get frequency table of charge results

# factor so that they come out in order in the plot below
outcomes$result_simple <- 
  factor(outcomes$result_simple,levels = 
           c("Dismissed", "Convicted", "Other", 
             "Held To Answer \n in Superior Court", 
             "Diversion"))
# plot
ggplot(outcomes, 
       aes(y=pct, 
           x=result_simple,
           fill = "#80d8f0")) + 
  geom_bar(stat="identity", show.legend = F) +
  ggtitle("Charge-level outcomes") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values="#80d8f0") +
  ylab("Percent of All Cases") +
  xlab("") +
  geom_text(
    aes(label= paste0(sprintf("%0.1f", round(pct, digits = 1)), "%", " \n ", "(", format(n, big.mark = ","), " cases) ") ), 
    position=position_dodge(width=0.9), 
    vjust=.5)

rm(outcomes) # remove object from workspace

#### "most if not all of the 23% of people who have all their charges dismissed 
#### (61,699 in 2017-18)"
dismissals %>%
  filter(pct_dismissed == 1) %>% # filter for individuals who have all their charges dismissed
  pull(person_id) %>% # isolate just the unique person_ids
  length() # count how many there are

rm(dismissals)




################################################################################
# Diversion
################################################################################

####  "0.72% of the individuals received diversion for at least one charge"
data %>%
  filter(result_description == "Diversion") %>% # filter for only charges that are diverted
  pull(person_id) %>% # isolate just the person_ids
  unique() %>% # get only unique ones (don't want to double-count people who had multiple charges diverted)
  length() %>% # count how many people that is
  `/` (length(unique(data$person_id))) # divide that by the total number of unique individuals in the dataset

#### "(1,924 out of 266,405 people)"
length(unique((data %>% filter(result_description == "Diversion"))$person_id)) # unique people who had at least one charge diverted
length(unique(data$person_id)) # total number of unique individuals

#### "0.52% had all charges diverted"
diversion <- data %>% 
  group_by(person_id) %>% # within each unique person_id...
  summarise(diverted = sum(result_description == "Diversion", na.rm = T), # calculate # of charges diverted
            num_charges = n()) %>% # calculate total # of charges
  mutate(pct_crgs_diverted = diverted/num_charges) # calculate percent of charges diverted

nrow(diversion %>% # number of people
       filter(diverted == 1))/ # who for whom all their charges were diverted
  length(unique(alldata$person_id)) # divided by total number of people

#### "(1,395 out of 266,405 people)"
nrow(diversion %>% # number of people
       filter(diverted == 1))  # for whom all their charges were diverted

length(unique(data$person_id)) # number of total people

rm(diversion) # remove object from workspace


####  "the overall diversion rate (0.5%, not shown)"
nrow(data %>% # get the number of roww...
       filter(result_description == "Diversion")) / # ...when filtered for only diverted charges
  nrow(data) # divide by total number of rows (charges)


#### "Figure 6: Rate of diversion for charges with the highest total number of diverted charges"
charges_diverted <-
  # get the number of times each charge is diverted
  count(data %>% 
          filter(result_description == "Diversion"), # filter for just diverted charges
        offense_description, # create frequency table of the offense description
        name = "diverted", # name the frequency column
        sort = T) %>% # sort it highest to lowest
  # join this with the total number of each charges in the dataset (diverted or not)
  left_join(count(data,
                  offense_description,
                  name = "total")) %>%
  mutate(pct_diverted = diverted/total) %>% # calculate percent of each charge that's diverted
  head(5) %>% # take top 5
  # rename so they make more sense
  mutate(offense_description = 
           recode(offense_description,
                  `misd. cont. sub. poss. -- HS_11377_M` = "Possess controlled substance \n HS 11377",
                  `misd. paraphernalia -- HS_11364_M` = "Paraphernalia possession \n HS 11364",
                  `broadly misd. possession (narcotic, MECLOQUALONE/METHAQUALONE/ETC) -- HS_11350_M` = "Possess narcotic \n HS 11350",
                  `use/under influence of controlled substance -- HS_11550(A)_M` = "Use of a controlled substance \n HS 11550(a)",
                  `dui alcohol -- VC_23152(A)_M` = "DUI \n VC 23152(a)"))
# plot
ggplot(charges_diverted, 
       aes(y=pct_diverted*100, 
           x=reorder(offense_description, pct_diverted),
           fill = "#80d8f0")# ,
       #xmax = 100)
) + 
  geom_bar(stat="identity", show.legend = F) +
  ylim(0,100) +
  theme_minimal(base_size = 16) +
  ggtitle("Rate of diversion for 5 most diverted charges") +
  scale_fill_manual(values="#80d8f0") +
  ylab("Percent diverted") +
  xlab("") +
  geom_text(
    aes(label= paste0(sprintf("%0.1f", round(pct_diverted*100, digits = 1)), "%", 
                      " (", diverted, " diverted)")), 
    position=position_dodge(width=0.5), 
    vjust= .5,
    hjust=-.1,
    size = 5
  ) +
  coord_flip()

rm(charges_diverted) # remove object from workspace


#### "This total represents 0.36% of the total number of people prosecuted by the 
#### LADA’s office during that time period"
971 / # the LADA's office reported that 971 people were accepted into 6 programs
  length(unique(data$person_id)) # the total number of unique individuals in the data



# Recommendations
#####################

#### "the LADA’s office only diverts 0.7% of charges on the ACLU’s “Decline to charge / pre-plea diversion” list"
#### "(a diversion rate of 0.7% for charges on the ACLU's "Decline to charge / pre-plea diversion" list)"
data %>%
  filter(low_level == T) %>% # filter for only charges on the ACLU's list
  count2(result_description) # get a frequency table of charge results (dismissed, convicted, diverted, etc)


#### "less than 1% of all charges filed are diverted"
data %>%
  count2(result_description) %>% # get frequency table of charge results (dismissed, convicted, diverted, etc)
  head(6) # only look at the top 6 most frequent\

#### "and less than 1% of individuals prosecuted by the LADA’s office have even one charge diverted"
data %>%
  filter(result_description == "Diversion") %>% # filter for only diverted charges
  pull(person_id) %>% # get the person_ids
  unique() %>% # get only unique ids
  length() %>% # calculate how many unique ids there are
  `/` (length(unique(data$person_id))) # divide by total unique pepople in dataset




################################################################################
# Conclusion
################################################################################

#### "23% of all people prosecuted by the LADA’s office in 2017 and 2018 having all their charges dismissed"
dismissals <- # create dataframe where each person within a case is a row
  data %>% 
  mutate(dismissed = ifelse(result_description == "Dismissed", 1, 0)) %>% # create "dismissed" dummy
  group_by(person_id) %>% # group by person
  summarise(pct_dismissed = sum(dismissed/n())) # calculate, for each person, the % of charges dismissed

length(unique((dismissals %>% filter(pct_dismissed == 1))$person_id)) / # number of people with all charges dismissed
  length(unique(data$person_id)) # divided by total number of people

rm(dismissals)

#### "less than 1% of all individuals prosecuted by the office in 2017 and 2018 had even a single charge diverted"
data %>%
  filter(result_description == "Diversion") %>% # filter for only diverted charges
  pull(person_id) %>% # isolate just the person_ids
  unique() %>% # find all unique ids
  length() %>% # count number of unique ids
  `/` (length(unique(data$person_id))) # divide by total number of unique people in dataset

#### "the location data was incomplete (over 40% missing)"
sum(is.na(data$minor_zip_code))/ # total # of missing data points
  length(data$minor_zip_code) # divided by total number of data points

################################################################################
# Appendix 
################################################################################

#### Appendix 3: List of decline-to-charge and default-diversion charges

cat(unique((data %>% filter(low_level == T))$code_section_m_f), sep = " -- ")



# end






