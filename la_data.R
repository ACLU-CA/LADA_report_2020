################################################################################
# Introduction to this script
################################################################################
# This script takes the excel spreadsheet provided by the LADA’s office in 
# response to this request, and prepares it for the analyses conducted in the 
# ACLU’s 2020 report on the Los Angeles District Attorney's office.



################################################################################
# load packages
################################################################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, # for most everything
               readxl, # for read_excel()
               janitor, # for clean_names()
               data.table) # for fread() (and probably some other data manipulation)



################################################################################
# load external data
################################################################################

# read in dataframe of wobbler charges
wobblers <- fread("wobbler_final.csv")

# read in ACLU's list of charges they think DAs should decline to charge or default to pre-plea diversion
dtc <- fread("decline_to_charge.csv") %>%
  clean_names() %>%
  mutate(charge = gsub(" ", "_", charge))

# read in serious & violent felonies spreadsheet
svf <- read_csv("svf.csv")
  


################################################################################
# load LA District Attorney data & add columns
################################################################################

data <- 
  # bind each of the sheets of the excel spreadsheet together into one dataframe
  rbind(
    read_excel("20191114-ACLU_New.xlsx", sheet = "Part 3") %>% clean_names() %>% mutate(data_part = "part3"),
    read_excel("20191114-ACLU_New.xlsx", sheet = "Part 4") %>% clean_names() %>% mutate(data_part = "part4"),
    read_excel("20191114-ACLU_New.xlsx", sheet = "Part 5") %>% clean_names() %>% mutate(data_part = "part5"),
    read_excel("20191114-ACLU_New.xlsx", sheet = "Part 6") %>% clean_names() %>% mutate(data_part = "part6")
    ) %>%
  # prepare `case_category_description`, a variable found in "Part 1", which will be loaded next
  mutate(case_category_description = NA) %>%
  # reorder columns, so they line up with "Part 1", which will be loaded next
  select(data_part, masked_case_no, defendant_no, case_type, case_level, 
         sex, age_at_time_of_crime, charge_level, count_no, code, section, 
         plea, result, result_description, result_date, charge_city, minor_zip_code, 
         case_category_description) %>%
  
  # bind the last tab of the excel spreadsheet
  rbind(
    read_excel("20191114-ACLU_New.xlsx", sheet = "Part 1") %>% clean_names() %>% 
      # apparently all these cases are felonies, so make that so:
      mutate(case_level = "F", 
             charge_level = "F",
             data_part = "part1",
             case_type = NA,
             charge_city = NA) %>%
      # reorder columns to align with the previously loaded data
      select(data_part, masked_case_no, defendant_no, case_type, case_level, 
             sex, age_at_time_of_crime, charge_level, count_no, code, section, 
             plea, result, result_description, result_date, charge_city, minor_zip_code, 
             case_category_description)
    ) %>%
  
  # add unique id, with fixed width
  mutate(unique_id = sprintf("%06d", 1:nrow(.))) %>%
  
  # make zip codes no more than 5 digits
  mutate(code_section = paste(code, section, sep="_"),
         code_section_m_f = paste(charge_level, code_section, sep = "_"),
         minor_zip_code = substr(minor_zip_code, 1, 5)) %>%
  
  
  mutate(
         # add age group tags
         age_group = case_when(age_at_time_of_crime > 10 & age_at_time_of_crime < 18 ~ "minor_11-17",
                               age_at_time_of_crime >= 18 & age_at_time_of_crime < 26 ~ "young_adult_18-25",
                               age_at_time_of_crime >= 26 & age_at_time_of_crime < 91 ~ "adult_26-90",
                               TRUE ~ "other_or_missing"),
         # add unique charge identifier
         charge_id = paste(substr(data_part, 5,5),
                           masked_case_no,
                           defendant_no,
                           code_section,
                           sep = "_"),
         # add unique person identifier
         person_id = paste(substr(data_part, 5,5),
                           masked_case_no,
                           defendant_no,
                           sep = "_"),
         # add unique case identifier
         case_id = paste(substr(data_part, 5,5),
                           masked_case_no,
                           sep = "_"),
         # change data_part to be more descriptive
         data_part = recode(data_part,
                            part1 = "juvenile_motion_to_transfer", 
                            part3 = "juvenile_misdemeanors", 
                            part4 = "juvenile_felonies",
                            part5 = "adult_misdemeanors",
                            part6 = "adult_felonies"),
         # change plea_description to be more descriptive
         plea_description = recode(plea,
                                   DA = "Denies allegations",
                                   IN = "Not guilty by reason of insanity",
                                   NC = "Nolo contendere",
                                   NE = "Not guilty entered by court",
                                   NG = "Not guilty",
                                   NI = "Not guilty and not guilty by reason of insanity",
                                   PG = "Plead guilty")
         ) %>%
  
  # add wobbler tags
  mutate(wobbler = case_when(code_section %in% wobblers$charge ~ T,
                             TRUE ~ F)) %>%
  
  # add charges from the list of charges that ACLU thinks DAs should decline to charge or default to pre-plea diversion
  mutate(low_level = case_when((code_section %in% # exact matches in the DTC list
                                  (dtc %>% filter(include == "Include" & match_type == "Exact"))$charge) ~ T,
                               # partial matches from the DTC list:
                               (grepl(paste((dtc %>% filter(include == "Include" & match_type == "Partial"))$charge, collapse="|"),
                                      code_section)) ~ T,
                               # add "driving w/o license" charges:
                               grepl("14601", code_section) ~ T, 
                               # add "not returning lost property" charges:
                               grepl("485", code_section) ~ T, 
                               # add "criminal threat" charges:
                               grepl("422", code_section) ~ T, 
                               # add "Possession of vandalism/graffiti 
                               # paraphernalia with intent to commit vandalism/graffiti" charges:
                               grepl("592.2", code_section) ~ T,
                               # add "Repeat petty theft" charges:
                               grepl("490.2", code_section) ~ T, 
                               # "repeat petty theft + "not returning lost property":
                               code_section == "PC_485&490.2" ~ T, 
                               # if none of the above is true, charge is not a DTC :
                               TRUE ~ FALSE)) %>% 
  
  # some of the partial matches from the DTC list catch things we don't want. Remove those things.
  mutate(low_level = case_when(
    # remove certain charges using exact matches
    (code_section %in%
       (dtc %>% filter(include == "Exclude" & match_type == "Exact"))$charge) ~ F,
    # partial matches from the DTC list:
    (grepl(paste((dtc %>% filter(include == "Exclude" & match_type == "Partial"))$charge, collapse="|"),
           code_section)) ~ F,
    TRUE ~ low_level)) %>%
  
  # add serious & violent felonies tags
  mutate(svf = case_when(code_section_m_f %in% svf$code_section ~ TRUE,
                         TRUE ~ FALSE)) %>%
  replace_na(list(wobbler = FALSE, low_level = FALSE, low_level_old = FALSE))



################################################################################
# add text descriptions of charges
################################################################################

# data downloaded 3/24/2020 from https://oag.ca.gov/law/code-tables
# which reports the data was updated 3/5/2020

chargecodes <- read_csv("macrcode.csv", col_names = FALSE) %>%
  # add column names
  `colnames<-`(c("BCS_code", "no_2","no_3", "m_f", "offense_number", 
                 "penal_code", "offense_description", "sentence")) %>%
  mutate(offense_description = tolower(offense_description), # make lowercase for easier reading
         key = paste(penal_code, # create a "key" to merge into our data
                     gsub(" ", "", offense_number), 
                     m_f, 
                     sep = "_"))

chargekey <- chargecodes %>%
  group_by(key) %>%
  # in situations where a key has multiple descriptions, append the descriptions together:
  summarise(offense_description = paste(offense_description, collapse = " and/or ")) %>% 
  # add the charge code / statute to the end so you can see the description & the statute:
  mutate(offense_description = paste(offense_description, key, sep = " -- "))

data <- data %>%
  # create a key in our data that will match the offense description key
  mutate(key = paste0(code_section, "_", charge_level)) %>% 
  # join the descriptions to our data
  left_join(chargekey) %>% 
  # replace any missing descriptions (NAs) with the statute
  mutate(offense_description = coalesce(offense_description, key), 
         # hand-enter the most common charges that didn't merge in
         offense_description = 
           recode(offense_description, 
                  "HS_11364_M" = "misd. paraphernalia -- HS_11364_M",
                  "HS_11377_M" = "misd. cont. sub. poss. -- HS_11377_M",
                  "VC_12500(A)_I" = "infr. drive w/o license -- VC_12500(A)_I",
                  "PC_484(A)&490.2_M" = "misd. petty theft: - $950 -- PC_484(A)&490.2_M",
                  "PC_594(A)_M" = "misd. vandalism property -- PC_594(A)_M",
                  "VC_16028(A)_I" = "infr. driving w/o insurance -- VC_16028(A)_I",
                  "HS_11350_M" = "broadly misd. possession (narcotic, MECLOQUALONE/METHAQUALONE/ETC) -- HS_11350_M",
                  "HS_11377_F" = "fel. possess controlled subst. HS_11377_F"))


# create checkpoint. `alldata` contains all charges, we will remove duplicates below
alldata <- data


################################################################################
# Get rid of charges that were "Held to Answer in Superior Court" (HTA) which also have another disposition.
# This is because the LADA's office said that HTA gets entered, and then once there
# is a disposition from the superior court, a new entry gets created that shows
# the result of the superior court. So this duplicates these charges, because
# one is HTA and the other is that actual result, but it's for the same charge.
################################################################################

# identify all charges where at least one result was HTA
hta <- data %>%
  filter(charge_id %in%
      (data %>% filter(result_description == "Held To Answer in Superior Court"))$charge_id
  )

# identify all charges where the ONLY result is HTA
hta_only <- data %>%
  filter(charge_id %in% 
           (hta %>% 
              group_by(charge_id) %>% 
              summarise(n = n()) %>% 
              filter(n == 1))$charge_id
         )

# determine which charges to REMOVE
hta_remove <- data %>%
  filter(result_description == "Held To Answer in Superior Court") %>%
  mutate(remove = 
           # when the HTA case is the only result, don't remove it
           case_when(unique_id %in% hta_only$unique_id ~ FALSE,
                     # otherwise, there is another result, so remove it
                     TRUE ~ TRUE))

# add a column which shows which charges should be removed
data <- data %>%
  # join in the column `remove` which shows which charges to remove
  left_join(hta_remove %>% select(remove, unique_id)) %>%
  replace_na(list(remove = FALSE)) %>%
  # filter out the charges which should be removed
  filter(remove != TRUE) %>%
  # get rid of the column which showed which charges should be removed, since they're gone now
  select(-remove)


# remove unncessary objects from workspace
rm(list = c('chargecodes', 'chargekey', 
            'hta', 'hta_only', 'hta_remove', 
            'wobblers', 'svf', 'dtc'))


# end


