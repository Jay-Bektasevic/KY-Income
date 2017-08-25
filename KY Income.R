
# Jay Bektasevic

library(tidyverse)
library(dplyr)
library(stringr)

 
#   ____________________________________________________________________________
#   Income                                                                  ####

# The Statistics of Income (SOI) division bases its ZIP code data on administrative records of individual income 
# tax returns (Forms 1040) from the Internal Revenue Service (IRS) Individual Master File (IMF) system. Included in 
# these data are returns filed during the 12-month period, January 1, 2015 to December 31, 2015.
# ZIP Code data are based on population data that was filed and processed by the IRS during the 2015 calendar year.
# Data do not represent the full U.S. population because many individuals are not required to file an individual income tax return.
# Excluded from the data are tax returns with a negative adjusted gross income.

# We could have gotten the family income data from acs package table B19101. But we came to the conclusion that irs data would be 
# a better representation of people's income. 


income <- read_csv("https://www.irs.gov/pub/irs-soi/14zpallagi.csv")

# if you want to import specific columns
#income <- read_csv("https://www.irs.gov/pub/irs-soi/14zpallagi.csv")[,c("agi_stub", "zipcode", "N1")]

# filter on KY
# income <- filter(income, STATEFIPS ==21 )


#income bracket Size of adjusted gross income
# agi_stub =      1 = $1 under $25,000
#                 2 = $25,000 under $50,000
#                 3 = $50,000 under $75,000
#                 4 = $75,000 under $100,000
#                 5 = $100,000 under $200,000
#                 6 = $200,000 or more

#income <- filter(income, agi_stub == 1)

# Select only important columns
income <- select(income, STATE, zipcode, agi_stub, N1, A02650 )


# Rename columns
names(income)<- c("STATE", "zipcode", "adjusted_gross_income", "Number of returns", "Total income amount (Thousands)")
income$Avg_Income <- 1000*(income$`Total income amount (Thousands)`/income$`Number of returns`)
income$zipcode <- as.numeric(income$zipcode)

# Filter for totals nationally and on state level
incomeTotals <- filter(income, zipcode == 0)
stateTotals <- aggregate(incomeTotals$`Number of returns`, list(category = incomeTotals$STATE), FUN = sum)
incomeTotals <- left_join(incomeTotals, stateTotals, by = c("STATE" = "category"))
incomeTotals$percent <- (incomeTotals$`Number of returns`/incomeTotals$x)
names(incomeTotals)[6] <- "Total State"
incomeTotals <- incomeTotals[,-2]
zipTotal <- aggregate(income$`Number of returns`, by= list(category =income$zipcode), FUN = sum)

income <- left_join(income, zipTotal, by = c("zipcode" = "category"))
income$percent <- (income$`Number of returns`/income$x)

# leave only zip data
income <- filter(income, zipcode != 0)

income$adjusted_gross_income <- str_replace(income$adjusted_gross_income, "^1$", "$1 under $25,000" )
income$adjusted_gross_income <- str_replace(income$adjusted_gross_income, "^2$", "$25,000 under $50,000" )
income$adjusted_gross_income <- str_replace(income$adjusted_gross_income, "^3$", "$50,000 under $75,000" )
income$adjusted_gross_income <- str_replace(income$adjusted_gross_income, "^4$", "$75,000 under $100,000" )
income$adjusted_gross_income <- str_replace(income$adjusted_gross_income, "^5$", "$100,000 under $200,000" )
income$adjusted_gross_income <- str_replace(income$adjusted_gross_income, "^6$", "$200,000 or more" )

write.csv(income, "zip_income.csv")
write.csv(incomeTotals, "ZipIncomeTotals.csv")

x <- spread(income, key = adjusted_gross_income, value = percent )
x <- x[,c(2, 7:12)]
x[is.na(x)] <- 0

x <- aggregate(x ,by= list(zip =x$zipcode), FUN = sum)

write.csv(x, "x_zip_income.csv")
########################################################################################

# # not sure if we need this part. I found a better solution below using HUD USPS ZIP CODE CROSSWALK FILES
# # county FIPS code
# 
# df_county <- read_csv("https://query.data.world/s/abpmvvpj8atq2glpovjf2d7ss")
# df_county <- filter(df_county, STATEFP == 21)
# df_county$COUNTYFP <- formatC(df_county$COUNTYFP, width=3, flag="0")  
# 
# df_zip <- read_csv("https://query.data.world/s/bl3lzykaskl6xqiup2r63tf81")[,c("ZCTA5", "STATE", "COUNTY", "GEOID", "ZPOP")]
# df_zip <- read_csv("https://query.data.world/s/bl3lzykaskl6xqiup2r63tf81")
# names(df_zip)[1] <- "ZIP"
# 
# 
# df_county$COUNTYFP <- as.numeric(df_county$COUNTYFP)
# df_county$STATEFP <- as.numeric(df_county$STATEFP)
# 
# df_zip_ky <- filter(df_zip, STATE == 21)
# df_zip_ky$ZIP <- as.numeric(df_zip_ky$ZIP)
# df_zip_ky$COUNTY <- as.numeric(df_zip_ky$COUNTY)
# df_zip_ky$STATE <- as.numeric(df_zip_ky$STATE)
#     
# 
# df_zip_co_ky <- left_join(df_zip_ky,df_county, by = c("COUNTY" = "COUNTYFP"))
# 
# county_inc_ZIP <- left_join(df_zip_co_ky, income, by = c("ZIP" = "zipcode"))
# 
# county_inc_ZIP <- select(county_inc_ZIP, -STATE.x, -COUNTY, -STATEFP, -CLASSFP)
# 
# county_inc_ZIP$AVG_ZIP_INCOME <- 1000*(county_inc_ZIP$`Total income amount (Thousands)`/county_inc_ZIP$`Number of returns`)
# 
# write.csv(county_inc_ZIP, "zip_income.csv")

########################################################################################

# Census data the easy

library(acs)

api.key.install(key="5f9deb6e79b4ab96ddaa0775a6bb5e22887550dc")
# In theory, you could even use this to get all the tracts from all the 3,225 counties in the country.
# Unfortunately (or perhaps fortunately), this is just too much for R to download. As a result we will focus on KY. 
# if you want to focus on any other state simply put that state abr in the geo object below.

geo <- geo.make(state=c("KY"), county="*", tract="*")



 
#   ____________________________________________________________________________
#   Fetch Median Earnings                                                              ####

# income1 <- acs.fetch(endyear = 2015, span = 5, geography = geo,
#                    table.number = "B23006", col.names = "pretty")
# 
#  
# names(attributes(income1))
# 
#  attr(income1, "acs.colnames")
#  attr(income1, "geography")
# 
#  # convert to a data.frame and pad for merging
#  income1_df <- data.frame(paste0(str_pad(income1@geography$state, 2, "left", pad="0"),
#                                     str_pad(income1@geography$county, 3, "left", pad="0"),
#                                     str_pad(income1@geography$tract, 6, "left", pad="0")),
#                              income1@estimate[,c("Total:", 
#                                                  "Less than high school graduate:", 
#                                                  "Less than high school graduate: In labor force:", 
#                                                  "Less than high school graduate: In labor force: In Armed Forces", 
#                                                  "Less than high school graduate: In labor force: Civilian:", 
#                                                  "Less than high school graduate: In labor force: Civilian: Employed", 
#                                                  "Less than high school graduate: In labor force: Civilian: Unemployed", 
#                                                  "Less than high school graduate: Not in labor force", 
#                                                  "High school graduate (includes equivalency):", 
#                                                  "High school graduate (includes equivalency): In labor force:", 
#                                                  "High school graduate (includes equivalency): In labor force: In Armed Forces", 
#                                                  "High school graduate (includes equivalency): In labor force: Civilian:", 
#                                                  "High school graduate (includes equivalency): In labor force: Civilian: Employed", 
#                                                  "High school graduate (includes equivalency): In labor force: Civilian: Unemployed", 
#                                                  "High school graduate (includes equivalency): Not in labor force", 
#                                                  "Some college or associate's degree:", 
#                                                  "Some college or associate's degree: In labor force:", 
#                                                  "Some college or associate's degree: In labor force: In Armed Forces", 
#                                                  "Some college or associate's degree: In labor force: Civilian:", 
#                                                  "Some college or associate's degree: In labor force: Civilian: Employed", 
#                                                  "Some college or associate's degree: In labor force: Civilian: Unemployed", 
#                                                  "Some college or associate's degree: Not in labor force", 
#                                                  "Bachelor's degree or higher:", 
#                                                  "Bachelor's degree or higher: In labor force:", 
#                                                  "Bachelor's degree or higher: In labor force: In Armed Forces", 
#                                                  "Bachelor's degree or higher: In labor force: Civilian:", 
#                                                  "Bachelor's degree or higher: In labor force: Civilian: Employed", 
#                                                  "Bachelor's degree or higher: In labor force: Civilian: Unemployed", 
#                                                  "Bachelor's degree or higher: Not in labor force"
#                              )],
#                              stringsAsFactors = FALSE)
# 
# # # Rename columns
#  names(income1_df)<- c("GEOID",
#                        "Total_Income",
#                        "With_wage_or_salary_income",
#                        "No_wage_or_salary_income"
#                        )


#   ____________________________________________________________________________
#   Fetch table B15003 - Educational Attainment for the Population 25 Years ####


edu_attain <- acs.fetch(endyear = 2015, span = 5, geography = geo,
                        table.number = "B15003", col.names = "pretty")

# use of col.names = "pretty" above gives the full column definitions
# if you want Census variable IDs use col.names="auto". Here are the
# variables we want with pretty and auto results.
#"Household Income: Total:" ("B19001_001")
#"Household Income: $200,000 or more" ("B19001_017")


# the resulting "edu_attain" object is not a data.frame it's a list
# to see what's available

names(attributes(edu_attain))

attr(edu_attain, "acs.colnames")
attr(edu_attain, "geography")

# convert to a data.frame and pad for merging
edu_attain_df <- data.frame(paste0(str_pad(edu_attain@geography$state, 2, "left", pad="0"), 
                               str_pad(edu_attain@geography$county, 3, "left", pad="0"), 
                               str_pad(edu_attain@geography$tract, 6, "left", pad="0")), 
                            edu_attain@estimate[,c("Educational Attainment for the Population 25 Years and Over: Total:",
                                                   "Educational Attainment for the Population 25 Years and Over: No schooling completed",
                                                   "Educational Attainment for the Population 25 Years and Over: Nursery school",
                                                   "Educational Attainment for the Population 25 Years and Over: Kindergarten",
                                                   "Educational Attainment for the Population 25 Years and Over: 1st grade",
                                                   "Educational Attainment for the Population 25 Years and Over: 2nd grade",
                                                   "Educational Attainment for the Population 25 Years and Over: 3rd grade",
                                                   "Educational Attainment for the Population 25 Years and Over: 4th grade",
                                                   "Educational Attainment for the Population 25 Years and Over: 5th grade",
                                                   "Educational Attainment for the Population 25 Years and Over: 6th grade",
                                                   "Educational Attainment for the Population 25 Years and Over: 7th grade",
                                                   "Educational Attainment for the Population 25 Years and Over: 8th grade",
                                                   "Educational Attainment for the Population 25 Years and Over: 9th grade",
                                                   "Educational Attainment for the Population 25 Years and Over: 10th grade",
                                                   "Educational Attainment for the Population 25 Years and Over: 11th grade",
                                                   "Educational Attainment for the Population 25 Years and Over: 12th grade, no diploma",
                                                   "Educational Attainment for the Population 25 Years and Over: Regular high school diploma",
                                                   "Educational Attainment for the Population 25 Years and Over: GED or alternative credential",
                                                   "Educational Attainment for the Population 25 Years and Over: Some college, less than 1 year",
                                                   "Educational Attainment for the Population 25 Years and Over: Some college, 1 or more years, no degree",
                                                   "Educational Attainment for the Population 25 Years and Over: Associate's degree",
                                                   "Educational Attainment for the Population 25 Years and Over: Bachelor's degree",
                                                   "Educational Attainment for the Population 25 Years and Over: Master's degree",
                                                   "Educational Attainment for the Population 25 Years and Over: Professional school degree",
                                                   "Educational Attainment for the Population 25 Years and Over: Doctorate degree"
                                                   
                            )], 
                        stringsAsFactors = FALSE)

# Trim dataframe if needed
# edu_attain_df <- select(edu_attain_df, 1:3)
# rownames(edu_attain_df)<-1:nrow(edu_attain_df)

# Rename columns
names(edu_attain_df)<- c("GEOID",
                         "Total",
                         "No_schoolin_completed",
                         "Nursery_school",
                         "Kindergarten",
                         "n1st_grade",
                         "n2nd_grade",
                         "n3rd_grade",
                         "n4th_grade",
                         "n5th_grade",
                         "n6th_grade",
                         "n7th_grade",
                         "n8th_grade",
                         "n9th_grade",
                         "n10th_grade",
                         "n11th_grade",
                         "n12th_grade_no_diploma",
                         "Regular_high_school_diploma",
                         "GED_or_alternative_credential",
                         "Some_college_less_than_1_year",
                         "Some_college_1_or_more_years_no_degree",
                         "Associate_degree",
                         "Bachelor_degree",
                         "Master_degree",
                         "Professional_school_degree",
                         "Doctorate Degree")

# edu_attain_df$GEOID <- str_sub(edu_attain_df$GEOID, start = 1, end = 5)

# edu_attain_df <- left_join(income1_df, edu_attain_df,  by = c("GEOID" = "GEOID")) 

#   ____________________________________________________________________________
#   Fetch Census track to zip data                                          ####

library(readxl)
# When a Census tract, county or CBSA is split by a ZIP code, that tract, county or CBSA code is duplicated in 
# the crosswalk file. In the example below tract 01001020200 is split by two different ZIP codes, 36008 and 36067, 
# which appear in the ZIP column. The ratio of residential addresses in the first tract-ZIP record to the total 
# number of residential addresses in the tract is .0272 (2.72%). The remaining residential addresses in that tract 
# (97.28%) fall into the second tract-ZIP record. So, for example, if one wanted to allocate data from Census tract 
# 01001020200 to the ZIP code level, one would multiply the number of observations in the Census tract by the residential 
# ratio for each ZIP code associated with that Census tract. Note that the sum of each ratio column for each distinct 
# ZIP code may not always equal 1.00 (or 100%) due to rounding issues.

# Download Zip tract file, I had to use some of the hacking skills Bob showed us last night to get the file url.
tmp = tempfile(fileext = ".xlsx")
# Will need to download since its in xlsx format
download.file(url = "https://www.huduser.gov/portal/datasets/usps/TRACT_ZIP_122015.xlsx", destfile = tmp, mode="wb")
ZIP_TRACT <- read_excel(tmp, col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric"))
ZIP_TRACT <- ZIP_TRACT[,c(1:3)]

# Join ZIP_TRACT with edu_attain_df. This is to get the zips for education attainment dataframe.
edu_attain_df <- left_join(edu_attain_df, ZIP_TRACT, by = c("GEOID" = "TRACT")) 

# arrange columns 
edu_attain_df <- edu_attain_df[,c(28,27,1:26) ]

# numbers would need to be multiplied by the res_ratio to get the accurate numbers for the zip as explained above.
edu_attain_df <- mutate(edu_attain_df, 
                        wTotal  = RES_RATIO * edu_attain_df$Total ,
                        wNo_schoolin__completed  = RES_RATIO * edu_attain_df$No_schoolin_completed ,
                        wNursery_school  = RES_RATIO * edu_attain_df$Nursery_school ,
                        wKindergarten  = RES_RATIO * edu_attain_df$Kindergarten ,
                        w1st_grade  = RES_RATIO * edu_attain_df$n1st_grade ,
                        w2nd_grade  = RES_RATIO * edu_attain_df$n2nd_grade ,
                        w3rd_grade  = RES_RATIO * edu_attain_df$n3rd_grade ,
                        w4th_grade  = RES_RATIO * edu_attain_df$n4th_grade ,
                        w5th_grade  = RES_RATIO * edu_attain_df$n5th_grade ,
                        w6th_grade  = RES_RATIO * edu_attain_df$n6th_grade ,
                        w7th_grade  = RES_RATIO * edu_attain_df$n7th_grade ,
                        w8th_grade  = RES_RATIO * edu_attain_df$n8th_grade ,
                        w9th_grade  = RES_RATIO * edu_attain_df$n9th_grade ,
                        w10th_grade  = RES_RATIO * edu_attain_df$n10th_grade ,
                        w11th_grade  = RES_RATIO * edu_attain_df$n11th_grade ,
                        w12th_grade_no_diploma  = RES_RATIO * edu_attain_df$n12th_grade_no_diploma ,
                        wRegular_high_school_diploma  = RES_RATIO * edu_attain_df$Regular_high_school_diploma ,
                        wGED_or_alternative_credential  = RES_RATIO * edu_attain_df$GED_or_alternative_credential ,
                        wSome_college_less_than_1_year  = RES_RATIO * edu_attain_df$Some_college_less_than_1_year ,
                        wSome_college_1_or_more_years_no_degree  = RES_RATIO * edu_attain_df$Some_college_1_or_more_years_no_degree ,
                        wAssociate_degree  = RES_RATIO * edu_attain_df$Associate_degree ,
                        wBachelor_degree  = RES_RATIO * edu_attain_df$Bachelor_degree ,
                        wMaster_degree  = RES_RATIO * edu_attain_df$Master_degree ,
                        wProfessional_school_degree  =  RES_RATIO * edu_attain_df$Professional_school_degree,
                        wDoctorate_Degree = RES_RATIO * edu_attain_df$`Doctorate Degree`
                        )
# group by zipcode
w_edu_attain <- edu_attain_df %>%
    select(2, 28:53) %>%
    group_by(ZIP) %>%
    summarise(wTotal = sum(wTotal,  na.rm = TRUE),
              wSome_college_less_than_1_year = sum(wSome_college_less_than_1_year,  na.rm = TRUE),
              wSome_college_1_or_more_years_no_degree = sum(wSome_college_1_or_more_years_no_degree,  na.rm = TRUE),
              wRegular_high_school_diploma = sum(wRegular_high_school_diploma,  na.rm = TRUE),
              wProfessional_school_degree = sum(wProfessional_school_degree,  na.rm = TRUE),
              wNursery_school = sum(wNursery_school,  na.rm = TRUE),
              wNo_schoolin__completed = sum(wNo_schoolin__completed,  na.rm = TRUE),
              wMaster_degree = sum(wMaster_degree,  na.rm = TRUE),
              wKindergarten = sum(wKindergarten,  na.rm = TRUE),
              wGED_or_alternative_credential = sum(wGED_or_alternative_credential,  na.rm = TRUE),
              wBachelor_degree = sum(wBachelor_degree,  na.rm = TRUE),
              wAssociate_degree = sum(wAssociate_degree,  na.rm = TRUE),
              w9th_grade = sum(w9th_grade,  na.rm = TRUE),
              w8th_grade = sum(w8th_grade,  na.rm = TRUE),
              w7th_grade = sum(w7th_grade,  na.rm = TRUE),
              w6th_grade = sum(w6th_grade,  na.rm = TRUE),
              w5th_grade = sum(w5th_grade,  na.rm = TRUE),
              w4th_grade = sum(w4th_grade,  na.rm = TRUE),
              w3rd_grade = sum(w3rd_grade,  na.rm = TRUE),
              w2nd_grade = sum(w2nd_grade,  na.rm = TRUE),
              w1st_grade = sum(w1st_grade,  na.rm = TRUE),
              w12th_grade_no_diploma = sum(w12th_grade_no_diploma,  na.rm = TRUE),
              w11th_grade = sum(w11th_grade,  na.rm = TRUE),
              w10th_grade = sum(w10th_grade,  na.rm = TRUE),
              wDoctorate_Degree = sum(wDoctorate_Degree, na.rm = TRUE)
            )
# sub NaN with 0
w_edu_attain[w_edu_attain=="NaN"] <- 0

w_edu_attain <- mutate(w_edu_attain, "Less_Than_High_School" = wNursery_school + 
                                                               wNo_schoolin__completed + 
                                                               w9th_grade + 
                                                               w8th_grade + 
                                                               w7th_grade + 
                                                               w6th_grade + 
                                                               w5th_grade + 
                                                               w4th_grade + 
                                                               w3rd_grade + 
                                                               w2nd_grade + 
                                                               w1st_grade + 
                                                               w12th_grade_no_diploma + 
                                                               w11th_grade + 
                                                               w10th_grade +
                                                               wKindergarten,
                                    "High_School_or_Equivalent" = wRegular_high_school_diploma + 
                                                                  wGED_or_alternative_credential,
                       "Associates_or_Some_College" = wSome_college_less_than_1_year+ 
                                                      wSome_college_1_or_more_years_no_degree +
                                                      wAssociate_degree,
                       "Doctoral_Professional_Masters" = wDoctorate_Degree + wMaster_degree + wProfessional_school_degree
                                                        
                                                
)
w_edu_attain <- select(w_edu_attain, -c(wNursery_school,
                                        wNo_schoolin__completed,
                                        w9th_grade,
                                        w8th_grade,
                                        w7th_grade,
                                        w6th_grade,
                                        w5th_grade,
                                        w4th_grade,
                                        w3rd_grade,
                                        w2nd_grade,
                                        w1st_grade,
                                        w12th_grade_no_diploma,
                                        w11th_grade,
                                        w10th_grade,
                                        wKindergarten,
                                        wRegular_high_school_diploma, 
                                            wGED_or_alternative_credential,
                                        wSome_college_less_than_1_year,
                                            wSome_college_1_or_more_years_no_degree ,
                                            wAssociate_degree,
                                        wMaster_degree,
                                        wProfessional_school_degree,
                                        wDoctorate_Degree
                                        
                                        ))

# Example: precentage of Bachelor degree's for specific tracts 

w_edu_attain$percentLess_Than_High_School <- (w_edu_attain$Less_Than_High_School /w_edu_attain$wTotal)
w_edu_attain$percentHigh_School_or_Equivalent  <- (w_edu_attain$High_School_or_Equivalent /w_edu_attain$wTotal)
w_edu_attain$percentAssociates_or_Some_College <- (w_edu_attain$Associates_or_Some_College /w_edu_attain$wTotal)
w_edu_attain$percentBachelor_degree <- (w_edu_attain$wBachelor_degree /w_edu_attain$wTotal)
w_edu_attain$percentDoctoral_Professional_Masters  <- (w_edu_attain$Doctoral_Professional_Masters /w_edu_attain$wTotal)

w_edu_attain[w_edu_attain=="NaN"] <- 0

w_edu_attain$PostHighschool <- w_edu_attain$percentDoctoral_Professional_Masters + w_edu_attain$percentBachelor_degree + w_edu_attain$percentAssociates_or_Some_College
w_edu_attain$Highandless <- w_edu_attain$percentLess_Than_High_School + w_edu_attain$percentHigh_School_or_Equivalent

#Export to plot in Tableau
write.csv(w_edu_attain, "edu_attainment.csv")


# Additional formating for ploting
z <- w_edu_attain[, c(1,13,14)]

z <- gather(w_edu_attain,PostHighschool, Highandless, key = edu, value = percent )
