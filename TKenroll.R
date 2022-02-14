

library(tidyverse)
library(MCOE)
library(here)
library(googlesheets4)
library(lubridate)
library(readxl)



con <- MCOE::mcoe_sql_con()

### Import files ------

# pop.proj <- read_excel("2021SeriesReportsw (1).xlsx",
#                        sheet = "2021 Series Table",
#                        range = "A2:S3365")
# 
# mry.proj <- pop.proj %>%
#     filter(County == "MONTEREY") %>%
#     select(`School Year`,
#            Kindergarten) %>%
#     mutate(perc = Kindergarten/)
    
pop.4.proj <- read_excel("P2B_County_Age.xlsx",
                       sheet = "County Population by Age",
                       range = "A3:BA3365") # Doens't get all counties

mry.4.proj <- pop.4.proj %>%
    filter(County == "Monterey County",
           Age == 4) %>%
    pivot_longer(cols = `2010`:`2060`) %>%
    mutate(perc.of.2019 = value/6366)

# https://data.chhs.ca.gov/dataset/cdph_live-birth-by-zip-code

births <- read_csv("2021-10-18_births_final_zip_year_sup.csv")

mry.zips <- 
c(95076L, 93906L, 93905L, 93955L, 93940L, 93901L, 93960L, 93933L, 93907L, 93927L, 93908L, 93930L, 93923L, 93950L, 95012L, 93926L, 93451L, 93924L, 95039L, 95004L, 93953L, 93920L, 93925L, 93426L, 93944L, 93921L, 93450L, 93943L, 93928L, 93954L, 93902L, 93915L, 93912L, 93922L, 93932L, 93942L, 93962L)

births.mry <- births %>%
    filter(ZIP_Code %in% mry.zips,
           Year >= 2012
           ) 

birth.by.year <- births.mry %>%
    group_by(Year) %>%
    transmute(total = sum(Count, na.rm = TRUE)) %>%
    unique()


ggplot(births.mry, aes(x = Year, y = Count, group = ZIP_Code))+
    geom_line()


sas <- haven::read_sas("EDGE_ZCTALOCALE_2021_LOCALE.sas7bdat")

# https://nces.ed.gov/programs/edge/Geographic/RelationshipFiles
school.zip <- read_excel("grf21_lea_zcta5ce20.xlsx")


school.zip.mry <- school.zip %>%
    filter(ZCTA5CE20 %in% mry.zips)

write.csv(school.zip.mry, "School Zip crosswalk.csv")


schools <- read_csv("School Zip crosswalk.csv")



zip.births.mry <- schools %>%
    left_join(births.mry, by = c("ZCTA5CE20" = "ZIP_Code")) %>%
    group_by(NAME_LEA21, Year) %>%
    transmute(babies = sum(Count, na.rm = TRUE)) %>%
    unique() %>%
    mutate(short.name = str_sub(NAME_LEA21 ,1,6),
           year.of.birth = Year)

enrollment.sum.old <- enrollment.sum %>%
    mutate(year.of.birth = as.numeric(YEAR) + 1995,
           year.of.five = as.numeric(YEAR) + 2000,
           short.name = str_sub(DISTRICT ,1,6)) %>%
    filter(!str_detect(DISTRICT,"High"),
           !str_detect(DISTRICT,"Education"))

zip.births.mry2  <- zip.births.mry %>%
    left_join(enrollment.sum.old)

#  TK files https://www.cde.ca.gov/ds/ad/filestkdata.asp

TK <- tbl(con, "TK") %>%
    filter(Year == max(Year),
           Reporting_Level == "District",
           Race =="All",
           Gender == "All",
           Subgroup == "All") %>%
    collect() 

# P2B_County_Age for projection of 4 year olds compared to 2020.  
# From https://www.dof.ca.gov/forecasting/demographics/projections/

TK2 <- TK %>%
    filter( str_starts(CDS_Code,"27")
    ) %>%
    select(Name, Total_Kindergarten_Enrollment_Census_Day, Kindergarten_TK_Program_Participation_Census_Day) %>%
    mutate(k_without_TK = Total_Kindergarten_Enrollment_Census_Day - Kindergarten_TK_Program_Participation_Census_Day)


#  From https://www.cde.ca.gov/ds/ad/filesenr.asp

ENROLL <- tbl(con, "ENROLLMENT") %>%
    filter(#YEAR == "19", # To match the last TK data file year 19-20
           COUNTY == "Monterey") %>%
    collect() 


enrollment.sum <- ENROLL %>%
    group_by(DISTRICT, YEAR) %>%
    summarise(k.tot = sum(KDGN),
              gr1.tot = sum(GR_1))

enrollment.sum.19 = enrollment.sum %>%
    filter(YEAR == "19")


joint <- left_join(TK2, enrollment.sum.19, by = c("Name" = "DISTRICT")
                   ) %>%
    group_by(Name) %>%
    mutate(est.tk20.gr1enr = round2(pull(mry.4.proj[(mry.4.proj$name == 2020),"perc.of.2019"])*gr1.tot*as.numeric(mdy("12/2/2016") - mdy("9/2/2016"))/365, 0 ), 
           est.tk23.gr1enr = round2(pull(mry.4.proj[(mry.4.proj$name == 2023),"perc.of.2019"])*gr1.tot*as.numeric(mdy("2/2/2018") - mdy("9/2/2017"))/365, 0 ), 
           est.tk24.gr1enr = round2(pull(mry.4.proj[(mry.4.proj$name == 2024),"perc.of.2019"])*gr1.tot*as.numeric(mdy("4/2/2019") - mdy("9/2/2018"))/365, 0 ),
           est.tk25.gr1enr = round2(pull(mry.4.proj[(mry.4.proj$name == 2025),"perc.of.2019"])*gr1.tot*as.numeric(mdy("6/2/2019") - mdy("9/2/2018"))/365, 0 ),
           est.tk23.TKactual = round2(pull(mry.4.proj[(mry.4.proj$name == 2020),"perc.of.2019"])* Kindergarten_TK_Program_Participation_Census_Day* as.numeric(mdy("2/2/2018") - mdy("9/2/2017")) / as.numeric(mdy("12/2/2016") - mdy("9/2/2016")),0 ),
    )  %>%
    arrange(Name) %>%
    mutate(
        est.tk23.mean = mean(c(est.tk23.gr1enr,est.tk23.TKactual)),
        est.tk23.teachers = ceiling( est.tk23.mean/12 )
        
    )




ss <- "https://docs.google.com/spreadsheets/d/1p5EPFn9zkZGClmYPFC09cin8QzBFS5ZVa9y0O5GO84c/edit#gid=1739770238"

write_sheet(joint, ss = ss)


#  Total population changes 
#  Preschool enrollment
#  


