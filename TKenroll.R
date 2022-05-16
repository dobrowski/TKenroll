

library(tidyverse)
library(MCOE)
library(here)
library(googlesheets4)
library(lubridate)
library(readxl)
library(ggthemes)
library(googlesheets4)
library(leaflet)

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

### Population Estimates ---- 

# P2B_County_Age for projection of 4 year olds compared to 2020.  
# From https://www.dof.ca.gov/forecasting/demographics/projections/

pop.4.proj <- read_excel("P2B_County_Age.xlsx",
                       sheet = "County Population by Age",
                       range = "A3:BA3365") # Doens't get all counties

mry.4.proj <- pop.4.proj %>%
    filter(County == "Monterey County",
           Age == 4) %>%
    pivot_longer(cols = `2010`:`2060`) %>%
    mutate(perc.of.2019 = value/6366,
          year.of.birth = as.numeric(name) - 4)

mry.4.proj.slim <- mry.4.proj %>%
  select(year.of.birth,
         county.4yo.2019.perc = perc.of.2019)

### Births ----
# https://data.chhs.ca.gov/dataset/cdph_live-birth-by-zip-code

births <- read_csv("2021-10-18_births_final_zip_year_sup.csv")

mry.zips <- 
c(95076L, 93906L, 93905L, 93955L, 93940L, 93901L, 93960L, 93933L, 93907L, 93927L, 93908L, 93930L, 93923L, 93950L, 95012L, 93926L, 93451L, 93924L, 95039L, 95004L, 93953L, 93920L, 93925L, 93426L, 93944L, 93921L, 93450L, 93943L, 93928L, 93954L, 93902L, 93915L, 93912L, 93922L, 93932L, 93942L, 93962L)

births.mry <- births %>%
    filter(ZIP_Code %in% mry.zips,
           Year >= 2008
           ) 

birth.by.year <- births.mry %>%
    group_by(Year) %>%
    transmute(total = sum(Count, na.rm = TRUE)) %>%
    unique()


ggplot(births.mry, aes(x = Year, y = Count, group = ZIP_Code))+
    geom_line()


### School Enrollment -------
#  From https://www.cde.ca.gov/ds/ad/filesenr.asp

ENROLL <- tbl(con, "ENROLLMENT") %>%
    filter(#YEAR == "19", # To match the last TK data file year 19-20
        COUNTY == "Monterey") %>%
    collect() 


enrollment.sum <- ENROLL %>%
    group_by(DISTRICT, YEAR) %>%
    summarise(k.tot = sum(KDGN),
              gr1.tot = sum(GR_1))

enrollment.sum.19 <- enrollment.sum %>%
    filter(YEAR == "19")

#  TK files https://www.cde.ca.gov/ds/ad/filestkdata.asp

TK <- tbl(con, "TK") %>%
    filter(#Year == max(Year),
           Reporting_Level == "District",
           Race =="All",
           Gender == "All",
         #  Subgroup == "All"
           ) %>%
    collect() 

TK2 <- TK %>%
    filter( str_starts(CDS_Code,"27"),
            Subgroup == "ALL"
    ) %>%
    select(Year, Name, Total_Kindergarten_Enrollment_Census_Day, Kindergarten_TK_Program_Participation_Census_Day) %>%
    mutate(k_without_TK = Total_Kindergarten_Enrollment_Census_Day - Kindergarten_TK_Program_Participation_Census_Day)



###  School Districts to Zip Codes ----
# https://nces.ed.gov/programs/edge/Geographic/RelationshipFiles
# school.zip <- read_excel("grf21_lea_zcta5ce20.xlsx")
# 
# school.zip.mry <- school.zip %>%
#     filter(ZCTA5CE20 %in% mry.zips)
# 
# write.csv(school.zip.mry, "School Zip crosswalk.csv")

#  My reduced version with only primary 
schools <- read_csv("School Zip crosswalk.csv")

### Joining Schools to Births ----

zip.births.mry <- schools %>%
    left_join(births.mry, by = c("ZCTA5CE20" = "ZIP_Code")) %>%
    group_by(NAME_LEA21, year.of.birth = Year) %>%
    transmute(babies = sum(Count, na.rm = TRUE)) %>%
    unique() %>%
    mutate(short.name = str_sub(NAME_LEA21 ,1,6)  )

joiner <- TK2 %>%
    mutate(year.of.birth = as.numeric(str_sub(Year, 1,4)) -5, 
           short.name = str_sub(Name ,1,6)
    ) %>%
    filter(!str_detect(Name,"Education")) %>%
    full_join(zip.births.mry) %>%
    select(-Name)

# 
# enrollment.sum.old <- enrollment.sum %>%
#     mutate(year.of.birth = as.numeric(YEAR) + 1995,
#            year.of.five = as.numeric(YEAR) + 2000,
#            short.name = str_sub(DISTRICT ,1,6)) %>%
#     filter(!str_detect(DISTRICT,"High"),
#            !str_detect(DISTRICT,"Education"))
# 
# zip.births.mry2  <- zip.births.mry %>%
#     left_join(enrollment.sum.old)

# lm.model <- zip.births.mry2 %>%
#     group_by(NAME_LEA21) %>%
#     nest() %>%
#     mutate(lm_obj =  map(data, ~lm(k.tot~babies, data = .x))) %>%
#     mutate(prediction = map2(lm_obj, data, function(.lm, .data) predict(.lm, .data) )) %>%
#     unnest(cols = c(data, prediction)) 
           

lm.model <- joiner %>%
    group_by(NAME_LEA21) %>%
    nest() %>%
    mutate(lm_obj =  map(data, ~lm(k_without_TK~babies, data = .x))) %>%
    mutate(prediction = map2(lm_obj, data, function(.lm, .data) predict(.lm, .data) )) %>%
    mutate(graph =  map(data,
                        ~ggplot(aes(y = k_without_TK, x =babies, label = year.of.birth), data = .x) +
                            theme_hc() +
                            geom_point() +
                            geom_smooth(method="lm", se = FALSE) +
                            geom_label() +
                            # expand_limits(y = c(0,10),
                            #               x = c(0,10)) +
                            labs(title = NAME_LEA21[1],
                                 x = "Number of babies born in associated zip code(s)",
                                 y = "Kinder enrollment excluding TK 5 years later")
                        )) %>%
    mutate(graph.births =  map(data,
                        ~ggplot(aes(y = babies, x = (year.of.birth)), data = .x) +
                            theme_hc() +
                            geom_line() +
                            scale_x_continuous(breaks = 2008:2020) +
#                   expand_limits(x = c(2008,2020)) +
                   labs(title = NAME_LEA21[1],
                        x = "Year",
                        y = "Number of Births in Associated Zip Code(s)")
    )) %>%
    unnest(cols = c(data, prediction)) %>%
    arrange(NAME_LEA21,Year)



lm.predicts <- lm.model %>%
    select(NAME_LEA21, year.of.birth, prediction2023 = prediction) %>%
    filter(year.of.birth == 2018) %>%
    select(-year.of.birth) %>%
    mutate(Year = "2019-20")


lm.multi <- lm.model %>%
  mutate(year.of.kinder = paste0(year.of.birth + 5,"-", year.of.birth + 6 -2000)  ) %>%
  select(NAME_LEA21, short.name, year.of.birth, year.of.kinder, babies, Total_Kindergarten_Enrollment_Census_Day:k_without_TK, prediction)

to.join.multi <- enrollment.sum %>%
  filter(# YEAR == 20,
         !str_detect(DISTRICT,"Education"),
         !str_detect(DISTRICT,"High")) %>%
  transmute(year.of.kinder = as.numeric(YEAR) + 2000,
         year.of.first = as.numeric(YEAR) + 2001,
            short.name = str_sub(DISTRICT ,1,6),
         k.tot = k.tot,
            gr1.tot = gr1.tot
  )

meta <- to.join.multi %>%
  left_join(to.join.multi, by = c("year.of.first" = "year.of.kinder", "short.name" = "short.name")) %>%
  mutate(year.of.kinder = paste0(year.of.kinder,"-", year.of.kinder + 1 -2000) ,
         year.of.first = paste0(year.of.first,"-", year.of.first + 1 -2000)  ) %>%
  select(short.name, year.of.kinder, year.of.first, k.tot = k.tot.x, gr1.tot = gr1.tot.y)


latest.TK.slim <- lm.multi %>%
  select(NAME_LEA21, year.of.birth, Kindergarten_TK_Program_Participation_Census_Day) %>% 
  filter(!is.na(Kindergarten_TK_Program_Participation_Census_Day)) %>%
  group_by(NAME_LEA21) %>%
  filter(year.of.birth == max(year.of.birth)) %>%
  ungroup() %>%
  select(NAME_LEA21, latest.tk.tot = Kindergarten_TK_Program_Participation_Census_Day)



multi.meta <- lm.multi %>%
  left_join(meta)




###
# 
# joint <- left_join(TK2, enrollment.sum.19, by = c("Name" = "DISTRICT")
#                    ) %>%
#     left_join(lm.model) %>%
# #    group_by(Name) %>%
#     mutate(est.tk20.gr1enr = round2(pull(mry.4.proj[(mry.4.proj$name == 2020),"perc.of.2019"])*gr1.tot*as.numeric(mdy("12/2/2016") - mdy("9/2/2016"))/365, 0 ), 
#            est.tk23.gr1enr = round2(pull(mry.4.proj[(mry.4.proj$name == 2023),"perc.of.2019"])*gr1.tot*as.numeric(mdy("2/2/2018") - mdy("9/2/2017"))/365, 0 ), 
#            est.tk24.gr1enr = round2(pull(mry.4.proj[(mry.4.proj$name == 2024),"perc.of.2019"])*gr1.tot*as.numeric(mdy("4/2/2019") - mdy("9/2/2018"))/365, 0 ),
#            est.tk25.gr1enr = round2(pull(mry.4.proj[(mry.4.proj$name == 2025),"perc.of.2019"])*gr1.tot*as.numeric(mdy("6/2/2019") - mdy("9/2/2018"))/365, 0 ),
#            est.tk23.TKactual = round2(pull(mry.4.proj[(mry.4.proj$name == 2020),"perc.of.2019"])* Kindergarten_TK_Program_Participation_Census_Day* as.numeric(mdy("2/2/2018") - mdy("9/2/2017")) / as.numeric(mdy("12/2/2016") - mdy("9/2/2016")),0 ),
#     )  %>%
#     arrange(Name) %>%
#     mutate(
#         est.tk23.mean = round2( mean(c(est.tk23.gr1enr,est.tk23.TKactual)),0 ) ,
#         est.tk23.teachers = ceiling( est.tk23.mean/12 )
#         
#     )# %>%
# #    ungroup()
# 
# joint2 <- left_join(joint,lm.model)

enrollment.sum.20 <- enrollment.sum %>%
    filter(YEAR == 20,
           !str_detect(DISTRICT,"Education"),
           !str_detect(DISTRICT,"High")) %>%
    transmute(year.in.first = "2020-21",
           Year = "2019-20",
           short.name = str_sub(DISTRICT ,1,6),
           gr1.tot = gr1.tot
    )


enrollment.sum.20.slim <- enrollment.sum.20 %>%
  ungroup() %>%
  select(short.name, 
         latest.1gr.tot = gr1.tot)

  

#  Check if there is a duplication with Salinas High and MCOE first thing!!!!

joint <- lm.model %>%
    left_join(enrollment.sum.20) %>%
    left_join(lm.predicts) %>%
    filter(Year == "2019-20") %>%
    mutate(est.tk20.county = round2(pull(mry.4.proj[(mry.4.proj$name == 2020),"perc.of.2019"])*gr1.tot*as.numeric(mdy("12/2/2016") - mdy("9/2/2016"))/365, 0 ), 
           est.tk23.county = round2(pull(mry.4.proj[(mry.4.proj$name == 2023),"perc.of.2019"])*gr1.tot*as.numeric(mdy("2/2/2018") - mdy("9/2/2017"))/365, 0 ), 
           est.tk24.county = round2(pull(mry.4.proj[(mry.4.proj$name == 2024),"perc.of.2019"])*gr1.tot*as.numeric(mdy("4/2/2019") - mdy("9/2/2018"))/365, 0 ),
           est.tk25.county = round2(pull(mry.4.proj[(mry.4.proj$name == 2025),"perc.of.2019"])*gr1.tot*as.numeric(mdy("6/2/2019") - mdy("9/2/2018"))/365, 0 ),
           est.tk23.TKactual = round2(pull(mry.4.proj[(mry.4.proj$name == 2023),"perc.of.2019"])* Kindergarten_TK_Program_Participation_Census_Day* as.numeric(mdy("2/2/2018") - mdy("9/2/2017")) / as.numeric(mdy("12/2/2016") - mdy("9/2/2016")),0 ),
           est.tk23.zip = round2(prediction2023*as.numeric(mdy("2/2/2018") - mdy("9/2/2017"))/365, 0 ), 
           )  %>%
    mutate(
        est.tk23.mean = round2( mean(c(est.tk23.county,est.tk23.TKactual,est.tk23.zip)),0 ) ,
        est.tk23.teachers = ceiling( est.tk23.mean/12 )
        
    ) %>%
    ungroup() %>%
    arrange(NAME_LEA21)



calendar.portion <- tribble(
  ~year.of.kinder, ~calendar.perc,
"2020-21"  ,as.numeric(mdy("12/2/2016") - mdy("9/2/2016"))/365, 
"2021-22"  ,as.numeric(mdy("12/2/2017") - mdy("9/2/2017"))/365, 
"2022-23"  ,as.numeric(mdy("2/2/2019") - mdy("9/2/2018"))/365, 
"2023-24"  ,as.numeric(mdy("4/2/2020") - mdy("9/2/2019"))/365,
"2024-25"  ,as.numeric(mdy("6/2/2021") - mdy("9/2/2020"))/365,
"2025-26" ,as.numeric(mdy("9/2/2022") - mdy("9/2/2021"))/365,
)



alt.joint <- multi.meta %>%
  left_join(mry.4.proj.slim) %>%
  left_join(calendar.portion) %>%
  left_join(enrollment.sum.20.slim) %>%
  left_join(latest.TK.slim) %>%
  mutate(est.1gr = round2(county.4yo.2019.perc*latest.1gr.tot*calendar.perc, 0),
         est.TKactual = round2(county.4yo.2019.perc*latest.tk.tot*calendar.perc * 365/as.numeric(mdy("12/2/2016") - mdy("9/2/2016")), 0),
         est.zip.births = round2(unlist( unname(prediction))*calendar.perc,0)
         ) %>%
  mutate(
    est.mean = round2( (est.1gr+est.TKactual+est.zip.births)/3 ,0),
    est.teachers = ceiling( est.mean/12) )


alt.joint.shiny <- alt.joint %>%
  select(NAME_LEA21, Year = year.of.kinder, starts_with("est")) %>%
  filter(Year >= "2022-23") %>%
  ungroup()


write_csv(alt.joint, "joint.csv")

ss <- "https://docs.google.com/spreadsheets/d/1p5EPFn9zkZGClmYPFC09cin8QzBFS5ZVa9y0O5GO84c/edit#gid=1739770238"

write_sheet(joint, ss = ss)
write_rds(joint,here("TKestimates","joint.rds"))

write_rds(alt.joint.shiny,here("TKestimates","altjoint.rds"))

write_rds(schools,here("TKestimates","schools.rds"))


#  Total population changes 
#  Preschool enrollment
#  



### EETDG ------


cc1 <-  read_csv(here("data","ChildCareCenters02272022.csv"))
cc2 <- read_csv(here("data","CHILDCAREHOMEmorethan802272022.csv"))

ccs <- rbind(cc1,cc2) 

ccs2 <- ccs %>%
  filter(`County Name` == "MONTEREY",
         `Facility Status` != "CLOSED") %>%
  mutate(preschool = str_detect(`Facility Name`, "PRESCHOOL"),
         state = str_detect(`Facility Name`, "STATE"),)

write_csv(ccs2, "Monterey List.csv")


# https://www.cde.ca.gov/fg/fo/profile.asp?id=5771&recID=5771

mry.allocation <- 1466143

applicants <- c("Salinas City Elementary School District",
  "Alisal Union Elementary School District",
  "North Monterey County Unified School District",
  "Monterey Peninsula Unified School District",
  "San Ardo Union Elementary School District",
  "King City Union Elementary School District",
  "Gonzales Unified School District",
  "Carmel Unified School District",
  "Monterey Bay Charter School",
  "Monterey County Office of Education")


# The percentage of TK and K pupils eligible for free and reduced-price meals.
# The percentage of dual language learners that the LEA is serving or is planning to serve in a CSPP or TK program.
# The percentage of pupils with disabilities the LEA is serving or planning to serve in an inclusive CSPP or TK program.
# The percentage of pupils served, or planned to be served in a full day CSPP, TK, or K programs offered by the LEA or community-based organizations.
# The extent to which applicants operate in an attendance area where a significant disproportionality of particular race or ethnicities, as describes in Section 1418(d) of Title 20 of the United States Code, has been identified in special education.
# The extent to which the LEA is located in an area that has more than three young children, three to five years of age, inclusive, for every licensed childcare slot.
# The extent to which applicants plan to partner with community based CSPP and Head Start programs in their program area to ensure those teachers have access to professional development along with teachers employed by the LEA.


# 5712 four year olds in 2022 estimate

total <- zip.births.mry %>%
    filter(year.of.birth == 2018) %>%
#    filter(NAME_LEA21 %in% applicants) %>%
    ungroup() %>%
    mutate(#total = sum(babies),
           teachers = ceiling(babies/12), 
           # portion = babies/total,
           # allocation = portion*mry.allocation,
           # all.per.teacher = allocation/teachers,
           mry.portion = babies/6941, # total 2018 county babies 6941
           mry.allocation = mry.portion*mry.allocation,
           mry.all.per.teacher = mry.allocation/teachers,
    ) %>%
  mutate(short.name = str_sub(NAME_LEA21 ,1,6) %>% str_to_lower() ) 
  

total.joint <- joint %>%
 #   filter(year.of.birth == 2018) %>%
    filter(NAME_LEA21 %in% applicants) %>%
    ungroup() %>%
    mutate(total = sum(est.tk23.mean),
           teachers = ceiling(est.tk23.mean/12), 
           portion = est.tk23.mean/total,
           allocation = portion*mry.allocation,
           all.per.teacher = allocation/teachers,
           mry.portion = babies/6941,
           mry.allocation = mry.portion*mry.allocation,
           mry.all.per.teacher = mry.allocation/teachers,
    )

# 
# Tiered system
# - If use the slots available, ok 
# - If not use the slots, given into round 2
# - If use extra slots, initial ones covered and extras put into pull in round 2
# - Round 2
# - Cover extras from round 1
# - If not cover all the extras, funded by priority zip code portionally
# - If cover all extras, and still have funds, pull in round 3
# - Round 3 
# - Available for any educations in priotity areas
# - Round 4 
# - Available for any educators in non-priority areas



TK.eedgt <- TK %>%
  filter( str_starts(CDS_Code,"27"),
          Year == max(Year)
   #       Subgroup == "All"
  ) %>%
  select(Year, Name, Subgroup, Total_Kindergarten_Enrollment_Census_Day, Kindergarten_TK_Program_Participation_Census_Day) %>%
  mutate(k_without_TK = Total_Kindergarten_Enrollment_Census_Day - Kindergarten_TK_Program_Participation_Census_Day) %>%
  mutate(Subgroup = recode(Subgroup, "Mig" = "MIG")) %>%
  pivot_wider(names_from = c(Subgroup), values_from = c(Total_Kindergarten_Enrollment_Census_Day, Kindergarten_TK_Program_Participation_Census_Day,k_without_TK )) %>%
  mutate(perc.EL = k_without_TK_EL/k_without_TK_ALL,
         perc.SD = k_without_TK_SD/k_without_TK_ALL)





cspp <- read_sheet("https://docs.google.com/spreadsheets/d/1JMPrB_Qvvw94rTCdf8u2flP34PP_LJAF-GzG-bulOQg/edit#gid=2114456333")%>%
  mutate(head_start = str_detect(`Facility Name`, "HEAD") )


cspp2 <- cspp %>%
  filter(CSPP == TRUE,
         `Facility Type` != "INFANT CENTER") %>%
  mutate(NAME_LEA21 = `LEA Geographically associated`) %>%
  select(NAME_LEA21, `Facility Name`,Licensee ,`Facility Capacity`, `Facility City`, `Facility Zip`, `Facility Capacity`, `Facility Type`, head_start) 



write_rds(cspp2,here("TKestimates","cspp-list.rds"))

cspp3 <- cspp2 %>%
  group_by(`LEA Geographically associated`) %>%
  transmute(lea.cspp.capacity = sum(`Facility Capacity`)) %>% 
  distinct()


cspp.w.o.headstart <- cspp2 %>%
  filter(head_start == FALSE) %>%
  group_by(`LEA Geographically associated`) %>%
  transmute(lea.cspp.capacity.excluding.head.start = sum(`Facility Capacity`))%>% 
  distinct()


cspp.sum <- cspp3 %>%
  left_join(cspp.w.o.headstart) %>%
  mutate(short.name = str_sub(`LEA Geographically associated` ,1,6) %>% str_to_lower() ) %>%
  left_join(total) %>%
  mutate(babies.to.cspp = babies/lea.cspp.capacity)


### county-wide estimates

mry.4.proj %>%
    filter(name >= 2022,
           name <= 2026) %>%
ggplot( aes(x = name, y = value) ) +
    geom_col(fill = "blue") +
    coord_flip(ylim = c(5400, 5800)) +
    theme_hc() 


graph.df <- calendar.portion %>%
    mutate(name = str_c(20 ,str_sub(year.of.kinder,6,7))) %>%
    left_join(mry.4.proj) %>%
    mutate(tks = calendar.perc*value)



ggplot() +
    geom_bar(data = graph.df, stat = "identity", position = "identity", width = 0.7, 
             aes(x = name, y = value, fill = "Total")) +
    geom_bar(data = graph.df, stat = "identity", position = "identity", width = 0.35, 
             aes(x = name, y = tks, fill = "TK Eligible")) +
    geom_text(data = graph.df, stat = "identity", position = "identity", color = "white", 
             aes(x = name, y = tks - 300, label = label_comma(accuracy = 1)(tks)) )+
    coord_flip() +
    theme_minimal() +
    scale_x_discrete(
        limits = graph.df$name,
        expand = c(0, 0.5)
    ) +
    scale_fill_manual(
        guide = "legend",
        values = c("Total" = "lightblue", "TK Eligible" = "dodgerblue3"),
        limits = c("Total", "TK Eligible")
    ) +
    theme(
        legend.position = c(1 ,1.05),
        legend.justification = 1,
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.key.size = unit(0.7, "lines"),
        legend.text = element_text(size = 9,
                                   color = "gray40",
                                   face = "bold",
                                   family = "sans"),
        plot.margin = unit(c(2, 1, 0.4, 1), "lines"),
        axis.title = element_blank(),
        axis.text = element_text(
            size = 10,
            color = "gray40",
            face = "bold",
            family = "sans"),
        panel.grid.major.x = element_line(
            color = "grey", 
            size = 0.3),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
    ) +
    labs(title = "Total Number of 4-Year-olds in Monterey County \nand those TK Eligible",
         caption = "From 2023 to 2026, legislation annually expands the range of birthdays for eligiblity.")


ggsave("countywide 4 yr old.png", width = 7, height = 5)
