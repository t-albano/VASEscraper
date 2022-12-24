library(data.table)
library(tidyverse)

vase <- fread("VASE_rawdata.csv")
tea22 <- fread("TEA22_rawdata.csv")

# check variable names
colnames(vase)
colnames(tea22)

# rename column names
vase <- vase %>%
  rename("Art_Region" = Region) %>%
  mutate("Region" = NA)

tea22 <- tea22 %>%
  select(CAMPNAME, CPE0312C, GRDLOW,
         GRDHIGH, REGION, DISTNAME) %>%
  rename("School" = CAMPNAME,
         "Enrolled" = CPE0312C,
         "Lowest_Grade" = GRDLOW,
         "Highest_Grade" = GRDHIGH,
         "Region" = REGION,
         "District" = DISTNAME) %>%
  filter(Highest_Grade %in% c(9, 10, 11, 12)) #reduce size of df and repetitiveness of school names

# standardize Region distinctions
# remove subgrouping of region to match with TEA
for (i in (1:len(vase$Art_Region))){
  if (vase$Art_Region[i][len(vase$Art_Region[i])] %in% c("N", "E", "S", "W")){
    vase$Region <- substring(vase$Art_Region[i],1,len(vase$Art_Region[i])-1)
  }
}

# remove leading zeros and apostrophe of Region to match with VASE
for (i in (1:len(tea22$Region))){
  if("'0" == substring(tea22$Region[i], 1, 2)){
    tea22$Region[i] <- tea22$Region[i][3]
  }
  if("'" == tea22$Region[i][1]){
    tea22$Region[i] <- substring(tea22$Region[i], 2, 3)
  }
}

# standardize VASE HS names in similar format as TEA22
# somewhat standardize names to later aid in fuzzy matching
unique(vase$School)

# clean HS and ECHS at end of names first

# function to clean up endings
re_string <- function(vect, suffixes, ending){
  column <- vect
  for (i in suffixes){
    for (j in 1:length(column)){
      word <- paste("",i)
      if(word == substring(column[j],nchar(column[j])-nchar(word)+1,nchar(column[j]))){
        a = substring(column[j],1,nchar(column[j])-nchar(word))
        column[j] <- paste0(a, ending)
      }
    }
  }
  return(column)
}

# endings to clean up
end_hs <- c("HS", "hs", "High Schoo.l", 
          "High School", "Hs", "Highschool",
          "H.s.", "High", "High-school",
          "H. S.", "H")

end_echs <- c("E C H S", "Echs", "Early College HS",
             "Early College High School", "Early College H S")

end_extra <- c("Faa", ", Reg. 16", ", Reg. 20w",
               "9", "ISD", "Isd")


vase$School <- re_string(vase$School, end_hs, " H S")
vase$School <- re_string(vase$School, end_echs, " ECHS")
vase$School <- re_string(vase$School, end_extra, "")
tea22 <- re_string(tea22$School, end_echs, " ECHS")


unique(vase$School)


# clean Region in TEA22 to aid in fuzzy matching

# collapse VASE HS results for each year to count of 2d, 3d, and total

# fuzzy join by school name to get district 
#(see if join all important columns, cause then just select ones needed for each data set)


# edit school name formatting
sapply(vase, class)

col_fix <- c("Division", "Region", "School", "Gold_Seal", "Dimension")
vase <- vase %>% 
  mutate_at(col_fix, factor)

vase <- vase %>% 
  mutate_at("School", as.character)

sapply(vase, class)

# save as VASE_data_visualization csv

# *reduce VASE HS for just the 2022 year

# *fuzzy join by school name to get district

# save as VASE_data_statTests.csv

# Replace spaced out H S with combined HS
test <- fuzzyjoin::stringdist_left_join(vase, tea22, by = c("School","Region"),
                                   mode ='left', ignore_case = TRUE, method = 'jw',
                                   distance_col = NULL)