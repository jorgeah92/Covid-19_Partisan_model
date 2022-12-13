# Load Covid data.
c <- read.csv("covid-data-cleaned.csv", header = TRUE)
party <- read.csv("Party_by_state.csv", header=FALSE)
# Clean party data
names(party) <- as.matrix(party[3, ])
party <- party[-c(1:3), -c(7)]
# Add party dataframe to c dataframe
b <- left_join(c, party, by=c( "state" = "Location"))
# Convert values under sip to date values so that they can be subtracted
b$date_sip_start <- as.character(b$date_sip_start)
b$date_sip_start <- as.Date(b$date_sip_start, format='%m/%d/%Y')

b$date_sip_end <- as.character(b$date_sip_end)
b$date_sip_end[b$date_sip_end == '0'] <- "7/2/2020" # Last day data was updated for shelter in place
b$date_sip_end <- as.Date(b$date_sip_end, format='%m/%d/%Y')

# Form date difference
b$sip_duration <- b$date_sip_end - b$date_sip_start
b$sip_duration[is.na(b$sip_duration)] <- 0


transformPoliticalParty <- function(partyList) {
  
  numericList <- c()
  
  for (party in partyList) {
    code = 3
    if (!is.na(party)) {
      if (party == "Republican") {
        code = 1
      }
      if (party == 'Democrat') {
        code = 2
      }
    } else {
      code = 3
    }

    numericList <- c(numericList, code)
  }
  numericList
}



c.data<- data.frame(b$total_cases/b$total_test_results, b$total_deaths,b$pop_density , as.numeric(b$sip_duration) , b$pcnt_below_pov_2018 , b$prop_pop_0_18 , b$prop_pop_19_25 , b$prop_pop_26_34 , b$prop_pop_35_54 , b$prop_pop_55_64 , b$prop_pop_65_over)
c.data<- data.frame(b$total_cases/b$total_test_results, b$total_deaths, b$death_100k, b$pop_density , b$pcnt_ser_ill_risk_covid, as.numeric(b$sip_duration) , b$pcnt_below_pov_2018 , b$prop_pop_0_18 , b$prop_pop_19_25 , b$prop_pop_26_34 , b$prop_pop_35_54 , b$prop_pop_55_64 , b$prop_pop_65_over)

c.data<- data.frame(b$total_cases/b$total_test_results, b$total_deaths, b$death_100k, b$pop_density , transformPoliticalParty(b$`Governor Political Affiliation`) ,b$pcnt_ser_ill_risk_covid, as.numeric(b$sip_duration) , b$pcnt_below_pov_2018 , b$prop_pop_0_18 , b$prop_pop_19_25 , b$prop_pop_26_34 , b$prop_pop_35_54 , b$prop_pop_55_64 , b$prop_pop_65_over)

c.data<- data.frame(b$total_cases/b$total_test_results, b$total_deaths/b$pop_2018, b$death_100k, b$pop_density , transformPoliticalParty(b$`Governor Political Affiliation`))
cor(c.data)


cmodel1 <- lm(total_cases/total_test_results ~ log(pop_density), data=b)

summary(cmodel1)

cmodel2 <- lm(total_cases/total_test_results ~ sip_duration + log(pop_density), data=b)

summary(cmodel2)


cmodel3 <- lm(total_cases/total_test_results ~ sip_duration + log(pop_density) + `Governor Political Affiliation`, data=b)

summary(cmodel3)

cmodel4 <- lm(log(total_deaths/pop_2018) ~ sip_duration + log(pop_density) + transformPoliticalParty.b..Governor.Political.Affiliation.., data=b)

cmodel4 <- lm(total_deaths/pop_2018 ~ sip_duration + log(pop_density) + transformPoliticalParty(`Governor Political Affiliation`), data=b)


summary(cmodel4)

cmodel5 <- lm(log(total_deaths/pop_2018) ~ sip_duration + log(pop_density) + transformPoliticalParty(`Governor Political Affiliation`), data=b)

cmodel6 <- lm(log(total_deaths/pop_2018) ~ transformPoliticalParty(`Governor Political Affiliation`), data=b)

# Arizona duplicates.  Discarded.

