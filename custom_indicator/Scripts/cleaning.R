#KP disaggs cleaning
kp_disaggs_counts <- read.csv("Data/kp_disaggs_counts_fy23_q1.csv")

table(kp_disaggs_counts$All.Target.Populations)

kp_disaggs_counts_clean <- kp_disaggs_counts %>%
  rename(indicator = Data.Element.Short.Name, population = All.Target.Populations, otherdisaggregate = PEPFAR) %>%
  separate(indicator, c("indicator", "numdenom","keypop"), sep = "[ ]") %>%
  mutate(numdenom = recode(numdenom,
                           "(N)" = "Numerator",
                           "(D)" = "Denominator"), #if not numerator then denominator, so no null values
         population = recode(population,
                "Female Sex Workers" = "FSW",
                "Transgender People" = "TG"),
         otherdisaggregate = recode(otherdisaggregate,
                                 "NON-PEPFAR Supported Site" = "non-PEPFAR",
                                 "PEPFAR Supported Site" = "PEPFAR"))

#Age and Sex cleaning
age_sex_counts <- read.csv("Data/age_sex_counts_fy23_q1.csv")
  
age_sex_snapshot <- read.csv("Data/age_sex_snapshots_fy23_q1.csv")
  
