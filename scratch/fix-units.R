library(tidyverse)
library(hector.rcmip)

dat <- "output/your_data.csv" %>%
  read_csv()

dat2 <- dat %>%
  mutate_at(
    vars(matches("[[:digit:]]{4}")),
    function(x)
      case_when(
        dat$Variable == "Emissions|CO2" ~ ud_convert2(x, "Pg [C]", "Mt [CO2]"),
        TRUE ~ x
      )
  )

dat2 %>%
  filter(Variable == "Emissions|CO2")

write_csv(dat2, "output/your_data.csv")
