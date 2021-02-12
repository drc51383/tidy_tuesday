# Tidy Tuesday Project
# Wealth and income over time

# Created by David Christafore
# Created on 02/12/2021

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2021-02-09')
student_debt <- tuesdata$student_debt

ggplot(student_debt, aes(year, loan_debt, color = race)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1989, 2016, 3)) +
  scale_y_continuous(breaks = seq(0, 15000, 2500), labels = scales::comma) +
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + 
  labs(x = "",
       y = "Loan Debt (2016 dollars)",
       title = "Student Loan Debt by Race/Ethnicity",
       subtitle = "Average family student loan debt for aged 25-55",
       color = "",
       caption = "Data source: Urban Institute \n Created by: David Christafore"
  )
