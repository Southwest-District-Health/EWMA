

library(readr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(ggrepel)
library(qcc)
library(svglite)


pdraft <-  read_excel("data.xlsx")

pdraft <- pdraft %>%
  filter (  Jurisdiction == "Health District 3")

unique (pdraft$Jurisdiction)

pdraft$date <- as.Date(pdraft$date, format = "%YYY/%MM/%DD")

# Calculate daily incidence
daily_cases <- incidence2::incidence(pdraft, date_index = "date")

# Complete the sequence to include all dates with 0 counts for missing dates
daily_cases <- daily_cases %>%
  tidyr::complete(date_index = seq.Date(
    from = min(date_index, na.rm = TRUE),
    to = max(date_index, na.rm = TRUE),
    by = "day"),
    fill = list(count = 0)) %>%
  rename(I = count, date = date_index)


# Summarize daily cases into weekly incidence
pweek <- daily_cases %>%
  mutate(week= as.Date(cut(date, breaks = "week"))) %>%
  group_by(week) %>%
  summarize(case = sum(I)) %>%
  rename(date = week)


SWDHpop <- read_excel("SWDHpop.xlsx")

pweek <- pweek %>%
  mutate (year = format (date, "%Y"))

pweek <- pweek %>%
  mutate(year = as.numeric(year)) %>%  # Convert `year` in pweek to numeric
  inner_join(SWDHpop, by = "year")


pweek$case <- pweek$casen/pweek$pop*100000

# Filter the data for the year 2024
pweek <- pweek %>%
 filter(year(date) == 2024)

#Filter out latest month
pweek <- pweek %>%
  slice(-n())

pweekb <- pweek

#-----------------------------------------------------------------------------------------------------------------#
      #-----------------------------------------------------------------------------------------------------#
             #---------------------------------------------------------------------------------------#
                     #-----------------------------------------------------------------------------#

pweek <- read_excel("~/Data/pweek.xlsx")
View(pweek)



pweekb <- pweek[c (1:107), ]  # Adjust the range as needed


# Calculate the Exponentially Weighted Moving Average (EWMA) : Baseline
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- pweekb$case[1]
pweekb <- pweekb%>%
  mutate(ewma = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(pweekb)) {
  pweekb$ewma[i] <- alpha * pweekb$case[i] +
    (1 - alpha) * pweekb$ewma[i - 1]
}

baseline_chart <- qcc(pweekb$case, type = "xbar.one", title = "Control Chart")


# Extract center line and control limits from the baseline chart
baseline_center <- baseline_chart$center
baseline_lcl <- baseline_chart$limits[, "LCL"]
baseline_ucl <- baseline_chart$limits[, "UCL"]


# Calculate the Exponentially Weighted Moving Average (EWMA) 
alpha <- 0.3
# Calculate the initial value of ewma
initial_value <- pweek$case[1]
pweek <- pweek%>%
  mutate(ewma = ifelse(row_number() == 1, initial_value, NA)) # Initialize with NA
# Calculate the remaining values of ewma using the EWMA formula
for (i in 2:nrow(pweek)) {
  pweek$ewma[i] <- alpha * pweek$case[i] +
    (1 - alpha) * pweek$ewma[i - 1]
}


# Identify points above the Upper Control Limit (UCL)
pweek$out_of_control <- ifelse(pweek$ewma > baseline_ucl, TRUE, FALSE)
pweek$out_of_control1 <- ifelse(pweek$ewma > baseline_center, TRUE, FALSE)



pweek_new <- read_excel("~/Data/pweek_new.xlsx")
pweek_new$date <- as.Date(pweek_new$date)



# Create a ggplot for EWMA
plot_week <- ggplot(pweek_new, aes(x = date, y = ewma)) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = baseline_center, ymax = baseline_ucl), fill = "lightgrey", alpha = 0.3) +
  geom_point(aes(color = out_of_control1), size = 1) +
  geom_hline(yintercept = baseline_ucl, linetype = "dashed", color = "black") +
  geom_hline(yintercept = baseline_center, linetype = "dashed", color = "black") +
  geom_point(aes(x = date, y = case), shape = "X", color = "blue", size = 1.4) +
  labs(subtitle = "EWMA (Exponentially weighted moving average) Control Chart for the weekly rate of Pertussis per 100,000 
population in Southwest Idaho; Data as of August 2024",
       caption= "Blue X indicates observed cases",
       x = "",
       y = "") +
  theme_minimal() +
  scale_color_manual(values = c("black", "red"), labels = c("", ""), guide = FALSE) +
  annotate("text", x = min(pweek_new$date), y = baseline_ucl, label = "Upper Limit", vjust = -1, hjust = 0, size = 3) +
  annotate("text", x = min(pweek_new$date), y = baseline_center, label = "Baseline", vjust = -1, hjust = 0, size = 3) +
  scale_x_date(date_breaks = "month", date_labels = "%b%Y")+
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text.x = element_text(size = 7))

ggsave("output_EWMA_week.svg", plot_week, device = "svg")


# Filter the data for the year 2024
pweek_new <- pweek %>%
  filter((year(date) == 2023 & month(date) >= 5) | year(date) == 2024)


# Filter the data for the year 2024
pweek_new <- pweek %>%
  filter(year(date) >= 2017)



# Create a ggplot for EWMA with conditional coloring of points
plot_week <- ggplot(pweek_new, aes(x = date, y = ewma)) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = baseline_center, ymax = baseline_ucl), fill = "lightgrey", alpha = 0.3) +
  geom_point(aes(color = case_when(
    ewma > baseline_ucl ~ "Above Upper Limit",
    ewma > baseline_center ~ "Above Baseline",
    TRUE ~ "Below Baseline"
  )), size = 1.5) +
  geom_hline(yintercept = baseline_ucl, linetype = "dashed", color = "black") +
  geom_hline(yintercept = baseline_center, linetype = "dashed", color = "black") +
  #geom_point(aes(x = date, y = case), shape = "X", color = "blue", size = 1.4) +
  annotate("text", x = min(pweek_new$date), y = baseline_ucl, label = "Upper Limit", vjust = -0.5, hjust = 0, size = 2) +
  annotate("text", x = min(pweek_new$date), y = baseline_center, label = "Baseline", vjust = -0.5, hjust = 0, size = 2) +
  labs(subtitle = "EWMA (Exponentially weighted moving average) Control Chart for the weekly cases of Pertussis 
       in SWDH; Data as of 10/21/2024",
       caption= "Blue X indicates observed incidence rates",
       x = "",
       y = "") +
  theme_minimal() +
  scale_color_manual(values = c("Below Baseline" = "black", "Above Baseline" = "orange", "Above Upper Limit" = "red")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b%Y") +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text.x = element_text(size = 7, angle= 45, hjust = 1),
    legend.position = "none",  # Hides the legend
    panel.grid.major.x = element_line(color = "grey", size = 0.5),  # Show only major grid lines for the x-axis
    panel.grid.minor.x = element_blank(),  # Remove minor grid lines for the x-axis
    panel.grid.major.y = element_blank(),  # Optionally remove y-axis grid lines if not needed
    panel.grid.minor.y = element_blank()   # Remove minor y-axis grid lines
  )

ggsave("output_EWMA_week.svg", plot_week, device = "svg")



