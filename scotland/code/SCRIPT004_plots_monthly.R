# Script that generates the plots (counts for admissions and procedures)

library(ggplot2)
# Plot admissions
setwd("~/CCU003_04/Jun2022/results")
data = read.csv("data_to_plot_adm_monthly.csv", stringsAsFactors = F)[,-1]

data = data %>%
  mutate(emergency_admission = if_else(emergency_admission == 0,"Elective","Emergency"))
# in aes, change between adm_any and adm_prim for different plots.
ggplot(data = data, mapping = aes(adm_month,adm_prim, color = factor(adm_year))) +
  geom_line(size=0.7) +
  labs(x = "Month",
       y = "Number of admissions") +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_color_discrete(name = NULL,
                       labels = c("2016-2019","2020","2021")) +
  facet_grid(emergency_admission ~ CVD, scales = "free_y" ) + 
  theme(legend.position = c(0.06,0.92),
        axis.text.x = element_text(size = 7)) +
  ggtitle("Scotland") 

# Plot procedures
setwd("~/CCU003_04/Jun2022/results")
data = read.csv("data_to_plot_proc_monthly.csv", stringsAsFactors = F)[,-1]

data = data %>%
  mutate (emergency_admission = if_else(emergency_admission == 0,"Elective","Emergency"))

ggplot(data = data, mapping = aes(adm_month,proc, color = factor(adm_year))) +
  geom_line(size=0.7) +
  labs(x = "Month",
       y = "Number of procedures") +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_color_discrete(name = NULL,
                       labels = c("2016-2019","2020","2021")) +
  facet_grid(emergency_admission ~ CVD, scales = "free_y") +
  theme(legend.position = c(0.06,0.92),
        axis.text.x = element_text(size = 7)) +
  ggtitle("Scotland")

# Plot all admissions
setwd("~/CCU003_04/Jun2022/results")
data = read.csv("data_to_plot_adm_all_monthly.csv", stringsAsFactors = F)[,-1]

# in aes, change between adm_any and adm_prim for different plots.
ggplot(data = data, mapping = aes(adm_month,adm_prim, color = factor(adm_year))) +
  geom_line(size=0.7) +
  labs(x = "Month",
       y = "Number of admissions") +
  scale_y_continuous(breaks = seq(0,2000,250),
                     labels = scales::comma) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_color_discrete(name = NULL,
                       labels = c("2016-2019","2020","2021")) +
  facet_wrap(~ CVD) + 
  theme(legend.position = c(0.06,0.92),
        axis.text.x = element_text(size = 7)) +
  ggtitle("Scotland") 

# Plot all procedures
setwd("~/CCU003_04/Jun2022/results")
data = read.csv("data_to_plot_proc_all_monthly.csv", stringsAsFactors = F)[,-1]

ggplot(data = data, mapping = aes(adm_month,proc, color = factor(adm_year))) +
  geom_line(size=0.7) +
  labs(x = "Weeks",
       y = "Number of procedures") +
  scale_y_continuous(breaks = seq(0,2000,250),
                     labels = scales::comma) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_color_discrete(name = NULL,
                       labels = c("2016-2019","2020","2021")) +
  facet_wrap(~ CVD) + 
  theme(legend.position = c(0.06,0.92),
        axis.text.x = element_text(size = 7)) +
  ggtitle("Scotland")