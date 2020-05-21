library(tidyverse)
library(ggbeeswarm)

#### Coral survival -------------------------

# Read in data for growth and survival analyses
Staghorn_growth_survival <- read_csv("~/Google Drive/Stier Lab/People/Kai Kopecky/Projects/Acropora pulchra density dependence/Code/staghorn_density_dependence/2019-04-03__growth-consumption_data-for-two-way-anova.csv")

# Convert density to a factor with properly ordered levels
Staghorn_growth_survival$density <- as.factor(Staghorn_growth_survival$density)

Staghorn_growth_survival$density <- ordered(Staghorn_growth_survival$density, levels = c("Low", "Mid", "High"))

# Rename column for corallivore exposure variable
Staghorn_growth_survival <- Staghorn_growth_survival %>% 
  rename(Corallivores = exposure)

# Rename values within corallivore exposure variable
Staghorn_growth_survival <- Staghorn_growth_survival %>%  
  mutate(Corallivores = case_when(Corallivores == "caged" ~ "Absent",
                                  Corallivores == "exposed" ~ "Present"))

# Two-way ANOVA of survival by density treatment and predator exposure
two_way_anova <- aov(survivorship ~ density + Corallivores, data = Staghorn_growth_survival)
summary(two_way_anova)

# Create summary table of average coral survival with standard error by density and corallivore exposure
survival_summary <- Staghorn_growth_survival %>% 
  group_by(density, Corallivores) %>% 
  summarize(mean_survival = mean(survivorship),
            sd_survival = sd(survivorship),
            se_survival = sd_survival/sqrt(n()))

# Create plot displaying means (+/- standard error for corallivores present only) for coral survival 
survival_plot <- ggplot() +
  geom_line(data = survival_summary, 
            aes(x = density, y = mean_survival, group = Corallivores)) +
  geom_point(data = survival_summary,
             aes(x = density, y = mean_survival, shape = Corallivores), 
             size = 3) +
  geom_errorbar(data = survival_summary %>% 
                  filter(Corallivores == "Present"),
                aes(x = density,
                    ymin = mean_survival - se_survival,
                    ymax = mean_survival + se_survival),
                width = 0) +
  labs(x = "Density treatment",
       y = "Proportional coral survival") +
  scale_y_continuous(limits = c(0, 1.02),
                     expand = c(0,0)) +
  theme_classic()

survival_plot

#### Vertical height ------------------------------------------------------

# Read in data for vertical height analyses
growth_ratio_stats <- read_csv("~/Documents/Moorea_2018/Coral density dependence experiment/Growth_Stats/Growth_ratio_stats.csv")

# Planned contrasts test of proportional changes in vertical height
model_vh <- aov(vertical_height_change_prop~density, data = growth_ratio_stats)

summary(model_vh)

growth_ratio_stats$density <- as.factor(growth_ratio_stats$density)

contrasts(growth_ratio_stats$density)

summary.lm(model_vh)

growth_ratio_stats$density <- ordered(growth_ratio_stats$density, levels = c("Low", "Mid", "High"))

contrastmatrix <- cbind(c(-1,1,0),c(0,1,-1))
View(contrastmatrix)
contrasts(growth_ratio_stats$density) <- contrastmatrix
summary.lm(aov(vertical_height_change_prop~density, data = growth_ratio_stats))

contrastmatrix_combined <- cbind(c(-1,2,-1),c(-1,0,1))

contrasts(growth_ratio_stats$density) <- contrastmatrix_combined 
str(growth_ratio_stats$density)
height_model <- aov(vertical_height_change_prop~density, data = growth_ratio_stats)
summary.lm(height_model)


# Visualization for change in vertical height for corals protected from corallivores 
Staghorn_growth_survival$bommie <- as.factor(Staghorn_growth_survival$bommie)

vh_summary <- Staghorn_growth_survival %>% 
  filter(Corallivores == "Absent") %>% 
  group_by(density) %>% 
  summarize(vh_avg = mean(vertical_height_change_prop),
            vh_sd = sd(vertical_height_change_prop),
            vh_se = vh_sd/sqrt(n()))

vertical_height_plot <- 

ggplot()+
  geom_point(data = vh_summary, 
             aes(x = density, y = vh_avg),
             size = 3)+
  geom_errorbar(data = vh_summary, 
                aes(x = density,
                ymin = vh_avg - vh_se, 
                ymax = vh_avg + vh_se), 
                width = 0) +
  geom_jitter(data = Staghorn_growth_survival %>% 
                filter(Corallivores == "Absent"),
              aes(x = density, y = vertical_height_change_prop),
              height = 0,
              width = 0.1,
              alpha = 0.4) +
  scale_y_continuous(limits = c(0, max(Staghorn_growth_survival$vertical_height_change_prop) + 0.01),
                     expand = c(0,0),
                     breaks = seq(from = 0, to = 0.4, by = 0.05)) +
  labs(x = "Density treatment",
       y = "Proportional coral growth") +
  theme_classic()
