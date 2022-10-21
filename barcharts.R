library(tidyverse)

# Load dataset ----------------------------------------------------------------------------------------------------------
venier2019replicationsj <- read_csv("venier2019replicationsj.csv")
venier2019replicationcmj <- read_csv("venier2019replicationcmj.csv")

# Bar plot for squat jump ---------------------------------------------------------------------------------------------------

sj_plot <- venier2019replicationsj %>%
  gather(key = "supplement", value = "squatjump", caffeine, placebo)
head(sj_plot, 3)


ggplot(sj_plot, aes(x = supplement, y = squatjump, fill = supplement)) + 
  stat_summary(fun= "mean", geom = "col", width = 0.7) + 
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = .1, fun.args = list(mult = 1)) +
  scale_y_continuous(name = "Jump height (cm)") +
  scale_x_discrete(name = "Squat Jump", 
                   labels = c("Caffeine", "Placebo")) +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 0.5, size=12, face="plain"),
        axis.title.x = element_text(hjust = 0.5, size=16, face="bold"),
        axis.title.y = element_text(vjust = 2, size=12, face="plain"),
        axis.text.y = element_text(size=10, face="bold"),
        panel.border = element_rect(color = "grey", size = 2),
        legend.position="none") +
  scale_fill_viridis_d(option = "E")

ggsave("squatbarchart.png",
       plot = last_plot(),
       device = "png",
       width = NA,
       height = NA,
       dpi = 300,
       limitsize = TRUE
)

# Bar plot for countermovement jump ---------------------------------------------------------------------------------------------------

cmj_plot <- venier2019replicationcmj %>%
  gather(key = "supplement", value = "cmj", caffeine, placebo)
head(cmj_plot, 3)

ggplot(cmj_plot, aes(x = supplement, y = cmj, fill = supplement)) + 
  stat_summary(fun= "mean", geom = "col", width = 0.7) + 
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = .1, fun.args = list(mult = 1)) +
  scale_y_continuous(name = "Jump height (cm)") +
  scale_x_discrete(name = "Countermovement Jump", 
                   labels = c("Caffeine", "Placebo")) +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 0.5, size=12, face="plain"),
        axis.title.x = element_text(hjust = 0.5, size=16, face="bold"),
        axis.title.y = element_text(vjust = 2, size=12, face="plain"),
        axis.text.y = element_text(size=10, face="bold"),
        panel.border = element_rect(color = "grey", size = 2),
        legend.position="none") +
  scale_fill_viridis_d(option = "E")

ggsave("cmjbarchart.png",
       plot = last_plot(),
       device = "png",
       width = NA,
       height = NA,
       dpi = 300,
       limitsize = TRUE
)
