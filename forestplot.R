library(tidyverse)

venier2019replicationsj <- read_csv("venier2019replicationsj.csv")
venier2019replicationcmj <- read_csv("venier2019replicationcmj.csv")
venierdata <- read_csv("venierdata.csv")

# Forest plot for CMJ data ---------
## Calculate replication study ES for cmj -------------
venier2019replicationcmj$diff = venier2019replicationcmj$caffeine - venier2019replicationcmj$placebo

es_rep_cmj = effectsize::cohens_d(venier2019replicationcmj$diff, paired = TRUE)

es_rep_cmj <-
  es_rep_cmj %>% mutate(study_id = c("Replication study")) # add identifier

## Calculate original study ES for cmj -------------
venierdata$diff = venierdata$cmjcaff - venierdata$cmjpla

es_ori_cmj = effectsize::cohens_d(venierdata$diff, paired = TRUE)

es_ori_cmj <-
  es_ori_cmj %>% mutate(study_id = c("Original study")) # add identifier

## Labels for cmj forest plot -------------
labelcmjrep <- "0.31 [-0.02, 0.64]"
labelcmjorig <- "1.51 [0.86, 2.22]"

## Join datasets -----------------
ori_cmj <- as.data.frame(es_ori_cmj)
rep_cmj <- as.data.frame(es_rep_cmj)

cmjplot <-
  merge(ori_cmj, rep_cmj, by = c("Cohens_d", "CI", "CI_low", "CI_high", "study_id"), all = TRUE)

## Plot -----------------------------
ggplot(cmjplot,
       aes(
         y = study_id,
         x = Cohens_d,
         xmin = CI_low,
         xmax = CI_high
       )) +
  ggtitle("Cohen's dz [95%CI]") +
  geom_point() +
  geom_errorbarh(height = .1) +
  geom_vline(
    xintercept = 0,
    color = 'black',
    linetype = 'dashed',
    alpha = .4
  ) +
  theme_minimal() +
  scale_x_continuous(name = "Observed Effect Size", limits = c(-1, 3.2)) +
  scale_y_discrete(name = "") +
  annotate("text",
           x = 2.8,
           y = 2,
           label = labelcmjrep) +
  annotate("text",
           x = 2.8,
           y = 1,
           label = labelcmjorig) +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.94),
    panel.background = element_blank()
  )

ggsave(
  "forestcmj.png",
  plot = last_plot(),
  device = "png",
  width = NA,
  height = NA,
  dpi = 300,
  limitsize = TRUE,
  bg = '#ffffff'
)

# Forest plot for squat jump data ---------
## Calculate replication study ES for sj -------------
venier2019replicationsj$diff = venier2019replicationsj$caffeine - venier2019replicationsj$placebo

es_rep_sj = effectsize::cohens_d(venier2019replicationsj$diff, paired = TRUE)

es_rep_sj <-
  es_rep_sj %>% mutate(study_id = c("Replication study"))

## Calculate original study ES for sj -------------
venierdata$diff = venierdata$sjcaff - venierdata$sjpla

es_ori_sj = effectsize::cohens_d(venierdata$diff, paired = TRUE)

es_ori_sj <-
  es_ori_sj %>% mutate(study_id = c("Original study"))

## Labels for sj forest plot -------------
labelsjrep <- "0.28 [-0.05, 0.61]"
labelsjorig <- "0.57 [0.08, 1.08]"

## Join datasets -----------------
ori_sj <- as.data.frame(es_ori_sj)
rep_sj <- as.data.frame(es_rep_sj)

sjplot <-
  merge(ori_sj, rep_sj, by = c("Cohens_d", "CI", "CI_low", "CI_high", "study_id"), all = TRUE)

## Plot -----------------------------
ggplot(sjplot,
       aes(
         y = study_id,
         x = Cohens_d,
         xmin = CI_low,
         xmax = CI_high
       )) +
  ggtitle("Cohen's dz [95%CI]") +
  geom_point() +
  geom_errorbarh(height = .1) +
  geom_vline(
    xintercept = 0,
    color = 'black',
    linetype = 'dashed',
    alpha = .4
  ) +
  theme_minimal() +
  scale_x_continuous(name = "Observed Effect Size", limits = c(-1, 3.2)) +
  scale_y_discrete(name = "") +
  annotate("text",
           x = 2.8,
           y = 2,
           label = labelsjrep) +
  annotate("text",
           x = 2.8,
           y = 1,
           label = labelsjorig) +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.94),
    panel.background = element_blank()
  )

ggsave(
  "forestsquat.png",
  plot = last_plot(),
  device = "png",
  width = NA,
  height = NA,
  dpi = 300,
  limitsize = TRUE,
  bg = '#ffffff'
)
