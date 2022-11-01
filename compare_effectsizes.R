library(tidyverse)
library(TOSTER)

# Load datasets ------
venier2019replicationcmj <- read_csv("venier2019replicationcmj.csv")
venier2019replicationsj <- read_csv("venier2019replicationsj.csv")

# Analyze the Countermovement Jump (CMJ) Data ------
# Data assumed in long format with y1 and y2 as the outcome variables
## Create difference score ----

venier2019replicationcmj$diff = venier2019replicationcmj$caffeine - venier2019replicationcmj$placebo

## Compute dz ----
es_rep_cmj = effectsize::cohens_d(venier2019replicationcmj$diff, paired = TRUE)

## Get original effect size for CMJ -----

# Original author provided the raw data for this study
BSDA::tsum.test(mean.x = 36.3579 - 34.7526,
                s.x = 1.06431,
                n.x = 19)
es_ori_cmj = (36.3579 - 34.7526)/1.06431

## Z-test ------
compare_smd(smd1 = es_rep_cmj$Cohens_d,
            n1 = 19,
            smd2 = es_ori_cmj,
            n2 = length(venier2019replicationcmj$participant),
            paired = TRUE)


# Analyze the Squat Jump (SJ) Data ------

## Create difference score ----

venier2019replicationsj$diff = venier2019replicationsj$caffeine - venier2019replicationsj$placebo

## Compute dz ----

es_rep_sj = effectsize::cohens_d(venier2019replicationsj$diff, paired = TRUE)

## Get original effect size for SJ-----

# Original author provided the raw data for this study
BSDA::tsum.test(mean.x = 31.9474 - 30.7947,
                s.x = 2.01394,
                n.x = 19)
es_ori_sj = (31.9474 - 30.7947)/2.01394

## Z-test
compare_smd(smd1 = es_rep_sj$Cohens_d,
            n1 = 19,
            smd2 = es_ori_sj,
            n2 = length(venier2019replicationsj$participant),
            paired = TRUE)

