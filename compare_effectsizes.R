library(tidyverse)

# Load datasets ------
venier2019replicationcmj <- read_csv("venier2019replicationcmj.csv")
venier2019replicationsj <- read_csv("venier2019replicationsj.csv")

# Functions ----
## p to z -----
p_from_z = function(x){
  2*pnorm(-abs(unlist(x)))
}

## compare paired sample studies (cohen's dz)
compare_dz = function(d_ori,
                      n_ori,
                      d_rep,
                      n_rep) {
  se_ori = sqrt( 1/n_ori + (d_ori^2/(2*n_ori)) )
  se_rep = sqrt( 1/n_rep + (d_rep^2/(2*n_rep)) )
  se_diff = sqrt(se_ori^2 + se_rep^2)
  z = (d_ori - d_rep)/se_diff
  names(z) = "z"
  pval = p_from_z(z)
  alt_meth = "two-sided"
  null = 0
  par = c(n_ori,n_rep)
  names(par) = c("N original", "N replication")
  rval <- list(statistic = z, parameter = par, p.value = pval,
               #conf.int = cint, 
               estimate = d_ori-d_rep, 
               null.value = 0,
               alternative = "two.sided",
               method = "Difference in Cohen's dz", 
               data.name = "Summary Statistics")
  class(rval) <- "htest"
  return(rval)
}

# Analyze the Countermovement Jump (CMJ) Data ------
# Data assumed in long format with y1 and y2 as the outcome variables

venier2019replicationcmj$diff = venier2019replicationcmj$caffeine - venier2019replicationcmj$placebo
t_test_res = t.test(venier2019replicationcmj$diff)

es_rep_cmj = effectsize::cohens_d(venier2019replicationcmj$diff, paired = TRUE)


# Get original effect size for CMJ -----

# Original author provided the raw data for this study
BSDA::tsum.test(mean.x = 36.3579 - 34.7526,
                s.x = 1.06431,
                n.x = 19)
es_ori_cmj = (36.3579 - 34.7526)/1.06431


compare_dz(d_ori = es_ori_cmj,
           n_ori = 19,
           d_rep = es_rep_cmj$Cohens_d,
           n_rep = length(venier2019replicationcmj$participant))


# Analyze the Squat Jump (SJ) Data ------
# Data assumed in long format with y1 and y2 as the outcome variables

venier2019replicationsj$diff = venier2019replicationsj$caffeine - venier2019replicationsj$placebo

t_test_res_sj = t.test(venier2019replicationsj$diff)

es_rep_sj = effectsize::cohens_d(venier2019replicationsj$diff, paired = TRUE)

# Get original effect size for SJ-----

# Original author provided the raw data for this study
BSDA::tsum.test(mean.x = 31.9474 - 30.7947,
                s.x = 2.01394,
                n.x = 19)
es_ori_sj = (31.9474 - 30.7947)/2.01394


compare_dz(d_ori = es_ori_sj,
           n_ori = 19,
           d_rep = es_rep_sj$Cohens_d,
           n_rep = length(venier2019replicationcmj$participant))

