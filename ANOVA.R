#ANOVA

read.table("cimaq_perfo_behav.csv")
data = read.table("cimaq_perfo_behav.csv", sep = ",", header = TRUE)
data = read.table("cimaq_perfo_behav.csv", sep = ",", h = T)

install.packages("afex")
install.packages("lsmeans")

library(afex); library(lsmeans); library(ggpubr); library(ggplot2); library(survminer); library(zoo)

aov_ez(id="ID", dv="AM_Perfo", between="Group", data=data)
AM_Perfo.aov = aov_ez(id="ID", dv="AM_Perfo", between="Group", data=data)
AM_Perfo.aov$data

aov_ez(id="ID", dv="IM_Perfo", between="Group", data=data)
IM_Perfo.aov = aov_ez(id="ID", dv="IM_Perfo", between="Group", data=data)
IM_Perfo.aov$data

lsmeans(AM_Perfo.aov, "Group", contr="pairwise")
lsmeans(IM_Perfo.aov, "Group", contr="pairwise")

ggboxplot(data, x = "Group", y = "IM_Perfo", ylim = c(-3, 3), color = "Group",
          add = "jitter", legend = "none", order = c("Controls", "SCD", "SCD+", "MCI"),
          ylab = "Item memory performance d' (Z scores)") +
  geom_hline(yintercept = mean(data$IM_Perfo),linetype = 2)+ #Add horizontal line at base mean+        # Add global annova p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.", hide.ns = TRUE)+
  font("xlab", size = 12, color = "black", face = "bold")+
  font("ylab", size = 12, color = "black", face = "bold")+
  font("xy.text", color = "black", size = 12, face="bold")+
  rremove("xlab")

  #END