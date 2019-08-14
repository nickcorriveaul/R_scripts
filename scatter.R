read.table("cimaq_dataset.csv")
data = read.table("cimaq_dataset.csv", sep = ",", header = TRUE)
data = read.table("cimaq_dataset.csv", sep = ",", h = T)

library(zoo)
library(ggplot2)

#cleanup

cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "black"))
#Scatterplot

scatter = ggplot(data, aes(MoCA, CS_Parietal_Sup_L_winsored, color = Group))
scatter +
  geom_point() +
  theme(axis.text=element_text(size=12, face ="bold"), axis.title=element_text(size=12, face="bold")) +
  geom_smooth(method = "lm") +
  xlab("MoCA (/30)")+
  ylab("Left superior parietal activation (Z scores)") +
  xlim(22, 30) +
  ylim(-1.8, 2.4) +
  cleanup

scatter = ggplot(data, aes(CSF_Tau_Winsorized_Z, CS_Hippocampus_L))
scatter +
  geom_point() +
  theme(axis.text=element_text(size=12, face ="bold"), axis.title=element_text(size=12, face="bold")) +
  geom_smooth(method = "lm") +
  xlab("CSF t-tau")+
  ylab("Left hippocampal activation (Z scores)") +
  xlim(-1.8, 2.4) +
  ylim(-2.7,2.4) +
  cleanup

scatter = ggplot(data, aes(CSF_Tau_Winsorized, CS_Hippocampus_L))
scatter +
  geom_point() +
  theme(axis.text=element_text(size=12, face ="bold"), axis.title=element_text(size=12, face="bold")) +
  geom_smooth(method = "lm") +
  xlab("CSF t-tau")+
  ylab("Left hippocampal activation (Z scores)") +
  xlim(66, 676) +
  ylim(-2.7,2.4) +
  cleanup