read.table("cimaq_dataset.csv")
data = read.table("cimaq_dataset.csv", sep = ",", header = TRUE)
data = read.table("cimaq_dataset.csv", sep = ",", h = T)

library(ggplot2); library(ISLR); library(tidyr)

cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "black"))

ggplot(data, aes(x = Mean_thickness,y = CS_Parietal_Sup_L_winsored, color = Group)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  theme(axis.text=element_text(size=15, face ="bold"), axis.title=element_text(size=15, face="bold")) +
  ylim(-2.5,2.5) +
  xlim(-1.7, 2.0) +
  xlab("Mean cortical thickness' (Z scores)") +
  ylab("Left superior parietal activation (Z scores)") +
  cleanup

ggplot(data, aes(x = mean_composite,y = WS_Temporal_Inf_R, color = Diagnostic_2)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  stat_smooth(method = "lm") +
  theme(axis.text=element_text(size=15, face ="bold"), axis.title=element_text(size=15, face="bold")) +
  xlab("Cortical thickness") +
  ylab("Right inferior temporal activation") +
  cleanup

scatter = ggplot(data, aes(d_Item_Memory, WS_Temporal_Inf_R, color = Diagnostic_2))
scatter +
  geom_point() +
  theme(axis.text=element_text(size=12, face ="bold"), axis.title=element_text(size=12, face="bold")) +
  geom_smooth(method = "lm") +
  xlab("Item Memory performance") +
  ylab("Right inferior temporal activation") +
  cleanup

ggplot(data, aes(x = rh_composite,y = CS_Parietal_Sup_L_winsored, color = Group)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  theme(axis.text=element_text(size=15, face ="bold"), axis.title=element_text(size=15, face="bold")) +
  ylim(-2.01,2.00) +
  xlim(-1.2, 2.04) +
  xlab("Right hemisphere thickness (Z scores)") +
  ylab("Left superior parietal activation (Z scores)") +
  cleanup