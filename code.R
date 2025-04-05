# Install/Load packages ------------------------------------
packages = c("adespatial", "tidyverse", "vegan", "readxl", "hrbrthemes",
             "viridis", "ggbeeswarm", "ggthemes", "iNEXT",
             "spaa", "cowplot", "FactoMineR", "factoextra",
             "writexl", "fields", "reshape2", "ade4", "readr")
lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    } 
    library(x, character.only = TRUE)
  }
)

# Getting data ready ----------------------
data = read_excel("ditches.xlsx")
categorias = data[,c(1:4)]
road = data[,c(9:11)]
env = data[,c(12:18)]
community = data[,c(19:35)]

data$maximum_length_m<- as.numeric(data$maximum_length_m)
data$maximum_width_m <- as.numeric(data$maximum_width_m)
data$maximum_depth_cm <- as.numeric(data$maximum_depth_cm)
data$depth_m <- data$maximum_depth_cm / 100

V = (2/3) * pi * data$maximum_length_m * data$maximum_width_m  * data$depth_m
env$Volume <- V
road$`Road distance` = as.numeric(road$`Road distance`)
env$Stream_dist= as.numeric(env$Stream_dist)
env$pH = as.numeric(env$pH)
env$DO = as.numeric(env$DO)
env$Temp = as.numeric(env$Temp)
env$Cond = as.numeric(env$Cond)
env$TS = as.numeric(env$TS)

filtered_itanhaem <- read_csv("filtered_itanhaem.csv")
df_pluviosidade = filtered_itanhaem[-1,]

# Figure 2 ---------------------------------------
df_pluviosidade$Month <- as.factor(df_pluviosidade$Month)
df_pluviosidade$Month <- factor(df_pluviosidade$Month, levels = c( "January",
                                         "February", "March", "April", "May", "June",
                                         "July", "August", "September", "October", 
                                         "November", "December"))
df_summary_pluviosidade <- df_pluviosidade %>%
  group_by(Month) %>%
  summarise(
    mean_value = mean(Balneario_Gaivota, na.rm = TRUE),
    sd_value = sd(Balneario_Gaivota, na.rm = TRUE)
  ) %>%
  mutate(
    Season = ifelse(Month %in% c("November", "December", "January", "February", "March"), "WP", "DP"),
    Month = factor(Month, levels = c( "January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"))
  )


fig_2 = ggplot(df_summary_pluviosidade, aes(x = Month, y = mean_value, fill = Season)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = mean_value, ymax = mean_value + sd_value),
                width = 0.2, color = "black")+
  theme_tufte(base_size = 20)+
  scale_fill_manual(values = c("WP" = "#1f78b4", 
                               "DP" = "#33a02c"))+
  theme(
    axis.line = element_line(color = "grey50", linewidth = 0.5),
    legend.position = c(0.93, 0.88),  
    legend.background = element_blank(), 
    legend.box.background = element_blank(),
    legend.text = element_text(size = 24)
  )+
  labs(fill = NULL, x = "Months", y = "Mean annual rainfall per month (mm)")+
  scale_y_continuous(expand = c(0,0), limits = c(0,500))

ggsave("Figure 2.jpg", fig_2, width = 14, height = 9)

# Table 1 -------------------------
env_df = env
env_df = as.data.frame(env_df[,-9])
env_df = as.data.frame(env_df[,-2])
env_df$length_m = data$maximum_length_m
env_df$width_m = data$maximum_width_m
env_df$depth_m = data$depth_m


env_df$rain = data$rain

summary_env <- env_df %>%
  group_by(rain) %>%  
  summarise(across(everything(), 
                   list(Min = ~min(.x, na.rm = TRUE),
                        Max = ~max(.x, na.rm = TRUE),
                        Mean = ~mean(.x, na.rm = TRUE),
                        SD = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

write_xlsx(summary_env, "Environmental_Variables_Summary.xlsx")

# PCA & Figure S1 ---------------------
pca.p <- PCA(X = env_df[,c(1:7)],
             scale.unit = TRUE, graph = FALSE)

fviz_screeplot(pca.p, addlabels = TRUE, ylim = c(0, 70), main = "", 
               xlab = "Dimensões",
               ylab = "Porcentagem de variância explicada") 
var_env <- get_pca_var(pca.p)
summary(pca.p)
# 38.6 , 22.8% 



fig_3 = fviz_pca_biplot(pca.p,
                           geom.ind = "point", 
                           fill.ind = env_df$rain, 
                           col.ind = "black",
                           alpha.ind = 0.7,
                           pointshape = 21,
                           pointsize = 7,
                           palette = c("#33a02c","#1f78b4"), 
                           col.var = "black",
                           invisible = "quali",
                           title = NULL) +
  labs(x = "PC1 (38.6%)", y = "PC2 (22.8%)", fill = NULL)+
  theme_tufte(base_size = 18)+
  theme(
    axis.line = element_line(color = "grey50", linewidth = 0.5)
  )+
  xlim(c(-3.5, 4)) +
  ylim(c(-2, 4.5)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 8.5),         
    legend.key.size = unit(0.4, "cm"),               
    legend.spacing = unit(0.15, "cm"),              
    legend.justification = c("left", "top"),  
    legend.position = c(0.05, 0.95),          
    legend.box.just = "left",
    legend.margin = margin(2.5, 2.5, 2.5, 2.5),
    legend.background = element_blank(),      
    legend.key = element_blank()             
  )

fig_3

ggsave("Figure 3.jpg", fig_3)

# Table 2 -------------------
abundancia_total <- data %>%
  group_by(rain) %>%
  summarise(across(18:34, sum))
rowSums(abundancia_total[,2:18])

# iNEXT & Figure 5 ------------------
datlist <- list()
com_inext <- community

com_inext_wp <- decostand(com_inext[c(1:9, 31:36),],
                          method = "pa")
com_inext_dp <- decostand(com_inext[c(10:30),],
                          method = "pa")

datlist$WP <-data.frame(t(com_inext_wp))
datlist$DP <-data.frame(t(com_inext_dp))

datlist

result <- iNEXT(datlist,
                q = 0,
                datatype = "incidence_raw",
                endpoint = 40,
                se = TRUE, 
                nboot = 999)

fig_5 = ggiNEXT(result, type = 1)+
  scale_colour_manual(values = c("WP" = "#1f78b4", 
                                 "DP" = "#33a02c")) +
  scale_fill_manual(values = c("WP" = "#1f78b4", 
                               "DP" = "#33a02c"),
                    guide = guide_legend(reverse = TRUE))+
  theme_tufte(base_size = 15)+
  labs(
    y = "Species richness") +
  theme(
    axis.line = element_line(color = "grey50", linewidth = 0.5),
    legend.position = c(0.2, 0.8),  
    legend.background = element_blank(), 
    legend.box.background = element_blank()
  ) 

fig_5

ggsave("Figure 5.jpg", fig_5)

# PERMANOVA & PERMDISP -------------
data$rain <- as.factor(data$rain)
levels(data$rain)

distancia <- vegdist(community, method = "bray")

resultado_permanova <- adonis2(distancia ~ data$rain, permutations = 999)
print(resultado_permanova)

dispersao <- betadisper(distancia, data$rain)
anova(dispersao)  

# NMDS & Figure 6 ------------------
nmds_result <- metaMDS(community, distance = "bray", k = 2, trymax = 100)
nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_scores$Periodo <- data$rain

stress_value <- round(nmds_result$stress, 3)

nmds_plot = ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, fill = Periodo)) +
  stat_ellipse(type = "norm", geom = "polygon", alpha = 0.3,
               color = "black")+
  geom_point(size =6, shape = 21, alpha = 0.8,
             color = "black") +  
  labs(fill = NULL, color = NULL,
       x = "NMDS Axis 1", y = "NMDS Axis 2") +
  theme_tufte(base_size = 15)+
  theme(
    axis.line = element_line(colour = "grey50", linewidth = 0.5),
    legend.position = c(0.99, 0.99),  
    legend.justification = c("right", "top"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.background = element_rect(fill = "white", color = "grey70", size = 0.2),
    legend.box.background = element_rect(color = "grey70")
  )+
  annotate("text", 
           x = Inf, y = -Inf,  # Posiciona no canto inferior direito
           label = paste0("Stress = ", stress_value), 
           hjust = 1.1, vjust = -1.1,  # Ajusta o alinhamento
           size = 4, fontface = "italic")+
  scale_fill_manual(values = c("Wet Period" = "#1f78b4", 
                               "Drier Period" = "#33a02c"),
                    guide = guide_legend(reverse = TRUE))+
  scale_x_continuous(limits = c(-3, 4), breaks = seq(-2, 4, by = 2))+
  scale_y_continuous(limits = c(-3, 2.5), breaks = seq(-2, 2, by = 1))

nmds_plot

species_scores <- as.data.frame(scores(nmds_result, display = "species"))
species_scores$Especie <- rownames(species_scores)  # Adiciona os nomes das espécies

nmds_sp = ggplot(species_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(color = "black", size = 4, shape = 21,
             alpha = 0.8, fill = "black")+
  ggrepel::geom_text_repel(aes(label = paste0("italic('", Especie, "')")),
                           parse = TRUE,  
                           color = "black", size = 3)+
  labs(x = "NMDS Axis 1", y = "NMDS Axis 2") +
  theme_tufte(base_size = 15)+
  theme(
    axis.line = element_line(color = "grey50", linewidth = 0.5)
  )+
  scale_x_continuous(limits = c(-3, 4), breaks = seq(-2, 4, by = 2))+
  scale_y_continuous(limits = c(-2.5, 2.5), breaks = seq(-2, 2, by = 1))

nmds_sp

nmds_complete = plot_grid(nmds_plot, nmds_sp, nrow = 1, labels = "AUTO")
nmds_complete

ggsave("Figure 6.jpg", nmds_complete, width = 12)

# RDA & Figure 7 -----------------
abundancia_hellinger <- decostand(community, method = "hellinger")

env$Highway_dist = data$`highway_distance (m)`

rda_modelo <- rda(abundancia_hellinger ~ Stream_dist +
                    pH + DO + Temp +Volume, data = env)
anova(rda_modelo, by = "terms", permutations = 999)


vifvalores <- vif.cca(rda_modelo)
print(vifvalores)

summary(rda_modelo)

eigenvalues <- eigenvals(rda_modelo)
variancia_explicada <- eigenvalues / sum(eigenvalues) * 100
print(variancia_explicada)

plot(rda_modelo, scaling = 2)

especies_scores <- as.data.frame(scores(rda_modelo, display = "species"))
variaveis_scores <- as.data.frame(scores(rda_modelo, display = "bp"))

especies_scores$Especie <- rownames(especies_scores)
variaveis_scores$Variavel <- rownames(variaveis_scores)

sc = 3
rda_sc = scores(rda_modelo, scaling = sc)
biplot <- ggplot() +
  geom_segment(data = data.frame(rda_sc$biplot),
               mapping = aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(4, "mm")),
               color = 'brown4') +
  geom_text(data = data.frame(rda_sc$biplot),
            aes(x = RDA1 * 1.05, y = RDA2 * 1.05,
                label = rownames(data.frame(rda_sc$biplot))),
            color = "brown4") +
  labs(x = 'RDA 1', y = 'RDA 2') +
  theme_tufte(base_size = 15)+
  theme(
    axis.line = element_line(color = "grey50", linewidth = 0.5)
  )

biplot

rda_fig = biplot +
  geom_point(data = especies_scores,
             mapping = aes(x = RDA1, y = RDA2),
             shape = 22, alpha = 0.4, size = 2,
             fill = "black") +
  ggrepel::geom_text_repel(data = especies_scores, 
                           aes(x = RDA1, y = RDA2, 
                               label = paste0("italic('", Especie, "')")),
                           parse = TRUE, 
                           color = "black", size = 3, alpha = 0.7)+
  labs(x = "RDA1 (14.44%)", y = "RDA2 (5.31%)")+
  scale_x_continuous(limits = c(-0.55, 0.6))

rda_fig

ggsave("Figure 7.jpg", rda_fig)

# Mantel & Figure 8 -------------------
spatial = read_excel("spatial.xlsx")
spatial <- spatial[, c("Lat", "Long")]
spatial <- as.matrix(spatial)
mode(spatial) <- "numeric"
spatial[, "Lat"] <- jitter(spatial[, "Lat"], amount = 0.0001)
spatial[, "Long"] <- jitter(spatial[, "Long"], amount = 0.0001)
Dist.km <- as.dist(rdist.earth(spatial, miles = F))
comp.pad <- decostand(community, "hellinger")
env.pad <- decostand(env, "standardize")
dissimil.com <- vegdist(comp.pad, "bray")

mantel(Dist.km, dissimil.com) 

matrix.dist <- data.frame(x = melt(as.matrix(Dist.km))$value, 
                          y = melt(as.matrix(dissimil.com))$value)

fig_8 = ggplot(matrix.dist , aes(x, y)) +
  geom_point(size = 6, shape = 21, fill = "black", alpha = 0.4) +
  labs(x = "Geographical distance (km)", 
       y = "Dissimilarity (Bray-Curtis)") +
  theme_tufte(base_size = 15)+
  theme(
    axis.line = element_line(color = "grey50", linewidth = 0.5),
    legend.position = c(0.2, 0.8),  
    legend.background = element_blank(), 
    legend.box.background = element_blank()
  )

fig_8

ggsave("Figure 8.jpg", fig_8)
