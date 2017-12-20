#Pectus-cluster
#The optimal number of clusters
#See the Pectus_cluster data dictionary for variable definitions

#________________________________________________________________________________packages, libraries
pacman::p_load(Hmisc, NbClust, coin, ggplot2, gridExtra)

#_________________________________________________________________________________import table

raw_data <- read.csv("C:/Users/csuehs/Documents/00-Projects_Montpellier/PECTUS-Cluster/R_pectus_cluster/pectus_cluster_data.csv")

#________________________________________________________________________________replace missing values by column means

imputed_data <- raw_data

for(i in 1:ncol(imputed_data)){
    imputed_data[is.na(imputed_data[,i]), i] <- mean(imputed_data[,i], na.rm = TRUE)
}

#________________________________________________________________________________select variables for clustering and scale them

#The following variables were chosen for clustering: age, sex, BMI, 
#psychosocial impact, dyspnea, palpitations, the Haller index and FVC. 


clust_me <- data.frame(imputed_data$age,
                       imputed_data$sex,
                       imputed_data$bmi,
                       imputed_data$psychosocial,
                       imputed_data$dyspnea,
                       imputed_data$palpit,
                       imputed_data$haller,
                       imputed_data$fvc)

colnames(clust_me) <- c("age",
                        "sex",
                        "bmi",
                        "psychosocial",
                        "dyspnea",
                        "palpit",
                        "haller",
                        "fvc")

clust_me <- scale(clust_me)

#_________________________________________________________________________________create a distance matrix

dist.res <- dist(clust_me, method = "euclidean")

#_________________________________________________________________________________create first dendrogram: ascending algorithm


#helpful website: http://www.sthda.com/english/wiki/determining-the-optimal-number-of-clusters-3-must-known-methods-unsupervised-machine-learning
#helpful website: http://larmarange.github.io/analyse-R/classification-ascendante-hierarchique.html        
#La fonction de base pour le calcul d'un dendrogramme est hclust en précisant le 
#critère d'aggrégation avec method. Dans notre cas, nous allons opter pour la 
#méthode de Ward appliquée au carré des distances (ce qu'on indique avec method 
#= "ward.D2"4, l'option method = "ward.D" correspondant à la version 
#« classique ») :


arbre <- hclust(dist.res, method = "ward.D2")
inertie <- sort(arbre$height, decreasing = TRUE)

#________________________________________________________________________________create graphic for cluster number decision

pdf(file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/figures/figure_2_optimal_cluster_number.pdf",
    width = 8,
    height = 4)

layout(matrix(c(1,2,2), nrow = 1, ncol = 3))

plot(inertie[1:10], type = "s", xlab = "Number of clusters", 
     ylab = "Inertia", lwd = 2)
abline(v = 2, lty=5, col = "gray69")
minor.tick(nx=2, tick.ratio=1)

plot(arbre,
     xlab = "Patients",
     ylab = "Height (dissimilarity)",
     sub = "",
     main = "",
     cex = 0.7, 
     labels = )
rect.hclust(arbre, k = 2, border = "gray69")

dev.off()

tiff(file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/figures/figure_2_optimal_cluster_number.tiff", units = "in",
    width = 8, height = 4, res = 300)

layout(matrix(c(1,2,2), nrow = 1, ncol = 3))

plot(inertie[1:10], type = "s", xlab = "Number of clusters", 
     ylab = "Inertia", lwd = 2)
abline(v = 2, lty=5, col = "gray69")
minor.tick(nx=2, tick.ratio=1)

plot(arbre,
     xlab = "Patients",
     ylab = "Height (dissimilarity)",
     sub = "",
     main = "",
     cex = 0.7, 
     labels = )
rect.hclust(arbre, k = 2, border = "gray69")

dev.off()



#________________________________________________________________________________create graphic for cluster number decision in French

pdf(file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/figures/optimal_cluster_number_french.pdf",
    width = 8,
    height = 4)

layout(matrix(c(1,2,2), nrow = 1, ncol = 3))

plot(inertie[1:10], type = "s", xlab = "Nombre de clusters", 
     ylab = "Inertie", lwd = 2)
abline(v = 2, lty=5, col = "gray69")
minor.tick(nx=2, tick.ratio=1)

plot(arbre,
     xlab = "Patients",
     ylab = "Hauteur (dissimilarité)",
     sub = "",
     main = "",
     cex = 0.7, 
     labels = )
rect.hclust(arbre, k = 2, border = "gray69")

dev.off()





#________________________________________________________________________________divide patients into two groups

typo <- cutree(arbre, 2)
table(typo)

#________________________________________________________________________________calculate means for each group

cent <- aggregate(clust_me, by = list(typo), FUN = mean)
cent <- as.matrix(cent[,2:9])

#________________________________________________________________________________perform k-means clustering

#we are expecting 2 groups with centers iniated by previously determined means.

km <- kmeans(clust_me, centers = cent, iter.max = 10)

#________________________________________________________________________________write k-means output to file

sink('C:/Users/csuehs/Dropbox/PECTUS-cluster/clustering and comparisons/kmeans-output.txt')

    km

sink()

#________________________________________________________________________________create a rawdata subsets for clusters and change fvc and tlc to percentages

raw_data$c <- km$cluster
raw_data$fvc <- raw_data$fvc *100
raw_data$tlc <- raw_data$tlc *100

c1 <- subset(raw_data, c == 1)
c2 <- subset(raw_data, c == 2)

#________________________________________________________________________________get descriptive data for each cluster

eff <- function(x) {length(which(!is.na(x)))}
per_func <- function(x) {mean(x, na.rm = TRUE)*100}
quartile_1 <- function(x) {quantile(x, probs = seq(0,1,0.25), na.rm = TRUE)[2]}
quartile_3 <- function(x) {quantile(x, probs = seq(0,1,0.25), na.rm = TRUE)[4]}
shap <- function(x) {as.numeric(shapiro.test(x)[2])}

#________________qualitative vars for cluster 1


qual_vars <- data.frame(c1$sex,
                        c1$pec_fam,
                        c1$psychosocial,
                        c1$dyspnea,
                        c1$palpit,
                        c1$anom_puls)

count<- sapply(qual_vars, eff)
nums <- sapply(qual_vars, sum, na.rm = TRUE)
percentage_of_1s <- sapply(qual_vars, per_func)

options(scipen = 999)
qual_var_results <- data.frame(count, nums, percentage_of_1s)
write.csv(qual_var_results, file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/clustering and comparisons/c1_percentages.csv")
write.csv2(qual_var_results, file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/clustering and comparisons/c1_percentages_csv2.csv")

#________________qualitative vars for cluster 2

qual_vars <- data.frame(c2$sex,
                        c2$pec_fam,
                        c2$psychosocial,
                        c2$dyspnea,
                        c2$palpit,
                        c2$anom_puls)

count<- sapply(qual_vars, eff)
nums <- sapply(qual_vars, sum, na.rm = TRUE)
percentage_of_1s <- sapply(qual_vars, per_func)

options(scipen = 999)
qual_var_results <- data.frame(count, nums, percentage_of_1s)
write.csv(qual_var_results, file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/clustering and comparisons/c2_percentages.csv")
write.csv2(qual_var_results, file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/clustering and comparisons/c2_percentages_csv2.csv")

#________________quanitative vars for cluster 1


quant_vars <- data.frame(c1$bmi,
                         c1$age,
                         c1$haller,
                         c1$fvc,
                         c1$tlc,
                         c1$rv.tlc,
                         c1$reserve,
                         c1$o2_puls,
                         c1$vent_threshold,
                         c1$vo2max,
                         c1$hr_max,
                         c1$fev1.fvc,
                         c1$frc)

c1q <- quant_vars
colnames(c1q) <- c("bmi",
                   "age",
                   "haller",
                   "fvc",
                   "tlc",
                   "rv.tlc",
                   "reserve",
                   "o2_puls",
                   "vent_threshold",
                   "vo2max",
                   "hr_max",
                   "fev1.fvc",
                   "frc")

count<- sapply(quant_vars, eff)
minimum <- sapply(quant_vars, min, na.rm = TRUE)
maximum <- sapply(quant_vars, max, na.rm = TRUE)
mean <- sapply(quant_vars, mean, na.rm = TRUE)
st_d <- sapply(quant_vars, sd, na.rm = TRUE)
median <- sapply(quant_vars, median, na.rm = TRUE)
quartile_25p <- sapply(quant_vars, quartile_1)
quartile_75p <- sapply(quant_vars, quartile_3)
shapiro_p <- sapply(quant_vars, shap)

options(scipen = 999)
quant_var_results <- data.frame(count, minimum, maximum, mean, 
                                st_d, median, quartile_25p, quartile_75p, shapiro_p)

write.csv(quant_var_results, file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/clustering and comparisons/c1_quantitative.csv")
write.csv2(quant_var_results, file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/clustering and comparisons/c1_quantitative_csv2.csv")

#________________quanitative vars for cluster 2

quant_vars <- data.frame(c2$bmi,
                         c2$age,
                         c2$haller,
                         c2$fvc,
                         c2$tlc,
                         c2$rv.tlc,
                         c2$reserve,
                         c2$o2_puls,
                         c2$vent_threshold,
                         c2$vo2max,
                         c2$hr_max,
                         c2$fev1.fvc,
                         c2$frc)
c2q <- quant_vars
colnames(c2q) <- c("bmi",
                   "age",
                   "haller",
                   "fvc",
                   "tlc",
                   "rv.tlc",
                   "reserve",
                   "o2_puls",
                   "vent_threshold",
                   "vo2max",
                   "hr_max",
                   "fev1.fvc",
                   "frc")

count<- sapply(quant_vars, eff)
minimum <- sapply(quant_vars, min, na.rm = TRUE)
maximum <- sapply(quant_vars, max, na.rm = TRUE)
mean <- sapply(quant_vars, mean, na.rm = TRUE)
st_d <- sapply(quant_vars, sd, na.rm = TRUE)
median <- sapply(quant_vars, median, na.rm = TRUE)
quartile_25p <- sapply(quant_vars, quartile_1)
quartile_75p <- sapply(quant_vars, quartile_3)
shapiro_p <- sapply(quant_vars, shap)

options(scipen = 999)
quant_var_results <- data.frame(count, minimum, maximum, mean, 
                                st_d, median, quartile_25p, quartile_75p, shapiro_p)

write.csv(quant_var_results, file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/clustering and comparisons/c2_quantitative.csv")
write.csv2(quant_var_results, file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/clustering and comparisons/c2_quantitative_csv2.csv")

#________________________________________________________________________________compare clusters (quantitative)


ttest_pvalue <- function(x,y) {
    k <- t.test(x,y)
    as.numeric(k[3])}

wilcox_pvalue <- function(x,y) {
    k <- wilcox.test(x,y)
    as.numeric(k[3])}

permute_pvalue <- function(x,y) {
    grp_1 <- rep(1, length(x))
    grp_2 <- rep(2, length(y))
    val_1 <- data.frame(x, grp_1)
    colnames(val_1) <- c("XXX", "group")
    val_2 <- data.frame(y, grp_2)
    colnames(val_2) <- c("XXX", "group")
    dataa <- rbind(val_1, val_2)
    k <- independence_test(formula = XXX~group, data = dataa)
    pvalue(k)}

tt <- c(); wcmw <- c(); perm <- c()

for (j in 1:dim(c1q)[2]){
    tt[j] <- ttest_pvalue(c1q[,j], c2q[,j])
    wcmw[j] <- wilcox_pvalue(c1q[,j], c2q[,j])
    perm[j] <- permute_pvalue(c1q[,j], c2q[,j])
}

variable <- c("bmi",
              "age",
              "haller",
              "fvc",
              "tlc",
              "rv.tlc",
              "reserve",
              "o2_puls",
              "vent_threshold",
              "vo2max",
              "hr_max",
              "fev1.fvc",
              "frc")

options(scipen = 999)
two_group_comparisons <- data.frame(variable, tt, wcmw, perm)
colnames(two_group_comparisons) <- c("variable", "t-test", "Mann Whitney-Wilcoxon", "permutation")

write.csv(two_group_comparisons, file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/clustering and comparisons/cluster_comparisons_quantitative_variables.csv")
write.csv(two_group_comparisons, file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/clustering and comparisons/cluster_comparisons_quantitative_variables_csv2.csv")

#________________________________________________________________________________compare clusters (quantitative)

qvars <- c("sex", 
           "pec_fam",
           "psychosocial",
           "dyspnea",
           "palpit",
           "anom_puls")

qual_vars <- data.frame(raw_data$sex,
                        raw_data$pec_fam,
                        raw_data$psychosocial,
                        raw_data$dyspnea,
                        raw_data$palpit,
                        raw_data$anom_puls)

colnames(qual_vars) <- qvars

clusterz <- km$cluster
clusterz[clusterz == 1] <- "c1"
clusterz[clusterz == 2] <- "c2"

chi2_p_value <- function(x) {  #x is a qualitative variable and y is a grouping variable
    ct <- table(x,clusterz) 
    chi2 <- chisq.test(ct)
    as.numeric(chi2[3])
}

fish_p_value <- function(x) {  #x is a qualitative variable and y is a grouping variable
    ct <- table(x,clusterz) 
    fishy <- fisher.test(ct)
    as.numeric(fishy[1])
}

khi2 <- sapply(qual_vars, chi2_p_value)
fisher <- sapply(qual_vars, fish_p_value)

qual_compare <- cbind(khi2, fisher)

write.csv(qual_compare, file = 'C:/Users/csuehs/Dropbox/PECTUS-cluster/clustering and comparisons/cluster_comparisons_chi2.csv')
write.csv2(qual_compare, file = 'C:/Users/csuehs/Dropbox/PECTUS-cluster/clustering and comparisons/cluster_comparisons_chi2_csv2.csv')


##################################################################################create a box plotfunciton

pc_boxplot <- function(var1, tit, pval) {
    
    if (pval >= 0.001){
        tity <- paste("Cluster (P = ",as.character(pval),")", sep = "")
       
    } else {
        tity <- "Cluster (P < 0.001)"
        }
    
    mean1 <- mean(var1[raw_data$c == 1], na.rm = TRUE)
    mean2 <- mean(var1[raw_data$c == 2], na.rm = TRUE)
    
    sd1 <- sd(var1[raw_data$c == 1], na.rm = TRUE)
    sd2 <- sd(var1[raw_data$c == 2], na.rm = TRUE)
    
    width <- 0.3   #width of sd box
    
    l1 <- 1-width
    r1 <- 1+width
    t1 <- mean1 + sd1
    b1 <- mean1 - sd1
    
    l2 <- 2-width
    r2 <- 2+width
    t2 <- mean2 + sd2
    b2 <- mean2 - sd2 
    
    bp <- ggplot(raw_data, aes(x = c, y = var1)) +
        geom_rect(xmin = l1,
                  xmax = r1,
                  ymin = b1,
                  ymax = t1,
                  fill = "grey",
                  color = "grey") +
        geom_rect(xmin = l2,
                  xmax = r2,
                  ymin = b2,
                  ymax = t2,
                  fill = "grey",
                  color = "grey") +
        geom_boxplot(aes(group = c, y = var1), alpha = 0.5, outlier.colour = NA) + 
        geom_point(position = position_jitter(width = 0.2), size = 2) + 
        geom_point(aes(x = 1, y = mean1, size = 5)) + 
        geom_point(aes(x = 2, y = mean2, size = 5)) + 
        xlab(tity) +
        ylab(tit) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 2))+
        theme(legend.position = "none",
              axis.title=element_text(size=18),
              axis.text.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(colour = "black"))
    
    bp
    
    }

   
##################################################################################create a barchart function


pc_barchart <- function (var1, tit, pval) {
    
    if (pval >= 0.001){
        tity <- paste("Cluster (P = ",as.character(pval),")", sep = "")
        
    } else {
        tity <- "Cluster (P < 0.001)"
    }
    
    df <- data.frame(cluzter = c("1", "2"),
                     fh = c(mean(var1[raw_data$c == 1], na.rm = TRUE)*100, 
                            mean(var1[raw_data$c == 2], na.rm = TRUE)*100))
    
    
    bc <- ggplot(df, aes(x = cluzter, y = fh)) +
        geom_bar(stat = "identity", fill = "white", color = "black", width = 0.5) +
        xlab(tity) +
        ylab(tit) +
        theme(legend.position = "none",
              axis.title=element_text(size=18),
              axis.text.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(colour = "black"))
    
    bc
    
}


##################################################################################tie all the plots together



pdf(file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/figures/all_single_plots.pdf")

pc_boxplot(raw_data$age, tit = "Age (années)", pval = 0.043)
pc_barchart(raw_data$sex, tit = "Sexe (% male)", pval = 0.113)
pc_boxplot(raw_data$bmi, tit = "IMC kg/m^2", pval = 0.00001)
pc_boxplot(raw_data$haller, tit = "Haller index", pval = 0.0001)
pc_barchart(raw_data$pec_fam, tit = "% Antécedent familial", pval = 0.529)
pc_barchart(raw_data$dyspnea, tit = "% Dyspnée", pval = 0.713)
pc_barchart(raw_data$palpit, tit = "% Palpitations", pval = 0.652)
pc_barchart(raw_data$psychosocial, tit = "% Retentissement psycho-social", pval = 0.726)
pc_boxplot(raw_data$fvc, tit = "CVF, % predicted", pval = 0.0001)
pc_boxplot(raw_data$tlc, tit = "CPT, % predicted", pval = 0.0001)
pc_boxplot(raw_data$rv.tlc, tit = "VR/CPT", pval = 0.0001)
pc_boxplot(raw_data$fev1.fvc, tit = "VEMS/CVF", pval = 0.058)
pc_boxplot(raw_data$frc, tit = "CRF", pval = 0.162)
pc_boxplot(raw_data$o2_puls, tit = "VO2/FC (mL/min)", pval = 0.0001)
pc_barchart(raw_data$anom_puls, tit = "% Anomalie VO2/FC", pval = 0.237)
pc_boxplot(raw_data$vent_threshold, tit = "Seuil (%)", pval = 0.004)
pc_boxplot(raw_data$vo2max, tit = "VO2max (%)", pval = 0.003)
pc_boxplot(raw_data$hr_max, tit = "FC max", pval = 0.967)
pc_boxplot(raw_data$reserve, tit = "Réserve (%)", pval = 0.611)

dev.off()


age_plot <- pc_boxplot(raw_data$age, tit = "Age (years)", pval = 0.043)
sex_plot <- pc_barchart(raw_data$sex, tit = "Sex (% male)", pval = 0.113)
#bmi_plot <- pc_boxplot(raw_data$bmi, tit = "BMI kg/m^2", pval = 0.00001)
bmi_plot <- pc_boxplot(raw_data$bmi, tit = expression(paste("BMI (kg/", m^2, ")")), pval = 0.00001)
hal_plot <- pc_boxplot(raw_data$haller, tit = "Haller index", pval = 0.0001)
fam_plot <- pc_barchart(raw_data$pec_fam, tit = "% Family History", pval = 0.529)
dysp_plot <- pc_barchart(raw_data$dyspnea, tit = "% Dyspnea", pval = 0.713)
palp_plot <- pc_barchart(raw_data$palpit, tit = "% Palpitations", pval = 0.652)
pp_plot <- pc_barchart(raw_data$psychosocial, tit = "% Psycho-social Impact", pval = 0.726)
fvc_plot <- pc_boxplot(raw_data$fvc, tit = "FVC, % predicted", pval = 0.0001)
tlc_plot <- pc_boxplot(raw_data$tlc, tit = "TLC, % predicted", pval = 0.0001)
rt_plot <- pc_boxplot(raw_data$rv.tlc, tit = "RV/TLC", pval = 0.0001)
ff_plot <- pc_boxplot(raw_data$fev1.fvc, tit = "FEV1/FVC", pval = 0.058)
frc_plot <- pc_boxplot(raw_data$frc, tit = "FRC (L)", pval = 0.162)
#o2p_plot <- pc_boxplot(raw_data$o2_puls, tit = "O2 pulse (mL/min)", pval = 0.0001)
o2p_plot <- pc_boxplot(raw_data$o2_puls, tit = expression(paste(O[2], " pulse (mL/min)")), pval = 0.0001)
#anom_plot <- pc_barchart(raw_data$anom_puls, tit = "% Abnormal O2 pulse", pval = 0.237)
anom_plot <- pc_barchart(raw_data$anom_puls, tit = expression(paste("% Abnormal ", O[2], " pulse")), pval = 0.237)
vt_plot <- pc_boxplot(raw_data$vent_threshold, tit = "Ventilation Threshold (%)", pval = 0.004)
#v02_plot <- pc_boxplot(raw_data$vo2max, tit = "VO[2]max (%)", pval = 0.003)
v02_plot <- pc_boxplot(raw_data$vo2max, tit = expression(paste(VO[2], "max (%)")), pval = 0.003)
hrm_plot <- pc_boxplot(raw_data$hr_max, tit = "Max Heart Rate", pval = 0.967)
res_plot <- pc_boxplot(raw_data$reserve, tit = "Ventilatory Reserve (%)", pval = 0.611)



pdf(file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/figures/figure_3_all_plots.pdf",
    width = 20, height = 16)


grid.arrange(age_plot,
             sex_plot,
             bmi_plot,
             hal_plot,
             fam_plot,
             dysp_plot,
             palp_plot,
             pp_plot,
             fvc_plot,
             tlc_plot,
             rt_plot,
             ff_plot,
             frc_plot,
             o2p_plot,
             anom_plot,
             vt_plot,
             v02_plot,
             hrm_plot,
             res_plot,
             nrow = 4)

dev.off()

tiff(file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/figures/figure_3_all_plots.tiff", units = "in",
    width = 20, height = 16, res = 200)


grid.arrange(age_plot,
             sex_plot,
             bmi_plot,
             hal_plot,
             fam_plot,
             dysp_plot,
             palp_plot,
             pp_plot,
             fvc_plot,
             tlc_plot,
             rt_plot,
             ff_plot,
             frc_plot,
             o2p_plot,
             anom_plot,
             vt_plot,
             v02_plot,
             hrm_plot,
             res_plot,
             nrow = 4)

dev.off()





############################################################################______qualitative only


pdf(file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/figures/percentages_plots.pdf",
    width = 10, height = 15)


grid.arrange(sex_plot,
             fam_plot,
             dysp_plot,
             palp_plot,
             pp_plot,
             anom_plot,
             nrow = 3)

dev.off()

############################################################################______quantitative only

pdf(file = "C:/Users/csuehs/Dropbox/PECTUS-cluster/figures/box_plots.pdf",
    width = 20, height = 20)


grid.arrange(age_plot,
             bmi_plot,
             hal_plot,
             fvc_plot,
             tlc_plot,
             rt_plot,
             ff_plot,
             frc_plot,
             o2p_plot,
             vt_plot,
             v02_plot,
             hrm_plot,
             res_plot,
             nrow = 4)

dev.off()
