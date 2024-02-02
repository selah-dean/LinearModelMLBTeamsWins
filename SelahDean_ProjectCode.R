#Selah Dean 
#Project - What Contributes to a Major League Baseball Team’s Wins in a Season?

library(tidyverse)
library(reshape2)
library(viridis) 
library(sjPlot)
library(MASS)
library(corrplot)
library(ggpubr)
library(flextable)
library(factoextra)

batting <- read.csv("batting.csv")
pitching <- read.csv("pitching.csv")
fielding <- read.csv("fielding.csv")
standing <- read.csv("standing.csv")

batting <- batting %>% dplyr::select(Tm, PA, R, H, X2B, X3B, HR, SB, CS, BB, SO, GDP, HBP, IBB, LOB, Year)
pitching <- pitching %>% dplyr::select(Tm, tSho, H, R, ER, HR, BB, IBB, SO, HBP, WP, BF, LOB, Year)
fielding <- fielding %>% dplyr::select(Tm, PO, A, E, DP, Year)
standing <- standing %>% dplyr::select(Tm, W, W.L., Year)

data <- list(batting, pitching, fielding, standing) %>% reduce(inner_join, by=c("Tm", "Year"))
data <- data %>% relocate(c(Tm, Year))

replace_chars <- function(col_name) {
  col_name <- gsub(".x", "", col_name)
  col_name <- gsub(".y", "A", col_name)
}

data <- data %>%
  rename_with(replace_chars, matches("\\.x|\\.y"))

data_2023 <- data %>% filter(Year==2023)
data_train <- data %>% filter(Year!=2023)


win_distr <- ggplot(data_train) + geom_histogram(aes(x=W),color="slategray", fill="slategray1",binwidth=3) + 
  xlab("Wins") + ggtitle("Figure 1: Distribution of Wins") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, face="bold")) 

ggsave("Figures/figure1_windistr.jpg", win_distr, width=5, height=5)

features <- data_train[,3:32]

# sampled_columns <- sample(colnames(features), 5)
# data1 <- subset(features, select=sampled_columns)
# pairs(data1)

corr_mat <- round(cor(features),2)
melted_corr_mat <- melt(corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white", lwd = 1, linetype = 1) +
  scale_fill_viridis() + xlab("") + ylab("") + ggtitle("Covariance") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(fill = "Covariance")

#par(mar = c(5, 5, 1, 2))

jpeg("Figures/figure2_correlation_plot.jpg", width = 800, height = 800)
corrplot(cor(features), type="upper",  
         order="hclust", tl.col="black", tl.srt=45)
mtext("Figure 2: Correlation Between Features", cex=1.5, side=3, font=2)
dev.off()

for_model <- data_train[,3:33]
full_model <- lm(W~., data=for_model)

tab_model(full_model, title="Table 1: Full Model Summary", file="Figures/table1.html")

resid_fitted_full <- ggplot(full_model, aes(x = .fitted, y = .resid)) +
  geom_point() + geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(se = FALSE, color = "red") + 
  xlab("Fitted Values") + ylab("Residuals") +
  ggtitle("Residuals vs. Fitted") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


qq_resid_full <- ggplot(full_model, aes(sample = .resid)) + stat_qq() + stat_qq_line() + 
  xlab("Theoretical Quantiles") + ylab("Standardized Residuals") + 
  ggtitle("Q-Q Residuals") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

shapiro.test(full_model$resid)

full_model_plot <- ggarrange(resid_fitted_full, qq_resid_full, ncol=2, nrow=1)
full_model_plot <- annotate_figure(full_model_plot, top = text_grob("Figure 3: Full Model Residuals",size = 14, face="bold"))

ggsave("Figures/figure3_fullmodel_residuals.jpg", full_model_plot, width=8, height=5)

both_sel <- stepAIC(full_model, direction = "both")
tab_model(both_sel, title="Table 2: Reduced Model Summary", file="Figures/table2.html")
summary(both_sel)

anova <- anova(full_model, both_sel, test="F")
anova_table <- flextable(anova)
anova_table <- add_header_lines(anova_table, 
                       values = c("Analysis of Variance Table\nModel 1: Full Model\nModel 2: Stepwise Reduced Model")) 

resid_fitted_reduced <- ggplot(both_sel, aes(x = .fitted, y = .resid)) +
  geom_point() + geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(se = FALSE, color = "red") + 
  xlab("Fitted Values") + ylab("Residuals") +
  ggtitle("Residuals vs. Fitted") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

qq_resid_reduced <- ggplot(full_model, aes(sample = .resid)) + stat_qq() + stat_qq_line() + 
  xlab("Theoretical Quantiles") + ylab("Standardized Residuals") + 
  ggtitle("Q-Q Residuals") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

reduced_model_plot <- ggarrange(resid_fitted_reduced, qq_resid_reduced, ncol=2, nrow=1)
reduced_model_plot <- annotate_figure(reduced_model_plot, top = text_grob("Figure 4: Reduced Model",size = 14, face="bold"))

ggsave("Figures/figure4_reducedmodel_residuals.jpg", reduced_model_plot, width=8, height=5)




pca <- princomp(features, cor=TRUE)
summary(pca)



PCs <- pca$scores  ## the principal components are the scores
eigenvalues <- get_eigenvalue(pca)


scree_plot <- ggplot(eigenvalues, aes(x=1:30, y=eigenvalue)) + 
  geom_line() + 
  geom_point(size=3, shape=21, color = 'black', fill='white', stroke = 1) + 
  geom_hline(yintercept=1, linetype="dashed", color = "red") + 
  scale_x_continuous(limits = c(1, 30), breaks = 1:30) + 
  theme_bw() + theme(plot.title=element_text(hjust=0.5)) + 
  xlab("Principle Component") + ylab("Eigenvalue") + 
  ggtitle("Scree Plot")

expl_var <- ggplot(eigenvalues, aes(x=1:30, y=cumulative.variance.percent)) + 
  geom_line() + 
  geom_point(size=3, shape=21, color = 'black', fill='white', stroke = 1) + 
  scale_x_continuous(limits = c(1, 30), breaks = 1:30) + 
  theme_bw() + theme(plot.title=element_text(hjust=0.5)) + 
  xlab("Top k Principle Components") + ylab("Cumulative Variance") + 
  ggtitle("Cumulative Variance Explained by Top k Principal Components")


pca_plot <- ggarrange(scree_plot, expl_var, nrow=1, ncol=2)
pca_plot <- annotate_figure(pca_plot, top = text_grob("Figure 5: PCA",size = 14, face="bold"))

ggsave("Figures/figure5_pca.jpg", pca_plot, width=12, height=6)


data_pc <- data.frame(W = data_train$W, PCs)
lm_pca4 <- lm(W ~ Comp.1 + Comp.2 + Comp.3 + Comp.4, data=data_pc)
tab_model(lm_pca4)

lm_pca7 <- lm(W ~ Comp.1 + Comp.2 + Comp.3 + Comp.4 + Comp.5+ Comp.6 + Comp.7, data=data_pc)
tab_model(lm_pca7)

ggplot(lm_pca4, aes(x = .fitted, y = .resid)) +
  geom_point() + geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(se = FALSE, color = "red") + 
  xlab("Fitted Values") + ylab("Residuals") +
  ggtitle("Residuals vs. Fitted") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

ggplot(lm_pca4, aes(sample = .resid)) + stat_qq() + stat_qq_line() + 
  xlab("Theoretical Quantiles") + ylab("Standardized Residuals") + 
  ggtitle("Q-Q Residuals") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

ggplot(lm_pca7, aes(x = .fitted, y = .resid)) +
  geom_point() + geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(se = FALSE, color = "red") + 
  xlab("Fitted Values") + ylab("Residuals") +
  ggtitle("Residuals vs. Fitted") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

ggplot(lm_pca7, aes(sample = .resid)) + stat_qq() + stat_qq_line() + 
  xlab("Theoretical Quantiles") + ylab("Standardized Residuals") + 
  ggtitle("Q-Q Residuals") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

data_2023_features <- data_2023[,3:32]
new_pca <- data.frame(predict(pca, data_2023_features))
predict(lm_pca4, newdata=new_pca)

predictions <- data.frame(W=data_2023$W, 
                          full_pred = predict(full_model, newdata=data_2023), 
                          reduced_pred = predict(both_sel, newdata=data_2023), 
                          pca4_pred = predict(lm_pca4, newdata=new_pca), 
                          pca7_pred = predict(lm_pca7, newdata=new_pca))

pred <- ggplot(predictions) + geom_point(aes(x=full_pred, y = W, color="Full"), size=2) + 
  geom_point(aes(x=reduced_pred, y=W, color="Reduced"), size=2) + 
  geom_point(aes(x=pca4_pred, y=W, color="PCA (4 Components)"), size=2) + 
  geom_point(aes(x=pca7_pred, y=W, color="PCA (7 Components)"), size=2) + 
  geom_abline(intercept=0, slope=1) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, face="bold")) + 
  labs(title = "Figure 6: Actual vs Predicted Number of Wins", x = "Predicted Wins", y = "Actual Wins", color = "Model Used for Predictions")  

ggsave("Figures/figure6_predictions.jpg", pred, width=8, height=4)




test_r2 <- data.frame(
  full = summary(lm(W ~ full_pred, data=predictions))$adj.r.squared, 
  reduced = summary(lm(W ~ reduced_pred, data=predictions))$adj.r.squared, 
  pca4 = summary(lm(W ~ pca4_pred, data=predictions))$adj.r.squared, 
  pca7 = summary(lm(W ~ pca7_pred, data=predictions))$adj.r.squared
)
  
  



test_full_model <- lm(W ~ full_pred, data=predictions)
test_reduced_model <- lm(W ~ reduced_pred, data=predictions)
test_pca4_model <- lm(W ~ pca4_pred, data=predictions)
test_pca4_model <- lm(W ~ pca7_pred, data=predictions)


