# Instalação de pacotes

pacotes <- c("ggplot2", "scatterplot3d", "rgl", "GGally", "tidyr", "MVN",
             "biotools", "factoextra", "psych", "MASS", "ggrepel", "stringr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Importando o cojunto de dados dos pardais sobreviventes da tempestade
# Survivorship =   sobrevivência
# Total_length =   comprimento total;
# Alar_extent =    extensão alar;
# L_beak_head =    comprimento do bico e cabeça;
# L_humerous =     comprimento do úmero;
# L_keel_sternum = comprimento da quilha do esterno.

# dados em .txt
pardais <- read.table(file = "Bumpus_sparrows.txt", 
                      header = TRUE)
pardais

# transformando sobrevivencia em fator
pardais$Survivorship
is.factor(pardais$Survivorship)

pardais$Survivorship <- as.factor(pardais$Survivorship)
pardais$Survivorship
# character serve para sequências de caracteres, ou seja, textos. 
# factor serve para categorias.

# Gráfico de dispersão ----------------------------------------------------

# Gráfico de dispersão 2D utilizando a função plot

plot(x = pardais$Total_length, 
     y = pardais$Alar_extent)

# Adicionando os rótulos dos eixos x e y
plot(x = pardais$Total_length, 
     y = pardais$Alar_extent,
     xlab = "Comprimento total (mm)",
     ylab = "Extensão alar (mm)")

# Adicionando a informação 
par(mar=c(4.1, 4.1, 4.1, 8.1), xpd=TRUE)

plot(x = pardais$Total_length, 
     y = pardais$Alar_extent,
     xlab = "Comprimento total (mm)",
     ylab = "Extensão alar (mm)",
     col = pardais$Survivorship,
     pch = 19) #pinte a bolinha inteira

legend("topright", inset=c(-.60,0), legend=c("NS","S"), pch=19, title="Sobrevivência",  col = c(1,2)) #não funcionou


# Utilizando pacotes 

library(ggplot2)

ggplot(data = pardais, aes(x = Total_length, y = Alar_extent)) +
  geom_point()

ggplot(data = pardais, aes(x = Total_length, y = Alar_extent)) +
  geom_point(aes(color = Survivorship)) 

ggplot(data = pardais, aes(x = Total_length, y = Alar_extent)) +
  geom_point(aes(color = Survivorship)) +
  labs(
    x = "Comprimento total (mm)",
    y = "Extensão alar (mm)",
    color = "Sobrevivência") +
  scale_color_manual(values=c("black", "red"),
                     labels = c(NS = "Não sobreviventes", S = "Sobreviventes")) +
  theme(legend.position="bottom") 


# Gráfico de dispersão 3D

#install.packages("scatterplot3d") # Instala o pacote ScatterPlot3D
library("scatterplot3d")
scatterplot3d(x = pardais$Total_length, 
              y = pardais$Alar_extent, 
              z = pardais$L_beak_head)

?scatterplot3d


#install.packages("rgl")
library("rgl")
plot3d(x = pardais$Total_length,
       y = pardais$Alar_extent,
       z = pardais$L_beak_head)


plot3d(x = pardais$Total_length,
       y = pardais$Alar_extent,
       z = pardais$L_beak_head,
       xlab = "Comprimento total",
       ylab = "Extensao alar", 
       zlab = "Comprimento do bico e cabeca",
       col = as.integer(pardais$Survivorship),
       size=5)
legend3d("topright", legend = levels(pardais$Survivorship), col = c(2,1), pch=19)


# Matriz de dispersão ---------------------------------------------------

pairs(x = pardais[,-1])

pairs(pardais[,-1], 
      labels = c("C. total", 
                 "Extensão alar", 
                 "C. bico \n e cabeça", 
                 "C. úmero", 
                 "C. quilha do \n esterno"),
      col = c("black", "red")[pardais$Survivorship],
      pch = 19)


#install.packages("GGally")
library("GGally")
ggpairs(pardais[,-1])

ggpairs(pardais,
        columns = 2:6,
        aes(color = Survivorship),
        columnLabels =  c("C. total", 
                          "Extensão \n alar", 
                          "C. bico e \n cabeça", 
                          "C. úmero", 
                          "C. quilha do \n esterno"),
        legend = 1) +
  scale_color_manual(values = c(S = "black", NS = "red")) +
  scale_fill_manual(values = c(S = "black", NS = "red")) +
  labs(color = "Survivorship") +
  theme(legend.position = "bottom")


####----------------------#######

library(factoextra)
library(ggfortify)

colnames(pardais)
df <- pardais[c(2:6)]

pca_res <- prcomp(df, scale = TRUE)
summary(pca_res)
print(pca_res) 

biplot <- data.frame(pca_res$x, Survivorship = pardais$Survivorship)
biplot

ggplot(data = biplot, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Survivorship), size = 4) +
  labs(
    x = "PC1 (72.32%)",
    y = "PC2 (10.63%)",
    color = "Survivorship") +
  scale_color_manual(values=c("red", "green")) +
  theme(legend.position="bottom") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  theme_bw()

fviz_pca_biplot(pca_res, ggtheme = theme_bw(), repel = T)

autoplot(pca_res)
bird.pca = autoplot(pca_res, data = pardais, colour = 'Survivorship',
                    loadings = TRUE, loadings.colour = 'grey70',
                    loadings.label = TRUE, loadings.label.size = 3)

bird.pca +
  theme_bw()

#dev.print(tiff, "./Figures/PLratio.tiff", compression = "lzw", res=600, height=6, width=9, units="in")

fviz_pca_ind(pca_res,
             col.ind = pardais$Survivorship, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)


###----------------------------###

#ANOVA
cranios <- read.table("Egyptian skulls.txt", h=T)
cranios

cranios$Period <- as.factor(cranios$Period)

ggplot(data = cranios,
       aes(y = Maximum.breadth)) +
  geom_boxplot(aes (fill = Period))

ggplot(data = cranios,
       aes(x = Maximum.breadth)) +
  geom_histogram() 

fit <- lm(Maximum.breadth ~ Period, data = cranios)
anova(fit)
qf(p = .95, df1 = 4, df2 = 145)

tukey.test <- TukeyHSD(aov(fit))
tukey.test

#MANOVA
fit <- manova(cbind(Maximum.breadth, Basibregmatic.height, 
                    Basialveolar.length, Nasal.height) 
              ~ Period, data = cranios)
fit
summary(fit, test = "Wilks")

#Normality
library(MVN)
mvn(data = cranios[,2:5], mvnTest = "royston", univariatePlot =
      "qqplot")


####-------######

colnames(cranios)
df <- cranios[c(2:5)]

pca_res <- prcomp(df, scale = TRUE)
summary(pca_res)
print(pca_res) 

biplot <- data.frame(pca_res$x, Period = cranios$Period)
biplot

ggplot(data = biplot, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Period), size = 4) +
  labs(
    x = "PC1 (33.43%)",
    y = "PC2 (30.16%)",
    color = "Period") +
  scale_color_manual(values=c("red", "green", "grey", "lightblue", "darkblue")) +
  theme(legend.position="bottom") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  theme_bw()

#HUES

fviz_pca_biplot(pca_res, ggtheme = theme_bw(), repel = T)

dogs.pca = autoplot(pca_res, data = cranios, colour = 'Period',
                    loadings = TRUE, loadings.colour = 'grey70',
                    loadings.label = TRUE, loadings.label.size = 3)

dogs.pca +
  theme_bw()

#dev.print(tiff, "./Figures/PLratio.tiff", compression = "lzw", res=600, height=6, width=9, units="in")

dogs.pca +
  theme_bw() +
annotate("text", x = -0.2, y = 0.3, hjust = 0 , 
         label = bquote('p-value = 7.01e-07'), size = 3)

