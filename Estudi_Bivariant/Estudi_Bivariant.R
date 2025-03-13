#######################################
# Estudi Bivariant entre Age i Fare
# Utilitzant el dataset Titanic built-in de R
#######################################

# Instal·lar el paquet si no està instal·lat
if (!requireNamespace("titanic", quietly = TRUE)) {
  install.packages("titanic")
}
if (!requireNamespace("vcd", quietly = TRUE)) {
  install.packages("vcd")
}
library(vcd)
if (!requireNamespace("ggmosaic", quietly = TRUE)) {
  install.packages("ggmosaic")
}
library(ggmosaic)
# Carregar el paquet
library(titanic)
library(ggplot2)
library(dplyr)

# Carregar el dataset Titanic
data <- titanic::titanic_train

#######################################
# Preparació de les dades
#######################################

# Seleccionar només les columnes d'interès
data_clean <- data %>%
  select(Survived, Age, Fare) %>%
  na.omit() %>%
  mutate(
    # Categorització d'edat
    Age_Group = cut(Age,
                    breaks = c(0, 12, 18, 30, 50, 100),
                    labels = c("Infant", "Adolescent", "Jove", "Adult", "Granadult"),
                    include.lowest = TRUE),
    
    # Categorització de tarifes (basat en totes les dades)
    Fare_Group = cut(Fare,
                     breaks = quantile(Fare, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                     labels = c("Baix", "Mitjà", "Alt"),
                     include.lowest = TRUE)
  )

# Separar en supervivents i morts
supervivents <- data_clean %>% filter(Survived == 1)
morts <- data_clean %>% filter(Survived == 0)

#######################################
# Anàlisi descriptiva
#######################################

# Estadístiques descriptives per als grups
summary(supervivents$Age)
summary(supervivents$Fare)
summary(morts$Age)
summary(morts$Fare)

#######################################
# Crear taules de contingència
#######################################


crear_taula_completa <- function(dades, grup) {
  # Crear taula amb sumes marginals
  taula <- table(dades$Age_Group, dades$Fare_Group) %>% 
    addmargins() 
  
  # Afegir noms descriptius
  dimnames(taula) <- list(
    c(paste("Edat", levels(dades$Age_Group)), "Total Edat"),
    c(paste("Tarifa", levels(dades$Fare_Group)), "Total Tarifa")
  )
  
  # Formatar la sortida
  cat("\n", paste0("Taula de contingència ", grup, " (Edat vs Tarifa)"), "\n")
  print(taula)
}

crear_taula_completa(supervivents, "SUPERVIVENTS")
crear_taula_completa(morts, "MORTS")


#######################################
# Visualització per Grups d'Edat


# Preparar dades per a visualització
data_visual <- data_clean %>%
  mutate(
    Survived = factor(Survived, levels = c(1,0), labels = c("Supervivents", "Morts"))
  )

# Gràfic comparatiu d'edat
ggplot(data_visual, aes(x = Age_Group, fill = Fare_Group)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Survived, ncol = 1) +
  labs(title = "Distribució per Edat i Tarifa",
       subtitle = "Comparativa entre Supervivents i Morts",
       x = "Grup d'Edat",
       y = "Nombre de Passatgers",
       fill = "Tarifa") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################################
# Visualització per Categories de Tarifa


# Gràfic comparatiu de tarifa
ggplot(data_visual, aes(x = Fare_Group, fill = Age_Group)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Survived, ncol = 1) +
  labs(title = "Distribució per Tarifa i Edat",
       subtitle = "Comparativa entre Supervivents i Morts",
       x = "Categoria de Tarifa",
       y = "Nombre de Passatgers",
       fill = "Grup d'Edat") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################################
# Versió Percentual per Comparar Proporcions


# Versió normalitzada per a comparació percentual
ggplot(data_visual, aes(x = Age_Group, fill = Fare_Group)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Survived) +
  labs(title = "Distribució Percentual per Edat i Tarifa",
       x = "Grup d'Edat",
       y = "Percentatge",
       fill = "Tarifa") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  theme_minimal()

#######################################
# Taula de contingència per frequències

# Funció per taules relatives (percentatges)

crear_taula_relativa <- function(dades, grup) {
  taula <- table(dades$Age_Group, dades$Fare_Group)
  
  # CORRECCIÓ: Parèntesis per a l'ordre correcte d'operacions
  taula_relativa <- (prop.table(taula) * 100) %>% 
    addmargins() %>%
    round(1)
  
  dimnames(taula_relativa) <- list(
    c(paste("Edat", levels(dades$Age_Group)), "Total Edat"),
    c(paste("Tarifa", levels(dades$Fare_Group)), "Total Tarifa")
  )
  
  cat("\n", paste0("Taula RELATIVA ", grup, " (%)"), "\n")
  print(taula_relativa)
}


# Executar per a tots els grups
grups <- list(
  list(dades = supervivents, nom = "SUPERVIVENTS"),
  list(dades = morts, nom = "MORTS")
)

for (grup in grups) {
  crear_taula_relativa(grup$dades, grup$nom)
}

#######################################
# Diagrames de mosaic per grups

# Configurar l'àrea de dibuix
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

# Mosaic per supervivents
mosaic(~ Age_Group + Fare_Group, 
       data = supervivents,
       main = "SUPERVIVENTS",
       labeling_args = list(
         set_varnames = c(Age_Group = "Grup d'Edat", Fare_Group = "Tarifa"),
         rot_labels = c(20, 0, 0, 0)
       ),
       shade = TRUE,
       legend = FALSE)

# Mosaic per morts
mosaic(~ Age_Group + Fare_Group, 
       data = morts,
       main = "MORTS",
       labeling_args = list(
         set_varnames = c(Age_Group = "Grup d'Edat", Fare_Group = "Tarifa"),
         rot_labels = c(20, 0, 0, 0)
       ),
       shade = TRUE)

# Frequencies esperades

if (!requireNamespace("titanic", quietly = TRUE)) install.packages("titanic")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("vcd", quietly = TRUE)) install.packages("vcd")

library(titanic)
library(dplyr)
library(vcd)

# Carregar i preparar dades CORRECTAMENT
data_clean <- titanic_train %>%
  select(Survived, Age, Fare) %>%
  na.omit() %>%
  mutate(
    Age_Group = cut(Age,
                    breaks = c(0, 12, 18, 30, 50, 100),
                    labels = c("Infant", "Adolescent", "Jove", "Adult", "Granadult")),
    Age_Group = fct_collapse(
      Age_Group,
      "Infant/Adolescent" = c("Infant", "Adolescent"),
      "Adult+" = c("Jove", "Adult", "Granadult")
    ),
    Fare_Group = cut(Fare,
                     breaks = quantile(Fare, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                     labels = c("Baix", "Mitjà", "Alt")),
    Survived = factor(Survived, labels = c("Morts", "Supervivents"))
  )

# 1. Taula de freqüències esperades
crear_taula_esperada <- function(dades, grup) {
  taula_obs <- table(dades$Age_Group, dades$Fare_Group)
  esperades <- round(chisq.test(taula_obs)$expected, 1)
  
  cat("\nTaula ESPERADA -", grup, ":\n")
  print(addmargins(esperades))
}

# Aplicar als grups
supervivents <- data_clean %>% filter(Survived == "Supervivents")
morts <- data_clean %>% filter(Survived == "Morts")

crear_taula_esperada(supervivents, "Supervivents")
crear_taula_esperada(morts, "Morts")

## Generar mosaics

#######################################
# Estadistic x^2
library(dplyr)

# Funció per calcular i mostrar resultats del test χ²
calcular_qui_quadrat <- function(dades, grup) {
# Crear taula de contingència
taula <- table(dades$Age_Group, dades$Fare_Group)

# Realitzar test de Qui-quadrat
test <- chisq.test(taula)

# Mostrar resultats
cat("\n", paste0("Test χ² per a ", grup, ":"), "\n")
cat("Estadístic χ²:", round(test$statistic, 3), "\n")
cat("p-valor:", format.pval(test$p.value, digits = 3), "\n")
cat("Graus de llibertat:", test$parameter, "\n")

# Mostrar advertències si hi ha freqüències esperades baixes
if (any(test$expected < 5)) {
  cat("\nADVERTÈNCIA: Algunes freqüències esperades són <5. Considereu:\n",
      "- Agrupar categories\n",
      "- Utilitzar el test exacte de Fisher\n")
  }
}

# Aplicar als dos grups
calcular_qui_quadrat(supervivents, "SUPERVIVENTS")
calcular_qui_quadrat(morts, "MORTS")

## Taula residus x^2
library(vcd)

# Funció per crear taula de residus de Qui-quadrat
generar_taula_residus <- function(dades, grup) {
  # Crear taula de contingència
  taula <- table(dades$Age_Group, dades$Fare_Group)
  
  # Calcular residus estandarditzats
  residus <- round(residuals(chisq.test(taula), type = "pearson"), 2)
  
  # Convertir a data.frame amb noms descriptius
  residus_df <- as.data.frame.matrix(residus)
  colnames(residus_df) <- paste("Tarifa", colnames(residus_df))
  rownames(residus_df) <- paste("Edat", rownames(residus_df))
  
  # Afegir capçalera
  cat("\n", paste0("Residus de Qui-quadrat (", grup, "):"), "\n")
  print(residus_df)
}

# Generar taules per als dos grups
generar_taula_residus(supervivents, "Supervivents")
generar_taula_residus(morts, "Morts")

# Heatmap residus
library(ggplot2)
library(reshape2)
library(dplyr)

# Funció corregida per crear heatmap
plot_heatmap <- function(dades, grup) {
  # Calcular residus i convertir a tibble amb rownames
  residus <- chisq.test(table(dades$Age_Group, dades$Fare_Group))$residuals %>%
    as.data.frame.matrix() %>%
    tibble::rownames_to_column("Age_Group") %>%
    melt(id.vars = "Age_Group", variable.name = "Fare_Group", value.name = "Residu")
  
  # Crear heatmap
  ggplot(residus, aes(x = Fare_Group, y = Age_Group, fill = Residu)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "blue", 
      midpoint = 0, limits = c(-3, 3)
    ) +
    geom_text(aes(label = round(Residu, 1)), color = "black", size = 3) +
    labs(
      title = paste("Residus del χ² -", grup),
      x = "Tarifa", y = "Grup d'Edat"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Generar gràfics
plot_heatmap(supervivents, "Supervivents")
plot_heatmap(morts, "Morts")

#######################################
# Covariancia

# Filtrar dades i calcular covariància
cov_supervivents <- data_clean %>% 
  filter(Survived == "Supervivents") %>% 
  select(Age, Fare) %>% 
  cov(use = "complete.obs")  # Ignora valors NA

cov_morts <- data_clean %>% 
  filter(Survived == "Morts") %>% 
  select(Age, Fare) %>% 
  cov(use = "complete.obs")

# Funció per mostrar resultats
mostrar_cov <- function(cov_matrix, grup) {
  cat("\nMatriu de covariància per a", grup, ":\n")
  print(cov_matrix)
  cat("\nCovariància entre Age i Fare:", cov_matrix[1, 2])
}

# Resultats
mostrar_cov(cov_supervivents, "Supervivents")
mostrar_cov(cov_morts, "Morts")

##
# Visualitzacio covariancia
library(ggplot2)
library(gridExtra)  # Per combinar gràfics

# Filtrar dades
supervivents <- data_clean %>% filter(Survived == "Supervivents")
morts <- data_clean %>% filter(Survived == "Morts")

# Funció per crear gràfics
crear_grafic <- function(dades, grup, covariancia) {
  ggplot(dades, aes(x = Age, y = Fare)) +
    geom_point(alpha = 0.6, color = ifelse(grup == "Supervivents", "#1b9e77", "#d95f02")) +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    labs(
      title = paste("Relació Age-Fare:", grup),
      subtitle = paste("Covariància =", round(covariancia, 1)),
      x = "Edat", 
      y = "Preu del bitllet"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
}

# Generar gràfics
g1 <- crear_grafic(supervivents, "Supervivents", cov_supervivents[1,2])
g2 <- crear_grafic(morts, "Morts", cov_morts[1,2])

# Mostrar junts
grid.arrange(g1, g2, ncol = 2)

##
# Calcular correlació de Pearson
cor_supervivents <- cor(supervivents$Age, supervivents$Fare, use = "complete.obs")
cor_morts <- cor(morts$Age, morts$Fare, use = "complete.obs")

# Tests de significació (opcional)
test_supervivents <- cor.test(supervivents$Age, supervivents$Fare)
test_morts <- cor.test(morts$Age, morts$Fare)

# Mostrar resultats
cat("Supervivents:\n",
    "  Coeficient de correlació (r) =", round(cor_supervivents, 3), "\n",
    "  p-valor =", format.pval(test_supervivents$p.value, digits = 3), "\n\n")

cat("Morts:\n",
    "  Coeficient de correlació (r) =", round(cor_morts, 3), "\n",
    "  p-valor =", format.pval(test_morts$p.value, digits = 3))

## Visualitzacio pearson
library(ggplot2)
library(gridExtra)

# Funció per crear gràfics
plot_cor <- function(dades, grup, r) {
  ggplot(dades, aes(x = Age, y = Fare)) +
    geom_point(alpha = 0.5, color = ifelse(grup == "Supervivents", "#1b9e77", "#d95f02")) +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    labs(
      title = paste("Correlació Age-Fare:", grup),
      subtitle = paste("Pearson r =", round(r, 3)),
      x = "Edat", 
      y = "Preu del bitllet"
    ) +
    theme_minimal()
}

# Generar gràfics
g1 <- plot_cor(supervivents, "Supervivents", cor_supervivents)
g2 <- plot_cor(morts, "Morts", cor_morts)

# Mostrar junts
grid.arrange(g1, g2, ncol = 2)

## Desigualtat cauchy
# Verificar la desigualtat de Cauchy-Schwarz per a cada grup
verificar_cauchy_schwarz <- function(dades) {
  # Calcular covariància
  cov_xy <- cov(dades$Age, dades$Fare, use = "complete.obs")
  
  # Calcular desviacions estàndard
  sigma_x <- sd(dades$Age, na.rm = TRUE)
  sigma_y <- sd(dades$Fare, na.rm = TRUE)
  
  # Producte de desviacions
  producte_sigmas <- sigma_x * sigma_y
  
  # Retornar resultats
  data.frame(
    Covariància = abs(cov_xy),
    Producte_Desviacions = producte_sigmas,
    Es_Compleix = abs(cov_xy) <= producte_sigmas
  )
}

# Aplicar als dos grups
resultat_supervivents <- verificar_cauchy_schwarz(supervivents)
resultat_morts <- verificar_cauchy_schwarz(morts)

# Mostrar resultats
cat("--- Supervivents ---\n")
print(resultat_supervivents)

cat("\n--- Morts ---\n")
print(resultat_morts)

##
## Visualitzacio covariància
library(ggplot2)
library(gridExtra)

# Filtrar dades
supervivents <- data_clean %>% filter(Survived == "Supervivents")
morts <- data_clean %>% filter(Survived == "Morts")

# Calcular correlacions
r_supervivents <- round(cor(supervivents$Age, supervivents$Fare, use = "complete.obs"), 3)
r_morts <- round(cor(morts$Age, morts$Fare, use = "complete.obs"), 3)

# Funció per crear gràfics
crear_scatter <- function(dades, grup, r) {
  ggplot(dades, aes(x = Age, y = Fare)) +
    geom_point(color = ifelse(grup == "Supervivents", "#1b9e77", "#d95f02"), 
               alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    labs(
      title = paste0(grup, " (r = ", r, ")"),
      x = "Edat",
      y = "Preu del bitllet"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      panel.grid.minor = element_blank()
    )
}

# Generar gràfics
plot_supervivents <- crear_scatter(supervivents, "Supervivents", r_supervivents)
plot_morts <- crear_scatter(morts, "Morts", r_morts)

# Mostrar junts
grid.arrange(plot_supervivents, plot_morts, ncol = 2)



#######################################
# Modelització: Regressió Lineal
#######################################

# Filtrar dades i ajustar model
model_supervivents <- lm(Fare ~ Age, data = supervivents)

# Extreure coeficients
intercept_sup <- round(coef(model_supervivents)[1], 2)
pendent_sup <- round(coef(model_supervivents)[2], 2)

cat("Recta de regressió (Supervivents):\n")
cat("Fare =", pendent_sup, "* Age +", intercept_sup, "\n")

model_morts <- lm(Fare ~ Age, data = morts)
intercept_morts <- round(coef(model_morts)[1], 2)
pendent_morts <- round(coef(model_morts)[2], 2)

cat("Recta de regressió (Morts):\n")
cat("Fare =", pendent_morts, "* Age +", intercept_morts, "\n")

#######################################
# Visualització gràfica
#######################################

library(ggplot2)
library(gridExtra)

# Funció per crear gràfics amb equació
crear_grafic_regressio <- function(dades, grup, pendent, intercept, color) {
  ggplot(dades, aes(x = Age, y = Fare)) +
    geom_point(color = color, alpha = 0.6) +
    geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE) +
    geom_text(
      x = max(dades$Age, na.rm = TRUE) * 0.7, 
      y = max(dades$Fare, na.rm = TRUE) * 0.8,
      label = paste0("y = ", pendent, "x + ", intercept),
      color = "black",
      size = 5
    ) +
    labs(
      title = paste("Recta de Regressió -", grup),
      x = "Edat",
      y = "Preu del bitllet"
    ) +
    theme_minimal()
}

# Generar gràfics
g1 <- crear_grafic_regressio(supervivents, "Supervivents", pendent_sup, intercept_sup, "#1b9e77")
g2 <- crear_grafic_regressio(morts, "Morts", pendent_morts, intercept_morts, "#d95f02")

# Mostrar junts
grid.arrange(g1, g2, ncol = 2)

#####################################
# Coeficient de deternimació
#####################################
# Model de regressió
model_supervivents <- lm(Fare ~ Age, data = supervivents)

# R²
r_squared_sup <- summary(model_supervivents)$r.squared

cat("Coeficient de determinació (Supervivents):", round(r_squared_sup, 3), "\n")
# Sortida: 0.026

model_morts <- lm(Fare ~ Age, data = morts)
r_squared_morts <- summary(model_morts)$r.squared

cat("Coeficient de determinació (Morts):", round(r_squared_morts, 3), "\n")
# Sortida: 0.006

library(ggplot2)

# Dades per a gràfic
df_r2 <- data.frame(
  Grup = c("Supervivents", "Morts"),
  R2 = c(r_squared_sup, r_squared_morts)
)

# Diagrama de barres
ggplot(df_r2, aes(x = Grup, y = R2, fill = Grup)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  labs(
    title = "Coeficient de Determinació (R²)",
    subtitle = "Variabilitat de Fare explicada per Age",
    x = "",
    y = "R²"
  ) +
  theme_minimal()
###############################
# Diagrames de residus

library(dplyr)

# 1. Residus pels Supervivents
residus_supervivents <- supervivents %>%
  select(Age, Fare) %>%
  na.omit() %>%
  mutate(
    Fare_Predich = predict(model_supervivents),
    Residu = Fare - Fare_Predich
  ) %>%
  rename(Edat = Age, Preu_Observat = Fare, Preu_Predit = Fare_Predich)

# Mostrar primeres files
cat("Taula de Residus - Supervivents:\n")
head(residus_supervivents)

# 2. Residus pels Morts
residus_morts <- morts %>%
  select(Age, Fare) %>%
  na.omit() %>%
  mutate(
    Fare_Predich = predict(model_morts),
    Residu = Fare - Fare_Predich
  ) %>%
  rename(Edat = Age, Preu_Observat = Fare, Preu_Predit = Fare_Predich)

# Mostrar primeres files
cat("\nTaula de Residus - Morts:\n")
head(residus_morts)

library(ggplot2)
library(gridExtra)

# Funció per crear diagrames de residus
plot_residus <- function(model, dades, grup, color) {
  # Afegir residus i valors ajustats al dataset
  dades$residus <- residuals(model)
  dades$ajustats <- predict(model)
  
  # Diagrama Residus vs Ajustats
  p1 <- ggplot(dades, aes(x = ajustats, y = residus)) +
    geom_point(color = color, alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(se = FALSE, color = "black") +
    labs(
      title = paste("Residus vs Ajustats -", grup),
      x = "Valors ajustats", 
      y = "Residus"
    ) +
    theme_minimal()
  
  # Diagrama Residus vs Age
  p2 <- ggplot(dades, aes(x = Age, y = residus)) +
    geom_point(color = color, alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(se = FALSE, color = "black") +
    labs(
      title = paste("Residus vs Edat -", grup),
      x = "Edat", 
      y = "Residus"
    ) +
    theme_minimal()
  
  # Retornar els dos gràfics
  grid.arrange(p1, p2, ncol = 2)
}

# Generar models
model_supervivents <- lm(Fare ~ Age, data = supervivents)
model_morts <- lm(Fare ~ Age, data = morts)

# Generar gràfics
plot_residus(model_supervivents, supervivents, "Supervivents", "#1b9e77")
plot_residus(model_morts, morts, "Morts", "#d95f02")
