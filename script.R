# your_script.R
library(dplyr)

# Créez un exemple de dataframe
df <- data.frame(x = 1:10, y = rnorm(10))

# Écrivez le dataframe dans un fichier CSV
write.csv(df, "results.csv", row.names = FALSE)

print("Fichier CSV généré : output/results.csv")
