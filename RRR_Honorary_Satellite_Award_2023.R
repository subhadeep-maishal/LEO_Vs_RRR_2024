# Load necessary libraries

#install.packages("generics")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("purrr")
#install.packages("tidyr")


library(ggplot2)
library(generics)
library(dplyr)
library(purrr)
library(tidyr)




# Load the CSV file
file_path <- 'C:/Users/Subhadeep Maishal/Music/JUP/test_data/CBN_INPUT.csv'
df <- read.csv(file_path)

# Exclude 'CONNET_CBN' column
df_long <- df %>%
  select(-CONNET_CBN) %>%
  pivot_longer(cols = -DATE, names_to = "Index", values_to = "Values", values_drop_na = TRUE)




#SELECT THE CODE AND RUN. OK.

# Plotting using time series
ggplot(df_long, aes(x = DATE, y = Values, color = Index, group = Index)) +
  geom_line(size = 0.5) +
  geom_point(size = 1, shape = 8, fill = "black") +
  
  labs(title = 'Time Series Plot Of oceanic indices example',
       x = 'DATE',
       y = 'Values') +
  
  scale_color_manual(values = c('AAO' = '#4e79a7', 'AO' = '#f28e2b', 'DMI' = '#e15759', 'MEIv2' = '#76b7b2',
                                'NAO' = '#59a14f', 'ONI' = '#edc949', 'PDO' = '#af7aa1', 'SOI' = '#ff9da7',
                                'TNA' = '#9c755f', 'TSA' = '#bab0ac', 'WHWP' = '#7f7f7f', 'WP' = '#000000')) +
  
  theme_minimal()





#remove line back
# Plotting using ggplot2 with points and smoother lines
ggplot(df_long, aes(x = DATE, y = Values, color = Index, group = Index)) +
  geom_line(size = 0.5, alpha = 0) +
  geom_point(size = 1, shape = 8, fill = "black") +
  
  labs(title = 'Time Series Plot Of oceanic indices example',
       x = 'DATE',
       y = 'Values') +
  
  scale_color_manual(values = c('AAO' = '#4e79a7', 'AO' = '#f28e2b', 'DMI' = '#e15759', 'MEIv2' = '#76b7b2',
                                'NAO' = '#59a14f', 'ONI' = '#edc949', 'PDO' = '#af7aa1', 'SOI' = '#ff9da7',
                                'TNA' = '#9c755f', 'TSA' = '#bab0ac', 'WHWP' = '#7f7f7f', 'WP' = '#000000')) +
  
  theme_minimal() +
  theme_void()









# Plotting box plots using ggplot2
ggplot(df_long, aes(x = Index, y = Values, fill = Index)) +
  geom_boxplot() +
  
  labs(title = 'Box Plot Of oceanic indices example',
       x = 'Index',
       y = 'Values') +
  
  scale_fill_manual(values = c('AAO' = '#4e79a7', 'AO' = '#f28e2b', 'DMI' = '#e15759', 'MEIv2' = '#76b7b2',
                               'NAO' = '#59a14f', 'ONI' = '#edc949', 'PDO' = '#af7aa1', 'SOI' = '#ff9da7',
                               'TNA' = '#9c755f', 'TSA' = '#bab0ac', 'WHWP' = '#7f7f7f', 'WP' = '#000000')) +
  
  theme_minimal()







# REALY YOU NEED that

# Plotting violin plot using ggplot2
ggplot(df_long, aes(x = Index, y = Values, fill = Index)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.2, fill = "white", color = "black") +
  
  labs(title = 'Violin Plot Of oceanic indices example',
       x = 'Index',
       y = 'Values') +
  
  scale_fill_manual(values = c('AAO' = '#4e79a7', 'AO' = '#f28e2b', 'DMI' = '#e15759', 'MEIv2' = '#76b7b2',
                               'NAO' = '#59a14f', 'ONI' = '#edc949', 'PDO' = '#af7aa1', 'SOI' = '#ff9da7',
                               'TNA' = '#9c755f', 'TSA' = '#bab0ac', 'WHWP' = '#7f7f7f', 'WP' = '#000000')) +
  
  theme_minimal()








#GREAT

#install.packages("networkD3")
library(networkD3)
#install.packages("corrplot")
library(corrplot)

# Load the CSV file
file_path <- 'C:/Users/Subhadeep Maishal/Music/JUP/test_data/CBN_INPUT.csv'
df <- read.csv(file_path)

# Select only the columns of interest
selected_cols <- df[, c('AAO', 'AO', 'DMI', 'MEIv2', 'NAO', 'ONI', 'PDO', 'SOI', 'TNA', 'TSA', 'WHWP', 'WP')]

# Calculate the correlation matrix
cor_matrix <- cor(selected_cols, method = "pearson")

# Display the correlation matrix
print(cor_matrix)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "color")





#YOU NEED THAT
# Customize color palette (you can choose your own)
color_palette <- colorRampPalette(c("#4575b4", "#91bfdb", "#313695"))(12)

# Visualize the correlation matrix with corrplot
corrplot(cor_matrix, 
         method = "color", 
         col = color_palette,   # Specify the color palette
         type = "lower",        # Display lower triangle only
         order = "hclust",      # Hierarchical clustering order
         tl.col = "black",      # Label color
         tl.srt = 45,            # Label rotation angle
         tl.cex = 0.8,           # Label size
         addCoef.col = "black",  # Coefficient color
         number.cex = 0.7,       # Coefficient size
         p.mat = cor_matrix,     # Show p-values
         sig.level = 0.01        # 
)

#HELLO, IT'S LOOKS PRETTY, you like sandwiches :)  ;-) 
#     BEST
#   Subhadeep
#Happy coding to you too! Have a wonderful day!






# START AGAIN

#install.packages("circlize")
library(circlize)
library(dplyr)

# Load the CSV file
# Load the CSV file
# Load the CSV file
# Load the CSV file
file_path <- 'C:/Users/Subhadeep Maishal/Music/JUP/test_data/CBN_INPUT.csv'
df <- read.csv(file_path)

# Select only the columns of interest
selected_cols <- df[, c('AAO', 'AO', 'DMI', 'MEIv2', 'NAO', 'ONI', 'PDO', 'SOI', 'TNA', 'TSA', 'WHWP', 'WP')]

# Calculate the correlation matrix with handling missing values
cor_matrix <- cor(selected_cols, method = "pearson", use = "pairwise.complete.obs")

# Convert correlation matrix to data frame for networkD3
cor_df <- as.data.frame(as.table(cor_matrix))
cor_df <- cor_df[cor_df$Var1 != cor_df$Var2, ]  # Exclude diagonal elements

# Create a chord diagram
chord_diagram <- chordDiagram(cor_df, transparency = 0.5)

# Display the chord diagram
chord_diagram






#WHICH ONE IS GOOD Monarch/
#ARE YOU "Royal HighnesS" ?.























