# Clear Global Environment #
rm(list = ls()) 

# Load necessary packages #
library(tidyverse)
library(vegan)
library(scatterplot3d)
library(vegan3d)
library(ggrepel)

# Load Data sets #
Surface_Sediments_2019_F4 <- read_csv("C:/Users/14128/Desktop/Osburn Lab/Analysis/2019S/Surface/2019_Surface_Sediments_F4.csv")
Surface_Sediments_2018_F4 <- read_csv("C:/Users/14128/Desktop/Osburn Lab/Analysis/2018S and 2019W/Surface/Quantification/2018_Surface_Sediments_F4.csv")
Surface_Brines_2019_F4 <- read_csv("C:/Users/14128/Desktop/Osburn Lab/Analysis/2019S/Surface/2019_Surface_Brines_F4.csv")
Surface_Brines_2018_F4 <- read_csv("C:/Users/14128/Desktop/Osburn Lab/Analysis/2018S and 2019W/Surface/Quantification/2018_Surface_Brines_F4.csv")
Surface_Salt_2019_F4 <- read_csv("C:/Users/14128/Desktop/Osburn Lab/Analysis/2019S/Surface/2019_Surface_Salt_F4.csv")
Surface_Mats_2018_F4 <- read_csv("C:/Users/14128/Desktop/Osburn Lab/Analysis/2018S and 2019W/Surface/Quantification/2018_Surface_Mats_F4.csv")
Surface_Mats_2019_F4 <- read_csv("C:/Users/14128/Desktop/Osburn Lab/Analysis/2019S/Surface/2019_Surface_Mats_F4.csv")
Surface_Sediments_Alkanes <- read_csv("C:/Users/14128/Desktop/Osburn Lab/Analysis/2019S/Surface/Surface_Sediments_Alkanes.csv") %>% 
  filter(Category != "PAIBE" & Category != "Unknown" & Category != "FAME" & Category != "Ester")

# Lake Data #
All_Lake_Data <- read_csv("C:/Users/14128/Desktop/Osburn Lab/Analysis/All_Lake_Data.csv")
Surface <- All_Lake_Data %>%  # Subset data set into surface
  filter(Category == "Surface") %>% 
  filter(Type == "Brine" | Type == "Ice" | Type == "Mat" | Type == "Salt" | Type == "Sediment")

# Merge Data frames #
Compiled_df <- full_join(Surface_Sediments_2019_F4, Surface_Sediments_2018_F4) %>%  
  full_join(Surface_Brines_2019_F4) %>%  
  full_join(Surface_Brines_2018_F4) %>% 
  full_join(Surface_Salt_2019_F4) %>% 
  full_join(Surface_Mats_2018_F4) %>% 
  full_join(Surface_Mats_2019_F4) %>% 
  full_join(Surface_Sediments_Alkanes) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(SLSL_Spring, SLSU_Spring, SLSL_Sediment, SLSU_Sediment))

# Filter out Unnecessary Categories of Lipids #
Compiled_wide <- Compiled_df %>% 
  filter(Category != "Unknown" & Category != "Aromatic" & Category != "PAH" & Category != "Ketone" & Category != "Organosulfur" & Category != "Acetate" & Category != "Standard" & Category != "?" & Category != "Dimethoxy" & Category != "Siloxane" & Category != "Unkown" & Category != "Underivatized FAME" & Category != "Alkane" & Category != "Sterol")

# Convert to Long Format #
Compiled_long <- gather(Compiled_wide, Sample, Quantification, `BL1-1_Sediment`:`BL2-1_Brine`, factor_key = TRUE) %>% 
  select(-"Compound") 

Compiled_sum <- aggregate(Quantification ~ ., Compiled_long, sum) # Sum the total of each category

# Convert back to wide format #
Compiled_wide2 = Compiled_sum %>% 
  spread(Category, Quantification) %>% 
  column_to_rownames("Sample")

# Compute NMDS #
set.seed(123)
Compiled_wide_relative <- decostand(Compiled_wide2, method = "total") # Convert to Relative Abundance
nmds <- metaMDS(Compiled_wide_relative, 
                distance = "bray", 
                k = 4, 
                trymax = 250, 
                maxit = 999)
plot(nmds)

# Shepards test/goodness of fit
goodness(nmds) # Produces a results of test statistics for goodness of fit for each point
stressplot(nmds) # Produces a Shepards diagram

# Compute Vectors #
(fit <- envfit(nmds, Compiled_wide2, perm = 999))
scores(fit, "vectors")
plot(fit, p.max = 0.01, col = "red")
vectors <- as.data.frame(scores(fit, display = "vectors"))
vectors <- cbind(vectors, Compound = rownames(vectors))

# Calculate Data Scores #
data.scores = as.data.frame(scores(nmds)) %>%  
  rownames_to_column(var = "Sample") %>%  
  left_join(Surface, by = "Sample")

# Manual Colors #
Colors <- c("Basque Lake 1" = "orange2", "Basque Lake 2" = "red1", "Basque Lake 4" = "red4", "Last Chance Lake" = "yellow", "Salt Lake" = "blue4")
# Manual Shapes #
Shapes <- c("Brine" = 21, "Ice" = 24, "Mat" = 22, "Salt" = 23, "Sediment" = 25)

# Visualize NMDS #
Surface_NMDS <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(shape = Type, fill = Lake), size = 7) + 
  geom_segment(data = vectors,
               aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
  geom_text(data = vectors, aes(x = NMDS1, y = NMDS2, label = Compound),
            size = 4) +
  scale_fill_manual(values = Colors, 
                    guide = guide_legend(override.aes = list(shape = 21))) +
  scale_shape_manual(values = Shapes) +
  theme_bw() + 
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16))
plot(Surface_NMDS)
