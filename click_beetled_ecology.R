#################################


###########ECOLOGY ELATERIDAE USING PICS#######
rm(list = ls())  # Removes all variables and objects
library(dplyr)
library(vegan)
library(ggplot2)
library(patchwork)
library(tibble)
library(iNEXT)
library(ggrepel)
library(knitr)
##UPLOAD CSV


##morphospecies

df <- read.csv("morphospecies_count_FINAL.csv")


#total number of beetles####

total_beetles <- df %>%
  summarise(total = sum(TOTAL_NUMBER_BEETLES, na.rm = TRUE))  ## 5848

initial_total_beetles <- df %>%
  summarise(total = sum(initial_total_number, na.rm = TRUE))  ## 


total_beetles <- df %>%
  summarise(total = sum(TOTAL_NUMBER_BEETLES, na.rm = TRUE))  ## 5848

print(initial_total_beetles)



print(total_beetles)

#total number of Elateridae#### 

unique(df$N_ELATERIDAE[!grepl("^\\d+$", df$ELATERIDAE)])


final_elaterids <- df %>%
  summarise(total = sum(as.numeric(check_count), na.rm = TRUE)) ## to convert the column to numeric before summing

print(final_elaterids) ### total 559 

initial_elaterids <- df %>%
  summarise(total = sum(as.numeric(initial_elateridae), na.rm = TRUE)) ## to convert the column to numeric before summing

print(initial_elaterids)


##per site info 

site_number <- df %>%
  mutate(ELATERIDAE_num = as.numeric(check_count)) %>%
  group_by(SITE) %>%
  summarise(total_elaterids = sum(ELATERIDAE_num, na.rm = TRUE))
print(site_number)



###make Data for DAMPA and MURLEN + AIZAWL
dampa <- df %>%
   filter(SITE == "Dampa")

murlen <- df %>%
  filter(SITE == "Murlen")

aizawl <- df %>%
  filter(SITE == "Aizawl")


###analyse the plots per site

dampa_plots <- dampa %>%
  mutate(ELATERIDAE_num = as.numeric(check_count)) %>%
  group_by(PLOT) %>%
  summarise(total_elaterids = sum(ELATERIDAE_num, na.rm = TRUE))

print(dampa_plots)



murlen_plots <- murlen %>%
  mutate(ELATERIDAE_num = as.numeric(check_count)) %>%
  group_by(PLOT) %>%
  summarise(total_elaterids = sum(ELATERIDAE_num, na.rm = TRUE))
print(murlen_plots)


####  PLOT THE INFO
# Filter to only keep plots with >= 1 individual
dampa_filtered <- dampa_plots %>% filter(total_elaterids >= 1)
murlen_filtered <- murlen_plots %>% filter(total_elaterids >= 1)

base_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Dampa plot
p_dampa <- ggplot(dampa_filtered, aes(x = PLOT, y = total_elaterids)) +
  geom_col(fill = "steelblue") +
  labs(x = "Dampa Plot", y = "Total individuals") +
  ylim(0, 160) +
  base_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 0, vjust = 1, hjust = 0.5),
    axis.title.x = element_text(size = 16)  # optional: bigger x-axis title
  )


# Murlen plot
p_murlen <- ggplot(murlen_filtered, aes(x = PLOT, y = total_elaterids)) +
  geom_col(fill = "forestgreen") +
  labs(x = "Murlen Plot", y = "Total individuals") +
  ylim(0, 160) +
  base_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 0, vjust = 1, hjust = 0.5),
    axis.title.x = element_text(size = 16)  # optional: bigger x-axis title
  )
  


# Combine side by side
t_elateridae <- p_dampa + p_murlen

t_elateridae
ggsave(
  filename = "dampa_murlen_total.png",
  plot     = t_elateridae,
  width    = 14,   # give more horizontal room
  height   = 7,
  units    = "in",
  dpi      = 300
)





#assess accuracy of sample completeness suggested by Chao & Josh ######

install.packages("iNEXT")

library(iNEXT)
library(tibble)


###First overall view Dampa vs Murlen vs Aizawl###

species_dampa <- dampa %>%
  filter(PLOT != "") %>% 
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), sum, na.rm = TRUE))

print(species_dampa)
species_murlen <- murlen  %>%
  filter(PLOT != "") %>% 
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), sum, na.rm = TRUE))

species_aizawl <- aizawl  %>%
  filter(PLOT != "") %>% 
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), sum, na.rm = TRUE))

dampa_summary <- species_dampa %>%
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

murlen_summary <- species_murlen %>%
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), ~ sum(.x, na.rm = TRUE)), .groups = "drop")


aizawl_summary <- species_aizawl %>%
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), ~ sum(.x, na.rm = TRUE)), .groups = "drop")


# Sum species across all Dampa plots
dampa_site <- dampa_summary %>%
  select(starts_with("s_")) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  unlist()  # convert to vector

# Sum species across all Murlen plots
murlen_site <- murlen_summary %>%
  select(starts_with("s_")) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  unlist()


aizawl_site <- aizawl_summary %>%
  select(starts_with("s_")) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  unlist()

#RAREFACTION AND EXTRAPOLATION####

all_sites <- list(
  Dampa = dampa_site,
  Murlen = murlen_site,
  Aizawl = aizawl_site
)
# Run iNEXT analysis for diversity orders 0, 1, 2
iNEXT_out <- iNEXT(all_sites, q = c(0, 1, 2), datatype = "abundance")

# Collapse species counts across all samples per site:
aizawl_abund <- colSums(aizawl %>% select(starts_with("s_")))

# Create named list for iNEXT
all_sites_list <- list(Dampa = dampa_abund, Murlen = murlen_abund, Aizawl = aizawl_abund)

iNEXT_all_sites <- iNEXT(all_sites_list, q = 0, datatype = "abundance")
ggiNEXT(iNEXT_all_sites, type = 1) 


rar_extr <- ggiNEXT(iNEXT_all_sites, type = 1) +
  labs(
    x = "Total Elateridae Beetles Sampled",
    y = "Total Elateridae Morphospecies Diversity"
  ) +
  theme_minimal()

rar_extr


ggsave(
  filename = "rar_extr.png",
  plot     = rar_extr,
  width    = 9,   # give more horizontal room
  height   = 5,
  units    = "in",
  dpi      = 300
)



########### DAMPA accuracy check ####  

# Each row = a plot, each column = species count
dampa_matrix <- species_dampa %>%
  as.data.frame() %>%
  column_to_rownames("PLOT") %>%
  t()

##abundance vectors, one per plot
dampa_list <- as.list(as.data.frame(dampa_matrix))  # list of columns
names(dampa_list) <- colnames(dampa_matrix)



# Estimate sample completeness and diversity
dampa_sampling <- iNEXT(dampa_list, q = c(0, 1, 2), datatype = "abundance")

dampa_plots <- ggiNEXT(dampa_sampling, type = 2)  # Diversity vs. sample size


ggiNEXT(dampa_sampling, type = 3) # Sample coverage vs. sample size


# View estimated diversity and coverage
out$AsyEst  # Asymptotic estimates



##############  MURLEN accuray check ###########




murlen_matrix <- species_murlen %>%
  as.data.frame() %>%
  column_to_rownames("PLOT") %>%
  t()

##abundance vectors, one per plot
murlen_list <- as.list(as.data.frame(murlen_matrix))  # list of columns
names(murlen_list) <- colnames(murlen_matrix)


# Estimate sample completeness and diversity
out_murlen <- iNEXT(murlen_list, q = c(0, 1, 2), datatype = "abundance")

# Plot: Diversity vs. sample size
murlen_plots <- ggiNEXT(out_murlen, type = 2)

murlen_plots
plots <- dampa_plots + murlen_plots


plots


# Make your ggplots from iNEXT objects
p_dampa  <- ggiNEXT(dampa_sampling,  type = 2) +
  theme_minimal(base_size = 12)

p_murlen <- ggiNEXT(out_murlen, type = 2) +
  theme_minimal(base_size = 12)

# Add in-panel labels and a little extra space at the top
p_dampa_lab <- p_dampa +
  coord_cartesian(ylim = c(0, 1.05), clip = "off") +           # headroom above 1.00
  annotate("text", x = -Inf, y = Inf, label = "a) Dampa",
           hjust = -0.05, vjust = 1.6, fontface = "bold", size = 4) +
  theme(plot.margin = margin(t = 10, r = 8, b = 6, l = 12))

p_murlen_lab <- p_murlen +
  coord_cartesian(ylim = c(0, 1.05), clip = "off") +
  annotate("text", x = -Inf, y = Inf, label = "b) Murlen",
           hjust = -0.05, vjust = 1.6, fontface = "bold", size = 4) +
  theme(plot.margin = margin(t = 10, r = 12, b = 6, l = 8))

# Side-by-side
plots <- p_dampa_lab + p_murlen_lab



ggsave(
  filename = "rar_extr_plots.png",
  plot     = plots,
  width    = 10,   # give more horizontal room
  height   = 5,
  units    = "in",
  dpi      = 300
)




##############################################################################
################ G A M M A     D I V E R S I T Y   #######################

###INCLUDES species richness, Shannon and simpson in hills numbers ####


##a widely used measure of biodiversity that considers both the number of species (richness) and their relative abundance (evenness) in a community.
##higher shannon indicates greater diversity


library(iNEXT)


##Include aizawl
aizawl <- df %>%
  filter(SITE == "Aizawl")


species_aizawl <- aizawl %>%
  filter(PLOT != "") %>%  # remove blank PLOTs
  select(PLOT, num_range("s_", 1:94)) %>%
  filter(rowSums(across(num_range("s_", 1:94))) > 0)  # remove rows with 0 total count


aizawl_summary <- species_aizawl %>%
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), ~ sum(.x, na.rm = TRUE)), .groups = "drop")


aizawl_matrix <- species_aizawl %>%
  select(starts_with("s_")) %>%
  as.data.frame()


str(dampa_list)
str(murlen_list)
# First, ensure you're working with the full species abundance matrix
# Each row is a plot, each column is a morphospecies (starts with "s_")
dampa_matrix <- dampa_summary %>%
  select(starts_with("s_")) %>%
  as.data.frame()

murlen_matrix <- murlen_summary %>%
  select(starts_with("s_")) %>%
  as.data.frame()


# Sum across ALL plots in each site to get total abundance per morphospecies
dampa_total <- colSums(dampa_matrix)
murlen_total <- colSums(murlen_matrix)
aizawl_total <- colSums(aizawl_matrix)

# Create combined list
combined_sites <- list(
  Dampa = dampa_total,
  Murlen = murlen_total,
  Aizawl = aizawl_total
)

# Now run iNEXT
output_sites <- iNEXT(combined_sites, q = c(0, 1, 2), datatype = "abundance")

print(output_sites)

# Extract the asymptotic estimates
asymptotic_table <- output_sites$AsyEst
print(asymptotic_table)




output_sites_estimates <- output_sites$AsyEst


p7 <- ggplot(filter(output_sites_estimates, Diversity == "Species richness"),
             aes(x = Assemblage, y = Observed, fill = Observed)) +
  geom_col() +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  scale_fill_gradient(low = "purple", high = "orange") +
  labs(x = "Site", y = "Morphospecies Richness", ,fill = "Total number") +
  theme_minimal()



p8 <- ggplot(filter(output_sites_estimates, Diversity == "Shannon diversity"),
             aes(x = Assemblage, y = Observed, fill = Observed)) +
  geom_col() +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  scale_fill_gradient(low = "purple", high = "orange") +
  labs(x = "Site", y = "Shannon diversity", fill = "Value") +
  theme_minimal()

p9 <- ggplot(filter(output_sites_estimates, Diversity == "Simpson diversity"),
             aes(x = Assemblage, y = Observed, fill = Observed)) +
  geom_col() +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  scale_fill_gradient(low = "purple", high = "orange") +
  labs(x = "Site", y = "Simpson diversity", fill = "Value") +
  theme_minimal()


gamma <- p7 + p8 +p9


ggsave(
  filename = "gamma.png",
  plot     = gamma,
  width    = 10,   # give more horizontal room
  height   = 5,
  units    = "in",
  dpi      = 300
)



######################################################################
############### PLOTS     SHANNON DIVERISITY##################

# Extract abundance matrix (morphospecies columns)
murlen_matrix <- murlen_summary %>%
  select(starts_with("s_"))

# Make a list of abundance vectors by plot
murlen_list_plots <- split(murlen_matrix, murlen_summary$PLOT)
murlen_list_plots <- lapply(murlen_list, colSums)

# Do the same for Dampa
dampa_matrix <- dampa_summary %>%
  select(starts_with("s_"))

dampa_list <- split(dampa_matrix, dampa_summary$PLOT)
dampa_list <- lapply(dampa_list, colSums)

# For Dampa
out_dampa_plots <- iNEXT(dampa_list, q = c(0, 1, 2), datatype = "abundance")

# For Murlen
out_murlen_plots <- iNEXT(murlen_list, q = c(0, 1, 2), datatype = "abundance")


# View results
out_dampa_plots$AsyEst
out_murlen_plots$AsyEst


# Extract the asymptotic diversity estimates
asy_table <- out_dampa_plots$AsyEst

# Format as a nice table
kable(asy_table, caption = "Asymptotic Diversity Estimates (iNEXT)")


#####GRAPHSSS###


dampa_estimates <- out_dampa_plots$AsyEst

dampa_estimates$Assemblage <- factor(dampa_estimates$Assemblage,
                                     levels = c("plot 1", "plot 2", "plot 3", "plot 4"),
                                     labels = c("1", "2", "3", "4")
)

p1 <- ggplot(filter(dampa_estimates, Diversity == "Species richness"),
             aes(x = Assemblage, y = Observed, fill = Observed)) +
  geom_col() +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  scale_fill_gradient(low = "darkgoldenrod1", high = "coral") +
  labs(x = "Plot", y = "Morphospecies Richness", ,fill = "Total number") +
  theme_minimal()



p2 <- ggplot(filter(dampa_estimates, Diversity == "Shannon diversity"),
             aes(x = Assemblage, y = Observed, fill = Observed)) +
  geom_col() +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  scale_fill_gradient(low = "darkgoldenrod1", high = "coral") +
  labs(x = "Plot", y = "Shannon diversity", fill = "Value") +
  theme_minimal()

p3 <- ggplot(filter(dampa_estimates, Diversity == "Simpson diversity"),
             aes(x = Assemblage, y = Observed, fill = Observed)) +
  geom_col() +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  scale_fill_gradient(low = "darkgoldenrod1", high = "coral") +
  labs(x = "Plot", y = "Simpson diversity", fill = "Value") +
  theme_minimal()


p1 + p2 +p3


#NO LIGHT TRAP IN THE ANALYSIS####
##RAREFACTION
# 1) Remove light-trap samples
dampa_nolt <- dampa %>% filter(!is.na(PLOT), PLOT != "", tolower(TRAP) != "light trap")

# 3) Sum to plot level (species × plot matrix)
dampa_no_light <- dampa_nolt %>%
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  column_to_rownames("PLOT") %>%
  t() %>% as.data.frame()

# 4) Drop empty plots and run iNEXT
dampa_no_light <- dampa_no_light[, colSums(dampa_no_light) > 0, drop = FALSE]
out_dampa_no_light <- iNEXT(dampa_no_light, q = c(0,1,2), datatype = "abundance")
d_no_light <- ggiNEXT(out_dampa_no_light, type = 2); ggiNEXT(dampa_no_light, type = 3)


##GAMMA DIVERISTY
dampa_estimates_no_light <- out_dampa_no_light$AsyEst

dampa_estimates_no_light$Assemblage <- factor(dampa_estimates_no_light$Assemblage,
                                     levels = c("plot 1", "plot 2", "plot 3", "plot 4"),
                                     labels = c("1", "2", "3", "4")
)

p1 <- ggplot(filter(dampa_estimates_no_light, Diversity == "Species richness"),
             aes(x = Assemblage, y = Observed, fill = Observed)) +
  geom_col() +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  scale_fill_gradient(low = "darkgoldenrod1", high = "coral") +
  labs(x = "Plot", y = "Morphospecies Richness", ,fill = "Total number") +
  theme_minimal()



p2 <- ggplot(filter(dampa_estimates_no_light, Diversity == "Shannon diversity"),
             aes(x = Assemblage, y = Observed, fill = Observed)) +
  geom_col() +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  scale_fill_gradient(low = "darkgoldenrod1", high = "coral") +
  labs(x = "Plot", y = "Shannon diversity", fill = "Value") +
  theme_minimal()

p3 <- ggplot(filter(dampa_estimates_no_light, Diversity == "Simpson diversity"),
             aes(x = Assemblage, y = Observed, fill = Observed)) +
  geom_col() +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  scale_fill_gradient(low = "darkgoldenrod1", high = "coral") +
  labs(x = "Plot", y = "Simpson diversity", fill = "Value") +
  theme_minimal()


dampa_gamma_no_light <- p1 + p2 +p3

ggsave(
  filename = "dampa_gamma_no_light.png",
  plot     = dampa_gamma_no_light,
  width    = 10,   # give more horizontal room
  height   = 5,
  units    = "in",
  dpi      = 300
)

###MURLEN NO LIGHT NO CORE PLOT


# filter out light traps + "plot core forest"
murlen_filt <- murlen %>%
  filter(!is.na(PLOT),
         PLOT != "",
         PLOT != "plot core forest",
         tolower(TRAP) != "light trap")



# 3) Sum to plot level (species × plot matrix)
# sum species counts per plot
murlen_comm <- murlen_filt %>%
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  column_to_rownames("PLOT") %>%
  t() %>% as.data.frame()


dampa_no_light <- dampa_nolt %>%
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  column_to_rownames("PLOT") %>%
  t() %>% as.data.frame()

# 4) Drop empty plots and run iNEXT
dampa_no_light <- dampa_no_light[, colSums(dampa_no_light) > 0, drop = FALSE]
out_dampa_no_light <- iNEXT(dampa_no_light, q = c(0,1,2), datatype = "abundance")
d_no_light <- ggiNEXT(out_dampa_no_light, type = 2); ggiNEXT(dampa_no_light, type = 3)



# drop plots with no individuals
murlen_no_light <- murlen_comm[, colSums(murlen_comm) > 0, drop = FALSE]

out_murlen_no_light <- iNEXT(murlen_no_light, q = c(0,1,2), datatype = "abundance")

m_no_light <- ggiNEXT(out_murlen_no_light, type = 2); ggiNEXT(out_murlen_no_light, type = 3)
m_no_light


p_dampa_lab <- d_no_light +
  coord_cartesian(ylim = c(0, 1.05), clip = "off") +           # headroom above 1.00
  annotate("text", x = -Inf, y = Inf, label = "a) Dampa",
           hjust = -0.05, vjust = 1.6, fontface = "bold", size = 4) +
  theme(plot.margin = margin(t = 10, r = 8, b = 6, l = 12))

p_murlen_lab <- m_no_light +
  coord_cartesian(ylim = c(0, 1.05), clip = "off") +
  annotate("text", x = -Inf, y = Inf, label = "b) Murlen",
           hjust = -0.05, vjust = 1.6, fontface = "bold", size = 4) +
  theme(plot.margin = margin(t = 10, r = 12, b = 6, l = 8))

# Side-by-side
plots_n_L <- p_dampa_lab + p_murlen_lab



plots_n_L

ggsave(
  filename = "no_light_rar_ext.png",
  plot     = plots_n_L,
  width    = 14,   # give more horizontal room
  height   = 7,
  units    = "in",
  dpi      = 300
)




##murlen no light gamma diveristy

murlen_estimates <- out_murlen_no_light$AsyEst

murlen_estimates$Assemblage <- factor(murlen_estimates$Assemblage,
                                      levels = c("plot 1", "plot 2", "plot 3", "plot 4"),
                                      labels = c("1", "2", "3", "4")
)


p4 <- ggplot(filter(murlen_estimates, Diversity == "Species richness"),
             aes(x = Assemblage, y = Observed, fill = Observed)) +
  geom_col() +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  scale_fill_gradient(low = "darkseagreen2", high = "cornflowerblue") +
  labs(x = "Plot", y = "Morphospecies Richness", ,fill = "Total Number") +
  theme_minimal()


p5 <- ggplot(filter(murlen_estimates, Diversity == "Shannon diversity"),
             aes(x = Assemblage, y = Observed, fill = Observed)) +
  geom_col() +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  scale_fill_gradient(low = "darkseagreen2", high = "cornflowerblue") +
  labs(x = "Plot", y = "Shannon Diveristy", fill = "Value") +
  theme_minimal()

p6 <- ggplot(filter(murlen_estimates, Diversity == "Simpson diversity"),
             aes(x = Assemblage, y = Observed, fill = Observed)) +
  geom_col() +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  scale_fill_gradient(low = "darkseagreen2", high = "cornflowerblue") +
  labs(x = "Plot", y = "Simpson diversity", fill = "Value") +
  theme_minimal() 


murlen_no_L <- p4 + p5 +p6
murlen_no_L

ggsave(
  filename = "no_light_murlent.png",
  plot     = murlen_no_L,
  width    = 10,   # give more horizontal room
  height   = 5,
  units    = "in",
  dpi      = 300
)





###beta diversity : difference in species composition between different ecological habitats


###comparing 3 different sites :Dampa, Murlen, Aizawl 

##select the columns needed 

species_data <- df %>%
  filter(SITE != "") %>%  # remove blank SITE
  dplyr::select(SITE, num_range("s_", 1:94))   ##this select a range of columns

##aggregate species per site

species_per_site <- species_data %>%
  group_by(SITE) %>%
  summarise(across(starts_with("s_"), \(x) sum(x, na.rm = TRUE)), .groups = 'drop')


##remove the habitat column for Bray-curtis computation

species_matrix <- species_per_site %>% 
  dplyr::select(-SITE)

# Calculate Bray-Curtis dissimilarity (beta diversity) between habitats
beta_diversity <- vegdist(species_matrix, method = "bray")

print(beta_diversity)
# Convert the dissimilarity object to a matrix for easier manipulation
bray_curtis_matrix <- as.matrix(beta_diversity)

# View the Bray-Curtis pairwise dissimilarity matrix: 0= 2 sites have same composition 1= 2 sites do NOT share any species
print(bray_curtis_matrix)
View(bray_curtis_matrix)


#######################################################################################
#################analyse the morphospecies per site and per plot


library(dplyr)
##filter the csv per site

dampa <- df %>%
  filter(SITE == "Dampa")

murlen <- df %>%
  filter(SITE == "Murlen")


#################### ### # # #DAMPAAA # # # # # 
##select the columns needed in this case PLOT

species_dampa <- dampa %>%
  filter(PLOT != "") %>%  # remove blank SITE
  dplyr::select(PLOT, num_range("s_", 1:94))   ##this select a range of columns


##aggregate species per PLOT

species_dampa_plot <- species_dampa %>%
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), \(x) sum(x, na.rm = TRUE)), .groups = 'drop')


##remove the PLOT column for Bray-curtis computation

species_dampa_plot_matrix <- species_dampa_plot %>% 
  dplyr::select(-PLOT)

# Calculate Bray-Curtis dissimilarity (beta diversity) between PLOTS in DAMPA
species_dampa_plot_matrix <- species_dampa_plot_matrix[rowSums(species_dampa_plot_matrix) > 0, ]

bray_DAMPA <- vegdist(species_dampa_plot_matrix, method = "bray")


print(bray_DAMPA)

# Convert the dissimilarity object to a matrix for easier manipulation
bray_curtis_dampa <- as.matrix(bray_DAMPA)

# View the Bray-Curtis pairwise dissimilarity matrix: 0= 2 sites have same composition 1= 2 sites do NOT share any species
print(bray_curtis_dampa)

########################### # # # # # MURLEN # # ## # #


##select the columns needed in this case PLOT

species_murlen <- murlen %>%
  filter(PLOT != "") %>%  # remove blank PLOTs
  dplyr::select(PLOT, num_range("s_", 1:94)) %>%
  filter(rowSums(across(num_range("s_", 1:94))) > 0)  # remove rows with 0 total count

##aggregate species per PLOT

species_murlen_plot <- species_murlen %>%
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), \(x) sum(x, na.rm = TRUE)), .groups = 'drop')


##remove the habitat column for Bray-curtis computation

species_murlen_plot_matrix <- species_murlen_plot %>% 
  dplyr::select(-PLOT)

# Calculate Bray-Curtis dissimilarity (beta diversity) between PLOTS in MURLEN
species_murlen_plot_matrix <- species_murlen_plot_matrix[rowSums(species_murlen_plot_matrix) > 0, ]

bray_MURLEN <- vegdist(species_murlen_plot_matrix, method = "bray")


print(bray_MURLEN)

# Convert the dissimilarity object to a matrix for easier manipulation
bray_curtis_murlen <- as.matrix(bray_MURLEN)

# View the Bray-Curtis pairwise dissimilarity matrix: 0= 2 sites have same composition 1= 2 sites do NOT share any species
print(bray_curtis_murlen)




###############################################
#non-metric multidimensional scaling (NMDS)####### identify community relationship ####

###Dampa

##remove the plot and create a matrix


species_dampa_plot_clean_matrix <- species_dampa_plot %>% 
  dplyr::select(-PLOT)

# Keep plot IDs separate
plot_ids <- species_dampa_plot$PLOT


# Run NMDS (Bray-Curtis is common for species data)
set.seed(123)  # reproducibility
nmds <- metaMDS(species_dampa_plot_clean_matrix, distance = "bray", k = 2, trymax = 100)

# Extract NMDS scores (sites)
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))
nmds_scores$PLOT <- plot_ids

# Extract species scores (vectors)
species_scores <- as.data.frame(scores(nmds, display = "species"))
species_scores$species <- rownames(species_scores)
# Remove rows with NA or NaN
species_scores_clean <- species_scores[complete.cases(species_scores), ]



##check stress 
nmds$stress


plot_colors <- c("blue", "green", "orange", "brown")

# Plot NMDS biplot
dampa_NMDS1 <- ggplot() +
  # Points for plots
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2), color = plot_colors,  size = 5) +
  geom_text_repel(data = nmds_scores, aes(x = NMDS1, y = NMDS2, label = PLOT), size = 4.5, fontface = "bold") +
  
  # Arrows for species
  geom_segment(data = species_scores_clean,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(data = species_scores_clean, aes(x = NMDS1, y = NMDS2, label = species),
                  color = "black", size = 4) +
  
  theme_minimal() +
  coord_equal()


dampa_NMDS1


### Label only the “important” species (e.g., farthest from origin)

sp <- species_scores_clean
sp$len <- sqrt(sp$NMDS1^2 + sp$NMDS2^2)

top <- sp %>% slice_max(len, n = 20)  # label top 20 by vector length

dampa_NMDS <- ggplot() +
  # Points for plots
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2), color = plot_colors,  size = 5) +
  geom_text_repel(data = nmds_scores, aes(x = NMDS1, y = NMDS2, label = PLOT), size = 4.5, fontface = "bold") +
  
  # Arrows for species
  geom_segment(data = species_scores_clean,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(data = top, aes(NMDS1, NMDS2, label = species),
                  max.overlaps = Inf, size = 4)

dampa_NMDS


###B) Collapse duplicates and label once (show how many species share that spot)


sp_collapsed <- species_scores_clean %>%
  group_by(NMDS1, NMDS2) %>%
  summarise(label = paste(species, collapse = ", "),
            n = n(), .groups = "drop") %>%
  mutate(label = ifelse(n > 1, paste0(label, " (", n, ")"), label))

ggplot() +
  # Points for plots
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2), color = plot_colors,  size = 5) +
  geom_text_repel(data = nmds_scores, aes(x = NMDS1, y = NMDS2, label = PLOT), size = 3) +
  
  # Arrows for species
  geom_segment(data = species_scores_clean,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(data = sp_collapsed, aes(NMDS1, NMDS2, label = label),
                  max.overlaps = Inf, size = 3)


###################################
#####################################
###########   MURLEN ################


# Remove PLOT column for distance calculation  --already done in species_dampa_plot_matrix

##filter species with NA



##remove the plot and create a matrix


species_murlen_plot_clean_matrix <- species_murlen_plot %>% 
  dplyr::select(-PLOT)

# Keep plot IDs separate
plot_ids_murlen <- species_murlen_plot$PLOT


# Run NMDS (Bray-Curtis is common for species data)
set.seed(123)  # reproducibility
nmds_murlen <- metaMDS(species_murlen_plot_clean_matrix, distance = "bray", k = 2, trymax = 100)

# Extract NMDS scores (sites)
nmds_scores_murlen <- as.data.frame(scores(nmds_murlen, display = "sites"))
nmds_scores_murlen$PLOT <- plot_ids_murlen

# Extract species scores (vectors)
species_scores_murlen <- as.data.frame(scores(nmds_murlen, display = "species"))
species_scores_murlen$species <- rownames(species_scores_murlen)
# Remove rows with NA or NaN
species_scores_clean_murlen <- species_scores_murlen[complete.cases(species_scores_murlen), ]


plot_colors_murlen <- c("blue", "green", "orange", "violet", "turquoise", "darkgreen")

# Plot NMDS biplot
murlen_NMDS <- ggplot() +
  # Points for plots
  geom_point(data = nmds_scores_murlen, aes(x = NMDS1, y = NMDS2), color = plot_colors_murlen,  size = 6) +
  geom_text_repel(data = nmds_scores_murlen, aes(x = NMDS1, y = NMDS2, label = PLOT), size = 5, fontface = "bold") +
  
  # Arrows for species
  geom_segment(data = species_scores_clean_murlen,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(data = species_scores_clean_murlen, aes(x = NMDS1, y = NMDS2, label = species),
                  color = "black", size = 4) +
  
  theme_minimal() +
  coord_equal()
murlen_NMDS 

##check stress 
nmds_murlen$stress

murlen_NMDS
NMDS <- dampa_NMDS1 + murlen_NMDS
NMDS

ggsave(
  filename = "NMDS_murlen_Dampa_new1.png",
  plot     = NMDS,
  width    = 13,   # give more horizontal room
  height   = 5,
  units    = "in",
  dpi      = 300
)




### Label only the “important” species 

sp_murlen <- species_scores_clean_murlen
sp$len <- sqrt(sp$NMDS1^2 + sp$NMDS2^2)

top_murlen <- sp %>% slice_max(len, n = 20)  # label top 20 by vector length

ggplot() +
  # Points for plots
  geom_point(data = nmds_scores_murlen, aes(x = NMDS1, y = NMDS2), color = plot_colors_murlen,  size = 6) +
  geom_text_repel(data = nmds_scores_murlen, aes(x = NMDS1, y = NMDS2, label = PLOT), size = 8) +
  
  # Arrows for species
  geom_segment(data = species_scores_clean_murlen,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(data = top, aes(NMDS1, NMDS2, label = species),
                  max.overlaps = Inf, size = 4)




###B) Collapse duplicates and label once (show how many species share that spot)


sp_collapsed_murlen <- species_scores_clean_murlen %>%
  group_by(NMDS1, NMDS2) %>%
  summarise(label = paste(species, collapse = ", "),
            n = n(), .groups = "drop") %>%
  mutate(label = ifelse(n > 1, paste0(label, " (", n, ")"), label))


sp_collapsed_murlen1 <- sp %>%
  group_by(NMDS1, NMDS2) %>%
  summarise(species_list = sort(species), n = n(), .groups = "drop") %>%
  mutate(
    label = ifelse(
      n > 5,
      paste0(species_list[1], "–", species_list[n], " (", n, ")"),  # collapsed
      paste(species_list, collapse = ", ")                          # just list them
    )
  )




ggplot() +
  # Points for plots
  geom_point(data = nmds_scores_murlen, aes(x = NMDS1, y = NMDS2), color = plot_colors_murlen,  size = 6) +
  geom_text_repel(data = nmds_scores_murlen, aes(x = NMDS1, y = NMDS2, label = PLOT), size = 7) +
  
  # Arrows for species
  geom_segment(data = species_scores_clean_murlen,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(data = sp_collapsed_murlen1, aes(NMDS1, NMDS2, label = label),
                  max.overlaps = Inf, size = 4)



###################################
################################################
### per site NMDS  analysis #############



####aggregate species per site already done as species_per_site

##remove the site column already done in species_matrix <- species_per_site %>% select(-SITE)


# Keep plot IDs separate
sites_id <- species_per_site$SITE

# Run NMDS (Bray-Curtis is common for species data)
set.seed(123)  # reproducibility
nmds_sites <- metaMDS(species_matrix, distance = "bray", k = 2, trymax = 100)

# Extract NMDS scores (sites)
nmds_scores_sites <- as.data.frame(scores(nmds_sites, display = "sites"))
nmds_scores_sites$SITE <- sites_id

# Extract species scores (vectors)
species_scores_sites <- as.data.frame(scores(nmds_sites, display = "species"))
species_scores_sites$species <- rownames(species_scores_sites)


plot_colors_sites <- c("orange", "green", "blue")

# Plot NMDS biplot
ggplot() +
  # Points for plots
  geom_point(data = nmds_scores_sites, aes(x = NMDS1, y = NMDS2), color = plot_colors_sites,  size = 6) +
  geom_text_repel(data = nmds_scores_sites, aes(x = NMDS1, y = NMDS2, label = SITE), size = 5) +
  
  # Arrows for species
  geom_segment(data = species_scores_sites,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(data = species_scores_sites, aes(x = NMDS1, y = NMDS2, label = species),
                  color = "black", size = 4) +
  
  theme_minimal() +
  coord_equal()

nmds_sites$stress





####### D A M P A ######

##USE the already made matrix for Dampa
plot_groups <- factor(rownames(species_dampa_plot_matrix))

result <- multipatt(species_dampa_plot_matrix, cluster = plot_groups, control = how(nperm=999))
summary(result)



######   M U R L E N ####

plot_groups_MURLEN <- factor(rownames(species_murlen_plot_matrix))

result <- multipatt(species_murlen_plot_matrix, cluster = plot_groups_MURLEN, control = how(nperm=999))
summary(result)



########################################################################
################  S I M P S O N S'     D I V E R S I T Y################


##investigate the Simpson's diversity index
###measure of diversity which takes into account the number of species present, as well as the relative abundance of each species. 

######################Dampa

# Step 1: Sum all species per plot (across collection events)
dampa_summary <- species_dampa %>%
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), ~ sum(.x, na.rm = TRUE)), .groups = "drop")


# Step 2: Calculate Simpson's Diversity Index per plot
dampa_summary <- dampa_summary %>%
  rowwise() %>%
  mutate(simpson = diversity(c_across(starts_with("s_")), index = "simpson")) %>%
  ungroup()

print(dampa_summary$simpson)
simpson_values <- c(0.9237426, 0.8832200, 0.8887567, 0.8980716)
plots <- c("plot 1", "plot 2", "plot 3", "plot 4")

simpson_summary <- data.frame(
  plot = plots,
  simpson_index = simpson_values
)

print(simpson_summary)

#### light trap was only performed on plot 1 it is something to take into consideration 
##investigating light trap separately 

# Calculate diversity for light trap samples only
light_trap_summary <- dampa %>%
  filter(TRAP == "light trap") %>%
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  rowwise() %>%
  mutate(simpson = diversity(c_across(starts_with("s_")), index = "simpson")) %>%
  ungroup()

# Calculate diversity for other traps only
other_traps_summary <- dampa %>%
  filter(TRAP != "light trap") %>%
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  rowwise() %>%
  mutate(simpson = diversity(c_across(starts_with("s_")), index = "simpson")) %>%
  ungroup()



print(light_trap_summary$simpson)

print(other_traps_summary$simpson)




################# M U R L E N #########

# Step 1: Sum all species per plot (across collection events)
murlen_summary <- species_murlen %>%
  group_by(PLOT) %>%
  summarise(across(starts_with("s_"), ~ sum(.x, na.rm = TRUE)), .groups = "drop")


# Step 2: Calculate Simpson's Diversity Index per plot
murlen_summary <- murlen_summary %>%
  rowwise() %>%
  mutate(simpson = diversity(c_across(starts_with("s_")), index = "simpson")) %>%
  ungroup()

print(murlen_summary$simpson)




#################################################################
############### M A P S #############

world <- ne_countries(scale = "medium", returnclass = "sf")



library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggrepel)
library(rnaturalearth)

# Data 
world   <- ne_countries(scale = "medium", returnclass = "sf")
land    <- "#d9f0d3"
sea     <- "#c6dbef"

# Mizoram bbox as sf
miz_bbox <- st_as_sfc(st_bbox(c(xmin = 91, ymin = 21, xmax = 94, ymax = 25), crs = 4326))

# one point per site
sites <- plots_df_clean %>% group_by(SITE) %>% slice(1) %>% ungroup()

# sites inside bbox (for zoom)
sites_miz <- sites %>%
  filter(dplyr::between(longitud, 91, 94),
         dplyr::between(latitud, 21, 25))

global_map <- ggplot() +
  geom_sf(data = world, fill = land, color = "grey60", linewidth = 0.3) +
  geom_sf(data = miz_bbox, fill = NA, color = "red", linewidth = 1.2) +
  coord_sf(expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = sea, colour = NA),
    panel.grid.major = element_line(color = "white")
  )

# 2) Zoomed Mizoram map — black labels, no legend
mizoram_map <- ggplot() +
  geom_sf(data = world, fill = land, color = "grey60", linewidth = 0.3) +
  geom_sf(data = miz_bbox, fill = NA, color = "red", linewidth = 0.9, linetype = "dashed") +
  geom_point(data = sites_miz, aes(longitud, latitud), size = 3, color = "blue") +
  ggrepel::geom_text_repel(
    data = sites_miz,
    aes(longitud, latitud, label = SITE),
    color = "black",
    size = 3,
    max.overlaps = 20,
    box.padding = 0.3,
    point.padding = 0.2
  ) +
  coord_sf(xlim = c(91, 94), ylim = c(21, 25), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = sea, colour = NA),
    panel.grid.major = element_line(color = "white")
  )

# 3) Combine maps
map1 <- global_map + mizoram_map + plot_layout(widths = c(2.8, 1))
map1

