############################
############### MERGING PHYLOGENETIC WITH ECOLOGY #########################
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsave)
library(patchwork) 


###

abun <- read.csv("morphospecies_count.csv")
meta <- read.csv("subfamily_morphospecies.csv")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# extract abundance data for realm and subfamily
ab_long <- abun %>%
  mutate(SITE = str_to_title(str_trim(SITE))) %>%
  pivot_longer(matches("^s_\\d+$"), names_to = "morphospecies_n", values_to = "count") %>%
  mutate(
    morphospecies_n = str_replace_all(str_to_lower(str_squish(morphospecies_n)),
                                      "^s[._\\s]*0*([0-9]+)$", "s_\\1"),
    count = as.numeric(count)
  ) %>% filter(count > 0)

meta2 <- meta %>%
  mutate(
    site = str_to_title(str_trim(site)),
    morphospecies_n = str_replace_all(str_to_lower(str_squish(morphospecies_n)),
                                      "^s[._\\s]*0*([0-9]+)$", "s_\\1")
  )

# join 
master <- ab_long %>%
  left_join(meta2 %>% select(site, morphospecies_n, subfamily, realm),
            by = c("SITE" = "site", "morphospecies_n"))



#########
### PLOTS
realms <- ggplot(rich_by_realm, aes(SITE, n_morphospp, fill = realm)) +
  geom_col() +
  labs(x = "Site", y = "Number of morphospecies", fill = "Realm") +
  scale_fill_viridis_d(option = "F") +         # or: scale_fill_viridis_d(option="C")
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11)
  )

realms
subfamilies <- ggplot(ab_by_subfamily, aes(SITE, n_individuals, fill = subfamily)) +
  geom_col() +
  scale_fill_viridis_d(option = "B")  +
  labs(x = "Site", y = "Abundance (morphospecies count)", fill = "Subfamily") +
  theme_classic() +                                  # <- add the plus!
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12),          # increase tick labels
    axis.text.y  = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11)
  )  


subfamilies



#
#  Make summaries 
subfamily_abundance <- master %>%
  mutate(subfamily = ifelse(is.na(subfamily), "Unknown", subfamily)) %>%
  group_by(SITE, subfamily) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop")

realm_abundance <- master %>%
  mutate(realm = ifelse(is.na(realm), "Unknown", realm)) %>%
  group_by(SITE, realm) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop")



# Build the two plots from those exact objects
realms <- ggplot(realm_abundance, aes(SITE, total_count, fill = realm)) +
  geom_col() + 
  scale_fill_viridis_d(option = "D") +
  labs(x = "Site", y = "Abundance (morphospecies count)", fill = "Realm") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11)
  )

realms
subfamilies <- ggplot(subfamily_abundance, aes(SITE, total_count, fill = subfamily)) +
  geom_col() + 
  scale_fill_viridis_d(option = "B") +
  labs(x = "Site", y = "Abundance (morphospecies count)", fill = "Subfamily") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11)
  )

subfamilies
#################### MORPHOSPECIES TO REALM ####




# choose the morphospecies names
master2 <- master %>%
  mutate(morpho = coalesce(morpho_std, morphospecies_n))

# Counts per site × realm (morphospecies) 
realm_morpho <- master %>%
  mutate(realm = ifelse(is.na(realm) | realm == "", "Unknown", realm)) %>%
  distinct(SITE, morphospecies_n, realm) %>%
  count(SITE, realm, name = "n_morphospp")

realm_morpho

####plot


morpho_realm <- ggplot(realm_morpho, aes(x = SITE, y = n_morphospp, fill = realm)) +
  geom_col() +
  scale_fill_viridis_d(option = "E") +
  labs(x = "Site", y = "Number of Morphospecies", fill = "Realm") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11)
  )
p <- morpho_realm + realms + subfamilies


