
####phylogenetic trees####
####GET THE LIBRARY READY####

library(ggtree)
library(ape)
library(dplyr)
library(stringr)
library(scales)
library(ggplot2)
library(TDbook)
rm(list = ls())  # Removes all variables and objects

###data
tree <- read.tree("name_of_your_tree.tree")                                      
phy <- as.phylo(tree)
meta <- read.csv("name_of_your_csv.csv", stringsAsFactors = FALSE)       



#####################CSV and tree
#########################


tip_tbl <- tibble(label = phy$tip.label) %>%
  mutate(id_token = sub("_.*$", "", label))   # barcode before first "_"

## 1) Clean metadata and keep only useful rows
meta_clean <- meta %>%
  mutate(
    mt_id = trimws(as.character(mt_id)),
    label = trimws(as.character(label))
  ) %>%
  filter(!is.na(mt_id), mt_id != "", !is.na(label), label != "") %>%
  distinct(mt_id, .keep_all = TRUE)

old <- tree$tip.label

## 2) Exact match lookup
exact_map <- setNames(meta_clean$label, meta_clean$mt_id)
exact_hit <- unname(exact_map[old])            # NA when no exact match

## 3) Prefix match (longest mt_id wins)
meta_pref <- meta_clean %>% arrange(desc(nchar(mt_id)))
prefix_hit <- sapply(old, function(tip) {
  i <- which(startsWith(tip, meta_pref$mt_id))
  if (length(i)) meta_pref$label[i[1]] else NA_character_
})

## 4) Final labels: exact > prefix > original
new_labels <- ifelse(!is.na(exact_hit), exact_hit,
                     ifelse(!is.na(prefix_hit), prefix_hit, old))

## 5) Apply to tree
tree$tip.label <- new_labels
phy <- as.phylo(tree)

###########CIRCULAR BIG TREE######################



stopifnot(all(c("label","realm") %in% names(meta)))
meta <- meta %>%
  mutate(label = trimws(as.character(label)),
         realm = na_if(trimws(as.character(realm)), "NA")) %>%
  distinct(label, .keep_all = TRUE)

# Focus tips to highlight on new barcodes from Mizoram
focus <- c("AIZSW1_5-5-25-1",
           "AIZSW1B_5-5-25-1-24",
           "AIZSW1B_5-5-25-01-30",
           "AIZSW1B_5-5-25-02-3",
           "AIZSW1B_5-5-25-02-13",
           "DTP1DF1_13-5-25-01-6",
           "DTP1LT1_6-5-25-01-1",
           "DTP1LT2_8-5-25-01-25",
           "DTP1LT2_8-5-25-01-27",
           "DTP1LT3_8-5-25-01_B4",
           "DTP1LT4_8-5-25-03-7",
           "DTP1LT4_8-5-25-03-11",
           "DTP1LT5B_21-5-25-2-4",
           "DTP1LT5B_21-5-25-3-6",
           "DTP1LT5B_21-5-25-3-8",
           "DTP1LT5B_21-5-25-11",
           "DTP1MA2_21-5-25-1-13",
           "DTP2FI3B_13-5-25-2-1",
           "DTP2MA1_13-5-25-1-10",
           "DTP3FI2A_21-5-25-2-13",
           "DTP3FI2B_21-5-25-2-6",
           "DTP4FI1A_22-5-25-2-8",
           "DTP4FI1A_22-5-25-2-9",
           "DTP4FI2A_13-5-25-9",
           "DTP4HC1_8-5-25-2-RE1",
           "DTP4MA1_22-5-25-02-2",
           "DTP4MA2_13-5-25-01-4",
           "JHHC1_25-4-25_02-9",
           "JHLT1_12-5-25-01-3",
           "MUP1F4B_17-5-25-01-1",
           "MUP1F4B_17-5-25-01-2",
           "MUP1FI2_3-5-25-03-2",
           "MUP1FI2_3-5-25-03-3",
           "MUP1FI2_3-5-25-03-4",
           "MUP1FI2_3-5-25-03-5",
           "MUP1FI4A_17-5-25-7",
           "MUP1FI4A_17-5-25-10",
           "MUP1FI4B_17-5-25-02-6",
           "MUP1HC4_1-5-25-01-8",
           "MUP1MA4_7-5-25-02-6",
           "MUP1SW1_17-5-25-01-7",
           "MUP2FI2_17-5-25-1-12",
           "MUP3FI2_17-5-25-01-3",
           "MUP3MA1_14-5-25-01-5",
           "MUP3MA3_17-5-25-02-4",
           "MUP4FI1_3-5-25-02-7",
           "MUP4FI2_17-5-25-02-1",
           "MUP4FI2_17-5-25-03-2",
           "MUP4MA3_17-5-25-01-3",
           "MUP5LT1a_15-5-25-2-1",
           "MUP5LT1a_15-5-25-4-1",
           "MUP5LT1D_15-5-25-1-1",
           "MUP5LT1D_15-5-25-1-2",
           "MUP5LT1E_15-5-25-1-6",
           "MUP5LT1I_15-5-25-01-6",
           "MUP5LT3A_17-5-25-7",
           "MUP5LT4A_17-5-25-1-2",
           "MUP5LT4A_17-5-25-1-6",
           "MUP5LT4B_17-5-25-3-7",
           "MUP5LT5b_17-5-25-25",
           "MUP5LT5b_17-5-25-28",
           "MUP6SW1_16-5-25-02-4",
           "MUP6SW1_16-5-25-02-5",
           "DTP1DF1_13-5-25-01_-7",
           "MUP5LT1a_15-5-25-2-3",
           "MUP6SW1_16-5-25-2"
)  #all mizo barcodes


######### circular big tree 3.7  
#########circular big tree 3.7k###
# Packages
library(ggplot2)
library(ggtree)
library(ggtreeExtra)
library(dplyr)
library(stringr)
library(ggnewscale)
library(scales)

# DATA
tree <- read.tree("FINAL_RENAMED_tree9_mizo_barcodes_GTR.tree")
meta <- read.csv("mizo_barcodes_realm.csv", stringsAsFactors = FALSE)
phy <- as.phylo(tree)


CIRCULAR COI TREE
#Tree proceduce
clean metadata & allow join by label and mt_id####
stopifnot(any(c("label","mt_id") %in% names(meta)))

###add the realm, subfamily info to the label

meta3 <- meta %>%
  mutate(
    label     = if ("label" %in% names(.)) trimws(as.character(label)) else NA_character_,
    mt_id     = if ("mt_id" %in% names(.)) trimws(as.character(mt_id)) else NA_character_,
    realm     = na_if(trimws(as.character(realm)), "NA"),
    subfamily = na_if(trimws(as.character(subfamily)), "NA")
  ) %>%
  distinct(label, .keep_all = TRUE)

# Build a tip table matched to tree; attach realm/subfamily from label, else from mt_id 
tip_tbl <- tibble(label = phy$tip.label) %>%
  mutate(id_token = sub("_.*$", "", label)) %>%
  left_join(meta %>% select(label, realm, subfamily), by = "label") %>%
  rename(realm_by_label = realm, subf_by_label = subfamily) %>%
  { if ("mt_id" %in% names(meta)) {
    left_join(., meta %>% select(mt_id, realm, subfamily),
              by = c("id_token" = "mt_id"))
  } else .
  } %>%
  rename(realm_by_id = realm, subf_by_id = subfamily) %>%
  mutate(
    realm     = coalesce(realm_by_label, realm_by_id),
    subfamily = coalesce(subf_by_label, subf_by_id)
  ) %>%
  select(label, id_token, realm, subfamily)



# quick diagnostics: to ensure meta data works
unmatched_meta <- meta3 %>% filter(is.na(label))
unmatched_tips <- tip_tbl %>%
  anti_join(meta %>% select(label), by = "label")
if (nrow(unmatched_meta)) {
  message("Metadata rows missing a label (mt_id not found among tip ID tokens):")
  print(unmatched_meta %>% select(mt_id, realm, subfamily) %>% head())
}
if (nrow(unmatched_tips)) {
  message("Tree tips not found in metadata (by label):")
  print(unmatched_tips %>% head())
}


###  focus groups by prefix to highlight barcodes from Mizoram 
tip_tbl <- tip_tbl %>%
  mutate(focus_group = case_when(
    label %in% focus & startsWith(id_token, "DTP")                 ~ "Dampa",
    label %in% focus & startsWith(id_token, "MUP")                 ~ "Murlen",
    label %in% focus & (startsWith(id_token, "JHLT") |
                          startsWith(id_token, "JHHC") |
                          startsWith(id_token, "AIZ"))               ~ "Aizawl",
    TRUE ~ NA_character_
  ))

View(tip_tbl)


### MAKE THE PALETTES for realms and subfamilies
library(RColorBrewer)

realms <- sort(unique(na.omit(tip_tbl$realm)))

# Use the Set3 palette (pastel-like, high contrast)
distinct_colors <- brewer.pal(min(length(realms), 10), "PRGn")

# Assign to realms
pal_realm <- setNames(distinct_colors[seq_along(realms)], realms)
pal_sub <- c(
  Agrypninae="yellowgreen", Cardiophorinae="orange", Dendrometrinae="turquoise3",
  Elaterinae="steelblue", Tetralobinae="violetred", Thylacosterninae="skyblue",
  Negastriinae="purple", Oxynopterinae="blue1", Lissominae="sandybrown",
  Semiotinae="slategray3", Lampyridae="darkblue", Hemiopinae="limegreen",
  Melanotinae="darkgreen", Cantharidae="yellow", Morostomatinae="salmon",
  Physodactylinae="chocolate4", Lycidae="violet", Paulusiellinae="cornsilk2",
  Parablacinae="darkgrey", Hapatesinae="rosybrown", Pityobiinae="red"
)

pal_focus <- c("Dampa"="red","Murlen"="blue","Aizawl"="green3")





######    
#####  MAIN TREE ###

realms <- sort(unique(na.omit(tip_tbl$realm)))

# Use a palette for realms
distinct_colors <- brewer.pal(min(length(realms), 10), "Spectral")
# Assign to realms
pal_realm <- setNames(distinct_colors[seq_along(realms)], realms)

# base tree
p <- ggtree(phy, layout = "circular", size = 0.5, color = "black",
            branch.length = "none")

## Add a bit of room outside the tree for the rings
p <- p + xlim_tree(max(p$data$x, na.rm = TRUE) + 2)

# Ring 1: SUBFAMILY (inner)
p1 <- p + ggtreeExtra::geom_fruit(
  data    = rings,
  geom    = geom_tile,
  mapping = aes(y = label, fill = subfamily),
  pwidth  = 6,
  offset  = 0.07
) +
  scale_fill_manual(values = pal_sub, name = "Subfamily", na.translate = FALSE)


p1 <- p1 + ggnewscale::new_scale_fill()

# Ring 2: REALM (outer)
p2 <- p1 + ggtreeExtra::geom_fruit(
  data    = rings,
  geom    = geom_tile,
  mapping = aes(y = label, fill = realm),   # drop width in aes()
  pwidth  = 7,                           # thickness of the ring
  offset  = 0.15
) +
  scale_fill_manual(values = pal_realm, name = "Realm", na.translate = FALSE)




# use coordinates from the current ggtree
tree_xy  <- p$data %>% filter(isTip) %>% select(label, x, y)
df_focus <- tree_xy %>% left_join(tip_tbl %>% select(label, focus_group), by = "label") %>%
  filter(!is.na(focus_group))

p3 <- p2 +
  geom_point(data = df_focus, aes(x = x, y = y, color = focus_group),
             size = 2.5, show.legend = TRUE) +
  scale_color_manual(values = pal_focus, name = "Mizoram")
##THIS ONE WORKS
# ---- theme & save ----
p3 <- p3 + theme(
  legend.position = "right",
  legend.title = element_text(size = 10),
  legend.text  = element_text(size = 10)
)

print(p3)



############## ADDING COLOR TO BRANCHES
install.packages("ggbreak")
library(ggbreak)


# First, figure out which branches belong to each focus_group
library(tidytree)

# COLOR BRANCHES by focus_group

# 1) Map each child node to its parent; find the root node id
edge      <- phy$edge                   # 2-column matrix: parent, child
parent_of <- setNames(edge[,1], edge[,2])
root      <- setdiff(edge[,1], edge[,2])[1]

# 2) Which tips are in focus
focus_map <- tip_tbl %>% 
  dplyr::select(label, focus_group) %>% 
  dplyr::filter(!is.na(focus_group))

# tip index for those labels
tip_idx <- setNames(match(focus_map$label, phy$tip.label), focus_map$label)

# 3) Walk from each focus tip up to the root, collect all nodes on those paths
path_to_root <- function(child) {
  out <- integer()
  cur <- child
  while (!is.na(cur) && cur != root) {
    out <- c(out, cur)
    cur <- parent_of[as.character(cur)]
  }
  out
}

# 4) Assign each node to a focus_group (overlaps resolved by pal_focus order)
node_ids   <- seq_len(length(phy$tip.label) + phy$Nnode)
node_group <- setNames(rep(NA_character_, length(node_ids)), node_ids)

for (fg in names(pal_focus)) {
  tips_fg  <- tip_idx[focus_map$focus_group == fg]
  if (length(tips_fg)) {
    nodes_fg <- unique(unlist(lapply(tips_fg, path_to_root)))
    tgt <- as.character(nodes_fg)
    node_group[tgt][is.na(node_group[tgt])] <- fg
  }
}

node_df <- tibble::tibble(
  node = as.integer(names(node_group)),
  focus_group = unname(node_group)
) %>%
  mutate(focus_group = ifelse(is.na(focus_group), "Other", focus_group))

# Palette with "Other" set to black
pal_focus_full <- c(pal_focus, Other = "black")

# 5) Plot: color branches by focus_group, keep others black
p4 <- ggtree(phy, layout = "circular", branch.length = "none",
             aes(color = focus_group), size = 0.3) %<+% node_df +
  scale_color_manual(values = pal_focus, na.value = "black", name = "Mizoram")

# make room for rings
p5 <- p4 + xlim_tree(max(p4$data$x, na.rm = TRUE) + 2)

# rings as before
p6 <- p5 +
  ggtreeExtra::geom_fruit(
    data = rings, geom = geom_tile,
    mapping = aes(y = label, fill = subfamily),
    pwidth = 5, offset = 0.08
  ) +
  scale_fill_manual(values = pal_sub, name = "Subfamily", na.translate = FALSE) +
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    data = rings, geom = geom_tile,
    mapping = aes(y = label, fill = realm),
    pwidth = 7, offset = 0.15
  ) +
  scale_fill_manual(values = pal_realm, name = "Realm", na.translate = FALSE)

# dots at the tips
tree_xy  <- p4$data %>% dplyr::filter(isTip) %>% dplyr::select(label, x, y)
df_focus <- tree_xy %>%
  dplyr::left_join(tip_tbl %>% dplyr::select(label, focus_group), by = "label") %>%
  dplyr::filter(!is.na(focus_group))

p6 <- p6 + geom_point(data = df_focus,
                      aes(x = x, y = y, color = focus_group),
                      size = 2, show.legend = TRUE)

p7 <- p6 + theme(
  legend.position = "bottom",
  legend.box = "vertical",
  legend.title = element_text(size = 5),
  legend.text  = element_text(size = 5)
)

p7


#####RECTANGULAR TREE##
p4 <- ggtree(phy, layout = "rectangular", #branch.length = "none"
             aes(color = focus_group), size = 0.3) %<+% node_df +
  scale_color_manual(values = pal_focus, na.value = "black", name = "Mizoram")

p4 <- p4 + scale_x_break(c(0.03, 0.09)) + hexpand(.02)

# make room for rings (use p4's data, not p$data)
p5 <- p4 + xlim_tree(max(p4$data$x, na.rm = TRUE) + 2)

# rings as before
p6 <- p5 +
  ggtreeExtra::geom_fruit(
    data = rings, geom = geom_tile,
    mapping = aes(y = label, fill = subfamily),
    pwidth = 0.2, offset = 0.05
  ) +
  scale_fill_manual(values = pal_sub, name = "Subfamily", na.translate = FALSE) +
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    data = rings, geom = geom_tile,
    mapping = aes(y = label, fill = realm),
    pwidth = 0.2, offset = 0.2
  ) +
  scale_fill_manual(values = pal_realm, name = "Realm", na.translate = FALSE)

# dots at the tips (use p4's coordinates)
tree_xy  <- p4$data %>% dplyr::filter(isTip) %>% dplyr::select(label, x, y)
df_focus <- tree_xy %>%
  dplyr::left_join(tip_tbl %>% dplyr::select(label, focus_group), by = "label") %>%
  dplyr::filter(!is.na(focus_group))

p6 <- p6 + geom_point(data = df_focus,
                      aes(x = x, y = y, color = focus_group),
                      size = 2, show.legend = TRUE)

p7 <- p6 + theme(
  legend.position = "right",
  legend.title = element_text(size = 9),
  legend.text  = element_text(size = 9)
)


p7





### C O N S T R A I N T      T R E E   ###




tree <- read.tree("YOUR_TREE.tree") 
phy1 <- as.phylo(tree)
meta <- read.csv("your_csv.csv") 

head(meta)

###extact data from mt csv
library(dplyr)

phy1 <- as.phylo(tree)

# 1) Tip table from the tree (label = tip name)
tip_tbl <- tibble(label = phy$tip.label) %>%
  mutate(id_token = sub("_.*$", "", label))   # finding barcode before first "_"

# 2) Clean metadata and join by mt_id -> id_token
meta_clean <- meta %>%
  mutate(
    mt_id     = trimws(as.character(mt_id)),
    subfamily = na_if(trimws(as.character(subfamily)), "")
  ) %>%
  distinct(mt_id, .keep_all = TRUE)

# Join metadata to tips using the barcode prefix
tip_tbl <- tip_tbl %>%
  left_join(meta_clean %>% select(mt_id, subfamily),
            by = c("id_token" = "mt_id"))


#### Extract data

meta$subfamily <- factor(meta$subfamily)

meta2 <- tip_tbl %>%
  dplyr::select(label, subfamily) %>%
  dplyr::filter(!is.na(subfamily)) %>%
  dplyr::distinct()
##use the palette for subfamily 

pal_sub <- c(
  Agrypninae="yellowgreen", Cardiophorinae="orange", Dendrometrinae="turquoise3",
  Elaterinae="steelblue", Tetralobinae="violetred", Thylacosterninae="skyblue",
  Negastriinae="purple", Oxynopterinae="blue1", Lissominae="indianred3",
  Semiotinae="slategray3", Lampyridae="darkblue", Hemiopinae="limegreen",
  Melanotinae="darkgreen", Cantharidae="yellow", Morostomatinae="salmon",
  Physodactylinae="chocolate4", Lycidae="violet", Paulusiellinae="cornsilk2",
  Parablacinae="darkgrey", Hapatesinae="rosybrown", Pityobiinae="red", Prosterninae="navyblue"
)


meta2 <- tip_tbl %>% dplyr::select(label, subfamily) %>% dplyr::distinct()

p <- ggtree(phy1, size = 0.8) + theme_tree2()
p <- p %<+% meta2 +
  geom_tippoint(aes(color = subfamily), size = 2, alpha = 0.95) +
  scale_color_manual(
    name = "subfamily",
    values = pal_sub,
    na.value = "grey80",     # color for NA subfamilies
    na.translate = FALSE     # don't add an NA key to the legend
  ) +
  guides(color = guide_legend(override.aes = list(size = 9)))
p
dpi <- 600
dev.copy(tiff, filename = "tree_r_constraint_FINAL.tiff",
         width  = par("din")[1] * dpi,
         height = par("din")[2] * dpi,
         res = dpi, compression = "lzw")
dev.off()





####focus TREES on Mizoram samples #####


###create a meta data with label, subfamily and realm info
##tip_tbl already made 

####for CAMPSOSTERNUS### 

library(ape)
library(tidytree)
library(treeio)

library(tidytree)

mini_focus <- c("JHHC1_25-4-25_02-9", "DTP1LT1_6-5-25-01-1", "MUP5LT1I_15-5-25-01-6")

tree2 <- tree_subset(phy, "MUP5LT1I_15-5-25-01-6", levels_back = 7) ##choose number of levels you want to see

p <- ggtree(tree2, size = 0.8) + theme_tree2()
p <- p %<+% tip_tbl +
  geom_tippoint(aes(color = realm), size = 4, alpha = 0.95) +
  scale_color_manual(
    name = "subfamily",
    values = pal_realm,
    na.value = "grey80",     # color for NA subfamilies
    na.translate = FALSE     # don't add an NA key to the legend
  ) +
  guides(color = guide_legend(override.aes = list(size = 6)))
p

#simple tree campsosternus####

p1 <- ggtree(tree2, size = 0.8) + theme_tree2()
p1 <- p1 %<+% tip_tbl +
  geom_tippoint(aes(color = realm), size = 4, alpha = 0.95) +
  scale_color_manual(name = "Realm", values = pal_realm,
                     na.value = "grey80", na.translate = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 7)))

mini_focus <- c("JHHC1_25-4-25_02-9",
                "DTP1LT1_6-5-25-01-1",
                "MUP5LT1I_15-5-25-01-6")

# red dots exactly on tips
p1 <- p1 + geom_tippoint(aes(subset = label %in% mini_focus),
                         color = "red", size = 3.8, show.legend = FALSE)

# compute a small offset (~1% of tree width)
xr <- diff(range(p$data$x, na.rm = TRUE))
lab_off <- xr * 0.01

# labels close to tips (no alignment column)
p1 <- p1 + geom_tiplab(
  aes(subset = label %in% mini_focus, label = label),
  color = "black", fontface = "bold",
  align = FALSE, hjust = 0, offset = lab_off, size = 3,
  show.legend = FALSE
)

# add a little right margin so labels aren’t clipped
p1 <- p1 + expand_limits(x = max(p$data$x, na.rm = TRUE) + lab_off * 6) +
  coord_cartesian(clip = "off")

p1

p1 <- p1 +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    plot.margin = margin(5.5, 5.5, 35, 5.5) # extra bottom room
  ) +
  guides(color = guide_legend(override.aes = list(size = 6), nrow = 1))

p1

#####WITH LABELS

# 0) remove any old label layers to avoid duplicates
p <- ggtree(tree2, size = 0.8) + theme_tree2()
p <- p %<+% tip_tbl +
  geom_tippoint(aes(color = realm), size = 3.8, alpha = 0.95) +  # bigger dots here
  scale_color_manual(name = "realm", values = pal_realm,
                     na.value = "grey80", na.translate = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 6)))     # legend dot size

p$layers <- Filter(function(ly) {
  !inherits(ly$geom, "GeomTiplab") && !inherits(ly$geom, "GeomText") && !inherits(ly$geom, "GeomLabel")
}, p$layers)

# 1) put the legend outside (bottom)

p <- p + theme(
  legend.position = "bottom",
  legend.direction = "horizontal",
  plot.margin = margin(5.5, 10, 30, 5.5)  # extra space for bottom legend
) +
  guides(color = guide_legend(override.aes = list(size = 6), nrow = 1))

# 2) label ALL tips once (grey), close to the tips
xr <- diff(range(p$data$x, na.rm = TRUE))
lab_off <- xr * 0.012

p <- p + geom_tiplab(
  aes(label = label),
  color = "black", size = 3,
  align = FALSE, hjust = 0, offset = lab_off,
  show.legend = FALSE
)

# 3) overlay ONLY red dots for the mini-focus tips (no red labels)
mini_focus <- c("JHHC1_25-4-25_02-9",
                "DTP1LT1_6-5-25-01-1",
                "MUP5LT1I_15-5-25-01-6")

p <- p + geom_tippoint(
  aes(subset = label %in% mini_focus),
  color = "red", size = 3.8, show.legend = FALSE
) +
  coord_cartesian(clip = "off") +
  expand_limits(x = max(p$data$x, na.rm = TRUE), size = 2.6,
                show.legend = FALSE)

p

lab_off <- diff(range(p$data$x, na.rm = TRUE)) * 0.01
p1 <- p + coord_cartesian(clip = "off") +          # allow drawing outside panel
  xlim(0, max(p$data$x, na.rm = TRUE) +    # extend x-range
         lab_off * 12) +
  theme(plot.margin = margin(5.5, 140, 5.5, 5.5))

p1
ggsave("tree.png", p, width = 8, height = 10, dpi = 300)
p <- p +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    plot.margin = margin(5.5, 5.5, 35, 5.5) # extra bottom room
  ) +
  guides(color = guide_legend(override.aes = list(size = 6), nrow = 1))

p

##Modify tip label to shorten name####

# build a table of the tips in the (sub)set tree you're plotting
tip_df <- tibble(label = tree2$tip.label)

# helper: make "Genus species Country" from a long label
make_short <- function(lbl) {
  parts <- strsplit(lbl, "_", fixed = TRUE)[[1]]
  if (length(parts) < 5) return(lbl)  # not enough fields; keep as-is
  
  # try to find a binomial after the 4th underscore
  # (first token with Capitalized + next all-lowercase)
  gs <- NA_character_
  for (i in 5:(length(parts) - 1)) {
    if (grepl("^[A-Z][a-z-]+$", parts[i]) && grepl("^[a-z-]+$", parts[i + 1])) {
      gs <- paste(parts[i], parts[i + 1])
      break
    }
  }
  country <- parts[length(parts)]
  
  if (!is.na(gs)) {
    paste(gs, country)
  } else {
    # fallback: everything after the 4th "_"
    paste(parts[5:length(parts)], collapse = " ")
  }
}

# compute short labels and keep focus labels unchanged
tip_df <- tip_df %>%
  mutate(
    label_short = vapply(label, make_short, character(1)),
    label_show  = if_else(label %in% mini_focus, label, label_short)
  )

# plot (your code) + attach label_show for tiplab
p <- ggtree(tree2, size = 0.8) + theme_tree2()
p <- p %<+% tip_tbl +
  geom_tippoint(aes(color = realm), size = 3.8, alpha = 0.95) +
  scale_color_manual(name = "Realm", values = pal_realm,
                     na.value = "grey80", na.translate = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 6)))

# remove any existing tip labels
p$layers <- Filter(function(ly) {
  !inherits(ly$geom, "GeomTiplab") && !inherits(ly$geom, "GeomText") && !inherits(ly$geom, "GeomLabel")
}, p$layers)

# add compact labels (use label_show we just made)
xr <- diff(range(p$data$x, na.rm = TRUE))
lab_off <- diff(range(p$data$x, na.rm = TRUE)) * 0.01

p <- p %<+% tip_df +   # attach label_show to plotting data
  geom_tiplab(aes(label = label_show),
              color = "black", size = 3,
              align = FALSE, hjust = 0, offset = lab_off,
              show.legend = FALSE) +
  coord_cartesian(clip = "off")

# red dots for mini_focus (unchanged)
p <- p + geom_tippoint(aes(subset = label %in% mini_focus),
                       color = "red", size = 3.8, show.legend = FALSE)

p


# Extend x-axis to allow space for long labels
p_campsosternus <- p + coord_cartesian(clip = "off") +
  xlim(0, max(p$data$x, na.rm = TRUE) + lab_off * 12) +
  theme(
    plot.margin = margin(5.5, 200, 40, 5.5),  # extra space on right (200) and bottom (40)
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical"
  ) +
  guides(color = guide_legend(nrow = 1, override.aes = list(size = 6)))


####### fOR ELATERINAE ####

###choose the barcode to focus on

tree3 <- tree_subset(tree, "DTP1LT4_8-5-25-03-11", levels_back = 3)

elaterinae_focus <- c("MUP5LT4A_17-5-25-1-2",
                      "MUP3MA1_14-5-25-01-5",
                      "MUP5LT1D_15-5-25-1-1",
                      "MUP1FI4A_17-5-25-7",
                      "DTP1LT4_8-5-25-03-11")

# base tree with realm dots
p3 <- ggtree(tree3, size = 0.5) + theme_tree2()
p3 <- p3 %<+% tip_tbl +
  ggtree::geom_tippoint(aes(color = realm), size = 2, alpha = 0.9) +
  scale_color_manual(name = "Realm", values = pal_realm,
                     na.value = "grey80", na.translate = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 7)))

# find coordinates for focus tips
coords <- p3$data %>%
  filter(isTip, label %in% elaterinae_focus) %>%
  left_join(num_map, by = "label")

# add red dots at tips (plain geom_point)
p3 <- p3 + ggplot2::geom_point(
  data = coords,
  aes(x = x, y = y),
  color = "red", size = 3, inherit.aes = FALSE
)

# add numbers slightly offset
xr <- diff(range(p3$data$x, na.rm = TRUE))
lab_off <- xr * 0.05

p3 <- p3 + ggplot2::geom_text(
  data = coords,
  aes(x = x + lab_off, y = y, label = num),
  color = "black", fontface = "bold", size = 3.5,
  inherit.aes = FALSE
)

# expand so numbers aren’t clipped
p3 <- p3 +
  expand_limits(x = max(p3$data$x, na.rm = TRUE) + lab_off * 6)

p3

p4 <- p3 + theme(
  legend.position = "bottom",
  legend.box = "vertical",
  legend.title = element_text(size = 9),
  legend.text  = element_text(size = 9)
)
p4

