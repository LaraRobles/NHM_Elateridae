Welcome to GitHub Repertory for MSc Thesis "Integrating ecological and phylogenetic approaches to uncover biodiversity and biogeographical influences in Mizoram: insights from Elateridae (Coleoptera)"


The present study applies an integrated ecological and phylogenetic framework to investigate Elateridae communities in Mizoram, assessing species richness, spatial variation, and potential endemicity. 
Using the SITE-100 standardised protocol, we combined morphospecies sorting, DNA barcoding, and phylogenetic analyses to:
(i) characterise community diversity and turnover across Mizoram sites; 
(ii) place Mizoram Elateridae within a global phylogenetic framework; 
(iii) assess the influence of biogeographical realms on Elateridae diversity and distribution; and 
(iv) provide a foundation for conservation strategies and a baseline for future research.

This Repertory contains:

PHYLOGENETIC ANALYSIS:
-------------------------

##AMINO ACID ANALYSIS

#Fasta file

Analysis:
AA_supermatrix.fasta

-AA ModelFinder (IQTREE)
MF_iqtree_partitions_v4.best_scheme.nex
MF_iqtree_partitions_v4.iqtree
MF_iqtree_partitions_v4.treefile

-AA analysis RAxML-NG
raxml-ng_aa_v4.raxml.bestTree
raxml-ng_aa_v4.raxml.bootstraps
raxml-ng_aa_v4.raxml.support

##NUCLEOTIDE ANALYSIS

#FASTA file 
nt_supermatrix.fasta

Analysis:

-IQTREE mset GTR+F+I+G4
nt_iqtree.iqtree
nt_iqtree.treefile

-IQTREE partition by gene and 1+2 codons
gene_codon_iqtree_v4.iqtree
RENAMED_gene_codon_iqtree_v4.treefile

-RAxML nt --model GTR+FO+R10 --bs-metric tbe --bs-trees 100 --tree pars{10}
raxml_nt_realigned.raxml.bestModel
raxml_nt_realigned.raxml.bestTree
raxml_nt_realigned.raxml.bootstraps
raxml_nt_realigned.raxml.mlTrees
raxml_nt_realigned.raxml.support

-RAxML nt partition by gene and 1+2 codons 
raxml_v4_gene_model+codon12.raxml.bestModel
raxml_v4_gene_model+codon12.raxml.bestTree
raxml_v4_gene_model+codon12.raxml.bootstraps
raxml_v4_gene_model+codon12.raxml.mlTrees
raxml_v4_gene_model+codon12.raxml.reduced.phy
RENAMED_raxml_v4_gene+codon12.raxml.support

##RY-RECODE

#FASTA 
RY_recoded.fasta

Analysis:

best_tree_RY.treefile
binary_updated_V4.raxml.bestModel
binary_updated_V4.raxml.bootstraps
binary_updated_V4.raxml.mlTrees_RENAMED.mlTree
binary_updated_V4.raxml.reduced.phy
binary_updated_V4.raxml.support

BARCODES + MIZORAM DATA
----------------------
-Input
constraint_tree_method_RY.tree
delimitation_single_mizo_barcodes.fasta
Mizoram_COI_raw.fasta
raw_barcode_data_COX1.fasta  (prior alignment)
FINAL_COX1_mizo_barcodes.fasta  (post alignment)

-Delimitation output
COI_barcodes_Mizoram_binary_constrained_GTR.tree
ptp_results.txt
mizo_delimitation_info.txt
COI_species_delimitation.csv

DATING
---------------

Input
-dates.txt
-dating.fasta
-COI_barcodes_Mizoram_binary_constrained_GTR.tree

Output:
-DATED_elateridae_tree.treefile


Thanks

Lara
