
#' ### 2. Genomic plots - ggbio
#' 
## ------------------------------------------------------------------------


#--- new ---#
# BiocManager : install and manager packages from Bioconductor project #
 if (!requireNamespace("BiocManager", quietly=TRUE))
   install.packages("BiocManager")
 BiocManager::install("ggbio")

 # BiocManager::install("EnsDb.Hsapiens.v75")

library(ggplot2)
library(ggbio)
library(biovizBase)
library(GenomicRanges)
#library(EnsDb.Hsapiens.v75)
#an annotation databases generated from Ensembl.



#### Circular plot: 22 chromosome in one circle , also annotation

data("CRC", package = "biovizBase") 
#load CRC data from biovizBase
? CRC #a sample data of Genomic sequencing of colorectal adenocarcinomas

# loaded 3 data: [1] "crc.gr"  "hg19sub" "mut.gr"
# crc.gr
# GRanges object with 650 ranges and 17 metadata columns:

#   seqinfo: 22 sequences from an unspecified genome 
head(crc.gr,2 )
# contain sample, rarrangement, link info

names(values(crc.gr))
# [1] "individual"        "str1"              "class"             "span"             
# [5] "tumreads"          "normreads"         "gene1"             "gene2"            
# [9] "site1"             "site2"             "fusion"            "quality"          
# [13] "score"             "BPresult"          "validation_result" "to.gr"            
# [17] "rearrangements"   


head(hg19sub)
# seqnames      ranges strand
# <Rle>   <IRanges>  <Rle>
#   [1]        1 1-249250621      *
#   [2]        2 1-243199373      *
#   [3]        3 1-198022430      *
#   [4]        4 1-191154276      *
#   [5]        5 1-180915260      *
#   [6]        6 1-171115067      *

#GRanges object with 22 ranges and 0 metadata columns:
#seqinfo: 22 sequences from hg19 genome

head(mut.gr)
# GRanges object with 1 range and 10 metadata columns:
#   seqnames    ranges strand | Hugo_Symbol Entrez_Gene_Id   Center NCBI_Build   Strand
# <Rle> <IRanges>  <Rle> |    <factor>      <integer> <factor>  <integer> <factor>
#   [1]        1  11003085      + |      TARDBP          23435    Broad         36        +
#   Variant_Classification Variant_Type Reference_Allele Tumor_Seq_Allele1
# <factor>     <factor>         <factor>          <factor>
#   [1]               Missense          SNP                G                 G
# Tumor_Seq_Allele2
# <factor>
#   [1]                 A
# -------


#-------------------------------------------#
##Ex1: stepwise demo: stepwise to see the layers of compoments from inside to outside if not manual set radius/track width ##

p1 <- ggbio()+
      circle(hg19sub, geom = "ideo", fill = "gray70"); p1 # Ideogram 

p1 <- p1+  circle(hg19sub, geom = "scale", size = 2) ; p1 #scale
  
p1 <- p1+   circle(hg19sub, geom = "text", aes(label = seqnames), vjust = 0, size = 3)
      p1 #add labels

#p1 <- p1+   circle(hg19sub, geom = "text", aes(label = seqnames), vjust = 0, size = 4,col='red')

p1 <-  p1+circle(mut.gr, geom = "rect", color = "steelblue", radius=33); p1 
      # add a "rectangle" track to show somatic mutation-looks like vertical segments.
      

#Next, add some "links" to show the rearrangement: intra and inter
#add a "point" track with grid background for rearrangement data and map ‘y‘ to variable
#      "score", map ‘size‘ to variable "tumreads", rescale the size to a proper size range.      

p1 <- p1 + circle(gr.crc1, geom = "point", aes(y = score, size = tumreads),
              color = "red", grid = TRUE, radius = 30) + scale_size(range = c(1, 2.5)); p1  

#Finally, let’s add links and map color to rearrangement types. Remember you need to specify
#‘linked.to‘ parameter to the column that contain end point of the data.     

p1 <- p1 + circle(gr.crc1, geom = "link", linked.to = "to.gr", aes(color = rearrangements),
                radius = 23)
p1
# try that in one step above , no need to set the radius ##
##Ex1: .. single sample
gr.crc1 <- crc.gr[values(crc.gr)$individual == "CRC-1"]
p <- ggbio() +
  circle(gr.crc1, geom = "link", linked.to = "to.gr", aes(color = rearrangements)) +
  circle(gr.crc1, geom = "point", aes(y = score, size = tumreads),
         color = "red", grid = TRUE) + scale_size(range = c(1, 2.5)) +
  circle(mut.gr, geom = "rect", color = "steelblue") +    # add a "rectangle" track to show somatic mutation-looks like vertical segments.
  circle(hg19sub, geom = "ideo", fill = "gray70") + # Ideogram 
  circle(hg19sub, geom = "scale", size = 2) +  #scale
  circle(hg19sub, geom = "text", aes(label = seqnames), vjust = 0, size = 3) #add labels
p


      
###########################################################
##.. multiple samples
# 9 single circular plots put together in one page, since we cannot keep too many
# tracks, we only keep ideogram and links. Here is one sample.

table(values(crc.gr)$individual)

#
#CRC-1 CRC-2 CRC-3 CRC-4 CRC-5 CRC-6 CRC-7 CRC-8 CRC-9 
#24     5   126    92    86    52    22    66   177 

grl <- split(crc.gr, values(crc.gr)$individual)
# make a list to apply the same steps for each sample 

## need "unit", load grid
library(grid)

# use lapply to apply to the list 
crc.lst <- lapply(grl, function(gr.cur){
  print(unique(as.character(values(gr.cur)$individual)))
  cols <- RColorBrewer::brewer.pal(3, "Set2")[2:1]  # call palette name
  names(cols) <- c("interchromosomal", "intrachromosomal")
  p <- ggbio() + circle(gr.cur, geom = "link", linked.to = "to.gr", aes(color = rearrangements)) +
    circle(hg19sub, geom = "ideo",   color = "gray70", fill = "gray70") +
    scale_color_manual(values = cols) +
    labs(title = (unique(values(gr.cur)$individual))) +
    theme(plot.margin = unit(rep(0, 4), "lines"))
})

arrangeGrobByParsingLegend(crc.lst, widths = c(4, 1), legend.idx = 1, ncol = 3)


############################################################################
#### Plot gene model (ENSEMBL) : http://www.ensembl.org # 
library(ggplot2)
library(EnsDb.Hsapiens.v75)
ensdb <- EnsDb.Hsapiens.v75

# In the example below we load an Ensembl based annotation package 
# for human gene and protein annotations defined in Ensembl version 75.

# It depends on  ensembldb package to retrieve gene/transcript/exons annotations 
# stored in an Ensembl based database 
# https://bioconductor.org/packages/release/bioc/vignettes/ensembldb/inst/doc/ensembldb.html

# Ensembl  transcript ID =  any spliced transcripts (ENST...) with overlapping coding sequence,
# 9 Ensembl transcript ID  shown here from GRCh37 genome

library(ensembldb )
(Tx <-transcripts(ensdb, filter = GeneNameFilter("GREB1")))


autoplot(ensdb, which=GeneNameFilter("GREB1")) 

# Growth regulation by estrogen in breast cancer 1
# autoplot is a generic function to visualize various data object
#  which = A GRanges object to subset the result to plot the transcripts


###################################################
#BiocManager::install("EnsDb.Hsapiens.v86")

#library(EnsDb.Hsapiens.v86)
ensdb2 <- EnsDb.Hsapiens.v86

# In the example below we load an Ensembl based annotation package 
# for human gene and protein annotations defined in Ensembl version 86.

Tx2 <- transcripts(ensdb2, filter = GeneNameFilter("GREB1"))
#GRanges object with 10 ranges from GRCh38 genome (Genome Reference Consortium Human Build 38)
autoplot(ensdb2, which=GeneNameFilter("GREB1")) #Growth regulation by estrogen in breast cancer 1


