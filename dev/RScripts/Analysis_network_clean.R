library(bipartite)
library(vegan)
library(network)
library(igraph)
library(gplots)

#Function to build a colored matrix (weighted one mode projection of the bipartite network. To be called like: myImagePlot(network)
# It has some problems, as the matrix (that should be a square matrix), gets a kind of rectangular representation, because the cells of each value are rectangular and not squares. I think I tried to fix that modifying the par(mar...), but I think it just modified the legend. I need to keep trying things, or maybe change the function to color the matrix. The most important thing is, WHICH INFORMATION IS THIS MATRIX GIVING US? It has to do with nestedness or not?
# CODE SOURCE: http://www.phaget4.org/R/image_matrix.html    http://www.phaget4.org/R/myImagePlot.R
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}
myImagePlot <- function(x){
    min <- min(x)
    max <- max(x)
    yLabels <- rownames(x)
    xLabels <- colnames(x)
    #title <-c()
    title <-"One-mode projection"
    # check for additional function arguments
    #if( length(list(...)) ){
    #  Lst <- list(...)
    #  if( !is.null(Lst$zlim) ){
    #    min <- Lst$zlim[1]
    #    max <- Lst$zlim[2]
    #  }
    #  if( !is.null(Lst$yLabels) ){
    #    yLabels <- c(Lst$yLabels)
    #  }
    #  if( !is.null(Lst$xLabels) ){
    #    xLabels <- c(Lst$xLabels)
    #  }
    #  if( !is.null(Lst$title) ){
    #    title <- Lst$title
    #  }
    #  }
    # check for null values
    if( is.null(xLabels) ){
        xLabels <- c(1:ncol(x))
    }
    if( is.null(yLabels) ){
        yLabels <- c(1:nrow(x))
    }
    
    layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
    
    # Red and green range from 0 to 1 while Blue ranges from 1 to 0
    ColorRamp <- rgb( seq(0,1,length=256),  # Red
    seq(0,1,length=256),  # Green
    seq(1,0,length=256))  # Blue
    ColorLevels <- seq(min, max, length=length(ColorRamp))
    
    # Reverse Y axis
    #reverse <- nrow(x) : 1
    #yLabels <- yLabels[reverse]
    #x <- x[reverse,]
    
    # Data Map
    #par(mar = c(3,5,2.5,2))
    par(mar = c(2,2,2,2))
    image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="Fungi",
    ylab="Fungi", axes=FALSE, zlim=c(min,max))
    if( !is.null(title) ){
        title(main=title)
    }
    axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
    axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
    cex.axis=0.7)
    
    # Color Scale
    #par(mar = c(3,2.5,2.5,2))
    par(mar = c(2,2,2,2))
    image(1, ColorLevels,
    matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
    col=ColorRamp,
    xlab="",ylab="",
    xaxt="n")
    
    layout(1)
}


#Upload Salva's matrices (which are of the size numberoftagsXnumberoftags). It is quite rudimentary, as I haven't found out the way into which I can read his matrix directly from a csv. I copy the csv into a .txt file (just changing the extension). Then I substitute all the ',' by nothing, so that I get the data separated by just spaces. When uploading this matrix into Python it was even worse, because there was not an explicit '\n', but a '\r' I recall. Syed solved that, so I should save that part of code, or either find out how to read a rare csv (it's not a common csv I think).
#IMPORTANT: I need to specify the number of columns!!!


tagsnum=128
salva_matrix = matrix(scan('~/Dropbox/CSSS/Projects/Lichens ecological network/SALVA SIMULATIONS/new_data/parasitism_sigmoid/data_observed_pairs_parasitism5_t2.txt'),ncol=tagsnum,byrow=TRUE) #rows = fungi; cols = algae


#Correcting the matrix from Salva, so as to eliminate all the rows and cols which are zeros, and have a proper bipartite matrix (for some analysis I needed that the zero cols/rows had been eliminated).

salva_matrix = matrix(scan('~/Downloads/genet_coocurrence_matrix.txt'),ncol=107,byrow=TRUE) ##AIXO ??S UN INTENT DE LLEGIR LA MATRIU D'EN SIMON. NO ACONSEGUIT. ARA NO RECORDO COM LA VAIG CARREGAR A R. HAURIA DE TORNAR ENRERE A L'R DEL PORT??TIL I VEURE QUINES COMANDES VAIG FER. LET'S SEE SI ENCARA APAREIXEN.
salva_matrix
length(salva_matrix)

bool_zero_rows<-rep(0,tagsnum)
bool_zero_rows
length(bool_zero_rows)
bool_zero_cols<-rep(0,tagsnum)
bool_zero_cols
length(bool_zero_cols)
salva_matrix

for (i in 1:tagsnum)
{
    bool = 1
    for (j in 1:tagsnum)
    {
        if (salva_matrix[i,j] == 1){
            bool = 0
        }
    }
    if (bool == 1)
    {
        bool_zero_rows[i] = 1
    }
    else
    {
        bool_zero_rows[i] = 0
    }
}

for (j in 1:tagsnum)
{
    bool = 1
    for (i in 1:tagsnum)
    {
        if (salva_matrix[i,j] == 1){
            bool = 0
        }
    }
    if (bool == 1)
    {
        bool_zero_cols[j] = 1
    }
    else
    {
        bool_zero_cols[j] = 0
    }
}

bool_zero_cols
bool_zero_rows
count_cols = 0
count_rows = 0
for (i in 1:tagsnum){
    if (bool_zero_cols[i] == 0){
        count_cols = count_cols + 1
    }
}
count_cols
for (i in 1:tagsnum){
    if (bool_zero_rows[i] == 0){
        count_rows = count_rows + 1
    }
}
count_rows

salva_corrected <- array(0, dim=c(count_rows,count_cols))
salva_corrected
ip=1
jp=1
for (i in 1:tagsnum){
    if (bool_zero_rows[i] == 0){
        jp=1
        for (j in 1:tagsnum){
            if (bool_zero_cols[j]==0){
                salva_corrected[ip,jp] = salva_matrix[i,j]
                jp = jp + 1
            }
        }
        ip = ip + 1
    }
}
salva_corrected
write.table(salva_corrected, file="~/Dropbox/CSSS/Projects/Lichens ecological network/SALVA SIMULATIONS/new_data/bipartite_matrices/C0M0P1_Sat0Sig1_t2m5.txt", row.names=FALSE, col.names=FALSE)

######## FROM HERE, YOU CAN GO TO THE MATLAB PROGRAM, WHERE MODULARITY AND NESTEDNESS ARE CALCULATED (AS WELL AS ITS STATISTICAL SIGNIFICANCE)

read.table("~/Dropbox/CSSS/Projects/Lichens ecological network/Real network data/simon_matrix.txt", header = FALSE, sep = "", quote = "\"'",
           dec = ".", numerals = c("allow.loss", "warn.loss", "no.loss"),
           row.names, col.names, as.is = !stringsAsFactors,
           na.strings = "NA", colClasses = NA, nrows = -1,
           skip = 0, check.names = TRUE, fill = !blank.lines.skip,
           strip.white = FALSE, blank.lines.skip = TRUE,
           comment.char = "#",
           allowEscapes = FALSE, flush = FALSE,
           stringsAsFactors = default.stringsAsFactors(),
           fileEncoding = "", encoding = "unknown", text, skipNul = FALSE)

salva_matrix=read.table("~/Dropbox/CSSS/Projects/Lichens ecological network/Real network data/simon_matrix.txt", header = FALSE, sep = "")
salva_matrix[1,2]
length(salva_matrix)
########  NOW STARTS THE ANALYSIS in R #############
#Sorting the matrix, so as the rows and columns are rearranged according to decreasing node degree
salva_sorted = sortweb(salva_matrix, sort.order="dec", sequence=NULL)
salva_sorted

#If we want to convert the matrix to NxN (N=fungi+algae), to do analysis that are coded for NxN matrices
as.one.mode(salva_matrix, fill = 0, project="full", weighted=TRUE)
#See one-mode projection, Newman, p.124
#If we want the to know the algae ("higher = cols")that are connected to the same fungi
algae_onemode=as.one.mode(salva_matrix, fill = 0, project="higher", weighted=TRUE)
algae_onemode
#If we want the to know the fungi ("lower = rows") that are connected to the same algae
fungi_onemode=as.one.mode(salva_matrix, fill = 0, project="lower", weighted=TRUE)
fungi_onemode
#Switch weighted to =FALSE if you don't want this matrix to be weighted.

algae_sorted = sortweb(algae_onemode, sort.order="dec", sequence=NULL)
algae_sorted
fungi_sorted = sortweb(fungi_onemode, sort.order="dec", sequence=NULL)
fungi_sorted

#Representing this one mode projections, as weighted matrices, ordered by decreasing degree both in rows and cols
myImagePlot(algae_sorted)
myImagePlot(fungi_sorted)
myImagePlot(salva_sorted)
#Several measures at the network level.
network_data = networklevel(salva_matrix, index="ALLBUTDD", level="both", weighted=FALSE,
ISAmethod="Bluethgen",  SAmethod = "Bluethgen", extinctmethod = "r",
nrep = 100, CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,
logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,
fcdist="euclidean", legacy=FALSE)
write.table(network_data, "~/Desktop/Network_level_measures.txt", sep="\t")

#Several measures at the species level.
species_data = specieslevel(salva_matrix, index="ALLBUTD", level="both", logbase=exp(1), low.abun=NULL,
high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)
write.table(species_data, "~/Desktop/Species_level_measures.txt", sep="\t")

##Visualization

#visbweb(...) This function draws a food web as a grid using a matrix. Colnames and rownames are used as labels and entries in the matrix are visualized by text and colours. It can also be used to plot bipartite webs in the style of Va??zquez et al. (2009). see p. 143 package 'Bipartite'
#Aqu?? m'hi deixava representar salva_matrix i salva_sorted, per?? no s?? perqu?? no em deixava representar algae_onemode ni fungi_onemode. I per aix?? vaig utilitzar la funci?? externa per pintar la matriu de colors.

visweb(salva_matrix, type="diagonal",  prednames=TRUE, preynames=TRUE, labsize=1,
plotsize=12, square="interaction", text="no", frame=NULL, textsize=1,
textcol="red", pred.lablength=10, prey.lablength=10, clear=TRUE,
xlabel="Fungi", ylabel="Algae", boxes=TRUE, circles=FALSE, circle.col="black",
circle.min=0.2, circle.max=2, outerbox.border="white",
outerbox.col="white", box.border="black", box.col="black", def.col="blue",
max.digits=4, NA.col="red")

#plotweb(...) A two dimensional matrix is plotted as a bipartite graph. (utilitat nul??la, i a m??s ??s molt lletja la representaci??, millor utilitzar gephi. Encara no s?? com utilitzar-lo, el tinc instal??lat. La J??lia el va saber fer anar, potser github hi ha els codis?don't think so) see p.93 package 'Bipartite'
plotweb(salva_sorted,
method = "normal", empty = TRUE, labsize = 1, ybig = 1,  y.width.low = 0.1,
y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL,
arrow="no",  col.interaction="grey80", col.high = "grey10",
col.low="grey10",  bor.col.interaction ="black", bor.col.high="black",
bor.col.low="black", high.lablength = NULL, low.lablength = NULL,
sequence=NULL, low.abun=NULL, low.abun.col="green",
bor.low.abun.col ="black", high.abun=NULL, high.abun.col="red",
bor.high.abun.col="black", text.rot=0, text.high.col="black",
text.low.col="black", adj.high=NULL, adj.low=NULL, plot.axes = FALSE,
low.y=0.5, high.y=1.5, add=FALSE, y.lim=NULL, x.lim=NULL, low.plot=TRUE,
high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL,
low.lab.dis = NULL, abuns.type="additional")


#CREATE EDGE LIST
web2edges(salva_corrected, webName=NULL, weight.column=FALSE, both.directions=FALSE,
is.one.mode=FALSE, out.files=c("edges", "names", "groups")[1:2],
return=FALSE, verbose=FALSE)  #try the function, because I remember that depending on the options, not always worked (Look at AnalysisNetworkLichens.R, where I have three copies of the same function)
web2edges(fungi_onemode, webName=NULL, weight.column=FALSE, both.directions=FALSE,
is.one.mode=FALSE, out.files=c("edges", "names", "groups")[1:2],
return=FALSE, verbose=FALSE) #aquest estava a AnalysisNetworksLichens2_canvisparalels.R

###TO STORE THE PLOTS!
pdf(file="~/Desktop/Degreedistr_both.pdf")
#myImagePlot(fungi_sorted) #WHATEVER COMMAND THAT PRODUCES A PLOT
dev.off()

#Degree distribution, fitted to power law, exponential function,
##Other stuff tried### -that I don't remeber if they work properly-
nestedness(salva_matrix, null.models = TRUE, n.nulls = 100, popsize = 30,n.ind = 7, n.gen = 2000, binmatnestout=FALSE)

nestedrank(sample, method = "NODF", weighted=TRUE, normalise=TRUE, return.matrix=FALSE)

nested(sample, method = "binmatnest2", rescale=FALSE, normalised=TRUE)

computeModules(salva_matrix, deep = FALSE, deleteOriginalFiles = TRUE,
steps = 1000000, tolerance = 1e-10, experimental = FALSE)
