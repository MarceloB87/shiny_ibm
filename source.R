read.out = function(i) {
ngen = read.table(ngenfiles[i])
gi = read.table(gifiles[i])
gj = read.table(gjfiles[i])
phen = read.table(pfiles[i])
spnum = read.table(spnumfiles[i])
host = read.table(rfiles[i])
spp = data.frame(ngen = ngen, gi = gi, gj = gj, phe = phen, spnum = spnum, host = host)
names(spp) = c("ngen", "i", "j", "p", "sp", "host")
return(spp)
}

# For big files
fread.out = function(i){
ngen = fread(ngenfiles[i])
gi = fread(gifiles[i])
gj = fread(gjfiles[i])
phen = fread(pfiles[i])
spnum = fread(spnumfiles[i])
host = fread(rfiles[i])
spp = data.frame(ngen = ngen, gi = gi, gj = gj, phe = phen, spnum = spnum, host = host)
names(spp) = c("ngen", "i", "j", "p", "sp", "host")
return(spp)
}

# Read parameters
update.par = function(){
par = read.csv("data/parameters.txt", header = F)
names(par) = c("nsim","ngen","gridi","gridj","K","genome","n_offspring","rmut","drate","alfa","R","Nattempt","modelrep","modeldisp","modelresource","G")
return(par)
}
# Load data
par = update.par()

# files = list.files("./data")
# ngenfiles = files[grep("ngen", files)]
# gifiles = files[grep("gi", files)]
# gjfiles = files[grep("gj", files)]
# pfiles = files[grep("phen", files)]
# spnumfiles = files[grep("spnum", files)]
# rfiles = files[grep("host", files)]
# gridfiles = files[grep("grid", files)]
# linfiles = files[grep("lineage", files)]

fread.out <- function(i){
ngen = fread(paste0("data/",ngenfiles[i]))
gi = fread(paste0("data/",gifiles[i]))
gj = fread(paste0("data/",gjfiles[i]))
phen = fread(paste0("data/",pfiles[i]))
spnum = fread(paste0("data/",spnumfiles[i]))
host = fread(paste0("data/",rfiles[i]))
spp = data.frame(ngen = ngen, gi = gi, gj = gj, phe = phen, spnum = spnum, host = host)
names(spp) = c("ngen", "i", "j", "p", "sp", "host")
return(spp)}


# Plot spatial position at time t

plot.spatial <- function(spp, t, pal, pal_z, pal_host, pal_dif, type = "sp_id") { 

sppinsta = spp %>% filter(ngen == t)

if(type == "sp_id"){color = pal[sppinsta$sp]}
if(type == "phenotype"){color = pal_z[sppinsta$p - min(spp$p) + 1]}
if(type == "host"){color = pal_host[sppinsta$host - 14]}
if(type == "dif"){color = pal_dif[abs(sppinsta$p - sppinsta$host)+2]}

par(mar = c(0,2,4,2))  
plot(sppinsta$i-0.5, sppinsta$j-0.5, col = alpha(color, 1), pch = 15, cex = 1.7,
     xlab = "", ylab = "", xaxt = "n", yaxt = "n", xlim = c(-0.1,50),
     ylim = c(-0.1,50.1), xaxs = "i", yaxs = "i", main = paste0("Generation: ", t))
}



#plot.spatial(spp, 100, pal, pal_z, pal_host, type = "host")

# # Plot barplot de abundância de fenótipo de cada espécie no instante t.

plot.phen = function(spp, t, pal, ymax, xlim){
  sppgen <- spp %>% filter(ngen == t)
  sps = unique(sppgen$sp)
  
  plot(1,1,type="n",ylim = c(0,ymax), xlim = xlim,
       xlab = "Phenotype", ylab = "Number of Individuals", yaxs = "i", xaxs = "i",
       main = paste0("Generation: ", t), cex.lab = 1)
  for(s in 1:length(sps)){
    ss = sps[s]
    data <- sppgen %>% filter(sp == ss)
    hist(data$p,  breaks = 1:max(data$p), col = alpha(pal[ss],0.9), 
       border = pal[ss], add = TRUE)
  }
  axis(1, labels = F, at = xlim)
  }

## Plot number of species x time

plot.diver <- function(diversity, t) {
par(mar = c(4,4,2,2))  
plot(diversity, type = "l", xlab = "Generation", ylab = "Species richness", lwd = 2, 
     col = "steelblue", xlim = c(0,1500), ylim = c(0,180))
abline(v = t, lty = 3)
points(diversity[t,], pch = 16, col = 2)
}

# Create list of phenotype distribution

phen.time.list <- function(spp) {
  phen.list = list()
  sps = unique(spp$sp)
  for(s in 1:length(sps)){
 # for(s in 1:10){
  ss = sps[s]
  sppsp <- spp %>% filter(sp == ss)
  phen.dist <- sppsp %>% select(ngen, p) %>% group_by(ngen) %>% distinct()
  phen.list[[s]] = phen.dist
  }
  return(phen.list)
}

  give.phen.all <- function(spp) {
  
  phen.all <- spp %>% select(ngen, p) %>% group_by(ngen) %>% distinct()
 
  return(phen.all)
}


# Plot phenotype distribution of one species x time

plot.phen.sp <- function(phen.all, species, yrange = c(15,85)){

sppsp <- spp %>% filter(sp == species)
phen.dist <- sppsp %>% select(ngen, p) %>% group_by(ngen) %>% distinct()  
    
plot(phen.all, pch = 16, col = "grey", xlim = c(0, 1500), ylim = c(yrange[1],yrange[2]), xlab = "Generations", ylab = "Phenotype")
points(phen.dist, pch = 16, col = alpha(viridis(1), 0.5))
    
#points(phen.list[[sp]], pch = 16, col = alpha(viridis(1), 0.5))
}

# Plot abundance of one species x time

get.abund.list <- function(spp, sps){
  abund.list = list()
  sps = unique(spp$sp)
 # for (s in 1:10){
  for (s in sps){
    species = s
    sp.a <- spp %>% filter(sp == species) %>% select(ngen, sp) %>% group_by(ngen) %>% summarize(a = n())
    abund.list[[s]] = sp.a
  }
  return(abund.list)
}

plot.abund.sp <- function(abund.list, sps, species, ymax){
  
  plot(1,1, type = "n", xlab = "Generations", ylab = "Number of individuals",
       xlim = c(0,1500), ylim = c(0,ymax))
  
  #for(s in c(1:10)[-species]){
  for(s in sps[-species]){
    lines(abund.list[[s]], col = "grey")  
  }
  lines(abund.list[[species]], col = 2)
}

## Get host number

get.host.range <- function(spp){
  host.list <- spp %>% group_by(sp, ngen) %>% summarize(h = n_distinct(host))
  return(host.list)
}

plot.host.sp <- function(host.list, species, sps, ymax = 15) {

plot(1,1, type = "n", xlim = c(0,1500), ylim = c(0,ymax), xlab = "Generations", ylab = "Number of hosts")

for(s in sps[-species]){
  host_sp <- host.list %>% filter(sp == s)  
  lines(host_sp$ngen, host_sp$h, col = "grey")
}
  
host_sp <- host.list %>% filter(sp == species)  
if (nrow(host_sp)>1){
lines(host_sp$ngen, host_sp$h, col = alpha("red", 0.8)) } else {
points(host_sp$ngen, host_sp$h, col = alpha("red", 0.8), pch = 16, cex = 1)  
}

}

## Draw circle radius

draw.radius <- function(R = 3, selected = 3) {
par(mar = rep(2,4))
plot(1,1,type = "n", xlim = c(0,25), ylim = c(0,25), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
grid(25, 25, lwd = 1, lty = 1)
for(ax in 1:4){axis(ax, labels = F, lwd.tick = 0)}
points(12.5, 12.5, pch = 16)
draw.circle(12.5,12.5, R, lwd = 1.7, border = "steelblue")
draw.circle(12.5,12.5, selected, lwd = 2, border = "red")
}


# plot alpha

plot.alpha <- function(alpha, alphas, selected) {
  
  par(mar = c(4,4,2,2))
  z = c(seq(from=-60,to=60,by=0.1))
  w_selected = exp(-alphas[ abs(selected - 8) ]*((z-0)^2))
  w = exp(-alphas[ abs(alpha - 8) ]*((z-0)^2))
  
  #Axis labels
  ylab = expression(paste("Consumer fitness (", italic(W[i]), ")" ))
  xlab = expression(paste(Phenotypic," ",fit," ","(",italic(p[i,j]))- paste(italic(r[i,j]),")"))
  
  plot(z, w, type="l", ylab = ylab, xlab = xlab, lwd=1.7, ylim = c(0,1), xlim = c(-50,50),
       col="steelblue")
  if(is.integer(selected)){lines(z, w_selected, col = 2, lwd = 2)}
  
}

# Update color pallete

# Set colour palletes
return.pallete.list <- function(spp) {
pal.list = list()

pal.list$spatial <- sample(viridis(max(spp$sp)), max(spp$sp))
pal.list$z = magma(max(spp$p) - min(spp$p) + 1)
pal.list$host = rev(tableau_seq_gradient_pal("Blue")(seq(0, 1, length = 71)))
pal.list$dif = rev(heat.colors(max(abs(spp$p - spp$host))+2))

return(pal.list)
}


### Plot index

plot.proportion <- function(index_table, lin_abundance, index_name, limit, alphas, sign, show.text = FALSE){

if(index_name == "HEI"){
  lin_abundance = "both"
}  
  
if(lin_abundance == "small"){
if(sign == "positive"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(small > 0) /n()))}
if(sign == "equal"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(small == 0) /n()))}
if(sign == "negative"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(small < 0) /n()))}
}
                                                       

if(lin_abundance == "big"){
  if(sign == "positive"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(big > 0) /n()))}
  if(sign == "equal"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(big == 0) /n()))}
  if(sign == "negative"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(big < 0) /n()))}
}


if(lin_abundance == "both"){
  if(sign == "positive"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(both > 0) /n()))}
  if(sign == "equal"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(both == 0) /n()))}
  if(sign == "negative"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(both < 0) /n()))}
}

p <- ggplot(prop, aes(R, log(alfa))) + geom_tile(aes(fill = prop),
  colour = "white") + scale_fill_gradient2(index_name, low = "red", mid = "white",
   high = "royalblue4", midpoint = 0, limits = c(0,limit)) +
  labs(x = "Dispersal radius (R)", y = expression("Intensity of selection (log("*alpha*"))")) +
  scale_x_continuous(breaks=seq(1, 111, 2)) + 
  scale_y_continuous(breaks=round(log(alphas),1)) 

if(show.text == TRUE){
p2 = p + geom_text(aes(label = round(prop, 2)))
plot(p2) } else {
plot(p)
}
  
}

#max.i = get.max.prop(oi.df, "small")
#plot.proportion(hsi, lin_abundance = "big", index_name = "OI", limit = 0.1, alphas, sign = "equal", show.text = TRUE)


get.max.prop <- function(index_table, lin_abundance = "small"){
if(lin_abundance == "small"){
prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(small > 0) /n()),
                                                       prop_eq = (sum(small == 0) /n()),
                                                       prop_neg = (sum(small < 0) /n()))
}
if(lin_abundance == "big"){
  prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(big > 0) /n()),
                                                         prop_eq = (sum(big == 0) /n()),
                                                         prop_neg = (sum(big < 0) /n()))
}
if(lin_abundance == "both"){
  prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(both > 0) /n()),
                                                         prop_eq = (sum(both == 0) /n()),
                                                         prop_neg = (sum(both < 0) /n()))
}
return(data.frame(positive = max(prop$prop), equal = max(prop$prop_eq), negative = max(prop$prop_neg)) + 0.01)
}

####

#plot.proportion <- function(index_table, lin_abundance, index_name, limit, alphas, sign, show.text = FALSE){
# sign = "positive"
# index_table <- hsi
#   
get.proportion <- function(index_table, lin_abundance, sign, index_name){

  if(index_name == "HEI"){
    lin_abundance = "both"
  }  
  
  if(lin_abundance == "small"){
    if(sign == "positive"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(small > 0) /n()))}
    if(sign == "equal"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(small == 0) /n()))}
    if(sign == "negative"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(small < 0) /n()))}
  }
  
  
  if(lin_abundance == "big"){
    if(sign == "positive"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(big > 0) /n()))}
    if(sign == "equal"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(big == 0) /n()))}
    if(sign == "negative"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(big < 0) /n()))}
  }
  
  
  if(lin_abundance == "both"){
    if(sign == "positive"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(both > 0) /n()))}
    if(sign == "equal"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(both == 0) /n()))}
    if(sign == "negative"){prop = index_table %>% group_by(R, alfa) %>% summarize(prop = (sum(both < 0) /n()))}
  }
  
return(prop)
}

###
boxplot.R <- function(prop_table, index_name, ylimit){
par(mar = c(4,4,2,2))
boxplot(prop_table$prop ~ prop_table$R, xlab = "Dispersal radius (R)", ylab = index_name, pch = 16,
                  col = "steelblue", ylim = c(0,ylimit)) 
}

##

boxplot.alpha <- function(prop_table, index_name, alphas, ylimit){

xlabels = formatC(alphas, format = "e", digits = 1)[7:1]
xlab_alpha = expression("Intensity of selection (log("*alpha*"))")

par(mar = c(4,4,2,2))
boxplot(prop_table$prop ~ prop_table$alfa, xaxt = "n", xlab = xlab_alpha, ylab = index_name, pch = 16,
                  col = "steelblue", ylim = c(0, ylimit))

axis(1, at = 1:7, labels = xlabels)
}

###

boxplot.index.r <- function(index_table, index_name, ylimit, adjusty = FALSE){
  par(mar = c(4,4,2,2))
  
  if(adjusty == FALSE){
  boxplot(index_table$mean ~ index_table$R, xlab = "Dispersal radius (R)", ylab = index_name, pch = 16,
          col = "steelblue")  } else {
  boxplot(index_table$mean ~ index_table$R, xlab = "Dispersal radius (R)", ylab = index_name, pch = 16,
          col = "steelblue", ylim = ylimit)          
          }
}

##

boxplot.index.alpha <- function(index_table, index_name, alphas, ylimit, adjusty = FALSE){
  
  xlabels = formatC(alphas, format = "e", digits = 1)[7:1]
  xlab_alpha = expression("Intensity of selection (log("*alpha*"))")
  
  par(mar = c(4,4,2,2))
  if(adjusty == FALSE){
  boxplot(index_table$mean ~ index_table$alfa, xaxt = "n", xlab = xlab_alpha, ylab = index_name, pch = 16,
          col = "steelblue")  } else {
  boxplot(index_table$mean ~ index_table$alfa, xaxt = "n", xlab = xlab_alpha, ylab = index_name, pch = 16,
          col = "steelblue", ylim = ylimit)         
            
          }
  
  axis(1, at = 1:7, labels = xlabels)
}


###

filter.index.means <- function(index_table, lin_abundance, index_name = "OI", filter = "positive"){
  
  if(index_name == "HEI"){
    lin_abundance = "both"
  }  
  
  if(lin_abundance == "small"){
    if(filter == "positive"){
    means_table = index_table %>% filter(small > 0) } else{
      means_table = index_table %>% filter(small < 0)
    }
  }
  
  
  if(lin_abundance == "big"){
    if(filter == "positive"){
      means_table = index_table %>% filter(big  >0) } else{
        means_table = index_table %>% filter(big < 0)
      }
  }
  
  
  if(lin_abundance == "both"){
    if(filter == "positive"){
      means_table = index_table %>% filter(both  >0) } else{
        means_table = index_table %>% filter(both < 0)
      }
  }
  
  return(means_table)
}

###
get.index.means <- function(index_table, lin_abundance, index_name = "OI"){
  
  if(index_name == "HEI"){
    lin_abundance <- "both"
  }  
  
  if(lin_abundance == "small"){
    means_table = index_table %>% group_by(R, alfa) %>% summarize(mean = mean(small))
  }
  
  
  if(lin_abundance == "big"){
    means_table = index_table %>% group_by(R, alfa) %>% summarize(mean = mean(big))
  }
  
  
  if(lin_abundance == "both"){
    means_table = index_table %>% group_by(R, alfa) %>% summarize(mean = mean(both))
  }
  
  return(means_table)
}

###
plot.index.means <- function(means_table, alphas, index_name, ylimit, show.text = TRUE, adjusty = FALSE) {

if(adjusty == FALSE){
  limit_range = range(means_table$mean)
} else {
  limit_range = ylimit
}
  
p <- ggplot(means_table, aes(R, log(alfa))) + geom_tile(aes(fill = mean),
        colour = "white") + scale_fill_gradient2(index_name, low = "red", mid = "white",
        high = "royalblue4", midpoint = 0, limits = limit_range) +
        labs(x = "Dispersal radius (R)", y = expression("Intensity of selection (log("*alpha*"))")) +
        scale_x_continuous(breaks=seq(1, 111, 2)) + 
        scale_y_continuous(breaks=round(log(alphas),1)) 

if(show.text == TRUE){
  p2 = p + geom_text(aes(label = round(mean, 2)))
  plot(p2) } else {
    plot(p)
  }

}

# 
hist.prop <- function(index_plot, lin_abundance, sign, index_name){
gp <- get.proportion(index_plot, lin_abundance, sign = sign, index_name = index_name)

if(sum(gp$prop) > 0) {
hist(gp$prop, xlab = index_name, col = "steelblue", main = "", xlim = c(0,1))
abline(v = mean(gp$prop), col = "orange", lty = 2, lwd = 2) } else
{
hist(-15, xlab = index_name, col = "steelblue", main = "", xlim = c(0,1), ylim = c(0,42), freq = FALSE)  
}

}


###

plot.legend = function(spp, pal, input.type){
  grid = matrix(1:length(pal), 1, length(pal))
  
  side = nrow(grid)
  par(mar = c(18,2,2,2))
  plot(1,1,type = "n", panel.first = grid(), xaxs="i", yaxs="i",
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
       xlim=c(0,ncol(grid)),ylim=c(0,nrow(grid)), main = "")
  
  mtext(input.type, outer = T, cex = 1.2, side = 3, line = -1.5)
  if(input.type == "Species"){
    g = viridis(length(pal))
  } else {
    g = pal
  }
  
  for(i in 1:nrow(grid)){
    for(j in 1:ncol(grid)){
      polygon(c(0+j-1,j,j,0+j-1),c(side-i,side-i,side-i+1,side-i+1), col = g[grid[i,j]], border = NA)
    }}
  
  box(which = "plot", lty = "solid")
  
  if(input.type != "Species"){
    if(input.type == "Phenotype"){adj = min(spp$p)}
    if(input.type == "Host"){adj = 14}
    if(input.type == "Phenotype-Host difference"){adj = 2}
    
    lab.pos <- round(c(1,length(pal)*0.5, length(pal))+0.001)
    axis(1, at = lab.pos - 0.5, labels = lab.pos + adj)
  }
}

