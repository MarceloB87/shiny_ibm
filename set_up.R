#### This files sets the variables that sould stay at the global environment

# Read data files
files = list.files("./data")
ngenfiles = files[grep("ngen", files)]
gifiles = files[grep("gi", files)]
gjfiles = files[grep("gj", files)]
pfiles = files[grep("phen", files)]
spnumfiles = files[grep("spnum", files)]
rfiles = files[grep("host", files)]
gridfiles = files[grep("grid", files)]
 linfiles = files[grep("lineage", files)]

# Load parameters and simulation outputs
par = update.par()
#spp <- fread.out(1) 
load("./data/spp.Rdata")

alphas <- unique(par$alfa)
Rs <- unique(par$R)
show_n_digits = c(1,2,4,4,5,6,6)[7:1]

# Fixed parameters table

par_descriptions <- c("Grid size",
                      "Genome size",
                      "Fecundity", 
                      "Mutation rate",
                      "Genetic threshold",
                      "Number of probings",
                      "Host spatial correlation")

par_values <- c(50, 100, 4, 0.001, 5, 5, 5)

par_table = data.frame("Parameter" = par_descriptions, "Values" = par_values)

# Set colour palletes
pal_spatial = sample(viridis(max(spp$sp)), max(spp$sp))
pal_z = magma(max(spp$p) - min(spp$p) + 1)
pal_host = rev(tableau_seq_gradient_pal("Blue")(seq(0, 1, length = 71)))
pal_dif = rev(heat.colors(max(abs(spp$p - spp$host))+2))

#phen.list <-phen.time.list(spp) # Initial values
#phen.all <- give.phen.all(spp) # Initial values

# Set variables

max_gen = max(spp$ngen)

# host.list <- get.host.range(spp)
# max_host <- max(host.list$h)

## load("./data/spplist_final.RData")
#load("./data/phenall.rData")
load("./data/df_prop.Rdata")
load("./data/df_prop_first.Rdata")
load("./data/shiny_index.Rdata")
load("./data/first.Rdata")

## Bundle list


load("./data/spp1.Rdata")
load("./data/spp2.Rdata")
load("./data/spp3.Rdata")
load("./data/spp4.Rdata")
load("./data/spp5.Rdata")
load("./data/spp6.Rdata")
load("./data/spp7.Rdata")
load("./data/spp8.Rdata")
load("./data/spp9.Rdata")
load("./data/spp10.Rdata")
load("./data/spp11.Rdata")
load("./data/spp12.Rdata")
load("./data/spp13.Rdata")
load("./data/spp14.Rdata")
load("./data/spp15.Rdata")
load("./data/spp16.Rdata")
load("./data/spp17.Rdata")
load("./data/spp18.Rdata")
load("./data/spp19.Rdata")
load("./data/spp20.Rdata")
load("./data/spp21.Rdata")
load("./data/spp22.Rdata")
load("./data/spp23.Rdata")
load("./data/spp24.Rdata")
load("./data/spp25.Rdata")
load("./data/spp26.Rdata")
load("./data/spp27.Rdata")
load("./data/spp28.Rdata")
load("./data/spp29.Rdata")
load("./data/spp30.Rdata")
load("./data/spp31.Rdata")
load("./data/spp32.Rdata")
load("./data/spp33.Rdata")
load("./data/spp34.Rdata")
load("./data/spp35.Rdata")
load("./data/spp36.Rdata")
load("./data/spp37.Rdata")
load("./data/spp38.Rdata")
load("./data/spp39.Rdata")
load("./data/spp40.Rdata")
load("./data/spp41.Rdata")
load("./data/spp42.Rdata")



spp.list <- list(spp1, spp2, spp3, spp4, spp5, spp6, spp7, spp8, spp9, spp10,
                spp11, spp12, spp13, spp14, spp15, spp16, spp17, spp18, spp19, spp20,
                spp21, spp22, spp23, spp24, spp25, spp26, spp27, spp28, spp29, spp30,
                spp31, spp32, spp33, spp34, spp35, spp36, spp37, spp38, spp39, spp40,
                spp41, spp42)
##

load("./data/phenlist_par1.Rdata")
load("./data/phenlist_par2.Rdata")

phen.all.list = list()
phen.all.list[1:21] = phen.all.list_part1
phen.all.list[22:42] = phen.all.list_part2

##

hsi$small = 1 - hsi$small 
hsi$big = 1 - hsi$big 
hsi.both$both = 1 - hsi.both$both

sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, "Index"),
      th(rowspan = 2, "Lineage"),
      th(colspan = 2, "Positive"),
      th(colspan = 2, "Equal to 0"),
      th(colspan = 2, "Negative")
    ),
    tr(
      lapply(rep(c("Mean", "SD"), 3), th)
    )
  )
))

