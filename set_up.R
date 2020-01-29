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

load("./data/spplist_final.RData")
load("./data/phenall.rData")
load("./data/df_prop.rData")
load("./data/df_prop_first.rData")
load("./data/shiny_index.Rdata")
load("./data/first.Rdata")

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

