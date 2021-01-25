library(sna)
library(tsna)
library(ndtv)

##
## Tworzenie sieci
##

nd = networkDynamic(basen.net = NULL, edge.toggles = NULL, vertex.toggles = NULL,
                    edge.spells = NULL, vertex.spells = NULL, edge.changes = NULL,
                    vertex.changes = NULL, network.list = NULL, onsets = NULL, termini = NULL,
                    vertex.pid = NULL, start = NULL, end = NULL, net.obs.period = NULL, verbox = TRUE, create.TEAs = FALSE,
                    edge.TEA.names = NULL, vertex.TEA.names = NULL)

##
## Wyznaczanie średniego degree
##

data(McFarland_cls33_10_16_96)
tDegree(cls33_10_16_96)
mean(tDegree(cls33_10_16_96), na.rm = TRUE)

##
## Temporalne ściezki i ich wizualizacja
##

data(moodyContactSim)
v10path <- tPath(moodyContactSim, v = 10, start = 0)
plot(v10path)

plotPaths(moodyContactSim, v10path)

v1path <- tPath(moodyContactSim, v = 1, start = 0)
plotPaths(moodyContactSim, list(v10path, v1path))

##
## Wczytanie gotowej sieci statycznej
##

StaticEdges <- read.csv('lab5/StaticEdgelist.csv')
thenetwork <- network(StaticEdges, directed = FALSE, bipartite = FALSE)
plot(thenetwork)

##
## Wczytanie węzłów i krawędzi dynamicznych
##

DynamicEdges <- read.csv('lab5/DynamicEdges.csv')

tn <- networkDynamic(
  thenetwork,
  edge.spells = DynamicEdges,
  vertex.spells = DynamicNodes)
network.dynamic.check(tn)
filmstrip(tn, displaylabels = FALSE)


##
## Animacja zmian w sieci dynamicznej
##

compute.animation(
  tn,
  animation.mode = 'kamadakawai',
  slice.par = list(
    start = 1260,
    end = 1300,
    interval = 1,
    aggregate.dur = 20,
    rule = 'any'
  )
)

render.d3movie(
  tn,
  displaylabels = FALSE
)

##
## Dynamika krawędzi
##

plot(tEdgeFormation(tn, time.interval = .25))

##
## Dynamika betweenness oraz degree
##

dynamicBetweenness <- tSnaStats(
  tn,
  snafun = "centralization",
  start = 1260,
  end = 1320,
  time.interval = 1,
  aggregate.dur = 20,
  FUN = 'betweenness'
)
plot(dynamicBetweenness)

dynamicDegree <- tSnaStats(
  tn,
  snafun = "centralization",
  start = 1260,
  end = 1320,
  time.interval = 1,
  aggregate.dur = 20,
  FUN = 'degree'
)
plot(dynamicDegree)

##
## Ścieżki fwd i bkwd
##

tReach(nd, direction = c('fwd', 'bkwd'), sample = network.size(nd), start, end, graph.step.time = 0)
fwd_reach <- tReach(tn)
bkwd_reach <- tReach(tn, direction = 'bkwd')
plot(fwd_reach, bkwd_reach)

##
## Wizualizacja fwd
##

FwdPath <- tPath(tn, v = 3, direction = 'fwd')
plotPaths(tn, FwdPath, displaylabels = FALSE, vertex.col = 'white')

##
## Wizualizacja bkwd
##

BkwdPath <- tPath(tn, v = 3, direction = 'bkwd', type = 'latest.depart')
plotPaths(tn, BkwdPath, path.col = rgb(0, 97, 255, max = 255, alpha = 166),
          edge.label.col = rgb(0, 0, 0, 0), vertex.col = 'white')
