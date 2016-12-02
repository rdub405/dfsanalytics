library(GA)

ff.data <- bk


ff.data2 <- ff.data[,c("Position", "Name", "Salary", "DFPoints","teamAbbrev","AvgPointsPerGame","CEILING")]

colnames(ff.data2) <- c("Position", "Name", "Salary", "Points1", "Team","Points2", "Points3");
ff.data2$Points = ff.data2$Points3
ff.data2$Points[is.na(ff.data2$Points3)] <- ff.data2$Points1[is.na(ff.data2$Points3)]

NumLineups <- 100
Population <- 400
Generations <- 10
MinTeams <- 2
MaxTeams <-9
Prefix <- 'GA'
Salary <- 60000

getplayers2 <- function(data, pos) {
  return(unlist(which(data[[1]] %in% pos)  ));
}

f.pg = getplayers2(ff.data2, c("PG"))
f.sg = getplayers2(ff.data2, c("SG"))
f.sf = getplayers2(ff.data2, c("SF"))
f.pf = getplayers2(ff.data2, c("PF"))
f.c = getplayers2(ff.data2, c("C"))
#ff.fl = getplayers(ff.data, c("sf", "pf", "sg"))

ffscore2 <- function(x) {
  team_idx = ffteam2(x)
  team = ff.data2[team_idx,]
  # can't be above 60k
  if( sum(team$Salary) > 60000) { return(0); }
  
  # can't have the same player
  if( length(unique(team$Name)) < 9) { return(0); }
  
  if( length(unique(team$Team)) < MinTeams || length(unique(team$Team)) > MaxTeams) { return(0); }
  
  return(sum(team$Points));
}

ffteam2 <- function(x) {
  # Find players
  pg1 = ceiling(x[1]);
  sg1 = ceiling(x[2]);
  sg2 = ceiling(x[3]);
  sf1 = ceiling(x[4]);
  sf2 = ceiling(x[5]);
  pf1 = ceiling(x[6]);
  pf2 = ceiling(x[7]);
  pg2 = ceiling(x[8]);
  ds = ceiling(x[9]);
  
  return( c(f.pg[pg1], f.sg[sg1], f.sg[sg2], f.sf[sf1], f.sf[sf2],f.pf[pf1], f.pf[pf2], f.pg[pg2], f.c[ds]));
}

maketeam <- function(x) {
  team_idx = ffteam2(x)
  team = ff.data2[team_idx,]
  return(team);
}


getplayers <-function(data, pos) {
  res = data[data[["Position"]] %in% pos, c("Position", "Name", "Salary", "DFPoints","teamAbbrev","AvgPointsPerGame","CEILING")];
  colnames(res) <- c("Position", "Name", "Salary", "Points1", "Team","Points2", "Points3");
  res$Points = res$Points3
  res$Points[is.na(res$Points3)] <- res$Points1[is.na(res$Points3)]
  
  return(res);
}

ff.pg = getplayers(ff.data, c("PG"))
ff.sg = getplayers(ff.data, c("SG"))
ff.sf = getplayers(ff.data, c("SF"))
ff.pf = getplayers(ff.data, c("PF"))
ff.c = getplayers(ff.data, c("C"))

maxdim = c(length(f.pg), length(f.sg), length(f.sg), length(f.sf), length(f.sf),  length(f.pf), length(f.pf), length(f.pg), length(f.c))

all.results = list()
for(i in 1:NumLineups) {
  out = ga(type="real-valued", fitness=ffscore2, min=rep(0, 9), max=maxdim, popSize=Population, maxiter=Generations, keepBest=TRUE, parallel=FALSE)
  team = maketeam(out@solution)
  team$Run = paste(Prefix,i)
  team$Fitness = ffscore2(out@solution)
  all.results = c(all.results, list(team))
}

all.teams = do.call("rbind", all.results)

LineupsTest <- all.teams

LineupsTest
#write.csv(LineupsTest, file = "test.csv")


