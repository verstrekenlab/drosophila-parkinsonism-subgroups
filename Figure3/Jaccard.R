### Jaccard similarity

### define groups based on sleep features:
behA <- c("Gdh", "Lrrk","VAC14", "vham89", "eIF4G", "Omi", "Vps13", "Vps35", "Synj", "Tango14", "Dj1aDj1b", "iPLA2VIA", "Rab39","GBA", "Punch", "Rme8")
behB <- c("CHCHD2", "Pink1", "Auxillin", "nutcracker", "anne", "Coq2", "Loqs", "park" )

### trees based on genetic interaction:
GI1 <- c("Punch", "Pink1","Auxillin", "Vps13", "CHCHD2","Rab39", "Omi","nutcracker","Vps35","Gdh", "park","anne")
GI2 <- c("Tango14","Lrrk","Dj1aDj1b", "iPLA2VIA","GBA","Synj","eIF4G","VAC14","Rme8","Coq2", "Loqs", "vham89")



### Jaccard similarity with tree a and subgroups defined by sleep
jaccard <- function(sleepgroup, GIgroup) {
  intersection = length(intersect(sleepgroup, GIgroup))
  union = length(sleepgroup) + length(GIgroup) - intersection
  return (intersection/union)
}

jaccard(behA, GI1)


jaccard(behB, GI1)


### Jaccard similarity with tree b and subgroups defined by sleep

jaccard(behA, GI2)

jaccard(behB, GI2)

