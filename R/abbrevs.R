tt = function(x) table(table(x))
lu = function(x) length(unique(x))
g = pillar::glimpse

'%nin%' = function(a, b) {
  (!(a %in% b))
}
