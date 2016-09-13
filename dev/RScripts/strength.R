#strength(mat, type="Barrat") DOESN'T WORK!

strength
data(Safariland)
s1 <- strength(Safariland, type="Barrat")
s2 <- strength(Safariland, type="Bascompte")
plot(s1, s2, log="x")
cor.test(s1, s2, type="ken")
# for lower level:
strength(t(Safariland))




strength(graph) #FUNCIONA SI HO APLICO AL GRAPH SQUARE MATRIX CREAT A ASSORTATIVITY. 