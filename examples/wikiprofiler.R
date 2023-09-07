library(clusterProfiler)
library(wikiprofiler)

data(geneList, package="DOSE")

de <- names(geneList)[1:100]
x <- enrichWP(de, organism='Homo sapiens')


de2 <- bitr(de, "ENTREZID", "SYMBOL", OrgDb='org.Hs.eg.db')
#### if needed
# de2 <- de2[!duplicated(de2[,1]), ]
value <- setNames(geneList[de2[,1]], de2[,2])

ID <- x$ID[1]

p1 = wpplot(ID)
p2 = p1 |> wp_bgfill(value, low='darkgreen', high='firebrick', legend_x = .9, legend_y = .95)
p3 = p2 |> wp_shadowtext()

wpsave(p1, 'f1.png', width=10, height=7)
wpsave(p2, 'f2.png', width=10, height=7)
wpsave(p3, 'f3.png', width=10, height=7)
