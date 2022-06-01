```r
> ID
[1] "WP63_117935"
> value
   Pgd Taldo1   Rpia  G6pdx   Pgls    Tkt    Rpe 
31.027 67.677 20.317 17.210 41.867 45.300 12.217 
> 
> p1 = wpplot(ID)
> p2 = p1 |> wp_bgfill(value, low='darkgreen', high='firebrick')
> p3 = p2 |> wp_shadowtext()
> 
> g = aplot::plot_list(p1, p2, p3, design="AABB\nCCCC")
> print(g)
```

![](inst/figures/2022-06-01_17-58.png)

