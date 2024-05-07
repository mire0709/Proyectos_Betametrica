PROYECTO UNIDAD 5
================
Mireya Rios
2024-05-01

Del archivo BOL_BP_MAY_2017 seleccione los siguientes indicadores, para
los bancos, sin considerar las columnas de TOTALES.1.Activos
productivos/total activos,2.Morosidad cartera total,3.Gastos de
Operación/Margen Financiero,4.Rentabilidad del ejercicio/ activo
promedio,5.Fondos disponibles/total depositos corto plazo

``` r
library(readxl)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(cluster)
library(devtools)
```

    ## Warning: package 'devtools' was built under R version 4.3.3

    ## Loading required package: usethis

    ## Warning: package 'usethis' was built under R version 4.3.3

``` r
library(factoextra)
```

    ## Loading required package: ggplot2

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(fpc)
```

    ## Warning: package 'fpc' was built under R version 4.3.3

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
base<-read_excel("C:\\Users\\USUARIO\\Desktop\\R CURSO\\MODULO5\\MATERIAL\\MACHINE-LEARNING-1_SL8ZUQ\\MACHINE LEARNING 1\\parte 2\\BOL_BP_MAY_ 2017.xlsx",sheet = "INDICADORES",skip=6)
```

    ## New names:
    ## • `` -> `...2`

``` r
names(base)
```

    ##  [1] "COD. MAF."                       "...2"                           
    ##  [3] "NOMBRE DEL INDICADOR"            "BP GUAYAQUIL"                   
    ##  [5] "BP PACIFICO"                     "BP PICHINCHA"                   
    ##  [7] "BP PRODUBANCO"                   "BANCOS PRIVADOS GRANDES"        
    ##  [9] "BP AUSTRO"                       "BP BOLIVARIANO"                 
    ## [11] "BP CITIBANK"                     "BP GENERAL RUMIÑAHUI"           
    ## [13] "BP INTERNACIONAL"                "BP LOJA"                        
    ## [15] "BP MACHALA"                      "BP SOLIDARIO"                   
    ## [17] "BP PROCREDIT"                    "BANCOS PRIVADOS MEDIANOS"       
    ## [19] "BP AMAZONAS"                     "BP COMERCIAL DE MANABI"         
    ## [21] "BP LITORAL"                      "BP COOPNACIONAL"                
    ## [23] "BP CAPITAL"                      "BP FINCA"                       
    ## [25] "BP DELBANK"                      "BP D-MIRO S.A."                 
    ## [27] "BP BANCODESARROLLO"              "BP VISIONFUND ECUADOR"          
    ## [29] "BANCOS PRIVADOS PEQUEÑOS"        "TOTAL BANCOS PRIVADOS"          
    ## [31] "BANCOS PRIVADOS COMERCIALES"     "BANCOS PRIVADOS CONSUMO"        
    ## [33] "BANCOS PRIVADOS VIVIENDA"        "BANCOS PRIVADOS DE MICROEMPRESA"

``` r
final<-base %>% select(- c("...2","BANCOS PRIVADOS GRANDES","BANCOS PRIVADOS MEDIANOS","BANCOS PRIVADOS PEQUEÑOS",
                           "TOTAL BANCOS PRIVADOS","BANCOS PRIVADOS COMERCIALES","BANCOS PRIVADOS CONSUMO","BANCOS PRIVADOS VIVIENDA","BANCOS PRIVADOS DE MICROEMPRESA","COD. MAF."))
final$`NOMBRE DEL INDICADOR`<-trimws(final$`NOMBRE DEL INDICADOR`)
names(final)
```

    ##  [1] "NOMBRE DEL INDICADOR"   "BP GUAYAQUIL"           "BP PACIFICO"           
    ##  [4] "BP PICHINCHA"           "BP PRODUBANCO"          "BP AUSTRO"             
    ##  [7] "BP BOLIVARIANO"         "BP CITIBANK"            "BP GENERAL RUMIÑAHUI"  
    ## [10] "BP INTERNACIONAL"       "BP LOJA"                "BP MACHALA"            
    ## [13] "BP SOLIDARIO"           "BP PROCREDIT"           "BP AMAZONAS"           
    ## [16] "BP COMERCIAL DE MANABI" "BP LITORAL"             "BP COOPNACIONAL"       
    ## [19] "BP CAPITAL"             "BP FINCA"               "BP DELBANK"            
    ## [22] "BP D-MIRO S.A."         "BP BANCODESARROLLO"     "BP VISIONFUND ECUADOR"

``` r
final <- final %>% 
  filter(`NOMBRE DEL INDICADOR` %in% c("ACTIVOS PRODUCTIVOS / TOTAL ACTIVOS",
                                       "MOROSIDAD DE LA CARTERA TOTAL",
                                       "GASTOS DE OPERACION  / MARGEN FINANCIERO",
                                       "RESULTADOS DEL EJERCICIO / ACTIVO PROMEDIO",
                                       "FONDOS DISPONIBLES / TOTAL DEPOSITOS A CORTO PLAZO"))

#convertir en numericos

final<-as.data.frame(t(final))
colnames(final) <- final[1, ]
final <- final[-1, ] 
final <- final %>%
  mutate_all(as.numeric)
```

Construya un cluster jerarquico. Grafique y comente su composición,
usando al menos 2 distancias y dos métodos de clasificación

``` r
#Previamente se escalan las variables para evitar influya de manera arbitraria alguna de las variables
data <- as.data.frame(scale(final))
#Distancia Euclideana - Metodo wards
cluster<-hclust(dist(data,method ="euclidean"),
                method = "ward.D")
plot(cluster,hang = -0.01,cex=0.8)
```

![](PROYECTO_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#Distancia Euclideana - Metodo Promedio
cluster2<-hclust(dist(data,method ="euclidean"),
                method = "average")
plot(cluster2,hang = -0.01,cex=0.8)
```

![](PROYECTO_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
par(mfrow=c(1,2))
plot(cluster,hang = -0.01,cex=0.8);plot(cluster2,hang = -0.01,cex=0.8)
```

![](PROYECTO_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

``` r
#distancia<-dist(data,method ="euclidean")
#distancia #mientras más cercano a 0 este, menor distancia
cluster$merge
```

    ##       [,1] [,2]
    ##  [1,]   -5  -11
    ##  [2,]   -1   -2
    ##  [3,]   -3   -4
    ##  [4,]   -6    2
    ##  [5,]   -8  -17
    ##  [6,]  -14  -22
    ##  [7,]   -9    3
    ##  [8,]  -10    4
    ##  [9,]  -12    5
    ## [10,]  -13    6
    ## [11,]   -7    7
    ## [12,]  -19   10
    ## [13,]  -20  -21
    ## [14,]    1    8
    ## [15,]   11   14
    ## [16,]  -15  -16
    ## [17,]   12   13
    ## [18,]   16   17
    ## [19,]    9   15
    ## [20,]   18   19
    ## [21,]  -23   20
    ## [22,]  -18   21

``` r
grupos<-as.data.frame(cutree(cluster, k=4))

#Metodo divisivo: DIANA
ncluster<-diana(data,metric="euclidean") #divisible 0.82 cercano a 1, buena clasificación.
par(mfrow=c(1,2))
plot(ncluster)
```

![](PROYECTO_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->

``` r
plot(cluster,hang = -0.01,cex=0.8,main = "Dendrogram of DIANA")
cluster1<-hcut(data,k=4,stand=T,
               hc_metric = "euclidean",
               hc_method = "ward.D")
fviz_dend(cluster1, rect=T,
          cex=0.5,
          k_colors=c("blue","red","black","skyblue"))
```

    ## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
    ## of ggplot2 3.3.4.
    ## ℹ The deprecated feature was likely used in the factoextra package.
    ##   Please report the issue at <https://github.com/kassambara/factoextra/issues>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
fviz_cluster(object = list(data=data, cluster=cutree(cluster, k=4)),
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE,
             labelsize = 8)
```

![](PROYECTO_files/figure-gfm/unnamed-chunk-2-5.png)<!-- -->
\##Construya un cluster no jerarquico. Determine el número óptimo de
clusters, grafique y comente.

``` r
###cálculo de los clúster óptimos
library(NbClust)
clusteroptimo<-NbClust(data,
                       distance = "euclidean",
                       min.nc=2,
                       max.nc=6,
                       method = "ward.D",
                       index="all")
```

![](PROYECTO_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

    ## *** : The Hubert index is a graphical method of determining the number of clusters.
    ##                 In the plot of Hubert index, we seek a significant knee that corresponds to a 
    ##                 significant increase of the value of the measure i.e the significant peak in Hubert
    ##                 index second differences plot. 
    ## 

![](PROYECTO_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

    ## *** : The D index is a graphical method of determining the number of clusters. 
    ##                 In the plot of D index, we seek a significant knee (the significant peak in Dindex
    ##                 second differences plot) that corresponds to a significant increase of the value of
    ##                 the measure. 
    ##  
    ## ******************************************************************* 
    ## * Among all indices:                                                
    ## * 6 proposed 2 as the best number of clusters 
    ## * 13 proposed 3 as the best number of clusters 
    ## * 2 proposed 4 as the best number of clusters 
    ## * 1 proposed 5 as the best number of clusters 
    ## * 2 proposed 6 as the best number of clusters 
    ## 
    ##                    ***** Conclusion *****                            
    ##  
    ## * According to the majority rule, the best number of clusters is  3 
    ##  
    ##  
    ## *******************************************************************

``` r
#3 cluster
cluster_nojerarquico2<-kmeans(data,3)
cluster_nojerarquico2 # 62.3% como porcentaje de separación, ,mientras más cercano a 1, existe una mayor calidad en el modelo
```

    ## K-means clustering with 3 clusters of sizes 1, 1, 21
    ## 
    ## Cluster means:
    ##   ACTIVOS PRODUCTIVOS / TOTAL ACTIVOS MOROSIDAD DE LA CARTERA TOTAL
    ## 1                        -1.812706737                     3.5968638
    ## 2                         1.788055120                    -0.3408909
    ## 3                         0.001173887                    -0.1550463
    ##   GASTOS DE OPERACION  / MARGEN FINANCIERO
    ## 1                                4.1344555
    ## 2                               -0.4479673
    ## 3                               -0.1755471
    ##   RESULTADOS DEL EJERCICIO / ACTIVO PROMEDIO
    ## 1                                -2.82258654
    ## 2                                 2.01635092
    ## 3                                 0.03839217
    ##   FONDOS DISPONIBLES / TOTAL DEPOSITOS A CORTO PLAZO
    ## 1                                         -0.3471036
    ## 2                                          4.1995802
    ## 3                                         -0.1834513
    ## 
    ## Clustering vector:
    ##           BP GUAYAQUIL            BP PACIFICO           BP PICHINCHA 
    ##                      3                      3                      3 
    ##          BP PRODUBANCO              BP AUSTRO         BP BOLIVARIANO 
    ##                      3                      3                      3 
    ##            BP CITIBANK   BP GENERAL RUMIÑAHUI       BP INTERNACIONAL 
    ##                      3                      3                      3 
    ##                BP LOJA             BP MACHALA           BP SOLIDARIO 
    ##                      3                      3                      3 
    ##           BP PROCREDIT            BP AMAZONAS BP COMERCIAL DE MANABI 
    ##                      3                      3                      3 
    ##             BP LITORAL        BP COOPNACIONAL             BP CAPITAL 
    ##                      3                      3                      1 
    ##               BP FINCA             BP DELBANK         BP D-MIRO S.A. 
    ##                      3                      3                      3 
    ##     BP BANCODESARROLLO  BP VISIONFUND ECUADOR 
    ##                      3                      2 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1]  0.0000  0.0000 41.4896
    ##  (between_SS / total_SS =  62.3 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
#evaluando el cluster
silueta<-silhouette(cluster_nojerarquico2$cluster,
                    dist(data,method = "euclidean"))
fviz_silhouette(silueta) # no tienen que ser negativo, las barras sean cercanas a 1
```

    ##   cluster size ave.sil.width
    ## 1       1    1          0.00
    ## 2       2    1          0.00
    ## 3       3   21          0.63

![](PROYECTO_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
fviz_cluster(cluster_nojerarquico2,data=data,ellipse.type = "convex") + 
  theme_classic()
```

![](PROYECTO_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

``` r
#promedios de cada cluster
cluster_nojerarquico2$centers
```

    ##   ACTIVOS PRODUCTIVOS / TOTAL ACTIVOS MOROSIDAD DE LA CARTERA TOTAL
    ## 1                        -1.812706737                     3.5968638
    ## 2                         1.788055120                    -0.3408909
    ## 3                         0.001173887                    -0.1550463
    ##   GASTOS DE OPERACION  / MARGEN FINANCIERO
    ## 1                                4.1344555
    ## 2                               -0.4479673
    ## 3                               -0.1755471
    ##   RESULTADOS DEL EJERCICIO / ACTIVO PROMEDIO
    ## 1                                -2.82258654
    ## 2                                 2.01635092
    ## 3                                 0.03839217
    ##   FONDOS DISPONIBLES / TOTAL DEPOSITOS A CORTO PLAZO
    ## 1                                         -0.3471036
    ## 2                                          4.1995802
    ## 3                                         -0.1834513

``` r
#numero de iteraciones
cluster_nojerarquico2$iter
```

    ## [1] 2

``` r
datos_k<-data %>% mutate(grp=cluster_nojerarquico2$cluster)
medoides<-pam(data,3) 
medoides
```

    ## Medoids:
    ##                       ID ACTIVOS PRODUCTIVOS / TOTAL ACTIVOS
    ## BP PRODUBANCO          4                          -0.1576594
    ## BP CAPITAL            18                          -1.8127067
    ## BP VISIONFUND ECUADOR 23                           1.7880551
    ##                       MOROSIDAD DE LA CARTERA TOTAL
    ## BP PRODUBANCO                            -0.6243137
    ## BP CAPITAL                                3.5968638
    ## BP VISIONFUND ECUADOR                    -0.3408909
    ##                       GASTOS DE OPERACION  / MARGEN FINANCIERO
    ## BP PRODUBANCO                                       -0.2953297
    ## BP CAPITAL                                           4.1344555
    ## BP VISIONFUND ECUADOR                               -0.4479673
    ##                       RESULTADOS DEL EJERCICIO / ACTIVO PROMEDIO
    ## BP PRODUBANCO                                         0.05888989
    ## BP CAPITAL                                           -2.82258654
    ## BP VISIONFUND ECUADOR                                 2.01635092
    ##                       FONDOS DISPONIBLES / TOTAL DEPOSITOS A CORTO PLAZO
    ## BP PRODUBANCO                                                 -0.4915569
    ## BP CAPITAL                                                    -0.3471036
    ## BP VISIONFUND ECUADOR                                          4.1995802
    ## Clustering vector:
    ##           BP GUAYAQUIL            BP PACIFICO           BP PICHINCHA 
    ##                      1                      1                      1 
    ##          BP PRODUBANCO              BP AUSTRO         BP BOLIVARIANO 
    ##                      1                      1                      1 
    ##            BP CITIBANK   BP GENERAL RUMIÑAHUI       BP INTERNACIONAL 
    ##                      1                      1                      1 
    ##                BP LOJA             BP MACHALA           BP SOLIDARIO 
    ##                      1                      1                      1 
    ##           BP PROCREDIT            BP AMAZONAS BP COMERCIAL DE MANABI 
    ##                      1                      1                      1 
    ##             BP LITORAL        BP COOPNACIONAL             BP CAPITAL 
    ##                      1                      1                      2 
    ##               BP FINCA             BP DELBANK         BP D-MIRO S.A. 
    ##                      1                      1                      1 
    ##     BP BANCODESARROLLO  BP VISIONFUND ECUADOR 
    ##                      1                      3 
    ## Objective function:
    ##    build     swap 
    ## 1.231795 1.231795 
    ## 
    ## Available components:
    ##  [1] "medoids"    "id.med"     "clustering" "objective"  "isolation" 
    ##  [6] "clusinfo"   "silinfo"    "diss"       "call"       "data"

``` r
gm<-fviz_cluster(medoides,data=data)
gm
```

![](PROYECTO_files/figure-gfm/unnamed-chunk-3-5.png)<!-- -->

``` r
#METODO CLARA
modeloclara<-clara(data,k=3,sample=5)
modeloclara
```

    ## Call:     clara(x = data, k = 3, samples = 5) 
    ## Medoids:
    ##                       ACTIVOS PRODUCTIVOS / TOTAL ACTIVOS
    ## BP PRODUBANCO                                  -0.1576594
    ## BP CAPITAL                                     -1.8127067
    ## BP VISIONFUND ECUADOR                           1.7880551
    ##                       MOROSIDAD DE LA CARTERA TOTAL
    ## BP PRODUBANCO                            -0.6243137
    ## BP CAPITAL                                3.5968638
    ## BP VISIONFUND ECUADOR                    -0.3408909
    ##                       GASTOS DE OPERACION  / MARGEN FINANCIERO
    ## BP PRODUBANCO                                       -0.2953297
    ## BP CAPITAL                                           4.1344555
    ## BP VISIONFUND ECUADOR                               -0.4479673
    ##                       RESULTADOS DEL EJERCICIO / ACTIVO PROMEDIO
    ## BP PRODUBANCO                                         0.05888989
    ## BP CAPITAL                                           -2.82258654
    ## BP VISIONFUND ECUADOR                                 2.01635092
    ##                       FONDOS DISPONIBLES / TOTAL DEPOSITOS A CORTO PLAZO
    ## BP PRODUBANCO                                                 -0.4915569
    ## BP CAPITAL                                                    -0.3471036
    ## BP VISIONFUND ECUADOR                                          4.1995802
    ## Objective function:   1.231795
    ## Clustering vector:    Named int [1:23] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 ...
    ##  - attr(*, "names")= chr [1:23] "BP GUAYAQUIL" "BP PACIFICO" "BP PICHINCHA" "BP PRODUBANCO" "BP AUSTRO" "BP BOLIVARIANO" "BP CITIBANK" ...
    ## Cluster sizes:            21 1 1 
    ## Best sample:
    ##  [1] BP GUAYAQUIL           BP PACIFICO            BP PICHINCHA          
    ##  [4] BP PRODUBANCO          BP AUSTRO              BP BOLIVARIANO        
    ##  [7] BP CITIBANK            BP GENERAL RUMIÑAHUI   BP INTERNACIONAL      
    ## [10] BP LOJA                BP MACHALA             BP SOLIDARIO          
    ## [13] BP PROCREDIT           BP AMAZONAS            BP COMERCIAL DE MANABI
    ## [16] BP LITORAL             BP COOPNACIONAL        BP CAPITAL            
    ## [19] BP FINCA               BP DELBANK             BP D-MIRO S.A.        
    ## [22] BP BANCODESARROLLO     BP VISIONFUND ECUADOR 
    ## 
    ## Available components:
    ##  [1] "sample"     "medoids"    "i.med"      "clustering" "objective" 
    ##  [6] "clusinfo"   "diss"       "call"       "silinfo"    "data"

``` r
table(modeloclara$clustering)
```

    ## 
    ##  1  2  3 
    ## 21  1  1

``` r
gclara<-fviz_cluster(modeloclara,data=data)
#grupo<-grid.arrange(gm,gc,gclara,ncol=3)
centroide<-kmeans(data,2)
gc<-fviz_cluster(centroide,data=data)
grupo<-grid.arrange(gm,gc,gclara,ncol=3) 
```

![](PROYECTO_files/figure-gfm/unnamed-chunk-3-6.png)<!-- -->
