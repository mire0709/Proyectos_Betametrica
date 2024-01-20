Introduccion a la Estadística
================
Mireya Rios
2024-01-20

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(openxlsx)
library(lubridate)
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

\#Teoría y principales definiciones

``` r
data<-read.csv("C:\\Users\\USUARIO\\Desktop\\R CURSO\\MATERIAL\\BASES_DATOS_BASES_ZS2ZD2\\Iowa_Liquor_Sales.csv",stringsAsFactors=F,
header=T)

datos <- data %>% mutate(
  Sale..Dollars.=(as.numeric(substr(data$Sale..Dollars.,2,15))),
  City=toupper(City),
  Date=as.Date(Date,format="%m/%d/%Y"),
  anio=lubridate::year(Date)) %>% 
      rename(ventas=Sale..Dollars.,
             ciudad=City,
             categoria=Category.Name,
             nombre_tienda=Store.Name)
```

\#PRUEBAS

``` r
datos %>%  group_by(ciudad) %>% 
  summarise(suma=sum(ventas))
```

    ## # A tibble: 3 × 2
    ##   ciudad           suma
    ##   <chr>           <dbl>
    ## 1 CEDAR RAPIDS 2378280.
    ## 2 DAVENPORT    1884349.
    ## 3 WATERLOO     1149095.

\#PROMEDIO DE VENTAS POR CIUDAD

``` r
datos %>%  group_by(ciudad) %>% 
  summarise(promedio_ventas=mean(ventas))
```

    ## # A tibble: 3 × 2
    ##   ciudad       promedio_ventas
    ##   <chr>                  <dbl>
    ## 1 CEDAR RAPIDS            85.4
    ## 2 DAVENPORT               98.2
    ## 3 WATERLOO               101.

\#PROMEDIO DE VENTAS POR SUCURSAL

``` r
datos %>%  group_by(nombre_tienda) %>% 
  summarise(promedio_ventas=mean(ventas)) %>% 
  arrange(-promedio_ventas)
```

    ## # A tibble: 188 × 2
    ##    nombre_tienda                      promedio_ventas
    ##    <chr>                                        <dbl>
    ##  1 Sam's Club 6514 / Waterloo                    516.
    ##  2 Sam's Club 8238 / Davenport                   486.
    ##  3 Sam's Club 8162 / Cedar Rapids                468.
    ##  4 Artisan Grain Distillery                      419.
    ##  5 Hilltop Grocery                               302.
    ##  6 Sam's Food                                    209.
    ##  7 Sycamore Convenience                          187.
    ##  8 Fareway Stores #151 / Cedar Rapids            186.
    ##  9 EZ Stop / Davenport                           178.
    ## 10 Logan Convenience Store                       175.
    ## # ℹ 178 more rows

\#EXPANDIENDO RESULTADOS

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats 1.0.0     ✔ stringr 1.5.1
    ## ✔ ggplot2 3.4.4     ✔ tibble  3.2.1
    ## ✔ purrr   1.0.2     ✔ tidyr   1.3.0
    ## ✔ readr   2.1.5     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
datos %>%  group_by(ciudad,anio) %>% 
  summarise_at(vars(ventas),
               list(maximo=~max(.,na.rm = T),
                    minimo=~min(.,na.rm = T),
                    media=~mean(.,na.rm = T)))
```

    ## # A tibble: 9 × 5
    ## # Groups:   ciudad [3]
    ##   ciudad        anio maximo minimo media
    ##   <chr>        <dbl>  <dbl>  <dbl> <dbl>
    ## 1 CEDAR RAPIDS  2015  8971.   3.36  90.5
    ## 2 CEDAR RAPIDS  2016  8250    0     80.7
    ## 3 CEDAR RAPIDS  2017   297    6.75  67.8
    ## 4 DAVENPORT     2015  7555.   5.18 104. 
    ## 5 DAVENPORT     2016  5882.   3.36  91.8
    ## 6 DAVENPORT     2017   306    6.75  81.5
    ## 7 WATERLOO      2015  7555.   3.36 104. 
    ## 8 WATERLOO      2016  5666.   0     99.7
    ## 9 WATERLOO      2017   306    6.75  73.3

\#PIVOTEANDO LA TABLA

``` r
library(tidyverse)
datos %>%  group_by(ciudad,anio) %>% 
  summarise_at(vars(ventas),
               list(maximo=~max(.,na.rm = T),
                    minimo=~min(.,na.rm = T),
                    media=~mean(.,na.rm = T))) %>% 
  select(ciudad,media,anio) %>% 
  pivot_wider(names_from = ciudad,
              values_from = media,
              values_fill = 0)
```

    ## # A tibble: 3 × 4
    ##    anio `CEDAR RAPIDS` DAVENPORT WATERLOO
    ##   <dbl>          <dbl>     <dbl>    <dbl>
    ## 1  2015           90.5     104.     104. 
    ## 2  2016           80.7      91.8     99.7
    ## 3  2017           67.8      81.5     73.3

\#PIVOTEANDO Y AGREGANDO VALORES

``` r
datos %>%  group_by(ciudad,anio) %>% 
  summarise_at(vars(ventas),
               list(maximo=~max(.,na.rm = T),
                    minimo=~min(.,na.rm = T),
                    media=~mean(.,na.rm = T))) %>% 
  select(ciudad,media,anio,maximo) %>% 
  pivot_wider(names_from = ciudad,
              values_from = c(media,maximo),names_prefix = "empresas_",
              values_fill = 0)
```

    ## # A tibble: 3 × 7
    ##    anio media_empresas_CEDAR RAP…¹ media_empresas_DAVEN…² media_empresas_WATER…³
    ##   <dbl>                      <dbl>                  <dbl>                  <dbl>
    ## 1  2015                       90.5                  104.                   104. 
    ## 2  2016                       80.7                   91.8                   99.7
    ## 3  2017                       67.8                   81.5                   73.3
    ## # ℹ abbreviated names: ¹​`media_empresas_CEDAR RAPIDS`,
    ## #   ²​media_empresas_DAVENPORT, ³​media_empresas_WATERLOO
    ## # ℹ 3 more variables: `maximo_empresas_CEDAR RAPIDS` <dbl>,
    ## #   maximo_empresas_DAVENPORT <dbl>, maximo_empresas_WATERLOO <dbl>

\#MEDIA ACOTADA

``` r
datos %>%  group_by(ciudad,anio) %>% 
  summarise_at(vars(ventas),
               list(maximo=~max(.,na.rm = T),
                    minimo=~min(.,na.rm = T),
                    media=~mean(.,na.rm = T,trim = 0.99))) %>% #el trim me acota al 0.90
  select(ciudad,media,anio,maximo) %>% 
  pivot_wider(names_from = ciudad,
              values_from = c(media,maximo),names_prefix = "empresas_",
              values_fill = 0)
```

    ## # A tibble: 3 × 7
    ##    anio media_empresas_CEDAR RAP…¹ media_empresas_DAVEN…² media_empresas_WATER…³
    ##   <dbl>                      <dbl>                  <dbl>                  <dbl>
    ## 1  2015                       44.0                   55.6                   51  
    ## 2  2016                       37.1                   45                     40.5
    ## 3  2017                       15                     24.8                   18.8
    ## # ℹ abbreviated names: ¹​`media_empresas_CEDAR RAPIDS`,
    ## #   ²​media_empresas_DAVENPORT, ³​media_empresas_WATERLOO
    ## # ℹ 3 more variables: `maximo_empresas_CEDAR RAPIDS` <dbl>,
    ## #   maximo_empresas_DAVENPORT <dbl>, maximo_empresas_WATERLOO <dbl>

\#MEDIDAS DE DISPERSION

``` r
#QUANTILES
#QUINTILES

quantile(datos$ventas,probs = seq(0,1,0.2)) #corresponede a 1/5 quintiles
```

    ##      0%     20%     40%     60%     80%    100% 
    ##    0.00   14.99   31.05   80.64  135.00 8970.66

``` r
##cuartiles 
quantile(datos$ventas,probs = seq(0,1,0.25)) #1/4
```

    ##      0%     25%     50%     75%    100% 
    ##    0.00   20.16   44.04  126.00 8970.66

``` r
##deciles

quantile(datos$ventas,probs = seq(0,1,0.10)) #1/10
```

    ##      0%     10%     20%     30%     40%     50%     60%     70%     80%     90% 
    ##    0.00   10.50   14.99   23.98   31.05   44.04   80.64  117.00  135.00  179.88 
    ##    100% 
    ## 8970.66

\#USANDO DPLYR

``` r
datos %>%  group_by(ciudad,anio) %>% 
  summarise_at(vars(ventas),
               list(maximo=~max(.,na.rm = T),
                    minimo=~min(.,na.rm = T),
                    media=~mean(.,na.rm = T,trim = 0.20),
                    p_25=~quantile(.,probs = 0.25,na.rm = T),
                    mediana=~quantile(.,probs = 0.50,na.rm = T))) %>% 
  select(ciudad,media,anio,maximo,p_25,mediana) %>% 
  pivot_wider(names_from = ciudad,
              id_cols = anio,
              values_from = c(media,maximo,p_25,mediana),names_prefix = "empresas_",
              values_fill = 0)
```

    ## # A tibble: 3 × 13
    ##    anio media_empresas_CEDAR RAP…¹ media_empresas_DAVEN…² media_empresas_WATER…³
    ##   <dbl>                      <dbl>                  <dbl>                  <dbl>
    ## 1  2015                       59.0                   68.5                   68.5
    ## 2  2016                       53.3                   64.0                   63.3
    ## 3  2017                       44.8                   62.7                   53.6
    ## # ℹ abbreviated names: ¹​`media_empresas_CEDAR RAPIDS`,
    ## #   ²​media_empresas_DAVENPORT, ³​media_empresas_WATERLOO
    ## # ℹ 9 more variables: `maximo_empresas_CEDAR RAPIDS` <dbl>,
    ## #   maximo_empresas_DAVENPORT <dbl>, maximo_empresas_WATERLOO <dbl>,
    ## #   `p_25_empresas_CEDAR RAPIDS` <dbl>, p_25_empresas_DAVENPORT <dbl>,
    ## #   p_25_empresas_WATERLOO <dbl>, `mediana_empresas_CEDAR RAPIDS` <dbl>,
    ## #   mediana_empresas_DAVENPORT <dbl>, mediana_empresas_WATERLOO <dbl>

\#AGREGANDO INFORMACION

``` r
datos %>%  select(ciudad,ventas) %>%
  group_by(ciudad) %>% 
  mutate(n_tile=ntile(ventas,5)) %>% 
  group_by(ciudad,n_tile) %>% 
  summarise(sum_ventas=sum(ventas)) %>% 
 pivot_wider(names_from = ciudad,
              values_from = c(sum_ventas),names_prefix = "empresas_",
              values_fill = 0)
```

    ## `summarise()` has grouped output by 'ciudad'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 4
    ##   n_tile `empresas_CEDAR RAPIDS` empresas_DAVENPORT empresas_WATERLOO
    ##    <int>                   <dbl>              <dbl>             <dbl>
    ## 1      1                  56855.             41192.            26412.
    ## 2      2                 124557.             93252.            56595.
    ## 3      3                 234912.            215943.           119383.
    ## 4      4                 580392.            456691.           272335.
    ## 5      5                1381565            1077271.           674369.

\#TAREA

``` r
##TOP 5 DE TIENDAS (PROMEDIO DE VENTAS), PARA EL AÑO 2016, PARA LA CIUDAD CEDAR RAPIDS
datos %>% filter(anio==2016,ciudad=="CEDAR RAPIDS") %>% 
  group_by(nombre_tienda) %>% 
  summarise(Ventas_Promedio=mean(ventas))%>% 
  arrange(desc(Ventas_Promedio)) %>% 
  head(5)
```

    ## # A tibble: 5 × 2
    ##   nombre_tienda                      Ventas_Promedio
    ##   <chr>                                        <dbl>
    ## 1 Sam's Club 8162 / Cedar Rapids                354.
    ## 2 Fareway Stores #151 / Cedar Rapids            338.
    ## 3 Benz Distributing                             171.
    ## 4 Leo1  /  Cedar Rapids                         163.
    ## 5 Target Store T-1771 / Cedar Rapids            162.

``` r
##¿Cuáles fueron los 5 últimos vendedores (promedio de ventas, para el 2016, para DAVENPORT)?
datos %>% filter(anio==2016,ciudad=="DAVENPORT") %>% 
  group_by(Vendor.Name) %>% 
  summarise(Ventas_Promedio=mean(ventas))%>% 
  arrange(Ventas_Promedio) %>% 
  head(5)
```

    ## # A tibble: 5 × 2
    ##   Vendor.Name                   Ventas_Promedio
    ##   <chr>                                   <dbl>
    ## 1 Luxco-St Louis                           36.5
    ## 2 A HARDY USA LTD                          37.0
    ## 3 Rumcoqui and Co                          38.3
    ## 4 Prestige Wine & Spirits Group            42.1
    ## 5 Dehner Distillery                        42.7

``` r
##¿Cuál es el top 5 de productos más vendidos, para el 2016 y 2017, por ciudad? 
datos %>% filter(anio==2016 | anio==2017) %>% 
  group_by(ciudad,anio,Item.Description) %>% 
  summarise(Total_Productos=sum(Bottles.Sold))%>% 
  arrange(desc(Total_Productos)) %>% 
  group_by(ciudad,anio) %>% 
  slice_head(n=5)
```

    ## `summarise()` has grouped output by 'ciudad', 'anio'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 30 × 4
    ## # Groups:   ciudad, anio [6]
    ##    ciudad        anio Item.Description              Total_Productos
    ##    <chr>        <dbl> <chr>                                   <int>
    ##  1 CEDAR RAPIDS  2016 Malibu Coconut Rum                       8114
    ##  2 CEDAR RAPIDS  2016 Uv Blue (raspberry) Vodka                6178
    ##  3 CEDAR RAPIDS  2016 New Amsterdam Peach                      4677
    ##  4 CEDAR RAPIDS  2016 Bailey's Original Irish Cream            3471
    ##  5 CEDAR RAPIDS  2016 Rumchata                                 3162
    ##  6 CEDAR RAPIDS  2017 Malibu Coconut Rum                        249
    ##  7 CEDAR RAPIDS  2017 Bailey's Original Irish Cream             170
    ##  8 CEDAR RAPIDS  2017 Rumchata                                  147
    ##  9 CEDAR RAPIDS  2017 Bacardi Limon                             116
    ## 10 CEDAR RAPIDS  2017 Bailey's Salted Caramel                    82
    ## # ℹ 20 more rows
