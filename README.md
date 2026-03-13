 
> # -----------------------------------------
> # ANALISIS DE ENVIOS - GLOBAL LOGISTICS ISC
> # -----------------------------------------
> 
> # Librerias
> library(tidyverse)
── Attaching core tidyverse packages ───────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.2.0     ✔ readr     2.2.0
✔ forcats   1.0.1     ✔ stringr   1.6.0
✔ ggplot2   4.0.2     ✔ tibble    3.3.1
✔ lubridate 1.9.5     ✔ tidyr     1.3.2
✔ purrr     1.2.1     
── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package to force all conflicts to become errors
> 
> # -----------------------------------------
> # GENERACION DEL DATASET (600 REGISTROS)
> # -----------------------------------------
> 
> set.seed(123)
> 
> n <- 600
> 
> datos <- tibble(
+     envio_id = 1:n,
+     tipo_transporte = sample(c("Terrestre","Aereo"), n, replace = TRUE),
+     distancia_km = round(rnorm(n, mean = 800, sd = 300)),
+     clima = sample(c("Soleado","Lluvia","Tormenta"), n, replace = TRUE),
+     costo_envio = round(runif(n, 200, 1200),2)
+ )
> 
> # generar tiempo de entrega dependiendo del transporte
> datos <- datos %>%
+     mutate(
+         tiempo_entrega_hrs = ifelse(
+             tipo_transporte == "Terrestre",
+             distancia_km/60 + rnorm(n,5,2),
+             distancia_km/500 + rnorm(n,2,1)
+         )
+     )
> 
> # introducir valores NA en distancia
> indices_na <- sample(1:n, 40)
> datos$distancia_km[indices_na] <- NA
> 
> # visualizar primeros registros
> head(datos)
# A tibble: 6 × 6
  envio_id tipo_transporte distancia_km clima    costo_envio tiempo_entrega_hrs
     <int> <chr>                  <dbl> <chr>          <dbl>              <dbl>
1        1 Terrestre                585 Lluvia          827.              14.1 
2        2 Terrestre                574 Lluvia          304.              18.5 
3        3 Terrestre                518 Soleado         392.               9.14
4        4 Aereo                    484 Lluvia         1042.               3.67
5        5 Terrestre                669 Soleado         679.              15.6 
6        6 Aereo                    899 Tormenta        977.               3.33
> 
> 
> # -----------------------------------------
> # FASE 1: CALIDAD DE DATOS
> # -----------------------------------------
> 
> # 1. Limpieza - imputar NA con mediana
> mediana_distancia <- median(datos$distancia_km, na.rm = TRUE)
> 
> datos <- datos %>%
+     mutate(
+         distancia_km = ifelse(is.na(distancia_km),
+                               mediana_distancia,
+                               distancia_km)
+     )
> 
> # 2. Estadisticos descriptivos
> media_tiempo <- mean(datos$tiempo_entrega_hrs)
> desv_tiempo <- sd(datos$tiempo_entrega_hrs)
> 
> media_tiempo
[1] 11.10805
> desv_tiempo
[1] 8.395943
> 
> 
> # -----------------------------------------
> # FASE 2: ANALISIS DE RELACIONES
> # -----------------------------------------
> 
> # Correlacion de Pearson
> correlacion <- cor(datos$distancia_km,
+                    datos$tiempo_entrega_hrs,
+                    method = "pearson")
> 
> correlacion
[1] 0.3330663
> 
> 
> # -----------------------------------------
> # FASE 3: COMPARACION DE MODALIDADES
> # -----------------------------------------
> 
> # 1. Agrupacion por tipo de transporte
> resumen_transporte <- datos %>%
+     group_by(tipo_transporte) %>%
+     summarise(
+         promedio_tiempo = mean(tiempo_entrega_hrs),
+         costo_promedio = mean(costo_envio)
+     )
> 
> resumen_transporte
# A tibble: 2 × 3
  tipo_transporte promedio_tiempo costo_promedio
  <chr>                     <dbl>          <dbl>
1 Aereo                      3.54           712.
2 Terrestre                 18.5            693.
> 
> 
> # 2. Prueba T (t-test)
> t_test <- t.test(
+     tiempo_entrega_hrs ~ tipo_transporte,
+     data = datos
+ )
> 
> t_test

	Welch Two Sample t-test

data:  tiempo_entrega_hrs by tipo_transporte
t = -48.377, df = 330.88, p-value < 2.2e-16
alternative hypothesis: true difference in means between group Aereo and group Terrestre is not equal to 0
95 percent confidence interval:
 -15.54479 -14.32999
sample estimates:
    mean in group Aereo mean in group Terrestre 
               3.539771               18.477163 

> 
> 
> # -----------------------------------------
> # FASE 4: VISUALIZACION
> # -----------------------------------------
> 
> # 1. Grafico de dispersion
> 
> ggplot(datos,
+        aes(x = distancia_km,
+            y = tiempo_entrega_hrs,
+            color = tipo_transporte)) +
+     geom_point(alpha = 0.7) +
+     labs(
+         title = "Relación entre Distancia y Tiempo de Entrega",
+         x = "Distancia (km)",
+         y = "Tiempo de Entrega (hrs)"
+     ) +
+     theme_minimal()
> 
> 
> 
> # 2. Boxplot de tiempos por transporte
> 
> ggplot(datos,
+        aes(x = tipo_transporte,
+            y = tiempo_entrega_hrs,
+            fill = tipo_transporte)) +
+     geom_boxplot() +
+     labs(
+         title = "Comparación de Tiempos de Entrega por Modalidad",
+         x = "Tipo de Transporte",
+         y = "Tiempo de Entrega (hrs)"
+     ) +
+     theme_minimal()




