# Propuesta de Optimización del Paquete `reddPrec`

Tras una revisión detallada del código fuente del paquete `reddPrec`, se han identificado áreas críticas de mejora centradas principalmente en la gestión de procesos paralelos y la eficiencia computacional. A continuación se detalla la propuesta de optimización.

## 1. Gestión de Recursos en Procesos Paralelos (Crítico)

**Problema Identificado:**
Actualmente, cinco funciones principales (`qcPrec`, `hmg_Ts`, `gridPcp`, `gapFilling`, `eqc_Ts`) inicializan un backend paralelo utilizando `registerDoParallel(cores=ncpu)`. Sin embargo, **ninguna de estas funciones cierra explícitamente el clúster** al finalizar su ejecución.

Esto provoca que, tras cada ejecución, los procesos "worker" (Rscript.exe en Windows) permanezcan activos en memoria (zombies), consumiendo recursos del sistema hasta que se cierra la sesión de R principal o se mata el proceso manualmente. En entornos de producción o ejecuciones repetidas, esto puede colapsar la memoria del sistema.

**Archivos Afectados:**
*   `R/qcPrec.R`
*   `R/hmg_Ts.R`
*   `R/gridPcp.R`
*   `R/gapFilling.R`
*   `R/eqc_Ts.R`

**Solución Propuesta:**
Reemplazar la llamada implícita `registerDoParallel(cores=ncpu)` por una gestión explícita del clúster asegurando su cierre mediante `on.exit()`. Esto garantiza que el clúster se cierre incluso si la función falla o se detiene por un error.

**Implementación Técnica:**
Cambiar:
```r
registerDoParallel(cores=ncpu)
```
Por:
```r
if (ncpu > 1) {
  cl <- parallel::makeCluster(ncpu)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)
} else {
  registerDoParallel(cores=1)
}
```
*Nota: Se añade una comprobación para `ncpu > 1` para evitar la sobrecarga de crear un clúster si solo se solicita un núcleo.*

## 2. Optimización de Arquitectura en `gridPcp` (Rendimiento)

**Problema Identificado:**
La función `gridPcp` presenta un cuello de botella arquitectónico cuando procesa múltiples días (`length(dates) > 1`).
*   **Flujo Actual:** Bucle `for` secuencial sobre los días -> llamada a `predday` -> Bucle `foreach` paralelo sobre los píxeles de la rejilla.
*   **Ineficiencia:** Se levanta y gestiona la paralelización miles de veces (una vez por píxel/bloque) para cada día. Además, R tiene que serializar y enviar datos a los workers repetidamente.

**Solución Propuesta:**
Invertir la jerarquía de paralelización. Es computacionalmente más eficiente paralelizar el bucle más externo (los días) y mantener el cálculo interno (píxeles) secuencial dentro de cada worker.

**Cambios Sugeridos:**
1.  **Modificar `predday`:** Añadir un argumento `parallel = TRUE/FALSE`. Si es `FALSE`, usar `%do%` (secuencial) en lugar de `%dopar%`.
2.  **Refactorizar `gridPcp`:**
    *   Si hay múltiples fechas: Registrar el clúster y usar `foreach` para iterar sobre `dates`. Dentro del loop, llamar a `predday(..., parallel = FALSE)`.
    *   Si hay una sola fecha: Mantener el comportamiento actual (`predday(..., parallel = TRUE)`).

Esto reducirá drásticamente el "overhead" de comunicación entre procesos.

## 3. Otras Mejoras Menores

*   **Transposiciones Redundantes:** En `qcPrec.R`, la matriz `a` se transpone (`t(a)`) repetidamente dentro del bucle `while`. Si es posible, se debería trabajar con la orientación natural de los datos para evitar estas operaciones costosas en matrices grandes, o transponer una sola vez antes y después del bucle.
*   **Verificación de Tipos:** Asegurar que `ncpu` sea siempre un entero positivo.

## Plan de Acción

1.  **Fase 1 (Inmediata):** Aplicar la corrección de `stopCluster` en los 5 archivos afectados. Esto soluciona el requisito de "cerrar una vez ejecutadas".
2.  **Fase 2 (Opcional):** Refactorizar `gridPcp` y `predday` para implementar la paralelización óptima.

Estoy listo para aplicar la **Fase 1** inmediatamente si lo autoriza, ya que cumple directamente con su instrucción de "deben de cerrar una vez ejecutadas".
