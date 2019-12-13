
greennessr
==========

Paquete de R enmarcado dentro del proyecto MEDEA 3, concretamente en el objetivo de estimar la exposición a espacios verdes dentro de núcleos urbanos.

Mediante las funciones de está librería, se han generado distintas metodologías para la extracción de la variable de exposición a espacios verdes urbanos. Así el usuario tiene la posibilidad de comparar las diversas aproximaciones para el cálculo de dicha variable con un mismo paquete de funciones.

(falta extender un poco más la explicación)

Instalación
-----------

El paquete greennessr se puede instalar desde GitHub ejecutando el siguiente comando:

``` r
if (!"devtools" %in% installed.packages())
  install.packages()
devtools::install_github("jsegu/greennessr", build_opts = c("--no-resave-data", "--no-manual"))
```

**IMPORTANTE**: si trabajas desde un ordenador conectado a la red a través de un *proxy*, es muy importante que te asegures de que tu conexión esté bien configurada. Para ello debes ejecutar esta serie de comandos (sustituyendo el texto por los valores apropiados de tu centro: pregunta al servicio de informática):

``` r
if (!"httr" %in% installed.packages())
  install.packages("httr")
httr::set_config(
  httr::use_proxy(
    url      = "xxx.xxx.xxx.xx",
    port     = 0000,
    username = "usuario",
    password = "clave"
  )
)
```

Dudas y consultas
-----------------

Para cualquier duda, consulta o aportación lo puede realizar a través de los propios canales de GitHub: Issue y Pull Request. O bien contactando directamente con Jordi Segú Tell a través de esta dirección de correo electrónico <jordi.segu@isciii.es>.
