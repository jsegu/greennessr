
greennessr
==========

Paquete de R enmarcado dentro del proyecto MEDEA 3, concretamente creado para el objetivo de estimar la exposición a espacios verdes dentro de núcleos urbanos.

Los espacios verdes urbanos son beneficiosos para la salud de los ciudadanos que conviven en ciudades. Los arboles ayudan a reducir la contaminación y establecen una barrera verde, que por ejemplo, ayuda a la regulación de las temperaturas o para bajar los niveles de contaminación acústica producida por los automóviles. Además diversos estudios apuntan que el hecho de tener un parque o un equipamiento deportivo cercano al domicilio incentiva el deporte y la movilidad de las personas.

Mediante las funciones de esta librería, se han generado distintas metodologías para la extracción de la variable de exposición a espacios verdes urbanos. Así el usuario tiene la posibilidad de comparar las diversas aproximaciones para el cálculo de dicha variable con un mismo paquete de funciones.

Las funciones de *area\_verde* y *acceso\_verde* se enmarcan en el cálculo de dicha superficie utilizando los datos vectoriales de la base de datos [urban atlas](https://land.copernicus.eu/local/urban-atlas), este es un mapa de cubiertas y usos del suelo de las principales ciudades europeas creado por el proyecto Copernicus. La función *calcular\_ndvi*, se basa en el índice de vegetación normalizado (NDVI) calculado mediante imágenes del satélite landsat.

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
