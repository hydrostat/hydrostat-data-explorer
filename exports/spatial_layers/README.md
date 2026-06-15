# Runtime spatial layer

Required file:

```text
shiny_spatial_layers.rds
```

This serialized object contains the simplified spatial context layers used by the application. Source geometries and intermediate GeoJSON/GPKG products are rebuilt by the pipeline and are not required in the deployment bundle.
