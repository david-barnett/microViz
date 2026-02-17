# Constructor for psExtraInfo list objects (internal use)

Constructor for psExtraInfo list objects (internal use)

## Usage

``` r
new_psExtraInfo(
  tax_agg = character(),
  tax_trans = character(),
  tax_scale = character(),
  dist_method = character(),
  ord_info = new_psExtraOrdInfo()
)
```

## Arguments

- tax_agg:

  character string naming rank at which psExtra was aggregated

- tax_trans:

  character vector naming transformation(s) applied to taxa in psExtra

- tax_scale:

  character vector

- dist_method:

  character vector

- ord_info:

  psExtraOrdInfo list

## Value

psExtraInfo S3 class list
