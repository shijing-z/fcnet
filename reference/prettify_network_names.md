# Prettify network names for better readability

Prettify network names for better readability

## Usage

``` r
prettify_network_names(network_names)
```

## Arguments

- network_names:

  Character vector of lowercase network names

## Value

Character vector with prettified names

## Details

Applies formatting rules:

- 17-network subnetworks: defaultA, contB, etc. (uppercase A/B/C)

- Visual networks: visCent, visPeri (camelCase)

- Temporal parietal: tempPar (camelCase)

- 7-network: unchanged (vis, default, cont, etc.) Note: Attention
  networks are already normalized to salventattn/dorsattn
