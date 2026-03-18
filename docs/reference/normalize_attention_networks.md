# Normalize variations in attention network names

Normalize variations in attention network names

## Usage

``` r
normalize_attention_networks(network_names)
```

## Arguments

- network_names:

  Character vector of lowercase network names

## Value

Character vector with standardized attention network names

## Details

Handles variations across different Schaefer atlas versions and
preprocessing pipelines:

- Ventral attention: ventatt, ventattn, salventatt, salvatt, salvattn,
  ventralatt, ventralattn, salience, van – all mapped to salventattn

- Dorsal attention: dorsatt, dorsalatt, dorsalattn, dan – all mapped to
  dorsattn
