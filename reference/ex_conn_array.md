# Example Connectivity Array

A small 3D connectivity array (30 ROIs × 30 ROIs × 10 subjects) with
simulated correlation values for demonstration purposes.

## Usage

``` r
ex_conn_array
```

## Format

A 3D array with dimensions (30, 30, 10):

- Dimension 1:

  ROI (rows)

- Dimension 2:

  ROI (columns)

- Dimension 3:

  Subject

Dimnames include ROI names (from ex_roi_names) and subject IDs.

## Examples

``` r
dim(ex_conn_array)
#> [1] 30 30 10
dimnames(ex_conn_array)[[1]]  # ROI names
#>  [1] "Schaefer100.l_vis_1"                   
#>  [2] "Schaefer100.l_vis_2"                   
#>  [3] "Schaefer100.l_vis_3"                   
#>  [4] "Schaefer100.r_vis_1"                   
#>  [5] "Schaefer100.r_vis_2"                   
#>  [6] "Schaefer100.l_sommot_1"                
#>  [7] "Schaefer100.l_sommot_2"                
#>  [8] "Schaefer100.r_sommot_1"                
#>  [9] "Schaefer100.r_sommot_2"                
#> [10] "Schaefer100.l_dorsattn_post_1"         
#> [11] "Schaefer100.l_dorsattn_post_2"         
#> [12] "Schaefer100.r_dorsattn_post_1"         
#> [13] "Schaefer100.r_dorsattn_post_2"         
#> [14] "Schaefer100.l_salventattn_paroper_1"   
#> [15] "Schaefer100.l_salventattn_froperins_1" 
#> [16] "Schaefer100.r_salventattn_tempoccpar_1"
#> [17] "Schaefer100.r_salventattn_froperins_1" 
#> [18] "Schaefer100.l_limbic_ofc_1"            
#> [19] "Schaefer100.l_limbic_temppole_1"       
#> [20] "Schaefer100.r_limbic_ofc_1"            
#> [21] "Schaefer100.l_cont_par_1"              
#> [22] "Schaefer100.l_cont_pfcl_1"             
#> [23] "Schaefer100.r_cont_par_1"              
#> [24] "Schaefer100.r_cont_pfcl_1"             
#> [25] "Schaefer100.l_default_temp_1"          
#> [26] "Schaefer100.l_default_par_1"           
#> [27] "Schaefer100.r_default_temp_1"          
#> [28] "Schaefer100.r_default_par_1"           
#> [29] "AHIP"                                  
#> [30] "PHIP"                                  

# Access connectivity for first subject
ex_conn_array[, , 1]
#>                                        Schaefer100.l_vis_1 Schaefer100.l_vis_2
#> Schaefer100.l_vis_1                           1.0000000000         0.029443010
#> Schaefer100.l_vis_2                           0.0294430098         1.000000000
#> Schaefer100.l_vis_3                           0.2907521695         0.058920331
#> Schaefer100.r_vis_1                           0.1596018371         0.213979567
#> Schaefer100.r_vis_2                           0.0370401498        -0.018884030
#> Schaefer100.l_sommot_1                        0.3754205752         0.218652374
#> Schaefer100.l_sommot_2                       -0.0903614892         0.272565424
#> Schaefer100.r_sommot_1                       -0.1718723997         0.027266358
#> Schaefer100.r_sommot_2                       -0.2213212234        -0.121224207
#> Schaefer100.l_dorsattn_post_1                 0.0599026551         0.087308545
#> Schaefer100.l_dorsattn_post_2                 0.0763259415        -0.217109392
#> Schaefer100.r_dorsattn_post_1                 0.2066058696        -0.209452697
#> Schaefer100.r_dorsattn_post_2                 0.0293208290        -0.092130460
#> Schaefer100.l_salventattn_paroper_1          -0.0804544373         0.001696445
#> Schaefer100.l_salventattn_froperins_1         0.1835492663         0.224157963
#> Schaefer100.r_salventattn_tempoccpar_1        0.4825973217        -0.011471960
#> Schaefer100.r_salventattn_froperins_1         0.0777451780        -0.013324076
#> Schaefer100.l_limbic_ofc_1                   -0.3095979636        -0.037575422
#> Schaefer100.l_limbic_temppole_1               0.0642161692         0.046689801
#> Schaefer100.r_limbic_ofc_1                    0.0732709116        -0.230975248
#> Schaefer100.l_cont_par_1                      0.0009282831         0.033895732
#> Schaefer100.l_cont_pfcl_1                    -0.1344717090        -0.282617761
#> Schaefer100.r_cont_par_1                     -0.0243534021         0.200646611
#> Schaefer100.r_cont_pfcl_1                    -0.1230488257         0.383368364
#> Schaefer100.l_default_temp_1                 -0.2500068843        -0.293111325
#> Schaefer100.l_default_par_1                  -0.0222394668         0.211014042
#> Schaefer100.r_default_temp_1                  0.2476781128        -0.096647601
#> Schaefer100.r_default_par_1                  -0.3531817027        -0.147100202
#> AHIP                                         -0.2846302581         0.421306986
#> PHIP                                          0.0171921422         0.053934721
#>                                        Schaefer100.l_vis_3 Schaefer100.r_vis_1
#> Schaefer100.l_vis_1                            0.290752170          0.15960184
#> Schaefer100.l_vis_2                            0.058920331          0.21397957
#> Schaefer100.l_vis_3                            1.000000000         -0.11697655
#> Schaefer100.r_vis_1                           -0.116976547          1.00000000
#> Schaefer100.r_vis_2                           -0.234352301          0.16568404
#> Schaefer100.l_sommot_1                         0.095359683         -0.24129543
#> Schaefer100.l_sommot_2                         0.014783909          0.19827302
#> Schaefer100.r_sommot_1                         0.192822016          0.15248202
#> Schaefer100.r_sommot_2                         0.362749221         -0.20595060
#> Schaefer100.l_dorsattn_post_1                  0.410159117         -0.36325429
#> Schaefer100.l_dorsattn_post_2                 -0.214435480         -0.26443798
#> Schaefer100.r_dorsattn_post_1                 -0.454615997          0.26641521
#> Schaefer100.r_dorsattn_post_2                  0.191925752          0.11669720
#> Schaefer100.l_salventattn_paroper_1            0.026257509         -0.17655303
#> Schaefer100.l_salventattn_froperins_1         -0.084253914          0.04809723
#> Schaefer100.r_salventattn_tempoccpar_1         0.219129048          0.10052260
#> Schaefer100.r_salventattn_froperins_1          0.156516253         -0.09953791
#> Schaefer100.l_limbic_ofc_1                    -0.050737882         -0.21935166
#> Schaefer100.l_limbic_temppole_1                0.132820615         -0.23663850
#> Schaefer100.r_limbic_ofc_1                    -0.138094661          0.18590984
#> Schaefer100.l_cont_par_1                      -0.004134923         -0.31371219
#> Schaefer100.l_cont_pfcl_1                      0.137780965          0.13772919
#> Schaefer100.r_cont_par_1                       0.239338198         -0.24694166
#> Schaefer100.r_cont_pfcl_1                      0.275396673         -0.12667878
#> Schaefer100.l_default_temp_1                   0.063201520         -0.15148550
#> Schaefer100.l_default_par_1                    0.126487908          0.07726670
#> Schaefer100.r_default_temp_1                   0.164929601         -0.16065239
#> Schaefer100.r_default_par_1                    0.053626244         -0.06516179
#> AHIP                                          -0.117648259         -0.11781912
#> PHIP                                           0.007388502         -0.01809185
#>                                        Schaefer100.r_vis_2
#> Schaefer100.l_vis_1                           0.0370401498
#> Schaefer100.l_vis_2                          -0.0188840299
#> Schaefer100.l_vis_3                          -0.2343523005
#> Schaefer100.r_vis_1                           0.1656840384
#> Schaefer100.r_vis_2                           1.0000000000
#> Schaefer100.l_sommot_1                       -0.1157103762
#> Schaefer100.l_sommot_2                       -0.0001339495
#> Schaefer100.r_sommot_1                       -0.1371819451
#> Schaefer100.r_sommot_2                       -0.1711362343
#> Schaefer100.l_dorsattn_post_1                 0.1167502439
#> Schaefer100.l_dorsattn_post_2                 0.1511086988
#> Schaefer100.r_dorsattn_post_1                 0.1243338039
#> Schaefer100.r_dorsattn_post_2                 0.1288338553
#> Schaefer100.l_salventattn_paroper_1          -0.1494085655
#> Schaefer100.l_salventattn_froperins_1        -0.4157570164
#> Schaefer100.r_salventattn_tempoccpar_1        0.3072768197
#> Schaefer100.r_salventattn_froperins_1        -0.1121696626
#> Schaefer100.l_limbic_ofc_1                    0.0185267502
#> Schaefer100.l_limbic_temppole_1               0.4163204552
#> Schaefer100.r_limbic_ofc_1                   -0.2833012709
#> Schaefer100.l_cont_par_1                      0.2238254519
#> Schaefer100.l_cont_pfcl_1                    -0.2424047738
#> Schaefer100.r_cont_par_1                     -0.5731792874
#> Schaefer100.r_cont_pfcl_1                    -0.4593666296
#> Schaefer100.l_default_temp_1                 -0.2399778728
#> Schaefer100.l_default_par_1                  -0.1075540832
#> Schaefer100.r_default_temp_1                 -0.4169964469
#> Schaefer100.r_default_par_1                   0.1447183848
#> AHIP                                          0.4124851212
#> PHIP                                          0.0295123519
#>                                        Schaefer100.l_sommot_1
#> Schaefer100.l_vis_1                              0.3754205752
#> Schaefer100.l_vis_2                              0.2186523743
#> Schaefer100.l_vis_3                              0.0953596831
#> Schaefer100.r_vis_1                             -0.2412954293
#> Schaefer100.r_vis_2                             -0.1157103762
#> Schaefer100.l_sommot_1                           1.0000000000
#> Schaefer100.l_sommot_2                           0.0548720458
#> Schaefer100.r_sommot_1                           0.1954887264
#> Schaefer100.r_sommot_2                           0.4319002813
#> Schaefer100.l_dorsattn_post_1                   -0.1231707111
#> Schaefer100.l_dorsattn_post_2                    0.2075835958
#> Schaefer100.r_dorsattn_post_1                   -0.4652099741
#> Schaefer100.r_dorsattn_post_2                   -0.2204922624
#> Schaefer100.l_salventattn_paroper_1              0.7117410816
#> Schaefer100.l_salventattn_froperins_1           -0.1300794318
#> Schaefer100.r_salventattn_tempoccpar_1          -0.3544042810
#> Schaefer100.r_salventattn_froperins_1            0.2123144556
#> Schaefer100.l_limbic_ofc_1                      -0.1827869816
#> Schaefer100.l_limbic_temppole_1                  0.2071521796
#> Schaefer100.r_limbic_ofc_1                       0.2608452782
#> Schaefer100.l_cont_par_1                        -0.0639172033
#> Schaefer100.l_cont_pfcl_1                       -0.2816495070
#> Schaefer100.r_cont_par_1                        -0.0003811874
#> Schaefer100.r_cont_pfcl_1                        0.6879768586
#> Schaefer100.l_default_temp_1                    -0.0736632412
#> Schaefer100.l_default_par_1                     -0.1824585138
#> Schaefer100.r_default_temp_1                    -0.0832813450
#> Schaefer100.r_default_par_1                      0.1697981295
#> AHIP                                             0.0615757266
#> PHIP                                             0.2238533511
#>                                        Schaefer100.l_sommot_2
#> Schaefer100.l_vis_1                             -0.0903614892
#> Schaefer100.l_vis_2                              0.2725654244
#> Schaefer100.l_vis_3                              0.0147839086
#> Schaefer100.r_vis_1                              0.1982730196
#> Schaefer100.r_vis_2                             -0.0001339495
#> Schaefer100.l_sommot_1                           0.0548720458
#> Schaefer100.l_sommot_2                           1.0000000000
#> Schaefer100.r_sommot_1                          -0.0534638887
#> Schaefer100.r_sommot_2                           0.0979618350
#> Schaefer100.l_dorsattn_post_1                   -0.0486733976
#> Schaefer100.l_dorsattn_post_2                   -0.2699647783
#> Schaefer100.r_dorsattn_post_1                   -0.2533085045
#> Schaefer100.r_dorsattn_post_2                    0.0709126951
#> Schaefer100.l_salventattn_paroper_1             -0.2504262431
#> Schaefer100.l_salventattn_froperins_1            0.1629976420
#> Schaefer100.r_salventattn_tempoccpar_1           0.4661235722
#> Schaefer100.r_salventattn_froperins_1            0.2273223142
#> Schaefer100.l_limbic_ofc_1                      -0.2074611232
#> Schaefer100.l_limbic_temppole_1                 -0.2714682414
#> Schaefer100.r_limbic_ofc_1                      -0.0768339332
#> Schaefer100.l_cont_par_1                         0.2313101135
#> Schaefer100.l_cont_pfcl_1                        0.1794165689
#> Schaefer100.r_cont_par_1                        -0.0089375772
#> Schaefer100.r_cont_pfcl_1                        0.0571158175
#> Schaefer100.l_default_temp_1                     0.0224291164
#> Schaefer100.l_default_par_1                      0.0804881162
#> Schaefer100.r_default_temp_1                     0.0013166494
#> Schaefer100.r_default_par_1                     -0.1183154515
#> AHIP                                             0.1511010120
#> PHIP                                             0.1115358799
#>                                        Schaefer100.r_sommot_1
#> Schaefer100.l_vis_1                              -0.171872400
#> Schaefer100.l_vis_2                               0.027266358
#> Schaefer100.l_vis_3                               0.192822016
#> Schaefer100.r_vis_1                               0.152482019
#> Schaefer100.r_vis_2                              -0.137181945
#> Schaefer100.l_sommot_1                            0.195488726
#> Schaefer100.l_sommot_2                           -0.053463889
#> Schaefer100.r_sommot_1                            1.000000000
#> Schaefer100.r_sommot_2                           -0.389417073
#> Schaefer100.l_dorsattn_post_1                    -0.181524682
#> Schaefer100.l_dorsattn_post_2                    -0.054298957
#> Schaefer100.r_dorsattn_post_1                     0.062580720
#> Schaefer100.r_dorsattn_post_2                     0.024665896
#> Schaefer100.l_salventattn_paroper_1               0.232997984
#> Schaefer100.l_salventattn_froperins_1            -0.052879216
#> Schaefer100.r_salventattn_tempoccpar_1           -0.063785649
#> Schaefer100.r_salventattn_froperins_1            -0.191848652
#> Schaefer100.l_limbic_ofc_1                       -0.061080176
#> Schaefer100.l_limbic_temppole_1                   0.228621375
#> Schaefer100.r_limbic_ofc_1                       -0.141513874
#> Schaefer100.l_cont_par_1                          0.081490226
#> Schaefer100.l_cont_pfcl_1                         0.157361505
#> Schaefer100.r_cont_par_1                          0.008879022
#> Schaefer100.r_cont_pfcl_1                        -0.125396843
#> Schaefer100.l_default_temp_1                     -0.057744368
#> Schaefer100.l_default_par_1                      -0.227771142
#> Schaefer100.r_default_temp_1                     -0.321169584
#> Schaefer100.r_default_par_1                       0.245035680
#> AHIP                                              0.205441501
#> PHIP                                              0.159259465
#>                                        Schaefer100.r_sommot_2
#> Schaefer100.l_vis_1                               -0.22132122
#> Schaefer100.l_vis_2                               -0.12122421
#> Schaefer100.l_vis_3                                0.36274922
#> Schaefer100.r_vis_1                               -0.20595060
#> Schaefer100.r_vis_2                               -0.17113623
#> Schaefer100.l_sommot_1                             0.43190028
#> Schaefer100.l_sommot_2                             0.09796183
#> Schaefer100.r_sommot_1                            -0.38941707
#> Schaefer100.r_sommot_2                             1.00000000
#> Schaefer100.l_dorsattn_post_1                      0.13704401
#> Schaefer100.l_dorsattn_post_2                      0.12916083
#> Schaefer100.r_dorsattn_post_1                      0.04558546
#> Schaefer100.r_dorsattn_post_2                      0.07695087
#> Schaefer100.l_salventattn_paroper_1               -0.17552793
#> Schaefer100.l_salventattn_froperins_1              0.48481158
#> Schaefer100.r_salventattn_tempoccpar_1             0.02130776
#> Schaefer100.r_salventattn_froperins_1              0.40615211
#> Schaefer100.l_limbic_ofc_1                        -0.06133894
#> Schaefer100.l_limbic_temppole_1                    0.34748801
#> Schaefer100.r_limbic_ofc_1                        -0.45609890
#> Schaefer100.l_cont_par_1                          -0.12313197
#> Schaefer100.l_cont_pfcl_1                          0.02193804
#> Schaefer100.r_cont_par_1                           0.09231646
#> Schaefer100.r_cont_pfcl_1                          0.25811593
#> Schaefer100.l_default_temp_1                       0.23403378
#> Schaefer100.l_default_par_1                       -0.07351522
#> Schaefer100.r_default_temp_1                      -0.30292142
#> Schaefer100.r_default_par_1                       -0.40170652
#> AHIP                                               0.18401482
#> PHIP                                               0.20034467
#>                                        Schaefer100.l_dorsattn_post_1
#> Schaefer100.l_vis_1                                      0.059902655
#> Schaefer100.l_vis_2                                      0.087308545
#> Schaefer100.l_vis_3                                      0.410159117
#> Schaefer100.r_vis_1                                     -0.363254288
#> Schaefer100.r_vis_2                                      0.116750244
#> Schaefer100.l_sommot_1                                  -0.123170711
#> Schaefer100.l_sommot_2                                  -0.048673398
#> Schaefer100.r_sommot_1                                  -0.181524682
#> Schaefer100.r_sommot_2                                   0.137044012
#> Schaefer100.l_dorsattn_post_1                            1.000000000
#> Schaefer100.l_dorsattn_post_2                            0.055514838
#> Schaefer100.r_dorsattn_post_1                            0.095191906
#> Schaefer100.r_dorsattn_post_2                           -0.011251766
#> Schaefer100.l_salventattn_paroper_1                     -0.093029104
#> Schaefer100.l_salventattn_froperins_1                   -0.198054320
#> Schaefer100.r_salventattn_tempoccpar_1                  -0.024167873
#> Schaefer100.r_salventattn_froperins_1                   -0.023990591
#> Schaefer100.l_limbic_ofc_1                               0.218480595
#> Schaefer100.l_limbic_temppole_1                         -0.116439879
#> Schaefer100.r_limbic_ofc_1                               0.029124841
#> Schaefer100.l_cont_par_1                                 0.047898411
#> Schaefer100.l_cont_pfcl_1                                0.084186540
#> Schaefer100.r_cont_par_1                                 0.323391161
#> Schaefer100.r_cont_pfcl_1                               -0.328726119
#> Schaefer100.l_default_temp_1                             0.448327289
#> Schaefer100.l_default_par_1                             -0.019378257
#> Schaefer100.r_default_temp_1                             0.181960803
#> Schaefer100.r_default_par_1                             -0.383901811
#> AHIP                                                    -0.001306558
#> PHIP                                                     0.167705949
#>                                        Schaefer100.l_dorsattn_post_2
#> Schaefer100.l_vis_1                                       0.07632594
#> Schaefer100.l_vis_2                                      -0.21710939
#> Schaefer100.l_vis_3                                      -0.21443548
#> Schaefer100.r_vis_1                                      -0.26443798
#> Schaefer100.r_vis_2                                       0.15110870
#> Schaefer100.l_sommot_1                                    0.20758360
#> Schaefer100.l_sommot_2                                   -0.26996478
#> Schaefer100.r_sommot_1                                   -0.05429896
#> Schaefer100.r_sommot_2                                    0.12916083
#> Schaefer100.l_dorsattn_post_1                             0.05551484
#> Schaefer100.l_dorsattn_post_2                             1.00000000
#> Schaefer100.r_dorsattn_post_1                             0.20714434
#> Schaefer100.r_dorsattn_post_2                             0.10350644
#> Schaefer100.l_salventattn_paroper_1                      -0.10125941
#> Schaefer100.l_salventattn_froperins_1                    -0.08138449
#> Schaefer100.r_salventattn_tempoccpar_1                    0.23632333
#> Schaefer100.r_salventattn_froperins_1                    -0.03171833
#> Schaefer100.l_limbic_ofc_1                               -0.04370721
#> Schaefer100.l_limbic_temppole_1                           0.37940099
#> Schaefer100.r_limbic_ofc_1                                0.08924631
#> Schaefer100.l_cont_par_1                                 -0.02394004
#> Schaefer100.l_cont_pfcl_1                                 0.05758806
#> Schaefer100.r_cont_par_1                                 -0.27851351
#> Schaefer100.r_cont_pfcl_1                                -0.01010982
#> Schaefer100.l_default_temp_1                              0.18235821
#> Schaefer100.l_default_par_1                               0.18353042
#> Schaefer100.r_default_temp_1                              0.05003593
#> Schaefer100.r_default_par_1                              -0.08940382
#> AHIP                                                      0.28466774
#> PHIP                                                      0.07264910
#>                                        Schaefer100.r_dorsattn_post_1
#> Schaefer100.l_vis_1                                      0.206605870
#> Schaefer100.l_vis_2                                     -0.209452697
#> Schaefer100.l_vis_3                                     -0.454615997
#> Schaefer100.r_vis_1                                      0.266415213
#> Schaefer100.r_vis_2                                      0.124333804
#> Schaefer100.l_sommot_1                                  -0.465209974
#> Schaefer100.l_sommot_2                                  -0.253308505
#> Schaefer100.r_sommot_1                                   0.062580720
#> Schaefer100.r_sommot_2                                   0.045585456
#> Schaefer100.l_dorsattn_post_1                            0.095191906
#> Schaefer100.l_dorsattn_post_2                            0.207144335
#> Schaefer100.r_dorsattn_post_1                            1.000000000
#> Schaefer100.r_dorsattn_post_2                            0.003000890
#> Schaefer100.l_salventattn_paroper_1                     -0.166895204
#> Schaefer100.l_salventattn_froperins_1                    0.048966300
#> Schaefer100.r_salventattn_tempoccpar_1                  -0.315256471
#> Schaefer100.r_salventattn_froperins_1                    0.225506679
#> Schaefer100.l_limbic_ofc_1                              -0.102019522
#> Schaefer100.l_limbic_temppole_1                         -0.263431714
#> Schaefer100.r_limbic_ofc_1                              -0.042337912
#> Schaefer100.l_cont_par_1                                -0.030398522
#> Schaefer100.l_cont_pfcl_1                               -0.258252260
#> Schaefer100.r_cont_par_1                                -0.108206192
#> Schaefer100.r_cont_pfcl_1                               -0.213570768
#> Schaefer100.l_default_temp_1                            -0.001804314
#> Schaefer100.l_default_par_1                              0.142553799
#> Schaefer100.r_default_temp_1                            -0.075320419
#> Schaefer100.r_default_par_1                              0.106389130
#> AHIP                                                    -0.384496604
#> PHIP                                                     0.239919590
#>                                        Schaefer100.r_dorsattn_post_2
#> Schaefer100.l_vis_1                                       0.02932083
#> Schaefer100.l_vis_2                                      -0.09213046
#> Schaefer100.l_vis_3                                       0.19192575
#> Schaefer100.r_vis_1                                       0.11669720
#> Schaefer100.r_vis_2                                       0.12883386
#> Schaefer100.l_sommot_1                                   -0.22049226
#> Schaefer100.l_sommot_2                                    0.07091270
#> Schaefer100.r_sommot_1                                    0.02466590
#> Schaefer100.r_sommot_2                                    0.07695087
#> Schaefer100.l_dorsattn_post_1                            -0.01125177
#> Schaefer100.l_dorsattn_post_2                             0.10350644
#> Schaefer100.r_dorsattn_post_1                             0.00300089
#> Schaefer100.r_dorsattn_post_2                             1.00000000
#> Schaefer100.l_salventattn_paroper_1                       0.02859474
#> Schaefer100.l_salventattn_froperins_1                     0.12152566
#> Schaefer100.r_salventattn_tempoccpar_1                   -0.01064659
#> Schaefer100.r_salventattn_froperins_1                     0.04459547
#> Schaefer100.l_limbic_ofc_1                               -0.08309174
#> Schaefer100.l_limbic_temppole_1                           0.16446060
#> Schaefer100.r_limbic_ofc_1                               -0.44577607
#> Schaefer100.l_cont_par_1                                  0.25832108
#> Schaefer100.l_cont_pfcl_1                                 0.05213755
#> Schaefer100.r_cont_par_1                                  0.08588529
#> Schaefer100.r_cont_pfcl_1                                -0.26221675
#> Schaefer100.l_default_temp_1                             -0.22012764
#> Schaefer100.l_default_par_1                               0.29927069
#> Schaefer100.r_default_temp_1                              0.20144782
#> Schaefer100.r_default_par_1                              -0.18277626
#> AHIP                                                      0.08712146
#> PHIP                                                      0.02413522
#>                                        Schaefer100.l_salventattn_paroper_1
#> Schaefer100.l_vis_1                                          -0.0804544373
#> Schaefer100.l_vis_2                                           0.0016964445
#> Schaefer100.l_vis_3                                           0.0262575086
#> Schaefer100.r_vis_1                                          -0.1765530317
#> Schaefer100.r_vis_2                                          -0.1494085655
#> Schaefer100.l_sommot_1                                        0.7117410816
#> Schaefer100.l_sommot_2                                       -0.2504262431
#> Schaefer100.r_sommot_1                                        0.2329979841
#> Schaefer100.r_sommot_2                                       -0.1755279348
#> Schaefer100.l_dorsattn_post_1                                -0.0930291043
#> Schaefer100.l_dorsattn_post_2                                -0.1012594091
#> Schaefer100.r_dorsattn_post_1                                -0.1668952037
#> Schaefer100.r_dorsattn_post_2                                 0.0285947397
#> Schaefer100.l_salventattn_paroper_1                           1.0000000000
#> Schaefer100.l_salventattn_froperins_1                        -0.0502410436
#> Schaefer100.r_salventattn_tempoccpar_1                       -0.3061847283
#> Schaefer100.r_salventattn_froperins_1                        -0.2159619167
#> Schaefer100.l_limbic_ofc_1                                    0.0720691905
#> Schaefer100.l_limbic_temppole_1                              -0.0433476660
#> Schaefer100.r_limbic_ofc_1                                    0.1088924948
#> Schaefer100.l_cont_par_1                                      0.0714339064
#> Schaefer100.l_cont_pfcl_1                                     0.1346320143
#> Schaefer100.r_cont_par_1                                      0.0154550907
#> Schaefer100.r_cont_pfcl_1                                    -0.0005788842
#> Schaefer100.l_default_temp_1                                  0.1478849508
#> Schaefer100.l_default_par_1                                  -0.6502875709
#> Schaefer100.r_default_temp_1                                  0.0360643428
#> Schaefer100.r_default_par_1                                   0.1830153927
#> AHIP                                                         -0.0931152830
#> PHIP                                                          0.0998528036
#>                                        Schaefer100.l_salventattn_froperins_1
#> Schaefer100.l_vis_1                                              0.183549266
#> Schaefer100.l_vis_2                                              0.224157963
#> Schaefer100.l_vis_3                                             -0.084253914
#> Schaefer100.r_vis_1                                              0.048097232
#> Schaefer100.r_vis_2                                             -0.415757016
#> Schaefer100.l_sommot_1                                          -0.130079432
#> Schaefer100.l_sommot_2                                           0.162997642
#> Schaefer100.r_sommot_1                                          -0.052879216
#> Schaefer100.r_sommot_2                                           0.484811579
#> Schaefer100.l_dorsattn_post_1                                   -0.198054320
#> Schaefer100.l_dorsattn_post_2                                   -0.081384489
#> Schaefer100.r_dorsattn_post_1                                    0.048966300
#> Schaefer100.r_dorsattn_post_2                                    0.121525664
#> Schaefer100.l_salventattn_paroper_1                             -0.050241044
#> Schaefer100.l_salventattn_froperins_1                            1.000000000
#> Schaefer100.r_salventattn_tempoccpar_1                          -0.325531179
#> Schaefer100.r_salventattn_froperins_1                           -0.024055151
#> Schaefer100.l_limbic_ofc_1                                       0.329913545
#> Schaefer100.l_limbic_temppole_1                                 -0.085960846
#> Schaefer100.r_limbic_ofc_1                                       0.029510553
#> Schaefer100.l_cont_par_1                                        -0.024914913
#> Schaefer100.l_cont_pfcl_1                                        0.008845698
#> Schaefer100.r_cont_par_1                                         0.112675503
#> Schaefer100.r_cont_pfcl_1                                       -0.234689893
#> Schaefer100.l_default_temp_1                                    -0.105175485
#> Schaefer100.l_default_par_1                                      0.080109462
#> Schaefer100.r_default_temp_1                                     0.040226862
#> Schaefer100.r_default_par_1                                      0.092339460
#> AHIP                                                             0.040278486
#> PHIP                                                            -0.164264269
#>                                        Schaefer100.r_salventattn_tempoccpar_1
#> Schaefer100.l_vis_1                                                0.48259732
#> Schaefer100.l_vis_2                                               -0.01147196
#> Schaefer100.l_vis_3                                                0.21912905
#> Schaefer100.r_vis_1                                                0.10052260
#> Schaefer100.r_vis_2                                                0.30727682
#> Schaefer100.l_sommot_1                                            -0.35440428
#> Schaefer100.l_sommot_2                                             0.46612357
#> Schaefer100.r_sommot_1                                            -0.06378565
#> Schaefer100.r_sommot_2                                             0.02130776
#> Schaefer100.l_dorsattn_post_1                                     -0.02416787
#> Schaefer100.l_dorsattn_post_2                                      0.23632333
#> Schaefer100.r_dorsattn_post_1                                     -0.31525647
#> Schaefer100.r_dorsattn_post_2                                     -0.01064659
#> Schaefer100.l_salventattn_paroper_1                               -0.30618473
#> Schaefer100.l_salventattn_froperins_1                             -0.32553118
#> Schaefer100.r_salventattn_tempoccpar_1                             1.00000000
#> Schaefer100.r_salventattn_froperins_1                             -0.09257686
#> Schaefer100.l_limbic_ofc_1                                         0.34114593
#> Schaefer100.l_limbic_temppole_1                                   -0.20907252
#> Schaefer100.r_limbic_ofc_1                                         0.13481711
#> Schaefer100.l_cont_par_1                                          -0.09394209
#> Schaefer100.l_cont_pfcl_1                                          0.09268079
#> Schaefer100.r_cont_par_1                                          -0.49269881
#> Schaefer100.r_cont_pfcl_1                                          0.05259467
#> Schaefer100.l_default_temp_1                                       0.04663096
#> Schaefer100.l_default_par_1                                        0.16793096
#> Schaefer100.r_default_temp_1                                       0.37966997
#> Schaefer100.r_default_par_1                                       -0.13770540
#> AHIP                                                              -0.03457899
#> PHIP                                                               0.03445099
#>                                        Schaefer100.r_salventattn_froperins_1
#> Schaefer100.l_vis_1                                               0.07774518
#> Schaefer100.l_vis_2                                              -0.01332408
#> Schaefer100.l_vis_3                                               0.15651625
#> Schaefer100.r_vis_1                                              -0.09953791
#> Schaefer100.r_vis_2                                              -0.11216966
#> Schaefer100.l_sommot_1                                            0.21231446
#> Schaefer100.l_sommot_2                                            0.22732231
#> Schaefer100.r_sommot_1                                           -0.19184865
#> Schaefer100.r_sommot_2                                            0.40615211
#> Schaefer100.l_dorsattn_post_1                                    -0.02399059
#> Schaefer100.l_dorsattn_post_2                                    -0.03171833
#> Schaefer100.r_dorsattn_post_1                                     0.22550668
#> Schaefer100.r_dorsattn_post_2                                     0.04459547
#> Schaefer100.l_salventattn_paroper_1                              -0.21596192
#> Schaefer100.l_salventattn_froperins_1                            -0.02405515
#> Schaefer100.r_salventattn_tempoccpar_1                           -0.09257686
#> Schaefer100.r_salventattn_froperins_1                             1.00000000
#> Schaefer100.l_limbic_ofc_1                                        0.21022750
#> Schaefer100.l_limbic_temppole_1                                   0.04692252
#> Schaefer100.r_limbic_ofc_1                                        0.14003753
#> Schaefer100.l_cont_par_1                                         -0.15826360
#> Schaefer100.l_cont_pfcl_1                                        -0.27367652
#> Schaefer100.r_cont_par_1                                         -0.01396720
#> Schaefer100.r_cont_pfcl_1                                         0.15606080
#> Schaefer100.l_default_temp_1                                     -0.13307000
#> Schaefer100.l_default_par_1                                       0.17721468
#> Schaefer100.r_default_temp_1                                     -0.07613740
#> Schaefer100.r_default_par_1                                      -0.36586555
#> AHIP                                                             -0.19630939
#> PHIP                                                             -0.16760141
#>                                        Schaefer100.l_limbic_ofc_1
#> Schaefer100.l_vis_1                                   -0.30959796
#> Schaefer100.l_vis_2                                   -0.03757542
#> Schaefer100.l_vis_3                                   -0.05073788
#> Schaefer100.r_vis_1                                   -0.21935166
#> Schaefer100.r_vis_2                                    0.01852675
#> Schaefer100.l_sommot_1                                -0.18278698
#> Schaefer100.l_sommot_2                                -0.20746112
#> Schaefer100.r_sommot_1                                -0.06108018
#> Schaefer100.r_sommot_2                                -0.06133894
#> Schaefer100.l_dorsattn_post_1                          0.21848059
#> Schaefer100.l_dorsattn_post_2                         -0.04370721
#> Schaefer100.r_dorsattn_post_1                         -0.10201952
#> Schaefer100.r_dorsattn_post_2                         -0.08309174
#> Schaefer100.l_salventattn_paroper_1                    0.07206919
#> Schaefer100.l_salventattn_froperins_1                  0.32991355
#> Schaefer100.r_salventattn_tempoccpar_1                 0.34114593
#> Schaefer100.r_salventattn_froperins_1                  0.21022750
#> Schaefer100.l_limbic_ofc_1                             1.00000000
#> Schaefer100.l_limbic_temppole_1                       -0.01749751
#> Schaefer100.r_limbic_ofc_1                            -0.20129591
#> Schaefer100.l_cont_par_1                              -0.01051674
#> Schaefer100.l_cont_pfcl_1                             -0.12553174
#> Schaefer100.r_cont_par_1                              -0.41224815
#> Schaefer100.r_cont_pfcl_1                             -0.11643331
#> Schaefer100.l_default_temp_1                           0.27737477
#> Schaefer100.l_default_par_1                            0.08639296
#> Schaefer100.r_default_temp_1                           0.10789775
#> Schaefer100.r_default_par_1                           -0.22406417
#> AHIP                                                  -0.14392125
#> PHIP                                                  -0.12101311
#>                                        Schaefer100.l_limbic_temppole_1
#> Schaefer100.l_vis_1                                        0.064216169
#> Schaefer100.l_vis_2                                        0.046689801
#> Schaefer100.l_vis_3                                        0.132820615
#> Schaefer100.r_vis_1                                       -0.236638503
#> Schaefer100.r_vis_2                                        0.416320455
#> Schaefer100.l_sommot_1                                     0.207152180
#> Schaefer100.l_sommot_2                                    -0.271468241
#> Schaefer100.r_sommot_1                                     0.228621375
#> Schaefer100.r_sommot_2                                     0.347488014
#> Schaefer100.l_dorsattn_post_1                             -0.116439879
#> Schaefer100.l_dorsattn_post_2                              0.379400992
#> Schaefer100.r_dorsattn_post_1                             -0.263431714
#> Schaefer100.r_dorsattn_post_2                              0.164460601
#> Schaefer100.l_salventattn_paroper_1                       -0.043347666
#> Schaefer100.l_salventattn_froperins_1                     -0.085960846
#> Schaefer100.r_salventattn_tempoccpar_1                    -0.209072519
#> Schaefer100.r_salventattn_froperins_1                      0.046922520
#> Schaefer100.l_limbic_ofc_1                                -0.017497512
#> Schaefer100.l_limbic_temppole_1                            1.000000000
#> Schaefer100.r_limbic_ofc_1                                -0.006988101
#> Schaefer100.l_cont_par_1                                  -0.308515464
#> Schaefer100.l_cont_pfcl_1                                  0.197930535
#> Schaefer100.r_cont_par_1                                  -0.291180662
#> Schaefer100.r_cont_pfcl_1                                  0.256553381
#> Schaefer100.l_default_temp_1                              -0.189220467
#> Schaefer100.l_default_par_1                               -0.033122015
#> Schaefer100.r_default_temp_1                               0.114615633
#> Schaefer100.r_default_par_1                               -0.087431988
#> AHIP                                                      -0.023255727
#> PHIP                                                      -0.102765603
#>                                        Schaefer100.r_limbic_ofc_1
#> Schaefer100.l_vis_1                                   0.073270912
#> Schaefer100.l_vis_2                                  -0.230975248
#> Schaefer100.l_vis_3                                  -0.138094661
#> Schaefer100.r_vis_1                                   0.185909839
#> Schaefer100.r_vis_2                                  -0.283301271
#> Schaefer100.l_sommot_1                                0.260845278
#> Schaefer100.l_sommot_2                               -0.076833933
#> Schaefer100.r_sommot_1                               -0.141513874
#> Schaefer100.r_sommot_2                               -0.456098898
#> Schaefer100.l_dorsattn_post_1                         0.029124841
#> Schaefer100.l_dorsattn_post_2                         0.089246314
#> Schaefer100.r_dorsattn_post_1                        -0.042337912
#> Schaefer100.r_dorsattn_post_2                        -0.445776070
#> Schaefer100.l_salventattn_paroper_1                   0.108892495
#> Schaefer100.l_salventattn_froperins_1                 0.029510553
#> Schaefer100.r_salventattn_tempoccpar_1                0.134817115
#> Schaefer100.r_salventattn_froperins_1                 0.140037532
#> Schaefer100.l_limbic_ofc_1                           -0.201295905
#> Schaefer100.l_limbic_temppole_1                      -0.006988101
#> Schaefer100.r_limbic_ofc_1                            1.000000000
#> Schaefer100.l_cont_par_1                             -0.189474829
#> Schaefer100.l_cont_pfcl_1                             0.218240940
#> Schaefer100.r_cont_par_1                              0.022085295
#> Schaefer100.r_cont_pfcl_1                             0.069335361
#> Schaefer100.l_default_temp_1                          0.228492499
#> Schaefer100.l_default_par_1                           0.049788196
#> Schaefer100.r_default_temp_1                          0.078054713
#> Schaefer100.r_default_par_1                          -0.507532953
#> AHIP                                                  0.420720315
#> PHIP                                                  0.338281010
#>                                        Schaefer100.l_cont_par_1
#> Schaefer100.l_vis_1                                0.0009282831
#> Schaefer100.l_vis_2                                0.0338957321
#> Schaefer100.l_vis_3                               -0.0041349235
#> Schaefer100.r_vis_1                               -0.3137121879
#> Schaefer100.r_vis_2                                0.2238254519
#> Schaefer100.l_sommot_1                            -0.0639172033
#> Schaefer100.l_sommot_2                             0.2313101135
#> Schaefer100.r_sommot_1                             0.0814902259
#> Schaefer100.r_sommot_2                            -0.1231319721
#> Schaefer100.l_dorsattn_post_1                      0.0478984107
#> Schaefer100.l_dorsattn_post_2                     -0.0239400372
#> Schaefer100.r_dorsattn_post_1                     -0.0303985223
#> Schaefer100.r_dorsattn_post_2                      0.2583210825
#> Schaefer100.l_salventattn_paroper_1                0.0714339064
#> Schaefer100.l_salventattn_froperins_1             -0.0249149129
#> Schaefer100.r_salventattn_tempoccpar_1            -0.0939420924
#> Schaefer100.r_salventattn_froperins_1             -0.1582636014
#> Schaefer100.l_limbic_ofc_1                        -0.0105167434
#> Schaefer100.l_limbic_temppole_1                   -0.3085154640
#> Schaefer100.r_limbic_ofc_1                        -0.1894748287
#> Schaefer100.l_cont_par_1                           1.0000000000
#> Schaefer100.l_cont_pfcl_1                         -0.2430171662
#> Schaefer100.r_cont_par_1                           0.1843112142
#> Schaefer100.r_cont_pfcl_1                          0.1858928163
#> Schaefer100.l_default_temp_1                      -0.0245480333
#> Schaefer100.l_default_par_1                       -0.1090143359
#> Schaefer100.r_default_temp_1                       0.0790307140
#> Schaefer100.r_default_par_1                       -0.0013763452
#> AHIP                                              -0.0715259056
#> PHIP                                               0.1644314706
#>                                        Schaefer100.l_cont_pfcl_1
#> Schaefer100.l_vis_1                                 -0.134471709
#> Schaefer100.l_vis_2                                 -0.282617761
#> Schaefer100.l_vis_3                                  0.137780965
#> Schaefer100.r_vis_1                                  0.137729187
#> Schaefer100.r_vis_2                                 -0.242404774
#> Schaefer100.l_sommot_1                              -0.281649507
#> Schaefer100.l_sommot_2                               0.179416569
#> Schaefer100.r_sommot_1                               0.157361505
#> Schaefer100.r_sommot_2                               0.021938037
#> Schaefer100.l_dorsattn_post_1                        0.084186540
#> Schaefer100.l_dorsattn_post_2                        0.057588057
#> Schaefer100.r_dorsattn_post_1                       -0.258252260
#> Schaefer100.r_dorsattn_post_2                        0.052137549
#> Schaefer100.l_salventattn_paroper_1                  0.134632014
#> Schaefer100.l_salventattn_froperins_1                0.008845698
#> Schaefer100.r_salventattn_tempoccpar_1               0.092680791
#> Schaefer100.r_salventattn_froperins_1               -0.273676518
#> Schaefer100.l_limbic_ofc_1                          -0.125531736
#> Schaefer100.l_limbic_temppole_1                      0.197930535
#> Schaefer100.r_limbic_ofc_1                           0.218240940
#> Schaefer100.l_cont_par_1                            -0.243017166
#> Schaefer100.l_cont_pfcl_1                            1.000000000
#> Schaefer100.r_cont_par_1                             0.238633802
#> Schaefer100.r_cont_pfcl_1                           -0.249550889
#> Schaefer100.l_default_temp_1                         0.220633177
#> Schaefer100.l_default_par_1                         -0.189750637
#> Schaefer100.r_default_temp_1                         0.054336421
#> Schaefer100.r_default_par_1                         -0.008827209
#> AHIP                                                 0.136726609
#> PHIP                                                 0.102414108
#>                                        Schaefer100.r_cont_par_1
#> Schaefer100.l_vis_1                               -0.0243534021
#> Schaefer100.l_vis_2                                0.2006466107
#> Schaefer100.l_vis_3                                0.2393381975
#> Schaefer100.r_vis_1                               -0.2469416645
#> Schaefer100.r_vis_2                               -0.5731792874
#> Schaefer100.l_sommot_1                            -0.0003811874
#> Schaefer100.l_sommot_2                            -0.0089375772
#> Schaefer100.r_sommot_1                             0.0088790219
#> Schaefer100.r_sommot_2                             0.0923164580
#> Schaefer100.l_dorsattn_post_1                      0.3233911612
#> Schaefer100.l_dorsattn_post_2                     -0.2785135076
#> Schaefer100.r_dorsattn_post_1                     -0.1082061922
#> Schaefer100.r_dorsattn_post_2                      0.0858852909
#> Schaefer100.l_salventattn_paroper_1                0.0154550907
#> Schaefer100.l_salventattn_froperins_1              0.1126755032
#> Schaefer100.r_salventattn_tempoccpar_1            -0.4926988067
#> Schaefer100.r_salventattn_froperins_1             -0.0139671998
#> Schaefer100.l_limbic_ofc_1                        -0.4122481470
#> Schaefer100.l_limbic_temppole_1                   -0.2911806616
#> Schaefer100.r_limbic_ofc_1                         0.0220852949
#> Schaefer100.l_cont_par_1                           0.1843112142
#> Schaefer100.l_cont_pfcl_1                          0.2386338021
#> Schaefer100.r_cont_par_1                           1.0000000000
#> Schaefer100.r_cont_pfcl_1                         -0.1635713664
#> Schaefer100.l_default_temp_1                       0.2011856754
#> Schaefer100.l_default_par_1                        0.0064310029
#> Schaefer100.r_default_temp_1                       0.0924259705
#> Schaefer100.r_default_par_1                        0.1909616080
#> AHIP                                              -0.2566212549
#> PHIP                                              -0.4792358521
#>                                        Schaefer100.r_cont_pfcl_1
#> Schaefer100.l_vis_1                                -0.1230488257
#> Schaefer100.l_vis_2                                 0.3833683642
#> Schaefer100.l_vis_3                                 0.2753966726
#> Schaefer100.r_vis_1                                -0.1266787775
#> Schaefer100.r_vis_2                                -0.4593666296
#> Schaefer100.l_sommot_1                              0.6879768586
#> Schaefer100.l_sommot_2                              0.0571158175
#> Schaefer100.r_sommot_1                             -0.1253968432
#> Schaefer100.r_sommot_2                              0.2581159300
#> Schaefer100.l_dorsattn_post_1                      -0.3287261187
#> Schaefer100.l_dorsattn_post_2                      -0.0101098220
#> Schaefer100.r_dorsattn_post_1                      -0.2135707683
#> Schaefer100.r_dorsattn_post_2                      -0.2622167485
#> Schaefer100.l_salventattn_paroper_1                -0.0005788842
#> Schaefer100.l_salventattn_froperins_1              -0.2346898929
#> Schaefer100.r_salventattn_tempoccpar_1              0.0525946668
#> Schaefer100.r_salventattn_froperins_1               0.1560607997
#> Schaefer100.l_limbic_ofc_1                         -0.1164333090
#> Schaefer100.l_limbic_temppole_1                     0.2565533811
#> Schaefer100.r_limbic_ofc_1                          0.0693353608
#> Schaefer100.l_cont_par_1                            0.1858928163
#> Schaefer100.l_cont_pfcl_1                          -0.2495508888
#> Schaefer100.r_cont_par_1                           -0.1635713664
#> Schaefer100.r_cont_pfcl_1                           1.0000000000
#> Schaefer100.l_default_temp_1                        0.3158816430
#> Schaefer100.l_default_par_1                        -0.0739052810
#> Schaefer100.r_default_temp_1                        0.2981006690
#> Schaefer100.r_default_par_1                         0.1843724703
#> AHIP                                               -0.0739194466
#> PHIP                                                0.3347731184
#>                                        Schaefer100.l_default_temp_1
#> Schaefer100.l_vis_1                                    -0.250006884
#> Schaefer100.l_vis_2                                    -0.293111325
#> Schaefer100.l_vis_3                                     0.063201520
#> Schaefer100.r_vis_1                                    -0.151485499
#> Schaefer100.r_vis_2                                    -0.239977873
#> Schaefer100.l_sommot_1                                 -0.073663241
#> Schaefer100.l_sommot_2                                  0.022429116
#> Schaefer100.r_sommot_1                                 -0.057744368
#> Schaefer100.r_sommot_2                                  0.234033775
#> Schaefer100.l_dorsattn_post_1                           0.448327289
#> Schaefer100.l_dorsattn_post_2                           0.182358209
#> Schaefer100.r_dorsattn_post_1                          -0.001804314
#> Schaefer100.r_dorsattn_post_2                          -0.220127643
#> Schaefer100.l_salventattn_paroper_1                     0.147884951
#> Schaefer100.l_salventattn_froperins_1                  -0.105175485
#> Schaefer100.r_salventattn_tempoccpar_1                  0.046630956
#> Schaefer100.r_salventattn_froperins_1                  -0.133069995
#> Schaefer100.l_limbic_ofc_1                              0.277374772
#> Schaefer100.l_limbic_temppole_1                        -0.189220467
#> Schaefer100.r_limbic_ofc_1                              0.228492499
#> Schaefer100.l_cont_par_1                               -0.024548033
#> Schaefer100.l_cont_pfcl_1                               0.220633177
#> Schaefer100.r_cont_par_1                                0.201185675
#> Schaefer100.r_cont_pfcl_1                               0.315881643
#> Schaefer100.l_default_temp_1                            1.000000000
#> Schaefer100.l_default_par_1                            -0.289650787
#> Schaefer100.r_default_temp_1                            0.445198284
#> Schaefer100.r_default_par_1                            -0.101167745
#> AHIP                                                    0.331717467
#> PHIP                                                    0.125285238
#>                                        Schaefer100.l_default_par_1
#> Schaefer100.l_vis_1                                  -0.0222394668
#> Schaefer100.l_vis_2                                   0.2110140425
#> Schaefer100.l_vis_3                                   0.1264879078
#> Schaefer100.r_vis_1                                   0.0772667013
#> Schaefer100.r_vis_2                                  -0.1075540832
#> Schaefer100.l_sommot_1                               -0.1824585138
#> Schaefer100.l_sommot_2                                0.0804881162
#> Schaefer100.r_sommot_1                               -0.2277711419
#> Schaefer100.r_sommot_2                               -0.0735152153
#> Schaefer100.l_dorsattn_post_1                        -0.0193782567
#> Schaefer100.l_dorsattn_post_2                         0.1835304191
#> Schaefer100.r_dorsattn_post_1                         0.1425537988
#> Schaefer100.r_dorsattn_post_2                         0.2992706932
#> Schaefer100.l_salventattn_paroper_1                  -0.6502875709
#> Schaefer100.l_salventattn_froperins_1                 0.0801094619
#> Schaefer100.r_salventattn_tempoccpar_1                0.1679309572
#> Schaefer100.r_salventattn_froperins_1                 0.1772146846
#> Schaefer100.l_limbic_ofc_1                            0.0863929603
#> Schaefer100.l_limbic_temppole_1                      -0.0331220152
#> Schaefer100.r_limbic_ofc_1                            0.0497881960
#> Schaefer100.l_cont_par_1                             -0.1090143359
#> Schaefer100.l_cont_pfcl_1                            -0.1897506371
#> Schaefer100.r_cont_par_1                              0.0064310029
#> Schaefer100.r_cont_pfcl_1                            -0.0739052810
#> Schaefer100.l_default_temp_1                         -0.2896507872
#> Schaefer100.l_default_par_1                           1.0000000000
#> Schaefer100.r_default_temp_1                          0.0210066014
#> Schaefer100.r_default_par_1                           0.0008137322
#> AHIP                                                  0.4541906741
#> PHIP                                                 -0.1470113675
#>                                        Schaefer100.r_default_temp_1
#> Schaefer100.l_vis_1                                     0.247678113
#> Schaefer100.l_vis_2                                    -0.096647601
#> Schaefer100.l_vis_3                                     0.164929601
#> Schaefer100.r_vis_1                                    -0.160652395
#> Schaefer100.r_vis_2                                    -0.416996447
#> Schaefer100.l_sommot_1                                 -0.083281345
#> Schaefer100.l_sommot_2                                  0.001316649
#> Schaefer100.r_sommot_1                                 -0.321169584
#> Schaefer100.r_sommot_2                                 -0.302921418
#> Schaefer100.l_dorsattn_post_1                           0.181960803
#> Schaefer100.l_dorsattn_post_2                           0.050035934
#> Schaefer100.r_dorsattn_post_1                          -0.075320419
#> Schaefer100.r_dorsattn_post_2                           0.201447816
#> Schaefer100.l_salventattn_paroper_1                     0.036064343
#> Schaefer100.l_salventattn_froperins_1                   0.040226862
#> Schaefer100.r_salventattn_tempoccpar_1                  0.379669967
#> Schaefer100.r_salventattn_froperins_1                  -0.076137399
#> Schaefer100.l_limbic_ofc_1                              0.107897754
#> Schaefer100.l_limbic_temppole_1                         0.114615633
#> Schaefer100.r_limbic_ofc_1                              0.078054713
#> Schaefer100.l_cont_par_1                                0.079030714
#> Schaefer100.l_cont_pfcl_1                               0.054336421
#> Schaefer100.r_cont_par_1                                0.092425971
#> Schaefer100.r_cont_pfcl_1                               0.298100669
#> Schaefer100.l_default_temp_1                            0.445198284
#> Schaefer100.l_default_par_1                             0.021006601
#> Schaefer100.r_default_temp_1                            1.000000000
#> Schaefer100.r_default_par_1                             0.061552249
#> AHIP                                                   -0.237586806
#> PHIP                                                    0.012412786
#>                                        Schaefer100.r_default_par_1         AHIP
#> Schaefer100.l_vis_1                                  -0.3531817027 -0.284630258
#> Schaefer100.l_vis_2                                  -0.1471002023  0.421306986
#> Schaefer100.l_vis_3                                   0.0536262435 -0.117648259
#> Schaefer100.r_vis_1                                  -0.0651617945 -0.117819118
#> Schaefer100.r_vis_2                                   0.1447183848  0.412485121
#> Schaefer100.l_sommot_1                                0.1697981295  0.061575727
#> Schaefer100.l_sommot_2                               -0.1183154515  0.151101012
#> Schaefer100.r_sommot_1                                0.2450356799  0.205441501
#> Schaefer100.r_sommot_2                               -0.4017065158  0.184014819
#> Schaefer100.l_dorsattn_post_1                        -0.3839018110 -0.001306558
#> Schaefer100.l_dorsattn_post_2                        -0.0894038197  0.284667744
#> Schaefer100.r_dorsattn_post_1                         0.1063891297 -0.384496604
#> Schaefer100.r_dorsattn_post_2                        -0.1827762619  0.087121462
#> Schaefer100.l_salventattn_paroper_1                   0.1830153927 -0.093115283
#> Schaefer100.l_salventattn_froperins_1                 0.0923394598  0.040278486
#> Schaefer100.r_salventattn_tempoccpar_1               -0.1377053992 -0.034578989
#> Schaefer100.r_salventattn_froperins_1                -0.3658655469 -0.196309392
#> Schaefer100.l_limbic_ofc_1                           -0.2240641652 -0.143921251
#> Schaefer100.l_limbic_temppole_1                      -0.0874319882 -0.023255727
#> Schaefer100.r_limbic_ofc_1                           -0.5075329534  0.420720315
#> Schaefer100.l_cont_par_1                             -0.0013763452 -0.071525906
#> Schaefer100.l_cont_pfcl_1                            -0.0088272089  0.136726609
#> Schaefer100.r_cont_par_1                              0.1909616080 -0.256621255
#> Schaefer100.r_cont_pfcl_1                             0.1843724703 -0.073919447
#> Schaefer100.l_default_temp_1                         -0.1011677446  0.331717467
#> Schaefer100.l_default_par_1                           0.0008137322  0.454190674
#> Schaefer100.r_default_temp_1                          0.0615522490 -0.237586806
#> Schaefer100.r_default_par_1                           1.0000000000  0.178090425
#> AHIP                                                  0.1780904252  1.000000000
#> PHIP                                                 -0.0080049444  0.305570829
#>                                                PHIP
#> Schaefer100.l_vis_1                     0.017192142
#> Schaefer100.l_vis_2                     0.053934721
#> Schaefer100.l_vis_3                     0.007388502
#> Schaefer100.r_vis_1                    -0.018091855
#> Schaefer100.r_vis_2                     0.029512352
#> Schaefer100.l_sommot_1                  0.223853351
#> Schaefer100.l_sommot_2                  0.111535880
#> Schaefer100.r_sommot_1                  0.159259465
#> Schaefer100.r_sommot_2                  0.200344670
#> Schaefer100.l_dorsattn_post_1           0.167705949
#> Schaefer100.l_dorsattn_post_2           0.072649099
#> Schaefer100.r_dorsattn_post_1           0.239919590
#> Schaefer100.r_dorsattn_post_2           0.024135218
#> Schaefer100.l_salventattn_paroper_1     0.099852804
#> Schaefer100.l_salventattn_froperins_1  -0.164264269
#> Schaefer100.r_salventattn_tempoccpar_1  0.034450990
#> Schaefer100.r_salventattn_froperins_1  -0.167601412
#> Schaefer100.l_limbic_ofc_1             -0.121013113
#> Schaefer100.l_limbic_temppole_1        -0.102765603
#> Schaefer100.r_limbic_ofc_1              0.338281010
#> Schaefer100.l_cont_par_1                0.164431471
#> Schaefer100.l_cont_pfcl_1               0.102414108
#> Schaefer100.r_cont_par_1               -0.479235852
#> Schaefer100.r_cont_pfcl_1               0.334773118
#> Schaefer100.l_default_temp_1            0.125285238
#> Schaefer100.l_default_par_1            -0.147011367
#> Schaefer100.r_default_temp_1            0.012412786
#> Schaefer100.r_default_par_1            -0.008004944
#> AHIP                                    0.305570829
#> PHIP                                    1.000000000
```
