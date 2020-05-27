# Smoothing methods

Each method is applied on individual cells' data, and independently per chromosome.

## Runmeans
Simple mean over the genes within (window_size-1)/2 on each side.

## Pyramidal smoothing
Weighted mean over the genes within (window_size-1)/2 on each side. With `n = (window_size-1)/2`, the weight for the m-th gene in the neighborhood of a given gene is `(n+1-m)/(n+1)`

## Coordinates based method
_This feature is only available from version **1.1.3** onward._
Weighted mean over the genes within window_size base pairs on each side of a given gene. The weight for a gene with their center at m base pairs from the current gene's center is 1-(m/window_size). This method can give the "cleanest" limits for CNVs, when there is a large gap around it among the expressed genes.

