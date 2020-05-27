

- [i3 HMM](infercnv-i3-HMM-type): a three-state CNV model representing deletion, neutral, and amplification states.  This is very similar to the three-state model used by [HoneyBADGER](https://github.com/JEFworks/HoneyBADGER). This method leverages characteristics of the tumor cells residual expression intensities under the assumption that most signal corresponds to 'normal' and outlier intensities correspond to CNV.

- [i6 HMM](infercnv-i6-HMM-type): a six-state CNV model that predicts the following CNV levels:
    - 0x: complete loss
    - 0.5x: loss of one copy
    - 1x: neutral
    - 1.5x: addition of one copy
    - 2x: addition of two copies
    - 3x: essentially a placeholder for >2x copies but modeled as 3x.

The i6 HMM uses characteristics of the normal (reference) cells to generate an 'in silico spike-in' of single cell expression data containing varying levels of CNV that match the above six states.  This 'in silico spike-in' data progresses through the inferCNV processing steps along with the input data, and the resulting 'in silico spike-in' residual expression intensities are used to calibrate the HMM emission states for each of the corresponding CNV levels.  

Additionally, predicted CNV regions are further analyzed using a Bayesian Network (similar to [HoneyBADGER](https://github.com/JEFworks/HoneyBADGER) but with modifications) to compute the posterior probability of that CNV region belonging to a given state for each cell.  CNV regions with mean posterior probabilities of being normal (no CNV) that are above a maximum threshold are removed as likely false positive predictions.

Click the [i3 HMM](infercnv-i3-HMM-type) or [i6 HMM](infercnv-i6-HMM-type) links for more specific details for each method.

## CNV region prediction reports

CNV region reports are provided in the following files:

-  'HMM_CNV_predictions.*.pred_cnv_regions.dat' : summary containing the CNV region coordinates, state assignments, and cell groupings.  

The formatting is like so:

```
cell_group_name                         cnv_name        state   chr     start   end
malignant_MGH36.malignant_MGH36_s1      chr1-region_2   0.5     chr1    3696784 144612683
malignant_MGH36.malignant_MGH36_s1      chr1-region_4   1.5     chr1    151336778       156213123
malignant_MGH36.malignant_MGH36_s1      chr3-region_7   1.5     chr3    3168600 10285427
malignant_MGH36.malignant_MGH36_s1      chr3-region_9   1.5     chr3    45429998        49460186
malignant_MGH36.malignant_MGH36_s1      chr4-region_11  0.5     chr4    53179   187134610
malignant_MGH36.malignant_MGH36_s1      chr5-region_13  1.5     chr5    134181370       177037348
...
```

In the simple case where the HMM is run on all cells for a given patient sample, the cell group name will correspond to all cells for that sample.  However, if there was a search for tumor subclusters (tumor heterogeneity, enabled by Analysis Mode = Subclusters), then the sample may be divided into subclusters and you'll need to cross-reference the corresponding 'HMM_CNV_predictions.*.cell_groupings' file.

- 'HMM_CNV_predictions.*.cell_groupings' : provides a listing of tumor subclusters and cell membership.  Formatting is like so:

```
cell_group_name                         cell
malignant_MGH36.malignant_MGH36_s1      MGH36_P3_E06
malignant_MGH36.malignant_MGH36_s1      MGH36_P10_E07
malignant_MGH36.malignant_MGH36_s1      MGH36_P3_C04
malignant_MGH36.malignant_MGH36_s1      MGH36_P3_A09
malignant_MGH36.malignant_MGH36_s1      MGH36_P2_A08
malignant_MGH36.malignant_MGH36_s1      MGH36_P10_B08
malignant_MGH36.malignant_MGH36_s1      MGH36_P8_H09
...
```

- 'HMM_CNV_predictions.*.pred_cnv_genes.dat' : contains the per-cell, per-gene CNV state-level assignment.  The file formatting is like so:

```
cell_group_name                         gene_region_name state   gene    chr     start   end
malignant_MGH36.malignant_MGH36_s1      chr1-region_2   0.5     DFFB    chr1    3696784 3713068
malignant_MGH36.malignant_MGH36_s1      chr1-region_2   0.5     C1orf174        chr1    3773845 3801993
malignant_MGH36.malignant_MGH36_s1      chr1-region_2   0.5     RPL22   chr1    3805689 3816857
malignant_MGH36.malignant_MGH36_s1      chr1-region_2   0.5     ICMT    chr1    6241329 6269449
malignant_MGH36.malignant_MGH36_s1      chr1-region_2   0.5     ACOT7   chr1    6281253 6296032
malignant_MGH36.malignant_MGH36_s1      chr1-region_2   0.5     NOL9    chr1    6324329 6454451
malignant_MGH36.malignant_MGH36_s1      chr1-region_2   0.5     KLHL21  chr1    6581407 6614595
...
```

This makes it easy to find if there are certain genes of interest that are predicted to be amplified or deleted in any of the samples.  For example, this sample corresponds to oligodendroglioma (type of brain tumor), a tumor type where the EGFR gene is often found amplified and as a driver or contributor to tumorigenesis.  We can do a quick search for EGFR like so:

``` bash
grep EGFR HMM_CNV_predictions.HMMi6.hmm_mode-samples.Pnorm_0.5.pred_cnv_genes.dat
```
and we find:
```
malignant_MGH36.malignant_MGH36_s1	chr7-region_16	2	EGFR	chr7	54819943	54827667
malignant_93.malignant_93_s1	chr7-region_109	1.5	EGFR	chr7	54819943	54827667
malignant_97.malignant_97_s1	chr7-region_160	1.5	EGFR	chr7	54819943	54827667
```

so it appears that EGFR is predicted to be amplified as part of a larger CNV region containing one extra copy in samples: malignant_93.malignant_93 and malignant_97.malignant_97, and a CNV at 2x duplication in sample malignant_MGH36.malignant_MGH36.

>Note, one limitation of this approach is that the amplified or deleted gene of interest must be found within a larger region of CNV, and that the amplification or deletion level is assumed to be constant over that reported CNV region.  This is a useful operative assumption, but not an entirely valid one - as EGFR in this case may be more heavily amplified within that larger CNV region, more than other neighboring genes, and this feature would not be ascertained by this method.

- Finally, a file called 'HMM_CNV_predictions.*.genes_used.dat' is provided, which lists all genes that were under consideration in this analysis.  Not all genes in the genome will be included here, as this will only include genes that were found minimally expressed in cells and retained in the expression matrix during inferCNV processing.




