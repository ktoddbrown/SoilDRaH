# 2. Material and methods

## 2.1. Experimental area and layout

The experimental site is located in Montes de Toledo, central Spain, (39°31'N, 4°16'40''W).
The climate is classified as Mediterranean with annual precipitation of 469 mm and average temperature of 12.9°C.
The original dominant tree cover is Pyrenean oak (*Quercus pyrenaica* Willd).
Despite its scientific name, this species is seldom found in the Pyrenees and is native of sub-Mediterranean areas of central and southern Spain and Portugal as well as southern France.
Hereafter we will refer to *Q. pyrenaica* as oak.
A plantation in rows of Mediterranean Maritime pine (*Pinus pinaster* Ait. ssp. *mesogeensis*) was incorporated into the stand in 1950 after the removal of standing oaks.
Between the plantation rows, oak sprouts have grown and the stand is currently a mixed stand in rows with a dominant canopy of Mediterranean Maritime pine and an understory of oak.
However, in some places the oak has reached the same height as the pine.
The site has a north-west facing aspect and the slope is 21.5%.
This type of mixtures covers 105.325 ha in Spain [@MAPAMA2006].

In 2009, a long term thinning experiment was established by the Forest Research Centre-INIA to analyze the adaptive management role of thinning in the mitigation capacity of the stand to cope with climate change.
The experimental layout consisted of two thinning treatments from below: treatment LT, light thinning or removal of 25% of basal area found in unthinned plots; treatment HT, a heavy thinning or removal of 40% of basal area found in unthinned plots and a control (treatment CT or unthinned).
The three levels were replicated three times yielding 9 plots with an average size of 0.06 ha plot<sup>-1</sup>.
In order to avoid slope effects the shape of the plots is rectangular with the longest side (30 m) placed orthogonally to the slope.
Treatments were arranged in a latin-squared design.
Thinning consisted of removing pines from the lower diameter classes until the desired intensity was reached with the objective of analyzing the growth response of the remaining oaks and pines to changes in competition intensity.

In parallel to the thinning experiment, a nutrient cycling study was established in the same plots after the first thinning in 2009.
Four littertraps were installed per plot.
In February 2011, a decomposition bag experiment was incubated next to each littertrap (4 litter bag experiments on each plot) 8 bags were placed on each point containing 5.11±0.01 g of air dried foliar material from oaks and pines shed in the previous autumn following a similar proportion to that found in the litterfall traps (3.40±1.16 g for needles and 1.68±1.19 g for leaves).
The total number of incubated bags were 8 bags/point × 4 points/plot × 9 plots = 288.
Every three months, a bag per sample point was harvested (4 bags per plot) and transported into a raffia fiber bag to the lab the same day to avoid mass losses.
The first collection date was May 2011 and the last collection date was February 2013.

## 2.2. Mass loss and nutrient calculations

The air dried weight of needles and leaves was measured separately at every harvest and half of the sample was dried at 65°C to analyze nutrient content in the foliar material.
The other half was dried at 105°C to constant weight.
Total mass loss was calculated for the whole sample.
The elements analyzed were total carbon (C) in percentage, total nitrogen (N), phosphorus (P), calcium (Ca), magnesium (Mg) and potassium (K).

Total C and N were determined in a HCN-600 LECO.
Foliar samples were digested in a high pressure aqua regia system (ETHOS PLIS) and nutrient concentrations (mg g<sup>-1</sup> of dry matter) were measured using ICP-OES (Perkin-Elmer, Optima 2000).

## 2.3. Initial nutrient concentrations

The concentration of nutrient at the beginning of the experiment was calculated from a subsample of fresh litter collected in the litter traps in February 2011 (time 0 in our analyses).
Differences in the initial concentrations of C (%), N, Ca, P, K and Mg (all in mg g<sup>-1</sup>), were tested using a two-way ANOVA with litter type (leaves and needles), treatment (CT, LT and HT) and the interaction as main factors.
When assumptions of normality, homogeneity of variance, or linearity were violated we transformed the response variable according to an arcsine transformation (when the response is a proportion) or logarithm (when the response was non-linear).
A post hoc test for multiple comparisons was performed when any of the factors analyzed were significant with 95% probability.

## 2.4. Decomposition rates and decay models

Early stage decomposition rates (2 years after incubation) were calculated using a single exponential decay model [@Olson1963]:

$\ln (\frac{W_t}{W_0}) = -k \cdot t$ (1)

where $W_0$ is the original litter mass (g) (needles or leaves), $W_t$ is the remaining mass (g) after time $t$, which is the number of days since incubation, and $k$ is the decomposition rate (year<sup>-1</sup>).

The decomposition rate was repeatedly measured at four points within each plot.
This experimental layout confers a hierarchy and repeated-measures structure to the data, which must be taken into account in the analysis to avoid dependencies between observations.
We performed a linear mixed model analysis with three fixed effects (litter type, thinning intensity and their interaction) as well as one random effect (plot) and the hierarchy in the repeated measures structure (sample points within plots).
The final model to fit was:

$y_{ijsk} = (\alpha_i + \beta_j + \gamma_{ij} + \upsilon_s)t_k + \varepsilon_{ijk}$ (2)

where $y_{ijsk}$ is the fraction of litter type $i$ that has been decomposed at time $k$ in plot $s$ of treatment $j$, $\alpha_i$ is the fixed litter type effect $(\alpha_i = \alpha_1LT_1 + \alpha_2LT_2)$, $\beta_j$ is the fixed treatment effect $(\beta_j = \beta_1CT + \beta_2LT + \beta_3HT)$, $\gamma_{ij}$ is the litter type-treatment interaction effect $(\gamma_{ij} = \gamma_{11}LT_1CT + \gamma_{12}LT_1LT + \gamma_{13}LT_1HT + \gamma_{21}LT_2CT + \gamma_{22}LT_2LT + \gamma_{23}LT_2HT)$, $\upsilon_s$ is the random plot effect and $\varepsilon_{ijk}$ is the error term.
$LT_1$ is oak foliar litter and $LT_2$ is pine foliar litter.
CT, LT, and HT are control, light thinning and heavy thinning respectively.
After fitting, a hypothesis about similarity between litter type decomposition rates $(\alpha_1 = \alpha_2)$ and between and within treatment and litter types ($\gamma_{i,j} = \gamma_{i,j+n}$ and $\gamma_{ij=} \gamma_{i+n,j}$) were contrasted using a *F*-test.

## 2.5. C and nutrient concentration dynamics

We tested three kinds of concentration responses to decomposition: no response (the concentration is stable throughout the decomposition period) linear (increasing or decreasing) and quadratic response (concave or convex response).
We fitted a quadratic model (Eq. (2)) to test these three types of response of C and nutrient concentration ([@Aponte2012], [@Laskowski1995]) 

$y_{ijk} = \alpha + \alpha_i + \beta_j + \gamma_{ij} + u_s + (\alpha'_i + \beta'_j + \gamma'_{ij})t_k + (\alpha''_i + \beta''_j + \gamma''_{ij})t^2_k + \varepsilon_{ijk}$ (3)

where $\alpha$ is the intercept or initial concentration at time 0, $\alpha_i$ is the fixed litter type effect ($\alpha_i = \alpha_1LT_1 + \alpha_2LT_2$), $\beta_j$ is the fixed treatment effect ($\beta_j = \beta_1CT + \beta_2LT + \beta_3HT$), $\gamma_{ij}$ is the litter type-treatment interaction effect ($\gamma_{ij} = \gamma_{11}LT_1CT + \gamma_{12}LT_1LT + \gamma_{13}LT_1HT + \gamma_{21}LT_2CT + \gamma_{22}LT_2LT + \gamma_{23}LT_2HT$), $u_s$ is the random plot effect, $t_k$ is the time of harvest.

## 2.6. C and nutrient immobilization-release dynamics

The change in C and nutrient stock were evaluated as the percentage value of the remaining amount of each element compared to the initial content.
The dynamics of C and macro nutrients were studied as a function of time, treatment and litter type using the same model structure as in Eq. (3).

Statistical analyses were performed with the PROC MIXED procedure of SAS/STAT<sup>$\circledR$</sup> software v 9.3 for Windows<sup>$\circledR$</sup> (SAS Institute, Cary NC).
Plots were depicted using R-software [@R2016].
