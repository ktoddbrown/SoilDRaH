# Methods

## Study site
This experiment was part of a manipulative study of the impact of summer drought on the growth and physiology of *Larrea* and *Prosopis*, and the associated dynamics of soil resources (nutrients, water).
See @Reynolds1999 for a detailed description of that study and of the study site in south-central New Mexico, *ca.* 30km N of Las Cruces in the northern Chihuahuan Desert.
This area is part of the Jornada LTER located at the southern end of the Jornada del Muerto Basin of south-central New Mexico, U.S.A. (Doña Ana County).
The decomposition of *Larrea* litter was studied on a gently sloping, eroded terrace composed of shallow, coarse-textured soils (*ca.* 80% sand, 10% silt, 10% clay) and dominated by a sparse cover of *Larrea* (<30%) with scattered subshrubs and forbs (hereafter referred to as the *Larrea* site).
The decomposition of *Prosopis* litter was studied in a dune area located about 20 km NW of the *Larrea* site in a broad, flat subsidiary basin characterized by aeolian soils (*ca.* 86% sand, 6% silt, 8% clay) and dominated by a sparse cover of *Prosopis* (<20%), with a few other scattered forbs and grasses.

## Climate and drought treatment
Details regarding the climate of the Jornada Basin are provided in @Conley1992.
The mean annual temperature is 16°C.
The mean monthly maximum temperature is highest in June (36°C) and lowest in January (13°C).
Night-time freezing temperatures occur on average from late October through early April (165 days).
@Conley1992 characterized the semi-arid climate of the Jornada as having three distinct seasons: hot, dry springs (April–June); hot, moist summers (July–October); and cold, moderately dry winters (November–March).
Total annual precipitation is about 230 mm, of which nearly 65% falls during the summer period as localized showers associated with thunderstorms; about 25% falls in winter as rain and snow associated with frontal storms over the region; and about 10% occurs in the spring.

Given that @Reynolds1999 found that winter soil moisture recharge could begin as early as October, and because rainfall-initiated summer growth can occasionally begin mid-to late June, we designate the period from June to September as *summer* for the purposes of applying the drought treatment.
The rest of the year (October 1–May 30) is referred to as *winter/spring*.
Summer drought treatment (exclusion of June–September rainfall) was accomplished using small rain-excluding shelters designed to cover a specific shrub and an area about 5 × the size of its canopy.
The rain shelters were constructed of metal frames, which were open on the sides and covered with thin polyethylene film roofs (6 mil, 0.15 mm thick).
The polyethylene roof was only on the shelter during the summer drought treatment period, during which time there were also microclimate effects associated with polyethylene film (and to a lesser extent, the shelter framework).
Trasmissivity of the polyethylene averaged 85% for visible radiation (400–700 nm) and 82% for ultraviolet (340–400 nm).
Total solar radiation was reduced by 15–35% depending upon cloud cover and time of year.
Midday air temperatures under shelters differed only slightly from ambient ($\pm$1°C), but temperatures under the shrub canopies at the soil surface and at 10 cm depth differed by as much as 6°C and 2°C, respectively (descriptions and photos of shelters provided in @Reynolds1999).

## Litterbag sampling
In December of 1991 we collected leaves and excavated roots (1–8 mm dia.) from the two species at the two sites.
This material was air-dried and 5g was apportioned into a 12 × 12 cm$^2$ fiberglass screen (1.5 mm mesh) litterbag.
A total of 400 litterbags, 100 of each litter type of each species, were initially numbered and weighed and separated into two groups: those to be placed under shrubs that received natural rainfall (control) and those that were under shrubs subjected to summer rainfall exclusion in each year (treatment).
Litterbags containing leaf litter were placed on the soil surface under the respective shrub canopy, and litterbags containing root litter were buried 10 cm beneath the surface of the shrubs.
Six to eight litterbags were randomly collected once or twice a year during the 41-month duration of the study (January 1992 – June 1995).
At each sampling, litterbags were oven-dried to constant weight at 60°C and weighed to determine the litter mass remaining, and samples were digested to determine total Kjeldahl N, total C, and ash.
Except for the above determinations, we did not measure chemical composition of the litter.
However, for some of the data analyses and for the purposes of modeling litter decomposition, it was necessary to be able to partition the litter into several chemical fractions - e.g. soluble (metabolic), recalcitrant (structural), and lignin fractions.
These values were estimated from other studies and are reported in Table 1 along with initial litter chemistry measured in this study.

## Data analyses
Data for litter mass and N content were each subjected to a multiple analysis of variance (AOV) to identify effects of four different factors - species differences, litter differences (leaf vs. root), drought treatment, and time (an independent factor since at any one sampling time a completely independent sample was collected) - on the amount of litter mass remaining and its N content.
Because the principal focus of this study was on the effect of drought on decomposition, we also isolated that treatment effect by carrying out single-factor AOVs to determine specific effects of drought on the decomposition of individual litter types of each of the species.
In order to compare decomposition rates through time between species, litter types, and especially between control and drought treatment, we also fit data for litter mass through time to a double exponential model, chosen to estimate the decay constants for the labile (metabolic) and structural fractions of litter (@CepedaPizarro1990; @Urquiaga1998).
Instead of allowing four fitting parameters, however, we assumed a fixed metabolic fraction for the litter of the two species (25% and 24% for *Larrea* and *Prosopis*, respectively).
This was done to specifically partition the variation in litter mass decay between the decay rate constants associated with metabolic and structural fractions, and not into differences in metabolic fractions, which, in fact, were identical among the material split among control and drought treatments, and very similar among all the litter used in this experiment.
Also, we omitted the last data set sampled from this analysis since some of those samples had a gain rather than loss of mass.
All statistical analyses were performed using Data Desk (@Velleman1995).

## Model simulations
To predict C and N dynamics of litter decomposition, we used the CENTURY model (@Parton1987) because of its relative simplicity.
CENTURY is similar to a number of other models that have been developed to predict decay of various C and N pools representing different chemical fractions of litter and soil organic matter (SOM) (see @Parnas1976; @Anderson1979; @vanVeen1981; @Pastor1986; @Parton1987; @Moorhead1991).
Only the decomposition component of the CENTURY model was used for this study.
CENTURY simulates decay from two constituent fractions of litter (metabolic and structural) and three fractions of soil organic matter (active, slow, and passive).
Flows of C from the various fraction pools are governed by intrinsic decomposition rates ($k$), microbial efficiencies, and soil texture (for partitioning flow of carbon from active SOM into slow or passive SOM).
All rates of C flow are affected in the same way by functions of monthly soil temperature and moisture (ratio of monthly rainfall to potential evapo-transpiration).
Nitrogen dynamics are closely coupled to C flows, with movement of N determined as the product of C flow times the N/C ratio of recipient pool.
The N/C ratio of initial litter is fixed at 1/150 for the structural pool, but variable for the metabolic pool (depending on the N content of incoming litter).
Nitrogen mineralization occurs at the rate of C mineralization times the N/C ratio of specific pool, whereas N immobilization (from soil inorganic N) into an organic matter pool occurs if additional N is required to maintain the specified N/C ratio of pool.

Initial trials to use CENTURY with parameter values as presented in @Parton1987 resulted in overestimates of mass loss and poor predictions of the impacts of drought.
Thus, we incorporated several changes into CENTURY to make it more applicable to arid ecosystems in general.
The overestimation of litter decay rates appeared to be partly related to the partitioning of litter into metabolic and structural fractions (Eqn (2) in @Parton1987), which yielded a metabolic fraction that was too great for both species - a problem that has been encountered with other applications of CENTURY (@Moorhead1999).
Thus, we partitioned litter into the two fractions based on literature values for *Larrea* and *Prosopis* (see Table 1).
We changed the time step of the model from monthly to daily to accommodate the high within-month variation in moisture of the soil and litter pools.
Others (@Parton1996) have incorporated this change into CENTURY in order to predict ecosystem processes that operate at scales of less than a month.
Changing to a daily time step necessitated changing the monthly averaged temperature and moisture scalars for decay.

To account for the effect of temperature on decomposition, we used the same functional relationships employed by @Parton1987, but used daily average soil temperatures at 1 cm (for leaf litter) and 10 cm (for root litter).
Different methods were used for scaling the effect of moisture on leaf litter decomposition and root litter decomposition.
For leaf litter decomposition, we used the functional relationship in @Parton1987, in which the decomposition rate is scaled by a monthly moisture index, $M_d$, calculated as the ratio of precipitation (PPT) to potential evapo-transpiration (PET).
However, we calculated a daily value for PPT/PET based on a 30-day running sum of PPT and a 30-day running sum of PET.
PET was calculated using Eqn (12) of @Kemp1997.
For root litter decomposition, however, this simple moisture scalar resulted in overestimates of mass loss for both species and did not reproduce the effect of drought on root litter decay.
We thus presumed that root litter decay was more strongly related to actual soil moisture content.
Because we only had soil water measurements at 30 and 60 cm, we estimated soil water content at 10 cm using a soil water model (SWB) developed for the Jornada Basin (@Kemp1997).
The model was calibrated and tested for both the *Larrea* and *Prosopis* sites using water content data collected at 30 and 60cm depths.
We then calculated a moisture scalar ($M_{d–root}$), varying from 0 to 1, which was a function of soil water content:
$$ M_{d–root} = \left( \text{SWC}_{10} - \text{SWC}_{min} \right) /  \left( \text{SWC}_{max} - \text{SWC}_{min} \right) $$
where SWC$_{10}$ is the volumetric soil water content (in this case, the 5–15 cm soil layer), SWC$_{max}$ equals maximum volumetric water content of the soil (approximately field capacity), and SWC$_{min}$ equals a minimum volumetric water content where decomposition rate is assumed to be zero.
We determined that the best fit to data for non-droughted *Larrea* root decomposition was with $\text{SWC}_{max}$ equal to the value at -0.02 MPa and $\text{SWC}_{min}$ equal to the value at -1.5 MPa.
We then used this single set of parameters for all simulations of root decay for control and treatment for both species.

With all of these changes in place, estimated rates of root litter decay were still somewhat too great, so, as a last resort, we decreased the intrinsic decay constants for the root litter pools by 20%, making them equivalent to those for surface litter pools.
Our justifications for this are that (i) arid ecosystems are dominated by fungi that probably have lower maximum decay rates than the bacteria-dominated agricultural soil upon which the decay constants of CENTURY were based (@Whitford1989); and (ii) our data provided no support for the assumption of @Parton1987 that decay rates of surface litter are fundamentally lower than those of below-ground litter - in fact, our data suggest the opposite.
Evaluation and comparison of model predictions were carried out using linear regression between observed mean values and predicted values corresponding to the times at which observed values were obtained (Table 2).
