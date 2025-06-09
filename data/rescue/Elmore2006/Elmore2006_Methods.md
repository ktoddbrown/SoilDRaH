# Materials and Methods

## Site description

The study took place on a portion of Pu'u Wa'awa'a Ranch on the north side of Hualalai volcano, on the Island of Hawai'i (Fig. 1).
Soils across the ranch are derived from 200 to 3500-year-old basaltic lava flows, yet we focused our work on a single age class 2500–3500 [@Wolfe1996].
Precipitation ranged from 625 mm at the lower, dryer, northern edge to 900 mm at the southern higher-elevation edge of the study site.
Elevation ranged from 850 to 1100 m.
Native plant communities are characteristic of dry montane forests throughout Hawai'i and are dominated by the tree *Metrosideros polymorpha* (ohia).
Other scattered endemic tree species are *Acacia koa* (koa), and *Sophora chrysophylla* (mamane), and *Erythrina sandwicensis* (wiliwili tree) [@MacCaughey1917, @Eggler1971].

Two C4 pasture grasses – *Pennisetum clandestinum* (kikuyu grass) and *P. setaceum* (fountain grass) – were introduced in approximately 1900, and have spread throughout the ranch in the past hundred years [@Blackmore2000, @StateofHawaii2003].
Dry montane woodlands are now restricted to several small patches around the ranch and to a wildlife preserve above ∼1200 m elevation.
No stands of *M. polymorpha* or other tree species remain in the grazed paddocks used in this study, but some are scattered elsewhere within the ranch.
Aerial photos from as recently as 1950 show forest stands existing in many grazed areas [@Blackmore2000], but our study area was already deforested in the 1950 air photos.
The distribution of *P. clandestinum* and *P. setaceum* plant communities is largely controlled by elevation, with *P. clandestinum* restricted to higher, wetter sites.
*P. setaceum* is dominant below 900 m elevation, but exists at lower cover all the way up to 1200 m.
The impact of grazing on the relative dominance of these two grasses has been previously observed.
At similar elevations, more heavily grazed areas exhibit higher *P. clandestinum* cover [@Blackmore2000].

Cattle have grazed the current production area of Pu'u Wa'awa'a Ranch continuously since ca. 1900.
Management has varied throughout this period, but important features of the management style have remained constant.
For example, a watering and supplemental feeding station centrally located on the ranch has been in use since the turn of the century (Fig. 1).
Between 1968 and 1980, this location became the central hub of a ‘Savory method’ intensive grazing system [@Savory1992], which consisted of several wedge-shaped paddocks, with the water and supplemental feeding operation located at the center.
Cattle were rotated between paddocks as grass conditions allowed.
In the last century, and in particular from 1968 to 1980, cattle grazed most of the ranch, but the region around the central water and supplemental feeding station underwent the highest intensity of grazing.
Stocking rates from 1968–1980 were ∼450 cattle in the 1106 ha area surrounding the central water and feed station, but decreased to 300–350 cattle after 1980 to adjust for the effects of drought.
Annual stocking rates throughout this period were, thus, 0.32–0.41 head ha$^{−1}$ (Micky Kato, ranch manager, personal communication).

## Remote Sensing

The fractional surface cover of photosynthetic vegetation (PV), nonphotosynthetic vegetation (NPV), and exposed substrate was estimated from high-resolution hyperspectral remote sensing data acquired by the Advanced Visible Infrared Imaging Spectrometer (AVIRIS) on October 16, 2001.
These data were acquired and processed in support of multiple projects across Hawai'i.
Data processing and characterization were detailed by [@Asner2005].
The image was corrected to surface reflectance using the Atmospheric Correction Now (ACORN) model (ImSpec LLC, Palmdale, CA), which simulates and removes the effects of atmospheric water vapor and aerosols.
Geo-referencing was completed to an accuracy of one pixel [@Asner2005] using a combination of topographic maps and field-acquired geographic positions.
The spatial resolution of each AVIRIS pixel was ∼10 m × 10 m.

Remote sensing analysis followed the methodology of [@Asner2005] and [@Asner2000].
Fractional cover values were estimated using spectral mixture analysis that assumes each pixel reflectance is the linear combination of endmember spectra [@Adams1986, @Smith1990, @Elmore2000].
Endmembers are pure materials, the lateral distribution of which provides a physically meaningful way to describe vegetation structure.
Chosen endmembers were PV (green leaf material), NPV (dry leaf and woody stem material), and substrate (soil and rock) surfaces.
Bundles of reference spectra of these materials were assembled from a database of spectra collected in previous work [@Asner1998, @Asner2000, @Asner2002].
Individual spectra from each endmember bundle were iteratively selected by the model and applied to the following equation:

$\rho(\lambda)_{pixel} = f_{PV}\rho(\lambda)PV + f_{NPV}\rho(\lambda)_{NPV} + f_{substrate}(\lambda)_{soil} + \epsilon$,

where $\rho(\lambda)_{PV}$ , $\rho(\lambda)_{NPV}$, $\rho(\lambda)_{substrate}$ are the endmember spectra of PV, NPV, and substrate surfaces, respectively.
The terms, $f_{PV}$, $f_{NPV}$, and $f_{substrate}$ are the fractional cover of endmember types and $\epsilon$ is residual error.
Using a Monte Carlo approach, the model iteratively selects endmembers from the spectral bundles and applies them to Eqn (1).
After 50 iterations of the model, an average fractional cover of the three endmembers was calculated.
The model, AutoMCU [@Asner2002], has been repeatedly used to make measurements of vegetation structure in a variety of environmental settings.
In a comparison with field measures of vegetation structure, [@Asner2005] validated the application of AutoMCU for use in Hawaiian environmental conditions at 13 diverse field sites representing both forest and grassland systems.
Linear regression results between AutoMCU results and 8–12, 50 m field transects located at each of the 13 sites were excellent for PV ($r^2 = 0.93$, $P<0.05$), NPV ($r^2=0.89$, $P<0.05$), and substrate ($r^2=0.92$, $P<0.01$).
For a full description of the data acquisition conditions, processing steps, and uncertainty analysis, see [@Asner2005].

We utilized four 1000 m transects to understand the impact of grazing on above and belowground C relative to the effects of elevation.
Two of these transects utilized a gradient in grazing intensity that developed in the vicinity of the central water and feed station and were positioned along isohyets and elevation contours to control for precipitation and elevation, respectively (Fig. 1).
These two profiles were roughly parallel since precipitation and elevation are highly correlated on the ranch.
Two more 1000 m transects were oriented to measure the effect of elevation and together extended from an elevation of 850–1050 m, passing through the central water and feed station located at 943 m.
The position of each transect was also constrained to fall within the single soil age class (2500–3500 years old; [@Wolfe1996]).
The fractional cover values of PV, NPV and substrate derived from the AVIRIS imagery were extracted along all field sampling transects (Fig. 1).
To minimize high-resolution variability and uncertainty in co-location of transect data and imagery, the pixels from a 30 m wide area around each field transect were averaged at 10 m intervals.

## Vegetation

Plant community composition was measured on the two transects positioned along isohyets and elevation contours using the point line transect method [@Canfield1941].
Sampling was spaced at 25 m intervals close to the grazing center, and spaced to 50 and 100 m intervals further from the center, for a total of 21 sampling locations on each transect.
Plant cover was dominated by the two *Pennisetum* C4 grass species and by the C3 annual *Senecio madagascariensis* (fireweed).
Measurement and analysis thus concentrated on the cover of these three species.

## Soils

Along each of the four transects, soil cores were collected at 25 m intervals close to the grazing center, and spaced to 50 and 100 m further from the center.
A total of 21 samples were collected on each of the transects oriented along the grazing gradient.
A total of 36 samples were collected along the two elevation gradients.
Soil samples were also acquired from three forested sites at elevations of 900 m (two sites, three cores each) and 1150 m (one site, three cores) (Fig. 1).
Each forested site was dominated by *M. polymorpha*.
At each pasture sampling location, three 10 cm deep cores were taken at 1 m intervals along a line perpendicular to the transect.
At forest sampling locations, cores were oriented N–S and spaced at 1 m intervals.
The three soil cores from each site were homogenized, and the total mass was recorded.
Samples were then dried at 50–60 °C for ∼72 h before recording a dry soil mass.
Dry samples were sieved with a 2 mm mesh, and the mass and volume (displacement method) of the large fraction (>2 mm) was recorded.
The small fraction was ground to a fine powder.
The bulk density of each sample was calculated by dividing the sample mass <2 mm by the core volume, adjusted for the volume of rocks and large roots collected.
Soil organic C concentration was measured in all samples using a combustion elemental analyzer (Costech CHN, Valencia, CA, USA).
C isotopes were measured in all pasture samples using a Finnegan mass spectrometer at Stanford University.

To account for variation in soil compaction due to cattle [@Daniel2002], we calculated SOC stocks by numerically adjusting core depth based on bulk density such that the mass of each core was identical to the mass of a 10 cm core at the least compacted site [@Veldkamp1994].
This technique resulted in calculating SOC stocks to a depth slightly less than 10 cm at sites closer to the grazing center due to the greater bulk density of these soils.
This technique, which is becoming standard practice [@Murty2002], is preferable to a similar technique that simply calculates the SOC stock as the product of the SOC concentration and the bulk density [@Veldkamp1994, @Neill1997].
The SOC concentration (kg C kg$^{−1}$ soil) in each soil core was thus converted to total soil organic C stock, SOC (kg C m$^{−2}$), as follows:

${\text{SOC}} = \text{CL}\rho_b \times 10^3$,

where C is the SOC concentration, $\rho_b$ (g cm$^-3$) is the soil bulk density, and L(m) is the soil depth, adjusted for compaction as described above.

The SOC of each core is the sum of residual SOC derived from forest sources (SOC$_f$) and SOC accumulated since forest conversion to pasture (SOC$_p$).
C from these sources differs in its $\delta^{13}C$ ratio, and therefore, the contribution of the two sources to total SOC was calculated using a pool mixing model [@Vitorello1989]:

$\text{SOC}\delta^{13}C_s=(\text{SOC}_f\delta^{13}C_f) + (\text{SOC}_p\delta^{13}C_p)$,

where $\delta^{13}C_s$ is the C isotope ratio measured at each site, $\delta^{13}C_f$ from forest soils and $\delta^{13}C_p$ from pasture sources.
We collected four samples (two low and two high grazing intensities) of fresh vegetation from the pastures and measured the average $\delta^{13}C_p$ as −13.0 ‰.
A mean forest isotope value of −28 ‰ was taken from measurements of Hawai'i *M. polymorpha* made by [@Townsend1995].
[@Townsend1995] also measured $\delta^{13}C$ in pasture grasses in Hawai'i and arrived at a value of −13.0 ‰, thus confirming our measurements.

For the two replicate transects oriented parallel to elevation contours and isohyets, respectively (Fig. 1; black lines), corresponding measurements at 25, 50, 75, and 100 m, etc. from the grazing center were paired, and the mean value was further analyzed.
Additionally, results from the transects oriented uphill and the other downhill from the grazing center (Fig. 1; gray lines) were analyzed as a single transect extending from an elevation of 875–1075 m, with the grazing center positioned at 940 m.
Therefore, analysis focused on two trends: those correlated with grazing intensity, holding elevation constant; and trends correlated with elevation.
