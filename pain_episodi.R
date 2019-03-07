
#SWAS data

# The following documentation is from the original excel file:
# SWAS-VTSSS PRIMARY SAMPLE SITES DATA SET:  1979-2011
# 
# FILE:  SWAS_VTSSS_PRIMARY_20130319.xlsx
# 
# DERIVATION:    SWAS_VTSSS_PRIMARY_20130319.accdb  (Table:  SAMPLE_MEASUREMENTS_20130319)
# 
# RULES FOR DATA SET CREATION: 
#   includes records for samples with complete analysis, defined as including pH, ANC, SO4, NO3, Cl, Ca, Mg, K, Na
# includes records for SWAS-VTSSS synoptic (survey) sites, quarterly sample sites, weekly sample sites, and episodic (high discharge) sample sites
# 
# NOTES:
#   analysis data are provided for SWAS-VTSSS primary sample sites (N = 461)
# the Sample field is unique for each record
# Synoptic, Quarterly, Weekly, and Episodic fields indicate association of sample records with specific data sets 
# additional information about the specific data sets is provided in separate files for the synoptic, quarterly, weekly, and episodic data sets
# sample records provided here can be associated with multiple data sets
# sample records that are not associated with a specific data set  (synoptic, quarterly, weekly, or episodic) are not provided
# values below method detection limits have not been censored (applies to some NO3, NH4, DOC, TMAL, and OMAL values)
# method detection limits are reported in SWAS_VTSSS_PRIMARY_20130319.accdb  (Table:  SAMPLE_MEASUREMENTS_20130319)
# this file includes corrected data for the same samples as SWAS_VTSSS_ALL_PRIMARY_20120925.xlsx
# 
# 
# 
# SITE DOCUMENTATION FILE:  SWAS_VTSSS_PRIMARY_SITE_DOCUMENTATION_20121017.xlsx
# 

# MEASUREMENTS AND  UNITS:  	

# ANC (acid neutralizing capacity)				畫q/L
# Cond (specific conductance)				猶/cm
# Temp (water temperature) 				蚓
# SO4 (sulfate)				畫q/L
# NO3 (nitrate)				畫q/L
# Cl (chloride)				畫q/L
# Ca (calcium ion)				畫q/L
# Mg (magnesium ion)				畫q/L
# K (potassium ion)				畫q/L
# Na (sodium ion)				畫q/L
# NH4 (ammonium)				畫q/L
# SIO2 (silica)				痠/L
# DOC (dissolved organic carbon)				mg/L
# TMAL (total monomeric aluminum)				痢/L
# OMAL (organic monomeric aluminum)				痢/L

V

#
str(pain.storm)

pain.storm$datetime2 <- round(as.POSIXlt(pain.storm$datetime, format="%Y-%m-%d %H:%M:%S"))
pain.storm[1:4,]

# p.pH_tmal <- ggplot(pain.storm, aes(x=pH, y=TMAL ))+
#   geom_point(size=5, shape=1)+
#   theme_classic()
# 
# p.pH_tmal
# 
# p.pH_omal <- ggplot(pain.storm, aes(x=pH, y=OMAL ))+
#   geom_point(size=5, shape=1)+
#   theme_classic()
# 
# p.pH_omal
# 
# p.ANC_tmal <- ggplot(pain.storm, aes(x=ANC, y=TMAL ))+
#   geom_point(size=5, shape=1)+
#   theme_classic()
# 
# p.ANC_tmal


############

# Brining in discharge

discharge <- read.csv(file="C:/Users/Jeff/Documents/SWAS/DISCHARGE/SWAS_Discharge_Hourly_20110914.csv", head=TRUE, fill=TRUE)
discharge$datetime <-as.POSIXct(strptime(discharge$datetime,"%m/%d/%Y %H:%M" ))

str(discharge)

pain.discharge <-  subset(discharge, discharge$StationID  == "PAIN")


pain.complete <- merge(pain.discharge, pain.storm, by="datetime")
V
