###Kongwa Mycotoxin Modelling for PlosOne submission
##compiled with R version 4.3.1 and the following dependencies:
require(DescTools)
require(sp)
require(ggplot2)
require(raster)
require(sf)
require(reshape2)
require(ggforce)
require(ggpubr)
require(elevatr)
require(lme4)
require(MuMIn)
require(merTools)
require(prospectr)
require(randomForest)
require(sjPlot)

##Set  working directory: 
#setwd()

##Save or load compiled code:
#save.image("kongwa_mycotoxin.RData")
#load("kongwa_mycotoxin.RData")

#####2019 survey responses for Table 1#####
kongwa_survey_responses_19<-read.csv("2019_kongwa_survey_responses.csv")

summary(kongwa_survey_responses_19)
length(na.exclude(kongwa_survey_responses_19$seed_type))
summary(na.exclude(as.factor(kongwa_survey_responses_19$seed_type)))#289/318 were save from previous year's harvest. Remaining (29/318) were purchased seeds
summary(na.exclude(as.factor(kongwa_survey_responses_19$intercropped)))/length(na.exclude(as.factor(kongwa_survey_responses_19$intercropped)))
summary(na.exclude(as.factor(kongwa_survey_responses_19$number_intercrop))[which(na.exclude(as.factor(kongwa_survey_responses_19$number_intercrop))!=0)])/length(na.exclude(as.factor(kongwa_survey_responses_19$number_intercrop))[which(na.exclude(as.factor(kongwa_survey_responses_19$number_intercrop))!=0)])

intercrop_crops<-as.factor(gsub("r ","r",unlist(strsplit(kongwa_survey_responses_19$intercrop_crop, ", "))))
summary(intercrop_crops)

intercrop_crops_table<-cbind.data.frame(table(intercrop_crops),
                                        c("Legume","Tuber","Legume","Legume","Fruit/Vegetable","Grain","Legume","Legume",
                                          "Fruit/Vegetable","Grain","Legume","Fruit/Vegetable","Grain","Oil Crop","Grain","Oil Crop",
                                          "Fruit/Vegetable"))
colnames(intercrop_crops_table)<-c("intercrop_crop","frequency","intercrop_type")
intercrop_crops_table$proportion<-intercrop_crops_table$frequency/257#257 is number of surveys respondents who did intercropping

ggplot(intercrop_crops_table,aes(reorder(intercrop_crop,-proportion),proportion*100,fill=intercrop_type))+
  geom_bar(stat = "identity")+theme_bw()+labs(x="Intercropped Crop",y="Percentage of Intercropped Fields")+
  guides(fill = guide_legend(title = "Crop Type"))

aggregate.data.frame(intercrop_crops_table$proportion,by=list(intercrop_crops_table$intercrop_type),FUN=sum)

summary(as.factor(na.exclude(kongwa_survey_responses_19$planting_month)))/length(na.exclude(kongwa_survey_responses_19$planting_month))
summary(as.factor(na.exclude(kongwa_survey_responses_19$harvest_month)))/length(na.exclude(kongwa_survey_responses_19$harvest_month))
summary(as.factor(na.exclude(kongwa_survey_responses_19$fertilizer)))/length(na.exclude(kongwa_survey_responses_19$fertilizer))
summary(as.factor(na.exclude(kongwa_survey_responses_19$water_stress)))/length(na.exclude(kongwa_survey_responses_19$water_stress))
summary(as.factor(na.exclude(kongwa_survey_responses_19$drought_period)))/length(na.exclude(kongwa_survey_responses_19$drought_period))
summary(as.factor(na.exclude(kongwa_survey_responses_19$pest_pressure)))/length(na.exclude(kongwa_survey_responses_19$pest_pressure))

length(as.factor(paste(kongwa_survey_responses_19$maize_maturity,",",kongwa_survey_responses_19$maize_dry,sep = "")))
table(as.factor(paste(kongwa_survey_responses_19$maize_maturity,",",kongwa_survey_responses_19$maize_dry,sep = "")))
table(as.factor(paste(kongwa_survey_responses_19$maize_maturity,",",kongwa_survey_responses_19$maize_dry,sep = "")))/329

summary(as.factor(na.exclude(kongwa_survey_responses_19$mature_rain)))/length(na.exclude(kongwa_survey_responses_19$mature_rain))
summary(as.factor(na.exclude(kongwa_survey_responses_19$maize_shelled)))/length(na.exclude(kongwa_survey_responses_19$maize_shelled))
summary(as.factor(na.exclude(kongwa_survey_responses_19$maize_storage_merged)))/length(na.exclude(kongwa_survey_responses_19$maize_storage_merged))
summary(as.factor(na.exclude(kongwa_survey_responses_19$sort_maize_merged)))/length(na.exclude(kongwa_survey_responses_19$sort_maize_merged))
summary(as.factor(ifelse(kongwa_survey_responses_19$maize_storage_merged=="none",kongwa_survey_responses_19$maize_storage_merged,"stored")))#were samples stored "stored", or take directly to mill post harvest "none"

#
#####2019 all survey/mycotoxins####
#load/curate 2019 data
survey_all_tz19<-read.csv("2019_kongwa_survey.csv")
survey_all_tz19$flour_type<-as.factor(survey_all_tz19$flour_type)
survey_all_tz19$grain_mill_id<-as.factor(survey_all_tz19$grain_mill_id)

#flour type
summary(as.factor(na.exclude(survey_all_tz19$flour_type)))/length(na.exclude(survey_all_tz19$flour_type))*100

#geom_mean of af and fum
Gmean(na.omit(survey_all_tz19$af_ppb),conf.level=0.95)
Gmean(na.omit(survey_all_tz19$fum_ppm),conf.level=0.95)

#percentage exceeding legal limits according to EAC standards
sum(na.exclude(survey_all_tz19$af_ppb)>10)/length(na.exclude(survey_all_tz19$af_ppb))*100
sum(na.exclude(survey_all_tz19$fum_ppm)>2)/length(na.exclude(survey_all_tz19$fum_ppm))*100

##convert walking time into distance for env factor buffer 
walk_dist_m<-survey_all_tz19$walking_distance_min*(3000/60)#if walk at 3km/hour
walk_dist_m[which(walk_dist_m>10000)]<-10000 #change any value >10km to 10km
walk_dist_m[is.na(walk_dist_m)]<-mean(na.omit(walk_dist_m))#id the nas and assign them the mean
walk_dist_m<-ifelse(walk_dist_m<500,500,walk_dist_m)#make 500m the minimum buffer
survey_all_tz19$walk_dist_m<-walk_dist_m
summary(survey_all_tz19$walk_dist_m)

summary(survey_all_tz19$walking_distance_min)
summary(cut(na.exclude(survey_all_tz19$walking_distance_min),c(0,60,120,180,10000),include.lowest=TRUE))/length(na.exclude(survey_all_tz19$walking_distance_min))

#spatial data frame using WGS84 coordinate reference system#spatial data frame using WGS84truehist() coordinate reference system
survey_spatial_tz19<-survey_all_tz19; coordinates(survey_spatial_tz19)<-c("longitude","latitude"); proj4string(survey_spatial_tz19)<-CRS("+init=epsg:4326")

#Kongwa District shape file in WGS84
kongwa_wards_shp<-st_transform(subset(read_sf("./kongwa_shapefile/TZwards.shp"),District_N=="Kongwa"),CRS("+init=epsg:4326"))

#
#####2019 eMODIS NDVI####
raster_test_crop<-raster("./emodis_ndvi/2018-2019/ea1914.tif")
plot(mask(raster_test_crop,kongwa_wards_shp),asp=1)

###########Supervised classification of agricultural land (trained on wet season NDVI imagery)
classified_wet<-mask(raster("./classified_wet_02_03_04.tif"),kongwa_wards_shp)
plot(classified_wet,col=c("dark green","white"));title("Kongwa Agricultural Land");lines(kongwa_wards_shp)#0= farmland, 1=non-farmland

classified_wet_filter<-crop(projectRaster(classified_wet,crs = crs(raster_test_crop),res=res(raster_test_crop),method="ngb"),raster_test_crop)#use nearest neighbors (ngb) method b/c it is a categorical 0/1. bilinear for continuous vars
classified_wet_filter<-raster(vals=values(classified_wet_filter),ext=extent(raster_test_crop),crs=crs(raster_test_crop),
                              nrows=dim(raster_test_crop)[1],ncols=dim(raster_test_crop)[2])
plot(classified_wet_filter)


#extract 2018-2019 raw NDVI in walking distance (meters) buffer with wet season ag land classification
ndvi_files_2019<-list.files("./emodis_ndvi/2018-2019/")
ndvi_wet_tz19<-matrix(nrow=nrow(survey_spatial_tz19),ncol=length(ndvi_files_2019))
for(i in 1:length(ndvi_files_2019)){
  crop_ndvi_raw<-raster::mask(raster(paste("./emodis_ndvi/2018-2019/",ndvi_files_2019[i],sep="")),classified_wet_filter,maskvalue=1)
  for(j in 1:nrow(survey_spatial_tz19)){
    ndvi_wet_tz19[j,i]<-raster::extract(crop_ndvi_raw,survey_spatial_tz19[j,],buffer=survey_spatial_tz19$walk_dist_m[j],fun=mean)
  }
}

colnames(ndvi_wet_tz19)<-paste("ndvi_dekad_",substr(unlist(strsplit(list.files("./emodis_ndvi/2018-2019/"),".tif")),3,6),sep = "")
ndvi_wet_tz19<-as.data.frame(ndvi_wet_tz19)
ndvi_wet_tz19<-(ndvi_wet_tz19-100)/100 #adjust to 0-1.0 NDVI values per: https://earlywarning.usgs.gov/fews/product/448

melt_ndvi_wet<-melt(ndvi_wet_tz19)

growing_season_dekads<-c("Dec. dekad 1","Dec. dekad 2","Dec. dekad 3",
                         "Jan. dekad 1","Jan. dekad 2","Jan. dekad 3",
                         "Feb. dekad 1","Feb. dekad 2","Feb. dekad 3",
                         "Mar. dekad 1","Mar. dekad 2","Mar. dekad 3",
                         "Apr. dekad 1","Apr. dekad 2","Apr. dekad 3",
                         "May dekad 1","May dekad 2","May dekad 3")

#extract NDVI percent median walking/wet season to df
ndvi_percmedian_tz19_files<-list.files("./emodis_ndvi_percmedian/2018-2019/")

ndvi_percmedian_tz19<-matrix(nrow=nrow(survey_spatial_tz19),ncol=length(ndvi_percmedian_tz19_files))
for(i in 1:length(ndvi_percmedian_tz19_files)){
  crop_ndvi_percmedian<-raster::mask(raster(paste("./emodis_ndvi_percmedian/2018-2019/",ndvi_percmedian_tz19_files[i],sep="")),classified_wet_filter,maskvalue=1)
  for(j in 1:nrow(survey_spatial_tz19)){
    ndvi_percmedian_tz19[j,i]<-raster::extract(crop_ndvi_percmedian,survey_spatial_tz19[j,],buffer=survey_spatial_tz19$walk_dist_m[j],fun=mean)
  }
}

colnames(ndvi_percmedian_tz19)<-paste("ndvi_pct_dekad_",substr(unlist(strsplit(list.files("./emodis_ndvi_percmedian/2018-2019/"),".tif")),3,6),sep = "")
ndvi_percmedian_tz19<-as.data.frame(ndvi_percmedian_tz19)

melt_ndvi_percmedian<-melt(ndvi_percmedian_tz19)
#

#########2019 Soil Grids######
#https://www.isric.org/explore/soilgrids/faq-soilgrids
soil_files<-list.files("./soil_grids/")[-grep(".xml",list.files("./soil_grids/"))]

soil250_test<-raster::crop(raster(paste("./soil_grids/",soil_files[1],sep="")),extent(kongwa_wards_shp))

#Wet season crop mask on soil
wet_soil_mask<-crop(projectRaster(classified_wet,crs = crs(soil250_test),res=res(soil250_test),method="ngb"),extent(soil250_test))
wet_soil_mask<-raster(vals=values(wet_soil_mask),ext=extent(soil250_test),crs=crs(soil250_test),
                      nrows=dim(soil250_test)[1],ncols=dim(soil250_test)[2])

#extract wet season masked soil properties as mean of a projected walking distance radius from each mill's coordinates
wetmask_soilgrids250m<-matrix(nrow=nrow(survey_spatial_tz19),ncol=length(soil_files))
for(i in 1:length(soil_files)){
  crop_soil<-raster::mask(raster::crop(raster(paste("./soil_grids/",soil_files[i],sep="")),extent(soil250_test)),wet_soil_mask,maskvalue=1)
  for(j in 1:nrow(survey_spatial_tz19)){
    wetmask_soilgrids250m[j,i]<-raster::extract(crop_soil,survey_spatial_tz19[j,],buffer=survey_spatial_tz19$walk_dist_m[j],fun=mean)
  }
}

wetmask_soilgrids250m<-as.data.frame(wetmask_soilgrids250m)
colnames(wetmask_soilgrids250m)<-unlist(strsplit(soil_files,".tif"))

melt_wetmask_soilgrids250m<-melt(wetmask_soilgrids250m)
ggplot(melt_wetmask_soilgrids250m,aes(variable,value))+geom_violin()+
  geom_jitter(width=1/32,size=0.1)+theme_bw()+theme(axis.text.x = element_text(angle=60, hjust=1))+
  labs(x="Soil Property",y="Value")+scale_y_continuous(breaks = scales::pretty_breaks(n = 8))

wetmask_soilgrids250m_millshedmean<-aggregate(wetmask_soilgrids250m,by=list(survey_spatial_tz19$grain_mill_id),FUN=mean)
melt_wetmask_soilgrids250m_millshedmean<-melt(wetmask_soilgrids250m_millshedmean)

#
#########2019 Maize NIR data######
#each ground maize sample was scanned with SCiO NIR device three times 
input<-read.csv("kongwa_scio_input.csv")
colnames(input)[1]<-"sample_id"
colnames(input)[6:336]<-seq(740,1070,1)

input<-as.data.frame(aggregate(input[,-1],by=list(input$sample_id),FUN = mean))#average each sample's 3 replicates
colnames(input)[1]<-"sample_id"

input_spectral<-input[,c(1,26:316)]#trims noisy wavelengths at beginning/end. mycotoxin data already in survey spatial df (match by mill # and sample #)

#preprocess the wavelengths with Savitzky Golay followed by Standard Normal Variate (corrects for light scattering, etc)
sav_golay_input<-savitzkyGolay(input_spectral[,2:292],1,1,13)
savgol_snv_input<-standardNormalVariate(sav_golay_input)

melt_savgol_snv_input<-melt(cbind.data.frame(input[,1],savgol_snv_input))
colnames(melt_savgol_snv_input)<-c("sample_id","wavelength","reflectance")
melt_savgol_snv_input$wavelength<-as.numeric(as.character(melt_savgol_snv_input$wavelength))
melt_savgol_snv_input$wavelength<-rep(seq(766,1044),each=306)

ggplot(melt_savgol_snv_input,aes(wavelength,reflectance,color=as.factor(sample_id)))+geom_line()+theme_bw()+guides(color="none")+
  labs(x="Wavelengths",y="Reflectance (SNV/Sav-Golay)")+scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 15))

#PCA on preprocessed wavelengths
savgol_snv_input_pca<-prcomp(savgol_snv_input,center = TRUE, scale. = TRUE)
savgol_snv_input_topPCs<-savgol_snv_input_pca$x[,1:5]
colnames(savgol_snv_input_topPCs)<-c("savgol_snv_PC1","savgol_snv_PC2","savgol_snv_PC3","savgol_snv_PC4","savgol_snv_PC5")

savgol_snv_input_all<-cbind.data.frame(input$sample_id,savgol_snv_input,savgol_snv_input_topPCs)
colnames(savgol_snv_input_all)[1]<-"sample_id"

##########2015 all survey/toxins#######
#import 2015 dataset
survey_15<-read.csv("2015_kongwa_survey.csv")
survey_15$fum_ppm<-ifelse(survey_15$fum_ppm==0,0.005,survey_15$fum_ppm)#change 0s to 0.005 (half the LOD of 0.1ppm) so it can be log-transformed
survey_15$log10_af_ppb<-log10(survey_15$af_ppb)
survey_15$log10_fum_ppm<-log10(survey_15$fum_ppm)

survey_15_spatial<-survey_15
coordinates(survey_15_spatial)<-c("longitude","latitude")
proj4string(survey_15_spatial)<-CRS("+init=epsg:4326")

#summary statistics
table(survey_15_spatial$maize_flour_type)/sum(table(survey_15_spatial$maize_flour_type))

table(survey_15_spatial$source)/sum(table(survey_15_spatial$source))

sum(survey_15_spatial$af_ppb>10)/length(survey_15_spatial$af_ppb)
sum(survey_15_spatial$fum_ppm>2)/length(survey_15_spatial$fum_ppm)

#Assign households to nearest mill coordinates
hh_mill_dist<-distance(subset(survey_15_spatial,hh_mill=="mill"),subset(survey_15_spatial,hh_mill=="hh"),pairs=TRUE)
which.min(hh_mill_dist[,1])
hh_mill_neighbor<-apply(hh_mill_dist, 2, function(x) order(x, decreasing=F)[1])#names ordered from 73-128 for hh in francis_henry, rows of nearest mill

survey_15_spatial@data[which(survey_15_spatial$hh_mill=="hh"),"mill_id"]<-survey_15_spatial$mill_id[hh_mill_neighbor]
survey_15_spatial@data[which(survey_15_spatial$hh_mill=="hh"),"mill_lat"]<-survey_15_spatial$mill_lat[hh_mill_neighbor]
survey_15_spatial@data[which(survey_15_spatial$hh_mill=="hh"),"mill_long"]<-survey_15_spatial$mill_long[hh_mill_neighbor]

survey_15_millspatial<-survey_15_spatial@data
coordinates(survey_15_millspatial)<-c("mill_long","mill_lat")
proj4string(survey_15_millspatial)<-CRS("+init=epsg:4326")

###########2015 SoilGrids250m############
#extract soil values to each millshed
survey_15_soil_millshed<-matrix(nrow=nrow(survey_15_millspatial@data),ncol=length(soil_files))
for(i in 1:length(soil_files)){
  crop_soil<-raster::mask(raster::crop(raster(paste("./soil_grids/",soil_files[i],sep="")),extent(soil250_test)),wet_soil_mask,maskvalue=1)
  for(j in 1:nrow(survey_15)){
    survey_15_soil_millshed[j,i]<-raster::extract(crop_soil,survey_15_millspatial[j,],buffer=5000,fun=mean)
  }
}

survey_15_soil_millshed<-as.data.frame(survey_15_soil_millshed)
colnames(survey_15_soil_millshed)<-unlist(strsplit(soil_files,".tif"))

melt_survey_15_soil_millshed<-melt(survey_15_soil_millshed)
survey_15_soil_millshed_means<-aggregate(survey_15_soil_millshed,list(survey_15_millspatial$mill_id),FUN=mean)
melt_survey_15_soil_millshed_means<-melt(survey_15_soil_millshed_means)

#Figure 3: combine 2015/2019 soil organic carbon ("soc") millshed means 
soc_15_millshed_mean<-melt_survey_15_soil_millshed_means[grep("soc",melt_survey_15_soil_millshed_means$variable),]
soc_15_millshed_mean$survey<-as.factor(2015)

soc_19_millshed_mean<-melt_wetmask_soilgrids250m_millshedmean[grep("soc",melt_wetmask_soilgrids250m_millshedmean$variable),]
soc_19_millshed_mean$survey<-as.factor(2019)

soc_1519_millshedmean<-rbind.data.frame(soc_15_millshed_mean,soc_19_millshed_mean)

figure3<-ggplot(soc_1519_millshedmean,
       aes(factor(variable,level=unique(soc_1519_millshedmean$variable)[c(1,5,3,4,6,2)]),value,color=survey))+geom_violin(scale="width")+
  geom_sina(scale="width",size=0.9)+theme_bw()+theme(axis.text.x = element_text(angle=60, hjust=1))+
  labs(x="Depth",y="Mean SOC (dg/kg)")+scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  geom_pwc(label="p.signif",p.adjust.method = "holm")+
  scale_x_discrete(labels=c("0-5 cm","5-15 cm","15-30 cm","30-60 cm","60-100 cm","100-200 cm"))+
  scale_color_manual(name="Growing Season",values=c("#D55E00","#56B4E9"),labels=c("2014-2015", "2018-2019"))+
  stat_summary(fun = "mean",geom = "crossbar",color = "black",width=0.5,aes(group=survey),position=position_dodge(width=0.9))

###########2015 eMODIS NDVI############
##millshed means (households assigned nearest mill) (with wet season crop mask)
ndvi_tz_15_files<-list.files("./emodis_ndvi/2014-2015")

ndvi_tz_15_millshed<-matrix(nrow=nrow(survey_15_millspatial),ncol=length(ndvi_tz_15_files))
for(i in 1:length(ndvi_tz_15_files)){
  ndvi_tz_15_mask_millshed<-raster::mask(raster(paste("./emodis_ndvi/2014-2015/",ndvi_tz_15_files[i],sep="")),classified_wet_filter,maskvalue=1)
  for(j in 1:nrow(survey_15_spatial)){
    ndvi_tz_15_millshed[j,i]<-raster::extract(ndvi_tz_15_mask_millshed,survey_15_millspatial[j,],buffer=5000,fun=mean)
  }
}

ndvi_tz_15_millshed<-as.data.frame(ndvi_tz_15_millshed)
colnames(ndvi_tz_15_millshed)<-unlist(strsplit(ndvi_tz_15_files,".tif"))
ndvi_tz_15_millshed<-(ndvi_tz_15_millshed-100)/100

melt_ndvi_15_millshed<-melt(ndvi_tz_15_millshed)

ndvi_tz_15_millshedmeans<-aggregate(ndvi_tz_15_millshed,by=list(survey_15_millspatial$mill_id),FUN=mean)

melt_ndvi_tz_15_millshedmeans<-melt(ndvi_tz_15_millshedmeans)
melt_ndvi_tz_15_millshedmeans$ndvi<-melt_ndvi_tz_15_millshedmeans$value
melt_ndvi_tz_15_millshedmeans$dekad_number<-as.factor(c(rep(seq(1,18,1),each=32)))
melt_ndvi_tz_15_millshedmeans$survey<-as.factor(2015)

ndvi_tz_19_millshedmeans<-aggregate(ndvi_wet_tz19,by=list(survey_spatial_tz19$grain_mill_id),FUN=mean)

melt_ndvi_tz_19_millshedmeans<-melt(ndvi_tz_19_millshedmeans)
melt_ndvi_tz_19_millshedmeans$ndvi<-melt_ndvi_tz_19_millshedmeans$value
melt_ndvi_tz_19_millshedmeans$dekad_number<-as.factor(c(rep(seq(1,18,1),each=length(unique(melt_ndvi_tz_19_millshedmeans$Group.1)))))
melt_ndvi_tz_19_millshedmeans$survey<-as.factor(2019)

melt_ndvi_tz_1519_millshedmeans<-rbind.data.frame(melt_ndvi_tz_15_millshedmeans,melt_ndvi_tz_19_millshedmeans)

pvals_ndvi<-numeric(length(unique(melt_ndvi_tz_1519_millshedmeans$dekad_number)))
for(i in 1:length(unique(melt_ndvi_tz_1519_millshedmeans$dekad_number))){
  pvals_ndvi[i]<-t.test(ndvi~survey,data=subset(melt_ndvi_tz_1519_millshedmeans,dekad_number==i),var.equal=FALSE)$p.value
}

pvals_ndvi_adjusted<-p.adjust(pvals_ndvi, method = "holm")
pvals_ndvi_adjusted<-cbind.data.frame(pvals_ndvi_adjusted,cut(pvals_ndvi_adjusted,
                                                              breaks=c(1,0.05,0.01,0.001,0.0001,0),
                                                              labels=c("****","***","**","*","ns")))
colnames(pvals_ndvi_adjusted)[2]<-"labels"
pvals_ndvi_adjusted$dekad_number<-unique(melt_ndvi_tz_1519_millshedmeans$dekad_number)

#extract 2014-2015 NDVI percent median 
dekads_2015percmedian<-list.files("./emodis_ndvi_percmedian/2014-2015")

ndvi_percmedian_15_millshed<-matrix(nrow=nrow(survey_15_millspatial),ncol=length(dekads_2015percmedian))

for(i in 1:length(dekads_2015percmedian)){
  ndvipercmed_tz_15_mask_millshed<-raster::mask(raster(paste("./emodis_ndvi_percmedian/2014-2015/",dekads_2015percmedian[i],sep="")),classified_wet_filter,maskvalue=1)
  for(j in 1:nrow(survey_15_millspatial)){
    ndvi_percmedian_15_millshed[j,i]<-raster::extract(ndvipercmed_tz_15_mask_millshed,survey_15_millspatial[j,],buffer=5000,fun=mean)
  }
}

colnames(ndvi_percmedian_15_millshed)<-substr(dekads_2015percmedian,1,9)
ndvi_percmedian_15_millshed<-as.data.frame(ndvi_percmedian_15_millshed)

ndvi_percmedian_15_millshedmean<-aggregate(ndvi_percmedian_15_millshed,by=list(survey_15_millspatial$mill_id),FUN=mean)
melt_ndvi_percmedian_15_millshedmean<-melt(ndvi_percmedian_15_millshedmean)
melt_ndvi_percmedian_15_millshedmean$dekad_number<-as.factor(c(rep(seq(1,18,1),each=32)))
melt_ndvi_percmedian_15_millshedmean$survey<-as.factor(2015)

ndvi_percmedian_tz19_millshedmean<-aggregate(ndvi_percmedian_tz19,by=list(survey_spatial_tz19$grain_mill_id),FUN=mean)
melt_ndvi_percmedian_tz19_millshedmean<-melt(ndvi_percmedian_tz19_millshedmean)
melt_ndvi_percmedian_tz19_millshedmean$dekad_number<-as.factor(c(rep(seq(1,18,1),each=length(unique(melt_ndvi_percmedian_tz19_millshedmean$Group.1)))))
melt_ndvi_percmedian_tz19_millshedmean$survey<-as.factor(2019)

melt_ndvi_percmedian_1519_millshedmean<-rbind.data.frame(melt_ndvi_percmedian_15_millshedmean,melt_ndvi_percmedian_tz19_millshedmean)

pvals_ndvipercmed<-numeric(length(unique(melt_ndvi_percmedian_1519_millshedmean$dekad_number)))
for(i in 1:length(unique(melt_ndvi_percmedian_1519_millshedmean$dekad_number))){
  pvals_ndvipercmed[i]<-t.test(value~survey,data=subset(melt_ndvi_percmedian_1519_millshedmean,dekad_number==i),var.equal=FALSE,alternative="two.sided")$p.value
}

pvals_ndvipercmed_adjusted<-p.adjust(pvals_ndvipercmed, method = "holm")
pvals_ndvipercmed_adjusted<-cbind.data.frame(pvals_ndvipercmed_adjusted,0.65)
colnames(pvals_ndvipercmed_adjusted)[2]<-"y"
pvals_ndvipercmed_adjusted$dekad_number<-unique(melt_ndvi_percmedian_1519_millshedmean$dekad_number)
pvals_ndvipercmed_adjusted$labels<-cut(pvals_ndvipercmed_adjusted$pvals_ndvipercmed_adjusted,
                                       breaks=c(1,0.05,0.01,0.001,0.0001,0),
                                       labels=c("****","***","**","*","ns"))

#Figure 2: combine Figure 2A/2B into single image
figure2<-ggarrange(ggplot(melt_ndvi_tz_1519_millshedmeans,aes(dekad_number,value,color=survey))+geom_violin(scale="width")+
            geom_sina(scale="width",size=0.9)+theme_bw()+
              theme(axis.text.x = element_text(size=8, angle=45, hjust=1),legend.key.size = unit(0.5, "cm"),legend.title=element_text(size=10))+
            labs(x="Dekad",y="NDVI",title="(A)")+scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
            scale_x_discrete(labels=growing_season_dekads)+
            geom_pwc(label="p.signif",p.adjust.method = "holm",label.size = 2.5,tip.length = 0.01)+
            scale_color_manual(name="Growing Season",values=c("#D55E00","#56B4E9"),labels=c("2014-2015", "2018-2019"))+
            stat_summary(fun = "mean",geom = "crossbar",color = "black",width=0.5,aes(group=survey),position=position_dodge(width=0.9)),
          ggplot(melt_ndvi_percmedian_1519_millshedmean,aes(dekad_number,value,color=survey))+geom_violin(scale="width")+
            geom_sina(scale="width",size=0.9)+theme_bw()+
            theme(axis.text.x = element_text(size=8, angle=45, hjust=1),legend.key.size = unit(0.5, "cm"),legend.title=element_text(size=10))+
            labs(x="Dekad",y="NDVI % Median",title="(B)")+scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
            scale_x_discrete(labels=growing_season_dekads)+
            geom_pwc(label="p.signif",p.adjust.method = "holm",label.size = 2.5,tip.length = 0.01)+
            scale_color_manual(name="Growing Season",values=c("#D55E00","#56B4E9"),labels=c("2014-2015", "2018-2019"))+
            stat_summary(fun = "mean",geom = "crossbar",color = "black",width=0.5,aes(group=survey),position=position_dodge(width=0.9)),
          ncol=1,nrow=2)

#
#######2019 Combine Data#####
#add elevation data
elevation<-get_elev_point(sf::st_as_sf(as.data.frame(survey_spatial_tz19@coords), coords = c("longitude", "latitude"), crs = 4326),src="aws")
survey_spatial_tz19$elevation_m<-elevation$elevation

#Figure 1: Kongwa map with mill locations from each survey
kongwa_elevation<-raster::mask(get_elev_raster(kongwa_wards_shp,z=12),kongwa_wards_shp)

tiff("Fig1.tif", width = 15, height = 15, units = "cm", res = 300,compression = "lzw")

raster::plot(kongwa_elevation,xlab = "Longitude", ylab = "Latitude",yaxt="n",xaxt="n",legend.width = 3,legend.args=list(text='Elevation (m)'))
axis(2,at=seq(-6.4,-5.6,0.2),labels=c("-6.4\u00B0","-6.2\u00B0","-6.0\u00B0","-5.8\u00B0","-5.6\u00B0"))
axis(1,at=seq(36.2,37,0.2),labels=c("36.2\u00B0","36.4\u00B0","36.6\u00B0","36.8\u00B0","37.0\u00B0"))
points(unique(coordinates(survey_spatial_tz19)),pch=20,col="black")
points(unique(coordinates(survey_15_millspatial)),pch=20,col="red")

dev.off()

#combine data on a per sample basis
all_preds_maizemask_sample<-cbind.data.frame(survey_spatial_tz19@data,wetmask_soilgrids250m,ndvi_wet_tz19)
all_preds_maizemask_sample<-merge(all_preds_maizemask_sample,savgol_snv_input_all,by="sample_id")

#2019 millshed means exclude factor/character variables that are unique to each sample
all_preds_maizemask_mill<-aggregate(all_preds_maizemask_sample[,c(7:375)],list(all_preds_maizemask_sample$grain_mill_id),FUN=mean,na.rm=TRUE)
colnames(all_preds_maizemask_mill)[1]<-"grain_mill_id"
all_preds_maizemask_mill<-cbind.data.frame(aggregate(all_preds_maizemask_sample[,c("village")],by=list(all_preds_maizemask_sample$grain_mill_id),FUN=unique)[,2],
                                           all_preds_maizemask_mill)
colnames(all_preds_maizemask_mill)[1]<-"mill_village"

#PCAs on 2019 millshed mean soil and ndvi data. then add top PCs to 2019 millshed df
ndvi_19_dekad_pca<-prcomp(all_preds_maizemask_mill[,grep("ndvi",colnames(all_preds_maizemask_mill))],center = TRUE, scale. = TRUE)
ndvi_19_dekad_topPCs<-ndvi_19_dekad_pca$x[,1:3]
colnames(ndvi_19_dekad_topPCs)<-c("ndvi_dekad_PC1","ndvi_dekad_PC2","ndvi_dekad_PC3")

soil_19_pca<-prcomp(all_preds_maizemask_mill[,grep("mean",colnames(all_preds_maizemask_mill))],center = TRUE, scale. = TRUE)
soil_19_topPCs<-soil_19_pca$x[,1:5]
colnames(soil_19_topPCs)<-c("soil_PC1","soil_PC2","soil_PC3","soil_PC4","soil_PC5")

all_preds_maizemask_mill<-cbind.data.frame(all_preds_maizemask_mill,ndvi_19_dekad_topPCs,soil_19_topPCs)
#
###########2019 Mycotoxin Models#############
###2019: model aflatoxin/fumonisins using spectral, ndvi PCs, and soil PCs
##2019 Aflatoxin
#mixed linear model (mill village as random effect)
mixedlm_afla_mill<-lmer(af_log10ppb~
                          savgol_snv_PC1+savgol_snv_PC2+savgol_snv_PC3+savgol_snv_PC4+savgol_snv_PC5+
                          ndvi_dekad_PC1+ndvi_dekad_PC2+ndvi_dekad_PC3+
                          soil_PC1+soil_PC2+soil_PC3+soil_PC4+soil_PC5+
                          (1|mill_village),
                        data=all_preds_maizemask_mill)
summary(mixedlm_afla_mill)
r.squaredGLMM(mixedlm_afla_mill)
RMSE.merMod(mixedlm_afla_mill)
plot(mixedlm_afla_mill)
shapiro.test(residuals(mixedlm_afla_mill))
##2019 FUM
#mixed linear model
mixedlm_fum_mill<-lmer(fum_log10ppm~
                         savgol_snv_PC1+savgol_snv_PC2+savgol_snv_PC3+savgol_snv_PC4+savgol_snv_PC5+
                         ndvi_dekad_PC1+ndvi_dekad_PC2+ndvi_dekad_PC3+
                         soil_PC1+soil_PC2+soil_PC3+soil_PC4+soil_PC5+
                         (1|mill_village),
                       data=all_preds_maizemask_mill)
summary(mixedlm_fum_mill)
r.squaredGLMM(mixedlm_fum_mill)
RMSE.merMod(mixedlm_fum_mill)
plot(mixedlm_fum_mill)
shapiro.test(residuals(mixedlm_fum_mill))

#predict flour type based on NIR
set.seed(5)
nir_flour_rf<-randomForest(all_preds_maizemask_sample[complete.cases(all_preds_maizemask_sample$flour_type),c(92:370)],
             all_preds_maizemask_sample$flour_type[complete.cases(all_preds_maizemask_sample$flour_type)],
             importance=TRUE,ntree=500)
nir_flour_rf

nir_flour_rf_imp<-varImpPlot(nir_flour_rf)
nir_flour_rf_imp<-cbind.data.frame(rownames(nir_flour_rf_imp),as.matrix(nir_flour_rf_imp[,1:2]))
colnames(nir_flour_rf_imp)<-c("wavelength","Perc_IncMSE","IncNodePurity")

nir_flour_rf_imp[order(nir_flour_rf_imp$Perc_IncMSE),]

figure4<-ggplot(nir_flour_rf_imp,aes(as.numeric(wavelength),Perc_IncMSE))+geom_point()+
  theme_bw()+labs(x="Wavelength (nm)",y="Increase MSE (%)")+
  scale_x_continuous(limits=c(766,1044),breaks=seq(750,1050,20))
#
###########2015 Combine Data############
#first calculate PCs for ndvi/soil data
ndvi_15_dekad_pca<-prcomp(ndvi_tz_15_millshed,center = TRUE, scale. = TRUE)
summary(ndvi_15_dekad_pca)
ndvi_15_dekad_topPCs<-ndvi_15_dekad_pca$x[,1:3]
colnames(ndvi_15_dekad_topPCs)<-c("ndvi_15_dekad_PC1","ndvi_15_dekad_PC2","ndvi_15_dekad_PC3")

soil_15_pca<-prcomp(survey_15_soil_millshed,center = TRUE, scale. = TRUE)
summary(soil_15_pca)
soil_15_topPCs<-soil_15_pca$x[,1:5]
colnames(soil_15_topPCs)<-c("soil_15_PC1","soil_15_PC2","soil_15_PC3","soil_15_PC4","soil_15_PC5")

#single data frame for survey, ndvi, and soil on a per sample basis (2015)
survey_15_sample<-cbind.data.frame(survey_15_millspatial@data,survey_15_soil_millshed,ndvi_tz_15_millshed,soil_15_topPCs,ndvi_15_dekad_topPCs)

#
###########2015 Mycotoxin Models#############
#2015 summary statistics
Gmean(survey_15_sample$af_ppb,conf.level=0.95)
Gmean(survey_15_sample$fum_ppm,conf.level=0.95)

summary(na.exclude(as.factor(survey_15_sample$maize_flour_type)))/length(na.exclude(as.factor(survey_15_sample$maize_flour_type)))

summary(as.factor(survey_15_sample$source))
summary(na.exclude(as.factor(survey_15_sample$source)))/length(na.exclude(as.factor(survey_15_sample$source)))

#linear models for all data and subset by source (homegrown vs. purchased)
summary(lm(log10_af_ppb~maize_flour_type,data=survey_15_sample))
summary(lm(log10_fum_ppm~maize_flour_type,data=survey_15_sample))

afla_full2015_lm<-lm(log10_af_ppb~.,data=survey_15_sample[,c("log10_af_ppb","maize_flour_type","source","elevation_m",
                                                                    "soil_15_PC1","soil_15_PC2","soil_15_PC3","soil_15_PC4","soil_15_PC5",
                                                                    "ndvi_15_dekad_PC1","ndvi_15_dekad_PC2","ndvi_15_dekad_PC3")])
summary(afla_full2015_lm)
MSE(afla_full2015_lm)
RMSE(afla_full2015_lm$fitted.values,afla_full2015_lm$model$log10_af_ppb)

afla_hg2015_lm<-lm(log10_af_ppb~.,data=subset(survey_15_sample,source=="homegrown")[,c("log10_af_ppb","maize_flour_type","elevation_m",
                                                                                              "soil_15_PC1","soil_15_PC2","soil_15_PC3","soil_15_PC4","soil_15_PC5",
                                                                                              "ndvi_15_dekad_PC1","ndvi_15_dekad_PC2","ndvi_15_dekad_PC3")])
summary(afla_hg2015_lm)
RMSE(afla_hg2015_lm$fitted.values,afla_hg2015_lm$model$log10_af_ppb)
MSE(afla_hg2015_lm)

afla_purc2015_lm<-lm(log10_af_ppb~.,data=subset(survey_15_sample,source=="purchased")[,c("log10_af_ppb","maize_flour_type","elevation_m",
                                                                                                "soil_15_PC1","soil_15_PC2","soil_15_PC3","soil_15_PC4","soil_15_PC5",
                                                                                                "ndvi_15_dekad_PC1","ndvi_15_dekad_PC2","ndvi_15_dekad_PC3")])
summary(afla_purc2015_lm)
RMSE(afla_purc2015_lm$fitted.values,afla_purc2015_lm$model$log10_af_ppb)
MSE(afla_purc2015_lm)

#2015 fumonisin linear models
fum_full2015_lm<-lm(log10_fum_ppm~.,data=survey_15_sample[,c("log10_fum_ppm","maize_flour_type","source","elevation_m",
                                                                    "soil_15_PC1","soil_15_PC2","soil_15_PC3","soil_15_PC4","soil_15_PC5",
                                                                    "ndvi_15_dekad_PC1","ndvi_15_dekad_PC2","ndvi_15_dekad_PC3")])
summary(fum_full2015_lm)
RMSE(fum_full2015_lm$fitted.values,fum_full2015_lm$model$log10_fum_ppm)
MSE(fum_full2015_lm)

fum_hg2015_lm<-lm(log10_fum_ppm~.,data=subset(survey_15_sample,source=="homegrown")[,c("log10_fum_ppm","maize_flour_type","elevation_m",
                                                                                              "soil_15_PC1","soil_15_PC2","soil_15_PC3","soil_15_PC4","soil_15_PC5",
                                                                                              "ndvi_15_dekad_PC1","ndvi_15_dekad_PC2","ndvi_15_dekad_PC3")])
summary(fum_hg2015_lm)
RMSE(fum_hg2015_lm$fitted.values,fum_hg2015_lm$model$log10_fum_ppm)
MSE(fum_hg2015_lm)

fum_purc2015_lm<-lm(log10_fum_ppm~.,data=subset(survey_15_sample,source=="purchased")[,c("log10_fum_ppm","maize_flour_type","elevation_m",
                                                                                                "soil_15_PC1","soil_15_PC2","soil_15_PC3","soil_15_PC4","soil_15_PC5",
                                                                                                "ndvi_15_dekad_PC1","ndvi_15_dekad_PC2","ndvi_15_dekad_PC3")])
summary(fum_purc2015_lm)
RMSE(fum_purc2015_lm$fitted.values,fum_purc2015_lm$model$log10_fum_ppm)
MSE(fum_purc2015_lm)

#####Additional Statistics and Figures#######
summary(c(all_preds_maizemask_mill$elevation_m,survey_15_sample$elevation_m))

t.test(survey_15_sample$af_ppb,survey_all_tz19$af_ppb, var.equal = FALSE)
t.test(survey_15_sample$fum_ppm,survey_all_tz19$fum_ppm, var.equal = FALSE)

t.test(survey_15_sample$log10_af_ppb,survey_all_tz19$log10_af_ppb, var.equal = FALSE)
t.test(survey_15_sample$log10_fum_ppm,survey_all_tz19$log10_fum_ppm, var.equal = FALSE)

all_toxins_2015<-cbind.data.frame("2015",survey_15_sample[,c("af_ppb","fum_ppm","log10_af_ppb","log10_fum_ppm")])
colnames(all_toxins_2015)<-c("year","af_ppb","fum_ppm","af_log10ppb","fum_log10ppm")

all_toxins_2019<-cbind.data.frame("2019",survey_all_tz19[,c("af_ppb","fum_ppm","af_log10ppb","fum_log10ppm")])
colnames(all_toxins_2019)[1]<-c("year")

all_toxins<-rbind.data.frame(all_toxins_2015,all_toxins_2019)
all_toxins_melt<-melt(all_toxins)
ggplot(all_toxins_melt,aes(variable,value,color=as.factor(year)))+geom_violin()

legal_limits <- data.frame(variable = c("af_log10ppb", "fum_log10ppm"), Z = c(log10(10),log10(2)))

#Figure 5: Mycotoxins by survey year
figure5<-ggplot(subset(all_toxins_melt,variable %in% c("af_log10ppb","fum_log10ppm")),aes(year,value))+
  geom_violin(scale = "count")+facet_grid(variable~.,scales = "free_y",
                                          labeller=labeller(variable = c(af_log10ppb ="Aflatoxins log10(\u03BCg/kg)",fum_log10ppm="Fumonisins log10(mg/kg)")))+
  geom_sina(scale="count")+labs(x="Survey Year",y="")+
  geom_abline(data = legal_limits,color=c("blue","blue"),linetype="dashed", aes(intercept = Z, slope = 0))+
  theme_bw()+geom_pwc(label = "p.signif",method = "t.test",vjust = 0.5)+
  geom_text(data=data.frame(x=0.65,y=c(1.2,0.5),lab=c("10 \u03BCg/kg ML","2 mg/kg ML"),variable=c("af_log10ppb","fum_log10ppm")),
            aes(x,y,label=lab),color="blue",size=3)

flour_plot_data<-survey_15_sample[,c("maize_flour_type","log10_af_ppb","log10_fum_ppm")]
flour_plot_data$survey<-2015
flour_plot_data$maize_flour_type<-ifelse(flour_plot_data$maize_flour_type=="soaked_sembe","kiwerege",flour_plot_data$maize_flour_type)

flour_plot_data_2019<-survey_all_tz19[,c("flour_type","af_log10ppb","fum_log10ppm")]
flour_plot_data_2019$survey<-2019


colnames(flour_plot_data_2019)<-c("maize_flour_type","log10_af_ppb","log10_fum_ppm","survey")

flour_plot_data<-rbind.data.frame(flour_plot_data,flour_plot_data_2019)
flour_plot_data$maize_flour_type<-factor(flour_plot_data$maize_flour_type, levels=c("dona","sembe","kiwerege"))


flour_plot_data_melt<-melt(flour_plot_data,id.vars = c("maize_flour_type","survey"))

#Figure 6: Mycotoxins divided by survey year and flour type
figure6<-ggadjust_pvalue(ggplot(subset(flour_plot_data_melt,maize_flour_type!="NA"),aes(maize_flour_type,value))+geom_violin(scale="count")+scale_x_discrete(labels=c("Dona", "Sembe", "Kiwerege"))+
                  geom_sina(scale="count",size=0.9)+facet_grid(variable~survey,scales = "free_y",labeller=labeller(variable = c(log10_af_ppb ="Aflatoxins log10(\u03BCg/kg)",log10_fum_ppm="Fumonisins log10(mg/kg)")))+theme_bw()+
                  labs(x="Flour Type",y="")+
                  geom_pwc(label = "p.format",method = "t.test"),
                label="p.adj.signif",p.adjust.method="holm")

summary(lm(log10_af_ppb~maize_flour_type,data=subset(flour_plot_data,survey==2015)))
pairwise.t.test(x=subset(flour_plot_data,survey==2015)$log10_af_ppb,
                g=subset(flour_plot_data,survey==2015)$maize_flour_type,
                p.adjust.method = "holm")
summary(lm(log10_af_ppb~maize_flour_type,data=subset(flour_plot_data,survey==2019)))

summary(lm(log10_fum_ppm~maize_flour_type,data=subset(flour_plot_data,survey==2015)))
pairwise.t.test(x=subset(flour_plot_data,survey==2015)$log10_fum_ppm,
                g=subset(flour_plot_data,survey==2015)$maize_flour_type,
                p.adjust.method = "holm")
summary(lm(log10_fum_ppm~maize_flour_type,data=subset(flour_plot_data,survey==2019)))
pairwise.t.test(x=subset(flour_plot_data,survey==2019)$log10_fum_ppm,
                g=subset(flour_plot_data,survey==2019)$maize_flour_type,
                p.adjust.method = "holm")

