#you'll need to get an API token from the census.gov, each are individual

##census_api_key("a2b0bffcbfc22406b200e2626ade32194a0b60c9", install= TRUE)

#specify variables list

vars<- c("B01003_001", "B03002_003", "B03002_004", "B19001_002", "B19001_003", "B19001_004", "B19001_005" , 
         "B19001_014", "B19001_015", "B19001_016","B19001_017", "B19001A_014","B19001A_015", "B19001A_016", 
         "B19001A_017", "B19001B_002", "B19001B_003", "B19001B_004" , "B19001B_005")

#pull data from 2104 5 year ACS, specifying census tract level data for Shelby County TN

shelby<- get_acs(geography = "tract", 
                 variables = vars,
                 state = "TN", 
                 county ="Shelby",
                 year = 2014,
                 output = "wide")


#calculate ICE income

shelby$ICEr<- (shelby$B19001_014E+ shelby$B19001_015E+shelby$B19001_016E+ shelby$B19001_017E)
shelby$ICEp<-(shelby$B19001_002E+shelby$B19001_003E+shelby$B19001_004E+shelby$B19001_005E)
shelby$ICEinc<-((shelby$ICEr - shelby$ICEp)/shelby$B01003_001E)



#calculate ICE race * income

shelby$ICEwr<-(shelby$B19001A_014E + shelby$B19001A_015E + shelby$B19001A_016E + shelby$B19001A_017E)
shelby$ICEbp<-(shelby$B19001B_002E + shelby$B19001B_003E + shelby$B19001B_004E + shelby$B19001B_005E)
shelby$ICEraceinc<- ((shelby$ICEwr-shelby$ICEbp)/shelby$B01003_001E)


#calculate ICE race

shelby$ICErace <- ((shelby$B03002_003E - shelby$B03002_004E)/shelby$B01003_001E)
#coerce to a dataframe

d<-as.data.frame(shelby)



#plot ICE Race * income 

ggplot(d, aes(x = ICEraceinc, color=ICEraceinc)) +
  geom_histogram( fill = "lightblue") + 
  geom_vline(aes(xintercept = median(ICEraceinc)), linetype = "dashed") +
  theme(panel.background = element_blank())+
  xlab("Shelby County ICE Race x Income") +
  ylab("Frequency")


#Load a shape file for Shelby county census tracts


shape<-tracts(47, county = 157, cb = TRUE, year = 2014)


# convert the GEOID to numeric and join up datasets

shape$GEOID <- as.numeric(shape$GEOID)
d$GEOID<-as.numeric(d$GEOID)

d<-merge(d, coi, by= GEOID, all.x=TRUE)


spatial<-geo_join(shape, d, by = "GEOID", how = "left")
ICEmap <- ggplot() + 
  geom_sf(data = spatial, aes(fill = ICEraceinc))


ICEmap + labs(x= "Racialized wealth inequality in Shelby County, TN", fill = "Concentration of white wealth")+ theme(panel.background = element_blank())


ICEmap2 <- ggplot() + 
  geom_sf(data = spatial, aes(fill = ICErace))


ICEmap2 + labs(x= "Racial segregation in Shelby County, TN", fill = "Concentration of white vs. Black residents")+ theme(panel.background = element_blank())



