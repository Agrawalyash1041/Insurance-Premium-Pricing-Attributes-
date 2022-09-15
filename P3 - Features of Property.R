########################################################################
#       Features of Property that affect the Premium Prices            #
########################################################################


## Lets see the factors that are affecting the Premium Prices of Property

Property_Premium <- subset(cleaned, select = c(BEDROOMS,ROOF_CONSTRUCTION,WALL_CONSTRUCTION,
                                               FLOODING,SUBSIDENCE,LISTED,NEIGH_WATCH,
                                               APPR_ALARM,APPR_LOCKS,PROP_TYPE,OWNERSHIP_TYPE,YEARBUILT,
                                               SAFE_INSTALLED,BUILDINGS_COVER,LAST_ANN_PREM_GROSS)) %>% 
  rename(Premium=LAST_ANN_PREM_GROSS)
View(Property_Premium)
attach(Property_Premium)



## 1)  Lets see the premium for each house type

Property_Type = Property_Premium %>% select(PROP_TYPE,Premium) %>% group_by(PROP_TYPE) %>% 
  summarise(Avg_Premium = round(mean(Premium),2)) %>% arrange(desc(Avg_Premium))
View(Property_Type)
attach(Property_Type)

# Lets plot the Graph for the same 
Pro_type_Bar = ggplot(Property_Type) + aes(reorder(x = PROP_TYPE,-Avg_Premium), y = Avg_Premium, fill = PROP_TYPE) +
  geom_bar(stat ="identity",width =0.5, color = "Blue")+
  geom_text(aes(label=paste(Avg_Premium),vjust=-0, hjust = -0.15)) +
  ggtitle("Average Premium for Property Type") + coord_flip()

Pro_type_Bar 

## For Property Type 37 the Average of Premium is the Highest.



## 2) lets check premium for the Properties for th year in which they were build.

Year_Premium = Property_Premium %>% select(YEARBUILT,Premium) %>% group_by(YEARBUILT) %>%
  summarise(Avg_Premium = round(mean(Premium),2)) %>% arrange(desc(Avg_Premium))
View(Year_Premium)
attach(Year_Premium)

# lets Plot the Bar for the Same

Year_Build_Bar = ggplot(Year_Premium) + aes(reorder(x = YEARBUILT,-Avg_Premium), y = Avg_Premium) +
  geom_bar(stat ="identity",width =0.5, fill = topo.colors(17))+
  geom_text(aes(label=paste(Avg_Premium),vjust=-0.35)) +
  ggtitle("Average Premium for Properties Yearwise")+
  xlab(label = "Year Build") 

Year_Build_Bar

## The properties that were build in the Year 1749 has the highest Mean Premium.



## 3) Lets check for the Premium based on their Ownership_Type.

Owner_Premium = Property_Premium %>% select(OWNERSHIP_TYPE,Premium) %>% group_by(OWNERSHIP_TYPE) %>%
  summarise(Avg_Premium = round(mean(Premium),2)) %>% arrange(desc(Avg_Premium))
View(Owner_Premium)
attach(Owner_Premium)

# Lets Plot for the same 

Ownership_Bar = ggplot(Owner_Premium) + aes(reorder(x = OWNERSHIP_TYPE,-Avg_Premium), y = Avg_Premium, fill = OWNERSHIP_TYPE) +
  geom_bar(stat ="identity",width =0.5, fill = cm.colors(14), colour = "Black")+
  geom_text(aes(label=paste(Avg_Premium),vjust=-0.35)) +
  ggtitle("Average Premium for Properties Ownership Type")+
  xlab(label = "Ownership_Type")
Ownership_Bar

## The Premium for the Properties with Ownership Type 3 has the highest Premium.



## 4) Lets check the Premium based on the number of bedroom does a property has.

Bedroom_Premium = Property_Premium %>% select(BEDROOMS,Premium) %>% group_by(BEDROOMS) %>%
  summarise(Avg_Premium = round(mean(Premium),2)) %>% arrange(desc(Avg_Premium))
View(Bedroom_Premium)
attach(Bedroom_Premium)

## lets plot for the same

Bedroom_Bar = ggplot(Bedroom_Premium) + aes(reorder(x = BEDROOMS,-Avg_Premium), y = Avg_Premium, fill = Avg_Premium) +
  geom_bar(stat ="identity",width =0.5, color = "darkblue", fill = rainbow(7))+
  geom_text(aes(label=paste(Avg_Premium),vjust=-0.35)) +
  ggtitle("Impact of No. of Bedrooms on Premium of Property") + 
  xlab(label = "No. of Bedrooms")
Bedroom_Bar

## the highest Premium is for Property with 7 Bedrooms , With an decreasing number of bedroom the premium also decreases



## 5) lets check for Premium Based on Wall Construction of a Property

Wall_Premium = Property_Premium %>% select(WALL_CONSTRUCTION,Premium) %>% group_by(WALL_CONSTRUCTION) %>%
  summarise(Avg_Premium = round(mean(Premium),2)) %>% arrange(desc(Avg_Premium))
View(Wall_Premium)
attach(Wall_Premium)

## lets plot for the same

Wall_Bar = ggplot(Wall_Premium) + aes(reorder(x = WALL_CONSTRUCTION,-Avg_Premium), y = Avg_Premium, fill = WALL_CONSTRUCTION) +
  geom_bar(stat ="identity",width =0.5, color = "darkblue", fill = topo.colors(19))+
  geom_text(aes(label=paste(Avg_Premium),hjust=-0.30)) +
  ggtitle("Impact of Wall Construction  on Premium of Property") + xlab(label = "Wall Construction") +
  coord_flip()
Wall_Bar

## The customer with Wall Construction 99 is more likely to pay more premium 



## 6) Lets Check Premium  for Roof Construction of a building.

Roof_Premium = Property_Premium %>% select(ROOF_CONSTRUCTION,Premium) %>% group_by(ROOF_CONSTRUCTION) %>%
  summarise(Avg_Premium = round(mean(Premium),2)) %>% arrange(desc(Avg_Premium))
View(Roof_Premium)
attach(Roof_Premium)

## lets plot for the same

Roof_Bar = ggplot(Roof_Premium) + aes(reorder(x = ROOF_CONSTRUCTION,-Avg_Premium), y = Avg_Premium, fill = ROOF_CONSTRUCTION) +
  geom_bar(stat ="identity",width =0.5, color = "darkblue", fill = terrain.colors(17))+
  geom_text(aes(label=paste(Avg_Premium),vjust=-0.35)) +
  ggtitle("Impact of Roof Construction  on Premium of Property") +
  xlab(label = "Roof Construction")
Roof_Bar

## The Property which has Roof Construction as 12 are more likely to pay higher premium



## 7) lets check the imppact of flooding on the premium of the property 


Flood_Premium = Property_Premium %>% select(FLOODING,Premium) %>% group_by(FLOODING) %>%
  summarise(Avg_Premium = round(mean(Premium),2)) %>% arrange(desc(Avg_Premium))
View(Flood_Premium)
attach(Flood_Premium)

## lets plot for the same

Flood_Bar = ggplot(Flood_Premium) + aes(reorder(x = FLOODING,-Avg_Premium), y = Avg_Premium) +
  geom_bar(stat ="identity",width =0.5, color = "Black" , fill = "violetred4")+
  geom_text(aes(label=paste(Avg_Premium),vjust=-0.25)) +
  ggtitle("Impact of Flooding  on Premium of Property") +
  xlab(label = "Flooding")
Flood_Bar

## We Can see that the Property with Flooding indicator as NO has slightly higher Premium.


## 8) Lets check for premium of Property based on Listed Property 


Listed_Premium = Property_Premium %>% select(LISTED,Premium) %>% group_by(LISTED) %>%
  summarise(Avg_Premium = round(mean(Premium),2)) %>% arrange(desc(Avg_Premium))
View(Listed_Premium)
attach(Listed_Premium)

## lets plot for the same

Listed_Bar = ggplot(Listed_Premium) + aes(reorder(x = LISTED,-Avg_Premium), y = Avg_Premium) +
  geom_bar(stat ="identity",width =0.5, color = "darkblue", fill = cm.colors(5))+
  geom_text(aes(label=paste(Avg_Premium),vjust=-0.25)) +
  ggtitle("Impact of Listed Property  on Premium of Property") +
  xlab(label = "Listed")
Listed_Bar


## We can see that the Property which is being Listed more than 5 times is more likely to have higher Premium.



## 9) Lets check for the Premium based on  Subsidence 

Subsidence_Premium = Property_Premium %>% select(SUBSIDENCE,Premium) %>% group_by(SUBSIDENCE) %>%
  summarise(Avg_Premium = round(mean(Premium),2)) %>% arrange(desc(Avg_Premium))
View(Subsidence_Premium)
attach(Subsidence_Premium)

## lets plot for the same

Subsidence_Bar = ggplot(Subsidence_Premium) + aes(reorder(x = SUBSIDENCE,-Avg_Premium), y = Avg_Premium, fill = SUBSIDENCE) +
  geom_bar(stat ="identity",width =0.5, color = "darkblue" , fill = "lightseagreen")+
  geom_text(aes(label=paste(Avg_Premium),vjust=-0.25)) +
  ggtitle("Impact of Subsidence on Premium of Property")+
  xlab(label = "Subsidence")
Subsidence_Bar

## We can see that The Property with No Subsidence has Slightly more Premium.



## 10 ) Lets check the Premium for the Property which has Neighbour Watch

Neigh_Premium = Property_Premium %>% select(NEIGH_WATCH,Premium) %>% group_by(NEIGH_WATCH) %>%
  summarise(Avg_Premium = round(mean(Premium),2)) %>% arrange(desc(Avg_Premium))
View(Neigh_Premium)
attach(Neigh_Premium)

## lets plot for the same

Neigh_Bar = ggplot(Neigh_Premium) + aes(reorder(x = NEIGH_WATCH,-Avg_Premium), y = Avg_Premium) +
  geom_bar(stat ="identity",width =0.5, color = "darkblue", fill = "deepskyblue4")+
  geom_text(aes(label=paste(Avg_Premium),vjust=-0.25)) +
  ggtitle("Impact of Neighbours Watch  on Premium of Property") +
  xlab(label = "Neigh_Watch")
Neigh_Bar

## Having Neighbour's watch does have an Impact on Slight Impact on Premium.



## 11) Lets check the Premium for the Properties with the Building Cover.

Cover_Premium = Property_Premium %>% select(BUILDINGS_COVER,Premium) %>% group_by(BUILDINGS_COVER) %>%
  summarise(Avg_Premium = round(mean(Premium),2)) %>% arrange(desc(Avg_Premium))
View(Cover_Premium)
attach(Cover_Premium)

## lets plot for the same

Cover_Bar = ggplot(Cover_Premium) + aes(reorder(x = BUILDINGS_COVER,-Avg_Premium), y = Avg_Premium, fill = BUILDINGS_COVER) +
  geom_bar(stat ="identity",width =0.5, color = "darkblue" , fill = "brown1")+
  geom_text(aes(label=paste(Avg_Premium),vjust=-0.25)) +
  ggtitle("Impact of BUILDING COVER on Premium of Property") +
  xlab(label = "Building Cover")
Cover_Bar

## The Premium of Properties with Building Cover is Slightly Higher.



## 12) Lets check the premium for the properties with Alarms 

Alarm_Premium = Property_Premium %>% select(APPR_ALARM,Premium) %>% group_by(APPR_ALARM) %>%
  summarise(Avg_Premium = round(mean(Premium),2)) %>% arrange(desc(Avg_Premium))
View(Alarm_Premium)
attach(Alarm_Premium)

## lets plot for the same

Alarm_Bar = ggplot(Alarm_Premium) + aes(reorder(x = APPR_ALARM,-Avg_Premium), y = Avg_Premium) +
  geom_bar(stat ="identity",width =0.5, color = "darkblue", fill = "springgreen4")+
  geom_text(aes(label=paste(Avg_Premium),vjust=-0.25)) +
  ggtitle("Impact of Alarm Devices  on Premium of Property") +
  xlab(label = "APPR_Alarm")
Alarm_Bar

## We can see that Properties with Alarm Devices Installed have More Premium.



## 13)  Lets check the premium for the Properties with Locks


Lock_Premium = Property_Premium %>% select(APPR_LOCKS,Premium) %>% group_by(APPR_LOCKS) %>%
  summarise(Avg_Premium = round(mean(Premium),2)) %>% arrange(desc(Avg_Premium))
View(Alarm_Premium)
attach(Alarm_Premium)

## lets plot for the same

Lock_Bar = ggplot(Lock_Premium) + aes(reorder(x = APPR_LOCKS,-Avg_Premium), y = Avg_Premium, fill = APPR_LOCKS) +
  geom_bar(stat ="identity",width =0.5, color = "darkblue", fill = "slategray4")+
  geom_text(aes(label=paste(Avg_Premium),vjust=-0.25)) +
  ggtitle("Impact of Locks on Premium of Property") + 
  xlab(label = "APRP_Locks")
Lock_Bar

## We cam see that the Premium of Properties with Locks are much Higher.



## 14) Lets check the Premium for the Properties with Safe Installed

Safe_Premium = Property_Premium %>% select(SAFE_INSTALLED,Premium) %>% group_by(SAFE_INSTALLED) %>%
  summarise(Avg_Premium = round(mean(Premium),2)) %>% arrange(desc(Avg_Premium))
View(Safe_Premium)
attach(Safe_Premium)

## lets plot for the same

Safe_Bar = ggplot(Safe_Premium) + aes(reorder(x = SAFE_INSTALLED,-Avg_Premium), y = Avg_Premium, fill = SAFE_INSTALLED) +
  geom_bar(stat ="identity",width =0.5, color = "darkblue", fill = "turquoise4")+
  geom_text(aes(label=paste(Avg_Premium),vjust=-0.25)) +
  ggtitle("Impact of Safe Installed on Premium of Property") +
  xlab(label = "Safe_Installed")
Safe_Bar

## The premium for the properties with SAFE Installed is much Higher than of which Not Installed.

