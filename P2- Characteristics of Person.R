##############################################################
# Characteristics of the Customers who are Likely to Default #
##############################################################

## Lets find characteristics of customers who are likelier to default on their payments.


## Step 1 :- Creating a Subset with features that will help in the analysis

Person_Policy = subset(cleaned, select = c(P1_SEX,P1_EMP_STATUS,P1_DOB,P1_MAR_STATUS,P1_POLICY_REFUSED,LAST_ANN_PREM_GROSS,PAYMENT_FREQUENCY,POL_STATUS)) %>% rename(Premium=LAST_ANN_PREM_GROSS)
View(Person_Policy)
attach(Person_Policy)


## Lets filter the the policy status as Live and payment frequency as 0

Policy_fil = Person_Policy %>% filter(POL_STATUS == "Live" & PAYMENT_FREQUENCY == 0 )
View(Policy_fil)



## 1) :- Lets apply this filter on gender column to know which gender is more likely to default

Gender_Def <- Policy_fil %>% group_by(P1_SEX) %>% summarise(Total_Defaults = n()) %>%
  mutate(Gen_Percentage = Total_Defaults/sum(Total_Defaults)*100) %>% arrange(desc(Total_Defaults))
View(Gender_Def)
attach(Gender_Def)

# Plottting the Gender_ Defaulters Bar

Gen_Bar <- ggplot(Gender_Def) + aes(x = P1_SEX, y = Gen_Percentage, fill = P1_SEX) + geom_bar(stat = 'Identity',width = 0.2, col = cm.colors(3))+
  geom_text(aes(label = paste(round(Gen_Percentage,2)),vjust = -0.25)) +
  ggtitle("Genderwise Policy Payment Defaulters ")
Gen_Bar  



## 2) :- Lets apply this filter on Employment status column

Emp_Def  = Policy_fil %>% group_by(P1_EMP_STATUS) %>% summarise(Defaults = n()) %>%
  mutate(Emp_Percentage = Defaults/sum(Defaults)*100) %>% arrange(desc(Emp_Percentage))
View(Emp_Def) 
attach(Emp_Def)
# Plotting the EMployee Defaulters Bar
Emp_Bar = ggplot(Emp_Def) + aes(reorder(x = P1_EMP_STATUS,-Emp_Percentage), y = Emp_Percentage, fill = P1_EMP_STATUS) + geom_bar(stat = 'Identity',width = 0.4, col = terrain.colors(11))+
  geom_text(aes(label = paste(round(Emp_Percentage,2)),vjust = -0.25)) +
  ggtitle("Employment Statuswise Policy Payment Defaulters ")+
  xlab(label = "Employment")
Emp_Bar



## 3) :- Lets put the Policy_fil on Mar_Status 
Mar_Def = Policy_fil %>% group_by(P1_MAR_STATUS) %>% summarise(Defaults = n()) %>%
  mutate(Mar_Percentage = Defaults/sum(Defaults)*100) %>% arrange(desc(Mar_Percentage))
View(Mar_Def) 
attach(Mar_Def)

## Lets plot for Mar_Def

Mar_Bar = ggplot(Mar_Def) + aes(reorder(x = P1_MAR_STATUS,-Mar_Percentage), y = Mar_Percentage, fill = P1_MAR_STATUS) + geom_bar(stat = 'Identity',width = 0.4, col = topo.colors(10))+
  geom_text(aes(label = paste(round(Mar_Percentage,2)),vjust = -0.25)) +
  ggtitle("Marital Statuswise Policy Payment Defaulters ") +
  xlab(label = "Marital Status")
Mar_Bar



## 4) Lets Find out the Non_Payment of Premium 

Non_Payment = Person_Policy %>% filter(PAYMENT_FREQUENCY==0) %>%
  select(Premium) %>% 
  ggplot(aes(x=Premium))+
  scale_x_continuous(limits=c(0,1000))+
  geom_density(color="royalblue4",fill="royalblue2")+
  ggtitle("Non Payment of Premium")+
  theme(plot.title=element_text(hjust = 0.4),
        axis.title.y =element_blank())
Non_Payment



## 5) Lets create the Df where the Policy status is not Live and Payment is Frequent

Policy_fil2 <- Person_Policy %>% filter(POL_STATUS !="Live", PAYMENT_FREQUENCY == 1)
View(Policy_fil2)



## 6) Lets see customers who are employed and Married and thier Policy is Cancelled/Lapsed 

Mar_Can = Policy_fil2 %>% group_by(P1_MAR_STATUS,P1_EMP_STATUS) %>% filter(P1_EMP_STATUS == 'E' & P1_MAR_STATUS == 'M') %>% summarise(Default = n()) %>% mutate(Emp_Mar_Per = Default/sum(Default)*100)
View(Mar_Can)  



## 7)  Lets see Employed Customers who are likely to default Genderwise.

Emp_Gen = Policy_fil2%>% group_by(P1_EMP_STATUS,P1_SEX)%>% filter(P1_EMP_STATUS == "E")%>%
  summarise(Defaults = n())%>% mutate(EMP_Gen_Per = Defaults/sum(Defaults)*100)

Emp_Gen

# lets plot the bar for the same 

Emp_Gen_Bar = ggplot(Emp_Gen) + aes(reorder(x=P1_SEX,-EMP_Gen_Per),y = EMP_Gen_Per, fill = P1_SEX)+ geom_bar(stat = 'Identity',width = 0.4)+
  geom_text(aes(label = paste(round(EMP_Gen_Per,2)),vjust = -0.25)) +
  ggtitle("Employed Defaulters Genderwise ") + 
  xlab(label = "SEX")
Emp_Gen_Bar

