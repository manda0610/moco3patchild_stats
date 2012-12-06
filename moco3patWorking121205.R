# Work with Amanda 121112_1749

# Read in data file
moco.3pat.df=read.csv("~/Documents/moco3pat_child_hidens.csv",header=TRUE)
#attach data file
attach(moco.3pat.df)

#List variables in data
names( moco.3pat.df )
#View levels of factors
levels( Pattern )
levels( Sex )
levels( PartID )
levels( Harm )
levels( Speed )

# define channel groups
l.lat=(iCh=="hc047-Avg")|(iCh=="hc051-Avg")|(iCh=="hc052-Avg")|(iCh=="hc058-Avg")|(iCh=="hc059-Avg")|(iCh=="hc064-Avg")|(iCh=="hc068-Avg")
l.med=(iCh=="hc060-Avg")|(iCh=="hc065-Avg")|(iCh=="hc066-Avg")|(iCh=="hc067-Avg")|(iCh=="hc069-Avg")|(iCh=="hc070-Avg")|(iCh=="hc073-Avg")
med=(iCh=="hc071-Avg")|(iCh=="hc072-Avg")|(iCh=="hc074-Avg")|(iCh=="hc075-Avg")|(iCh=="hc076-Avg")|(iCh=="hc081-Avg")|(iCh=="hc082-Avg")
r.med=(iCh=="hc077-Avg")|(iCh=="hc083-Avg")|(iCh=="hc084-Avg")|(iCh=="hc085-Avg")|(iCh=="hc088-Avg")|(iCh=="hc089-Avg")|(iCh=="hc090-Avg")
r.lat=(iCh=="hc091-Avg")|(iCh=="hc092-Avg")|(iCh=="hc094-Avg")|(iCh=="hc095-Avg")|(iCh=="hc096-Avg")|(iCh=="hc097-Avg")|(iCh=="hc098-Avg")
#Amanda edits Last Updated 121118
#Change channel groups into Factors
 r.lat <- factor(r.lat)
 r.med <- factor(r.med)
 med <- factor(med)
 l.med <- factor(l.med)
 l.lat <- factor(l.lat)
 
#Check Class
class( r.lat )
class( r.med )
class( med )
class( l.lat )
class( l.med )

# Make plots 
hist( SNR )
plot (Speed)
plot (Harm)
plot (Pattern)
plot (Sex)
plot (PartID)

#Run ANOVAs
pattern.aov = aov(SNR ~ Pattern, data=moco.3pat.df)
summary(pattern.aov)

speed.aov = aov(SNR ~ Speed, data=moco.3pat.df)
summary(speed.aov)

harm.aov = aov(SNR ~ Harm, data=moco.3pat.df)
summary(harm.aov)

sex.aov = aov(SNR ~ Sex, data=moco.3pat.df)
summary(sex.aov)

TukeyHSD(pattern.aov)

TukeyHSD(speed.aov)

TukeyHSD(harm.aov)

TukeyHSD(sex.aov)

plot(TukeyHSD(pattern.aov))

plot(TukeyHSD(speed.aov))

plot(TukeyHSD(harm.aov))

plot(TukeyHSD(sex.aov))


#Use this as guide for model with F.E. and M.E. 

#my.lme = lme( Mean1F1 ~ Scalp2*Pattern2*Speed2, data=Moco.3pat.lite, random = ~1 | Subj2 )

#Load Package for Analysis
library( nlme )

#Interactions for M.E. models
m3pc1.lme= lme( SNR ~ Pattern*Speed, data=moco.3pat.df, random = ~1 | PartID )
aov(m3pc1.lme)
summary(aov(m3pc1.lme))

m3pc2.lme= lme( SNR ~ Pattern*r.lat, data=moco.3pat.df, random = ~1 | PartID )
aov(m3pc2.lme)
summary(aov(m3pc2.lme))

m3pc3.lme= lme( SNR ~ Pattern*l.lat, data=moco.3pat.df, random = ~1 | PartID )
aov(m3pc3.lme)
summary(aov(m3pc3.lme))

m3pc4.lme= lme( SNR ~ Pattern*l.med, data=moco.3pat.df, random = ~1 | PartID )
aov(m3pc4.lme)
summary(aov(m3pc4.lme))

m3pc5.lme= lme( SNR ~ Pattern*r.med, data=moco.3pat.df, random = ~1 | PartID )
aov(m3pc5.lme)
summary(aov(m3pc5.lme))

m3pc6.lme= lme( SNR ~ Pattern*med, data=moco.3pat.df, random = ~1 | PartID )
aov(m3pc6.lme)
summary(aov(m3pc6.lme))

m3pc7.lme= lme( SNR ~ Pattern*Harm, data=moco.3pat.df, random = ~1 | PartID )
aov(m3pc7.lme)
summary(aov(m3pc7.lme))

m3pc8.lme= lme( SNR ~ Speed*Harm, data=moco.3pat.df, random = ~1 | PartID )
aov(m3pc8.lme)
summary(aov(m3pc8.lme))

m3pc9.lme= lme( SNR ~ Speed*r.lat, data=moco.3pat.df, random = ~1 | PartID )
aov(m3pc9.lme)
summary(aov(m3pc9.lme))

m3pc10.lme= lme( SNR ~ Speed*l.lat, data=moco.3pat.df, random = ~1 | PartID )
aov(m3pc10.lme)
summary(aov(m3pc10.lme))

m3pc11.lme= lme( SNR ~ Speed*l.med, data=moco.3pat.df, random = ~1 | PartID )
aov(m3pc11.lme)
summary(aov(m3pc11.lme))

m3pc12.lme= lme( SNR ~ Speed*r.med, data=moco.3pat.df, random = ~1 | PartID )
aov(m3pc12.lme)
summary(aov(m3pc12.lme))

m3pc13.lme= lme( SNR ~ Speed*med, data=moco.3pat.df, random = ~1 | PartID )
aov(m3pc13.lme)
summary(aov(m3pc13.lme))

m3pc14.lme= lme( SNR ~ Pattern*Speed*Harm, data=moco.3pat.df, random = ~1 | PartID )
aov(m3pc14.lme)
summary(aov(m3pc14.lme))


