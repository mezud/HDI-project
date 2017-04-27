install.packages('cluster')
install.packages('ggplot2')
install.packages('plotly')
install.packages('varhandle')
installed.packages('googleVis')



library(cluster)
library(ggplot2)
library(plotly)
library(varhandle)
library(googleVis)

df1<-read.csv('historical_index.csv')
# factor conversion
df1$Human.Development.Index..1990.<-unfactor(df1$Human.Development.Index..1990.)
df1$Human.Development.Index..2000.<-unfactor(df1$Human.Development.Index..2000.)


df1$year<-"1990"
names(df1)[3]<-"HDI"

df1<-df1[1:188,]
two<-df1[1:188,4]
three<-df1[1:188,5]
four<-df1[1:188,6]
five<-df1[1:188,7]
six<-df1[1:188,8]
seven<-df1[1:188,9]
df1[189:376,]<-0
df1[189:376,3]<-two[1:188]
df1[377:564,3]<-three
df1[565:752,3]<-four
df1[753:940,3]<-five
df1[941:1128,3]<-six
df1[1129:1316,3]<-seven

# filling country
ctr<-df1[1:188,2]
df1[189:376,2]<-ctr
df1[377:564,2]<-ctr
df1[565:752,2]<-ctr
df1[753:940,2]<-ctr
df1[941:1128,2]<-ctr
df1[1129:1316,2]<-ctr
# filling rank
r<-df1[1:188,1]
df1[189:376,1]<-r
df1[377:564,1]<-r
df1[565:752,1]<-r
df1[753:940,1]<-r
df1[941:1128,1]<-r
df1[1129:1316,1]<-r

# filling year
df1[189:376,10]<-"2000"
df1[377:564,10]<-"2010"
df1[565:752,10]<-"2011"
df1[753:940,10]<-"2012"
df1[941:1128,10]<-"2013"
df1[1129:1316,10]<-"2014"


df2<-df1[,c(1,10,2,3)]
df2$year<-as.numeric(df2$year)
df2$HDI<-as.numeric(df2$HDI)
df2$HDI.Rank<-as.numeric(df2$HDI.Rank)



##regions 
eur<-c("Albania","Gibraltar","Norway","Andorra","Greece","Poland","Armenia","Greenland","Portugal","Austria","Hungary","Romania","Azerbaijan","Iceland","Russian Federation","Belarus","Ireland","San Marino","Belgium","Isle of Man	Serbia","Bosnia and Herzegovina","Italy","Slovak Republic","Bulgaria","Kazakhstan","Slovenia","Channel Islands","Kosovo","Spain","Croatia","Kyrgyz Republic","Sweden","Cyprus","Latvia","Switzerland","Czech Republic","Liechtenstein","Tajikistan","Denmark","Lithuania","Turkey","Estonia","Luxembourg","Turkmenistan","Faroe Islands","Macedonia","FYR","Ukraine","Finland","Moldova","United Kingdom","France","Monaco",	"Uzbekistan","Georgia","Montenegro","Germany","Netherlands","Slovakia","Serbia")
eurf<-c("Norway","Sweden","Netharlands")

as<-c("American Samoa","Korea (Republic of)","Philippines","Australia","Lao","PDR","Samoa","Brunei","Darussalam","Macao SAR","China","Singapore","Cambodia","Malaysia","Solomon Islands","China	Marshall Islands","Taiwan","China","Fiji","Micronesia","Fed. Sts.","Thailand","French Polynesia","Mongolia","Timor-Leste Guam","Myanmar","Papua New Guinea","Hong Kong SAR","China Nauru	Tonga","Indonesia","New Caledonia	Tuvalu","Japan","New Zealand","Vanuatu","Kiribat","Northern Mariana Islands","Vietnam","Korea, Dem. People's Rep.","Palau","Hong Kong, China (SAR)","Viet Nam","Micronesia (Federated States of)","Kiribati")

caribb<-c("Antigua and Barbuda","Curacao","Paraguay","Argentina","Dominica","Peru","Aruba","Dominican Republic","Puerto Rico","Bahamas","Ecuador","Sint Maarten (Dutch part)","Barbados",	"El Salvador","St. Kitts and Nevis","Belize","Grenada","St. Lucia","Bolivia","Guatemala","St.Martin (French part)","Brazil","Guyana","St. Vincent and the Grenadines","British Virgin Islands","Haiti","Suriname","Cayman Islands","Honduras","Trinidad and Tobago","Chile","Jamaica","Turks and Caicos Islands","Colombia","Mexico","Uruguay","Costa Rica","Nicaragua","Venezuela","Cuba","Panama","Virgin Islands (U.S.)","Venezuela (Bolivarian Republic of)","Saint Kitts and Nevis","The former Yugoslav Republic of Macedonia","Saint Lucia","Saint Vincent and the Grenadines","Moldova (Republic of)","Bolivia (Plurinational State of)","Kyrgyzstan","Timor-Leste","Lao People's Democratic Republic","Sao Tome and Principe")

me<-c("Algeria","Jordan","Qatar","Bahrain","Kuwait","Saudi Arabia","Djibouti","Lebanon","Syrian Arab Republic","Egypt, Arab Rep.","Libya","Tunisia","IranIslamic Rep.","Malta","United Arab Emirates","Iraq","Morocco","West Bank and Gaza","Israel","Oman","Yemen, Rep.","Iran (Islamic Republic of)","Egypt","Palestine, State of")

na<-c("Bermuda","Canada","United States")
sa<-c("Afghanistan","India","Pakistan","Bangladesh","Maldives","Sri Lanka","Bhutan","Nepal")	

afr<-c("Angola","Gabon","Nigeria","Benin","Gambia, The","Rwanda","Botswana","Ghana","São Tomé and Principe","Burkina Faso","Guinea","Senegal","Burundi","Guinea-Bissau","Seychelles","Cabo Verde","Kenya","Sierra Leone","Cameroon","Lesotho","Somalia","Central African Republic","Liberia","South Africa","Chad","Madagascar","South Sudan","Comoros","Malawi","Sudan","Congo Dem. Rep.","Mali","Swaziland","Congo, Rep","Mauritania","Tanzania","Côte d'Ivoire","Mauritius",	"Togo","Equatorial Guinea","Mozambique","Uganda","Eritrea","Namibia","Zambia","Ethiopia","Niger","Zimbabwe","Brunei Darussalam","Tonga","Congo","Tanzania (United Republic of)","Yemen","Gambia","Congo (Democratic Republic of the)")

reg<-function(x){
  if (x %in% eur){
    paste('Europe and Central Asia')
  } else if (x %in% as) {
    paste('East Asia and Pacific')
  } else if (x %in% caribb) {
    paste('Latin America and Caribbean')
  } else if (x %in% me) {
    paste('Middle East and North Africa')
  } else if (x %in% na) {
    paste('North America')
  } else if (x %in% sa) {
    paste('South Asia')
  } else if (x %in% afr) {
    paste('Sub Saharan Africa')
  } 
}

df2$Region<-lapply(df2$Country, reg)


## plot 1: HDI and HDI rank for all the countries also showing regions
df3<-df2[,1:5]
df3<-na.omit(df3)
df4 <- data.frame(lapply(df3, as.character), stringsAsFactors=FALSE)

df4$year<-as.numeric(df4$year)
df4$HDI.Rank<-as.numeric(df4$HDI.Rank)
df4$HDI<-as.numeric(df4$HDI)
df4$Region<-as.factor(df4$Region)
plot1=gvisMotionChart(df4, 
                      idvar="Country", 
                      timevar="year")
plot(plot1)
cat(plot1$html$chart, file="1.html")

plotly_POST(plot1, filename = "plot1")
### showing regions average

m<-function(x){
  mean(x,na.rm = TRUE)
}
df5<-aggregate(df4, list(r = df4$Region,year=df4$year), m)

df5<-df5[,c(1,2,3,6)]
df5<-df5[-c(6,14,22,30,38,46,54),]

df5$year<-as.numeric(df5$year)
plot2=gvisMotionChart(df5, 
                      idvar="r", 
                      timevar="year")
plot(plot2)

cat(plot2$html$chart, file="2.html")
### Filtering out specific group
africa<-df4[df4$Region=='Middle East and North Africa',]

eur<-df4[df4$Region=='Europe and Central Asia',]

plot3=gvisMotionChart(eur, 
                      idvar="Country", 
                      timevar="year")
plot(plot3)

cat(plot3$html$chart, file="3.html")
###### Classification in 
cl<-df4[,c(1,4)]
cl <- na.omit(cl)
cl<-cl[cl$HDI!='NA',]
clusters <- hclust(dist(df3[, c(1,4)]))
plot(clusters)

k.means.fit <- kmeans(cl, 1)
clusplot(cl, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

## cluster dendogram
d <- dist(cl, method = "euclidean")
H.fit <- hclust(d, method="ward")
plot(H.fit)
groups <- cutree(H.fit, k=3)
rect.hclust(H.fit, k=3, border="red") 
cl$r<-df3$r

plot4<-ggplot(cl, aes(cl$HDI.Rank,cl$HDI))+geom_point()+stat_ellipse()
ggplotly(plot4)
plotly_POST(plot4, filename = "Cluster")
class(df3$r)

cls <- kmeans(cl[, 1:2], 2, nstart = 20)
cl$cluster=factor(cls$cluster)
centers=as.data.frame(cls$centers)
p<-ggplot(data=cl, aes(x=HDI.Rank, y=HDI, color=cluster))+geom_point()
fp <- ggplotly(p)
fp
plotly_POST(fp, filename = "Cluster")
############### gender development
gh<-read.csv('https://github.com/mezud/data1/blob/master/historical_index.csv')
g<-read.csv('gender_development.csv')
g<-g[1:188,]
g$r<-lapply(g$Country,reg)
gg<-data.frame(lapply(g, as.character), stringsAsFactors=FALSE)
gg$Mean.Years.of.Education..Male.<-as.numeric(gg$Mean.Years.of.Education..Male.)
gg$Human.Development.Index..Male.<-as.numeric(gg$Human.Development.Index..Male.)
gg$Estimated.Gross.National.Income.per.Capita..Male.<-as.numeric(gg$Estimated.Gross.National.Income.per.Capita..Male.)
gg$Estimated.Gross.National.Income.per.Capita..Female.<-as.numeric(gg$Estimated.Gross.National.Income.per.Capita..Female.)
gg<-na.omit(gg)
# relationship between male years of education, HDI and income
male_relation<-ggplot(data=gg, aes(x=gg$Mean.Years.of.Education..Male., y=gg$Human.Development.Index..Male.,label=gg$Country))+geom_point(aes(size = gg$Estimated.Gross.National.Income.per.Capita..Male.))+geom_smooth()

plot5<-ggplotly(male_relation)
plot5
plotly_POST(plot5, filename = "plot5")

male_relation_r<-ggplot(data=gg, aes(x=gg$Mean.Years.of.Education..Male., y=gg$Human.Development.Index..Male.,label=gg$Country,size=gg$Estimated.Gross.National.Income.per.Capita..Male.))+geom_point(aes(color=gg$r))+geom_smooth()

plot6<-ggplotly(male_relation_r)
plot6
plotly_POST(plot6, filename = "Male Education_HDI_Income")
gg$Human.Development.Index..Female.<-as.numeric(gg$Human.Development.Index..Female.)

gg$mhdi<-(gg$Human.Development.Index..Female.+gg$Human.Development.Index..Male.)/2
gg$mi<-(gg$Estimated.Gross.National.Income.per.Capita..Female.+gg$Estimated.Gross.National.Income.per.Capita..Male.)/2

hdi_top<-gg[,c(2,15)]
inc_top<-gg[,c(2,16)]

# relationship between female years of education, HDI and income
fem_df<-gg[,c(2,4,10,12,14)]
names(fem_df)[2]<-"HDIfemale"
names(fem_df)[3]<-"education"
names(fem_df)[4]<-"GNIpercapita"
fem_df$HDIfemale<-as.numeric(fem_df$HDIfemale)
fem_df$education<-as.numeric(fem_df$education)
female_rel<-ggplot(data=fem_df, aes(x=education, y=HDIfemale,label=Country))+geom_point(aes(size = GNIpercapita))+geom_smooth()

pl7<-ggplotly(female_rel)
pl7
plotly_POST(pl7, filename = "plot7")
female_rel_r<-ggplot(data=fem_df, aes(x=education, y=HDIfemale,label=Country,size=GNIpercapita))+geom_point(aes(color=fem_df$r))

pl8<-ggplotly(female_rel_r)
pl8
plotly_POST(pl8, filename = "female_edu_hdi_income")
## gender inequality
i<-read.csv('gender_inequality.csv')
i<-i[1:188,]
i$r<-lapply(i$Country,reg)
ii<-data.frame(lapply(i, as.character), stringsAsFactors=FALSE)

ii$Labour.Force.Participation.Rate..Male.<-as.numeric(ii$Labour.Force.Participation.Rate..Male.)
ii$Gender.Inequality.Index..GII.<-as.numeric(ii$Gender.Inequality.Index..GII.)
ii$Population.with.Secondary.Education..Male.<-as.numeric(ii$Population.with.Secondary.Education..Male.)
ii$Labour.Force.Participation.Rate..Female.<-as.numeric(ii$Labour.Force.Participation.Rate..Female.)

ii$Population.with.Secondary.Education..Female.<-as.numeric(ii$Population.with.Secondary.Education..Female.)
ii<-na.omit(ii)

male_part<-ggplot(data=ii, aes(x=ii$Labour.Force.Participation.Rate..Male., y=ii$Gender.Inequality.Index..GII.,label=ii$Country,size=gg$Estimated.Gross.National.Income.per.Capita..Male.))+geom_point(aes(color=r))+geom_smooth()

pl9<-ggplotly(male_part)
pl9
plotly_POST(pl9, filename = "GDI_laborforce_male")
female_part<-ggplot(data=ii, aes(x=ii$Labour.Force.Participation.Rate..Female., y=ii$Gender.Inequality.Index..GII.,label=ii$Country))+geom_point(aes(size = ii$Population.with.Secondary.Education..Female.))+geom_smooth()+geom_point(aes(color = r))

female_part<-ggplot(data=ii, aes(x=ii$Labour.Force.Participation.Rate..Female., y=ii$Gender.Inequality.Index..GII.,label=ii$Country,size = gg$Estimated.Gross.National.Income.per.Capita..Female.))+geom_point(aes(color = r))+geom_smooth()
pl10<-ggplotly(female_part)
pl10
plotly_POST(pl10, filename = "GDI_laborforce_female")
htmlwidgets::saveWidget(as.widget(pl10), "graph.html")


Sys.setenv("plotly_username"="mezbah")
Sys.setenv("plotly_api_key"="XaDRLOy9AJZPQzoXMuen")
plotly_POST(pl10, filename = "plot1")
male_part_r<-ggplot(data=i, aes(x=i$Labour.Force.Participation.Rate..Male., y=i$Gender.Inequality.Index..GII.,label=i$Country))+geom_point(aes(color=r))