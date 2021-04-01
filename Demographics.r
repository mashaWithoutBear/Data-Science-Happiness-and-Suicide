                library(tidyverse)
                library(sjmisc)
                library(ggplot2)
                library(hrbrthemes)
                library(scales)
                library(xtable)
                library(viridis)
                
                #population dataset: absolute # of people per age, gender, location
                popDF<-read.csv("../Data/POP_2015_2017.CSV")
                popDF2017<-popDF%>% filter(year_id=='2017', sex_name!='Both')
       
        #number of suicides per age, gender, location
        suicides<-read.csv("../Data/2017_suicide_rates_numbers.csv")
        suicides<- filter(suicides, metric_name=='Number')
        unique(suicides$sex_name)
        
        #join suicide, population, depression
        dfJoined<-inner_join(suicides, popDF2017, by=c("location_name"="location_name","age_name"="age_group_name","sex_name"="sex_name"))
       
       #add north America to the regions
        dfJoined<-mutate (dfJoined,location_name=ifelse(location_name=='Canada','North America - WB',location_name))
        dfJoined<-mutate (dfJoined,location_name=ifelse(location_name=='United States','North America - WB',location_name))
        dfJoined<-mutate (dfJoined,location_name=ifelse(location_name=='Greenland','North America - WB',location_name))
       
          #prepare for different views
        dfHelper<- dfJoined %>% 
          mutate(Region=ifelse(str_detect(location_name, 'WB'),'1','0'))%>% # we chose so called World Bank ('WB') regions
          #create own age categories
          mutate(Age_adjusted=ifelse(age_name=='<20 years'|age_name=='20 to 24','<25', age_name))%>%
          mutate(Age_adjusted=ifelse((age_name=='25 to 29'|age_name=='30 to 34'|age_name=='35 to 39'|age_name=='40 to 44'|age_name=='45 to 49'),'25 to 49',Age_adjusted))%>%
          mutate(Age_adjusted=ifelse((age_name=='50 to 54'|age_name=='55 to 59'|age_name=='60 to 64'|age_name=='65 to 69'|age_name=='70 to 74'|age_name=='75 to 79'|age_name=='80 plus'),'50+',Age_adjusted))
        
        #check results
        unique(dfJoined$age_name)
        unique(dfHelper$Age_adjusted)
        unique(dfHelper$Region)
        
        #use this df as basis for differnt views
        df<- dfHelper %>% 
          filter(Age_adjusted=='50+'|Age_adjusted=='25 to 49'|Age_adjusted=='<25'|Age_adjusted=='All Ages') %>% 
          filter(Region=='1')
        #rename Regions nicely for the plot and organize them
        df<-mutate(df,RegionPlot=str_wrap(gsub("- W.*","",location_name),width = 8))
        df<-mutate(df,RegionPlot = fct_relevel(RegionPlot,"Europe &\nCentral\nAsia","North\nAmerica","East\nAsia &\nPacific","Latin\nAmerica\n&\nCaribbean","Middle\nEast &\nNorth\nAfrica", "South\nAsia","Sub-\nSaharan\nAfrica"))
        
        #region/age view
        dfRegionRenamedAgeFiltered<-group_by(df,RegionPlot,Age_adjusted)
        summary<-summarise(dfRegionRenamedAgeFiltered,TotalSuicides=as.integer(sum(val.x)), TotalPop=sum(val.y))
        summary<-mutate(summary, SuicideRate=round((TotalSuicides*100000/TotalPop),10))
        summary

          #plot suicide rates per region/age
          jpeg(file="../Plots/SR_per_age_region2017_new_age_groups.jpeg")
          ggplot(summary) + 
            aes(fill= Age_adjusted, y=SuicideRate, x=RegionPlot )+
            geom_bar(stat="identity", position="dodge") + #position="dodge" für Bars nebeneinander
            scale_fill_viridis(discrete = T) +
            #reverse = TRUE
            ggtitle("Suicide rates per age group and region,2017") +
            xlab("Region")+
            ylab("Suicides/100K pop")+
            theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=11), text= element_text(size= 17))+
            labs(fill = "Age group")+
            guides(fill=guide_legend(reverse = TRUE))
          dev.off()
          
          #population structure
          summary_wo_all_ages<- filter(summary, Age_adjusted!="All Ages")
          summary_wo_all_ages<-mutate(summary_wo_all_ages,Age_adjusted = fct_relevel(Age_adjusted,"50+","25 to 49","<25"))
          
          #plot population structure per region/age
          jpeg(file="../Plots/population_structure2017.jpeg")
          ggplot(summary_wo_all_ages)+
            aes(fill= Age_adjusted, y=TotalPop, x=RegionPlot) + 
            geom_bar(stat="identity",position="fill",)+
            scale_fill_viridis_d(direction = -1,end = 0.7) +
            ggtitle("Population structure per region,2017") +
            xlab("Region")+
            ylab("% of pop in age group")+
            theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=11), text= element_text(size= 17))+
            labs(fill = "Age group")+
            scale_y_continuous(labels = scales::percent_format(accuracy = 1))
         dev.off()
         
         #age group and gender
         summaryAgeGender<-group_by(df,sex_name, Age_adjusted)
         summaryAgeGender<-summarise(summaryAgeGender,TotalSuicides=as.integer(sum(val.x)), TotalPop=sum(val.y))
         summaryAgeGender<-mutate(summaryAgeGender, SuicideRate=round((TotalSuicides*100000/TotalPop),10))
         summaryAgeGender
         
         
         #plot suicide rates per sex/age
         jpeg(file="../Plots/SR_per_sex_age2017_new_age_groups.jpeg")
         ggplot(summaryAgeGender) + 
           aes(fill= Age_adjusted, y=SuicideRate, x=sex_name )+
           geom_bar(stat="identity", position="dodge") +
           scale_fill_viridis(discrete = T) +
           #reverse = TRUE
           ggtitle("Suicide rates per age group and gender,2017") +
           xlab("Gender")+
           ylab("Suicides/100K pop")+
           theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=11), text= element_text(size= 17))+
           labs(fill = "Age group")+
           guides(fill=guide_legend(reverse = TRUE))
         dev.off()
          
         #gender and region
         dfWOAllAges<-filter(df, Age_adjusted!="All Ages")
         summaryAgeRegion<-group_by(dfWOAllAges,sex_name, RegionPlot)
         summaryAgeRegion<-summarise(summaryAgeRegion,TotalSuicides=as.integer(sum(val.x)), TotalPop=sum(val.y))
         summaryAgeRegion<-mutate(summaryAgeRegion, SuicideRate=round((TotalSuicides*100000/TotalPop),10))
         summaryAgeRegion
        
         
         #plot suicide rates per sex/region
         jpeg(file="../Plots/SR_per_sex_region2017_new_age_groups.jpeg")
         ggplot(summaryAgeRegion) + 
           aes(fill= sex_name, y=SuicideRate, x=RegionPlot )+
           geom_bar(stat="identity", position="dodge") +
           scale_fill_viridis(discrete = T) +
           #reverse = TRUE
           ggtitle("Suicide rates per gender and region,2017") +
           xlab("Region")+
           ylab("Suicides/100K pop")+
           theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=11), text= element_text(size= 17))+
           labs(fill = "Gender")+
           guides(fill=guide_legend(reverse = TRUE))
         dev.off()
  