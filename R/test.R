

a = adpc  %>%  select (contains(names(adsl)  ) )

ad <- adpc %>% mutate(ASUBJID=SUBJID) %>% select(-names(a) )    

adsl <- adsl %>% 
        mutate(ASUBJID=SUBJID)

analysis_data <- ad %>% 
                inner_join(adsl,by='ASUBJID')
            
dat <- adpc %>% 
       filter(AVAL >= 0) %>% 
       group_by(PARAM,TRT01A,VISITNUM,ATPT) %>% 
      summarise(
        mean=mean(AVAL),
        median=mean(AVAL),
        min=min(AVAL),
        max=max(AVAL),
        sd=sd(AVAL),
        lclm=mean(AVAL)-(1.96*sd(AVAL)/sqrt(length(AVAL))) ,
        Uclm=mean(AVAL)+(1.96*sd(AVAL)/sqrt(length(AVAL))) 
      ) %>% 
      ungroup()
        



dt <- adpc %>% 
      inner_join(adsl,by='USUBJID')