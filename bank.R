
```{r read_data}
bankruptcy <- readRDS(here::here("data/Bankruptcy.rds"))
```

```{r}
library(tidyverse)
library(naniar)
```


```{r}
gg_miss_upset(bankruptcy)
gg_miss_var(bankruptcy, show_pct = TRUE)

miss_var_summary(bankruptcy)

missing_vars <- miss_var_summary(bankruptcy) %>%
  filter(pct_miss > 0) %>%
  pull(variable)
```


```{r}
bankruptcy %>%
  ggplot(aes(x = Employees,
             y = EmplUnion)) +
  geom_miss_point()  +
  facet_wrap(~DENYOther)
```


```{r}
bankruptcy %>%
  ggplot(aes(x = Employees, fill = DENYOther)) +
  geom_boxplot()  +
  coord_flip()

```

```{r}
library(psych)

corPlot(bank, cex = 0.6)
```


# Impute with regression on number of employees

```{r}
bankruptcy %>%
  ggplot(aes(x = Assets,  # we have all info in assets
             y = Liab)) +
  geom_miss_point()
```

### Impute liab with mean

```{r}
bankruptcy %>%
  ggplot(aes(x = Assets,  # we have all info in assets
             y = Ebit)) +
  geom_miss_point()
```



```{r}
impute_mean(bankruptcy$Assets)

```




```{r}

library(simputation)

bank_imp <- bankruptcy %>%
  bind_shadow() %>%
  mutate(EmplUnion = as.double(EmplUnion)) %>%
  impute_lm(EmplUnion ~ (Employees + Sales + DENYOther) ) %>%
  add_label_shadow()


ggplot(bank_imp,
       aes(x = Employees,
           y = EmplUnion,
           color = any_missing)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")
```






```{r}
library('fastDummies')

bankruptcy <- dummy_cols(bankruptcy, select_columns = 'CityFiled')
bankruptcy <- dummy_cols(bankruptcy, select_columns = 'DENYOther')
bankruptcy <- dummy_cols(bankruptcy, select_columns = 'HeadStAtFiling')
bankruptcy <- dummy_cols(bankruptcy, select_columns = 'SICMajGroup')
bankruptcy <- dummy_cols(bankruptcy, select_columns = 'FirmEnd')


bank_imp %>%
  select_if(is.numeric)%>% #Only metric data
  scale%>% #Standardise
  dist->delta #Distance



bank_imp %>%
  pull(Name)%>% #Get beer names
  abbreviate()-> #Abbreviate
  attributes(delta)$Labels


mdsout<-cmdscale(delta,eig=TRUE)
str(mdsout$GOF)

str(mdsout$eig)

mdsout$points%>%
  as.data.frame()%>%
  rownames_to_column(var = 'Bank Name')->df

ggplot(df,aes(x=V1,y=V2,label=`Bank Name`))+ geom_text(size=2)





```


```{r}
df<-add_column(df,Manufacturer=bankruptcy$DENYOther)

ggplot(df,aes(x=V1,y=V2,col=Manufacturer,label=`Bank Name`))+
  geom_text(size=2)
```




