library(gapminder)
library(dplyr)
library(ggplot2)

View(gapminder)
dim(gapminder)


# Filtra os paises por moz
# Aplica DESC na coluna gdpPercap
moz <- gapminder%>%
  filter(country=="Mozambique")%>%
  arrange( desc(gdpPercap))

# mutate, cria nova coluna com operacao de uma coluna
# ou subistitui  coluna antiga por uma nova aperacao ->  mutate(pop = pop/100)
pop_manipulation <- gapminder%>%
  mutate(new_pop = pop/100)

## Usando todas as funcoees ate aqui
gapminder_2007 <- gapminder%>%
  filter(year == 2007)%>%
  mutate(lifeExpMonths = 12*lifeExp)%>%
  arrange(desc(lifeExpMonths))

######################

#dplyr + ggpplo2

# Change to put pop on the x-axis and gdpPercap on the y-axis
# aumentar*(alargar) a escala de x por log de 10, scale_x_log10()
# Colorir vsr categorica contient,  color = continent
# Tamanho, size = var numerica, define qual var vai ser aplicada tamanhos diferentes
# faceta - divisoes do grafico em quadros menores.
ggplot(gapminder_2007, aes(x = gdpPercap, y = pop , color = continent, size = pop)) + geom_point()+scale_x_log10()+
  facet_wrap(~ continent)


# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing pop
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = pop, ulation, faceted by yearsize = pop))+geom_point()+scale_x_log10()+facet_wrap(~year)



# Sumarizando dados
 gapminder%>%
  filter(year==2017)%>%
  summarize(mean_2017 = mean(lifeExp), total_pop = sum(pop))

 # summarize
 gapminder%>%
   filter(year == 2007)%>%
   group_by(country, year)%>%
   summarize(medianLifeExp = median(lifeExp))

 
 
 ggplot(gapminder, aes(x= year, y=pop, color = continent ))+geom_point()+expand_limits(y = 0)

 
 # Summarize medianGdpPercap within each continent within each year: by_year_continent
 by_year_continent <- gapminder %>%
   group_by(continent, year) %>%
   summarize(medianGdpPercap = median(gdpPercap))
 
 # Plot the change in medianGdpPercap in each continent over time
 ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color = continent)) +
   geom_point() +
   expand_limits(y = 0)
 
 
 
 
 ggplot(gapminder, aes(x= year , y = pop , color =continent ))+
   geom_line()+
   expand_limits(y=0)+
   scale_x_log10()
 
 
 # Summarize the median gdpPercap by year & continent, save as by_year_continent
 
 by_year_continent <- gapminder%>%
   group_by(year, continent)%>%
   summarize(medianGdpPercap = median(gdpPercap))
 
 
 # Create a line plot showing the change in medianGdpPercap by continent over time
 ggplot(by_year_continent, aes(x = year ,y = medianGdpPercap , color = continent))+
   geom_line()+
   expand_limits(y = 0)
 
 
 # Summarize the median gdpPercap by continent in 1952
 by_continent <- gapminder %>%
   filter(year == 1952) %>%
   group_by(continent) %>%
   summarize(medianGdpPercap = median(gdpPercap))
 
 # Create a bar plot showing medianGdp by continent
 ggplot(by_continent, aes(x = continent, y = medianGdpPercap)) +
   geom_col()
 
 # Create  a histogram
 # binwidth - a largura representa o nr de registos, se for 5, uma barra ocupa 5 registos, no caso 5 anos
 
 gapminder_1952 <- gapminder %>%
   filter(year == 1952) %>%
   mutate(pop_by_mil = pop / 1000000)
 
 # Create a histogram of population (pop_by_mil)
 
 ggplot(gapminder_1952, aes(x =pop_by_mil))+
   geom_histogram(bins = 50) # binwidth or bins
 
 ##############################################################
 gapminder_1952 <- gapminder %>%
   filter(year == 1952)
 
 # Create a histogram of population (pop), with x on a log scale
 
 ggplot(gapminder_1952, aes(x = pop))+
   geom_histogram()+scale_x_log10()
 
 ?ggplot
 
 ggplot(gapminder_1952, aes(x =continent , y =  gdpPercap ))+geom_boxplot()+scale_y_log10()+ggtitle("Comparing GDP per capita across")