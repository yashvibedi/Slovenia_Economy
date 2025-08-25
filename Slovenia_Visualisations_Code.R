library(dplyr)
library(patchwork)
library(ggplot2)

# The first two visualisations - f1, f2 - illustrate changes in population in Slovenia between 1990 and 2022.

population <- read.csv("population.csv") %>%
  select("TIME_PERIOD", "OBS_VALUE") %>%
  rename(year = TIME_PERIOD,
         pop = OBS_VALUE) %>%
  filter(year >= 1990)

emigration <- read.csv("emigration.csv") %>%
  select("TIME_PERIOD", "OBS_VALUE") %>%
  rename(year = TIME_PERIOD,
         emigrants = OBS_VALUE)

immigration <- read.csv("immigration.csv") %>%
  select("TIME_PERIOD", "OBS_VALUE")%>%
  rename(year = TIME_PERIOD,
         immigrants = OBS_VALUE)

df <- full_join(immigration, emigration, by = "year")
df <- full_join(df, population, by = "year")
df <- na.omit(df)

df$difference <- df$immigrants - df$emigrants

f1 <- ggplot(data = population, mapping = aes(x = year, y = pop)) +
  geom_line() +
  labs(x = "Year",
       y = "Population",
       title = "Population of Slovenia from 1990-2022",
       caption = "Source: Eurostat(demo_pjan)") +
  theme_minimal()+
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"),  
    axis.title.x = element_text(family = "Arial", face = "plain", size = 12),  
    axis.title.y = element_text(family = "Arial", face = "plain", size = 12),  
    axis.text = element_text(family = "Arial", size = 12),  
    plot.caption = element_text(family = "sans", size = 10, color = "gray")  
  )
print(f1)

imm_em <- ggplot(df, aes(x = year)) +
  geom_line(aes(y = immigrants, col = "Immigrants")) +
  geom_line(aes(y = emigrants, col = "Emigrants")) +
  labs(x = "",
       y = "Value",
       title = "Immigration and Emigration in Slovenia 1990-2022",
       colour = "Legend") +
  scale_color_manual(values = c("Immigrants" = "blue", "Emigrants" = "pink")) +
  theme_minimal()+
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"),  
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14), 
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14), 
    axis.text = element_text(family = "Arial", size = 12),  
    plot.caption = element_text(family = "sans", size = 10, color = "gray"),
    legend.text = element_text(family = "sans", size = 10)) 
  

net_imm <- ggplot(df, aes(x = year, y = difference)) +
  geom_line() +
  labs(x = "Year",
       y = "Net Immigration",
       Title = "Net Immigration in Slovenia",
       caption = "Source: Eurostat(migr_imm8)") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "purple"),  
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14),  
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14),  
    axis.text = element_text(family = "Arial", size = 12),  
    plot.caption = element_text(family = "sans", size = 10, color = "gray"),
    legend.text = element_text(family = "sans", size = 10)) 

f2 <- imm_em / net_imm
print(f2)

# The next two visualisations - f3, f4 - depict how unemployment varies with sex in Slovenia.

unemp <- read.csv("unemp", header = TRUE) %>%
  select("sex", "TIME_PERIOD", "OBS_VALUE") %>%
  rename(year = TIME_PERIOD,
         unemployed = OBS_VALUE) %>%
  group_by(year) %>%
  summarise(
    f_unemp = sum(unemployed[sex == "Females"]),
    m_unemp = sum(unemployed[sex == "Males"]),
    t_unemp = sum(unemployed[sex == "Total"])
  )

poverty <- read.csv("poverty", header = TRUE) %>%
  filter(unit != "Thousand persons") %>%
  select("sex", "TIME_PERIOD", "OBS_VALUE") %>%
  rename(year = TIME_PERIOD,
         poor = OBS_VALUE) %>%
  group_by(year) %>%
  summarise(
    f_poor = sum(poor[sex == "Females"]),
    m_poor = sum(poor[sex == "Males"]),
    t_poor = sum(poor[sex == "Total"])
  )
df2 <- full_join(unemp, poverty, by = "year")
df2 <- na.omit(df2)

f3 <- ggplot(df2, aes(x = year)) +
  geom_line(aes(y = f_unemp, col = "Female")) +
  geom_line(aes(y = m_unemp, col = "Male")) +
  geom_line(aes(y = t_unemp, col = "Total")) +
  labs(x = "Year",
       y = "Unemployment",
       title = "Unemployment (%) in Slovenia Based on Sex(2009-2023)",
       col = "Legend",
       caption = "Source:Eurostat (une_rt_a)") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "pink", "Total" = "orange")) +
  theme_minimal()+
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"), 
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14), 
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14), 
    axis.text = element_text(family = "Arial", size = 12), 
    plot.caption = element_text(family = "sans", size = 10, color = "gray"),
    legend.text = element_text(family = "sans", size = 10))  

print(f3)

unemp_t <- ggplot(df2, aes(x = t_unemp, y = t_poor)) +
  geom_point() +
  geom_line() +
  labs(x = "Total Unemployment",
       y = "Total Poverty",
       caption = "Source: Eurostat (une_rt_a)") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"),  
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14),  
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14), 
    axis.text = element_text(family = "Arial", size = 12), 
    plot.caption = element_text(family = "sans", size = 10, color = "gray"))  

unemp_m<- ggplot(df2, aes(x = m_unemp, y = m_poor)) +
  geom_point() +
  geom_line() +
  labs(x = "Male Unemployment",
       y= "Male Poverty") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"),  
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14),  
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14), 
    axis.text = element_text(family = "Arial", size = 12), 
    plot.caption = element_text(family = "sans", size = 10, color = "gray"))

unemp_f <- ggplot(df2, aes(x = f_unemp, y = f_poor)) +
  geom_point() +
  geom_line() +
  labs(x = "Female Unemployment",
       y = "Female Poverty",
       title = "Unemployment vs. Poverty in Slovenia ()") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"),  
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14),  
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14),  
    axis.text = element_text(family = "Arial", size = 12),  
    plot.caption = element_text(family = "sans", size = 10, color = "gray"))  

f4 <- unemp_f / unemp_m / unemp_t

print(f4)


# f5 and f6 visualise GDP per capita and growth in net exports in Slovenia.

gdppc <- read.csv("gdppc.csv") %>%
  select("TIME_PERIOD", "OBS_VALUE") %>%
  rename(year = TIME_PERIOD,
         gdppc = OBS_VALUE) %>%
  mutate(lag = lag(gdppc, n = 1)) %>%
  mutate(growth = 100*(gdppc - lag)/lag)

trade <- read.csv("trade.csv") %>%
  select("na_item", "TIME_PERIOD", "OBS_VALUE") %>%
  rename(ei = na_item,
         year = TIME_PERIOD,
         exp_imp = OBS_VALUE) %>%
  group_by(year) %>%
  summarise(
    exports = sum(exp_imp[ei == "Exports of goods and services"]),
    imports = sum(exp_imp[ei == "Imports of goods and services"])
  )
df1 <- full_join(gdppc, trade, by = "year")
df1 <- na.omit(df1) %>%
  mutate(diff = exports - imports) %>%
  mutate(diff_g = 100*(diff - lag(diff))/lag(diff))

#  select("year", "growth", "exports", "imports", "diff_g")

yr_gdppc <- ggplot(df1, aes(x = year, y = gdppc)) +
  geom_line() +
  labs(x = "",
       y = "GDP Per Capita",
       title = "GDP Per Capita in Slovenia 2012-2023") +
  theme_minimal()+
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"),
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14), 
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14), 
    axis.text = element_text(family = "Arial", size = 12),
    plot.caption = element_text(family = "sans", size = 10, color = "gray")
  )

yr_growth <- ggplot(df1, aes(x = year, y = growth, fill = growth > 0)) +
  geom_bar(stat = "identity") +  
  scale_fill_manual(values = c("red", "green"), labels = c("Negative", "Positive")) +
  labs(
    x = "Year",
    y = "GDP Per Capita Growth",
    title = "Growth in GDP Per Capita in Slovenia 2012-2023",
    caption = "Source: Eurostat (nama_10_pc)",
    fill = "Growth"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"),  
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14),  
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14),  
    axis.text = element_text(family = "Arial", size = 12),  
    plot.caption = element_text(family = "sans", size = 10, color = "gray")
  )

f5 <- yr_gdppc / yr_growth
print(f5)

p1 <- ggplot(df1, aes(x = year)) +
  geom_line(aes(y = exports, col = "Exports")) +
  geom_line(aes(y = imports, col = "Imports")) +
  labs(x = "",
       y = "Exports/Imports",
       title = "Exports and Imports in Slovenia 2012-2023",
#       caption = "Source: Eurostat (nama_10_gdp)",
       colour = "Legend") +
  scale_color_manual(values = c("Exports" = "blue", "Imports" = "pink")) +
  theme_minimal()+
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"),  
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14), 
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14), 
    axis.text = element_text(family = "Arial", size = 12), 
    plot.caption = element_text(family = "sans", size = 10, color = "gray"),
    legend.text = element_text(family = "sans", size = 10)) 


p2 <- ggplot(df1, aes(x = year, y = diff)) +
  geom_line() +
  labs(x = "Year",
       y = "Net Exports")+
#       caption = "Source: Eurostat (nama_10_gdp)") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"),  
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14),  
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14),  
    axis.text = element_text(family = "Arial", size = 12),
    plot.caption = element_text(family = "sans", size = 10, color = "gray"),
  )
p3 <- ggplot(df1, aes(x = year, y = diff_g, fill = diff_g > 0)) +
  geom_bar(stat = "identity") +  # Use 'identity' to plot actual values of diff_g
  scale_fill_manual(values = c("red", "green"), labels = c("Negative", "Positive")) +
  labs(
    x = "Year",
    y = "Growth in Net Exports",
    caption = "Source: Eurostat (nama_10_gdp)",
    fill = "Growth"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"), 
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14), 
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14), 
    axis.text = element_text(family = "Arial", size = 12),
    plot.caption = element_text(family = "sans", size = 10, color = "gray")
  )


f6 <- p1/p2/p3
print(f6)

# f7 and f8 show HICP in Slovenia.

hicp <- read.csv("hicp") %>%
  select("TIME_PERIOD", "OBS_VALUE") %>%
  rename(date = TIME_PERIOD,
         index_s = OBS_VALUE) %>%
  mutate(date = paste(date, "01", sep = "-")) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))
hicp_c <- read.csv("hicp_c") %>%
  select("geo", "TIME_PERIOD", "OBS_VALUE") %>%
  rename(country = geo,
         date = TIME_PERIOD,
         index_c = OBS_VALUE) %>%
  mutate(date = paste(date, "01", sep = "-")) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

df3 <- full_join(hicp, hicp_c, by = "date") %>%
  filter(format(date, "%m") == "01")

f7 <- ggplot(df3, aes(x = date, y = index_s)) +
  geom_line(col = "purple") +
  geom_point(col = "purple") +
  labs(title = "HICP of Slovenia 1996-2024",
       x = "Year",
       y = "HICP of Slovenia",
       caption = "Source: Eurostat (prc_hicp_manr, prc_hicp_midx, prc_hicp_mmor)") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"), 
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14), 
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14),  
    axis.text = element_text(family = "Arial", size = 12),  
    plot.caption = element_text(family = "sans", size = 10, color = "gray")) 
print(f7)

hicp_2024 <- ggplot(df3 %>% filter(date == as.Date("2024-01-01"), 
                      country %in% c("Slovenia", "Hungary", "Italy", "Croatia", "Austria")), 
       aes(x = reorder(country, index_c), y = index_c))+
  geom_line()+
  geom_point() +
  geom_linerange(aes(ymin = 0, ymax = index_c)) +
  labs(x = "HICP (2024)",
       y = "Country",
       title = "HICP in Slovenia and Neighbouring Countries")+
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"),  
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14),  
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14),  
    axis.text = element_text(family = "Arial", size = 12),  
    plot.caption = element_text(family = "sans", size = 10, color = "gray"))

hicp_2011 <- ggplot(df3 %>% filter(date == as.Date("2011-01-01"),
                           country %in% c("Slovenia", "Hungary", "Croatia", "Italy", "Austria")),
            aes(x = reorder(country, index_c), y = index_c))+
  geom_line()+
  geom_point()+
  geom_linerange(aes(ymin = 0, ymax = index_c)) +
  labs(x = "HICP (2011)",
       y = "Country")+
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"),  
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14),  
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14),  
    axis.text = element_text(family = "Arial", size = 12),  
    plot.caption = element_text(family = "sans", size = 10, color = "gray"))

hicp_1998 <- ggplot(df3 %>% filter(date == as.Date("1998-01-01"),
                      country %in% c("Slovenia", "Hungary", "Croatia", "Italy", "Austria")),
       aes(x = reorder(country, index_c), y = index_c))+
  geom_line()+
  geom_point()+
  geom_linerange(aes(ymin = 0, ymax = index_c)) +
  labs(x = "HICP (1998)",
       y = "Country",
       caption = "Source: Eurostat (prc_hicp_manr, prc_hicp_midx, prc_hicp_mmor)")+
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "serif", face = "bold", size = 18, color = "forestgreen"),  
    axis.title.x = element_text(family = "Arial", face = "plain", size = 14),  
    axis.title.y = element_text(family = "Arial", face = "plain", size = 14),  
    axis.text = element_text(family = "Arial", size = 12),  
    plot.caption = element_text(family = "sans", size = 10, color = "gray"))
f8 <- hicp_2024/hicp_2011/hicp_1998
print(f8)
