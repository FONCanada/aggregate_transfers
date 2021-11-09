source("setup.R")

########################################
# Load and Structure the Relevant Data #
########################################

# Historical Data from Tombe (2018, Canadian Tax Journal), 1867 to 2006
tombe_historic<-read.csv("TombeData.csv") # Units are in millions of current dollars

# Load the Finance of the Nation Macroeconomic Database
macro_data<-read.csv("https://github.com/FONCanada/FONmacro/raw/main/MacroData.csv")

# Get Modern Transfer Data from Statistics Canada
data_0716<-getTABLE("36100450") # Federal spending by province, 2007-Latest

####################
# Compile the Data #
####################

# Get the relevant transfer data from the current StatCan data
current_transfers<-data_0716 %>%
  filter(Levels.of.government=="Federal general government",
         Estimates=="To provincial and territorial general governments") %>%
  mutate(coord=str_sub(COORDINATE,-3)) %>%
  filter(coord=="108") %>%
  filter(!(is.na(short)),!(short %in% c("YT","NT","NU","CAN"))) %>%
  select(Year=Ref_Date,Province=GEO,Value)

# Compile transfer data into a single series
complete_series<-tombe_historic %>%
  rbind(current_transfers)

#########################################################
# Complete Series with Full Normalizations, 1867-Latest #
#########################################################

all_transfers_prov<-complete_series %>%
  left_join(
    macro_data %>% 
      filter(Region=="Canada") %>% 
      select(-Region) %>% 
      spread(Variable,Value) %>%
      select(Year,
             Prices=`Canada-Wide CPI (2021=100)`,
             GDP=`Nominal GDP (Millions)`),
    by="Year"
  ) %>%
  left_join(
    macro_data %>%
      filter(Variable=="Nominal GDP (Millions)") %>%
      select(Year,Province=Region,provGDP=Value) %>%
      drop_na(),
    by=c("Province","Year")
  ) %>%
  left_join(
    macro_data %>%
      filter(Variable=="Population") %>%
      select(Year,Province=Region,pop=Value) %>%
      drop_na(),
    by=c("Province","Year")
  ) %>%
  mutate(InfAdjust=100/Prices,
         realPC=1000000*InfAdjust*Value/pop,
         real=InfAdjust*Value,
         nom=Value,
         percapita=1000000*Value/pop,
         shareGDP=Value/GDP,
         shareGDP_prov=Value/provGDP) %>%
  select(Year,Province,percapita,realPC,shareGDP,real,nom,shareGDP_prov) %>%
  gather(Units,Value,-Year,-Province) %>%
  mutate(Units=ifelse(Units=="percapita","Dollars Per Capita ($)",Units),
         Units=ifelse(Units=="realPC","Real Dollars Per Capita ($ 2021)",Units),
         Units=ifelse(Units=="real","Real Dollars (Millions, $ 2021)",Units),
         Units=ifelse(Units=="nom","Nominal Dollars (Millions)",Units),
         Units=ifelse(Units=="shareGDP","Share of Canada's GDP",Units),
         Units=ifelse(Units=="shareGDP_prov","Share of Provincial GDP",Units)) %>%
  rename(Normalization=Units) %>%
  drop_na()
write.csv(all_transfers_prov,"AggregateTransfers.csv",row.names = F)
