### Data Manipulation in R with dplyr

## tbl, a special type of data.frame

# Convert the hflights data.frame into a hflights tbl
hflights = tbl_df(hflights)

# The lookup table
lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")

# Use lut to translate the UniqueCarrier column of hflights
hflights$UniqueCarrier = lut[hflights$UniqueCarrier]

# Inspect the resulting raw values of your variables
glimpse(hflights)

# The lookup table
lut <- c("A" = "carrier", "B" = "weather", "C" = "FFA", "D" = "security", "E" = "not cancelled")

# Recode the CancellationCode column with lut
hflights$CancellationCode = lut[hflights$CancellationCode]


## The five verbs and select in more detail

#. select(), which returns a subset of the columns,
#. filter(), that is able to return a subset of the rows,
#. arrange(), that reorders the rows according to single or multiple variables,
#. mutate(), used to add columns from existing data,
#. summarise(), which reduces each group to a single row by calculating aggregate measures.

## Select Verb

# Print out a tbl with the four columns of hflights related to delay
select(hflights,ActualElapsedTime,AirTime,ArrDelay,DepDelay)

# Print out the columns Origin up to Cancelled of hflights
print(select(hflights,Origin:Cancelled))

# Answer to last question: be concise!
select(hflights,Year:DayOfWeek,ArrDelay:Diverted)

## Mutate Verb

# Add a second variable loss_ratio to the dataset: m1
m1 <- mutate(hflights, loss = ArrDelay - DepDelay,loss_ratio = loss/DepDelay)

# Add the three variables as described in the third instruction: m2
m2 = mutate(hflights,TotalTaxi=TaxiIn + TaxiOut,ActualGroundTime=ActualElapsedTime - AirTime,Diff=TotalTaxi-ActualGroundTime)

## Filter verbs

# Select the flights that had JFK as their destination: c1
c1 <- filter(hflights, Dest == "JFK")

# Combine the Year, Month and DayofMonth variables to create a Date column: c2
c2 <- mutate(c1, Date = paste(Year, Month, DayofMonth, sep = "-"))

# Print out a selection of columns of c2
select(c2, Date, DepTime, ArrTime, TailNum)

## Arrange verb

# Arrange according to carrier and decreasing departure delays
arrange (hflights, UniqueCarrier, desc(DepDelay))

# Arrange flights by total delay (normal order).
arrange (hflights,ArrDelay+DepDelay)

## Sumarise verb

#. min(x) - minimum value of vector x.
#. max(x) - maximum value of vector x.
#. mean(x) - mean value of vector x.
#. median(x) - median value of vector x.
#. quantile(x, p) - pth quantile of vector x.
#. sd(x) - standard deviation of vector x.
#. var(x) - variance of vector x.
#. IQR(x) - Inter Quartile Range (IQR) of vector x.
#. diff(range(x)) - total range of vector x.

# Remove rows that have NA ArrDelay: temp1
temp1 = filter (hflights, !is.na(ArrDelay))

# Generate summary about ArrDelay column of temp1
summarise (temp1,   earliest=min(ArrDelay),
                    average=mean(ArrDelay),
                    latest=max(ArrDelay),
                    sd=sd(ArrDelay))

# Keep rows that have no NA TaxiIn and no NA TaxiOut: temp2
temp2 = filter(hflights, !is.na(TaxiIn), !is.na(TaxiOut))

# Print the maximum taxiing difference of temp2 with summarise()
summarise (temp2, max_taxi_diff=max(abs(TaxiIn-TaxiOut)))

## Pipe tool

# Write the 'piped' version of the English sentences.
hflights %>%
  mutate(diff = TaxiOut - TaxiIn) %>%
  filter(!is.na(diff)) %>%
  summarise(avg = mean(diff))
  
# Chain together mutate(), filter() and summarise()
mutate (hflights, RealTime = ActualElapsedTime+100,
                    mph = Distance / RealTime * 60) %>%
                        filter (!is.na(mph) & mph<70) %>%
                            summarise  (n_less = n(),
                                        n_dest = n_distinct(Dest),
                                        min_dist = min (Distance),
                                        max_dist = max (Distance))

hflights %>%
  mutate(RealTime = ActualElapsedTime + 100, mph = Distance / RealTime * 60) %>%
      filter (mph<105 | Cancelled==1 | Diverted == 1) %>%
        summarise ( n_non = n(),
                    n_dest = n_distinct (Dest),
                    min_dist = min (Distance),
                    max_dist = max (Distance) )
					
# dplyr is loaded, hflights is loaded with translated carrier names

## Group_by

# Ordered overview of average arrival delays per carrier
hflights %>%
    filter (!is.na(ArrDelay) & ArrDelay>0) %>%
        group_by (UniqueCarrier) %>%
            summarise(avg = mean(ArrDelay)) %>%
                mutate (rank = rank(avg)) %>%
                    arrange (rank)
					
# How many airplanes only flew to one destination from Houston? adv1
hflights %>%
  group_by(TailNum) %>%
  summarise(ndest = n_distinct(Dest)) %>%
  filter(ndest == 1) %>%
  summarise(nplanes = n())

# Find the most visited destination for each carrier: adv2
hflights %>% 
  group_by(UniqueCarrier, Dest) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)
  
## Dplyr and databases

# Set up a connection to the mysql database
my_db <- src_mysql(dbname = "dplyr", 
                   host = "dplyr.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                   port = 3306, 
                   user = "dplyr",
                   password = "dplyr")

# Reference a table within that source: nycflights
nycflights <- tbl(my_db, "dplyr")

# glimpse at nycflights
glimpse (nycflights)

# Ordered, grouped summary of nycflights
nycflights %>%
    group_by (carrier) %>%
        summarise ( n_flights = n (),
                    avg_delay = mean (arr_delay )) %>%
arrange (avg_delay)