# Functions to work with Hydromet Data from ETESA
# M. G. Castrellon, 3/14/2019

rename <- function(dat, oldnames, newnames) {
  datnames <- colnames(dat)
  datnames[which(datnames %in% oldnames)] <- newnames
  colnames(dat) <- datnames
  dat
}

DailyTable <- function(Yearly_Table, YY){
  Yearly_Table %>% 
    filter(Year == YY) %>% 
    select(-Day, -Year) %>% 
    gather(everything()) %>% 
    rename(c("everything()","value"),c("Month","Value")) %>% 
    group_by(Month) %>% 
    mutate(Day = seq(1,31,1)) %>% 
    ungroup() %>% 
    group_by(Day) %>% 
    mutate(Month = seq(1,12,1)) %>% 
    ungroup() %>% 
    mutate(Year = YY, Date = as.Date(paste(Year,Month,Day,sep="-"))) %>% 
    select(Date,Year,Month,Day,Value)
}

CreateDate <- function(df) {
  df %>% 
    mutate(Date = paste(Year,Month,Day,sep="-")) %>% 
    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
    arrange(Date)
}

SeparateDate <- function(df, fq) {
  # fq is the frequency of the data, either daily (D) or hourly (H)
  require(lubridate)
  if (fq == "D") {
    dt <- df %>% 
      separate(Date, c("Year","Month","Day")) %>% 
      mutate(Date = paste(Year,Month,Day,sep="-")) %>% 
      mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
      mutate(Year = as.integer(Year), Month = as.integer(Month), Day = as.integer(Day)) %>% 
      select(Date, everything()) 
    return(dt)
  }
  else if (fq == "H"){
    dt <- df %>% 
      mutate(Year = year(Date),
             Month = month(Date),
             Day = day(Date),
             Hour = hour(Date)) %>% 
      select(Date, Year, Month, Day, Hour, Value)
    return(dt)
  }
}

AssignMissingValues <- function(Data_Table, Freq) {
  # Freq is the frequency of the data, either daily (D) or hourly (H)
  require(lubridate)
  if (Freq == "D") {
    start <- min(Data_Table$Date)
    end <- max(Data_Table$Date)
    fulldate <- seq(ymd(start), ymd(end), by='1 day')
    temp <- with(Data_Table, Value[match(fulldate, Date)])
    df <- data.frame(Date = fulldate, Value = temp) %>% SeparateDate(Freq)
    return(df)
  }
  else if (Freq == "H") {
    start <- min(Data_Table$Date)
    end <- max(Data_Table$Date)
    fulldate <- seq(from=as.POSIXct(start), to=as.POSIXct(end), by='hour')
    temp <- with(Data_Table, Value[match(fulldate, Date)])
    df <- data.frame(Date = fulldate, Value = temp) %>% SeparateDate(Freq)
    return(df)
  }
  else {
    print("This frequency can't be handled by this function")
  }
}

# Functions to work with ETESA precipitation data

CompareValues <- function(OldData, NewData) {
  SubsetOld <- OldData %>% select(Date,Value)
  New_Table <- merge(NewData, SubsetOld, by = "Date") %>% 
    mutate(Def.Value = replace(Value.x, which(Value.y >= Value.x), Value.y[which(Value.y >= Value.x)])) %>% 
    mutate(Def.Value = replace(Def.Value, which(is.na(Def.Value)), Value.y[is.na(Def.Value)]))
  return(New_Table)
}

SmartCombine <- function(OldData, NewData, YYYY) {
  temp <- CompareValues(OldData, NewData) %>% 
    select(-Value.x,-Value.y) %>% rename("Def.Value","Value")
  New_Table <- OldData %>% 
    filter(Year < YYYY) %>% rbind.data.frame(temp)
  return(New_Table)
}

ETESA_Format <- function(df, id, sens, u){
  New_Hydro_Table <- df %>% 
    mutate(Station = id, Hour = 0, Sensor = sens, Unit = u, Quality = 0) %>% 
    select(Station, Date, Year, Month, Day, Hour, Sensor, Value, Unit, Quality)
  return(New_Hydro_Table)
}

CleanRainData <- function(Data_Table, ID) {
  Data_Table %>% 
    filter(Station == ID) %>%
    CreateDate() %>% AssignMissingValues() %>% 
    ETESA_Format(ID, 'LLUVIA', 'mm') %>% arrange(Date)
}

SaveCleanData <- function(Data_Table, ID, Name) {
  Data_Table %>% 
    filter(Station == ID) %>% CleanRainData(ID) %>% 
    write.csv(paste(ID,'_',Name,'_','Precip','.csv', sep=''), row.names = FALSE)
}