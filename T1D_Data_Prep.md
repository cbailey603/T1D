# Data Importation

<p>
I was diagnosed with type-1 diabetes in January of 2024, changing my
life forever….
</p>

    # Importing data 
    df.dexcom <- read.csv("Data/dexcom_data2.csv")
    df.carbs <- read.csv("Data/carbs_data.csv")
    df.basal <- read.csv("Data/basal_data.csv")
    df.exercise <- read.csv("Data/exercise_bydatetime.csv")

# Data Cleaning and Augmentation

## Cleaning: BG, Carbs, Bolus Insulin, and Exercise Data

    # Cleaning and editing CGM, blood glucose data 
    df.bg <- df.dexcom |>
      filter(Event.Type == "EGV",
             Index > 10) |>
      mutate(Time = ymd_hms(Timestamp..YYYY.MM.DDThh.mm.ss.),
             BG = as.numeric(Glucose.Value..mg.dL.)) |>
      select(Time, BG, Transmitter.ID)

    # Cleaning and editing carbs and bolus data
    df.carbs <- df.carbs |> 
      mutate(Date = as.Date(Date, "%m/%d/%y"),
             TimeofDay = format(as.POSIXct(Time.of.Day,format='%I:%M:%S %p'),format="%H:%M:%S"),
             Time = ymd_hms(paste(Date, TimeofDay)),
             Carbs = as.numeric(Carbs..grams..est..),
             Bolus = as.numeric(Bolus..units.)) |>
      select(Time, Carbs, Bolus, Food)

    # Cleaning and editing exercise data 
    df.exercise <- df.exercise |>
      mutate(
         Time = ymd_hms(paste(Date, Time))
        ) |>
      select(Time, Exercise)

## Transforming: Daily Basal into Effective Basal

    # Generating a daily basal table
    df.basal <- df.basal |>
      mutate(Date = as.Date(Date, "%m/%d/%Y"),
             TimeofDay = format(as.POSIXct(Time,format='%I:%M:%S %p'),format="%H:%M:%S"),
             Time = ymd_hms(paste(Date, TimeofDay)),
             Time = format(as.POSIXct(Time, tz = "UTC"), "%Y-%m-%d %H:%M:%S"),
             Basal = as.numeric(Basal)) |>
      select(Time, Basal)

    # Generating a time dataframe
    df.time <- data.frame(df.bg$Time)

    # Generating a basal per 5 minutes table
    for (i in 1:nrow(df.time)) {
      day <- format(as.Date(df.time$df.bg.Time[i]), "%Y-%m-%d %H:%M:%S")
      df.time$EffectiveBasal[i] <- (df.basal[df.basal$Time == day, 2]/288)
    }
    df.lantus <- df.time |>
      mutate(Time = df.bg.Time) |>
      select(-c(df.bg.Time))

## Combining: Dataframe

    # Rounding time to the nearest 5 minutes
    df.bg <- df.bg |> mutate(Time = lubridate::round_date(Time, "5 minutes"))
    df.carbs <- df.carbs |> mutate(Time = lubridate::round_date(Time, "5 minutes"))
    df.lantus <- df.lantus |> mutate(Time = lubridate::round_date(Time, "5 minutes"))

    # Combining dataframes
    df <- merge(df.bg, df.carbs, by.y = "Time", all = TRUE)
    df <- merge(df, df.lantus, by.y = "Time", all = TRUE)
    df <- merge(df, df.exercise, by.y = "Time", all = TRUE)
      
    # Removing early (pre-CGM) data and unnecessary columns
    df <- df |>
      filter(Time >= "2024-02-03 00:00:00") |>
      select(-Transmitter.ID, -Food)

    # Removing duplicate rows
    df <- df[!duplicated(df$Time), ]

    # Removing NA's
    df[is.na(df)] <- 0

## Transforming: Efective Carbs

<p>
Next, every meal I've eaten and logged needs to be transformed to resemble the level of "effective carbs" in my body at five minute intervals. Prior to this step, carbohydrate levels correspond to the time the meal began. However, digestion and absorption of carbohydrates into the bloodstream as glucose does not happen instantaneously, it is a gradual process. Therefore, the quantity of carbs consumed needs to be transformed to more closely resemble the way carbohydrates are digested by the body; aka "effective carbs." 

[Discuss research paper and the formula used to convert].

In the following code, I implement matrices, lists, nested for-loops, and if-else statements to transformed "Carbs" to "EffectiveCarbs"
</p>

    # Making a blank matrix to put meals into 
    mx_carbs <- matrix(, nrow = 48, ncol = 4)
    colnames(mx_carbs) <- c("Index", "DateTime", "Carb", "EffectiveCarbs")
    mx <- mx_carbs

    # Making an empty list to put meal matrices into
    meal_list <- list()

    # Extract meals into the list
    mealcounter = 0  
    for (i in 1:nrow(df)) {
      if (df$Carbs[i] != 0) {
          mealcounter <- mealcounter + 1
          for (j in 1:48) {
            mx[j,] <- c((i+j-1), df$Time[(i+j-1)], df$Carb[(i+j-1)], NA)
          }
          meal_list[[mealcounter]] <- mx
          mx <- mx_carbs
      }
      else {
        mx <- mx_carbs
        }
      }

    # Calculate the effective carbs per meal 
    for (i in 1:length(meal_list)) {
      meal = meal_list[[i]]
      Cmeal = as.numeric(meal[1, "Carb"])
      for (j in 1:48) {
        if (j >= 1 & j <= 3) {
          meal[j, "EffectiveCarbs"] <- 0
            }
        else if (j >= 4 & j < 13) {
          meal[j, "EffectiveCarbs"] = (j-3)*(0.11*Cmeal)
        }
        else if (j >= 13 & j <= 48) {
          x = (Cmeal - (0.023*Cmeal)*(j-3))
          if (x < 0) {
            meal[j, "EffectiveCarbs"] = 0
          }
          else {
          meal[j, "EffectiveCarbs"] = x
          }
        }
      }
      meal_list[[i]] <- meal
    }

    # Add Effective Carbs together back into the main dataframe 
    df$EffectiveCarbs <- 0
    for (i in 1:length(meal_list)) {
      meal = meal_list[[i]]
      x = as.data.frame(meal)
      index = as.numeric(x$Index)
      for (j in 1:48) {
        if (is.na(x$DateTime[j])) {}
        else {
        df$EffectiveCarbs[index[j]] <- df$EffectiveCarbs[index[j]] + x$EffectiveCarbs[j]
        }
      }
    }

## Transforming: Efective Bolus Insulin

<p>
Similarly to carbohydrates, bolus injections need to transformed. Fast-acting bolus insulin is designed to negate post meal spikes caused by carbohydrates, so their "effective" levels have a similar shape to "EffectiveCarbs."

[Discuss research paper and the formula used to convert].

In the following code, I implement matrices, lists, nested for-loops, and if-else statements to transformed "Bolus" to "EffectiveBolus"
</p>

    # Making a blank matrix to put bolus into 
    mx_bolus <- matrix(, nrow = 72, ncol = 4)
    colnames(mx_bolus) <- c("Index", "DateTime", "Bolus", "EffectiveBolus")
    mx <- mx_bolus

    # Making an empty list to put bolus matrices into
    bolus_list <- list()

    # Extract boluses into the list
    boluscounter = 0  
    for (i in 1:nrow(df)) {
      if (df$Bolus[i] != 0) {
          boluscounter <- boluscounter + 1
          for (j in 1:72) {
            mx[j,] <- c((i+j-1), df$Time[(i+j-1)], df$Bolus[(i+j-1)], NA)
          }
          bolus_list[[boluscounter]] <- mx
          mx <- mx_bolus
      }
      else {
        mx <- mx_bolus
        }
      }

    # Calculate the effective bolus per injection 
    for (i in 1:length(bolus_list)) {
      bolus = bolus_list[[i]]
      Ibol = as.numeric(bolus[1, "Bolus"])
      for (j in 1:72) {
        if (j >= 1 & j <= 12) {
          bolus[j, "EffectiveBolus"] = (j)*(5/72)*(Ibol)
        }
        else if (j >= 13 & j <= 72) {
          x = Ibol*(1-((1/72)*j))
          if (x < 0) {
            bolus[j, "EffectiveBolus"] = 0
          }
          else {
          bolus[j, "EffectiveBolus"] = x
          }
        }
      }
      bolus_list[[i]] <- bolus
    }

    # Add Effective Bolus together back into the main dataframe
    df$EffectiveBolus <- 0
    for (i in 1:length(bolus_list)) {
      bolus = bolus_list[[i]]
      x = as.data.frame(bolus)
      index = as.numeric(x$Index)
      for (j in 1:72) {
        df$EffectiveBolus[index[j]] <- df$EffectiveBolus[index[j]] + x$EffectiveBolus[j]
      }
    }

## Combining: Effective Insulin

    # Effective insulin levels can be summed together 
    df <- df |>
      mutate(EffectiveInsulin = EffectiveBasal + EffectiveBolus) 

## Exporting: Final Dataframe

    # Final clean 
    df.export <- df |>
      select(Time, BG, EffectiveCarbs, EffectiveInsulin, Exercise)

    # Export
    write.csv(df.export, "Data/df_model.csv")
