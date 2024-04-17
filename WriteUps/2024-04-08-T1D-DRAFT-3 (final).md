---
title: Type-1 Diabetes Analysis and Forecasting 
layout: post
post-image: "/assets/images/T1D_files/honeymoon_phase.png"
description: This is a personal, ongoing analysis of my recently diagnosed type-1 diabetes. Data is integral to type-1 diabetes management, as carbohydrate estimations and insulin dosage calculations regularly occur throughout the day. Through this project, I aim to understand my body better and improve my type-1 diabetes management. Additionally, personal insights could translate into helping the greater diabetes community. This is the very beginning of a lifelong project.
tags:
- R
- Machine-Learning
- Forecasting
- Visualization
---

# Introduction 

<p>
In January of 2024, I was diagnosed with type-1 diabetes. As I learned about my condition and what it would demand of me, I realized that my education in data analysis has given me the tools to tackle this lifelong challenge. With this realization, I decided to make the best of a bad situation and use my data analysis knowledge to track and analyze my diabetes. 
</p>


## A Brief Diabetes Lesson

<p>
Diabetes primarily concerns blood glucose (BG, henceforth) and insulin. Cells in the body are powered by glucose (sugar), and the blood delivers this glucose to all cells. Therefore, BG measures how much glucose is in the blood (measured as milligrams per deciliter in my data). Insulin is a hormone produced by the pancreas that removes glucose from the blood for use in the cells. Diabetes is a breakdown of this bodily function. Type-1 diabetes results from the immune system attacking the pancreas, stopping insulin production, sometimes completely. Type-2 diabetes is largely the result of insulin resistance. In both cases, the body cannot effectively remove glucose from the bloodstream for use in the cells, meaning glucose builds up in the blood, sometimes to dangerously high levels. A normal BG range is between 70-140 mg/dL. As an example of high BG levels, my BG was 430mg/dL when I was diagnosed. Given the importance of glucose to cell function, high BG levels negatively affect many parts of the body, from the eyes to the kidneys. To avoid these risks and maintain a normal BG, type-1 diabetics need to regularly inject insulin because their pancreas either does not produce any insulin or produces an insufficient amount. There are multiple types of insulin injections; however, type-1 diabetics usually take two types regularly: long-acting (basal) and short-acting (bolus). Basal injections are taken once a day and act as a baseline level of insulin that works throughout the day. Bolus injections are taken at meals and correspond to the amount of carbohydrates consumed, offsetting the BG spike that occurs when carbs are consumed. 
</p>

#### Insulin and Carbohydrates

<p>
The primary source of glucose comes from the carbohydrates we consume. Carbohydrates are one of the three main macronutrients, including protein and fat. Carbohydrates come from various foods, from breads, rice, and pasta to sweets like soda and candy. While some carbohydrate foods are healthier than others, all carbohydrates break down into glucose in the body, which increases BG levels. Therefore, type-1 diabetics need to gauge the amount of carbohydrates they consume and, potentially, take fast-acting insulin (bolus) to negate BG spikes. The bolus insulin dosage depends on a diabetic’s “carb ratio,” which represents how susceptible they are to BG spikes from carb consumption. This can and will change throughout a type-1 diabetic’s life as their pancreas’ ability to produce insulin eventually ceases. For example, my carb ratio is currently 1:37g, meaning I need one unit of fast-acting insulin for every 37g of carbohydrates. 
</p>

#### Exercise

<p>
Many additional factors affect BG, including stress and hormones. A key factor, which I included in my analysis, is exercise. Exercise extracts reserved glucose from the muscles and liver, which is then replenished by the glucose in the blood. This means that exercise lowers BG levels and increases insulin receptivity. While this is mainly beneficial, it can also lower BG to unsafe low levels, which can be dangerous. 
</p>

# Data: Sources
### Continuous Glucose Monitor (CGM)

<p>
The blood glucose data for this project comes from my continuous glucose monitor (CGM), a Dexcom G7. The continuous glucose monitor (CGM) is arguably the most impactful innovation in diabetes management. Also known as a “patch,” a CGM is applied to the skin where fatty tissue is located (I wear mine on my upper arm near my armpit). It can be paired with a smartphone using Bluetooth and provide diabetics with BG readings every five minutes, along with a trend graph and other pertinent reports. This data can be synchronously shared with healthcare providers and loved ones. However, most importantly, in my case, it can be downloaded as a comma-separated values file. The day I began using my CGM was the day I decided to embark on this project.
</p>

### Spreadsheets: Carbohydrates, Insulin, and Exercise

<p>
Once I identified the possibilities from my CGM data, I developed spreadsheets to track the other pertinent factors in my diabetes management. Starting in early February of 2024, I began logging my meals, capturing the date, time, estimated carbs in grams, what I was eating, and the dosage of my bolus injection, if applicable. Additionally, I logged the date and dosage of my daily basal insulin injection. I also created a spreadsheet to log the time I exercised. 
</p>

# Data: Preparation
<p>
The data is imported as .csv files, saved as dataframes, and prepped for analysis and modeling. This is done in the following code:
</p>
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
             TimeofDay = format(as.POSIXct(Time.of.Day,
             format='%I:%M:%S %p'),format="%H:%M:%S"),
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

<p>
</p>

## Transforming: Effective Basal

<p>
The end goal of this project is to analyze and model the relationship between blood glucose levels and the factors dictated by diabetes management (i.e. insulin, carbs, and exercise). To do this, I need those dependent variables to be on the same time scale as the blood glucose data, which is captured every five minutes. Therefore, I must transform the basal insulin into an "effective basal" insulin level. Basal insulin is injected daily, once a day, before bed in my case. I tracked my basal dosage in a spreadsheet by date. However, long-acting insulin behaves like it sounds by acting slowly throughout the day. The level of insulin in the body from long-acting basal is more or less constant and lasts roughly twenty-four hours straight. This type of insulin is designed to provide a baseline level of insulin for the body's use. Therefore, I can transform the basal dosage into five-minute increments with the following code:
</p>

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

## Combining: Single Dataframe

<p>
The following code aggregates and organizes all of the dataframes into one:
</p>

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

## Transforming: Effective Carbs

<p>
Next, every meal I've eaten and logged needs to be transformed to resemble the level of "effective carbs" in my body at five-minute intervals. Before this step, carbohydrate levels correspond to the time the meal began. However, the digestion and absorption of carbohydrates into the bloodstream as glucose does not happen instantaneously, it is a gradual process. Therefore, the quantity of carbs consumed needs to be transformed to more closely resemble how the body digests carbohydrates; aka "effective carbs." 
</p>

<p>
In my research for this project, I came across an excellent paper in the Diagnostics (Basel) scientific journal called "Feature Transformation for Efficient Blood Glucose Prediction in Type 1 Diabetes Mellitus Patients," by Hatim Butt, Ikramullah Khosa, and Muhammad Aksam Iftikhar. Similarly to the goals of this project, that paper aimed to generate an accurate forecast model for blood glucose levels. Therefore, I heavily utilized their methodology, one such example being the transformation of carbohydrates consumed into "effective carbs." Their methodology involved implementing a piece-wise function to convert a single amount of carbohydrates into a time series of data points of what they called "operative carbs." The plot of this piece-wise function can be seen in Figure 1 below:
</p>

<p>
</p>
<img src="/assets/images/T1D_files/effective_carbs.png">
<p>
</p>

<p>
In the following code, I adapt and utilize this piece-wise function for my analysis. I implement matrices, lists, nested for-loops, and if-else statements to transform "Carbs" to "Effective Carbs"
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

## Transforming: Effective Bolus 

<p>
Similarly to carbohydrates, bolus injections need to transformed. Fast-acting bolus insulin is designed to negate post-meal spikes caused by carbohydrates, so their "effective" levels are similar to "Effective Carbs." More specifically, in Figure 2 below, the operative level of bolus insulin over time can be seen. This image was obtained from Humalog's website. Humalog is the brand of bolus insulin I take. 

<p>
</p>
<img src="/assets/images/T1D_files/effective_bolus.png">
<p>
</p>

Similarly to the transformation for carbohydrates, a piece-wise function is utilized to transform the single insulin dose into a time series. In the following code, I implement matrices, lists, nested for-loops, and if-else statements to transform "Bolus" to "Effective Bolus"
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


# Visualization and Analysis

<p>
Now that our data is imported, cleaned, and correctly formatted, we can begin the analysis. In the case of diabetes, blood sugar is the critical variable. The health of a diabetic largely depends on their blood glucose levels and trends. It will be the independent variable in the regression analysis later in this report.  Therefore, let’s begin by examining blood glucose:
</p>

## Blood Glucose Analysis

<p>
As was previously stated, a healthy blood glucose range is between 70-180 (mg/dL). Let’s visualize the distribution of my BG: 
</p>

```
# Calculate optimal binwidth using the Freedman-Diaconis rule
bw <- 2 * IQR(df$BG) / length(df$BG)^(1/3)


# Distribution of blood sugar
ggplot(df, aes(x = BG)) +
    annotate("rect", fill = "gray",
           xmin = 70, xmax = 180, ymin = 0, ymax = 1000, alpha = .6) +
  geom_label(aes(x = 150, y = 875, label = "Normal BG Range"), 
             color = "gray", fill = "white", alpha = 0.6) +
  geom_histogram(color = "black", fill = "purple", binwidth = bw) +
  labs(title = "Blood Sugar Distribution",
       subtitle = "Including Normal BG Range",
       x = "BG (mg/dL)",
       y = "Frequency") 
```
<p>
</p>

<img style="float: left;margin:0 10px 10px 0" src="/assets/images/T1D_files/BGdist.png">

<p>
Firstly, the shape of the distribution indicates that it is not normally distributed; it is slightly skewed to the right. This makes sense, considering the differences in physical symptoms between a low BG (<70mg/dL) and a high BG (>180mg/dL). Low blood glucose is uncomfortable and dangerous. Symptoms include feeling lightheaded, dizzy, sweaty, and hungry; worse symptoms can arise if the low becomes drastic. Conversely, high blood glucose does not manifest physical symptoms unless levels are considerably high and/or long-term. Therefore, diabetics experiencing low blood sugar can and will react promptly to increased BG levels, usually by consuming glucose. Returning to the BG distribution, high blood glucose is more common than low blood glucose and persists for longer, resulting in a right-skewed distribution. Lastly, we can see that I spend most of my time in the normal blood glucose range, which is great! This can be quantified as time-in-range and is represented as a percentage, which is calculated below:
</p>


    # Percent in-range
    round(100*(sum(df$BG >= 70 & df$BG <= 180)/nrow(df)), 2)

    ## [1] 91.69


<p>
</p>

<p>
We can see that I have spent roughly 92% of my time “in range” since my diagnosis. The target my doctor gave me was to be at least 75% in range, so things have been going well! 
</p>

<p>
Lastly, as we can see in the summary statistics calculated below, my average blood glucose level is 123 mg/dL, and the standard deviation is 33.6. 
</p>

    # Summary statistics
    df |> summarise(BG.mean = mean(BG),
                    BG.sd = sd(BG),
                    BG.var = var(BG))

    ##    BG.mean    BG.sd  BG.var
    ## 1 123.1527 33.57915 1127.56

<p>
</p>

### Insulin Analysis: Honeymoon-Phase

<p>
The promising results from the initial analysis of my blood glucose levels seem surprising, given that I have just been diagnosed and am learning how to manage my condition. However, a substantial portion of my positive results can likely be explained by what’s known as the “honeymoon phase.” Depending on how early it is caught, type-1 diabetics can sometimes return to a more normal pancreas functioning for some time, ranging from weeks to years. This “honeymoon phase” indicates that the diabetic’s pancreas is still somewhat functional. Insulin therapy directly assists the pancreas’ functioning by reducing its workload, allowing the somewhat functional pancreas to continue producing insulin. However, it is temporary, and the immune system will eventually destroy the pancreas’ ability to produce insulin.
</p>

<p>
This “honeymoon phase” can be seen in my data by comparing my long-acting basal insulin and my average daily blood glucose level. In the following code, I will generate and plot those two subsets of data:
</p>

    # Calculating average BG every day from CGM data
    df.avgBG <- df |>
      mutate(Date = as_date(Time),
             Time = floor_date(Date)) |>
      group_by(Date) |>
      summarize(avgBG = mean(BG))

    # Cleaning old handwritten BG data (prior to CGM)
    df.old <- df.old |>
      mutate(Date = as_date(Date)) |>
      select(Date, avgBG) |>
      arrange(Date)

    # Aggregating all average BG data
    df.avgBG <- rbind(df.old, df.avgBG)

    # Combing average BG with basal
    df.basal <- df.basal |>
      mutate(Date = as.Date(Date, "%m/%d/%Y"),
             TimeofDay = format(as.POSIXct(Time,format='%I:%M:%S %p'),format="%H:%M:%S"),
             Time = ymd_hms(paste(Date, TimeofDay)),
             Time = format(as.POSIXct(Time, tz = "UTC"), "%Y-%m-%d %H:%M:%S"),
             Basal = as.numeric(Basal)) |>
      select(Time, Basal)
    df.basal <- cbind(df.avgBG, df.basal) |>
      select(Date, Basal, avgBG)

    # Generating new time-series to plot
    ts.honeymoon <- df.basal |>
      as_tsibble(index = Date)

    # Plotting average BG  
    xmin = df.basal$Date[1]
    xmax = df.basal$Date[nrow(df.basal)]
    gg.avgBG <- ggplot(ts.honeymoon, aes(x = Date, avgBG)) +  
      geom_point(aes(y = avgBG)) +
      geom_line(aes(y = avgBG)) +
      labs(title = "Daily Average Blood Glucose",
           x = "Date",
           y = "Avg. BG (mg/dL)") +
      annotate("rect", xmin = xmin, xmax = xmax, ymin = 70, ymax = 180, 
               alpha = .2)

    # Plotting basal units
    gg.basal <- ggplot(ts.honeymoon, aes(x = Date, Basal)) +  
      geom_point(aes(y = Basal)) +
      geom_line(aes(y = Basal)) +
      labs(title = "Daily Basal Insulin Dosage",
           x = "Date",
           y = "Basal Insulin (units)")

    # Combining into one plot
    ggarrange(gg.avgBG, gg.basal, ncol = 1, nrow = 2)

<p>
</p>


<img style="float: left;margin:0 10px 10px 0" src="/assets/images/T1D_files/honeymoon_phase.png">

<p>
</p>

<p>
My healthcare team has been superb in educating and helping me with my diabetes management. I regularly meet with my pharmacist to review my blood glucose levels and adjust my insulin doses and carb ratios. In the first few weeks of my diabetes management, I was routinely going low in the middle of the night and early morning, signaling that my long-acting basal insulin dose was too high. As the plot above shows, my basal insulin dose steadily dropped until settling at 4 units. However, my daily blood glucose average stayed well in range (ignoring January). This trend in the data indicates that as I began insulin therapy and brought my blood glucose levels back into range, my pancreas was able to “come back online,” for lack of a better phrase. The current basal dose of 4 units and bolus insulin, as needed with meals, is making up for the slack in my pancreas’ insulin production. I am grateful for the “honeymoon phase,” and long may it continue! 
</p>

<p>
</p>

### Carbs Analysis: Daily Intake

<p>
Since tracking this data, I have cataloged roughly 200 meals (snacks included) and the corresponding quantity of carbohydrates consumed. As was previously stated, carbs are an essential macronutrient, and it is important to eat a sufficient amount of them daily. However, research has been done to determine the benefits of a low-carb diet for type-1 diabetics. A 2019 paper in the journal Nutrients, called "Carbohydrate Restriction in Type 1 Diabetes: A Realistic Therapy for Improved Glycaemic Control and Athletic Performance?" explores the benefits and drawbacks of a low carbohydrate diet for type-1 diabetics. The paper summarizes that low carb diets "represent a strategy to improve glycaemic control and metabolic health in people with T1D." (Scott, Anderson, Morton, Wagenmakers, and Riddell, 2019) The paper also discusses the drawbacks and challenges of such a diet, including availability and cost, and potential low blood glucose episodes. With this in mind, let’s examine my daily carb intake and compare it to the ranges laid out in the paper.
</p>

    # Calculating daily carb intake
    df.daily.carbs <- df.carbs |>
      mutate(Date = as_date(Time),
             Time = floor_date(Date)) |>
      group_by(Date) |>
      summarize(DailyCarbs = sum(Carbs))

    # Generating new time-series to plot
    ts.daily.carbs <- df.daily.carbs |>
      as_tsibble(index = Date)

    # Plotting daily carbs
    kgs <- 57
    lowmin <- 1*kgs
    lowmax <- 3*kgs
    midmax <- 6*kgs
    highmax <- 8*kgs
    ggplot(ts.daily.carbs, aes(x = DailyCarbs)) +  
      geom_histogram(color = "black", fill = "royalblue", binwidth = 15) +
      labs(title = "Daily Carbohydrate Intake Distribution",
           subtitle = "Including Recommended Ranges of Daily Carb Intake (Based on Weight)",
           x = "Daily Carb Intake",
           y = "Frequency") +
      annotate("rect", fill = "pink",
               xmin = lowmin, xmax = lowmax, ymin = 0, ymax = 18, alpha = .2) +
      annotate("rect", fill = "darkgreen",
               xmin = lowmax, xmax = midmax, ymin = 0, ymax = 18, alpha = .2) +
      annotate("rect", fill = "orange",
               xmin = midmax, xmax = highmax, ymin = 0, ymax = 18, alpha = .2) +
      geom_label(aes(x = 150, y = 13, label = "Low"), 
               color = "red", fill = "white") + 
      geom_label(aes(x = 250, y = 13, label = "Moderate"), 
               color = "darkgreen", fill = "white") +
      geom_label(aes(x = 400, y = 13, label = "High"), color = "orange", fill = "white")  


<p>
</p>
<img src="/assets/images/T1D_files/dailycarbs.png">
<p>
</p>

<p>
The graph above shows that my daily carb intake largely falls in the outlined low-carbohydrate diet discussed in the study. While the study's conclusions indicate that my carb diet will have positive results, my carb intake can mostly be attributed to my aversion to taking bolus insulin when possible. As I said earlier in this report, my carb ratio is 1:37. Therefore, for most meals, I try to keep the amount of carbohydrates below the 37g level to avoid an injection. The consequence of this aversion is that I have been eating a low carbohydrate diet, which, luckily, seems beneficial, based on the available research.
</p>


### Exercise Analysis: Effect on BG

<p>
The last factor to analyze is exercise. The data I captured for exercise is admittedly limited. I cataloged the date and time my exercise began and ended. Ideally, heart rate data would be gathered with a fitness tracker wristband or Apple watch. Regardless, even with the limited nature of the available data, the effect of exercise on blood glucose is clear: 
</p>

    # BG during exercise
    df.bg_during_exercise <- df |>
      filter(Exercise == 1) |>
      mutate(Day = as_date(Time),
             Hour = hour(Time),
             Minute = minute(Time),
             TimeSpent = paste(Hour, Minute, sep = ":")) |>
      group_by(Day) |>
      select(BG, Day, TimeSpent)

    # Manipulating the time to "time from start of workout"
    time_vector <- format(seq.POSIXt(as.POSIXct(Sys.Date()),as.POSIXct(Sys.Date()+1),by = "5 min"), "%H:%M", tz="UTC")
    df.bg_during_exercise$TimeSpent[1:16] <- time_vector[1:16]
    df.bg_during_exercise$TimeSpent[17:26] <- time_vector[1:10]
    df.bg_during_exercise$TimeSpent[27:38] <- time_vector[1:12]
    df.bg_during_exercise$TimeSpent[39:47] <- time_vector[1:9]
    df.bg_during_exercise$TimeSpent[48:55] <- time_vector[1:8]
    df.bg_during_exercise$TimeSpent.num <- as.numeric(hm(df.bg_during_exercise$TimeSpent), "minutes")

    # Plotting 
    xmin <- df.bg_during_exercise$TimeSpent[1]
    xmax <- df.bg_during_exercise$TimeSpent[16]
    df.bg_during_exercise |>
      ggplot(aes(x=TimeSpent, y = BG, group = Day, color = Day)) +
      geom_point(aes(y = BG, color = factor(Day))) +
      geom_line(aes(y = BG, color = factor(Day))) +
      labs(title = "Blood Glucose While Exercising",
           x = "Length of Exercise",
           y = "BG (mg/dL)") +
      annotate("rect", xmin = xmin, xmax = xmax, ymin = 70, ymax = 180, 
               alpha = .2) +
      scale_x_discrete(breaks=c("00:15","00:30","00:45","01:00","01:15"),
                       labels=c("00:15","00:30","00:45","01:00","01:15"))

<p>
</p>
<img src="/assets/images/T1D_files/BG_during_exercise.png">
<p>
</p>

<p>
The graph above depicts my blood glucose from the start to the end of my workouts. The downward trend is apparent, demonstrating the benefits of exercise in blood glucose level management. An additional benefit of exercise that is not demonstrated here but is widely understood in the available diabetic research is that it increases insulin sensitivity and can reduce overall blood glucose levels for roughly twenty-four hours following a workout. However, the visual also highlights the potential danger of going low due to exercise, highlighting the need to have glucose readily available when exercising. 
</p>

# Regression Analysis

<p>
The last analysis I conducted before forecasting was regression analysis. Blood glucose was the dependent variable, and the independent variables were effective carbohydrates, effective insulin, time of day, and a factor of whether I was exercising or not. I explored some multivariate linear regression models that incorporated these factors with little success regarding goodness of fit. However, the model's performance improved vastly when I added lagged values of the dependent variable, blood glucose. This intuitively makes sense when we consider the time-series nature of blood glucose. This dramatic increase in performance made me pause and consider the validity of a linear regression model for this data; so I investigated the assumptions predicating linear regression: linearity, homoscedasticity, normality, and independence of variables. While independence and homoscedasticity were all verified, normality and linearity were not. The lack of linearity was of particular concern, and I found no discernible linear relationship between the dependent and independent variables. This fact and the statistical significance of the lagged dependent components in the model performance suggested that linear regression held little to no insights. I will proceed directly to time-series analysis and forecasting. 
</p>

# Forecasting 

<p>
Now that the data has been analyzed and understood well, a forecasting model that predicts my blood glucose levels can be generated. As discussed in more detail later in this report, a neural network model is the preferred choice for this application. Achieving a relatively accurate forecasting model for blood glucose levels will confirm the bodily relationships between carbohydrates, insulin, and blood sugar. Furthermore, it will demonstrate that with consistently thorough management, diabetes can be effectively managed. 
</p>

## Model Preparation

### Transformation: Box-Cox

<p>
As demonstrated in the visualization section of this report, blood glucose levels are not normally distributed but have a slight right skew. Normality is preferred for an optimal forecast model. Therefore, we will transform the dependent variable, blood glucose, using the box-cox transformation. 
</p>

    # Box-Cox transformation, via Guerrero optimization
    lambda <- BoxCox.lambda(ts$BG, method = c("guerrero"))
    ts$BG <- ts$BG^lambda
    transformed.ts <- as_tsibble(ts, index = Time)

### Train and Test Subsets

<p>
Next, the data needs to be split into train and test subsets. The train subset will be used in model generation for the machine-learning model to "learn" from. Then, the model will predict the outcome of the dependent variable, blood glucose, using the independent variables from the test subset. The predicted blood glucose outcome will then be compared against the actual blood glucose levels in the test subset to determine the model's accuracy. To utilize all variables in the data set and go easy on my computer, the train subset will be only four days of data, including exercise and the test subset will be a single day. 
</p>

    # Train
    train <- transformed.ts |>
      filter(Time >= "2024-03-11 00:00:00",
            Time < "2024-03-15 00:00:00") |>
      fill_gaps() |> 
      arrange(Time)

    # Test
    test <- transformed.ts |>
     filter(Time >= "2024-03-15 00:00:00",
            Time < "2024-03-16 00:00:00") |>
     fill_gaps() |>
     arrange(Time)

## Model Generation: Neural Network

<p>
The time series is non-stationary and autocorrelated, meaning the dependent variable's lagged values affect the dependent variable's current value. Therefore, the optimal forecasting model choice is a neural network model with an automatically optimized autoregression component. 
</p>

    fit <- train |> model(
     NNETAR = NNETAR(BG)) 


Generated Model | Lagged Inputs | P (1 for seasonal time-series) | Nodes in Hidden Layer | Seasonal Component 
--- | --- | --- | --- |--- 
<NNAR(26,1,14)[12]> | 26 | 1 | 14 | 12 

<p>
The neural network optimized with the 26 lagged input values and 14 nodes in the hidden layer of the network. 
</p>

## Forecasting: Visualization

<p>
</p>
<img src="/assets/images/T1D_files/ONE_DAY/NNETAR_forecast.png">
<p>
</p>

<p>
This plot shows the blood glucose values in black and the predicted values in blue for the fifth day. As we can see, the point forecast is not as dynamic as the actual values but does follow the general trend in the blood glucose data. Let's proceed to the accuracy metrics to inspect the strength of the model further.
</p>

### Accuracy: Point Forecast

Model | MAE | RMSE | MAPE 
--- | --- | --- | --- |--- 
NNETAR | 16.84926 | 22.63226 | 14.75651

<p>
The mean absolute error (MAE) measures the average error of the predicted values compared to the actual values from the test subset. The root mean squared error (RMSE) is a similar measurement, but instead of taking the absolute value, it takes the square root of the squared error. Both of these metrics are in the scale of the dependent variable, blood glucose, in this case. Therefore, we can summarize these metrics by saying the average error of the predicted blood glucose values is about 17 or 23 mg/dL, respectively. The mean absolute percentage error (MAPE) is not on the same scale as the dependent variable, as they are a percentage. A MAPE below 10% is considered excellent, and from 10%-20% is considered good. Therefore, a MAPE value of 15% for our model is respectable. 
</p>

### Accuracy: Prediction Interval

Model | CRPS
--- | --- 
NNETAR | 13.89529 

<p>
The continuous ranked probability score (CRPS) measures the accuracy of the forecast model's prediction intervals. Effectively, this percentage metric scores how well the predicted values fall within the generated prediction intervals. Therefore, our score of ~14% suggests a relatively accurate model. 
</p>


# Conclusion

<p>
This analysis has greatly enhanced my understanding of my diabetes and the management necessary to ensure long-term positive health outcomes. For example, I now greatly appreciate the benefits of a low-carbohydrate diet and regular exercise. Forecasting my blood glucose levels, utilizing the pertinent factors of carbs, insulin, and exercise, with relative accuracy, has solidified the importance of strict observance and execution of my diabetes management. Furthermore, my analysis and the existing research I read excite me about the future of diabetes research and care options. The diabetes management technology introduced in recent years already utilizes the methodology in this report. I am optimistic about the care options that will be developed in the future and perhaps even a cure. 
</p>

# Limitations 
<p>
The core limitation of this analysis relates to the variables included and the precision with which they were gathered. For example, exercise could be more comprehensively quantified by measuring heart rate and exertion levels. Additionally, the amount of consumed carbohydrates is an estimation done regularly throughout the day and may not always be accurate. This is especially true when eating out. Lastly, while CGM data is considered accurate for diabetes management, it can be less accurate during the first day after application, and other hardware and software issues can distort readings. 
</p>

# Resources

<p>
200 KwikPen: Hcps: Humalog® (insulin lispro) injection. U. (n.d.). https://www.humalog.com/hcp/u200#bioequivalence 
</p>
<p>
Butt, H., Khosa, I., & Iftikhar, M. A. (2023, January 17). Feature transformation for efficient blood glucose prediction in type 1 diabetes mellitus patients. Diagnostics (Basel, Switzerland). https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9913914/
</p>
<p>
De Paoli, B., D’Antoni, F., Merone, M., Pieralice, S., Piemonte, V., & Pozzilli, P. (2021, May 26). Blood glucose level forecasting on type-1-diabetes subjects during physical activity: A comparative analysis of different learning techniques. Bioengineering (Basel, Switzerland). https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8229703/ 
</p>
<p>
Scott, S. N., Anderson, L., Morton, J. P., Wagenmakers, A. J. M., & Riddell, M. C. (2019, May 7). Carbohydrate restriction in type 1 diabetes: A realistic therapy for improved glycaemic control and athletic performance?. Nutrients. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6566372/ 
</p>

