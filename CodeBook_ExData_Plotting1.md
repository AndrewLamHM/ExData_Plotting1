# Code BookThis code book summaries the data cleaning procedures, resulting data fields in x_combined.txt (output of step 4) and x_combined_mean_std (output of step 5).## A.Data Cleaning Procedures

The function run_analysis in run_analysis.R does the following.

1. delete any old source date used previously
2. download the zipped file, unzip to a sub-directory under the current working directory
3. load all lines of the text file
4. for each line, split by a separate to get a list consisting of 9 columns all in char
5. transform to a data frame with 9 columns with respective data type
6. plot the graph(s) and save to a device
## B.Source Data

House_power_consumption.txt### Variables* Date:			Date
* Time:			Date time
* Global_active_power: 	Double
* Global_reactive_power: 	Double
* Voltage: 		Double
* Global_intensity:	Double
* Sub_metering_1: 	Double
* Sub_metering_2: 	Double
* Sub_metering_3: 	Double

* 2880 rows
* 9 columns
## C. Plot 1
y-axis			Frequency
x-axis			Global Active Power (kW)

## D. Plot 2
y-axis			Global Active Power (kW)
x-axis			Datetime

## E. Plot 3
y-axis			Energy sub metering
x-axis			Datetime

## F. Plot 4
### plot 1 (top left)
y-axis			Global Active Power (kW)
x-axis			Datetime

### plot 2 (top right)
y-axis			Voltage
x-axis			Datetime

### plot 3 (bottom left)
y-axis			Energy sub metering
x-axis			Datetime

### plot 4 (bottom right)
y-axis			Global Reactive Power
x-axis			Datetime

