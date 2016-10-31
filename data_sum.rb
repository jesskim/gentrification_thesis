# Created by Jordan Holliday 10/8/2016
# To run:
# 1. In terminal, go to folder with .xls files
# 2. Run "convert2csv" (no quotes) to convert .xls files to .csv files and concatenate into all_data.csv
# 3. Run ruby "data_sum.rb"

# The output is a master .csv with all of the annualized property sales files with columns for year, zip code, neighborhood, and total sales


require "CSV" # to include ruby CSV utility
require "date" # to parse dates

sumByNeighborhood = Hash.new(0) # Start neighborhood hash (key-value pair) at 0 to add sales numbers
sumByZip = Hash.new(0) # Start zip code hash (key-value pair) at 0 to add sales numbers
sumByBorough = Hash.new(0) # Start zip code hash (key-value pair) at 0 to add sales numbers

CSV.foreach("all_data.csv") do |row| # iterate through each row of raw data
	next if row.nil? || row[-1].nil?
	next if row[-1].include? "S" # skip header row
	if row[-1].include?("/") # identify sales dates that are split by slashes
		new_date = row[-1].split("/") # create temp array to separate year, month, and day
		new_date[2] = "20" + new_date[2] # add the millenium & century to the year
		new_date = [new_date[2], new_date[0], new_date[1]].join("-") # reorder & join the date components with dashes
		row[-1] = new_date # replace the old date value with the newly-formatted date value
	end
	puts row[-1]
	year = Date.parse(row[-1]).year.to_s # parse the date values as dates and then extract year
 	sumByNeighborhood[year + " " + row[1].to_s] +=  row[-2].to_f # create key as year and neighborhood, and sum sales
 	sumByZip[year + " " + row[10].to_s] +=  row[-2].to_f # create key as year and zip, and sum sales
 	sumByBorough[year + " " + row[0].to_s] +=  row[-2].to_f # create key as year and zip, and sum sales
end

# CREATE CSV FOR NEIGHBORHOOD DATA
CSV.open("sum-data-neighborhood.csv", "wb") do |csv| # create and edit a new csv file
  sumByNeighborhood.each do |key, val| # iterate through each key-value pair
  	csv << [key[0..3], key[5..-1], val] # insert each key-value pair as additional rows
  end
end


# CREATE CSV FOR ZIP CODE DATA
CSV.open("sum-data-zip.csv", "wb") do |csv| # create and edit a new csv file
  sumByZip.each do |key, val| # iterate through each key-value pair
  	csv << [key[0..3], key[5..-1], val] # insert each key-value pair as additional rows
  end
end

# CREATE CSV FOR BOROUGH DATA
CSV.open("sum-data-borough.csv", "wb") do |csv| # create and edit a new csv file
  sumByBorough.each do |key, val| # iterate through each key-value pair
  	csv << [key[0..3], key[5..-1], val] # insert each key-value pair as additional rows
  end
end
