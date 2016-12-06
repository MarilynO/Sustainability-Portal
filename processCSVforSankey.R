#The jsonlite package has functions for exporting data in JSON format
#install.packages("jsonlite")

#Load the libraries
library(jsonlite)

###  Load the data & prepare the containers for the new data:

# Bring up a dialog box to choose the file to convert for a Sankey Diagram:
myData <- read.csv(file.choose(),header = TRUE)  # miniSTARS.csv


numberOfColumns <- length(names(myData)) # This will be used when adding additional columns
currentNumberCode <- 0 # this will be used to assign codes for the link data

# create the empty dataframe which will be populated with the names & their ids:
nameData <- as.data.frame(data.frame("name"=character(0), "id"=character(0), "value"=numeric(0), stringsAsFactors=FALSE))

## create the empty dataframe which will be populated with the source-links and values:
linkData <- as.data.frame(data.frame("source"=character(0), "link"=character(0), "value"=numeric(0), stringsAsFactors=FALSE))

      
### Go through each value, give it a code and add it to the list of "names":

for(i in 1:numberOfColumns) {  # this loop goes through each column:
  
  # create a column filled with zeroes that will be replaced with the assigned codes (it will automatically be named V + the column number):
  myData[[numberOfColumns+i]] <- vector(mode="numeric", length=length(myData[[1]]))
  
  # sort alphabetically (to put all identical values next to each other)
#  myData <- myData[order(myData[[i]]), ]
  myData <-myData[ order(myData[,i], myData[,i+1]), ] #NOTE: the earlier version may be fine, the second column doesn't matter at this point
  # assign a number code for the first item in the column:
  myData[1, numberOfColumns+i] <- currentNumberCode
  
  # put the name of this item in the variable "currentName"...
  currentName <-myData[1,i] # store the current value
  # ...then add the name to the nameData dataframe along with the "id" (which is a version with spaces replaced by underscores)
  nameData <- rbind(nameData, data.frame("name" = currentName, "id" = sub(" ","_",currentName)))

  # Now that the first item in the column has been given a number code, loop through the rest and give them
  #    their number codes:
  for(j in 2:length(myData[[i]])) { # note that this loop starts with the second item in the list since the first has already been included
    
    # check to see whether this item is identical to the previous item: 
    if (myData[j,i] == currentName){
      # if it is the same item, give it the same number code:
      myData[j,numberOfColumns+i] <- currentNumberCode
    }else{
      # if it is different, give it a new number code:
      currentNumberCode <- currentNumberCode + 1
      myData[j,numberOfColumns+i] <- currentNumberCode
  
      currentName <- myData[j,i] # make the new name the "current name"
      # ...and add it to the nameData list:
      nameData <- rbind(nameData, data.frame("name" = currentName, "id" = sub(" ","_",currentName)))
    } # end of the test for whether the item is identical to the previous item
    
  }  # end of the loop through items in a column
     # if there is another column, continue the loop, otherwise exit it
  
  # now increment the code again in preparation for processing the next column:
  currentNumberCode <- currentNumberCode + 1

}# end of the loop through the colunns. Now the names dataframe has been populated and the codes have been added
# to the original dataframe

# Now populate the #linkData" dataframe

# Start by creating a dataframe that includes only the newly-added columns which contain the codes
codeColumns <- myData[,(numberOfColumns + 1):length(colnames(myData))]

# Loop through the pairs of columns
for(k in 1:(length(colnames(codeColumns))-1)) {  # do this loop for each pair of columns:
  # 'k' is the number of the first of the pair of columns

  currentValue <-2
  # sort by value to put identical values next to each other:
  codeColumns <-codeColumns[ order(codeColumns[,k], codeColumns[,k+1]), ]
   
  # as long as there is a "next row, check to see whether the next row is the same as the current row
   for(l in 1:(length(codeColumns[,1])-1)) {
        # 'l' is the current row number & 'k' is the current column number
        #  populate "source" and "target" values:
        currentSource <- codeColumns[l,k]
        currentTarget <- codeColumns[l,k+1]
        nextSource <- codeColumns[l+1,k]
        nextTarget <- codeColumns[l+1,k+1]
        if(currentSource == nextSource & currentTarget == nextTarget){ # if they're the same then...
          currentValue <- currentValue + 2  # increase the value...
        } else { #...but if they're different...
          # add a row to the dataframe and populate the "source", "target" and "value"
          linkData <- rbind(linkData, data.frame("source" = currentSource, "target" = currentTarget, "value" = currentValue))
          currentValue <- 2 # reset the value to "2"

        }  # end of loop through the column 

   } # end of loop through rows
  # now add the final values
  linkData <- rbind(linkData, data.frame("source" = nextSource, "target" = nextTarget, "value" = currentValue))
} # end loop through pairs of columns

# Now that both "linkData" and "nameData" have been populated, write out the JSON files:

linkDataJSON <- toJSON(linkData)
nameDataJSON <- toJSON(nameData)

# This lines let you specify an existing file that will be overwritten
write(linkDataJSON, file.choose()) 
write(nameDataJSON, file.choose())

# Once these files have been created, they need to be pasted together to create the file the Sankey code expects to see

