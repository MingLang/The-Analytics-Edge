tmp = read.csv("do_filename (0)")

CountryQuaterly = data.frame(tmp$Origin.Country[1:(nrow(tmp)/96)],tmp$Destination.Country[1:(nrow(tmp)/96)])
for (i in 1:32)
  {CountryQuaterly[,i+2]=rep(0,324)}


for(i in 3:34)
  {
  colnames(CountryQuaterly)[i]=paste("Q",(i-3)%%4+1,".",ceiling((i-2)/4)+2007,sep="")
  }
colnames(CountryQuaterly)[1:2]=c("From","To")
tmp$MonthNo = match(tmp$Month,month.abb)

for(i in 1:324)
  {for(j in 1:32)
    {CountryQuaterly[i,j+2]=sum(tmp$TEU[which(tmp$Origin.Country==CountryQuaterly$From[i]
                                              & tmp$Destination.Country==CountryQuaterly$To[i]
                                          &tmp$Year==(ceiling(j/4)+2007)
                                          & tmp$MonthNo>=(((j-1)%%4)*3+1)
                                          &tmp$MonthNo<=(((j-1)%%4)*3+3))])
    }
  }