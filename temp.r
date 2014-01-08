 a@Coords
 indicies=data.frame()
 for(k in 1:length(Lat)){
    for(m in 1:length(Lon)){
    for(g in 1:nrow(a@Coords)){
    if(Lon[m]==a@Coords[g,1] & Lat[k]==a@Coords[g,2]){
    indicies<-r
     if(nrow(indicies)==0)) indicies=cbind(m,k)
     else indicies<-rbind(indicies,cbind(m,k))
    }} }}