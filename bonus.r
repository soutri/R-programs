spies=1
non_spies=3
total=spies+non_spies
c3=0
spies_old=0
non_spies_old=0
total_old=0
ns=kill(spies,non_spies)
spies=spies_left(spies)
non_spies=non_spies_left(ns)
total=total_left(spies,non_spies)
tt=judgement_senate(total,spies,non_spies)
print(tt)


spies_left<-function(s)
{
spies_old=spies
spies=s
return(spies)
}

non_spies_left<-function(ns)
{
  non_spies_old=non_spies
  non_spies=ns
  return(non_spies)
}
total_left<-function(s,ns)
{
  total_old=total
  total= spies+non_spies
  return(total)
}

day2<-function(s,ns,tot,k,a)
{
  if(total-total_old != 0)
  {
    if(spies_old-spies==0 && non_spies_old-non_spies!=0)
    {
      return(k*(ns/tot))
      
    }
    
    else if(spies_old-spies!=0 && non_spies_old-non_spies==0)
    {
      return(ns*(s/tot))
    }
    
    else if(spies_old-spies!=0 && non_spies_old-non_spies!=0)
      
    {
      return((a*(spies/tot))*(abs(k-a)*(non_spies/tot)))
      
      
    }
        
    
  }
  else
  {
    print("done")
  }
    
  
  
}

kill<-function(s,ns)
{
  ns=ns-s
  return(ns)
  
}

check<-function(s,ns)
{
  if(s<spies && ns<non_spies)
  {
    return(1)
  }
  else
  {
    return(0)
  }
}

judgement_senate<-function(tot1,sp,ns)
{
  #for(j in 1:tot1-1)
  {j=1
  
                  spies=sp-j
                  non_spies=ns
                  spies=spies_left(spies)
                  non_spies=non_spies_left(ns)
                  total=total_left(spies,non_spies)
                  c1=day2(spies,non_spies,total,j,0)
                  print(c1)
                  
        
                  non_spies=ns-j
                  spies=sp
                  spies=spies_left(spies)
                  non_spies=non_spies_left(ns)
                  total=total_left(spies,non_spies)
                  c2=day2(spies,non_spies,total,j,0)
                  print(c2)
                  
                 if(j==1)
                  {
                    c3=0          
                
                  }
            
                  else{
                    for(a in 1:j-1)
                    {
                      non_spies=ns-a
                      spies=sp-(j-a)
                      spies=spies_left(spies)
                      non_spies=non_spies_left(ns)
                      total=total_left(spies,non_spies)
                      c3=c3+day2(spies,non_spies,total,j,a)
                      
                      
                    }
                    print(c3)
                  }
        

    
  }
  
  return(c1+c2+c3)
}
