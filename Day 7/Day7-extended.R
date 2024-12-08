###This works from right to left, so it fails faster.  So much faster.


valid_concat_eq_back<-function(eq){
  ### start with the assumption that this is invalid
  ### this probably doesn't need to be a priority queue - but what the heck.
  pq<-priority_queue()
  ### start with the assumption that it is invalid
  valid<-FALSE
  ### put the elements into the queue
  pq$push(eq,0)
  while(pq$size()>0){
    e<-pq$pop()
    ### if e has length 2 check to see if it equals the target
    if(length(e)==2){
      if(e[1]==e[2]){
        ### if so, there's a valid answer - stop
        valid<-TRUE
        break
        ### otherwise keep checking
      }else{next}}
    ### as all of the numbers are positive and all operators can only make the number bigger
    ### if the first element is greater then target, no reason to continue
    if(e[2]>e[1]){next}
    ### used to find the last element of the equation
    leneq<-length(e)
    ### otherwise, add the first two elements and put the remainder into the queue
    eplus<-c(e[1]-e[leneq],e[2:(leneq-1)])
    pq$push(eplus,-eplus[1])
    ### or multiply them
    if(e[1]%%e[leneq]==0){
      emult<-c(e[1]/e[leneq],e[2:(leneq-1)])
      pq$push(emult,-emult[1])}
    ### or concatenate them
    if(e[1]%%(10^nchar(e[leneq]))==e[leneq]){
    econcat<-c(e[1]%/%(10^nchar(e[leneq])),e[2:(leneq-1)])
    pq$push(econcat,-econcat[1])}
    }
  valid}


input<-read_lines("../../AoCData/AOC2024/Day7.txt")
cal_eqs<-lapply(str_split(input, " |: "),as.numeric)

a<-Sys.time()
sum(sapply(cal_eqs,function(x){y<-valid_concat_eq(x)*x[1]}))
cat(Sys.time()-a)

a<-Sys.time()
sum(sapply(cal_eqs,function(x){y<-valid_concat_eq_back(x)*x[1]}))
cat(Sys.time()-a)
