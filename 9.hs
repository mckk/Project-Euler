
res = head (filter (\(a,b,c) -> a+b+c == 1000) triplets)
triplets = [(a,b,sqrt(a^2+b^2)) | a<-[1.0,2.0..1000], b<-[a..1000]]