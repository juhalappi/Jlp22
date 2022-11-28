#Laitoin jsub aliohjelmaan 3 argumenttia, eka pit‰‰ olla sinulla .false. niin et juutu J:n sis‰lle kun tulee vihre tai scripti loppuu, toka argumentti on scriptitiedoston nimi, ja jos sit‰ ei ole
#, niin etsit‰‰n j.par tiedostoa. (ain niin pit‰‰ laittaa virhe, jos sit‰ ei sinulla ole). Kolmas argumetti kertoo loppuuko ajo J:n hvirhetilateeseen.
#

JR<-function(input="j.par",output="JR.log",print=TRUE) {
      result<-.Fortran("j",as.character(input),FALSE)
#      if (result[[2]]) print("A call to j returned an error")
      if (print) a<-system("cat fort.6")
      file.rename("fort.6",output)
      list(error=result[[2]],output=output)
      }
      
#testi<-function(remain=FALSE,tulos=1) {
#      result<-.Fortran("testi",as.logical(remain),as.double(tulos))
#      result
#      }
      
      
# setwd("C:/Users/03191657/OneDrive - Valtion/work")