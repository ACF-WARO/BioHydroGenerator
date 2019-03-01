      program PondMonitor 
      
ccccccccccccccccccccccccccccccccccccccccccccccc
c Erwann Fillol, Action Contre la Faim (2017) c
ccccccccccccccccccccccccccccccccccccccccccccccc      


      
      implicit none

      integer*4 nbpixel,nbline,
     c          nbPondMax,year1,month1,decad1,yearMax,
     c          nbYearMax,StartExcel,AdmLevelMax,
c     c          sizehdr32R,
     c          SizeTiffR,OffTiffR,
     c          nbIDmax

     
      real*4    Lat0,Lon0,PixelSD
      


c Parameters Windows      
      parameter (nbpixel=7841)
      parameter (nbline=3630)
      
c Parameters Geo
      parameter (Lat0=27.37946430)
      parameter (Lon0=-18.00446430)
      parameter (PixelSD=0.00892857143)

c Parameters Fichier Sortie
      parameter (SizeTiffR=113880745)
      parameter (OffTiffR=14528)
      
c Parameters Data Base
      parameter (year1=1998)
c      parameter (decad1=11)
      parameter (decad1=1)
      parameter (yearMax=2100)

c Parameters Memory Alloc
      parameter (nbPondMax=999)
      parameter (nbYearMax=30)
      parameter (AdmLevelMax=5)
      parameter (nbIDmax=10000)
      
c Parameters Write
      Parameter (StartExcel=35796)


      
      
      
      integer*1 out8(nbpixel,nbline),
     c          buff8(2),Buff8R(4),    
     c          TiffR(SizeTiffR)
     
      integer*2 buff16 
      
      integer*4 i,j,nbPond,decad,ddeb,mdeb,Adm,o,poso,
     c          year,nbyeartot,nbdecadtot,yearStartFlag,
     c          yearLast,decadLast,nbYmin,KeepOld,
     c          decadS,yearS,nbdecadS,buff32S, 
     c          nbDmin,decadDeb,decadFin,nbdecadY,errorstat,
     c          nbDminOld,decadDebOld,decadFinOld,nbyeartotOld,
     c          decadi,decadj,yeari,vall,Pond,month,nbyearStart,decadf,
     c          IDPond(nbPondMax),AdmLevel(nbPondMax),
     c          AdmCode(nbPondMax),percent,nbdecadtotP,
     c          AdmIn(0:AdmLevelMax,nbpixel,nbline),
     c          imin(nbPondMax),imax(nbPondMax),
     c          jmin(nbPondMax),jmax(nbPondMax),
     c          nbPosMean(nbPondMax),
     c          in32(nbpixel,nbline),
     c          nbDremp(nbpixel,nbline),
     c          nbTremp(nbpixel,nbline),
     c          nbStackMean(nbPondMax,36),
     c          DateExcel(nbYearMax*36),
     c          DecadMeanMin(nbPondMax),decadMeanMax(nbPondMax),
     c          DecadMin(nbPondMax),decadMax(nbPondMax),
     c          IDstart(0:AdmLevelMax),IDnb(0:AdmLevelMax),
     c          IDi,IdAdmM,flag,Nb,
     c          IDAdm(nbIDmax),  
     c          IDimin(nbIDmax),
     c          IDimax(nbIDmax),
     c          IDjmin(nbIDmax),
     c          IDjmax(nbIDmax),
     c          IDnbStackMean(nbIDmax,36),
     c          time1(8),time2(8)    

      real*4    Latmin,Latmax,Lonmin,Lonmax,lat,Lon,
     c          PixelSizeMap(nbpixel,nbline),
     c          BuffR,pcYmin,
     c          LMonthB(12),LMonthNB(12),Edate,LastEdate,
     c          nbYremp(nbpixel,nbline),
     c          StackMeanMin(nbPondMax),StackMeanMax(nbPondMax),
     c          StackMin(nbPondMax),StackMax(nbPondMax),
     c          Summ,Summ1,Summ2,
     c          SumX,sumY,S2X,S2Y,SXY,
     c          diffH     

      real*4    Stack(nbPondMax,nbYearMax*36),
     c          StackMean(nbPondMax,36),
     c          PosiMean(nbPondMax),PosjMean(nbPondMax),
     c          IDStack(nbIDmax,nbYearMax*36),
     c          IDStackMean(nbIDmax,36),
     c          IDSurf(nbIDmax),     
     c          MeanAdm(nbIDmax),
     c          AnomAdm(nbIDmax,nbYearMax),
     c          R2Adm(nbIDmax),TrendAdm(nbIDmax),SigmaAdm(nbIDmax)
      
      character*100 filename,filename1,filename2,
     c              filenameHDR32R,filenameOut,
     c              IDName(nbIDmax)      
      character*10  NamePond(nbPondMax)
      character*8   decadnameAAAAMMDD(nbYearMax*36)
      character*4   numer4(0:9999),back4
      character*3   numer3(0:999),back3      
      character*3   MonthName(12)
      character*2   numer2(0:99),back2
      character*1   numer1(0:9),back1       
      character*4   decadnameMMDD(36)
      logical       filexist
      equivalence   (buff8,buff16)
      equivalence   (buff8R,buffR)
      
      

      
      
      do i=0,9999
         numer4(i)=
     c   char(int(i/1000)-10*int(i/10000)+48)//
     c   char(int(i/100)-10*int(i/1000)+48)//
     c   char(int(i/10)-10*int(i/100)+48)//
     c   char(int(i/1)-10*int(i/10)+48)
      enddo 

       do i=0,999
         numer3(i)=
     c   char(int(i/100)-10*int(i/1000)+48)//
     c   char(int(i/10)-10*int(i/100)+48)//
     c   char(int(i/1)-10*int(i/10)+48)
      enddo      

      do i=0,99
         numer2(i)=
     c   char(int(i/10)-10*int(i/100)+48)//
     c   char(int(i/1)-10*int(i/10)+48)
      enddo

      do i=0,9
         numer1(i)=
     c   char(int(i/1)-10*int(i/10)+48)
      enddo       
      
      
      data MonthName /'Jan','Feb','Mar',
     c                'Apr','May','Jun',
     c                'Jul','Aug','Sep',
     c                'Oct','Nov','Dec'/
     
      data LMonthB   /10.5,09.5,10.5,10.0,
     c                10.5,10.0,10.5,10.5,
     c                10.0,10.5,10.0,10.5/
     
      data LMonthNB  /10.5,09.0,10.5,10.0,
     c                10.5,10.0,10.5,10.5,
     c                10.0,10.5,10.0,10.5/     

   
cccccccccccccccccc      
c Initialisation c
cccccccccccccccccc    

       call date_and_time(VALUES=time1)
     
      filename='Lib/tmp/PondMonitor_Report.txt'
      open(9,file=filename)

      call system('cls')
      write(*,*) '*********************************'
      write(*,*) '*      PondMonitor (v2.0)       *'
      write(*,*) '* Action Contre la Faim (ACF-E) *'
      write(*,*) '*        Erwann Fillol (2017)   *'
      write(*,*) '*        erwann.fillol@gmail.com*'
      write(*,*) '*********************************'
      write(*,*)   
      write(*,'(a,$)') ' Initialisation   '
      
      write(9,*) '*********************************'
      write(9,*) '*      PondMonitor (v2.0)       *'
      write(9,*) '* Action Contre la Faim (ACF-E) *'
      write(9,*) '*        Erwann Fillol (2017)   *'
      write(9,*) '*        erwann.fillol@gmail.com*'
      write(9,*) '*********************************'
      write(9,*)
      write(9,*) 'Heure             :',
     c           time1(5),':',time1(6),':',time1(7)
      write(9,*) 'Date              :',
     c           time1(3),'/',time1(2),'/',time1(1)
      write(9,*)
      write(9,'(a,$)') ' Initialisation   '      

      
      Stack=0.
      IDStack=0.
      StackMean=0.
      IDStackMean=0.
      nbStackMean=0
      nbPosMean=0 
      IDsurf=0.
      buff16=0
      PosiMean=0.
      PosjMean=0.    
      
      back4=char(8)//char(8)//char(8)//char(8)
      back3=char(8)//char(8)//char(8)
      back2=char(8)//char(8) 
      back1=char(8)
      
      
      decadi=0
      do month=1,12
         do decad=1,21,10
            decadi=decadi+1
            decadnameMMDD(decadi)=numer2(month)//numer2(decad)
         enddo
      enddo         
            
      
      decadi=0
      EDate=StartExcel
      LastEdate=4.5
      do year=year1,nbYearMax+year1-1
         do month=1,12
            do decad=1,21,10
               if (decad.eq.1) then
                  Edate=Edate+LastEdate
               endif
               if (decad.eq.11) then
                  Edate=Edate+10.
               endif
               if (decad.eq.21) then
                  if (4*int(1.*year/4.).eq.year) then
                     Edate=Edate+LMonthB(month)
                     LastEdate=LMonthB(month)
                  else
                     Edate=Edate+LMonthNB(month)
                     LastEdate=LMonthNB(month)
                  endif
               endif
               decadi=decadi+1
               DateExcel(decadi)=nint(EDate)
               decadnameAAAAMMDD(decadi)=
     c         numer4(year)//numer2(month)//numer2(decad)
            enddo
         enddo
      enddo
      
c Lecture Fichiers Auxillaires
      filenameHDR32R='Lib\Ancillary\HDR\32R.hdr'


cccccccccccccccccccccccc
c Lecture PixelSizeMap c      
cccccccccccccccccccccccc      
      filename='Lib/Ancillary/Img/PixelSizeMap.img'
      open(10,file=filename,access='direct',
     crecl=4*nbpixel*nbline)
      read(10,rec=1) PixelSizeMap
      close(10)
    
cccccccccccccccccccccccccc
c Lecture fichiers Admin c
c et reperage des codes  c
cccccccccccccccccccccccccc

c Lecture fichier
      IDi=1
      do Adm=0,AdmLevelMax
         if (Adm.le.2) then
            filename='Lib/Ancillary/Img/ADM_'//
     c      numer1(Adm)//'.img'
            filename2='Lib/Ancillary/Img/ADM_'//
     c      numer1(Adm)//'.txt'
         else
            filename='Lib/Ancillary/Img/GEO_'//
     c      numer1(Adm)//'.img'
            filename2='Lib/Ancillary/Img/GEO_'//
     c      numer1(Adm)//'.txt'     
         endif
         open(10,file=filename,access='direct',
     c   recl=4*nbpixel*nbline)
         read(10,rec=1) in32
         close(10)
         AdmIn(Adm,:,:)=in32(:,:)         
         
         open(10,file=filename2)
         read(10,*) IDnb(Adm)
         if (IDnb(Adm).gt.0) then
            IDstart(Adm)=IDi
            do i=1,IDnb(Adm)
               read(10,*) IDAdm(IDi),
     c         IDimin(IDi),IDimax(IDi),
     c         IDjmin(IDi),IDjmax(IDi),
     c         IDSurf(IDi),IDName(IDi)
               IDi=IDi+1
            enddo
         endif
         close(10)
         
      enddo         

      
ccccccccccccccccccccccccccccccccc
c Lecture Fichier Geotiff Sorti c
ccccccccccccccccccccccccccccccccc      
      
      filename='Lib/Ancillary/Tiff/SuSa32R.tif'
      open(10,file=filename,access='direct',status='old',recl=SizeTiffR)
      read(10,rec=1) TiffR
      close(10)
      
      
ccccccccccccccccccccccccccccccc
c Lecture fichiers Parametres c
ccccccccccccccccccccccccccccccc

      filename='Param/PondMonitor_Param.txt'
      open(10,file=filename,form='formatted')
      read(10,*)
      read(10,*) pcYmin,nbDmin
      read(10,*) decadDeb,decadFin
      read(10,*) KeepOld
      close(10)
      
      if (pcYmin.lt.0.) then
         pcYmin=0.
      endif
      if (pcYmin.gt.100.) then
         pcYmin=100.
      endif
      
      if (decadDeb.gt.36) then
         decadDeb=36
      endif
      if (decadDeb.lt.0) then
         decadDeb=0
      endif
      if (decadFin.gt.36) then
         decadFin=36
      endif
      if (decadFin.lt.0) then
         decadFin=0
      endif
      
      if (decadFin.lt.decadDeb) then
         nbdecadY=36-(decadDeb-DecadFin)+1
      else
         nbdecadY=decadFin-decadDeb+1
      endif
      
c Effacage anciens fichiers
      if (KeepOld.ne.1) then
         call system('del /Q /S Data\Out\Water\Map\* > '// 
     c   'Lib\Cmd\OutPipe.txt')
         call system('copy /Y Lib\Ancillary\Tmp\tmp.txt '//
     c   'Data\Out\Water\Report\Water.tmp > '//
     c   'Lib\Cmd\OutPipe.txt')      
         call system('del /Q /S Data\Out\Water\Report\Water* > '// 
     c   'Lib\Cmd\OutPipe.txt')
         call system('copy /Y Lib\Ancillary\Tmp\tmp.txt '//
     c   'Data\Out\Water\Report\Pond.tmp > '//
     c   'Lib\Cmd\OutPipe.txt')       
         call system('del /Q /S Data\Out\Water\Report\Pond* > '// 
     c   'Lib\Cmd\OutPipe.txt')
         call system('copy /Y Lib\Ancillary\Tmp\tmp.txt '//
     c   'Data\Out\Water\Shape\Water.tmp > '//
     c   'Lib\Cmd\OutPipe.txt')       
         call system('del /Q /S Data\Out\Water\Shape\Water* > '// 
     c   'Lib\Cmd\OutPipe.txt')
      endif
      
      
cccccccccccccccccccccccccc      
c Lecture et verif liste c
cccccccccccccccccccccccccc

      nbPond=0
      filename='Param/PondMonitor_List.txt'
      open(10,file=filename,form='formatted')
      read(10,*)
10    nbPond=nbPond+1
      read(10,*,end=20) IDPond(nbPond),
     c                  Latmin,Latmax,Lonmin,Lonmax,
     c                  AdmLevel(nbPond),AdmCode(nbPond),
     c                  NamePond(nbPond)
      
      imin(nbPond)=nint((Lonmin-Lon0)/pixelSD)+1
      imax(nbPond)=nint((Lonmax-Lon0)/pixelSD)+1
      jmin(nbPond)=nint((Lat0-Latmax)/pixelSD)+1
      jmax(nbPond)=nint((Lat0-Latmin)/pixelSD)+1
      
      if (imin(nbPond).gt.imax(nbPond)) then
         buff32S=imin(nbPond)
         imin(nbPond)=imax(nbPond)
         imax(nbPond)=buff32S
      endif
      if (jmin(nbPond).gt.jmax(nbPond)) then
         buff32S=jmin(nbPond)
         jmin(nbPond)=jmax(nbPond)
         jmax(nbPond)=buff32S
      endif
      if (imin(nbPond).lt.1) then
         imin(nbPond)=1
      endif
      if (imax(nbPond).gt.nbpixel) then
         imax(nbPond)=nbpixel
      endif
      if (jmin(nbPond).lt.1) then
         jmin(nbPond)=1
      endif
      if (jmax(nbPond).gt.nbline) then
         jmax(nbPond)=nbline
      endif   
      
      if (AdmLevel(nbPond).lt.0) then
         AdmLevel(nbPond)=0
      endif
      
      if (AdmLevel(nbPond).gt.2) then
         filename='Lib/Ancillary/Img/GEO_'//
     c   numer1(AdmLevel(nbPond))//'.img'
         inquire(file=filename,exist=filexist)
         if (.not.filexist) then
            goto 20
         endif
      endif
      
c Reperage coordonnée i j dans le cas d'un choix "adm"      
      if ((AdmCode(nbPond).ne.0).and.
     c   (Lonmin.eq.0.).and.(Lonmax.eq.0.).and.
     c   (Latmin.eq.0.).and.(Latmax.eq.0.)) then
         Adm=AdmLevel(nbPond)
         do IDi=IDstart(Adm),IDstart(Adm)+IDnb(Adm)-1
            if (AdmCode(nbPond).eq.IDadm(IDi)) then
               imin(nbPond)=IDimin(IDi)
               imax(nbPond)=IDimax(IDi)
               jmin(nbPond)=IDjmin(IDi)
               jmax(nbPond)=IDjmax(IDi)
            endif
         enddo
      endif  
 
      goto 10
      
20    close(10)
      nbPond=nbPond-1
      if (nbPond.eq.0) then
         write(*,*) ': Erreur ! Aucun point detecte'
         write(9,*) ': Erreur ! Aucun point detecte'
         goto 9999
      endif
      
      write(*,*) ': OK'
      write(*,*)
      write(*,*) 'Nombre points     :',nbPond
      write(9,*) ': OK'
      write(9,*)
      write(9,*) 'Nombre points     :',nbPond      
      
      
c Test fichiers de sortie      
      do Pond=1,nbPond
         filename='Data/Out/Water/Report/Pond_'//
     c   numer4(IDPond(Pond))//'.csv'
         open(10,file=filename,form='formatted',err=99,iostat=errorstat)        
         write(10,*) NamePond(Pond)
         close(10)
      enddo 

      do Adm=0,AdmLevelMax
         if (Adm.le.2) then
            filename='Data/Out/Water/Report/Water_ADM_'//
     c      numer1(Adm)//'.csv'
         else
            filename='Data/Out/Water/Report/Water_GEO_'//
     c      numer1(Adm)//'.csv'  
         endif
         open(10,file=filename,form='formatted',err=99,iostat=errorstat)        
         write(10,*) Adm
         close(10)
      enddo     

99    if (errorstat.ne.0) then
         write(*,*) 'Fichier ouvert    : ',filename
         write(*,*) 'Veuillez Fermer et Relancer.'
         write(9,*) 'Fichier ouvert    : ',filename
         write(9,*) 'Veuillez Fermer et Relancer.'         
         goto 9999
      endif     
  
cccccccccccccccccccccccccccccc
c Calcul nb decad disponible c
cccccccccccccccccccccccccccccc
      
c Recherche premiere decad
      yearS=year1
      decadS=decad1
      nbdecadS=0
      nbyeartot=0
      nbyearStart=0     

4     filename='Data/In/SWB/SWB_'
     c//numer4(yearS)//decadnameMMDD(decadS)//'.img'
      inquire(file=filename,exist=filexist)
      if (.not.filexist) then
         decadS=decadS+1
         nbdecadS=nbdecadS+1         
         if (decadS.eq.37) then
            decadS=1
            yearS=yearS+1
            if (yearS.gt.yearMax) then
               write(*,*) ': Erreur ! Aucun fichier detecte.'
               write(9,*) ': Erreur ! Aucun fichier detecte.'
               goto 9999
            endif
         endif
         goto 4
      endif
      if (decadS.gt.decaddeb) then
         nbyearStart=nbyearStart+1
      endif


c Recherche decad suivante
      nbdecadtot=nbdecadS
      year=yearS
      decad=decadS

5     filename='Data/In/SWB/SWB_'
     c//numer4(year)//decadnameMMDD(decad)//'.img'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         decadLast=decad
         yearLast=year
         nbdecadtot=nbdecadtot+1
         
         if (decad.eq.decaddeb) then
            yearStartFlag=1
            nbyearStart=nbyearStart+1
         endif
         if ((decad.eq.decadfin).and.(yearStartFlag.eq.1)) then
            nbyeartot=nbyeartot+1
            yearStartFlag=0
         endif
         
         decad=decad+1
         if (decad.eq.37) then
            decad=1
            year=year+1
         endif
         goto 5
      endif

c Affichage
      write(*,*) 'Integration       : ',
     cdecadnameMMDD(decadDeb),' > ',decadnameMMDD(decadFin)     
      write(*,*) 'Decades           : ',
     cnumer4(yearS)//decadnameMMDD(decadS),' > ',
     cnumer4(yearLast)//decadnameMMDD(decadLast)
      write(*,*) 'Nombre de decades :',nbdecadtot-nbdecadS
      write(*,*) 'Annees            : '
     c   //numer4(year1)//' > '//numer4(year1+nbyearStart-1)
      write(*,*)
      
      write(9,*) 'Integration       : ',
     cdecadnameMMDD(decadDeb),' > ',decadnameMMDD(decadFin)     
      write(9,*) 'Decades           : ',
     cnumer4(yearS)//decadnameMMDD(decadS),' > ',
     cnumer4(yearLast)//decadnameMMDD(decadLast)
      write(9,*) 'Nombre de decades :',nbdecadtot-nbdecadS
      write(9,*) 'Annees            : '
     c   //numer4(year1)//' > '//numer4(year1+nbyearStart-1)      
      write(9,*)      
      
C       write(*,*) 'Nombre year start :',nbyearStart
C       write(*,*) 'Nombre year tot :',nbyeartot
C       write(*,*)
  
ccccccccccccccccccc      
c Calcul MareYear c
ccccccccccccccccccc

      write(*,'(a,a,$)') ' Calcul du masque  :      ',back2
      write(9,'(a,$)') ' Calcul du masque  :'     

c Récupération Ancien Fichier c
      filename='Data/Out/Raw/Water/Mask/WaterYear.old'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         open(10,file=filename)
         read(10,*) nbDminOld,nbyeartotOld,decadDebOld,decadFinOld
         close(10)
         
         if ((nbDmin.eq.nbDminOld).and.
     c       (nbyeartot.eq.nbyeartotOld).and.
     c       (decadDeb.eq.decadDebOld).and.
     c       (decadFin.eq.decadFinOld)) then
     
            filename='Data/Out/Raw/Water/Mask/WaterYear.img'
            open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
            read(10,rec=1) nbYremp
            close(10)
            
            filename='Data/Out/Raw/Water/Mask/WaterPourcent.img'
            open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
            read(10,rec=1) nbTremp
            close(10)            
            
            write(*,'(a,a,$)') back3,'OK'
            goto 25
         endif
      endif



      nbYremp=0.
      nbTremp=0
      nbdecadtotP=0
      buff16=0
      do year=1,nbyeartot
         nbDremp=0
         do decad=1,nbdecadY
            percent=
     c      (100.*((1.*year-1)*nbdecadY+1.*decad)/
     c      (nbyeartot*nbdecadY))
            write(*,'(a,I3.3,a,a,$)') back3,percent,' %',back2
            decadi=decad+decaddeb-1
            yeari=year+year1-1
            if (decadi.gt.36) then
               decadi=decadi-36
               yeari=yeari+1
            endif
            nbdecadtotP=nbdecadtotP+1
            filename='Data/In/SWB/SWB_'
     c      //numer4(yeari)//decadnameMMDD(decadi)//'.img'  
	         inquire(file=filename,exist=filexist)
            if (filexist) then
               open(10,file=filename,access='direct',status='old',
     c         recl=1*nbpixel*nbline)
               read(10,rec=1) out8
               close(10)
            else
               out8=0
            endif
            do i=1,nbpixel
               do j=1,nbline
                  buff8(1)=out8(i,j)
                  vall=buff16
                  if ((vall.ne.0).and.(vall.ne.255)) then          
                     nbDremp(i,j)=nbDremp(i,j)+1
                     nbTremp(i,j)=nbTremp(i,j)+1
                  endif
               enddo
            enddo
         enddo
         do i=1,nbpixel
            do j=1,nbline
               if (nbDremp(i,j).ge.nbDmin) then
                  nbYremp(i,j)=nbYremp(i,j)+1.
               endif
            enddo
         enddo
      enddo
      
c Ecriture Mask Raw c
      filename='Data/Out/Raw/Water/Mask/WaterYear.img'
      open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
      write(10,rec=1) nbYremp
      close(10)
      filename='Data\Out\Raw\Water\Mask\WaterYear.hdr'
      call system('copy '//filenameHDR32R//filename//
     c'> Lib\Cmd\OutPipe.txt') 
      filename='Data/Out/Raw/Water/Mask/WaterYear.old'
      open(10,file=filename)
      write(10,*) nbDmin,nbyeartot,decadDeb,decadFin
      close(10)
      filename='Data/Out/Raw/Water/Mask/WaterPourcent.img'
      open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
      write(10,rec=1) nbTremp
      close(10)
      filename='Data\Out\Raw\Water\Mask\WaterPourcent.hdr'
      call system('copy '//filenameHDR32R//filename//
     c'> Lib\Cmd\OutPipe.txt') 
      
c Ecriture Tiff Frequency c
25    do i=1,nbpixel
         do j=1,nbline
            buffR=100.*nbYremp(i,j)/nbyeartot
            if (buffR.lt.pcYmin) then
               buffR=0.
               nbYremp(i,j)=0
            endif
            do o=1,4
               poso=o+(i-1)*4+(j-1)*nbpixel*4
               TiffR(poso+OffTiffR)=buff8R(o)
            enddo
         enddo
      enddo       
      filename='Data/Out/Water/Map/Frequency.tif'
      open(10,file=filename,access='direct',
     crecl=SizeTiffR)
      write(10,rec=1) TiffR
      close(10)

c Ecriture Tiff Period c      
      do i=1,nbpixel
         do j=1,nbline
            if (nbYremp(i,j).ne.0) then
               buffR=100.*nbTremp(i,j)/nbdecadtotP
            else
               buffR=0.
            endif
            do o=1,4
               poso=o+(i-1)*4+(j-1)*nbpixel*4
               TiffR(poso+OffTiffR)=buff8R(o)
            enddo
         enddo
      enddo       
      filename='Data/Out/Water/Map/Period.tif'
      open(10,file=filename,access='direct',
     crecl=SizeTiffR)
      write(10,rec=1) TiffR
      close(10)      
      
      write(*,*)
      write(9,*) 'OK'

      
cccccccccccccccccccccccccccccccccccccccccccccccccccc
c Lecture et cumul sur chaque Pond et chaque Admin c
cccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      
      write(*,'(a,a,$)') ' Lecture           :      ',back2
      write(9,'(a,$)') ' Lecture           :'
      yeari=year1
      decadi=decad1
      nbYmin=nint(nbyeartot*pcYmin/100.)
      buff16=0
      do decad=1,nbdecadtot
         percent=nint((100.*decad)/nbdecadtot)
         write(*,'(a,I3.3,a,a,$)') back3,percent,' %',back2
         
         
         filename='Data/In/SWB/SWB_'
     c   //numer4(yeari)//decadnameMMDD(decadi)//'.img'
	      inquire(file=filename,exist=filexist)
         if (filexist) then
            open(10,file=filename,access='direct',status='old',
     c      recl=1*nbpixel*nbline)
            read(10,rec=1) out8
            close(10)

         
c Pond 
            do Pond=1,nbPond  
               do i=imin(Pond),imax(Pond)
                  do j=jmin(Pond),jmax(Pond)
               
                     buff8(1)=out8(i,j)
                     vall=buff16
                  
                     if ((vall.ne.0).and.(vall.ne.255)) then                  
                        if (((AdmIn(AdmLevel(Pond),i,j)
     c                     .eq.AdmCode(Pond))
     c                     .or.(AdmCode(Pond).eq.0))
     c                     .and.(nbYremp(i,j).ge.nbYmin)) then
                     
                           PosiMean(Pond)=PosiMean(Pond)+1.*i
                           PosjMean(Pond)=PosjMean(Pond)+1.*j
                           nbPosMean(Pond)=nbPosMean(Pond)+1
                     
                           Stack(Pond,decad)=Stack(Pond,decad)+
     c                     0.01*PixelSizeMap(i,j)

                        endif
                     endif
                  enddo
               enddo
            enddo
         
c Admin
            do Adm=0,AdmLevelMax
               do IDi=IDstart(Adm),IDstart(Adm)+IDnb(Adm)-1
                  do i=IDimin(IDi),IDimax(IDi)
                     do j=IDjmin(IDi),IDjmax(IDi)
                        
                        buff8(1)=out8(i,j)
                        vall=buff16
                        
                        if ((vall.ne.0).and.(vall.ne.255)                  
     c                  .and.(AdmIn(Adm,i,j).eq.IDAdm(IDi))
     c                  .and.(nbYremp(i,j).ge.nbYmin)) then
                              IDStack(IDi,decad)=
     c                        IDStack(IDi,decad)+
     c                        0.01*PixelSizeMap(i,j)
                        endif
                     enddo
                  enddo
               enddo
            enddo
            

            
            
         else
            Stack(:,decad)=-9999.
            IDStack(:,decad)=-9999.
         endif     
     
     
         decadi=decadi+1
         if (decadi.eq.37) then
            decadi=1
            yeari=yeari+1
         endif
         
      enddo
           
      Stack(:,nbdecadtot+1:nbYearMax*36)=-9999.
      IDStack(:,nbdecadtot+1:nbYearMax*36)=-9999.
      
      write(*,*)
      write(9,*) 'OK'


cccccccccccccccccccccccc      
c Calcul Année Moyenne c
cccccccccccccccccccccccc

      write(*,'(a,$)') ' Calcul           '
      write(9,'(a,$)') ' Calcul           '
  
      decadi=0
      do decad=1,nbdecadtot
         decadi=decadi+1
         if (decadi.eq.37) then
            decadi=1
         endif
 
c Somme sur Pond 
         do Pond=1,nbPond
            if (Stack(Pond,decad).ne.-9999.) then
               StackMean(Pond,decadi)=
     c         StackMean(Pond,decadi)+Stack(Pond,decad)
               nbStackMean(Pond,decadi)=
     c         nbStackMean(Pond,decadi)+1
            endif     
         enddo

c Somme sur Admin         
         do Adm=0,AdmLevelMax
            do IDi=IDstart(Adm),IDstart(Adm)+IDnb(Adm)-1
               if (IDStack(IDi,decad).ne.-9999.) then
                  IDStackMean(IDi,decadi)=
     c            IDStackMean(IDi,decadi)+
     c            IDStack(IDi,decad)
                  IDnbStackMean(IDi,decadi)=
     c            IDnbStackMean(IDi,decadi)+1
               endif
            enddo
         enddo        
      enddo
 
c Moyenne sur Pond 
      do Pond=1,nbPond
         do decad=1,36
            if (nbStackMean(Pond,decad).ne.0) then
               StackMean(Pond,decad)=
     c         StackMean(Pond,decad)/nbStackMean(Pond,decad)
            else
               StackMean(Pond,decad)=-9999.
            endif
         enddo
      enddo

c Moyenne sur Admin      
      do Adm=0,AdmLevelMax
         do IDi=IDstart(Adm),IDstart(Adm)+IDnb(Adm)-1
            do decad=1,36
               if (IDnbStackMean(IDi,decad).ne.0) then
                  IDStackMean(IDi,decad)=
     c            IDStackMean(IDi,decad)/
     c            IDnbStackMean(IDi,decad)
               else
                  IDStackMean(IDi,decad)=-9999.
               endif
            enddo
         enddo
      enddo 
      
cccccccccccccccccccccccccccccc      
c Statistiques Année Moyenne c
c Sur Pond (min, max, mean)  c
cccccccccccccccccccccccccccccc

      do Pond=1,nbPond
         StackMeanMax(Pond)=StackMean(Pond,1)
         StackMeanMin(Pond)=StackMean(Pond,1)
         DecadMeanMax(Pond)=1
         DecadMeanMin(Pond)=1
         do decad=2,36
            if (StackMean(Pond,decad).gt.StackMeanMax(Pond)) then
               StackMeanMax(Pond)=StackMean(Pond,decad)
               DecadMeanMax(Pond)=decad
            endif
            if (StackMean(Pond,decad).lt.StackMeanMin(Pond)) then
               StackMeanMin(Pond)=StackMean(Pond,decad)
               DecadMeanMin(Pond)=decad
            endif            
         enddo
      enddo    
      
ccccccccccccccccccccccccccccccc      
c Statistiques Periode totale c
ccccccccccccccccccccccccccccccc

      do Pond=1,nbPond
         StackMax(Pond)=-1./0
         StackMin(Pond)=+1./0
         DecadMax(Pond)=0
         DecadMin(Pond)=0
         do decad=1,nbdecadtot
            if (Stack(Pond,decad).ge.StackMax(Pond)) then
               StackMax(Pond)=Stack(Pond,decad)
               DecadMax(Pond)=decad
            endif
            if (Stack(Pond,decad).lt.StackMin(Pond)) then
               StackMin(Pond)=Stack(Pond,decad)
               DecadMin(Pond)=decad
            endif            
         enddo
      enddo 
      
      write(*,*) ': OK'
      write(9,*) ': OK'
      
      
ccccccccccccccccccccccccccccccccc
c Calcul Statistiques sur Admin c
ccccccccccccccccccccccccccccccccc
      SigmaAdm(:)=9999.
      TrendAdm(:)=-9999.
      R2Adm(:)=-9999.
      do Adm=0,AdmLevelMax
         do IDi=IDstart(Adm),IDstart(Adm)+IDnb(Adm)-1

            decadi=decadDeb-1
            Summ=0.
            Nb=0
            do decad=1,nbdecadY
               decadi=decadi+1
               if (decadi.eq.37) then
                  decadi=decadi-36
               endif
               
               if (IDStackMean(IDi,decadi).ne.-9999.) then
                  Summ=Summ+IDStackMean(IDi,decadi)
                  Nb=Nb+1
               endif
            enddo
            if (Nb.ne.0) then
               MeanAdm(IDi)=Summ/Nb
            else
               MeanAdm(IDi)=-9999.
            endif
 
c calcul anomalie 
            do year=1,nbyearStart
               decadi=decadDeb-1
               decadj=decadDeb-1+(year-1)*36
               Summ1=0.
               Summ2=0.
               Nb=0
               do decad=1,nbdecadY
                  decadi=decadi+1
                  decadj=decadj+1
                  if (decadi.eq.37) then
                     decadi=decadi-36
                  endif
                  if ((IDStack(IDi,decadj).ne.-9999.).and.
     c               (IDStackMean(IDi,decadi).ne.-9999.)) then
                     Summ1=Summ1+
     c               IDStack(IDi,decadj)
                     Summ2=Summ2+
     c               IDStackMean(IDi,decadi)
                     Nb=Nb+1
                  endif
               enddo
               if (Nb.ne.0) then
                  if (Summ2.ne.0) then
                     AnomAdm(IDi,year)=Summ1/Summ2
                  else
                     AnomAdm(IDi,year)=-0.01
                  endif
               else
                  AnomAdm(IDi,year)=-9999.
               endif          
            enddo
            
c calcul Sigma, Trend, R2
            nb=0.
            sumX=0.
            sumY=0.
            do year=1,nbyearStart
               if (AnomAdm(IDi,year).ge.0.) then
                  nb=nb+1.
                  sumX=sumX+1.*year
                  sumY=sumY+AnomAdm(IDi,year)
               endif
            enddo
            if (nb.gt.1.) then
               S2X=0.
               S2Y=0.
               SXY=0.
               sumX=sumX/nb
               sumY=sumY/nb
               do year=1,nbyearStart
                  if (AnomAdm(IDi,year).ge.0.) then
                     S2X=S2X+(1.*year-sumX)**2
                     S2Y=S2Y+(AnomAdm(IDi,year)-sumY)**2
                     SXY=SXY+(1.*year-sumX)*(AnomAdm(IDi,year)-sumY)
                  endif
               enddo
               
               if (S2Y.gt.0.) then
                  SigmaAdm(IDi)=sqrt(S2Y/nb)
               
                  S2X=S2X/(nb-1)
                  S2Y=S2Y/(nb-1)
                  SXY=SXY/(nb-1)
                    
                  TrendAdm(IDi)=SXY/S2X
               
                  R2Adm(IDi)=(SXY**2)/(S2X*S2Y)
               else
                  R2Adm(IDi)=-99.98
                  TrendAdm(IDi)=-99.98
                  SigmaAdm(IDi)=-99.98
               endif
            else
               R2Adm(IDi)=-0.01
               TrendAdm(IDi)=-0.01
               SigmaAdm(IDi)=-0.01
            endif               
         enddo
      enddo

               
      
cccccccccccc      
c Ecriture c
cccccccccccc

      write(*,'(a,$)') ' Ecriture          :'
      write(9,'(a,$)') ' Ecriture          :'

c Pond
      do Pond=1,nbPond
         filename='Data/Out/Water/Report/Pond_'//
     c   numer4(IDPond(Pond))//'.csv'
         open(10,file=filename,form='formatted')
         
         write(10,'(a,a,a)') 'Name;',NamePond(Pond),';'
         
         if (nbPosMean(Pond).ne.0) then
            Lat=Lat0-
     c      pixelSD*((PosjMean(Pond)/nbPosMean(Pond))-1.)
            Lon=Lon0+
     c      pixelSD*((PosiMean(Pond)/nbPosMean(Pond))-1.)            
         else
            Lat=-9999.
            Lon=-9999.
         endif
         
         write(10,'(a,F10.3,a)') 'Lat[ddN];',Lat,';'
         write(10,'(a,F10.3,a)') 'Lon[ddE];',Lon,';'
         
         write(10,'(a,F10.2,a)') 
     c   'Max[sqkm];',StackMeanMax(Pond),';'
         write(10,'(a,F10.2,a)') 
     c   'Min[sqkm];',StackMeanMin(Pond),';'        
         
c         write(10,'(a,F10.2,a,a,a)') 
c     c   'Mean_Max[sqkm];',StackMeanMax(Pond),
c     c   ';dekad;',decadnameMMDD(DecadMeanMax(Pond)),';'
c         write(10,'(a,F10.2,a,a,a)') 
c     c   'Mean_Min[sqkm];',StackMeanMin(Pond),
c     c   ';dekad;',decadnameMMDD(DecadMeanMin(Pond)),';'
     
c         write(10,'(a,F10.2,a,a,a)') 
c     c   'Max[sqkm];',StackMax(Pond),
c     c   ';dekad;',decadnameAAAAMMDD(DecadMax(Pond)),';'
c         write(10,'(a,F10.2,a,a,a)') 
c     c   'Min[sqkm];',StackMin(Pond),
c     c   ';dekad;',decadnameAAAAMMDD(DecadMin(Pond)),';'
         
         write(10,*) ';'
         write(10,*) ';'
         
         write(10,'(a,$)') ';Dekad;ExcelDate;Mean;'
         do year=year1,year1+nbyearStart-1
            write(10,'(a,a,$)') numer4(year),';'
         enddo
         write(10,*)
         
         decadi=decadDeb-1
         decadf=decadDeb-1
         do decad=1,nbdecadY
            decadf=decadf+1
            decadi=decadi+1
            if (decadi.eq.37) then
               decadi=decadi-36
            endif
            write(10,'(a,a,a,I5,$)') 
     c      ';',decadnameMMDD(decadi),';',DateExcel(decadf)
            write(10,'(a,F10.2,$)') ';',StackMean(Pond,decadi)
            do year=1,nbyearStart
               if ((year-1)*36+decadf.le.nbdecadtot) then
                  if (Stack(Pond,(year-1)*36+decadf).ne.-9999.) then
                     write(10,'(a,F10.2,$)') ';',
     c               Stack(Pond,(year-1)*36+decadf)
                  else
                     write(10,'(a,$)') ';'
                  endif
               endif
            enddo
            write(10,*) ';'
         enddo
         
         write(10,*) ';'
         write(10,*) ';'
         
         write(10,*) ';Dekad;ExcelDate;Surface[sqkm];'
         do decad=1,nbdecadtot
            if (Stack(Pond,decad).ne.-9999.) then
               write(10,'(a,a,a,I5,a,F10.2,a)') 
     c         ';',decadnameAAAAMMDD(decad),
     c         ';',DateExcel(decad),
     c         ';',Stack(Pond,decad),';'
            endif
         enddo
         close(10)
      enddo
      
      
c Admin
      do Adm=0,AdmLevelMax
c Ouverture fichier sortie
         if (Adm.le.2) then
            filenameOut='Data/Out/Water/Report/Water_ADM_'//
     c      numer1(Adm)//'.csv'
            filename='Lib/Ancillary/ADM_'//
     c      numer1(Adm)//'.img'
         else
            filenameOut='Data/Out/Water/Report/Water_GEO_'//
     c      numer1(Adm)//'.csv' 
            filename='Lib/User/GEO_'//
     c      numer1(Adm)//'.img'     
         endif
         open(11,file=filenameOut)
         write(11,'(a,a,a)') ';Source:;',filename,';'
         write(11,'(a,I10,a)') ';Nb_entities:;',IDnb(Adm),';'
         write(11,'(a)') ';Water Anomaly;[%];'
         write(11,'(a)') ';'
         write(11,'(a,$)') ';Name;ID;Area[sqkm];Mean[sqkm]'
         do year=1,nbyearStart
            write(11,'(a,$)') ';'//numer4(year+year1-1)
         enddo
C          write(11,'(a,$)') ';R2[%];Trend[%];Sigma[%]'
         write(11,'(a)') ';'
         
         
         
         
      
         do IDi=IDstart(Adm),IDstart(Adm)+IDnb(Adm)-1
            write(11,'(a,a,$)')     ';',IDName(IDi)
            write(11,'(a,I6,$)')    ';',IDAdm(IDi)
            write(11,'(a,I10,$)')   ';',nint(IDSurf(IDi))
            write(11,'(a,F10.2,$)') ';',MeanAdm(IDi)
            do year=1,nbyearStart         
               if (AnomAdm(IDi,year).lt.0.) then
                  AnomAdm(IDi,year)=-0.01
               endif     
               write(11,'(a,F10.2,$)') ';',100.*AnomAdm(IDi,year)
            enddo
C             write(11,'(a,F10.2,$)') ';',100.*R2Adm(IDi)
C             write(11,'(a,F10.2,$)') ';',100.*TrendAdm(IDi)
C             write(11,'(a,F10.2,$)') ';',100.*SigmaAdm(IDi)
            write(11,'(a)') ';'
         enddo
         close(11)
      enddo
      
c     Appel fonction matlab pour ecriture Shapefile
      filename='Lib\Bin\PondMonitor_csv2shp.exe'
      call SYSTEM(filename//
     c' > Lib\Cmd\Out_PondMonitor_csv2shp.txt')

         

      write(*,*) 'OK'
      write(9,*) 'OK'
 
       
9999  call date_and_time(VALUES=time2)
      diffH=time2(5)-time1(5)+
     c (1./60.)*(time2(6)-time1(6)+(1./60.)*(time2(7)-time1(7)))
      if (time1(3).ne.time2(3)) then
         diffH=diffH+24.
      endif
      
      write(*,*) 
      write(*,*) 'Temps execution   :',
     c int(diffH),'h',int(60.*(diffH-int(diffH))),'mm'
      write(*,*)
      write(*,'(a,$)') ' Operations terminees.'
      
      write(9,*) 'Temps execution   :',
     c int(diffH),'h',int(60.*(diffH-int(diffH))),'mm'
      write(9,*)
      write(9,*) 'Operations terminees.'
      close(9)
      
      filename1='Lib\tmp\PondMonitor_Report.txt'
      filename2='Data\Out\Water\Report\'
      call system('move '//filename1//filename2//
     c'> Lib\Cmd\OutPipe.txt')
      
      read(*,*)
      end 
      

      

            
            
      