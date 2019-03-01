      program HydroGenerator

ccccccccccccccccccccccccccccccccccccccccccccccc
c Erwann Fillol, Action Contre la Faim (2017) c
ccccccccccccccccccccccccccccccccccccccccccccccc
      
      implicit none

      integer*4 nblineblock,nbdecadmax,nbblock,
     c          year1,month1,decad1,nbYearMin,decadn1,
     c          FiltreSizeSigma,nbyearmax,blocksize,
     c          OutRawT,OutRaw,KeepOld,
     c          loop,sizeDilat,sizeFilterMax,
     c          nbpixel,nbline,
     c          LastStop,yearStop,monthStop,decadStop,
     c          nbtiffoutmax,
     c          sizeTiff32R,offTiff32R,   
     c          AdmLevelMax,nbIDmax	 

      real*8    e,BufferMeanFactor,
     c          Lat0,Lon0,PixelSD,PixelSKm  
     

      
c Definition fenetre
      parameter (nbpixel=7841)
      parameter (nbline=3630) 
      
c Definition parametres fichiers sorties      
      parameter (sizeTiff32R=113880745)      
      parameter (offTiff32R=14528) 
      
c Parameters Geo
      parameter (Lat0=27.379464)
      parameter (Lon0=-18.004464)
      parameter (PixelSD=0.00892857)
      parameter (PixelSKm=0.9728653504)  
          
c Parametres de definition allocation memoire      
      parameter (nblineblock=10)
      parameter (nbyearmax=25)      
      parameter (nbdecadmax=36*nbyearmax)
      parameter (blocksize=nblineblock*nbpixel)
      parameter (nbblock=nbline/nblineblock)
      parameter (nbtiffoutmax=nbyearmax*2+1)

c Parametres de la 1ere decade
      parameter (year1=1998)
      parameter (month1=1)
      parameter (decad1=1)
      parameter (decadn1=1)
      
c Parametres de la Dernière decade (LastStop : 1=oui, 0=non)
      parameter (LastStop=0)
      parameter (yearStop=2000)
      parameter (monthStop=01)
      parameter (decadStop=01)      

c Sorties Reelles (1=oui, 0=non)
C       parameter (OutRawT=1)
C       parameter (OutRaw=1)
C       parameter (KeepOld=1)
      
c Parametres filtrage temporel
c      parameter (nbYearMin=4)
      parameter (nbYearMin=2)
      
c Parametres Dilatation Mask
c      parameter (sizeDilat=2)
      parameter (sizeDilat=0)
	  
	  
c Parameters Memory Alloc
      parameter (AdmLevelMax=5)
      parameter (nbIDmax=100000)
      

c Parametres Buffering
      parameter (e=2.718281828)
      parameter (sizeFilterMax=200)
      
c Parametres Accessibilité Moyenne
      parameter (BufferMeanFactor=1.)





      integer*1     buff8S(2),out8(blocksize),
     c              buff8R(4),iError,
     c              Forage(nbpixel,nbline),
     c              ForageEq(blocksize*nbblock),
     c              mask(nbpixel,nbline),
     c              maskTM(nbpixel,nbline),
     c              maskTMEq(blocksize*nbblock),     
     c              maskDilat(nbpixel,nbline),
     c              maskPond(nbpixel,nbline),
     c              maskEq(blocksize*nbblock),    
     c              maskDilatEq(blocksize*nbblock),
     c              inSWB8(nbpixel,nbline),
     c              maskWater(nbpixel,nbline),
     c              Tiff32R(sizeTiff32R)
     

      integer*2     out16(blocksize),buff16S,
     c              SWBin(blocksize,nbdecadmax)

      integer*4     i,j,year,month,decad,FE,posi,poso,o,
     c              decadlast,monthlast,yearlast,nbdecadtot,
     c              decadnfirst,nbdecadfind,decadt,
     c              decadm,dd,decadd,decaddeb,valu,sizeFilter,
     c              decadyear(nbdecadmax),decadmonth(nbdecadmax),
     c              decaddecad(nbdecadmax),decadnum(nbdecadmax),
     c              bit,nbyearFlag,yearFlag,decadn,ForageFlag,
     c              nb,years(nbyearmax),nbyear,yeart,percent,
     c              ib,jb,tiffout,nbForage,
     c              in32(nbpixel,nbline),     
     c              is(-sizeFilterMax:sizeFilterMax),
     c              isb(-sizeDilat:sizeDilat),
     c              decadl,decaddebp,decadfinp,nbDecadSomme,
     c              typetiff(nbtiffoutmax),nbtiff,
     c              Adm,IDi,AdmIn(0:AdmLevelMax,nbpixel,nbline),
     c              IDstart(0:AdmLevelMax),IDnb(0:AdmLevelMax),     
     c              IDAdm(nbIDmax), 
     c              IDimin(nbIDmax),
     c              IDimax(nbIDmax),
     c              IDjmin(nbIDmax),
     c              IDjmax(nbIDmax),
     c              time1(8),time2(8)     

      real*4        outR(blocksize),
     c              SWBtype(blocksize,36),
     c              PixelSizeMap(nbpixel,nbline),
     c              FBackGround(nbpixel,nbline),P0,P1,      
     c              sSWBtype,sSWBin,buffR,
     c              Lon,Lat,RWater,
     c              MeanSWB(blocksize),MeanSWBType(blocksize),
     c              outImgR(nbpixel,nbline),
     c              SWBM(nbpixel,nbline),
     c              SWBR(nbpixel,nbline),
     c              SWBRLast(nbpixel,nbline),
     c              buff32R,diffH,
     c              MeanAdm(nbIDmax),
     c              AnomAdm(nbIDmax,nbyearmax),
     c              IDSurf(nbIDmax)
     
      real*8        f,nbR,moy,vall,R,vallP,maxi,
     c              Pond(-sizeFilterMax:sizeFilterMax,
     c                   -sizeFilterMax:sizeFilterMax)
     
      character*100 filename,filenameHDR32R,filenameHDR8U,
     c              filename1,filename2,filenameOut,
     c              filetiffin(nbtiffoutmax),
     c              filetiffou(nbtiffoutmax),
     c              filetiffcl(nbtiffoutmax),
     c              AdminIDname(nbIDmax)
     
      character*3   back3
      character*2   back2
      character*1   back1
      character*4   numer4(0:9999),ext,back4,decadnamep(72)
      character*2   numer3(0:999)
      character*2   numer2(0:99)
      character*1   numer1(0:9)      
      character*8   decadname,decadnames(nbdecadmax)
      logical       filexist
      equivalence   (buff8S,buff16S),
     c              (buff8R,buff32R),
     c              (maskDilat,maskDilatEq),
     c              (mask,maskEq),
     c              (maskTM,maskTMEq),
     c              (Forage,ForageEq)


      call date_and_time(VALUES=time1)
     
      filename='Lib/tmp/HydroGenerator_Report.txt'
      open(9,file=filename)      
     
      call system('cls')   
      write(*,*) '*********************************'
      write(*,*) '*     HydroGenerator (v3.0)     *'
      write(*,*) '* Action Contre la Faim (ACF-E) *'
      write(*,*) '*        Erwann Fillol (2017)   *'
      write(*,*) '*        erwann.fillol@gmail.com*'
      write(*,*) '*********************************'
      write(*,*)
      write(*,'(a,$)') ' Initialisation   '      

      write(9,*) '*********************************'
      write(9,*) '*     HydroGenerator (v3.0)     *'
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
      
      back4=char(8)//char(8)//char(8)//char(8)
      back3=char(8)//char(8)//char(8)
      back2=char(8)//char(8)
      back1=char(8)     
      
      buff16S=0
      buff32R=0.    
      AnomAdm(:,:)=0.
      MeanAdm(:)=0.

     

c Lecture Fichiers Auxillaires
      filenameHDR32R='Lib\Ancillary\HDR\32R.hdr'
      filenameHDR8U='Lib\Ancillary\HDR\8U.hdr'
      
      filename='Lib/Ancillary/Tiff/SuSa32R.tif'
      open(10,file=filename,access='direct',
     crecl=sizeTiff32R)
      read(10,rec=1) Tiff32R
      close(10)
	  
c Lecture PixelSizeMap         
C       filename='Lib/Ancillary/Img/PixelSizeMap.img'
C       open(10,file=filename,access='direct',
C      crecl=4*nbpixel*nbline)
C       read(10,rec=1) PixelSizeMap
C       close(10)
	  
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
     c         IDSurf(IDi),AdminIDname(IDi)
               IDi=IDi+1
            enddo
         endif
         close(10)
         
      enddo 	  


cccccccccccccccccccccccccccccccccccccccccc
c Initialisation nom decades integration c
cccccccccccccccccccccccccccccccccccccccccc
      month=1
      decad=1
      do decadl=1,72
         decadnamep(decadl)=numer2(month)//numer2(decad)
         decad=decad+10
         if (decad.eq.31) then
            decad=1
            month=month+1
            if (month.eq.13) then
               month=1
            endif
         endif
      enddo          

c Lecture fichier parametrage Buffering
      filename='Param/HydroGenerator_Param.txt'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         open(10,file=filename,status='old',
     c   form='formatted',err=98,iostat=iError)
         read(10,*)
         read(10,*) RWater
         if (RWater.lt.0.) then
            RWater=0.
         endif
         if (RWater.gt.60.) then
            RWater=60.
         endif       
c Fenetre temporelle
         read(10,*) decaddebp,decadfinp
         if (decaddebp.lt.1) then
             decaddebp=1
         endif
         if (decaddebp.gt.36) then
             decaddebp=36
         endif
         if (decadfinp.lt.1) then
             decadfinp=1
         endif
         if (decadfinp.gt.36) then
             decadfinp=36
         endif   
c         if (decaddebp.eq.decadfinp) then
c            decadfinp=decadfinp+1
c            if (decadfinp.eq.37) then
c               decadfinp=1
c            endif
c         endif

c Parametre forages
         read(10,*) ForageFlag 
C          read(10,*) P0,P1,ForageFlag
C          if (P0.lt.0.) then
C             P0=0.
C          endif
C          if (P0.gt.100.) then
C             P0=100.
C          endif
C          if (P1.lt.0.) then
C             P1=0.
C          endif
C          if (P1.gt.100.) then
C             P1=100.
C          endif  
C          P0=P0/100.
C          P1=P1/100. 

c Parametre Sorties
         read(10,*) OutRaw,KeepOld
         OutRawT=OutRaw

         close(10)
      else
         iError=9
      endif
      
98    if (iError.ne.0) then
         write(*,*) ': Erreur'
         write(*,*) 'Lecture parametrage impossible.'
         write(*,*) 'Verifier et Relancer.'
         
         write(9,*) ': Erreur'
         write(9,*) 'Lecture parametrage impossible.'
         write(9,*) 'Verifier et Relancer.'
         goto 9999
      endif 
      
c Lecture FBackGround
      filename='Lib/Ancillary/Img/FBackGround_AI.img'
      open(10,file=filename,access='direct',status='old',
     crecl=4*nbpixel*nbline)
      read(10,rec=1) FBackGround(:,:)
      close(10)
      FBackGround(:,:)=(P0-P1)*FBackGround(:,:)+P1

c Effacage anciens fichiers
      if (KeepOld.ne.1) then
         call system('del /Q /S Data\Out\Water\Access\* > '// 
     c   'Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Out\Raw\Water\Anomaly\* > '//
     c   'Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Out\Raw\Water\Buffer\* > '//
     c   'Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Out\Raw\Water\Type\SWB\* > '//
     c   'Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Out\Raw\Water\Mean\* > '//
     c   'Lib\Cmd\OutPipe.txt')
         call system('copy /Y Lib\Ancillary\Tmp\tmp.txt '//
     c   'Data\Out\Water\Report\Access.tmp > '//
     c   'Lib\Cmd\OutPipe.txt')    
         call system('del /Q /S Data\Out\Water\Report\Access* > '//
     c   'Lib\Cmd\OutPipe.txt') 
         call system('copy /Y Lib\Ancillary\Tmp\tmp.txt '//
     c   'Data\Out\Water\Shape\Access.tmp > '//
     c   'Lib\Cmd\OutPipe.txt')       
         call system('del /Q /S Data\Out\Water\Shape\Access* > '//
     c   'Lib\Cmd\OutPipe.txt')     
      endif 
      
      
c Initialisation des decades debut et fin integration
      if (decadfinp.lt.decaddebp) then
         decadfinp=decadfinp+36
      endif
      nbDecadSomme=decadfinp-decaddebp+1
      
c Lecture du Masque Terre/Mer
      filename='Lib/Ancillary/Img/LandMask.img'
      open(10,file=filename,access='direct',status='old',
     crecl=1*nbpixel*nbline)
      read(10,rec=1) MaskTM
      close(10)
      
      
c Initialisation des variables de buffering et de dilation du mask
      sizeFilter=nint(1.*RWater/PixelSkm+0.5)

      do jb=-sizeDilat,sizeDilat
         isb(jb)=nint((sizeDilat**2.-jb**2.)**0.5)
      enddo 
      
      do jb=-sizeFilter,sizeFilter
         is(jb)=nint((sizeFilter**2.-jb**2.)**0.5)
      enddo
      
      f=((1.*RWater/PixelSkm)**2.)/log(100.)
      do ib=-sizeFilter,sizeFilter
         do jb=-sizeFilter,sizeFilter
            R=ib*ib+jb*jb
            Pond(ib,jb)=e**(-R/f)
         enddo
      enddo
 
c Initialisation des decades supplementaires
      SWBin(:,:)=-9999 
      
      
c Carte des forages
      nbForage=0
      Forage(:,:)=0
      if (ForageFlag.eq.1)  then
         Forage(:,:)=0
         filename='Param/Bores_List.txt'
         open(10,file=filename)
         read(10,*)
18       read(10,*,iostat=iError) Lon,Lat
         if (iError.eq.0) then 
            i=nint((Lon-Lon0)/pixelSD)+1
            j=nint((Lat0-Lat)/pixelSD)+1
            if ((i.ge.1).and.(i.le.nbpixel).and.
     c          (j.ge.1).and.(j.le.nbline)) then
               Forage(i,j)=1
               nbForage=nbForage+1
            endif
            goto 18
         endif
         close(10)
      endif
      
      
      


cccccccccccccccccccccccccccccccccccccc      
c Determination du nombre de decades c
cccccccccccccccccccccccccccccccccccccc

      year=year1
      month=month1
      decad=decad1
      decadn=decadn1
      
      nbyear=0
      yeart=year-1
      nbdecadtot=0
      nbdecadfind=0
      decadnfirst=0
      
c Recherche première décade
9     decadname=numer4(year)//numer2(month)//numer2(decad)
      filename='Data/In/SWB/SWB_'//decadname//'.img'
      inquire(file=filename,exist=filexist)
      if (.not.(filexist)) then
         decadnfirst=decadnfirst+1
         nbdecadtot=nbdecadtot+1
         decadyear(nbdecadtot)=year
         decadmonth(nbdecadtot)=month
         decaddecad(nbdecadtot)=decad
         decadnum(nbdecadtot)=decadn 
         decadl=int(decad/10)+1+(month-1)*3
         if (decadl.eq.decaddebp) then
            yeart=yeart+1
            nbyear=nbyear+1
            years(nbyear)=yeart
         endif  

         decad=decad+10         
         if (decad.eq.31) then
            decad=1
            month=month+1
            if (month.eq.13) then
               month=1
               year=year+1
            endif
         endif

         decadn=decadn+1
         if (decadn.gt.36) then
            decadn=1
         endif
         
         if (.not.((year.eq.yearStop).and.(month.eq.monthStop).and.
     c  (decad.eq.decadStop).and.(LastStop.eq.1))) then
            goto 9
         endif    
      endif         
      

10    decadname=numer4(year)//numer2(month)//numer2(decad)
      filename='Data/In/SWB/SWB_'//decadname//'.img'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         yearlast=year
         monthlast=month
         decadlast=decad
         nbdecadtot=nbdecadtot+1
         nbdecadfind=nbdecadfind+1
         decadyear(nbdecadtot)=year
         decadmonth(nbdecadtot)=month
         decaddecad(nbdecadtot)=decad
         decadnum(nbdecadtot)=decadn
         

         decadl=int(decad/10)+1+(month-1)*3
         if (decadl.eq.decaddebp) then
            yeart=yeart+1
            nbyear=nbyear+1
            years(nbyear)=yeart
         endif  
         
         decad=decad+10         
         if (decad.eq.31) then
            decad=1
            month=month+1
            if (month.eq.13) then
               month=1
               year=year+1
            endif
         endif

         decadn=decadn+1
         if (decadn.gt.36) then
            decadn=1
         endif
         
         if (.not.((year.eq.yearStop).and.(month.eq.monthStop).and.
     c  (decad.eq.decadStop).and.(LastStop.eq.1))) then
            goto 10
         endif    
      endif         

c Initialisation du nom des decades pour lectures des fichiers
      do decad=1,nbdecadtot
         decadname=numer4(decadyear(decad))//
     c             numer2(decadmonth(decad))//
     c             numer2(decaddecad(decad))
         decadnames(decad)=decadname
      enddo
            

      if (nbdecadtot.ne.0) then
         write(*,*) ': OK'
         write(*,*)
         write(9,*) ': OK'
         write(9,*)         
      else
         write(*,*) ': Erreur !'
         write(*,*)
         write(*,*) 'Fichiers non-trouves !'
         write(*,*) 'Verifier la configuration'
         write(*,*) 'des fichiers et des repertoires.'
         write(9,*) ': Erreur !'
         write(9,*)
         write(9,*) 'Fichiers non-trouves !'
         write(9,*) 'Verifier la configuration'
         write(9,*) 'des fichiers et des repertoires.'         
         goto 9999
      endif
     
      write(*,'(a,F4.1,a)') 
     c ' Distance max      : ',RWater,' km'
      write(9,'(a,F4.1,a)') 
     c ' Distance max      : ',RWater,' km'     
     
C       write(*,*) 'FBackGround       : P0 =',
C      c   nint(100.*P0),back1,'% ; P1 =',
C      c   nint(100.*P1),back1,'%'
      
      if (ForageFlag.ne.0) then
          write(*,*) 
     c   'Nombre de forages :',nbForage
          write(9,*) 
     c   'Nombre de forages :',nbForage     
      endif
            
      
      write(*,*) 'Integration       : '
     c   //decadnamep(decaddebp)
     c   //' > '
     c   //decadnamep(decadfinp)
      write(*,*) 
      write(9,*) 'Integration       : '
     c   //decadnamep(decaddebp)
     c   //' > '
     c   //decadnamep(decadfinp)
      write(9,*)      
      
      write(*,*) 'Decades           : '
     c //decadnames(decadnfirst)//' > '//decadnames(nbdecadtot)
      write(*,*) 'Nombre de decades :',nbdecadfind
      write(*,*) 'Annees            : '
     c   //numer4(year1)//' > '//numer4(year1+nbyear-1)
      write(*,*)
      write(9,*) 'Decades           : '
     c //decadnames(decadnfirst)//' > '//decadnames(nbdecadtot)
      write(9,*) 'Nombre de decades :',nbdecadfind
      write(9,*) 'Annees            : '
     c   //numer4(year1)//' > '//numer4(year1+nbyear-1)
      write(9,*)      
      
ccccccccccccccccccccccccccccccccccccc
c Creation du mask des points d'eau c
ccccccccccccccccccccccccccccccccccccc

      write(*,'(a,a,$)') ' Creation du masque:      ',back2
      write(9,'(a,$)') ' Creation du masque:'     
      mask(:,:)=0
      maskDilat(:,:)=0
      do j=1,nbblock
         percent=nint(100.*(j-1.)/(nbblock-1.))
         write(*,'(a,I3.3,a,a,$)') back3,percent,' %',back2
         do decad=1,nbdecadtot

            if (((decadnum(decad).ge.decaddebp).and.
     c      (decadnum(decad).le.decadfinp)).or.
     c      ((decadfinp.gt.36).and.
     c      ((decadnum(decad)+36).le.decadfinp)))
     c      then    
               filename='Data/In/SWB/SWB_'//decadnames(decad)//'.img'
               inquire(file=filename,exist=filexist)
               if (filexist) then
                  open(10,file=filename,access='direct',status='old',
     c            recl=1*blocksize)
                  read(10,rec=j) out8
                  close(10)
               else
                  out8(:)=0
               endif
               do i=1,blocksize
                  buff8S(1)=out8(i)
                  vall=buff16S
                  if ((vall.eq.0).or.(vall.eq.255)) then
                     vall=0
                  else
                     vall=1
                  endif
                  if ((maskTMEq(i+(j-1)*blocksize).eq.2).or.
     c               (ForageEq(i+(j-1)*blocksize).eq.1)) then
                     vall=1
                  endif
                  SWBin(i,decad)=vall
               enddo
            endif
         enddo

c Filtrage         
         do i=1,blocksize
            nbyearFlag=0
            do year=1,nbyear
               yearFlag=0
               do decad=decaddebp,decadfinp-1
                  decadd=decad+(year-1)*36
                  if ((SWBin(i,decadd).gt.0.).and.
     c            (SWBin(i,decadd+1).gt.0.)) then
                     yearFlag=1
                  endif
               enddo
               if (yearFlag.eq.1) then
                  nbyearFlag=nbyearFlag+1
               endif
            enddo
            if (nbyearFlag.ge.nbYearMin) then
               maskEq(i+(j-1)*blocksize)=1
            endif
         enddo
      enddo
      

c Dilatation
      maskDilat(:,:)=mask(:,:)
      if (sizeDilat.ne.0) then
         do i=1+sizeDilat,nbpixel-sizeDilat
            do j=1+sizeDilat,nbline-sizeDilat
               if (mask(i,j).ne.0) then
                  do jb=-sizeDilat,sizeDilat
                     do ib=-isb(jb),isb(jb)
                        maskDilat(i+ib,j+jb)=maskTM(i+ib,j+jb)
                     enddo
                  enddo
               endif
            enddo
         enddo
      endif

c Ecriture des Masks Dilaté et non dilaté des points d'eau
      filename='Data/Out/Raw/Water/Mask/Mask_Dilat.img'
      open(10,file=filename,access='direct',
     c     recl=1*nbpixel*nbline)
      write(10,rec=1) maskDilat
      close(10)
      filename='Data\Out\Raw\Water\Mask\Mask_Dilat.hdr'
      call system('copy '//filenameHDR8U//filename//
     c'> Lib\Cmd\OutPipe.txt') 
      
      filename='Data/Out/Raw/Water/Mask/Mask.img'
      open(10,file=filename,access='direct',
     c     recl=nbpixel*nbline)
      write(10,rec=1) mask
      close(10)
      filename='Data\Out\Raw\Water\Mask\Mask.hdr'
      call system('copy '//filenameHDR8U//filename//
     c'> Lib\Cmd\OutPipe.txt')       
      
      maskPond(:,:)=mask(:,:)
      mask(:,:)=0
      
      write(9,*) 'OK'
    
c      goto 9999

c Boucle Principale sur les blocks
      write(*,*)
      write(*,'(a,a,$)') ' Calcul Anomalies  :      ',back2
      write(9,'(a,$)') ' Calcul Anomalies  :'      
      do j=1,nbblock
         percent=nint(100.*(j-1.)/(nbblock-1.))
         write(*,'(a,I3.3,a,a,$)') back3,percent,' %',back2

c Chargement des données SWB sur le block sur l'ensemble des decades
         do decad=1,nbdecadtot

            filename='Data/In/SWB/SWB_'//decadnames(decad)//'.img'
            inquire(file=filename,exist=filexist)
            if (filexist) then
               open(10,file=filename,access='direct',status='old',
     c         recl=1*blocksize)
               read(10,rec=j) out8
               close(10)

               do i=1,blocksize
                  buff8S(1)=out8(i)
                  vall=buff16S
                  if ((vall.eq.0).or.(vall.eq.255)) then
                     vall=0
                  else
                     vall=1
                  endif
                  if ((maskTMEq(i+(j-1)*blocksize).eq.2).or.
     c               (ForageEq(i+(j-1)*blocksize).eq.1)) then
                     vall=1
                  endif
                  SWBin(i,decad)=vall
               enddo
            else
               SWBin(:,decad)=-9999.
            endif
         enddo

         
c Filtrage temporel
         do i=1,blocksize
            if (maskDilatEq(i+(j-1)*blockSize).eq.0) then
               do decad=1,nbdecadtot
                  if (SWBin(i,decad).eq.1) then
                     maskEq(i+(j-1)*blocksize)=
     c               maskEq(i+(j-1)*blocksize)+1
                     SWBin(i,decad)=0.
                  endif
               enddo
            endif
         enddo
         


c Calcul de l'année type
         SWBtype(:,:)=0.
         do i=1,blocksize  
            do decad=1,36
               nb=0
               do year=1,nbyear+1
                  decadd=decad+(year-1)*36
                  if (SWBin(i,decadd).ne.-9999.) then
                     nb=nb+1
                     SWBtype(i,decad)=SWBtype(i,decad)+SWBin(i,decadd)
                  endif
               enddo
               if (nb.ne.0) then
                  SWBtype(i,decad)=SWBtype(i,decad)/nb
               else
                  SWBtype(i,decad)=-9999.
               endif
            enddo
         enddo
         
         if (OutRawT.eq.1) then
            do decad=1,36
               outR(:)=SWBtype(:,decad)
               filename='Data/Out/Raw/Water/'//
     c         'Type/SWB/SWB_'//numer2(decad)//'.img'
               open(10,file=filename,access='direct',recl=4*blocksize)
               write(10,rec=j) outR
               close(10)
            enddo
         endif


c Calcul de la moyenne pour chaque année complète
         do year=1,nbyear-1
            MeanSWB(:)=-9999.
            do i=1,blocksize
               nb=0
               sSWBin=0.
               do decadl=decaddebp,decadfinp
                  decadd=decadl+(year-1)*36
                  if (decadl.gt.36) then
                     decadt=decadl-36
                  else
                     decadt=decadl
                  endif
                  if (SWBin(i,decadd).ne.-9999.) then
                     sSWBin=sSWBin+SWBin(i,decadd)
                     nb=nb+1
                  else
                     if (SWBtype(i,decadt).ne.-9999.) then
                        sSWBin=sSWBin+SWBtype(i,decadt)
                        nb=nb+1
                     endif
                  endif
               enddo
               if (nb.ne.0) then
                  MeanSWB(i)=sSWBin/nb
               endif
            enddo

            filename='Data/Out/Raw/Water/Mean/Mean_'//
     c      numer4(years(year))//'.img'
            open(10,file=filename,access='direct',recl=4*blocksize)
            write(10,rec=j) MeanSWB
            close(10)           
         enddo
          
          
c Calcul de la moyenne pour l'année type complete
         MeanSWBType(:)=-9999.
         do i=1,blocksize
            nb=0
            sSWBtype=0.
            do decadl=decaddebp,decadfinp
               if (decadl.gt.36) then
                  decad=decadl-36
               else
                  decad=decadl
               endif
               if (SWBtype(i,decad).ne.-9999.) then
                  sSWBtype=sSWBtype+SWBtype(i,decad)
                  nb=nb+1
               endif
            enddo
            if (nb.ne.0) then
               MeanSWBType(i)=sSWBtype/nb
            endif
         enddo
            
         filename='Data/Out/Raw/Water/Mean/Mean_Mean.img'
         open(10,file=filename,access='direct',recl=4*blocksize)
         write(10,rec=j) MeanSWBType
         close(10)  
         
c Calcul de la moyenne pour l'année en cours

         year=nbyear
         MeanSWB(:)=-9999.
         MeanSWBType(:)=-9999.
         do i=1,blocksize
            nb=0
            sSWBin=0.
            sSWBtype=0.
            do decadl=decaddebp,decadfinp
               decadd=decadl+(year-1)*36
               decad=decadl
               if (decad.gt.36) then
                  decad=decad-36
               endif
               if ((SWBin(i,decadd).ne.-9999.).and.
     c            (SWBtype(i,decad).ne.-9999.)) then
                  sSWBin=sSWBin+SWBin(i,decadd)
                  sSWBtype=sSWBtype+SWBtype(i,decad)
                  nb=nb+1
               endif
            enddo
            if (nb.ne.0) then
               MeanSWB(i)=sSWBin/nb
               MeanSWBType(i)=sSWBtype/nb
            endif
         enddo

         filename='Data/Out/Raw/Water/Mean/Mean_'//
     c   numer4(years(year))//'.img'
         open(10,file=filename,access='direct',recl=4*blocksize)
         write(10,rec=j) MeanSWB
         close(10)
            
         filename='Data/Out/Raw/Water/Mean/Mean_Last.img'
         open(10,file=filename,access='direct',recl=4*blocksize)
         write(10,rec=j) MeanSWBType
         close(10)            
          
      enddo

cccccccccccccccccccccccc
c Ecriture fichier HDR c
cccccccccccccccccccccccc

      if (OutRawT.eq.1) then
         do decad=1,36
            filename='Data\Out\Raw\Water\'//
     c      'Type\SWB\SWB_'//numer2(decad)//'.hdr'
            call system('copy '//filenameHDR32R//filename//
     c      '> Lib\Cmd\OutPipe.txt') 
         enddo
      endif

      if (OutRaw.eq.1) then
         do year=1,nbyear
            filename='Data\Out\Raw\Water\Mean\Mean_'//
     c      numer4(years(year))//'.hdr'
            call system('copy '//filenameHDR32R//filename//
     c      '> Lib\Cmd\OutPipe.txt')
         enddo
         
         filename='Data\Out\Raw\Water\Mean\Mean_Mean.hdr'
         call system('copy '//filenameHDR32R//filename//
     c   '> Lib\Cmd\OutPipe.txt')

         filename='Data\Out\Raw\Water\Mean\Mean_Last.hdr'
         call system('copy '//filenameHDR32R//filename//
     c   '> Lib\Cmd\OutPipe.txt')
      endif     
      write(9,*) 'OK'
      

cccccccccccccccccccccccc
c Relecture, Buffering 
cccccccccccccccccccccccc

      write(*,*)
      write(*,'(a,a,$)') ' Buffering         :      ',back2
      write(9,'(a,$)') ' Buffering         :'         

c Buffer sur année type complète
cccccccccccccccccccccccccccccccc         
      filename='Data/Out/Raw/Water/Mean/Mean_Mean.img'
      open(10,file=filename,access='direct',
     crecl=4*nbpixel*nbline)
      read(10,rec=1) outImgR
      close(10)

      mask(:,:)=0
      SWBR(:,:)=-9999.
      do j=1,nbline
         do i=1,nbpixel
            if (outImgR(i,j).gt.0.) then
               maskWater(i,j)=1
            else
               maskWater(i,j)=0
            endif
         enddo
      enddo
      do j=1,nbline
         do i=1,nbpixel
            if (outImgR(i,j).gt.0.) then
               do jb=-sizeFilter,sizeFilter
                  do ib=-is(jb),is(jb)
                     if (((i+ib).ge.1).and.
     c                  ((i+ib).le.nbpixel).and.
     c                  ((j+jb).ge.1).and.
     c                  ((j+jb).le.nbline)) then
                           mask(i+ib,j+jb)=1
                     endif
                  enddo
               enddo
            endif
         enddo
      enddo
      do j=1,nbline
         do i=1,nbpixel
            if (maskTM(i,j).eq.0) then
               mask(i,j)=0
            endif
         enddo
      enddo   

      SWBR(:,:)=0
      do j=1,nbline
         percent=nint(100.*((j-sizeFilter)+
     c   0.*(nbline-2.*sizeFilter))/
     c   ((2.+nbyear)*(nbline-sizeFilter)))
         write(*,'(a,I3.3,a,a,$)') back3,percent,' %',back2
         do i=1,nbpixel         
            if (mask(i,j).ne.0) then
               nbR=0.
               moy=0.
c               maxi=0.
               do jb=-sizeFilter,sizeFilter
                  do ib=-is(jb),is(jb)
                     if (((i+ib).ge.1).and.
     c                  ((i+ib).le.nbpixel).and.
     c                  ((j+jb).ge.1).and.
     c                  ((j+jb).le.nbline)) then
                        vall=outImgR(i+ib,j+jb)
                        vallP=Pond(ib,jb)*vall
                        if (vall.ne.-9999.) then
                           moy=moy+Pond(ib,jb)*vall
                           nbR=nbR+Pond(ib,jb)
c                           maxi=max(vallP,maxi)
                        endif
                     endif
                  enddo
               enddo
               SWBR(i,j)=moy/nbR
c               SWBR(i,j)=maxi
            endif
         enddo
      enddo
C       do j=1,nbline
C          do i=1,nbpixel
C             if (maskTM(i,j).eq.0) then
C                SWBR(i,j)=-9999.
C             else
C                if (FBackGround(i,j).gt.SWBR(i,j)) then
C                   SWBR(i,j)=FBackGround(i,j)
C                endif
C             endif
C          enddo
C       enddo
      
      filename='Data/Out/Raw/Water/Buffer/Buffer_Mean.img'
      open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
      write(10,rec=1) SWBR
      close(10)
      if (OutRaw.eq.1) then   
         filename='Data\Out\Raw\Water\Buffer\'//
     c   'Buffer_Mean.hdr'
         call system('copy '//filenameHDR32R//filename//
     c   '> Lib\Cmd\OutPipe.txt')
      endif
      
c Admin
      do Adm=0,AdmLevelMax
         do IDi=IDstart(Adm),IDstart(Adm)+IDnb(Adm)-1
            nb=0
            do i=IDimin(IDi),IDimax(IDi)
               do j=IDjmin(IDi),IDjmax(IDi)
                  
                  vall=SWBR(i,j)
                  
                  if ((vall.gt.-9998.)                  
     c               .and.(AdmIn(Adm,i,j).eq.IDAdm(IDi))) then
                     MeanAdm(IDi)=MeanAdm(IDi)+vall
                     nb=nb+1
                  endif
               enddo
            enddo
            if (nb.gt.0) then
               MeanAdm(IDi)=MeanAdm(IDi)/nb
            else
               MeanAdm(IDi)=-9999.
            endif
         enddo
      enddo      
                      
         

         
c Buffer sur année type en cours
cccccccccccccccccccccccccccccccc         
      filename='Data/Out/Raw/Water/Mean/Mean_Last.img'
      open(10,file=filename,access='direct',
     crecl=4*nbpixel*nbline)
      read(10,rec=1) outImgR
      close(10)

      mask(:,:)=0
      SWBRlast(:,:)=-9999.
      do j=1,nbline
         do i=1,nbpixel
            if (outImgR(i,j).gt.0.) then
               maskWater(i,j)=1
            else
               maskWater(i,j)=0
            endif
         enddo
      enddo
      do j=1,nbline
         do i=1,nbpixel
            if (outImgR(i,j).gt.0.) then
               do jb=-sizeFilter,sizeFilter
                  do ib=-is(jb),is(jb)
                     if (((i+ib).ge.1).and.
     c                  ((i+ib).le.nbpixel).and.
     c                  ((j+jb).ge.1).and.
     c                  ((j+jb).le.nbline)) then
                        mask(i+ib,j+jb)=1
                     endif
                  enddo
               enddo
            endif
         enddo
      enddo
      do j=1,nbline
         do i=1,nbpixel
            if (maskTM(i,j).eq.0) then
               mask(i,j)=0
            endif
         enddo
      enddo
         
      SWBRlast(:,:)=0
      do j=1,nbline
         percent=nint(100.*((j-sizeFilter)+
     c   1.*(nbline-2.*sizeFilter))/
     c   ((2.+nbyear)*(nbline-2.*sizeFilter)))
         write(*,'(a,I3.3,a,a,$)') back3,percent,' %',back2
         do i=1,nbpixel           
            if (mask(i,j).ne.0) then
               nbR=0.
               moy=0.
c               maxi=0.
               do jb=-sizeFilter,sizeFilter
                  do ib=-is(jb),is(jb)
                     if (((i+ib).ge.1).and.
     c                  ((i+ib).le.nbpixel).and.
     c                  ((j+jb).ge.1).and.
     c                  ((j+jb).le.nbline)) then
                        vall=outImgR(i+ib,j+jb)
                        vallP=vall*Pond(ib,jb)
                        if (vall.ne.-9999.) then
                           moy=moy+vallP
                           nbR=nbR+Pond(ib,jb)
c                           maxi=max(maxi,vallP)
                        endif
                     endif
                  enddo
               enddo
               SWBRlast(i,j)=moy/nbR
c               SWBRlast(i,j)=maxi
            endif
         enddo
      enddo
C       do j=1,nbline
C          do i=1,nbpixel
C             if (maskTM(i,j).eq.0) then
C                SWBRlast(i,j)=-9999.
C             else
C                if (FBackGround(i,j).gt.SWBRlast(i,j)) then
C                   SWBRlast(i,j)=FBackGround(i,j)
C                endif
C             endif
C          enddo
C       enddo
         
      if (OutRaw.eq.1) then   
         filename='Data/Out/Raw/Water/Buffer/Buffer_Last.img'
         open(10,file=filename,access='direct',
     c   recl=4*nbpixel*nbline)
         write(10,rec=1) SWBRlast
         close(10) 

         filename='Data\Out\Raw\Water\Buffer\'//
     c   'Buffer_Last.hdr'
         call system('copy '//filenameHDR32R//filename//
     c   '> Lib\Cmd\OutPipe.txt')
      endif     
  
  
c Calcul de l'anomalie pour chaque année
cccccccccccccccccccccccccccccccccccccccc
         
      do year=1,nbyear
         if (year.eq.nbyear) then
            SWBR(:,:)=SWBRLast(:,:)
         endif
      
         filename='Data/Out/Raw/Water/Mean/Mean_'//
     c   numer4(years(year))//'.img'
         open(10,file=filename,access='direct',
     c   recl=4*nbpixel*nbline)
         read(10,rec=1) outImgR
         close(10)
         
         mask(:,:)=0
         do j=1,nbline
            do i=1,nbpixel
               if (outImgR(i,j).gt.0.) then
                  do jb=-sizeFilter,sizeFilter
                     do ib=-is(jb),is(jb)
                        if (((i+ib).ge.1).and.
     c                     ((i+ib).le.nbpixel).and.
     c                     ((j+jb).ge.1).and.
     c                     ((j+jb).le.nbline)) then
                           mask(i+ib,j+jb)=1
                        endif
                     enddo
                  enddo
               endif
            enddo
         enddo
         do j=1,nbline
            do i=1,nbpixel
               if (maskTM(i,j).eq.0) then
                  mask(i,j)=0
               endif
            enddo
         enddo
                  
         SWBM(:,:)=outImgR(:,:)         
         do j=1,nbline
            percent=nint(100.*((j-sizeFilter)+
     c      (1.+year)*(nbline-2.*sizeFilter))/
     c      ((2.+nbyear)*(nbline-2.*sizeFilter)))
            write(*,'(a,I3.3,a,a,$)') back3,percent,' %',back2
            do i=1,nbpixel         
               if (mask(i,j).ne.0) then
                  nbR=0.
                  moy=0.
c                  maxi=0.
                  do jb=-sizeFilter,sizeFilter
                     do ib=-is(jb),is(jb)
                        if (((i+ib).ge.1).and.
     c                     ((i+ib).le.nbpixel).and.
     c                     ((j+jb).ge.1).and.
     c                     ((j+jb).le.nbline)) then
                           vall=outImgR(i+ib,j+jb)
                           vallP=Pond(ib,jb)*vall
                           if (vall.ne.-9999.) then
                              moy=moy+Pond(ib,jb)*vall
                              nbR=nbR+Pond(ib,jb)
c                              maxi=max(maxi,vallP)
                           endif
                        endif
                     enddo
                  enddo
                  SWBM(i,j)=moy/nbR
c                  SWBM(i,j)=maxi  
               endif
            enddo
         enddo
C          do j=1,nbline
C             do i=1,nbpixel
C                if (maskTM(i,j).eq.0) then
C                   SWBM(i,j)=-9999.
C                else
C                   if (FBackGround(i,j).gt.SWBM(i,j)) then
C                      SWBM(i,j)=FBackGround(i,j)
C                   endif
C                endif
C             enddo
C          enddo 
         
         if (OutRaw.eq.1) then
c            filename='Data/Out/Raw/Water/Buffer/Mask_'//
c     c      numer4(years(year))//'.img'
c            open(10,file=filename,access='direct',
c     c      recl=1*nbpixel*nbline)
c            write(10,rec=1) mask
c            close(10)
            
c            filename='Data\Out\Raw\Water\Buffer\Mask_'//
c     c      numer4(years(year))//'.hdr'
c            call system('copy '//filenameHDR8U//filename//
c     c      '> Lib\Cmd\OutPipe.txt')        
         
            filename='Data/Out/Raw/Water/Buffer/Buffer_'//
     c      numer4(years(year))//'.img'
            open(10,file=filename,access='direct',
     c      recl=4*nbpixel*nbline)
            write(10,rec=1) SWBM
            close(10)
         
            filename='Data\Out\Raw\Water\Buffer\Buffer_'//
     c      numer4(years(year))//'.hdr'
            call system('copy '//filenameHDR32R//filename//
     c      '> Lib\Cmd\OutPipe.txt')
         endif

         do j=1,nbline
            do i=1,nbpixel
               if ((SWBM(i,j).ne.-9999.).and.(SWBR(i,j).ne.-9999.).and.
     c            (SWBR(i,j).ne.0.)) then
                  SWBM(i,j)=100.*SWBM(i,j)/SWBR(i,j)
               endif
               if (SWBR(i,j).eq.0.) then
                  SWBM(i,j)=-9998.
               endif
               if ((SWBM(i,j).eq.-9999.).or.(SWBR(i,j).eq.-9999.)) then
                  SWBM(i,j)=-9999.
               endif                  
            enddo
         enddo            
               
         filename='Data/Out/Raw/Water/Anomaly/'//
     c   'AnomalyExtend_'//numer4(years(year))//'.img'
         open(10,file=filename,access='direct',
     c   recl=4*nbpixel*nbline)
         write(10,rec=1) SWBM
         close(10)
         
         if (OutRaw.eq.1) then
            filename='Data\Out\Raw\Water\Anomaly\'//
     c      'AnomalyExtend_'//numer4(years(year))//'.hdr'
            call system('copy '//filenameHDR32R//filename//
     c      '> Lib\Cmd\OutPipe.txt')
         endif
         
         
c Admin
         do Adm=0,AdmLevelMax
            do IDi=IDstart(Adm),IDstart(Adm)+IDnb(Adm)-1
               nb=0
               do i=IDimin(IDi),IDimax(IDi)
                  do j=IDjmin(IDi),IDjmax(IDi)
                     
                     vall=SWBM(i,j)
                     
                     if ((vall.gt.-9998.)                  
     c               .and.(AdmIn(Adm,i,j).eq.IDAdm(IDi))) then
                        AnomAdm(IDi,year)=AnomAdm(IDi,year)+vall
                        nb=nb+1
                     endif
                  enddo
               enddo
               if (nb.gt.0) then
                  AnomAdm(IDi,year)=AnomAdm(IDi,year)/nb
               else
                  AnomAdm(IDi,year)=-9999.
               endif
            enddo
         enddo
         
c         do j=1,nbline
c            do i=1,nbpixel
c               mask(i,j)=250
c            enddo
c         enddo
c         do j=1,nbline
c            do i=1,nbpixel
c               valu=nint(100.*SWBM(i,j))
c               if (SWBM(i,j).eq.-2) then
c                  valu=250
c               else
c                  if (valu.lt.0) then
c                     valu=0
c                  endif
c                  if (valu.gt.200) then
c                     valu=200
c                  endif
c               endif
c               mask(i,j)=valu   
c            enddo
c         enddo  
c         filename='Data/Out/Raw/Water/Anomaly/'//
c     c   'AnomalyExtend8_'//numer4(years(year))//'.img'
c         open(10,file=filename,access='direct',
c     c   recl=1*nbpixel*nbline)
c         write(10,rec=1) mask
c         close(10) 

         do j=1,nbline
            do i=1,nbpixel
               if (maskPond(i,j).eq.0) then
c                  mask(i,j)=250
                  SWBM(i,j)=-9998.
               endif
               if (maskTM(i,j).eq.0) then
                  SWBM(i,j)=-9999.
               endif
            enddo
         enddo
         
         filename='Data/Out/Raw/Water/Anomaly/'//
     c   'Anomaly_'//numer4(years(year))//'.img'
         open(10,file=filename,access='direct',
     c   recl=4*nbpixel*nbline)
         write(10,rec=1) SWBM
         close(10)

         if (OutRaw.eq.1) then
            filename='Data\Out\Raw\Water\Anomaly\'//
     c      'Anomaly_'//numer4(years(year))//'.hdr'
            call system('copy '//filenameHDR32R//filename//
     c      '> Lib\Cmd\OutPipe.txt')
         endif     
         
c         filename='Data/Out/Raw/Water/Anomaly/'//
c     c   'Anomaly8_'//numer4(years(year))//'.img'
c         open(10,file=filename,access='direct',
c     c   recl=1*nbpixel*nbline)
c         write(10,rec=1) mask
c         close(10)  
         

         
         
      enddo
      write(9,*) 'OK'
      
cccccccccccccccccccccccccccccccccccc
c Relecture et re-écriture geotiff c
cccccccccccccccccccccccccccccccccccc
999   write(*,*)
      write(*,'(a,$)') ' Ecriture          :    '
      
      write(9,'(a,$)') ' Ecriture          :'
      
      nbtiff=1
      filetiffin(nbtiff)='Data/Out/Raw/Water/Buffer/Buffer_Mean.img'
      filetiffou(nbtiff)=
     c 'Data/Out/Water/Access/WaterAccess_Mean.tif'
      filetiffcl(nbtiff)='Data\Out\Raw\Water\Buffer\Buffer_Mean.*'
      typetiff(nbtiff)=1

      do year=1,nbyear
         ext=numer4(years(year))
      
C          nbtiff=nbtiff+1
C          filetiffin(nbtiff)='Data/Out/Raw/Water/Anomaly/Anomaly_'
C      c   //ext//'.img'
C          filetiffou(nbtiff)=
C      c   'Data/Out/Water/Access/Anomaly/WaterAccessAnomaly_'
C      c   //ext//'.tif'
C          filetiffcl(nbtiff)='Data\Out\Raw\Water\Anomaly\Anomaly_'
C      c   //ext//'.*'
C          typetiff(nbtiff)=2

         nbtiff=nbtiff+1
         filetiffin(nbtiff)='Data/Out/Raw/Water/Anomaly/AnomalyExtend_'
     c   //ext//'.img'
         filetiffou(nbtiff)=
     c   'Data/Out/Water/Access/AccessAnomaly_'
     c   //ext//'.tif'
         filetiffcl(nbtiff)='Data\Out\Raw\Water\Anomaly\AnomalyExtend_'
     c   //ext//'.*'
         typetiff(nbtiff)=2
      enddo         
      
      do tiffout=1,nbtiff
         percent=nint(100.*(tiffout-1.)/(1.*(nbtiff-1.)))
         write(*,'(a,I3.3,a,a,$)') back3,percent,' %',back2
		 
         filename=filetiffin(tiffout)
         open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
         read(10,rec=1) outImgR
         close(10)
         
c Limitation valeurs
c Anomaly %
         if (Typetiff(tiffout).eq.2) then
            do i=1,nbpixel
               do j=1,nbline
                  if (OutImgR(i,j).gt.200.) then
                     OutImgR(i,j)=200.
                  endif
               enddo
            enddo
         endif

c Mean
         if (Typetiff(tiffout).eq.1) then 
            do i=1,nbpixel
               do j=1,nbline
                  if (outImgR(i,j).ge.0.) then
                     outImgR(i,j)=BufferMeanFactor*outImgR(i,j)
                  endif
               enddo
            enddo
         endif  
         
c Ecriture Tiff      
         do i=1,nbpixel
            do j=1,nbline
               buff32R=outImgR(i,j)
               do o=1,4
                  poso=o+(i-1)*4+(j-1)*nbpixel*4
                  Tiff32R(poso+offTiff32R)=buff8R(o)
               enddo
            enddo
         enddo     
         filename=filetiffou(tiffout)
         open(10,file=filename,access='direct',
     c   recl=sizeTiff32R)
         write(10,rec=1) Tiff32R
         close(10)
         
c Effacement Fichier raw
         if (OutRaw.ne.1) then
            call system('del /Q '//filetiffcl(tiffout)// 
     c      '> Lib\Cmd\OutPipe.txt')
         endif         
         
      enddo    
      write(9,*) 'OK'
      
c Admin
      write(*,*)
      write(*,'(a,$)') ' Ecriture Shape    :'
      write(9,'(a,$)') ' Ecriture Shape    :'
      do Adm=0,AdmLevelMax
c Ouverture fichier sortie
         if (Adm.le.2) then
            filenameOut='Data/Out/Water/Report/Access_ADM_'//
     c      numer1(Adm)//'.csv'
            filename='Lib/Ancillary/ADM_'//
     c      numer1(Adm)//'.img'
         else
            filenameOut='Data/Out/Water/Report/Access_GEO_'//
     c      numer1(Adm)//'.csv' 
            filename='Lib/User/GEO_'//
     c      numer1(Adm)//'.img'     
         endif
         open(11,file=filenameOut)
         write(11,'(a,a,a)') ';Source:;',filename,';'
         write(11,'(a,I10,a)') ';Nb_entities:;',IDnb(Adm),';'
         write(11,'(a)') ';Access Anomaly;[%];'
         write(11,'(a)') ';'
         write(11,'(a,$)') ';Name;ID;Area[sqkm];Access Mean'
         do year=1,nbyear
            write(11,'(a,$)') 
     c      ';'//numer4(year+year1-1)
         enddo
         write(11,'(a)') ';'
      
         do IDi=IDstart(Adm),IDstart(Adm)+IDnb(Adm)-1
            write(11,'(a,a,$)')     ';',AdminIDname(IDi)
            write(11,'(a,I6,$)')    ';',IDAdm(IDi)
            write(11,'(a,I10,$)')   ';',nint(IDSurf(IDi))
            write(11,'(a,F10.8,$)') ';',MeanAdm(IDi)
            do year=1,nbyear        
               if (AnomAdm(IDi,year).lt.0.) then
                  AnomAdm(IDi,year)=-9999.
               endif     
               write(11,'(a,F10.2,$)') ';',AnomAdm(IDi,year)
            enddo
            write(11,'(a)') ';'
         enddo
         close(11)
      enddo
      
C     Appel fonction matlab pour ecriture Shapefile
      filename='Lib\Bin\HydroGenerator_csv2shp.exe'
      call SYSTEM(filename//
     c ' > Lib\Cmd\Out_HydroGenerator_csv2shp.txt')
     
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
      
      write(9,*)
      write(9,*) 'Temps execution   :',
     c int(diffH),'h',int(60.*(diffH-int(diffH))),'mm'
      write(9,*)
      write(9,*) 'Operations terminees.'
      close(9)

      filename1='Lib\tmp\HydroGenerator_Report.txt'
      filename2='Data\Out\Water\Report\'
      call system('move '//filename1//filename2//
     c'> Lib\Cmd\OutPipe.txt')      

      read(*,*)
      end 