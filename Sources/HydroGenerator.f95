      program HydroGenerator

!cccccccccccccccccccccccccccccccccccccccccccccccccccc
!c Erwann Fillol, Action Contre la Faim v4.1 (2018) c
!cccccccccccccccccccccccccccccccccccccccccccccccccccc
      
! gfortran -static -fopenmp Sources\HydroGenerator.f95 -o HydroGenerator.exe     
      
      
      
      implicit none
      
      integer*8      nbpixel,nbline,&
                     yearFirst,monthFirst,dayFirst,&
                     LastStop,yearStop,monthStop,dayStop,&
                     AdmLevelMax,nbAdminmax,&
                     MemAvailableMin,maskTMapply,&
                     nbyearmax,nbtiffoutmax,&
                     nbThreadMaxMax
                     
      real*8         factorMem,factorThread1,&
                     Lat0,Lon0,PixelSD,PixelSKm,&
                     SeuilAccessType,AccessTypeMin   
      
      
      
      
!c Definition fenetre
      parameter (nbpixel=7841)
      parameter (nbline=3630)
      
! Parameters Geo
      parameter (Lat0=27.37946430)
      parameter (Lon0=-18.00446430)
      parameter (PixelSD=0.00892857143)
      parameter (PixelSKm=0.9728653504)       
      
!c Parametres de la 1ere decade
      parameter (yearFirst=1998)
      parameter (monthFirst=4)
      parameter (dayFirst=11)
      
!c Parametres de la Dernière decade (LastStop : 1=oui, 0=non)
      parameter (LastStop=1)
      parameter (yearStop=2022)
      !parameter (yearStop=2001)
      parameter (monthStop=12)
      parameter (dayStop=21)
      
! Parametres Decoupes Admin
      parameter (AdmLevelMax=5)
      parameter (nbAdminmax=2000)
      
! Parametres Memoire
      parameter (MemAvailableMin=1000000000)
      parameter (factorMem=1./2.)
      parameter (factorThread1=2./8.)
      parameter (nbThreadMaxMax=256)     
      
! Parametres Mask terre/mer
      parameter (maskTMapply=1)

! Alloc mémoire alloc
      parameter (nbyearmax=25)
      parameter (nbtiffoutmax=nbyearmax*4+4)

! Seuil
      parameter (SeuilAccessType=0.20)
      parameter (AccessTypeMin=0.01)   
      
      
      
! Déclaration variables & Allocation mémoire

      integer*8      MemAvailable,RWater,decaddebp,decadfinp,ForageFlag,&
                     OutOpt,Compress,Masking,OutRaw,KeepOld,Quiet,iError,&
                     year,month,day,decadFirst,decadLast,nbdecad,decad,&
                     yearLast,monthLast,dayLast,nbdecadtot,nbyeartot,yearEnd,&
                     yearStart,time1(8),time2(8),FinalOk,i,j,nbYearDetect,nbSuccDetect,&
                     nbblock,decadint,nbdecadint,blocksize,MemAllocNeed,nbdecadalloc,&
                     nblineblock,nbyearalloc,allocstat,percent,yearl,&
                     nbtiff,typetiff(nbtiffoutmax),filttiff(nbtiffoutmax),&
                     iminMaskOut,imaxMaskOut,jminMaskOut,jmaxMaskOut,nbpixelMaskOut,nblineMaskOut,&
                     tiffout,yeari,&
                     sizeHDR32R,filesize,&
                     AdminID(nbAdminmax),nbAdmin,&
                     AdminIDimin(nbAdminmax),AdminIDimax(nbAdminmax),&
                     AdminIDjmin(nbAdminmax),AdminIDjmax(nbAdminmax),&
                     nbAdminalloc,Adm,IDi,nbID,&
                     nbThreadMax,nbThread1,nbThread2,nbThread3,years(nbyearmax),&
                     nbPond,IDPondR,iminR,imaxR,jminR,jmaxR,iminRR,imaxRR,jminRR,jmaxRR,AdmLevelR,AdmCodeR,&
                     flagOk,autorun
                     
                     
      real*4         AdminIDsurf(nbAdminmax)
                     
      
      real*8         P0,P1,diffH,frac,latmaxMaskOut,lonminMaskOut,&
                     surfAccess,sumAdminAccessType,&
                     LatminR,LatmaxR,LonminR,LonmaxR
      
      integer*1, dimension(:), allocatable ::& !(sizeHDR32R)
                     HDR32R
      
      integer*1, dimension(:,:), allocatable ::& !(nbpixel,nbline)
                     MaskTM
                     
      integer*4, dimension(:,:), allocatable ::& !(nbpixel,nbline)
                     MaskOut,Admin
                     
      real*4, dimension(:,:),allocatable ::& !(blocksize,nbyeartot)
                     AnomAccessYear
                     
      real*4, dimension(:), allocatable ::& !(blocksize)
                     outBlock32R,SommeAccessType,SommeAccessYear,SommeAccessNb,AccessTypeNb
                     
      real*4, dimension(:,:), allocatable ::& !(nbdecadint,blocksize)
                     AccessTypeBlock
                     
      real*4, dimension(:,:), allocatable ::& !(nbpixel,nbline)
                     outImgMaskOut32R,outImg32R,ImgR1,ImgR2,PixelSizeMap
                     
      real*8, dimension(:,:), allocatable ::& !(nbAdminmax,nbyearmax)    
                     SumAdminAccessYear                    
      
      character*100  filename,filename1,filename2,filename3,&
                     output1,output2,output3,filenameHDR32R,&
                     filetiffin(nbtiffoutmax),filetiffou(nbtiffoutmax),filetiffcl(nbtiffoutmax),&
                     AdminIDname(nbAdminmax),filenameAdmin(AdmLevelMax),&
                     NamePondR
      
      character*8    decadname
      character*4    numer4(0:9999),ext
      character*2    numer2(0:99)
      character*1    numer1(0:9)
      
      character*1    back1
      character*2    back2
      character*3    back3
      character*4    back4
      character*8    back8
      character*11   back11
      
      character*4    decadnamep(72)

      logical        filexist
                  
      
      INTEGER OMP_GET_MAX_THREADS
      INTEGER OMP_GET_NUM_THREADS
      INTEGER OMP_GET_THREAD_NUM      
     
        
      
      call date_and_time(VALUES=time1)
      
      
      do i=0,9999
         numer4(i)=&
         char(int(i/1000)-10*int(i/10000)+48)//&
         char(int(i/100)-10*int(i/1000)+48)//&
         char(int(i/10)-10*int(i/100)+48)//&
         char(int(i/1)-10*int(i/10)+48)
      enddo     

      do i=0,99
         numer2(i)=&
         char(int(i/10)-10*int(i/100)+48)//&
         char(int(i/1)-10*int(i/10)+48)
      enddo

      do i=0,9
         numer1(i)=&
         char(int(i/1)-10*int(i/10)+48)
      enddo      
      
      back11=char(8)//char(8)//char(8)//char(8)//char(8)//char(8)//char(8)//char(8)//char(8)//char(8)//char(8)
      back8=char(8)//char(8)//char(8)//char(8)//char(8)//char(8)//char(8)//char(8)
      back4=char(8)//char(8)//char(8)//char(8)
      back3=char(8)//char(8)//char(8)
      back2=char(8)//char(8)
      back1=char(8) 
      
      
! Determination du nombre de thread
      nbThread1=1
      nbThread2=1
      nbThread3=1
      nbThreadMax=1
      !$ nbThreadMax=OMP_GET_MAX_THREADS()
      !$ call OMP_SET_NESTED(.TRUE.)
      nbThreadMax=min(nbThreadMax,nbThreadMaxMax)
      nbThread1=max(int(factorThread1*nbThreadMax),1)
      nbThread2=max(int((1.*nbThreadMax-1.)/(1.*nbThread1)),1)
      nbThread3=max(nbThreadMax-1,1)

! Mode d'affichage
      quiet=0
      filename='Lib\Tmp\AutoRun_Mode.txt'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         open(10,file=filename)
         read(10,*) autorun
         close(10)
         call system('del /Q Lib\Tmp\AutoRun_Mode.txt > Lib\Cmd\OutPipe.txt')
      else
         autorun=0
      endif     
      

! Affichage et Rapport Excusion      
      filename='Lib\tmp\HydroGenerator_Report.txt'
      open(9,file=filename)         
     
     
      if (quiet.ne.1) then
      if (autorun.ne.1) call system('cls') 
      write(*,*) '**********************************'
      write(*,*) '*     HydroGenerator (v4.1)      *'
      write(*,*) '* Action Contre la Faim (ACF-E)  *'
      write(*,*) '*        Erwann Fillol (2018)    *'
      write(*,*) '*        erwann.fillol@gmail.com *'
      write(*,*) '**********************************'
      write(*,*)
      write(*,'(a,$)') ' Initialisation   '
      endif      

      write(9,*) '**********************************'
      write(9,*) '*     HydroGenerator (v4.1)      *'
      write(9,*) '* Action Contre la Faim (ACF-E)  *'
      write(9,*) '*        Erwann Fillol (2018)    *'
      write(9,*) '*        erwann.fillol@gmail.com *'
      write(9,*) '**********************************'
      write(9,*)
      write(9,*) 'Heure             : '//numer2(time1(5))//':'//numer2(time1(6))//':'//numer2(time1(7))
      write(9,*) 'Date              : '//numer2(time1(3))//'/'//numer2(time1(2))//'/'//numer4(time1(1))
      write(9,*)      
      write(9,'(a,$)') ' Initialisation   '
      
      
! Fichier flag
      flagOK=0
      filename='Lib\Tmp\HydroGenerator_Flag.txt'
      open(10,file=filename)
      write(10,*) flagOK
      close(10)      
      
! Lecture Fichiers Auxillaires
      filename='Lib\Ancillary\HDR\32R.hdr'
      inquire(file=filename, size=filesize, exist=filexist)
      if ((filexist).and.(filesize.ne.-1)) then
         sizeHDR32R=filesize
         allocate(HDR32R(1:sizeHDR32R))
         open(10,file=filename,access='direct',recl=sizeHDR32R)
         read(10,rec=1) HDR32R
         close(10)
      else
         sizeHDR32R=0
      endif

! Pixel Size Map pour calcul Admin
      allocate(PixelSizeMap(1:nbpixel,1:nbline),stat=allocStat)
      filename='Lib\Ancillary\Img\PixelSizeMap.img'
      open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
      read(10,rec=1) PixelSizeMap
      close(10)
      
! Lecture anciens paramètres et determination degré optimisation
      filename='Lib\Param_Old\HydroGenerator_Param_Old.txt'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         open(10,file=filename)       
         close(10)
         call system('del /Q '//filename//' > Lib\Cmd\OutPipe.txt')
      endif
         
      
! Initialisation nom decades integration
      month=1
      day=1
      do decad=1,72
         decadnamep(decad)=numer2(month)//numer2(day)
         day=day+10
         if (day.eq.31) then
            day=1
            month=month+1
            if (month.eq.13) then
               month=1
            endif
         endif
      enddo

! Lecture du Masque Terre/Mer
      if (maskTMApply.eq.1) then
         allocate(MaskTM(1:nbpixel,1:nbline),stat=allocStat)
         filename='Lib\Ancillary\Img\LandMask.img'
         open(10,file=filename,access='direct',status='old',recl=1*nbpixel*nbline)
         read(10,rec=1) MaskTM
         close(10)
      endif
      

 


!cccccccccccccccccccccccccc
!c Lecture fichier Param  c
!cccccccccccccccccccccccccc

! Lecture fichier
      filename='Param\HydroGenerator_Param.txt'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         open(10,file=filename,status='old',form='formatted',err=98,iostat=iError)
         read(10,*)
         read(10,*) RWater
         if (RWater.lt.0.) then
            RWater=0.
         endif
         if (RWater.gt.60.) then
            RWater=60.
         endif       
! Fenetre temporelle
         read(10,*) decaddebp,decadfinp
         if (decaddebp.lt.-1) then
             decaddebp=-1
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
         
! Parametre Accessibilité P0, P1 et ForageFlag
          read(10,*) P0,P1,ForageFlag
          if (P0.lt.0.) then
             P0=0.
          endif
          if (P0.gt.100.) then
             P0=100.
          endif
          if (P1.lt.0.) then
             P1=0.
          endif
          if (P1.gt.100.) then
             P1=100.
          endif  
          P0=P0/100.
          P1=P1/100. 
! Parametre Sorties
         read(10,*) OutOpt,Compress,Masking
         read(10,*) OutRaw,KeepOld
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
      
      
! Recherche fichier Admin et verification ouverture fichiers sortie
      nbAdmin=-1
      nbAdminalloc=0
      do Adm=0,AdmLevelMax
         if (Adm.le.2) then
            filenameAdmin(Adm)='Lib\Ancillary\Img\ADM_'//numer1(Adm)//'.img'
         else
            filenameAdmin(Adm)='Lib\Ancillary\Img\GEO_'//numer1(Adm)//'.img'
         endif
         inquire(file=filenameAdmin(Adm),exist=filexist)
         if (filexist) then
            nbAdmin=nbAdmin+1
         endif
      enddo
      do Adm=0,nbAdmin
         if (Adm.le.2) then
            filename='Output\Water\Report\Access_ADM_'//numer1(Adm)//'.csv'
         else
            filename='Output\Water\Report\Access_GEO_'//numer1(Adm)//'.csv'
         endif
         open(10,file=filename,form='formatted',iostat=iError,err=99)        
         write(10,*,iostat=iError,err=99) 'Test'
         close(10)
         if (Adm.le.2) then
            filename='Output\Water\Report\Water_ADM_'//numer1(Adm)//'.csv'
         else
            filename='Output\Water\Report\Water_GEO_'//numer1(Adm)//'.csv'
         endif
         open(10,file=filename,form='formatted',iostat=iError,err=99)        
         write(10,*,iostat=iError,err=99) 'Test'
         close(10)         
99       if (iError.ne.0) then
            if (quiet.ne.1) then
            write(*,*) ': Erreur'
            write(*,*) 'Fichier ouvert    : ',filename
            write(*,*) 'Veuillez Fermer et Relancer.'
            endif
            
            write(9,*) ': Erreur'
            write(9,*) 'Fichier ouvert    : ',filename
            write(9,*) 'Veuillez Fermer et Relancer.'
            goto 9999
         endif
         if (Adm.le.2) then
            filename='Lib\Ancillary\Img\ADM_'//numer1(Adm)//'.txt'
         else
            filename='Lib\Ancillary\Img\GEO_'//numer1(Adm)//'.txt'
         endif
         open(10,file=filename)
         read(10,*) nbID
         close(10)
         if (nbID.gt.nbAdminalloc) then
            if (nbID.gt.nbAdminmax) then
               nbID=nbAdminmax
            endif
            nbAdminalloc=nbID
         endif
      enddo
          
          
          
          
          
! Lecture et verif liste PondMonitor & verification fichiers sorties
      filename='Lib\Param_Old\PondMonitor_List.txt'
      open(12,file=filename,form='formatted')
      nbPond=0
      filename='Param\PondMonitor_List.txt'
      inquire(file=filename,exist=filexist)
      if (.not.filexist) goto 51
      open(11,file=filename,form='formatted')
      read(11,*)
50    read(11,*,end=51) IDPondR,LatminR,LatmaxR,LonminR,LonmaxR,AdmLevelR,AdmCodeR,NamePondR   
      
      if ((LatminR.eq.0.).and.(LatmaxR.eq.0.).and.(LonminR.eq.0.).and.(LonmaxR.eq.0.)) then
         LatminR=-180.
         LatmaxR=+180.
         LonminR=-180.
         LonmaxR=+180.
      endif
      
      iminR=nint((LonminR-Lon0)/pixelSD)+1
      imaxR=nint((LonmaxR-Lon0)/pixelSD)+1
      iminRR=min(iminR,imaxR)
      imaxRR=max(iminR,imaxR)
      iminRR=max(1,iminRR)
      imaxRR=min(nbpixel,imaxRR)
   
      jminR=nint((Lat0-LatmaxR)/pixelSD)+1
      jmaxR=nint((Lat0-LatminR)/pixelSD)+1
      jminRR=min(jminR,jmaxR)
      jmaxRR=max(jminR,jmaxR)
      jminRR=max(1,jminRR)
      jmaxRR=min(nbline,jmaxRR)
      
      nbPond=nbPond+1     
      write(12,*) IDPondR,iminRR,imaxRR,jminRR,jmaxRR,AdmLevelR,AdmCodeR,NamePondR
      
      filename='Output\Water\Report\Water_Pond_'//numer4(IDPondR)//'.csv'
      open(10,file=filename,form='formatted',iostat=iError,err=97)        
      write(10,*,iostat=iError,err=97) 'Test'
      close(10)   
97    if (iError.ne.0) then
         if (quiet.ne.1) then
         write(*,*) ': Erreur'
         write(*,*) 'Fichier ouvert    : ',filename
         write(*,*) 'Veuillez Fermer et Relancer.'
         endif
         
         write(9,*) ': Erreur'
         write(9,*) 'Fichier ouvert    : ',filename
         write(9,*) 'Veuillez Fermer et Relancer.'
         goto 9999
      endif      
      
      
      goto 50
51    close(11)
      close(12)
      
      
! Lecture mask si necessaire & Parametre fenetre sortie GEOTIFF
      allocate(MaskOut(1:nbpixel,1:nbline))
      filename='Lib\Ancillary\Img\MASK.img'
      inquire(file=filename,exist=filexist)   
      if ((Masking.eq.1).and.(filexist)) then
         open(10,file=filename,access='direct',status='old',recl=4*nbpixel*nbline)
         read(10,rec=1) MaskOut
         close(10)
         
         filename='Lib\Ancillary\Img\MASK.txt'
         open(10,file=filename)
         read(10,*) i
         read(10,*) i,iminMaskOut,imaxMaskOut,jminMaskOut,jmaxMaskOut
         close(10)
      else
         MaskOut(:,:)=1
         iminMaskOut=1
         imaxMaskOut=nbpixel
         jminMaskOut=1
         jmaxMaskOut=nbline
      endif
      nbpixelMaskOut=imaxMaskOut-iminMaskOut+1
      nblineMaskOut=jmaxMaskOut-jminMaskOut+1
      lonminMaskOut=Lon0+(iminMaskOut-1)*PixelSD
      latmaxMaskOut=Lat0-(jminMaskOut-1)*PixelSD      
      
      
      
         

! Lecture FBackGround
!      filename='Lib\Ancillary\Img\FBackGround_AI.img'
!      open(10,file=filename,access='direct',status='old',recl=4*nbpixel*nbline)
!      read(10,rec=1) FBackGround(:,:)
!      close(10)
!      FBackGround(:,:)=(P0-P1)*FBackGround(:,:)+P1
      
      
! Lecture du Masque Terre/Mer
!      filename='Lib\Ancillary\Img\LandMask.img'
!      open(10,file=filename,access='direct',status='old',recl=1*nbpixel*nbline)
!      read(10,rec=1) MaskTM
!      close(10)

! Reperage des décades SWB
      year=yearFirst
      month=monthFirst
      day=dayFirst
      decadFirst=(monthFirst-1)*3+(dayFirst-1)/10+1
      nbdecad=0
10    decadname=numer4(year)//numer2(month)//numer2(day)
      filename='Data\Raw\In\SWB\SWB_'//decadname//'.img'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         nbdecad=nbdecad+1
         dayLast=day
         monthLast=month
         yearLast=year
         decadLast=(monthLast-1)*3+(dayLast-1)/10+1
         day=day+10
         if (day.gt.21) then
            day=1
            month=month+1
            if (month.gt.12) then
               month=1
               year=year+1
            endif
         endif
         if (.not.((year.eq.yearStop).and.(month.eq.monthStop).and.(day.eq.dayStop).and.(LastStop.eq.1))) then 
            goto 10
         endif
      endif    
      nbdecadtot=nbdecad
      
! Calcul de decaddebp et decadfinp en cas de calcul retro
      if (decaddebp.le.0) then
         decaddebp=decadLast-decadfinp+1
         if (decaddebp.le.0) then
            decaddebp=decaddebp+36
         endif
         decadfinp=decadLast
      endif
      
! Rectification decadfinp & calcul nb decad integration
      if (decadfinp.lt.decaddebp) then
         decadfinp=decadfinp+36
      endif
      nbdecadint=decadfinp-decaddebp+1
      
! Calcul du nombre d'année
      nbyeartot=0
      if (decadfinp.ge.decadFirst) then
         nbyeartot=nbyeartot+1
         yearStart=yearFirst
      else
         yearStart=yearFirst+1
      endif
      if (decaddebp.le.decadLast) then
         nbyeartot=nbyeartot+1
         yearEnd=yearLast
      else
         yearEnd=yearLast-1
      endif
      nbyeartot=nbyeartot+(yearLast-yearStart-1)+1 ! +1 à vérifier
      do year=1,nbyeartot
         years(year)=year+yearStart-1
      enddo
      
      
! Effacage anciens fichiers
      if (KeepOld.ne.1) then
         call system('del /Q /S Output\Water\Access\* > Lib\Cmd\OutPipe.txt')        
         call system('del /Q /S Output\Water\Report\* > Lib\Cmd\OutPipe.txt')              
         call system('del /Q /S Output\Water\Shape\* > Lib\Cmd\OutPipe.txt')         
         call system('del /Q /S Output\Water\Map\* > Lib\Cmd\OutPipe.txt') 
         call system('del /Q /S Data\Raw\Water\WaterAccessAnomaly\* > Lib\Cmd\OutPipe.txt')
      endif
      if (OutOpt.ne.1) then
         call system('del /Q /S Data\Raw\Water\Mask\* > Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Raw\Water\WaterAccess\* > Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Raw\Water\WaterAccessType\* > Lib\Cmd\OutPipe.txt')   
      endif      
      
      
! Affichage
      if (nbdecadtot.ne.0) then
         if (quiet.ne.1) then
         write(*,*) ': OK'
         endif
         write(9,*) ': OK'        
         if (quiet.ne.1) then
         write(*,*)
         endif
         write(9,*)
      else
         if (quiet.ne.1) then
         write(*,*) ': Erreur !'
         write(*,*)
         write(*,*) 'Fichiers non-trouves !'
         write(*,*) 'Verifier la configuration'
         write(*,*) 'des fichiers et des repertoires.'
         endif
         write(9,*) ': Erreur !'
         write(9,*)
         write(9,*) 'Fichiers non-trouves !'
         write(9,*) 'Verifier la configuration'
         write(9,*) 'des fichiers et des repertoires.'         
         goto 9999
      endif
      
      if (quiet.ne.1) then
      write(*,*) 'Integration       : '//decadnamep(decaddebp)//' > '//decadnamep(decadfinp)
      endif
      write(9,*) 'Integration       : '//decadnamep(decaddebp)//' > '//decadnamep(decadfinp)
      
      write(output1,'(F6.2)') nint(RWater*100.)/100.
      write(output2,*) nint(100.*P0)
      write(output3,*) nint(100.*P1)
      if (quiet.ne.1) then    
      write(*,*) 'Accessibilite     : Dmax = '//trim(adjustl(output1))//' km'
      write(*,*) 'Background        : P0 = '//trim(adjustl(output2))//' % ; P1 = '//trim(adjustl(output3))//' %'
      endif
      write(9,*) 'Accessibilite     : Dmax = '//trim(adjustl(output1))//' km'
      write(9,*) 'Background        : P0 = '//trim(adjustl(output2))//' % ; P1 = '//trim(adjustl(output3))//' %'

      write(output1,*) nbPond
      write(*,*) 'Nombre points     : '//trim(adjustl(output1))
      write(9,*) 'Nombre points     : '//trim(adjustl(output1))
      
      write(output1,*) nbdecadtot
      write(output2,*) numer4(yearFirst)//numer2(monthFirst)//numer2(dayFirst)
      write(output3,*) numer4(yearLast)//numer2(monthLast)//numer2(dayLast)      
      if (quiet.ne.1) then
      write(*,*) 'Decades           : '//trim(adjustl(output2))//' > '//trim(adjustl(output3))
!      write(*,*) 'Nombre de decades : '//trim(adjustl(output1))
      write(*,*) 'Annees            : '//numer4(yearStart)//' > '//numer4(yearEnd)
!      write(*,*)
      endif
      write(9,*) 'Decades           : '//trim(adjustl(output2))//' > '//trim(adjustl(output3))
!      write(9,*) 'Nombre de decades : '//trim(adjustl(output1))
      write(9,*) 'Annees            : '//numer4(yearStart)//' > '//numer4(yearEnd)
!      write(9,*)
!      write(output1,*) DegOpt
!      write(9,*) 'Optimisation niv. : '//trim(adjustl(output1))
!      write(output1,*) nbThread1
!      write(output2,*) nbThread2
!      write(output3,*) nbThread3
!      write(output4,*) nbThreadMax
!      write(9,*) 'Processeurs (nb)  : '&
!      //trim(adjustl(output1))//'/'//trim(adjustl(output2))//'/'//trim(adjustl(output3))//'/'//trim(adjustl(output4))
!      write(output1,*) MemAvailable
!      write(9,*) 'Memoire           : '//trim(adjustl(output1))//' octets'
!      write(output1,*) nblineblock
!      write(9,*) 'Taille bloc       : '//trim(adjustl(output1))//' lignes'     




 
      
! Appel fonction WaterAccess
      nbSuccDetect=3
      nbYearDetect=3
      filename='Lib\Tmp\WaterAccess_Param.txt'
      open(10,file=filename)
      write(10,*) RWater,ForageFlag
      write(10,*) P0,P1
      write(10,*) nbSuccDetect,nbYearDetect
      write(10,*) decaddebp,decadfinp  
      write(10,*) quiet,compress
      close(10)

      filename='Lib\Bin\WaterAccess.exe'
      call SYSTEM(filename)
      
      filename='Lib\Tmp\WaterAccess_Flag.txt'
      open(10,file=filename)
      read(10,*) FinalOk
      close(10)
      
      if (FinalOk.ne.1) then
         write(*,*) 'Erreur Calcul Accessibilite.'
         goto 9999
      endif
      
      filename='Lib\Tmp\WaterAccess_Report.txt'
      open(10,file=filename)
18    read(10,'(a)',iostat=iError) output1
      if (iError.eq.0) then
         write(9,'(a)') output1
         goto 18
      endif
      close(10)
      
! Test memoire disponible
! Verification memoire disponible
      filename='Lib\Bin\MemAvailable.exe'
      call SYSTEM(filename)
      filename='Lib\Tmp\MemAvailable.txt'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         open(10,file=filename)
         read(10,*) MemAvailable
         close(10)
      else
         MemAvailable=MemAvailableMin
      endif   
      if (MemAvailable.lt.MemAvailableMin) then
         if (quiet.ne.1) then
         write(*,*) ': Erreur'
         write(*,*) 'Allocation memoire impossible.'
         endif
            
         write(9,*) ': Erreur'
         write(9,*) 'Allocation memoire impossible.'
         goto 9999
      endif      

! Allocation mémoire avec détermination taille du blocksize
      nbdecadalloc=nbdecadint
      nbyearalloc=nbyeartot
      do nblineblock=nbline,1,-1
         frac=(1.*nbline)/(1.*nblineblock)
         if (int(frac).eq.frac) then
            blocksize=nblineblock*nbpixel
            nbblock=nbline/nblineblock
            
            MemAllocNeed=&
            blocksize*4*(4+nbyearalloc)+&
            blocksize*4*nbdecadalloc*1+&
            (nbpixelMaskOut*nblineMaskOut)*4+&
            (nbpixel*nbline)*4*2+&
            (nbAdminalloc*(nbyearalloc+1))*8
            
            if (MemAllocNeed.le.factorMem*MemAvailable) then
               
               if (allocated(outBlock32R)) deallocate(outBlock32R)
               if (allocated(SommeAccessYear)) deallocate(SommeAccessYear)
               if (allocated(SommeAccessType)) deallocate(SommeAccessType)
               if (allocated(SommeAccessNb)) deallocate(SommeAccessNb)
               if (allocated(AccessTypeNb)) deallocate(AccessTypeNb)
               if (allocated(AccessTypeBlock)) deallocate(AccessTypeBlock)
               if (allocated(outImgMaskOut32R)) deallocate(outImgMaskOut32R)
               if (allocated(ImgR1)) deallocate(ImgR1)
               if (allocated(ImgR2)) deallocate(ImgR2)
               if (allocated(outImg32R)) deallocate(outImg32R)
               if (allocated(Admin)) deallocate(Admin)
               if (allocated(sumAdminAccessYear)) deallocate(sumAdminAccessYear)
               if (allocated(AnomAccessYear)) deallocate(AnomAccessYear)
               
               allocate(outBlock32R(1:blocksize),stat=allocStat)
               if (allocStat.ne.0) cycle
               allocate(AnomAccessYear(1:blocksize,1:nbyearalloc),stat=allocStat)
               if (allocStat.ne.0) cycle               
               allocate(SommeAccessYear(1:blocksize),stat=allocStat)
               if (allocStat.ne.0) cycle
               allocate(SommeAccessType(1:blocksize),stat=allocStat)
               if (allocStat.ne.0) cycle
               allocate(SommeAccessNb(1:blocksize),stat=allocStat)
               if (allocStat.ne.0) cycle
               allocate(AccessTypeNb(1:blocksize),stat=allocStat)
               if (allocStat.ne.0) cycle               

               allocate(AccessTypeBlock(1:nbdecadint,1:blocksize),stat=allocStat)
               if (allocStat.ne.0) cycle
               allocate(outImgMaskOut32R(1:nbpixelMaskOut,1:nblineMaskOut),stat=allocStat)
               if (allocStat.ne.0) cycle
               allocate(ImgR1(1:nbpixel,1:nbline),stat=allocStat)
               if (allocStat.ne.0) cycle
               allocate(ImgR2(1:nbpixel,1:nbline),stat=allocStat)
               if (allocStat.ne.0) cycle               
               allocate(outImg32R(1:nbpixel,1:nbline),stat=allocStat)
               if (allocStat.ne.0) cycle
               allocate(Admin(1:nbpixel,1:nbline),stat=allocStat)
               if (allocStat.ne.0) cycle 
               allocate(sumAdminAccessYear(1:nbAdminalloc,0:nbyearalloc),stat=allocStat)
               if (allocStat.ne.0) cycle
               exit
            endif
         endif
      enddo
      
      
      if (nblineblock.eq.0) then
         if (quiet.ne.1) then
         write(*,*) ': Erreur'
         write(*,*) 'Allocation memoire impossible.'
         endif
            
         write(9,*) ': Erreur'
         write(9,*) 'Allocation memoire impossible.'    
         goto 9999
      endif
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calcul des cartes d'anomalie de l'accessibilité !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      goto 999
      if (quiet.ne.1) then
      write(*,*)      
      write(*,'(a,a,$)') ' Calcul principal  :      ',back2 
      endif
      write(9,*)      
      write(9,'(a,$)') ' Calcul principal  :'     

      
      do j=1,nbblock
 
         if (quiet.ne.1) then
            percent=nint(100.*(j-1.)/nbblock)
            write(output1,'(I3.3)') percent        
            write(*,'(a,a,a,a,$)') back3,trim(adjustl(output1)),' %',back2
         endif 
         
! Chargement Access Type + calcul access moyenne      
         SommeAccessType(:)=0.
         SommeAccessNb(:)=0.
         do decadint=1,nbdecadint            
            decad=decadint+decaddebp-1
            
            filename='Data\Raw\Water\WaterAccessType\WaterAccessType_'//decadnamep(decad)//'.img'
            open(10,file=filename,access='direct',status='old',recl=4*blocksize)
            read(10,rec=j) outBlock32R
            close(10)
            AccessTypeBlock(decadint,:)=outBlock32R(:)
            
            do i=1,blocksize
               if (AccessTypeBlock(decadint,i).ge.0.) then
                  SommeAccessNb(i)=SommeAccessNb(i)+1.
                  SommeAccessType(i)=SommeAccessType(i)+AccessTypeBlock(decadint,i)
               endif
            enddo            
         enddo
         do i=1,blocksize
            if (SommeAccessNb(i).ne.0.) then
               outBlock32R(i)=SommeAccessType(i)/SommeAccessNb(i)
            else
               outBlock32R(i)=-9999.
            endif
         enddo
         filename='Data\Raw\Water\WaterAccessAnomaly\WaterAccessYear_Mean.img'
         open(10,file=filename,access='direct',recl=4*blocksize)
         write(10,rec=j) outBlock32R
         close(10)
         filename='Data\Raw\Water\WaterAccessAnomaly\WaterAccessYear_Mean.hdr'
         open(10,file=filename,access='direct',recl=sizeHDR32R)
         write(10,rec=1) HDR32R
         close(10)
          
! Calcul accessibilité annuelle         
         AccessTypeNb(:)=0.
         AnomAccessYear(:,:)=0.
         
         do yeari=1,nbyeartot
            year=yeari+yearStart-1
         
            SommeAccessYear(:)=0.
            SommeAccessType(:)=0. 
            SommeAccessNb(:)=0.
                       
            do decadint=1,nbdecadint
               
               yearl=year
               decad=decadint+decaddebp-1
               if (decad.gt.36) then
                  decad=decad-36
                  yearl=yearl+1
               endif
            
               filename='Data\Raw\Water\WaterAccess\WaterAccess_'//numer4(yearl)//decadnamep(decad)//'.img'
               inquire(file=filename,exist=filexist)
               if (filexist) then
                  open(10,file=filename,access='direct',status='old',recl=4*blocksize)
                  read(10,rec=j) outBlock32R
                  close(10)
                  
                  do i=1,blocksize
                     if ((outBlock32R(i).ne.-9999.).and.(AccessTypeBlock(decadint,i).ne.-9999.)) then
                        SommeAccessNb(i)=SommeAccessNb(i)+1.
                        SommeAccessYear(i)=SommeAccessYear(i)+outBlock32R(i)
                        SommeAccessType(i)=SommeAccessType(i)+AccessTypeBlock(decadint,i)
                     endif
                  enddo
               endif
            enddo
            
            do i=1,blocksize
               if (SommeAccessNb(i).eq.0.) then
                  SommeAccessYear(i)=-9999.
                  SommeAccessType(i)=-9999.
               else
                  SommeAccessYear(i)=SommeAccessYear(i)/SommeAccessNb(i)
                  SommeAccessType(i)=SommeAccessType(i)/SommeAccessNb(i)                             
                  if (SommeAccessYear(i).gt.0.) then
                     AccessTypeNb(i)=AccessTypeNb(i)+1.
                  endif
                  if ((SommeAccessType(i).eq.0.).or.(SommeAccessType(i).lt.AccessTypeMin)) then
                     SommeAccessYear(i)=-9998.
                     SommeAccessType(i)=-9998.
                     AnomAccessYear(i,yeari)=-9998.
                  else
                     AnomAccessYear(i,yeari)=100.*SommeAccessYear(i)/SommeAccessType(i)
                  endif
               endif
            enddo
            
            outBlock32R(:)=SommeAccessYear(:)
            filename='Data\Raw\Water\WaterAccessAnomaly\WaterAccessYear_'//numer4(year)//'.img'
            open(10,file=filename,access='direct',recl=4*blocksize)
            write(10,rec=j) outBlock32R
            close(10)
 
            outBlock32R(:)=SommeAccessType(:)
            filename='Data\Raw\Water\WaterAccessAnomaly\WaterAccessType_'//numer4(year)//'.img'
            open(10,file=filename,access='direct',recl=4*blocksize)
            write(10,rec=j) outBlock32R
            close(10)
            
         enddo
      
! Application du seuil du minimum de % d'année et sauvegarde
         do yeari=1,nbyeartot
            year=yeari+yearStart-1
            do i=1,blocksize
               if (AccessTypeNb(i).lt.SeuilAccessType*nbyeartot) then
                  ! AnomAccessYear(i,yeari)=-9997.
				  AnomAccessYear(i,yeari)=-9998.
               endif
            enddo
            
            outBlock32R(:)=AnomAccessYear(:,yeari)
            filename='Data\Raw\Water\WaterAccessAnomaly\WaterAccessAnomaly_'//numer4(year)//'.img'
            open(10,file=filename,access='direct',recl=4*blocksize)
            write(10,rec=j) outBlock32R
            close(10)
         enddo
         
      enddo
      
      
      if (quiet.ne.1) then
         percent=100
         write(output1,'(I3.3)') percent        
         write(*,'(a,a,a,a,$)') back3,trim(adjustl(output1)),' %',back2
      endif 
                  
! Copy fichier hdr      
      do year=yearStart,yearLast
         filename='Data\Raw\Water\WaterAccessAnomaly\WaterAccessYear_'//numer4(year)//'.hdr'
         open(10,file=filename,access='direct',recl=sizeHDR32R)
         write(10,rec=1) HDR32R
         close(10)      
         filename='Data\Raw\Water\WaterAccessAnomaly\WaterAccessType_'//numer4(year)//'.hdr'
         open(10,file=filename,access='direct',recl=sizeHDR32R)
         write(10,rec=1) HDR32R
         close(10)      
         filename='Data\Raw\Water\WaterAccessAnomaly\WaterAccessAnomaly_'//numer4(year)//'.hdr'
         open(10,file=filename,access='direct',recl=sizeHDR32R)
         write(10,rec=1) HDR32R
         close(10)
      enddo



!cccccccccccc
!c Ecriture c
!cccccccccccc

      if (quiet.ne.1) then
      write(*,*)
      write(*,'(a,$)') ' Ecriture          :            '
      endif
      write(9,*) 'OK'
      write(9,'(a,$)') ' Ecriture          :' 
  

!cccccccccccccccccccccccccccccccccccc      
!c Ecriture Fichiers Stat sur Admin c
!cccccccccccccccccccccccccccccccccccc    
      do Adm=0,nbAdmin

         filename=filenameAdmin(Adm)
         open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
         read(10,rec=1) Admin
         close(10)
            
!c Ouverture fichier sortie
         if (Adm.le.2) then
            filename1='Output\Water\Report\Access_ADM_'//numer1(Adm)//'.csv'
            filename3='Lib\Ancillary\Img\ADM_'//numer1(Adm)//'.txt'
         else
            filename1='Output\Water\Report\Access_GEO_'//numer1(Adm)//'.csv'
            filename3='Lib\Ancillary\Img\GEO_'//numer1(Adm)//'.txt'     
         endif
         open(11,file=filename1)
         write(11,*) ';Source:;',filename,';'
! Lecture info sur ADM & GEO
         open(10,file=filename3)
         read(10,*) nbID
         if (nbID.gt.0) then
            if (nbID.gt.nbAdminmax) then
               nbID=nbAdminmax
            endif
            do IDi=1,nbID
               read(10,*) AdminID(IDi),&
               AdminIDimin(IDi),AdminIDimax(IDi),&
               AdminIDjmin(IDi),AdminIDjmax(IDi),&
               AdminIDsurf(IDi),AdminIDname(IDi)
            enddo
         endif
         close(10)

         write(11,*) ';Nb_entities:;',nbID,';'
         write(11,*) ';Water_Accessibility_Anomaly:;[%];'
         write(11,*) ';'    
            
! Calcul Accessibilité
         SumAdminAccessYear(:,:)=0.
         do year=0,nbyeartot
            percent=nint(100.*(year+Adm*nbyeartot)/(1.*(nbyeartot*(nbAdmin+1.))))
            if (quiet.ne.1) then
               write(*,'(a,I3.3,a,$)') back11,percent,' % (1/4)'
            endif
            if (year.eq.0) then
               ext='Mean'
            else
               ext=numer4(years(year))
            endif
            filename='Data\Raw\Water\WaterAccessAnomaly\WaterAccessYear_'//ext//'.img'
            open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
            read(10,rec=1) ImgR1
            close(10)
            
            if (year.eq.0) then
               ImgR2(:,:)=100. ! Pour equilibrer le passage en % qui vient plus tard
            else
               filename='Data\Raw\Water\WaterAccessAnomaly\WaterAccessType_'//ext//'.img'
               open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
               read(10,rec=1) ImgR2
               close(10)
            endif               
                     
            
!            !$OMP PARALLEL DO NUM_THREADS(nbThread3) DEFAULT(SHARED) PRIVATE(surfAccess, sumAdminAccessType, i, j)
            do IDi=1,nbID
               surfAccess=0.
               sumAdminAccessType=0.
               do i=AdminIDimin(IDi),AdminIDimax(IDi)
                  do j=AdminIDjmin(IDi),AdminIDjmax(IDi)
                     if (AdminID(IDi).eq.Admin(i,j)) then
                        if ((ImgR1(i,j).ge.0.).and.(ImgR2(i,j).ge.0.)) then
                           sumAdminAccessYear(IDi,year)=sumAdminAccessYear(IDi,year)+ImgR1(i,j)*PixelSizeMap(i,j)
                           sumAdminAccessType=sumAdminAccessType+ImgR2(i,j)*PixelSizeMap(i,j)
                           surfAccess=surfAccess+PixelSizeMap(i,j)
                        endif                       
                     endif
                  enddo
               enddo
               if (surfAccess.gt.0.) then
                  sumAdminAccessYear(IDi,year)=100.*sumAdminAccessYear(IDi,year)/sumAdminAccessType
               else
                  sumAdminAccessYear(IDi,year)=-1.0
               endif
            enddo
!            !$OMP END PARALLEL DO
         enddo
         
         write(11,'(a,$)') ';NAME;IDBIOHYDRO;AREA[sqkm];ACCESS_MEAN;'
         do year=1,nbyeartot
            write(11,'(a,$)') 'ACCESS_'//numer4(years(year))//';'
         enddo
         write(11,*)
         do IDi=1,nbID
            write(11,'(a,a,a,I6,a,F10.0,a,$)')';',AdminIDname(IDi),';',AdminID(IDi),';',AdminIDsurf(IDi),';'
            do year=0,nbyeartot
               write(11,'(F18.3,a,$)') sumAdminAccessYear(IDi,year),';'
            enddo
            write(11,*)
         enddo
         close(11)
                  
      enddo    
      
      
!cccccccccccccccccccccccccccccccccccc
!c Relecture et re-écriture geotiff c
!cccccccccccccccccccccccccccccccccccc     
      nbtiff=0
      ! Anomalies annuelles
      do yeari=1,nbyeartot
         year=yeari+yearStart-1
         nbtiff=nbtiff+1
         filetiffin(nbtiff)='Data\Raw\Water\WaterAccessAnomaly\WaterAccessAnomaly_'//numer4(year)//'.img'
         filetiffou(nbtiff)='Output\Water\Access\AccessAnomaly_'//numer4(year)//'.tif'
         filetiffcl(nbtiff)='Data\Raw\Water\WaterAccessAnomaly\WaterAccessAnomaly_'//numer4(year)//'.*' 
         typetiff(nbtiff)=1
         filttiff(nbtiff)=1
      enddo
      ! Moyenne interannuelle
      nbtiff=nbtiff+1
      filetiffin(nbtiff)='Data\Raw\Water\WaterAccessAnomaly\WaterAccessYear_Mean.img'
      filetiffou(nbtiff)='Output\Water\Access\Access_Mean.tif'
      filetiffcl(nbtiff)='Data\Raw\Water\WaterAccessAnomaly\WaterAccessYear_Mean.*' 
      typetiff(nbtiff)=2
      filttiff(nbtiff)=1
      
      ! Nombre d'année détectée > Fréquence
      nbtiff=nbtiff+1
      filetiffin(nbtiff)='Data\Raw\Water\Mask\TotYearDetect.img'
      filetiffou(nbtiff)='Output\Water\Map\Frequency.tif'
      filetiffcl(nbtiff)='Data\Raw\Water\Mask\TotYearDetect.*' 
      typetiff(nbtiff)=3
      filttiff(nbtiff)=0   

      ! Nombre d'année détectée > Fréquence
      nbtiff=nbtiff+1
      filetiffin(nbtiff)='Data\Raw\Water\Mask\WaterYear.img'
      filetiffou(nbtiff)='Output\Water\Map\Period.tif'
      filetiffcl(nbtiff)='Data\Raw\Water\Mask\WaterYear.*' 
      typetiff(nbtiff)=4
      filttiff(nbtiff)=0        
      
      

         
      do tiffout=1,nbtiff   
         if (quiet.ne.1) then
            percent=nint(100.*(tiffout-1.)/(1.*(nbtiff-1.)))
            write(*,'(a,I3.3,a,$)') back11,percent,' % (2/4)'
         endif
         
         filename=filetiffin(tiffout)
         open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
         read(10,rec=1) outImg32R
         close(10)
         ImgR1(:,:)=outImg32R(:,:)

! Anomalies % Limitations
         if (typetiff(tiffout).eq.1) then
            do i=1,nbpixel
               do j=1,nbline
                  if (ImgR1(i,j).gt.200.) then
                     ImgR1(i,j)=200.
                  endif
                  if ((ImgR1(i,j).lt.0.).and.(ImgR1(i,j).gt.-9990.)) then
                     ImgR1(i,j)=0.
                  endif
               enddo
            enddo
         endif
         
! Conversion TotYearDetect en Frequency
         if (typetiff(tiffout).eq.3) then
            do i=1,nbpixel
               do j=1,nbline
                  if (ImgR1(i,j).ge.0.) then
                     ImgR1(i,j)=100.*ImgR1(i,j)/nbyeartot
                  endif
               enddo
            enddo
         endif 

! Conversion WaterYear en Period
         if (typetiff(tiffout).eq.4) then
            do i=1,nbpixel
               do j=1,nbline
                  if (ImgR1(i,j).ge.0.) then
                     ImgR1(i,j)=100.*ImgR1(i,j)
                  endif
               enddo
            enddo
         endif         
         
! Application mask terre/mer
         if (maskTMApply.eq.1) then
            do i=1,nbpixel
               do j=1,nbline
                  if (maskTM(i,j).eq.0) then
                     ImgR1(i,j)=-9999.
                  endif
               enddo
            enddo
         endif  

! Decoupage suivant le masque         
! Application du mask si necessaire
         do i=iminMaskOut,imaxMaskOut
            do j=jminMaskOut,jmaxMasKout
               if ((Masking.eq.1).and.(MaskOut(i,j).eq.0)) then
                  outImgMaskOut32R(i-iminMaskOut+1,j-jminMaskOut+1)=-9995.
               else
                  outImgMaskOut32R(i-iminMaskOut+1,j-jminMaskOut+1)=ImgR1(i,j)
               endif
            enddo
         enddo

! Sauvegarde         
         filename='Lib\Tmp\Img.img'
         open(10,file=filename,access='direct',recl=4*nbpixelMaskOut*nblineMaskOut)
         write(10,rec=1) outImgMaskOut32R
         close(10)
         filename='Lib\Tmp\Img.hdr'
         open(10,file=filename)
         write(10,'(a)') 'ENVI'
         write(10,'(a,I4)') 'samples = ',nbpixelMaskOut
         write(10,'(a,I4)') 'lines = ',nblineMaskOut
         write(10,'(a)') 'bands = 1'
         write(10,'(a)') 'header offset = 0'
         write(10,'(a)') 'file type = ENVI Standard'
         write(10,'(a)') 'data type = 4'
         write(10,'(a)') 'interleave = bsq'
         write(10,'(a)') 'sensor type = Unknown'
         write(10,'(a)') 'byte order = 0'
         write(10,'(a,F14.8,a,F14.8,a,F14.12,a,F14.12,a,a)') &
         'map info = {Geographic Lat/Lon, 1.0000, 1.0000, ',&
         lonminMaskOut,', ',latmaxMaskOut,', ',PixelSD,', ',PixelSD,', ',&
         'WGS-84, units=Degrees}'
         write(10,'(a)') 'wavelength units = Unknown'
         close(10)
         !call system('copy '//filenameHDR32R//filename//'> Lib\Cmd\OutPipe.txt')
         
         filename=filetiffou(tiffout)
         if (Compress.eq.1) then
            call system('Lib\Bin\gdal\bin\gdal_translate.exe "Lib\tmp\Img.img" "'//trim(filename)//&
            '" -co COMPRESS=LZW > Lib\Cmd\OutPipe.txt')           
         else
            call system('Lib\Bin\gdal\bin\gdal_translate.exe "Lib\tmp\Img.img" "'//trim(filename)//&
            '" -co COMPRESS=NONE > Lib\Cmd\OutPipe.txt')  
         endif
         call system('del /Q Lib\Tmp\Img.img > Lib\Cmd\OutPipe.txt')
         call system('del /Q Lib\Tmp\Img.hdr > Lib\Cmd\OutPipe.txt')
         
! Effacement Fichier raw
         if (OutRaw.ne.1) then
            call system('del /Q '//filetiffcl(tiffout)//'> Lib\Cmd\OutPipe.txt')
         endif
      enddo  

      
    
         
         

! Ecriture fichier ancien parametres
      filename='Lib\Param_Old\HydroGenerator_Param_Old.txt'
      open(10,file=filename)
      write(10,*) nbdecadtot,yearlast,monthlast,daylast,yearfirst,monthfirst,dayfirst
      write(10,*) yearStart,yearEnd,decaddebp,decadfinp      
      write(10,*) RWater,P0,P1,ForageFlag
      write(10,*) OutOpt,Quiet     
      close(10)         
      
! Appel fonction matlab pour ecriture Shapefile
      filename='Lib\Bin\HydroGenerator_csv2shp.exe'
      call SYSTEM(filename)

! Fichier flag
      flagOK=1
      filename='Lib\Tmp\HydroGenerator_Flag.txt'
      open(10,file=filename)
      write(10,*) flagOK
      close(10)

! Fichier Report
      write(9,*) 'OK'

9999  continue

!!!!!!!!!!!!!!!!!!!!!!!!
! Deallocation mémoire !
!!!!!!!!!!!!!!!!!!!!!!!!     
      if (allocated(MaskTM)) deallocate(MaskTM)
      if (allocated(outBlock32R)) deallocate(outBlock32R)
      if (allocated(SommeAccessYear)) deallocate(SommeAccessYear)
      if (allocated(SommeAccessType)) deallocate(SommeAccessType)
      if (allocated(SommeAccessNb)) deallocate(SommeAccessNb)
      if (allocated(AccessTypeNb)) deallocate(AccessTypeNb)
      if (allocated(AccessTypeBlock)) deallocate(AccessTypeBlock)
      if (allocated(MaskOut)) deallocate(MaskOut)
      if (allocated(outImgMaskOut32R)) deallocate(outImgMaskOut32R)
      if (allocated(ImgR1)) deallocate(ImgR1)
      if (allocated(ImgR2)) deallocate(ImgR2)
      if (allocated(outImg32R)) deallocate(outImg32R)
      if (allocated(HDR32R)) deallocate(HDR32R)
      if (allocated(PixelSizeMap)) deallocate(PixelSizeMap)
      if (allocated(SumAdminAccessYear)) deallocate(SumAdminAccessYear)
      if (allocated(Admin)) deallocate(Admin)
      if (allocated(AnomAccessYear)) deallocate(AnomAccessYear)
      
      
! Fichier Report
      call date_and_time(VALUES=time2)
      diffH=time2(5)-time1(5)+(1./60.)*(time2(6)-time1(6)+(1./60.)*(time2(7)-time1(7)))
      if (time1(3).ne.time2(3)) then
         diffH=diffH+24.
      endif
      
      write(output1,*) int(diffH)
      write(output2,*) int(60.*(diffH-int(diffH)))
      
      if (quiet.ne.1) then
      write(*,*) 
      write(*,*) 'Temps execution   : '//trim(adjustl(output1))//' h '//trim(adjustl(output2))//' mm'
      write(*,*)
      write(*,'(a,$)') ' Operations terminees.'
      endif
      
      write(9,*)
      write(9,*) 'Temps execution   : '//trim(adjustl(output1))//' h '//trim(adjustl(output2))//' mm'
      write(9,*)
      write(9,*) 'Operations terminees.'
      close(9)
      
      filename1='Lib\Tmp\HydroGenerator_Report.txt'
      filename2='Output\Water\Report\'
      call system('copy /Y '//filename1//filename2//'> Lib\Cmd\OutPipe.txt')
      
      if ((quiet.ne.1).and.(autorun.ne.1)) read(*,*)
end program     
      
      
      
      