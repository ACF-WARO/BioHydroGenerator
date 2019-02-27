program BioGenerator
!ccccccccccccccccccccccccccccccccccccccccccccccccccc
! Erwann Fillol, Action Contre la Faim v5.1 (2018) c
!ccccccccccccccccccccccccccccccccccccccccccccccccccc

! gfortran -static -fopenmp Sources\BioGenerator.f95 -o BioGenerator.exe
 
 
      implicit none
      



      integer*8 nblineblock,nbdecadmax,nbblock,&
                year1,month1,day1,decaldecad,&
                yearStop,monthStop,dayStop,&
                FiltreSizeSigma,nbyearmax,blocksize,&
                FiltreSize,&
                DMPMaxJumpFilter,LastStop,&
                nbpixel,nbline,nbtiffoutmax,&
!                sizeTiff16S,offTiff16S,&
!                sizeTiff32R,offTiff32R,&
!                sizehdr32R,&
                AdmLevelMax,nbPreWriteMax,& 
                sizeBuffMax,factorSizeMNT,&
                nbAdminmax,nbThreadMaxMax,&
                MemAvailableMin,PreWrite,FlushDisk,Force
     
      real*8    SeuilNDV,SigmaNDV,CoefDMP,&
                SeuilDMPlow,SeuilNBlow,DMPMaxJump,&
                pi,e,dMax,&
                Lat0,Lon0,PixelSD,&
                convMNT,PixelSKm,&
                factorMem,factorThread1

      
! Definition fenetre
      parameter (nbpixel=7841)
      parameter (nbline=3630)
      
! Parameters Geo
      parameter (Lat0=27.37946430)
      parameter (Lon0=-18.00446430)
      parameter (PixelSD=0.00892857143)
      parameter (PixelSKm=0.9728653504)      
      
! Definition parametres fichiers sorties
!      parameter (sizeTiff16S=56955085)      
!      parameter (offTiff16S=14528)
!      parameter (sizeTiff32R=113880745)     
!      parameter (offTiff32R=14528)         
!      parameter (sizehdr32R=334)

! Parametres de definition allocation memoire      
!      parameter (nblineblock=11)
!      parameter (nblineblock=55)
      parameter (nbyearmax=25)
!      parameter (nbyearmax=17)     
      parameter (nbdecadmax=36*nbyearmax)
!      parameter (blocksize=nblineblock*nbpixel)
!      parameter (nbblock=nbline/nblineblock)
      parameter (nbtiffoutmax=nbyearmax*4+4)
      parameter (nbPreWriteMax=nbdecadmax*4+36*2+nbyearmax*3+4)

! Parametres Decoupes Admin
      parameter (AdmLevelMax=5)
      parameter (nbAdminmax=2000)
      
! Parametres Water Access
      parameter (sizeBuffMax=200)
      parameter (factorSizeMNT=5)
      parameter (dMax=90.0)
      parameter (convMNT=1./3.)

! Parametres maths
      parameter (e=2.718281828)
      parameter (pi=3.141592654)

! Parametre Transformation DMP en Biomasse
      parameter (coefDMP=365.25/36)

! Parametres de la 1ere decade
      parameter (year1=1998)
      parameter (month1=4)
      parameter (day1=1)
      parameter (decaldecad=9)
!      parameter (year1=1999)
!      parameter (month1=1)
!      parameter (day1=1)
!      parameter (decaldecad=0)	  
      
! Parametres de la Dernière decade (LastStop : 1=oui, 0=non)
      parameter (LastStop=1)
      parameter (yearStop=2022)
!      parameter (yearStop=2001)      
      parameter (monthStop=12)
      parameter (dayStop=21)

! Parametres Filtrage temporel
      parameter (SeuilNDV=0.28)
      parameter (FiltreSize=4)
      parameter (SigmaNDV=3.0)
      parameter (FiltreSizeSigma=4)
      parameter (SeuilDMPlow=20)
      parameter (SeuilNBlow=5)
      parameter (DMPMaxJumpFilter=1)      
      parameter (DMPMaxJump=50)
      
! Parametres Memoire
      parameter (MemAvailableMin=1000000000)
      parameter (factorMem=1./2.)
      parameter (factorThread1=2./8.)
      parameter (nbThreadMaxMax=256)
      
! Parametres Disk
      parameter (PreWrite=0)
      parameter (FlushDisk=0)
      parameter (Force=0)
      




      integer*1     &
!                    buff8S(2),&
!                    buff8R(4),&
                    iError
!                    Tiff16S(sizeTiff16S),&
!                    hdr32R(sizehdr32R),&
!                    slope(nbpixel*factorSizeMNT,nbline*factorSizeMNT),&

     

      integer*2     buff16S

      integer*8     i,j,year,month,day,FE,poso,o,posd,&
                    nbdecadtot,decadyear(nbdecadmax),&
                    yearlast,monthlast,daylast,&
                    yearfirst,monthfirst,dayfirst,&
                    decadmonth(nbdecadmax),decaddecad(nbdecadmax),&
                    decadm,dd,decad,decadd,decadl,decadfirst,decadlast,loop,&
                    yearlastint,yearfirstint,&
                    ib,jb,jc,yearback,&
                    decadCumul(36),buff32S,& 
                    nbTot,years(nbyearmax),nbyear,percent,&
                    flagFiltre,nbLoopFiltre,sizeFiltre,warn,decadback,&
                    Compress,Quiet,&
                    decaddebp,decadfinp,nbDecadSomme,decadlastp,&
                    PondUseFlag,nbtiff,tiffout,&
                    typetiff(nbtiffoutmax),filttiff(nbtiffoutmax),&
                    iPreWrite,nbPreWrite,&
       
                    time1(8),time2(8),&
       
                    OutRaw,KeepOld,&
       
                    is(-sizeBuffmax:sizeBuffmax),&
                    DecadPond(nbdecadmax),&
!                    iSlope,jSlope,nbpas,pas,sl,&

                    PondAccFlag,sizeBuff,ForageFlag,ForageFlagOld,&
                    nbdecadpond,test1,test2,testPond,testForage,&
                    sizeBuffOld,nbForage,SWB,&
                    nbdecadSWB,Adm,nbID,flag,nbmissSWB,&
                    nbfullYearSWB,nbdecadyearfi,yearfi,&
                    
                    nbAdminalloc,&
                    
                    FinalOk,nbSuccDetect,nbYearDetect,&
                    
                    AdminID(nbAdminmax),nbAdmin,&
                    AdminIDimin(nbAdminmax),AdminIDimax(nbAdminmax),&
                    AdminIDjmin(nbAdminmax),AdminIDjmax(nbAdminmax),&
                    IDi,DegOpt,OutOpt,&
                    nbdecadtotOld,nbdecadalloc,nbyearalloc,&
                    yearlastOld,monthlastOld,daylastOld,&
                    yearfirstOld,monthfirstOld,dayfirstOld,& 
                    PondUseFlagOld,PondAccFlagOld,ForageFlagOld2,OutOptOld,&
                    allocStat,nbThreadMax,nbThread1,nbThread2,nbThread3,&
                    nbblockdone(nbThreadMaxMax),nbblockdonetot,ThreadID,ThreadID2,nbThreadActual,&
                    Masking,Profil,&
                    iminMaskOut,imaxMaskOut,jminMaskOut,jmaxMaskOut,nbpixelMaskOut,nblineMaskOut,&
                    lastdecadok,&
                    sizeHDR32R,filesize,flagOK,autorun

      integer*8     MemAvailable,MemAllocNeed

      real*4, dimension(:,:), allocatable ::& !(blocksize,nbdecadmax)
                    DMPin,NDVin !,DMPinb,NDVinb
      
      real*4, dimension(:,:), allocatable ::& !(blocksize,36)
                    CumulDMP,NDVtype,DMPtype
      
      real*4, dimension(:,:), allocatable ::& !(blocksize,nbyearmax)
                    BiomassBlk,sumDMPlastp,AnVI
                    
      real*4, dimension(:), allocatable ::& !(blocksize)
                    outBlock32R,PondDMPuse,PondDMPacc,PondDMPForage
                    
      real*8, dimension(:), allocatable ::& !(blocksize)
                    SigmaBlk,TrendBlk,TrendBlkP,&
                    R2Blk,sumDMPType,sumDMP,AnomDMP,&
                    AnomSigma,SigmaBlkLastp,VI
                    
      real*8, dimension(:,:), allocatable ::& !(nbAdminmax,nbyearmax)    
                    SumAdminBio,SumAdminVI
      
      character*1, dimension(:), allocatable ::& !(blocksize)  
                    out8
                    
      integer*1, dimension(:), allocatable ::& !(sizeHDR32R)
                    HDR32R                    
      
!      integer*1, dimension(:), allocatable ::& !(sizeTiff32R)
!                    Tiff32R
                    
      integer*2, dimension(:), allocatable ::& !(blocksize)  
                    out16
                    
      real*4, dimension(:,:), allocatable ::& !(nbpixel,nbline)
                    outImg32R,outImgMaskOut32R,&
                    PixelSizeMap
      
      integer*4, dimension(:,:), allocatable ::& !(nbpixel,nbline)
                    Admin,MaskOut                
            
      
      real*8, dimension(:,:), allocatable ::& !(nbpixel,nbline)
                    ImgR1,ImgR2
      
                    
       
      real*8        &
!                    buff32R,&
                    DiffNDV,frac,&
                    F1,F0,sDMPmin,SeuilCumulLastYear,&
                    NDV1,NDV2,DMP1,DMP2,Andv,Bndv,Admp,Bdmp,&
                    aNDVi,aNDVm,&
                    sDMPtype,sDMPtypeTot,sDMPin,&   
                    StackNDV(nbyearmax),StackDMP(nbyearmax),&
                    NDVTypeF(36),DMPTypeF(36),&
                    VIalpha,VIfact,&
                    sumback,nbback,sumBnDenVI,sumBnNumVI,&        
!                    dslope(256),dtot,fMNT,pasi,pasj,&
                    PBack,P,&
                    f,P0,P1,factan,d,RWater,&
                    fOld,P0Old,P1Old,factanOld,Lon,Lat,&
                    AdminIDsurf(nbAdminmax),&
                    surfBio,surfVI,&
                    F0Old,F1Old,RWaterOld2,P0Old2,P1Old2,&
                    diffH,aok,bok,fok
     
      real*8        Moy,Sigma,Somme,nb,&
                    S2X,S2Y,SXY,sumX,sumY,&
                    lonminMaskOut,latmaxMaskOut
     
      character*100 filename,filename1,filename2,filename3,&
                    filenameHDR32R,&
                    filetiffin(nbtiffoutmax),&
                    filetiffou(nbtiffoutmax),&
                    filetiffcl(nbtiffoutmax),&
                    filenameAdmin(0:AdmLevelMax),&
                    AdminIDname(nbAdminmax),&
                    output1,output2,output3,output4,&
                    PreWriteFile(nbPreWriteMax)
                    
      character*3   back3
      character*2   back2
      character*1   back1
      character*4   numer4(0:9999),ext,decadnamep(72),decadnameMMDD(36)
      character*2   numer2(0:99)
      character*1   numer1(0:9)
      character*8   decadname,decadnameYYYYMMDD(nbdecadmax),&
                    decadwarn(100),decadnameold,back8
      logical       filexist,filexist1,filexist2
!      equivalence   (buff8S,buff16S),&
!                    (buff8R,buff32R)
!                    (SigmaR,outImgR),&
!                    (AnomalyR,outImgRs),&
!                    (BioMeanR,outImgRs),&
!                    (FBackGround,PixelSizeMap)


     
! Determination du nombre de thread
      INTEGER OMP_GET_MAX_THREADS
      INTEGER OMP_GET_NUM_THREADS
      INTEGER OMP_GET_THREAD_NUM
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
 
! Date et Heure départ 
      call date_and_time(VALUES=time1)
     
! Rapport
      filename='Lib\tmp\BioGenerator_Report.txt'
      open(9,file=filename)
     
      if (quiet.ne.1) then
      if (autorun.ne.1) call system('cls')
      write(*,*) '**********************************'
      write(*,*) '*        BioGenerator (v5.1)     *'
      write(*,*) '* Action Contre la Faim (ACF-E)  *'
      write(*,*) '*        Erwann Fillol (2018)    *'
      write(*,*) '*        erwann.fillol@gmail.com *'
      write(*,*) '**********************************'
      write(*,*)
      !write(output1,'(F10.1)') (1.*MemAvailable)/(1024.**3)
      !write(*,*) 'Memoire libre     : '//trim(adjustl(output1))//' Gb'
      !write(output1,*) nbThreadMax
      !write(*,*) 'Nombre processeurs: '//trim(adjustl(output1))
      write(*,'(a,$)') ' Initialisation   '
      endif
      
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

      write(9,*) '**********************************'
      write(9,*) '*        BioGenerator (v5.1)     *'
      write(9,*) '* Action Contre la Faim (ACF-E)  *'
      write(9,*) '*        Erwann Fillol (2018)    *'
      write(9,*) '*        erwann.fillol@gmail.com *'
      write(9,*) '**********************************'
      write(9,*)
      write(9,*) 'Heure             : '//numer2(time1(5))//':'//numer2(time1(6))//':'//numer2(time1(7))
      write(9,*) 'Date              : '//numer2(time1(3))//'/'//numer2(time1(2))//'/'//numer4(time1(1))
      write(9,*)
      write(9,'(a,$)') ' Initialisation   '     

      factan=-9999.
      buff16S=0
!      buff32R=0.

! Fichier flag
      flagOK=0
      filename='Lib\Tmp\BioGenerator_Flag.txt'
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
!      filename='Lib\Ancillary\HDR\32R.hdr'
!      open(10,file=filename,access='direct',status='old',
!     crecl=sizehdr32R)
!      read(10,rec=1) hdr32R
!      close(10)
      
!      filename='Lib\Tiff\SuSa16S.tif'
!      open(10,file=filename,access='direct',
!     crecl=sizeTiff16S)
!      read(10,rec=1) Tiff16S
!      close(10)
      
!      filename='Lib\Ancillary\Tiff\SuSa32R.tif'
!      open(10,file=filename,access='direct',recl=sizeTiff32R)
!      read(10,rec=1) Tiff32R
!      close(10)      
      

      back8=char(8)//char(8)//char(8)//char(8)//char(8)//char(8)//char(8)//char(8)
      back3=char(8)//char(8)//char(8)
      back2=char(8)//char(8)
      back1=char(8)
      
! Allocation mémoire
      allocate(outImg32R(1:nbpixel,1:nbline),stat=allocStat)

! Lecture fichier parametrage
      filename='Param/BioGenerator_Param.txt'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         open(10,file=filename,status='old',form='formatted',err=98,iostat=iError)
         read(10,*)
! Lissage
         read(10,*) flagFiltre,nbLoopFiltre,sizeFiltre
         if (nbLoopFiltre.lt.0) then
            nbLoopFiltre=0
         endif
         if (nbLoopFiltre.gt.25) then
            nbLoopFiltre=25
         endif
         if (sizeFiltre.lt.0) then
            sizeFiltre=0
         endif
         if (sizeFiltre.gt.11) then
            sizeFiltre=11
         endif
! Fenetre temporelle
         read(10,*) decaddebp,decadfinp
         if (decaddebp.lt.0) then
             decaddebp=0
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
! Seuil Production Filtrage Anomalie
         read(10,*) sDMPmin,SeuilCumulLastYear
         if (SeuilCumulLastYear.lt.0.) then
            SeuilCumulLastYear=0.
         endif
         if (SeuilCumulLastYear.gt.100.) then
            SeuilCumulLastYear=100.
         endif
         if (sDMPmin.lt.0.) then
            sDMPmin=0.
         endif
         SeuilCumulLastYear=SeuilCumulLastYear/100.
! Parametre Usable
         read(10,*) PondUseFlag,F0,F1
         if (F0.lt.0.) then
            F0=0.
         endif
         if (F0.gt.100.) then
            F0=100.
         endif
         if (F1.lt.0.) then
            F1=0.
         endif
         if (F1.gt.100.) then
            F1=100.
         endif 
         F0=F0/100.
         F1=F1/100.
! Parametre Indice Vulnerabilite
         read(10,*) VIalpha
         if (VIalpha.lt.0.) then
            VIalpha=0.
         endif
         if (VIalpha.gt.100.) then
            VIalpha=100.
         endif      
! Parametre Accessible
         read(10,*) PondAccFlag,RWater,P0,P1,ForageFlag
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
! Parametres sortie
         read(10,*) Masking,Profil
         read(10,*) OutOpt,Compress
         read(10,*) OutRaw,KeepOld
         close(10)
      else
         iError=9
      endif
      
      
      
98    if (iError.ne.0) then
         if (quiet.ne.1) then
         write(*,*) ': Erreur'
         write(*,*) 'Lecture parametrage impossible.'
         write(*,*) 'Verifier et Relancer.'
         endif
         
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
            filename='Output\Biomass\Report\Biomass_ADM_'//numer1(Adm)//'.csv'
         else
            filename='Output\Biomass\Report\Biomass_GEO_'//numer1(Adm)//'.csv'
         endif
         open(10,file=filename,form='formatted',iostat=iError,err=99)        
         write(10,*,iostat=iError,err=99) 'Test'
         close(10)
         if (Adm.le.2) then
            filename='Output\Biomass\Report\VI_ADM_'//numer1(Adm)//'.csv'
         else
            filename='Output\Biomass\Report\VI_GEO_'//numer1(Adm)//'.csv'
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
         
      
! Effacage anciens fichiers
      if (KeepOld.ne.1) then
         call system('del /Q /S Output\Biomass\Anomaly\* > Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Output\Biomass\Biomass\* > Lib\Cmd\OutPipe.txt')     
         call system('del /Q /S Output\Biomass\Shape\* > Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Output\Biomass\Stat\* > Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Output\Biomass\VI\* > Lib\Cmd\OutPipe.txt') 
         call system('del /Q /S Output\Biomass\Report\Bio* > Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Output\Biomass\Report\VI* > Lib\Cmd\OutPipe.txt')   
         call system('del /Q /S Data\Raw\Biomass\Anomaly\* > Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Raw\Biomass\Biomass\* > Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Raw\Biomass\Cumul\* > Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Raw\Biomass\Filter\NDVI\* > Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Raw\Biomass\Type\NDVI\* > Lib\Cmd\OutPipe.txt')     
         call system('del /Q /S Data\Raw\Biomass\Stat\* > Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Raw\Biomass\VI\* > Lib\Cmd\OutPipe.txt')
         if (OutOpt.ne.1) then
            call system('del /Q /S Data\Raw\Biomass\Filter\DMP\* > Lib\Cmd\OutPipe.txt')
            call system('del /Q /S Data\Raw\Biomass\Type\DMP\* > Lib\Cmd\OutPipe.txt')
            call system('del /Q /S Data\Raw\Biomass\Ponderate\* > Lib\Cmd\OutPipe.txt')   
         endif     
      endif    




! Initialisation nom decades integration
      month=1
      day=1
      do decadl=1,72
         decadnamep(decadl)=numer2(month)//numer2(day)
         day=day+10
         if (day.eq.31) then
            day=1
            month=month+1
            if (month.eq.13) then
               month=1
            endif
         endif
      enddo         

      year=year1
      month=month1
      day=day1
      decadlastp=(month1-1)*3+(day1-1)/10+1

      nbdecadtot=0
      warn=0      
      
! Determination du nombre de decades
10    FE=0
      decadname=numer4(year)//numer2(month)//numer2(day)
      filename='Data\Raw\In\DMP\DMP_'//decadname//'.img'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         FE=FE+1
      endif
      filename='Data\Raw\In\NDVI\NDVI_'//decadname//'.img'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         FE=FE+1
      endif
      if (FE.eq.2) then     
      
         yearlast=year
         monthlast=month
         daylast=day
         decadlastp=decadlastp+1
         nbdecadtot=nbdecadtot+1
         decadyear(nbdecadtot)=year
         decadmonth(nbdecadtot)=month
         decaddecad(nbdecadtot)=day
         
         if ((year.eq.yearStop).and.(month.eq.monthStop).and.&
         (day.eq.dayStop).and.(LastStop.eq.1)) then
            goto 11
         endif
         
         day=day+10
         if (day.eq.31) then
            day=1
            month=month+1
            if (month.eq.13) then
               year=year+1
               month=1
               decadlastp=0
            endif
         endif

         goto 10
      else
         decadnameold=decadname
         day=day+10
         if (day.eq.31) then
            day=1
            month=month+1
            if (month.eq.13) then
               year=year+1
               month=1
               decadlastp=0
            endif
         endif
         FE=0
         decadname=numer4(year)//numer2(month)//numer2(day)
         filename='Data\Raw\In\DMP\DMP_'//decadname//'.img'
         inquire(file=filename,exist=filexist)
         if (filexist) then
            FE=FE+1
         endif
         filename='Data\Raw\In\NDVI\NDVI_'//decadname//'.img'
         inquire(file=filename,exist=filexist)
         if (filexist) then
            FE=FE+1
         endif
         if (FE.eq.2) then
            warn=warn+1 
            decadwarn(warn)=decadnameold
            
            yearlast=year
            monthlast=month
            daylast=day
            decadlastp=decadlastp+1
            nbdecadtot=nbdecadtot+1
            decadyear(nbdecadtot)=year
            decadmonth(nbdecadtot)=month
            decaddecad(nbdecadtot)=day
            goto 10
         endif
      endif
      
11    continue

      
! Initialisation des decades debut et fin integration
! Calcul de decaddebp et decadfinp en cas de calcul retro
      decadlast=(monthlast-1)*3+(daylast-1)/10+1
      if (decaddebp.le.0) then
         decaddebp=decadLast-decadfinp+1
         if (decaddebp.le.0) then
            decaddebp=decaddebp+36
         endif
         decadfinp=decadLast
      endif
      if (decadfinp.lt.decaddebp) then
         decadfinp=decadfinp+36
      endif
      nbDecadSomme=decadfinp-decaddebp+1
           
      
! Calcul du nombre année
      decadfirst=(month1-1)*3+(day1-1)/10+1
      if (decadfirst.le.decadfinp) then
         yearfirstint=year1
      else
         yearfirstint=year1+1
      endif
      if (decadlast.ge.decaddebp) then
         yearlastint=yearlast
      else
         yearlastint=yearlast-1
      endif
      nbyear=yearlastint-yearfirstint+1
      do year=1,nbyear
         years(year)=yearfirstint+year-1
      enddo

      
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
      
         
! Lecture anciens paramètres et determination degré optimisation
      filename='Lib\Param_Old\BioGenerator_Param_Old.txt'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         open(10,file=filename)
         read(10,*)& 
         nbdecadtotOld,&
         yearlastOld,monthlastOld,daylastOld,&
         yearfirstOld,monthfirstOld,dayfirstOld
         read(10,*)&
         PondUseFlagOld,F0Old,F1Old       
         read(10,*)&
         PondAccFlagOld,RWaterOld2,P0Old2,P1Old2,ForageFlagOld2
         read(10,*) OutOptOld         
         close(10)
         call system('del /Q '//filename//' > Lib\Cmd\OutPipe.txt')
      endif
      DegOpt=0
      if ((OutOpt.eq.1).and.(OutRaw.eq.0).and.(OutOptOld.eq.1).and.(Force.eq.0)) then
         if ((nbdecadtot.eq.nbdecadtotOld).and.&
            (yearlast.eq.yearlastOld).and.&
            (monthlast.eq.monthlastOld).and.&
            (daylast.eq.daylastOld)) then
            DegOpt=1
         endif
         if ((DegOpt.eq.1).and.&
            (PondUseFlag.eq.PondUseFlagOld).and.&
            (F0.eq.F0Old).and.&
            (F1.eq.F1Old).and.&
            (PondAccFlag.eq.PondAccFlagOld).and.&
            (RWater.eq.RWaterOld2).and.&
            (P0.eq.P0Old2).and.&
            (P1.eq.P1Old2).and.&
            (ForageFlag.eq.ForageFlagOld2)) then
            DegOpt=2
         endif
      endif
      

      
! Initialisation du nom des decades pour lectures des fichiers
      day=day1
      month=month1-(decaldecad/3)
      year=year1
      i=1
15    decadnameYYYYMMDD(i)=numer4(year)//numer2(month)//numer2(day)
      day=day+10
      if (day.eq.31) then
         day=1
         month=month+1
         if (month.eq.13) then
            month=1
            year=year+1
            if (year.gt.year1+nbyearmax-1) then
               goto 17
            endif
         endif
      endif
      i=i+1
      goto 15
      
17    day=day1
      month=month1-(decaldecad/3)
      do i=1,36
         decadnameMMDD(i)=numer2(month)//numer2(day)
         day=day+10
         if (day.eq.31) then
            day=1
            month=month+1
            if (month.eq.13) then
               month=1
            endif
         endif
      enddo


      if (nbdecadtot.ne.0) then
         if (quiet.ne.1) then
         write(*,*) ': OK'
         endif
         write(9,*) ': OK'        
         if (warn.ne.0) then
           if (warn.eq.1) then
              if (quiet.ne.1) then
              write(*,'(a)') ' Decade manquante  : '//decadwarn(warn)
              endif
              write(9,'(a)') ' Decade manquante  : '//decadwarn(warn)     
           else
              if (quiet.ne.1) then
              write(*,'(a,$)') ' Decades manquantes: '
              endif
              write(9,'(a,$)') ' Decades manquantes: '
              do loop=1,warn-1
                 if (quiet.ne.1) then
                 write(*,'(a,$)') decadwarn(loop)//'; '
                 endif
                 write(9,'(a,$)') decadwarn(loop)//'; '
              enddo
              if (quiet.ne.1) then
              write(*,'(a)') decadwarn(warn)
              endif
              write(9,'(a)') decadwarn(warn)
           endif
         endif
         if (quiet.ne.1) then
         write(*,*)
         endif
         write(9,*)
!         buff16S=0
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
      
      if (flagFiltre.eq.0) then
         if (quiet.ne.1) then
         write(*,*) 'Filtre spatial    : Nul'
         endif
         write(9,*) 'Filtre spatial    : Nul'
      else
         if (quiet.ne.1) then
         write(*,*) 'Filtre spatial    : '//numer2(nbLoopFiltre)//'*'//numer2(sizeFiltre)
         endif
         write(9,*) 'Filtre spatial    : '//numer2(nbLoopFiltre)//'*'//numer2(sizeFiltre)
      endif
      if (quiet.ne.1) then
      write(*,*) 'Integration       : '//decadnamep(decaddebp)//' > '//decadnamep(decadfinp)
      endif
      write(9,*) 'Integration       : '//decadnamep(decaddebp)//' > '//decadnamep(decadfinp)
      
      write(output1,*) nint(sDMPmin)
      write(output2,*) nint(100*SeuilCumulLastYear)
      if (quiet.ne.1) then
      write(*,*) 'Seuil Production  : Min = '//trim(adjustl(output1))//' kg/ha ; Seuil = '//trim(adjustl(output2))//' %'
      endif
      write(9,*) 'Seuil Production  : Min = '//trim(adjustl(output1))//' kg/ha ; Seuil = '//trim(adjustl(output2))//' %'   

      if (PondUseFlag.eq.0) then
         if (quiet.ne.1) then
         write(*,*) 'Utilisabilite     : Nul'
         endif
         write(9,*) 'Utilisabilite     : Nul'
      else
         write(output1,*) nint(100*F0)
         write(output2,*) nint(100*F1)
         if (quiet.ne.1) then
         write(*,*) 'Utilisabilite     : F0 = '//trim(adjustl(output1))//' % ; F1 = '//trim(adjustl(output2))//' %'
         endif
         write(9,*) 'Utilisabilite     : F0 = '//trim(adjustl(output1))//' % ; F1 = '//trim(adjustl(output2))//' %'     
      endif
      
      if (PondAccFlag.eq.0) then
         if (quiet.ne.1) then
         write(*,*) 'Accessibilite     : Nul'
         endif
         write(9,*) 'Accessibilite     : Nul'
      else
         write(output1,'(F6.2)') nint(RWater*100.)/100.
         write(output2,*) nint(100.*P0)
         write(output3,*) nint(100.*P1)
         if (quiet.ne.1) then    
         write(*,*) 'Accessibilite     : Dmax = '//trim(adjustl(output1))//' km ; P0 = '//trim(adjustl(output2))//&
                    ' % ; P1 = '//trim(adjustl(output3))//' %'
         endif
         write(9,*) 'Accessibilite     : Dmax = '//trim(adjustl(output1))//' km ; P0 = '//trim(adjustl(output2))//&
                    ' % ; P1 = '//trim(adjustl(output3))//' %'
      endif
      
      write(output1,*) nint(VIalpha)
      if (quiet.ne.1) then
      write(*,*) 'VI Vulnerabilite  : '//trim(adjustl(output1))//' %'
      endif
      write(9,*) 'VI Vulnerabilite  : '//trim(adjustl(output1))//' %'

      write(output1,*) nbdecadtot-warn
      if (quiet.ne.1) then
      write(*,*) 'Decades           : '//decadnameYYYYMMDD(1+decaldecad)//' > '//decadnameYYYYMMDD(nbdecadtot+decaldecad)
      write(*,*) 'Nombre de decades : '//trim(adjustl(output1))
      write(*,*) 'Annees            : '//numer4(year1)//' > '//numer4(year1+nbyear-1)
      endif
      write(9,*) 'Decades           : '//decadnameYYYYMMDD(1+decaldecad)//' > '//decadnameYYYYMMDD(nbdecadtot+decaldecad)
      write(9,*) 'Nombre de decades : '//trim(adjustl(output1))
      write(9,*) 'Annees            : '//numer4(year1)//' > '//numer4(year1+nbyear-1)

      
      



      
      
     
!ccccccccccccccccccccccc     
!c Calcul Water Access c
!ccccccccccccccccccccccc
      if (PondAccFlag.eq.1) then
         
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
18       read(10,*,iostat=iError) output1
         if (iError.eq.0) then
            write(9,*) output1
            goto 18
         endif
         close(10)
         
      endif      
               

! Vidage repertoires
      if (DegOpt.lt.1) then
         call system('del /Q Data\Raw\Biomass\Filter\DMP\* > Lib\Cmd\OutPipe.txt')
      endif
      if (DegOpt.lt.2) then
         call system('del /Q Data\Raw\Biomass\Ponderate\* > Lib\Cmd\OutPipe.txt')      
         call system('del /Q Data\Raw\Biomass\Type\DMP\* > Lib\Cmd\OutPipe.txt')
      endif

! PreWrite
      if (PreWrite.eq.1) then
         if (quiet.ne.1) then     
         write(*,'(a,a,$)') ' Pre-ecriture      :      ',back2 
         endif      
         write(9,'(a,$)') ' Pre-ecriture      :'   
         nbPreWrite=0
         if (DegOpt.lt.1) then
            if (OutRaw.eq.1) then
               do decad=1,nbdecadtot
                  nbPreWrite=nbPreWrite+1
                  PreWriteFile(nbPreWrite)='Data\Raw\Biomass\Filter\NDVI\NDVI_'//decadnameYYYYMMDD(decad)//'.img'         
               enddo
            endif
            if ((OutOpt.eq.1).or.(OutRaw.eq.1)) then
               do decad=1,nbdecadtot
                  nbPreWrite=nbPreWrite+1
                  PreWriteFile(nbPreWrite)='Data\Raw\Biomass\Filter\DMP\DMP_'//decadnameYYYYMMDD(decad)//'.img'
               enddo
            endif
         endif
         
         if (DegOpt.lt.2) then
            if ((OutOpt.eq.1).or.(OutRaw.eq.1)) then
               do decad=1,nbdecadtot            
                  nbPreWrite=nbPreWrite+1
                  PreWriteFile(nbPreWrite)='Data\Raw\Biomass\Ponderate\DMP_'//decadnameYYYYMMDD(decad)//'.img'
               enddo
               do decad=1,36
                  nbPreWrite=nbPreWrite+1
                  PreWriteFile(nbPreWrite)='Data\Raw\Biomass\Type\DMP\DMP_'//decadnameMMDD(decad)//'.img'
               enddo
            endif
            if (OutRaw.eq.1) then
               do decad=1,36
                  nbPreWrite=nbPreWrite+1
                  PreWriteFile(nbPreWrite)='Data\Raw\Biomass\Type\NDVI\NDVI_'//decadnameMMDD(decad)//'.img'
               enddo
            endif
         endif            
         
         nbPreWrite=nbPreWrite+1
         PreWriteFile(nbPreWrite)='Data\Raw\Biomass\Biomass\Biomass_Mean.img'

         do year=1,nbyear
            nbPreWrite=nbPreWrite+1
            PreWriteFile(nbPreWrite)='Data\Raw\Biomass\Biomass\Biomass_'//numer4(years(year))//'.img'
         enddo
         
         if (OutRaw.eq.1) then
            do year=1,nbyear
               do decadl=decaddebp,decadfinp
                  if (decadl.gt.36) then
                     decad=decadl-36
                  else
                     decad=decadl
                  endif
                  decadd=decadl+(year-1)*36
                  decadCumul(decad)=decadd
                  nbPreWrite=nbPreWrite+1
                  PreWriteFile(nbPreWrite)='Data\Raw\Biomass\Cumul\Cumul_'//decadnameYYYYMMDD(decadCumul(decad))//'.img'                    
               enddo
            enddo
         endif
         
         do year=1,nbyear
            nbPreWrite=nbPreWrite+1
            PreWriteFile(nbPreWrite)='Data\Raw\Biomass\VI\VI_'//numer4(years(year))//'.img'
         enddo

         nbPreWrite=nbPreWrite+1
         PreWriteFile(nbPreWrite)='Data\Raw\Biomass\Stat\Sigma.img'          
         nbPreWrite=nbPreWrite+1
         PreWriteFile(nbPreWrite)='Data\Raw\Biomass\Stat\Trend.img'        
         nbPreWrite=nbPreWrite+1
         PreWriteFile(nbPreWrite)='Data\Raw\Biomass\Stat\TrendPercent.img'        
         nbPreWrite=nbPreWrite+1
         PreWriteFile(nbPreWrite)='Data\Raw\Biomass\Stat\TrendR2.img'         
         
         do year=1,nbyear
            nbPreWrite=nbPreWrite+1
            PreWriteFile(nbPreWrite)='Data\Raw\Biomass\Anomaly\Anomaly_'//numer4(years(year))//'.img'

            nbPreWrite=nbPreWrite+1
            PreWriteFile(nbPreWrite)='Data\Raw\Biomass\Anomaly\AnomalySigma_'//numer4(years(year))//'.img'
         enddo
         
         outImg32R(:,:)=-9999.
         filename='Lib\Tmp\Empty.img'
         open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
         write(10,rec=1) outImg32R
         if (FlushDisk.eq.1) CALL FLUSH(10)
         close(10)         
         do iPreWrite=1,nbPreWrite
            if (quiet.ne.1) then
               percent=nint(100.*(iPreWrite-1.)/(nbPreWrite-1.))
               write(output1,'(I3.3)') percent
               write(*,'(a,a,a,a,$)') back3,trim(adjustl(output1)),' %',back2
            endif
            filename=PreWriteFile(iPreWrite)
!            open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
!            write(10,rec=1) outImg32R
!            if (FlushDisk.eq.1) CALL FLUSH(10)
!            close(10)
            call system('copy Lib\Tmp\Empty.img '//filename//'> Lib\Cmd\OutPipe.txt')
         enddo
         write(9,*) 'OK'
         call system('del Lib\Tmp\Empty.img')
      endif         
      
            
!cccccccccccccccccccc      
!c Calcul Principal c
!cccccccccccccccccccc

! Allocation mémoire avec détermination taille du blocksize
      nbdecadalloc=nbdecadtot+decaldecad+36
      nbyearalloc=nbyear
      do nblineblock=nbline,1,-1
         frac=(1.*nbline)/(1.*nblineblock)
         if (int(frac).eq.frac) then
            blocksize=nblineblock*nbpixel
            nbblock=nbline/nblineblock
            
            MemAllocNeed=&
            nbThread1*blocksize*(&
            4.*nbdecadalloc*2.+&
            2.+1.+4.+4.*3+&
            36.*4.*3.+&
            8.*10.+&
            nbyearalloc*4.*3.)      
            
            !write(*,*) MemAvailable,nblineblock,MemAllocNeed,nint(100.*MemAllocNeed/MemAvailable)/100.
            !read(*,*)
            
            if (MemAllocNeed.le.factorMem*MemAvailable) then
            
               allocate(DMPin(1:blocksize,1:nbdecadalloc),stat=allocStat)
               allocate(NDVin(1:blocksize,1:nbdecadalloc),stat=allocStat) 
               
               allocate(out16(1:blocksize),stat=allocStat)
               allocate(out8(1:blocksize),stat=allocStat)
               allocate(outBlock32R(1:blocksize),stat=allocStat)            
               allocate(PondDMPuse(1:blocksize),stat=allocStat)
               allocate(PondDMPacc(1:blocksize),stat=allocStat)              
               allocate(PondDMPForage(1:blocksize),stat=allocStat)
               
               allocate(NDVtype(1:blocksize,1:36),stat=allocStat)
               allocate(DMPtype(1:blocksize,1:36),stat=allocStat)   
               allocate(CumulDMP(1:blocksize,1:36),stat=allocStat)
               
               allocate(sumDMPType(1:blocksize),stat=allocStat)
               allocate(sumDMP(1:blocksize),stat=allocStat)         
               allocate(SigmaBlk(1:blocksize),stat=allocStat)
               allocate(TrendBlk(1:blocksize),stat=allocStat)
               allocate(TrendBlkP(1:blocksize),stat=allocStat)
               allocate(AnomDMP(1:blocksize),stat=allocStat)
               allocate(AnomSigma(1:blocksize),stat=allocStat)
               allocate(SigmaBlkLastp(1:blocksize),stat=allocStat)
               allocate(VI(1:blocksize),stat=allocStat)
               allocate(R2Blk(1:blocksize),stat=allocStat)     
               
               allocate(BiomassBlk(1:blocksize,1:nbyearalloc),stat=allocStat)
               allocate(sumDMPlastp(1:blocksize,1:nbyearalloc),stat=allocStat)
               allocate(AnVI(1:blocksize,1:nbyearalloc),stat=allocStat)       
   
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
               
               

      if (quiet.ne.1) then
      write(*,*)      
      write(*,'(a,a,$)') ' Calcul principal  :      ',back2 
      endif
      write(9,*)      
      write(9,'(a,$)') ' Calcul principal  :'     

 
      
! Initialisation des decades suplementaires
      DMPin(:,:)=-9999.
      NDVin(:,:)=-9999.    
      nbdecadtot=nbdecadtot+decaldecad      
      

! Boucle Principale sur les blocks

      nbblockdonetot=0
      jc=0 !Numero de block global
      
      !$OMP PARALLEL DO NUM_THREADS(nbThread1) DEFAULT(NONE) &
      !$OMP& PRIVATE(j, ThreadID, DMPin, NDVin, decad, filename, filexist, out16, i, out8, buff16S, Moy, nb, dd, Sigma, &
      !$OMP& Somme, NDV1, DMP1, NDV2, DMP2, Andv, Bndv, Admp, Bdmp, decadd, decadfirst, DiffNDV, aNDVm, decadm, aNDVi, &
      !$OMP& outBlock32R, PondDMPuse, PondDMPForage, DMPtype, year, poso, posd, StackDMP, DMPTypeF, NDVType, StackNDV, &
      !$OMP& NDVTypeF, sumDMPType, decadl, AnVI, decadCumul, sumDMP, CumulDMP, BiomassBlk, sumDMPlastp, nbback, sumback, &
      !$OMP& yearback, VIfact, VI, SigmaBlk, TrendBlk, TrendBlkP, R2Blk, sumX, sumY, S2X, S2Y, SXY, SigmaBlkLastp, &
      !$OMP& AnomDMP, AnomSigma, sDMPin, sDMPtype, nbTot, sDMPtypeTot, PondDMPacc, nbThreadActual, &
      !$OMP& aok, bok, fok, lastdecadok) &
      !$OMP& SHARED(jc, percent, nbblockdone, nbblock, quiet, back3, back2, DegOpt, nbdecadtot, blocksize, &
      !$OMP& OutRaw, OutOpt, Profil, PondUseFlag, decadnameYYYYMMDD, decadnameMMDD, F1, F0, numer4, VIalpha, &
      !$OMP& sDMPmin, SeuilCumulLastYear, nbThread1, nbThread2, nbblockdonetot, output1, &
      !$OMP& ForageFlag, PondAccFlag, nbyear, decaddebp, decadfinp, nbDecadSomme, decadlastp, years)
      do jb=1,nbblock
      
         !$OMP CRITICAL
         jc=jc+1
         j=jc !Numero de block local
         nbThreadActual=OMP_get_num_threads()
         ThreadID = OMP_get_thread_num()
         nbblockdonetot=nbblockdonetot+1
         percent=nint(100.*(nbblockdonetot-1.)/(nbblock-1.))
         write(output1,'(I3.3)') percent
         if (quiet.ne.1) then
            write(*,'(a,a,a,a,$)') back3,trim(adjustl(output1)),' %',back2
         endif
         !$OMP END CRITICAL
         

         
         if (DegOpt.lt.1) then       
! Chargement des données DMP, NDVI sur le block sur l'ensemble des decades
            DMPin(:,:)=-9999.
            NDVin(:,:)=-9999.
            
            !$OMP CRITICAL
            !write(*,*) 'Read start : ',ThreadID
            do decad=1,nbdecadtot
               filename='Data\Raw\In\DMP\DMP_'//decadnameYYYYMMDD(decad)//'.img'
               inquire(file=filename,exist=filexist)
               if (filexist) then
                  open(10,file=filename,access='direct',status='old',recl=2*blocksize)
                  read(10,rec=j) out16
                  close(10)
               else
                  out16(:)=-1
               endif
               
               !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED)
               do i=1,blocksize
                  if (out16(i).ge.0) then
                     DMPin(i,decad)=0.01*out16(i)
                  endif
               enddo
               !$OMP END PARALLEL DO
               
               filename='Data\Raw\In\NDVI\NDVI_'//decadnameYYYYMMDD(decad)//'.img'
               inquire(file=filename,exist=filexist)
               if (filexist) then
                  open(10,file=filename,access='direct',status='old',recl=1*blocksize)
                  read(10,rec=j) out8
                  close(10)
               else
                  out8=char(0)
               endif
               !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED)                  
               do i=1,blocksize
                  buff16S=ichar(out8(i))
                  if (buff16S.ne.0) then
                     NDVin(i,decad)=0.004*buff16S-0.1
                  endif
               enddo
               !$OMP END PARALLEL DO 
               
               !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED)   
               do i=1,blocksize               
                  if ((NDVin(i,decad).eq.-9999.).or.&
                      (DMPin(i,decad).eq.-9999.)) then
                     NDVin(i,decad)=-9999.
                     DMPin(i,decad)=-9999.
                  endif
               enddo
               !$OMP END PARALLEL DO 
               
            enddo
            !write(*,*) 'Read end : ',ThreadID
            !$OMP END CRITICAL
            
! Sauvegarde pour copie en début et fin de serie après filtrage
!            NDVinb(:,:)=NDVin(:,:)
!            DMPinb(:,:)=DMPin(:,:)
                           
            
! Filtre PassBas sur NDV & Filtre sur DMP Low & Filtre sur DMP max jump & Interpolation
            !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED) & 
            !$OMP& PRIVATE(decad, Moy, nb, dd, Sigma, Somme, NDV1, DMP1, &
            !$OMP& NDV2, DMP2, Andv, Bndv, Admp, Bdmp, decadd, decadfirst, DiffNDV, aNDVm, decadm, aNDVi)
            do i=1,blocksize
               do decad=1+FiltreSizeSigma,nbdecadtot-FiltreSizeSigma
                  Moy=0.
                  nb=0
                  do dd=-FiltreSizeSigma,FiltreSizeSigma
                     if ((NDVin(i,decad+dd).ne.-9999.).and.(dd.ne.0)) then
                        Moy=Moy+NDVin(i,decad+dd)
                        nb=nb+1
                     endif
                  enddo
                  if (nb.eq.2*FiltreSizeSigma) then
                     Sigma=0.
                     Moy=Moy/nb
                     do dd=-FiltreSizeSigma,FiltreSizeSigma
                        if (dd.ne.0) then
                           Sigma=Sigma+(NDVin(i,decad+dd)-Moy)**2
                        endif
                     enddo
                     Sigma=(Sigma/(nb-1))**.5
                     if (abs(NDVin(i,decad)-Moy).gt.SigmaNDV*Sigma) then
                        NDVin(i,decad)=-9999.
                        DMPin(i,decad)=-9999.
                     endif
                  endif
               enddo
   
               nb=0
               Somme=0.
               do decad=1,nbdecadtot
                  if (DMPin(i,decad).gt.0.) then
                     Somme=Somme+DMPin(i,decad)
                     nb=nb+1
                  endif
               enddo
               if (nb.ne.0) then
                  if (nb.le.SeuilNBlow) then
                     do decad=1,nbdecadtot
                        if (DMPin(i,decad).gt.0) then
                           DMPin(i,decad)=-9999.
                           NDVin(i,decad)=-9999.
                        endif
                     enddo
                  endif
               endif
               
               if (DMPMaxJumpFilter.eq.1) then
                  do decad=2,nbdecadtot-1
                     if ((DMPin(i,decad).ne.-9999.).and.&
                       (((DMPin(i,decad+1).ne.-9999.).and.&
                         (DMPin(i,decad)-DMPin(i,decad+1).gt.DMPMaxJump)).or.&
                        ((DMPin(i,decad-1).ne.-9999.).and.&
                         (DMPin(i,decad)-DMPin(i,decad-1).gt.DMPMaxJump)))) then
                        DMPin(i,decad)=-9999.
                        NDVin(i,decad)=-9999.
                     endif
                  enddo
               endif
                  
                     
   
               decad=1
21             if (NDVin(i,decad).eq.-9999.) then
                  decad=decad+1
                  if (decad.ge.nbdecadtot) then
                     goto 24
                  else
                     goto 21
                  endif
               endif            
22             if (NDVin(i,decad).ne.-9999.) then
                  decad=decad+1
                  if (decad.ge.nbdecadtot) then
                     goto 24
                  endif
                  goto 22
               endif
               NDV1=NDVin(i,decad-1)
               DMP1=DMPin(i,decad-1)
               dd=0
23             if (NDVin(i,decad+dd).eq.-9999.) then
                  dd=dd+1
                  if (decad+dd.gt.nbdecadtot) then
                     goto 24
                  endif
                  goto 23
               endif
               NDV2=NDVin(i,decad+dd)
               DMP2=DMPin(i,decad+dd)
               Andv=(NDV2-NDV1)/(1.*dd+1.)
               Bndv=NDV1-Andv*(1.*decad-1.)
               Admp=(DMP2-DMP1)/(1.*dd+1.)
               Bdmp=DMP1-Admp*(1.*decad-1.)
   
               do decadd=decad,decad+dd-1
                  NDVin(i,decadd)=Andv*decadd+Bndv
                  DMPin(i,decadd)=Admp*decadd+Bdmp
               enddo
               decad=decad+dd
               dd=0
               goto 22
24             continue
   
   
! Passe du filtre temporel BISE
               decad=1
               decadfirst=decad
30             if (NDVin(i,decad).eq.-9999.) then
                  decad=decad+1
                  decadfirst=decad
                  if (decad.ge.nbdecadtot-FiltreSize) then
                     goto 70
                  else
                     goto 30
                  endif
               endif
   
40             DiffNDV=1.-NDVin(i,decad+1)/NDVin(i,decad)
               if ((DiffNDV.gt.SeuilNDV).and.&
                   (NDVin(i,decad+1).ne.-9999.)) then
                  aNDVm=-9999.
                  decadm=decad
                  do decadd=decad+2,decad+FiltreSize
                     aNDVi=(NDVin(i,decadd)-NDVin(i,decad))/(decadd-decad)
                     if (aNDVi.gt.aNDVm) then
                        decadm=decadd
                        aNDVm=aNDVi
                     endif
                  enddo
                  do decadd=decad+1,decadm-1
                     NDVin(i,decadd)=-9999.
                     DMPin(i,decadd)=-9999.
                  enddo
                  decad=decadm
               endif
   
               if (decad+FiltreSize.lt.nbdecadtot-1) then
                  decad=decad+1
                  goto 40
               endif
   
               decad=decadfirst
50             if (NDVin(i,decad).ne.-9999.) then
                  decad=decad+1
                  if (decad.ge.nbdecadtot) then
                     goto 70
                  endif
                  goto 50
               endif
   
               NDV1=NDVin(i,decad-1)
               DMP1=DMPin(i,decad-1)
               dd=0
60             if (NDVin(i,decad+dd).eq.-9999.) then
                  dd=dd+1
                  if (decad+dd.gt.nbdecadtot) then
                     goto 70
                  endif
                  goto 60
               endif
   
               NDV2=NDVin(i,decad+dd)
               DMP2=DMPin(i,decad+dd)
               Andv=(NDV2-NDV1)/(1.*dd+1.)
               Bndv=NDV1-Andv*(1.*decad-1.)
               Admp=(DMP2-DMP1)/(1.*dd+1.)
               Bdmp=DMP1-Admp*(1.*decad-1.)
   
               do decadd=decad,decad+dd-1
                  NDVin(i,decadd)=Andv*decadd+Bndv
                  DMPin(i,decadd)=Admp*decadd+Bdmp
               enddo
               decad=decad+dd
               dd=0
               goto 50
70             continue
            enddo
            !$OMP END PARALLEL DO
            

! Sauvegardage des données filtrées
            !$OMP CRITICAL
            !write(*,*) 'Write start : ',ThreadID
            do decad=1,nbdecadtot
               if (OutRaw.eq.1) then
                  outBlock32R(:)=NDVin(:,decad)
                  filename='Data\Raw\Biomass\Filter\NDVI\NDVI_'//decadnameYYYYMMDD(decad)//'.img'
                  open(10,file=filename,access='direct',recl=4*blocksize)
                  write(10,rec=j) outBlock32R
                  if (FlushDisk.eq.1) CALL FLUSH(10)
                  close(10)
               endif

               if ((OutOpt.eq.1).or.(OutRaw.eq.1).or.(Profil.eq.1)) then
                  outBlock32R(:)=DMPin(:,decad)
                  filename='Data\Raw\Biomass\Filter\DMP\DMP_'//decadnameYYYYMMDD(decad)//'.img'
                  open(10,file=filename,access='direct',recl=4*blocksize)
                  write(10,rec=j) outBlock32R
                  if (FlushDisk.eq.1) CALL FLUSH(10)
                  close(10)
               endif
            enddo
            !write(*,*) 'Write end : ',ThreadID
            !$OMP END CRITICAL
         endif
         
! lecture données filtrées dans le cas OptDeg=1    
         if (DegOpt.eq.1) then
            !$OMP CRITICAL
            ! write(*,*) 'Read start : ',ThreadID
            do decad=1,nbdecadtot
               DMPin(:,decad)=outBlock32R(:)
               filename='Data\Raw\Biomass\Filter\DMP\DMP_'//decadnameYYYYMMDD(decad)//'.img'
               open(10,file=filename,access='direct',recl=4*blocksize)
               read(10,rec=j) outBlock32R
               close(10)
            enddo
            ! write(*,*) 'Read end : ',ThreadID
            !$OMP END CRITICAL
         endif            


!ccccccccccccccccccc
!c Ponderation DMP c
!ccccccccccccccccccc

         if (DegOpt.lt.2) then
! Initialisation Facteur Ponderation     
! Ponderation Usable
            if ((PondUseFlag.eq.1).or.(PondAccFlag.eq.1)) then
               !$OMP CRITICAL
               ! write(*,*) 'Read start : ',ThreadID
               if (PondUseFlag.eq.1) then
                  filename='Lib\Ancillary\Img\ConvFactor.img'
                  open(10,file=filename,access='direct',status='old',recl=4*blocksize)
                  read(10,rec=j) outBlock32R
                  close(10)
                  PondDMPuse(:)=((F1-F0)*outBlock32R(:)+F0)              
               else
                  PondDMPuse(:)=1.
               endif
   
! lecture PondForage sur le block
               if ((ForageFlag.eq.1).and.(PondAccFlag.eq.1)) then
                  filename='Data\Raw\Water\Bores\Bores.img'
                  open(10,file=filename,access='direct',recl=4*blocksize)
                  read(10,rec=j) outBlock32R
                  close(10)
                  PondDMPForage(:)=outBlock32R(:)
               else
                  PondDMPForage(:)=0.
               endif
               
               do decad=1,nbdecadtot                  
! Ponderation Accessible                  
                  if (PondAccFlag.eq.1) then
                     filename='Data\Raw\Water\WaterAccess\WaterAccess_'//decadnameYYYYMMDD(decad)//'.img'
                     inquire(file=filename,exist=filexist)
                     if (.not.filexist) then
                        decadd=decad-36*int((decad-1.)/36.)
                        filename='Data\Raw\Water\WaterAccessType\WaterAccessType_'//decadnameMMDD(decadd)//'.img'
                     endif
                     open(10,file=filename,access='direct',status='old',recl=4*blocksize)
                     read(10,rec=j) outBlock32R
                     close(10)
                     PondDMPacc(:)=outBlock32R(:)
! Maximisation pour les forages
                     !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED)  
                     do i=1,blocksize
                        if (PondDMPForage(i).gt.PondDMPacc(i)) then
                           PondDMPacc(i)=PondDMPForage(i)
                        endif
                     enddo
                     !$OMP END PARALLEL DO
                  else
                     PondDMPacc(:)=1.
                  endif
                  
                  !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED)  
                  do i=1,blocksize            
                     if ((DMPin(i,decad).ne.-9999.).and.(PondDMPacc(i).ne.-9999.)) then
                        DMPin(i,decad)=PondDMPuse(i)*PondDMPacc(i)*DMPin(i,decad)
                     else
                        DMPin(i,decad)=-9999.
                     endif
                  enddo
                  !$OMP END PARALLEL DO
               enddo
               ! write(*,*) 'Read end : ',ThreadID
               !$OMP END CRITICAL
            endif
            
            
!cccccccccccccccccccccccccccccccccccccccccccccccccc        
!c Calcul de l'année type moyenne  & Filtrage DMP c  
!cccccccccccccccccccccccccccccccccccccccccccccccccc
            DMPtype(:,:)=0.
            !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED) &
            !$OMP& PRIVATE(decad, nb, year, decadd, poso, posd, StackDMP, DMPTypeF) 
            do i=1,blocksize
               do decad=1,36
                  nb=0.
                  do year=1,nbyear
                     decadd=decad+(year-1)*36
                     if ((decadd.le.nbdecadtot).and.(DMPin(i,decadd).ne.-9999.)) then
                        nb=nb+1.
                        DMPtype(i,decad)=DMPtype(i,decad)+DMPin(i,decadd)
                     endif
                  enddo
                  if (nb.gt.0.) then
                     DMPtype(i,decad)=DMPtype(i,decad)/nb
                  else
                     DMPtype(i,decad)=-9999.
                  endif
               enddo
   
! Lissage Année Type            
               do decad=1,36
                  poso=0
                  do decadd=-1,1
                     posd=decad+decadd
                     if (posd.lt.1) posd=36
                     if (posd.gt.36) posd=1
                     if (DMPtype(i,posd).ne.-9999.) then
                        poso=poso+1
                        StackDMP(poso)=DMPtype(i,posd)
                     endif
                  enddo
                  if (poso.eq.3) then
                     DMPTypeF(decad)=0.25*(StackDMP(1)+StackDMP(3))+0.50*StackDMP(2)
                  else
                     DMPTypeF(decad)=-9999.   
                  endif
               enddo
               DMPType(i,:)=DMPTypeF(:)              
            enddo
            !$OMP END PARALLEL DO
            
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc         
!c Calcul de l'année type moyenne Median & Filtrage NDVI c  
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            if (OutRaw.eq.1) then
            NDVtype(:,:)=0.
            !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED) &
            !$OMP& PRIVATE(decad, nb, year, decadd, poso, posd, StackNDV, NDVTypeF) 
            do i=1,blocksize             
               do decad=1,36
                  nb=0.
                  do year=1,nbyear
                     decadd=decad+(year-1)*36
                     if (NDVin(i,decadd).ne.-9999.) then
                        nb=nb+1.
                        NDVtype(i,decad)=NDVtype(i,decad)+NDVin(i,decadd)
                     endif
                  enddo
                  if (nb.gt.0.) then
                     NDVtype(i,decad)=NDVtype(i,decad)/nb
                  else
                     NDVtype(i,decad)=-9999.
                  endif
               enddo
   
! Lissage Année Type            
               do decad=1,36
                  poso=0
                  do decadd=-1,1
                     posd=decad+decadd
                     if (posd.eq.0) then
                        posd=36
                     endif
                     if (posd.eq.37) then
                        posd=1
                     endif
                     if (NDVtype(i,posd).ne.-9999) then
                        poso=poso+1
                        StackNDV(poso)=NDVtype(i,posd)
                     endif
                  enddo
                  if (poso.eq.3) then
                     NDVTypeF(decad)=0.25*(StackNDV(1)+StackNDV(3))+0.5*StackNDV(2)
                  else
                     NDVTypeF(decad)=-9999.  
                  endif
               enddo
               NDVType(i,:)=NDVTypeF(:)             
            enddo
            !$OMP END PARALLEL DO
            endif            

! Sauvegardage des données pondérées & type
            !$OMP CRITICAL
            ! write(*,*) 'Write start : ',ThreadID
            if ((OutOpt.eq.1).or.(OutRaw.eq.1)) then
               do decad=1,nbdecadtot
                  outBlock32R(:)=DMPin(:,decad)
                  filename='Data\Raw\Biomass\Ponderate\DMP_'//decadnameYYYYMMDD(decad)//'.img'
                  open(10,file=filename,access='direct',recl=4*blocksize)
                  write(10,rec=j) outBlock32R
                  if (FlushDisk.eq.1) CALL FLUSH(10)
                  close(10)
               enddo
               do decad=1,36
                  outBlock32R(:)=DMPtype(:,decad)
                  filename='Data\Raw\Biomass\Type\DMP\DMP_'//decadnameMMDD(decad)//'.img'
                  open(10,file=filename,access='direct',recl=4*blocksize)
                  write(10,rec=j) outBlock32R
                  if (FlushDisk.eq.1) CALL FLUSH(10)
                  close(10)
               enddo
            endif
            
            if (OutRaw.eq.1) then
               do decad=1,36
                  outBlock32R(:)=NDVtype(:,decad)
                  filename='Data\Raw\Biomass\Type\NDVI\NDVI_'//decadnameMMDD(decad)//'.img'
                  open(10,file=filename,access='direct',recl=4*blocksize)
                  write(10,rec=j) outBlock32R
                  if (FlushDisk.eq.1) CALL FLUSH(10)
                  close(10)
               enddo
            endif
            ! write(*,*) 'Write end : ',ThreadID
            !$OMP END CRITICAL
         endif
         
! lecture données pondérées dans le cas OptDeg=2    
         if (DegOpt.eq.2) then 
            !$OMP CRITICAL
            ! write(*,*) 'Read start : ',ThreadID
            DMPin(:,:)=-9999.
            do decad=1,nbdecadtot
               filename='Data\Raw\Biomass\Ponderate\DMP_'//decadnameYYYYMMDD(decad)//'.img'
               open(10,file=filename,access='direct',recl=4*blocksize)
               read(10,rec=j) outBlock32R
               close(10)
               DMPin(:,decad)=outBlock32R(:)
            enddo
            do decad=1,36
               filename='Data\Raw\Biomass\Type\DMP\DMP_'//decadnameMMDD(decad)//'.img'
               open(10,file=filename,access='direct',recl=4*blocksize)
               read(10,rec=j) outBlock32R
               close(10)
               DMPtype(:,decad)=outBlock32R(:)
            enddo
            ! write(*,*) 'Read end : ',ThreadID
            !$OMP END CRITICAL
         endif              


! Calcul de la somme sur l'année type 
         sumDMPType(:)=0.
         !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED) PRIVATE(nb, decadl, decad)        
         do i=1,blocksize
            nb=0
            do decadl=decaddebp,decadfinp
               if (decadl.gt.36) then
                  decad=decadl-36
               else
                  decad=decadl
               endif
               if (DMPtype(i,decad).ne.-9999.) then
                  sumDMPType(i)=sumDMPType(i)+DMPtype(i,decad)
                  nb=nb+1
               endif
            enddo
            if (nb.eq.nbDecadSomme) then
               sumDMPType(i)=CoefDMP*sumDMPType(i)
            else
               sumDMPType(i)=-9999.
            endif
         enddo
         !$OMP END PARALLEL DO
         
         outBlock32R(:)=sumDMPType(:)
         filename='Data\Raw\Biomass\Biomass\Biomass_Mean.img'
         !$OMP CRITICAL
         ! write(*,*) 'Write start : ',ThreadID
         open(10,file=filename,access='direct',recl=4*blocksize)
         write(10,rec=j) outBlock32R
         if (FlushDisk.eq.1) CALL FLUSH(10)
         close(10)
         ! write(*,*) 'Write end : ',ThreadID
         !$OMP END CRITICAL

        

! Calcul des anomalies %, VI 		 
         AnVI(:,:)=-9999.
         do year=1,nbyear
         
! Calcul des anomalies % pour chaque année         
            AnomDMP(:)=-9999.
            !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED) &
            !$OMP& PRIVATE(nb, sDMPin, SDMPtype, nbTot, sDMPtypeTot, decadl, decad, decadd) 
            do i=1,blocksize
               nb=0.
               sDMPin=0.
               sDMPtype=0.
               nbTot=0.
               sDMPtypeTot=0.
               do decadl=decaddebp,decadfinp
                  if (decadl.gt.36) then
                     decad=decadl-36
                  else
                     decad=decadl
                  endif
                  decadd=decadl+(year-1)*36
                  if (DMPtype(i,decad).ne.-9999.) then
                     sDMPtypeTot=sDMPtypeTot+DMPtype(i,decad)
                     nbTot=nbTot+1.
                     if (DMPin(i,decadd).ne.-9999.) then
                        sDMPin=sDMPin+DMPin(i,decadd)
                        sDMPtype=sDMPtype+DMPtype(i,decad)
                        nb=nb+1.
                     endif
                  endif                  
               enddo
               
               if (nb.gt.0.) then
                  if (coefDMP*sDMPtypeTot.le.sDMPmin) then
                     if (sDMPtypeTot.eq.0.) then
                        AnomDMP(i)=-9998.
                     else
                        AnomDMP(i)=-9996.
                     endif
                  else
                     AnomDMP(i)=100.*sDMPin/sDMPtype                                 
                     if ((SDMPTypeTot.gt.0.).and.(sDMPtype/sDMPtypeTot.lt.SeuilCumulLastYear)) then
                        AnomDMP(i)=-9997.
                     endif
                  endif
               endif
            enddo
            !$OMP END PARALLEL DO
            
            !$OMP CRITICAL
            outBlock32R(:)=AnomDMP(:)
            filename='Data\Raw\Biomass\Anomaly\Anomaly_'//numer4(years(year))//'.img'
            open(10,file=filename,access='direct',recl=4*blocksize)
            write(10,rec=j) outBlock32R
            if (FlushDisk.eq.1) CALL FLUSH(10)
            close(10)

            outBlock32R(:)=AnomSigma(:)
            filename='Data\Raw\Biomass\Anomaly\AnomalySigma_'//numer4(years(year))//'.img'
            open(10,file=filename,access='direct',recl=4*blocksize)
            write(10,rec=j) outBlock32R
            if (FlushDisk.eq.1) CALL FLUSH(10)
            close(10)
            !$OMP END CRITICAL 
            
         
! Calcul de la somme sur chaque année (AnVI année en cours)
         
            decadCumul(:)=-9999
            do decadl=decaddebp,decadfinp
               if (decadl.gt.36) then
                  decad=decadl-36
               else
                  decad=decadl
               endif
               decadd=decadl+(year-1)*36
               decadCumul(decad)=decadd
            enddo
         
            sumDMP(:)=0.
            CumulDMP(:,:)=0.
            !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED) PRIVATE(nb, decadl, decad, decadd, lastdecadok, aok, bok, fok) 
            do i=1,blocksize
!               sumBnDenVI=0.
!               sumBnNumVI=0.
               nb=0.
               lastdecadok=0
               do decadl=decaddebp,decadfinp
                  if (decadl.gt.36) then
                     decad=decadl-36
                  else
                     decad=decadl
                  endif
                  decadd=decadl+(year-1)*36
                  if ((decadd.le.nbdecadtot).and.(DMPin(i,decadd).ne.-9999.)) then     
                     nb=nb+1.                   
                     sumDMP(i)=sumDMP(i)+DMPin(i,decadd)
                     lastdecadok=decadl
!                     sumBnNumVI=sumBnNumVI+DMPin(i,decadd)                    
!                     if (year.eq.nbyear) then
!                        sumback=0.
!                        nbback=0.
!                        do yearback=year,1,-1
!                           decadback=decadl+(yearback-1)*36
!                           VIfact=(1.-VIalpha/100.)**(year-yearback)
!                           if (DMPin(i,decadback).ne.-9999.) then
!                              sumback=sumback+VIfact*DMPin(i,decadback)
!                              nbback=nbback+VIfact
!                           else
!                              if (DMPtype(i,decad).ne.9999) then
!                                 sumback=sumback+VIfact*DMPtype(i,decad)                        
!                                 nbback=nbback+VIfact
!                              endif
!                           endif
!                        enddo
!                        if (nbback.ne.0.) then
!                           sumBnDenVI=sumBnDenVI+sumback/nbback
!                        endif
!                     endif
                         
                  else
                     if (DMPtype(i,decad).ne.-9999.) then
                        nb=nb+1.
                        if ((AnomDMP(i).ge.0.).and.(lastdecadok.ne.0)) then
                           aok=(AnomDMP(i)/100.-1.)/(lastdecadok-decadfinp)
                           bok=1.-aok*decadfinp
                           fok=aok*decadl+bok
                           sumDMP(i)=sumDMP(i)+DMPtype(i,decad)*fok
                        else
                           sumDMP(i)=sumDMP(i)+DMPtype(i,decad)
                        endif
                     endif
                  endif
                  if (nb.eq.decadl-decaddebp+1) then
                     CumulDMP(i,decad)=CoefDMP*sumDMP(i)
                  else
                     CumulDMP(i,decad)=-9999.
                  endif
               enddo
               if (nb.eq.nbDecadSomme) then
                  sumDMP(i)=CoefDMP*sumDMP(i)
               else
                  sumDMP(i)=-9999.
               endif
               
!               if (year.eq.nbyear) then
!                  if (sumBnDenVI.ne.0.) then
!                     AnVI(i,year)=sumBnNumVI/sumBnDenVI
!                  else
!                     AnVI(i,year)=-9998.
!                  endif
!               endif
                         
            enddo
            !$OMP END PARALLEL DO
            
            BiomassBlk(:,year)=sumDMP(:)
            
            outBlock32R(:)=sumDMP(:)
            filename='Data\Raw\Biomass\Biomass\Biomass_'//numer4(years(year))//'.img'
            !$OMP CRITICAL
            ! write(*,*) 'Write start : ',ThreadID
            open(10,file=filename,access='direct',recl=4*blocksize)
            write(10,rec=j) outBlock32R
            if (FlushDisk.eq.1) CALL FLUSH(10)
            close(10)

            
! Sauvegardage des données Cumulées
            if (OutRaw.eq.1) then
               do decad=1,36
                  if (decadCumul(decad).ne.-9999) then
                     outBlock32R(:)=CumulDMP(:,decad)
                     filename='Data\Raw\Biomass\Cumul\Cumul_'//decadnameYYYYMMDD(decadCumul(decad))//'.img'
                     
                     open(10,file=filename,access='direct',recl=4*blocksize)
                     write(10,rec=j) outBlock32R
                     if (FlushDisk.eq.1) CALL FLUSH(10)
                     close(10)
                  endif
               enddo
            endif
            ! write(*,*) 'Write end : ',ThreadID
            !$OMP END CRITICAL

! Mise en mémoire données Cumulées jusqu'à dernière decade année en cours
            sumDMPlastp(:,year)=CumulDMP(:,decadlastp)
         enddo
		 
		 
! Calcul de Sigma, Trend et R2
         SigmaBlk(:)=-9999.
         TrendBlk(:)=-9999.
         TrendBlkP(:)=-9999.
         R2Blk(:)=-9999.
         !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED) PRIVATE(nb, sumX, sumY, year, S2X, S2Y, SXY)
         do i=1,blocksize
            nb=0.
            sumX=0.
            sumY=0.
            do year=1,nbyear
               if (BiomassBlk(i,year).ge.0.) then
                  nb=nb+1.
                  sumX=sumX+1.*year
                  sumY=sumY+BiomassBlk(i,year)
               endif
            enddo
            if (nb.gt.1.) then
               S2X=0.
               S2Y=0.
               SXY=0.
               sumX=sumX/nb
               sumY=sumY/nb
               do year=1,nbyear
                  if (BiomassBlk(i,year).ge.0.) then
                     S2X=S2X+(1.*year-sumX)**2
                     S2Y=S2Y+(BiomassBlk(i,year)-sumY)**2
                     SXY=SXY+(1.*year-sumX)*(BiomassBlk(i,year)-sumY)
                  endif
               enddo
               
               if (S2Y.gt.0.) then
                  SigmaBlk(i)=sqrt(S2Y/nb)
               
                  S2X=S2X/(nb-1)
                  S2Y=S2Y/(nb-1)
                  SXY=SXY/(nb-1)
                    
                  TrendBlk(i)=SXY/S2X
                  TrendBlkP(i)=100.*TrendBlk(i)/sumY
               
                  R2Blk(i)=(SXY**2)/(S2X*S2Y)
               else
                  R2Blk(i)=-9998.
                  TrendBlk(i)=-9998.
                  TrendBlkP(i)=-9998.
                  SigmaBlk(i)=-9998.
               endif
            endif
         enddo
         !$OMP END PARALLEL DO
         
         !$OMP CRITICAL
         ! write(*,*) 'Write start : ',ThreadID        
         outBlock32R(:)=SigmaBlk(:)
         filename='Data\Raw\Biomass\Stat\Sigma.img'       
         open(10,file=filename,access='direct',recl=4*blocksize)
         write(10,rec=j) outBlock32R
         if (FlushDisk.eq.1) CALL FLUSH(10)
         close(10)         
         outBlock32R(:)=TrendBlk(:)    
         filename='Data\Raw\Biomass\Stat\Trend.img'        
         open(10,file=filename,access='direct',recl=4*blocksize)
         write(10,rec=j) outBlock32R
         if (FlushDisk.eq.1) CALL FLUSH(10)
         close(10)
         outBlock32R(:)=TrendBlkP(:)
         filename='Data\Raw\Biomass\Stat\TrendPercent.img'        
         open(10,file=filename,access='direct',recl=4*blocksize)
         write(10,rec=j) outBlock32R
         if (FlushDisk.eq.1) CALL FLUSH(10)
         close(10)        
         outBlock32R(:)=R2Blk(:)
         filename='Data\Raw\Biomass\Stat\TrendR2.img'         
         open(10,file=filename,access='direct',recl=4*blocksize)
         write(10,rec=j) outBlock32R
         if (FlushDisk.eq.1) CALL FLUSH(10)
         close(10)
         ! write(*,*) 'Write end : ',ThreadID
         !$OMP END CRITICAL

! Calcul Sigma jusqu'à fin année en cours
         SigmaBlkLastp(:)=-9999.
         !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED) PRIVATE(nb, sumY, year, S2Y)
         do i=1,blocksize
            nb=0.
            sumY=0.
            do year=1,nbyear
               if (sumDMPlastp(i,year).ge.0.) then
                  nb=nb+1.
                  sumY=sumY+sumDMPlastp(i,year)
               endif
            enddo
            if (nb.gt.1.) then
               S2Y=0.
               sumY=sumY/nb
               do year=1,nbyear
                  if (sumDMPlastp(i,year).ge.0.) then
                     S2Y=S2Y+(sumDMPlastp(i,year)-sumY)**2
                  endif
               enddo
               SigmaBlkLastp(i)=sqrt(S2Y/nb)
            endif
         enddo
         !$OMP END PARALLEL DO    		 
		 

! Calcul des anomalies Sigma pour chaque année  
         do year=1,nbyear
            AnomSigma(:)=-9999.
            !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED) &
            !$OMP& PRIVATE(nb, sDMPin, SDMPtype, nbTot, sDMPtypeTot, decadl, decad, decadd) 
            do i=1,blocksize
               nb=0.
               sDMPin=0.
               sDMPtype=0.
               nbTot=0.
               sDMPtypeTot=0.
               do decadl=decaddebp,decadfinp
                  if (decadl.gt.36) then
                     decad=decadl-36
                  else
                     decad=decadl
                  endif
                  decadd=decadl+(year-1)*36
                  if (DMPtype(i,decad).ne.-9999.) then
                     sDMPtypeTot=sDMPtypeTot+DMPtype(i,decad)
                     nbTot=nbTot+1.
                     if (DMPin(i,decadd).ne.-9999.) then
                        sDMPin=sDMPin+DMPin(i,decadd)
                        sDMPtype=sDMPtype+DMPtype(i,decad)
                        nb=nb+1.
                     endif
                  endif                  
               enddo
               
               if (nb.gt.0.) then
                  if (coefDMP*sDMPtypeTot.le.sDMPmin) then
                     if (sDMPtypeTot.eq.0.) then
                        AnomSigma(i)=-9998.
                     else
                        AnomSigma(i)=-9996.
                     endif
                  else
                     if (year.ne.nbyear) then
                        if (SigmaBlk(i).gt.0.) then
                           AnomSigma(i)=coefDMP*(sDMPin-sDMPtype)/SigmaBlk(i)                     
                        else
                           AnomSigma(i)=-9998.
                        endif
                     else
                        if (SigmaBlkLastp(i).gt.0) then
                           AnomSigma(i)=coefDMP*(sDMPin-sDMPtype)/SigmaBlkLastp(i)
                        else
                           AnomSigma(i)=-9998.
                        endif                        
                        if ((SDMPTypeTot.gt.0.).and.(sDMPtype/sDMPtypeTot.lt.SeuilCumulLastYear)) then
                           AnomSigma(i)=-9997.
                        endif
                     endif
                  endif
               endif
            enddo
            !$OMP END PARALLEL DO

			!$OMP CRITICAL
            outBlock32R(:)=AnomSigma(:)
            filename='Data\Raw\Biomass\Anomaly\AnomalySigma_'//numer4(years(year))//'.img'
            open(10,file=filename,access='direct',recl=4*blocksize)
            write(10,rec=j) outBlock32R
            if (FlushDisk.eq.1) CALL FLUSH(10)
            close(10)
            !$OMP END CRITICAL 
         enddo

		 

! Calcul AnVI (sauf année en cours)
!         do year=1,nbyear-1
         do year=1,nbyear
            !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED) PRIVATE(nbback, sumback, yearback, VIfact) 
            do i=1,blocksize
               nbback=0.
               sumback=0.
               do yearback=year,1,-1
                  if (BiomassBlk(i,yearback).ge.0.) then
                     VIfact=(1.-VIalpha/100.)**(year-yearback)
                     nbback=nbback+VIfact
                     sumback=sumback+VIfact*BiomassBlk(i,yearback)
                  endif
               enddo
               if (nbback.ne.0.) then
                  if (sumback.ne.0.) then
                     AnVI(i,year)=BiomassBlk(i,year)*nbback/sumback-1.
                     if (AnVI(i,year).lt.-1.) then
                        AnVI(i,year)=-1.
                     endif
                     if (AnVI(i,year).gt.1.) then
                        AnVI(i,year)=1.
                     endif                     
                  else
                     AnVI(i,year)=-9998.
                  endif
               else
                  AnVI(i,year)=-9999.
               endif
            enddo
            !$OMP END PARALLEL DO
!            filename='Data\Raw\Biomass\VI\AnVI_'//numer4(years(year))//'.img'        
!            open(10,file=filename,access='direct',recl=4*blocksize)
!            write(10,rec=j) AnVI(:,year) 
!            close(10)             
         enddo            

         !$OMP CRITICAL
         ! write(*,*) 'Write start : ',ThreadID
         do year=1,nbyear
            !$OMP PARALLEL DO NUM_THREADS(nbThread2) DEFAULT(SHARED) PRIVATE(sumback, nbback, VIfact) 
            do i=1,blocksize
               sumback=0.
               nbback=0.
               do yearback=year,1,-1
                  if (AnVI(i,yearback).gt.-9998.) then
                     VIfact=(1.-VIalpha/100.)**(year-yearback)
                     sumback=sumback+VIFact*AnVI(i,yearback)
                     nbback=nbback+VIfact
                  endif
               enddo
               if (nbback.ne.0.) then
                  VI(i)=sumback/nbback
               else
                  VI(i)=AnVI(i,year)
               endif
            enddo
            !$OMP END PARALLEL DO
            outBlock32R(:)=VI(:)
            filename='Data\Raw\Biomass\VI\VI_'//numer4(years(year))//'.img' 
            open(10,file=filename,access='direct',recl=4*blocksize)
            write(10,rec=j) outBlock32R
            if (FlushDisk.eq.1) CALL FLUSH(10)
            close(10)
         enddo
         ! write(*,*) 'Write end : ',ThreadID
         !$OMP END CRITICAL        
         
      enddo          
      !$OMP END PARALLEL DO
      
! Desallocation
      if (allocated(DMPin)) deallocate(DMPin)
      if (allocated(NDVin)) deallocate(NDVin)            
      if (allocated(out16)) deallocate(out16)
      if (allocated(out8)) deallocate(out8)               
      if (allocated(outBlock32R)) deallocate(outBlock32R)            
      if (allocated(PondDMPuse)) deallocate(PondDMPuse)
      if (allocated(PondDMPacc)) deallocate(PondDMPacc)      
      if (allocated(PondDMPForage)) deallocate(PondDMPForage)               
      if (allocated(NDVtype)) deallocate(NDVtype)
      if (allocated(DMPtype)) deallocate(DMPtype)            
      if (allocated(sumDMPType)) deallocate(sumDMPType)
      if (allocated(sumDMP)) deallocate(sumDMP)            
      if (allocated(BiomassBlk)) deallocate(BiomassBlk)               
      if (allocated(sumDMPlastp)) deallocate(sumDMPlastp)
      if (allocated(CumulDMP)) deallocate(CumulDMP)
      if (allocated(AnVI)) deallocate(AnVI)
      if (allocated(SigmaBlk)) deallocate(SigmaBlk)
      if (allocated(TrendBlk)) deallocate(TrendBlk)
      if (allocated(TrendBlkP)) deallocate(TrendBlkP)            
      if (allocated(AnomDMP)) deallocate(AnomDMP)
      if (allocated(AnomSigma)) deallocate(AnomSigma)
      if (allocated(SigmaBlkLastp)) deallocate(SigmaBlkLastp)
      if (allocated(VI)) deallocate(VI)            
      if (allocated(R2Blk)) deallocate(R2Blk)

      
!cccccccccccccccccccccccccccccccc
!c Ecriture des HDR et des META c
!cccccccccccccccccccccccccccccccc
      if (quiet.ne.1) then
      write(*,*)
      write(*,'(a,$)') ' Ecriture          :    '
      endif
      write(9,*) 'OK'
      write(9,'(a,$)') ' Ecriture          :'      
      
      if (OutRaw.eq.1) then       
         do decad=1,36
            filename='Data\Raw\Biomass\Type\NDVI\NDVI_'//decadnameMMDD(decad)//'.hdr'
            open(10,file=filename,access='direct',recl=sizeHDR32R)
            write(10,rec=1) HDR32R
            close(10)
         enddo

         do decad=1,nbdecadtot
            filename='Data\Raw\Biomass\Filter\NDVI\NDVI_'//decadnameYYYYMMDD(decad)//'.hdr'
            open(10,file=filename,access='direct',recl=sizeHDR32R)
            write(10,rec=1) HDR32R
            close(10)      

            filename='Data\Raw\Biomass\Cumul\Cumul_'//decadnameYYYYMMDD(decad)//'.img'
            inquire(file=filename,exist=filexist)
            if (filexist) then
               filename='Data\Raw\Biomass\Cumul\Cumul_'//decadnameYYYYMMDD(decad)//'.hdr'
               open(10,file=filename,access='direct',recl=sizeHDR32R)
               write(10,rec=1) HDR32R
               close(10)
            endif
         enddo
      endif
      
      
      if ((OutOpt.eq.1).or.(OutRaw.eq.1)) then
         do decad=1,nbdecadtot
            filename='Data\Raw\Biomass\Filter\DMP\DMP_'//decadnameYYYYMMDD(decad)//'.hdr'
            open(10,file=filename,access='direct',recl=sizeHDR32R)
            write(10,rec=1) HDR32R
            close(10)

            filename='Data\Raw\Biomass\Ponderate\DMP_'//decadnameYYYYMMDD(decad)//'.hdr'
            open(10,file=filename,access='direct',recl=sizeHDR32R)
            write(10,rec=1) HDR32R
            close(10)
         enddo        
         do decad=1,36      
            filename='Data\Raw\'//'Biomass\Type\DMP\DMP_'//decadnameMMDD(decad)//'.hdr'
            open(10,file=filename,access='direct',recl=sizeHDR32R)
            write(10,rec=1) HDR32R
            close(10)
         enddo
      endif
      
      
      if ((testPond.eq.1).and.(PondAccFlag.eq.1)) then
         do decad=1,36
            filename='Data\Raw\Water\PondSWBType\PondSWBtype_'//decadnameMMDD(decad)//'.hdr'
            open(10,file=filename,access='direct',recl=sizeHDR32R)
            write(10,rec=1) HDR32R
            close(10)
         enddo
      endif 
      
      
      filename='Data\Raw\Biomass\Stat\Sigma.hdr'
      open(10,file=filename,access='direct',recl=sizeHDR32R)
      write(10,rec=1) HDR32R
      close(10)

      filename='Data\Raw\Biomass\Stat\Trend.hdr'
      open(10,file=filename,access='direct',recl=sizeHDR32R)
      write(10,rec=1) HDR32R
      close(10)
     
      filename='Data\Raw\Biomass\Stat\TrendPercent.hdr'
      open(10,file=filename,access='direct',recl=sizeHDR32R)
      write(10,rec=1) HDR32R
      close(10)

      filename='Data\Raw\Biomass\Stat\TrendR2.hdr'
      open(10,file=filename,access='direct',recl=sizeHDR32R)
      write(10,rec=1) HDR32R
      close(10)
      
      filename='Data\Raw\Biomass\Biomass\Biomass_Mean.hdr'  
      open(10,file=filename,access='direct',recl=sizeHDR32R)
      write(10,rec=1) HDR32R
      close(10)
      
      do year=1,nbyear      
         filename='Data\Raw\Biomass\Biomass\Biomass_'//numer4(years(year))//'.hdr'        
         open(10,file=filename,access='direct',recl=sizeHDR32R)
         write(10,rec=1) HDR32R
         close(10)
         
         filename='Data\Raw\Biomass\Anomaly\Anomaly_'//numer4(years(year))//'.hdr'         
         open(10,file=filename,access='direct',recl=sizeHDR32R)
         write(10,rec=1) HDR32R
         close(10)
         
         filename='Data\Raw\Biomass\Anomaly\AnomalySigma_'//numer4(years(year))//'.hdr'         
         open(10,file=filename,access='direct',recl=sizeHDR32R)
         write(10,rec=1) HDR32R
         close(10)

         filename='Data\Raw\Biomass\VI\VI_'//numer4(years(year))//'.hdr'
         open(10,file=filename,access='direct',recl=sizeHDR32R)
         write(10,rec=1) HDR32R
         close(10)

!         filename='Data\Raw\Biomass\AnVI\AnVI_'//numer4(years(year))//'.hdr'
!         call system('copy '//filenameHDR32R//filename//
!     c   '> Lib\Cmd\OutPipe.txt')         
      enddo
      
      
      
!cccccccccccccccccccccccccccccccccccc      
!c Ecriture Fichiers Stat sur Admin c
!cccccccccccccccccccccccccccccccccccc

! Allocation mémoire    
      allocate(Admin(1:nbpixel,1:nbline),stat=allocStat)
      allocate(ImgR1(1:nbpixel,1:nbline),stat=allocStat)
      allocate(ImgR2(1:nbpixel,1:nbline),stat=allocStat)
      allocate(PixelSizeMap(1:nbpixel,1:nbline),stat=allocStat)           
      allocate(SumAdminBio(1:nbAdminalloc,0:nbyearalloc),stat=allocStat)
      allocate(SumAdminVI(1:nbAdminalloc,0:nbyearalloc),stat=allocStat)     
      allocate(MaskOut(1:nbpixel,1:nbline))

      
! Lecture mask si necessaire & Parametre fenetre sortie GEOTIFF
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
      allocate(outImgMaskOut32R(1:nbpixelMaskOut,1:nblineMaskOut),stat=allocStat)        
      
! Lecture PixelSizeMap
      filename='Lib\Ancillary\Img\PixelSizeMap.img'
      open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
      read(10,rec=1) PixelSizeMap
      close(10)
      
      do Adm=0,nbAdmin
         SumAdminBio(:,:)=0.
         sumAdminVI(:,:)=0.
         filename=filenameAdmin(Adm)
         open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
         read(10,rec=1) Admin
         close(10)
            
!c Ouverture fichier sortie
         if (Adm.le.2) then
            filename1='Output\Biomass\Report\Biomass_ADM_'//numer1(Adm)//'.csv'
            filename2='Output\Biomass\Report\VI_ADM_'//numer1(Adm)//'.csv'
            filename3='Lib\Ancillary\Img\ADM_'//numer1(Adm)//'.txt'
         else
            filename1='Output\Biomass\Report\Biomass_GEO_'//numer1(Adm)//'.csv'
            filename2='Output\Biomass\Report\VI_GEO_'//numer1(Adm)//'.csv'
            filename3='Lib\Ancillary\Img\GEO_'//numer1(Adm)//'.txt'     
         endif
         open(11,file=filename1)
         write(11,*) ';Source:;',filename,';'
         open(12,file=filename2)
         write(12,*) ';Source:;',filename,';'

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
         write(11,*) ';Biomass_Product_unit:;[tons];'
         write(11,*) ';'
         write(12,*) ';Nb_entities:;',nbID,';'
         write(12,*) ';Vulnerability_Index:;[No Unit];'
         write(12,*) ';'      
            
! Calcul Biomass & VI
         do year=0,nbyear
            percent=nint(100.*(year+Adm*nbyear)/(1.*(nbyear*(nbAdmin+1.))))
            if (quiet.ne.1) then
            if (Profil.ne.1) then
               write(*,'(a,I3.3,a,a,$)') back3,percent,' % (1/2)',back8
            else
               write(*,'(a,I3.3,a,a,$)') back3,percent,' % (1/3)',back8
            endif
            endif
            if (year.eq.0) then
               ext='Mean'
            else
               ext=numer4(years(year))
            endif
            filename='Data\Raw\Biomass\Biomass\Biomass_'//ext//'.img'
            open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
            read(10,rec=1) outImg32R
            close(10)
            ImgR1(:,:)=outImg32R(:,:)
            
            if (year.ne.0) then
               filename='Data\Raw\Biomass\VI\VI_'//ext//'.img'
               open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
               read(10,rec=1) outImg32R
               close(10)
               ImgR2(:,:)=outImg32R(:,:)
            else
               ImgR2(:,:)=-9999.
            endif           
            
            !$OMP PARALLEL DO NUM_THREADS(nbThread3) DEFAULT(SHARED) PRIVATE(surfBio, surfVI, i, j)
            do loop=1,nbID
               surfBio=0.
               surfVI=0.
               do i=AdminIDimin(loop),AdminIDimax(loop)
                  do j=AdminIDjmin(loop),AdminIDjmax(loop)
                     if ((AdminID(loop).eq.Admin(i,j)).and.(ImgR1(i,j).ge.0.)) then
                        sumAdminBio(loop,year)=sumAdminBio(loop,year)+0.001*ImgR1(i,j)*PixelSizeMap(i,j)
                        surfBio=surfBio+PixelSizeMap(i,j)
                     endif
                     if ((AdminID(loop).eq.Admin(i,j)).and.(abs(ImgR2(i,j)).le.1.)) then
                        sumAdminVI(loop,year)=sumAdminVI(loop,year)+ImgR2(i,j)*PixelSizeMap(i,j)
                        surfVI=surfVI+PixelSizeMap(i,j)
                     endif
                  enddo
               enddo
               if (surfBio.gt.0.) then
                  sumAdminBIO(loop,year)=sumAdminBIO(loop,year)
!                  sumAdminBIO(loop,year)=sumAdminBIO(loop,year)/surfBio
               else
                  sumAdminBIO(loop,year)=-9999.0
               endif
               if (surfVI.gt.0.) then   
                  sumAdminVI(loop,year)=sumAdminVI(loop,year)/surfVI
               else
                  sumAdminVI(loop,year)=-9999.
               endif
            enddo
            !$OMP END PARALLEL DO
         enddo
         
         write(11,'(a,$)') ';NAME;IDBIOHYDRO;AREA[sqkm];MEAN;'
         do year=1,nbyear
            write(11,'(a,$)') 'BIO_'//numer4(years(year))//';'
         enddo
         write(11,*)
         do loop=1,nbID
            write(11,'(a,a,a,I6,a,F10.0,a,$)')';',AdminIDname(loop),';',AdminID(loop),';',AdminIDsurf(loop),';'
            do year=0,nbyear
               write(11,'(F18.3,a,$)') sumAdminBio(loop,year),';'
            enddo
            write(11,*)
         enddo
         close(11)
         
         write(12,'(a,$)') ';NAME;IDBIOHYDRO;AREA[sqkm];'
         do year=1,nbyear
            write(12,'(a,$)') 'VI_'//numer4(years(year))//';'
         enddo
         write(12,*)
         do loop=1,nbID
            write(12,'(a,a,a,I6,a,F10.0,a,$)') ';',AdminIDname(loop),';',AdminID(loop),';',AdminIDsurf(loop),';'
            do year=1,nbyear
               write(12,'(F18.3,a,$)') sumAdminVI(loop,year),';'
            enddo
            write(12,*)
         enddo
         close(12)            
      enddo       

      
!cccccccccccccccccccccccccccccccccccc
!c Relecture et re-écriture geotiff c
!cccccccccccccccccccccccccccccccccccc

      nbtiff=1
      filetiffin(nbtiff)='Data\Raw\Biomass\Stat\Sigma.img'
      filetiffou(nbtiff)='Output\Biomass\Stat\Sigma.tif'
      filetiffcl(nbtiff)='Data\Raw\Biomass\Stat\Sigma.*'
      typetiff(nbtiff)=1
      filttiff(nbtiff)=1
      
      nbtiff=nbtiff+1
      filetiffin(nbtiff)='Data\Raw\Biomass\Stat\Trend.img'
      filetiffou(nbtiff)='Output\Biomass\Stat\Trend.tif'
      filetiffcl(nbtiff)='Data\Raw\Biomass\Stat\Trend.*'
      typetiff(nbtiff)=1
      filttiff(nbtiff)=1 
      
      nbtiff=nbtiff+1
      filetiffin(nbtiff)='Data\Raw\Biomass\Stat\TrendPercent.img'
      filetiffou(nbtiff)='Output\Biomass\Stat\TrendPercent.tif'
      filetiffcl(nbtiff)='Data\Raw\Biomass\Stat\TrendPercent.*'
      typetiff(nbtiff)=1
      filttiff(nbtiff)=1 

      nbtiff=nbtiff+1
      filetiffin(nbtiff)='Data\Raw\Biomass\Stat\TrendR2.img'
      filetiffou(nbtiff)='Output\Biomass\Stat\TrendR2.tif'
      filetiffcl(nbtiff)='Data\Raw\Biomass\Stat\TrendR2.*'
      typetiff(nbtiff)=1
      filttiff(nbtiff)=1

      nbtiff=nbtiff+1
      filetiffin(nbtiff)='Data\Raw\Biomass\Biomass\Biomass_Mean.img'
      filetiffou(nbtiff)='Output\Biomass\Biomass\Biomass_Mean.tif' 
      filetiffcl(nbtiff)='Data\Raw\Biomass\Biomass\Biomass_Mean.*'
      typetiff(nbtiff)=2
      filttiff(nbtiff)=2       
      
      do year=1,nbyear
         nbtiff=nbtiff+1
         filetiffin(nbtiff)='Data\Raw\Biomass\Biomass\Biomass_'//numer4(years(year))//'.img'
         filetiffou(nbtiff)='Output\Biomass\Biomass\Biomass_'//numer4(years(year))//'.tif'
         filetiffcl(nbtiff)='Data\Raw\Biomass\Biomass\Biomass_'//numer4(years(year))//'.*'      
         typetiff(nbtiff)=2
         filttiff(nbtiff)=2         

         nbtiff=nbtiff+1
         filetiffin(nbtiff)='Data\Raw\Biomass\Anomaly\Anomaly_'//numer4(years(year))//'.img'
         filetiffou(nbtiff)='Output\Biomass\Anomaly\Anomaly_'//numer4(years(year))//'.tif'
         filetiffcl(nbtiff)='Data\Raw\Biomass\Anomaly\Anomaly_'//numer4(years(year))//'.*'     
         typetiff(nbtiff)=3
         filttiff(nbtiff)=3
         
         nbtiff=nbtiff+1
         filetiffin(nbtiff)='Data\Raw\Biomass\Anomaly\AnomalySigma_'//numer4(years(year))//'.img'
         filetiffou(nbtiff)='Output\Biomass\Anomaly\AnomalySigma_'//numer4(years(year))//'.tif'
         filetiffcl(nbtiff)='Data\Raw\Biomass\Anomaly\AnomalySigma_'//numer4(years(year))//'.*'
         typetiff(nbtiff)=4
         filttiff(nbtiff)=3 

         nbtiff=nbtiff+1
         filetiffin(nbtiff)='Data\Raw\Biomass\VI\VI_'//numer4(years(year))//'.img'
         filetiffou(nbtiff)='Output\Biomass\VI\VI_'//numer4(years(year))//'.tif'
         filetiffcl(nbtiff)='Data\Raw\Biomass\VI\VI_'//numer4(years(year))//'.*'     
         typetiff(nbtiff)=5
         filttiff(nbtiff)=3        
      enddo
      
! Init Filtre Spatiale      
      if (flagFiltre.ne.0) then
         do jb=-sizeFiltre,sizeFiltre
            is(jb)=nint((sizeFiltre**2-jb**2)**0.5)
         enddo
      endif

      do tiffout=1,nbtiff
         percent=nint(100.*(tiffout-1.)/(1.*(nbtiff-1.)))
         if (quiet.ne.1) then
         if (Profil.ne.1) then
            write(*,'(a,I3.3,a,a,$)') back3,percent,' % (2/2)',back8
         else
            write(*,'(a,I3.3,a,a,$)') back3,percent,' % (2/3)',back8
         endif
         endif
         
      
         filename=filetiffin(tiffout)
         open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
         read(10,rec=1) outImg32R
         close(10)
         ImgR1(:,:)=outImg32R(:,:)
         
         
! Filtre Spatiale
         if (flagFiltre.ne.0) then
            ImgR2(:,:)=ImgR1(:,:)
            do loop=1,nbLoopFiltre
               do i=1+sizeFiltre,nbpixel-sizeFiltre
                  !$OMP PARALLEL DO NUM_THREADS(nbThread3) DEFAULT(SHARED) PRIVATE(moy, nb, jb, ib)
                  do j=1+sizeFiltre,nbline-sizeFiltre
                     if (ImgR1(i,j).gt.-9990.) then
                        moy=0.
                        nb=0.
                        do jb=-sizeFiltre,sizeFiltre
                           do ib=-is(jb),is(jb)
                              if (ImgR1(i+ib,j+jb).gt.-9990.) then
                                 moy=moy+ImgR1(i+ib,j+jb)
                                 nb=nb+1.
                              endif
                           enddo
                        enddo
                        ImgR2(i,j)=moy/nb   
                     else
                        ImgR2(i,j)=ImgR1(i,j)
                     endif
                  enddo
                  !$OMP END PARALLEL DO
               enddo
               ImgR1(:,:)=ImgR2(:,:)                   
            enddo
         endif  
     
         
! Limitation valeurs
! Anomaly %
         if (Typetiff(tiffout).eq.3) then
            do i=1,nbpixel
               !$OMP PARALLEL DO NUM_THREADS(nbThread3) DEFAULT(SHARED) PRIVATE(moy, nb, jb, ib)
               do j=1,nbline
                  if (ImgR1(i,j).gt.200.) then
                     ImgR1(i,j)=200.
                  endif
                  if ((ImgR1(i,j).lt.0.).and.(ImgR1(i,j).gt.-9990.)) then
                     ImgR1(i,j)=0.
                  endif
               enddo
               !$OMP END PARALLEL DO
            enddo
         endif 
         
         
! Anomaly Sigma
         if (Typetiff(tiffout).eq.4) then
            do i=1,nbpixel
               !$OMP PARALLEL DO NUM_THREADS(nbThread3) DEFAULT(SHARED) PRIVATE(moy, nb, jb, ib)
               do j=1,nbline
                  if (ImgR1(i,j).gt.10.) then
                     ImgR1(i,j)=10.
                  endif
                  if ((ImgR1(i,j).lt.-10.).and.(ImgR1(i,j).gt.-9990.)) then
                     ImgR1(i,j)=-10.
                  endif
               enddo
               !$OMP END PARALLEL DO
            enddo
         endif     
        
         
! Ecriture Tiff      
!         do i=1,nbpixel
!            do j=1,nbline
!               buff32R=outImgR(i,j)
!               do o=1,4
!                  poso=o+(i-1)*4+(j-1)*nbpixel*4
!                  Tiff32R(poso+offTiff32R)=buff8R(o)
!               enddo
!            enddo
!         enddo     
!         filename='Lib\Tmp\Tiff.tif'
!         open(10,file=filename,access='direct',recl=sizeTiff32R)
!         write(10,rec=1) Tiff32R
!         close(10)
         
         outImg32R(:,:)=ImgR1(:,:)

! Decoupage suivant le masque         
! Application du mask si necessaire
         do i=iminMaskOut,imaxMaskOut
            do j=jminMaskOut,jmaxMaskOut
               if ((Masking.eq.1).and.(MaskOut(i,j).eq.0)) then
                  outImgMaskOut32R(i-iminMaskOut+1,j-jminMaskOut+1)=-9995.
               else
                  outImgMaskOut32R(i-iminMaskOut+1,j-jminMaskOut+1)=outImg32R(i,j)
               endif
            enddo
         enddo
  
         
! Sauvegarde         
         filename='Lib\Tmp\Img.img'
         open(10,file=filename,access='direct',recl=4*nbpixelMaskOut*nblineMaskOut)
         write(10,rec=1) outImgMaskOut32R
         if (FlushDisk.eq.1) CALL FLUSH(10)
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
      
! Ecriture fichier ancien parametres pour optimisation
      filename='Lib\Param_Old\BioGenerator_Param_Old.txt'
      open(10,file=filename)
      write(10,*) nbdecadtot-decaldecad,yearlast,monthlast,daylast,year1,month1,day1
      write(10,*) PondUseFlag,F0,F1       
      write(10,*) PondAccFlag,RWater,P0,P1,ForageFlag
      write(10,*) OutOpt,Quiet,Profil      
      close(10)
      
      
! Desallocation memoire
      if (allocated(Admin)) deallocate(Admin)
      if (allocated(ImgR1)) deallocate(ImgR1)
      if (allocated(ImgR2)) deallocate(ImgR2)
      if (allocated(PixelSizeMap)) deallocate(PixelSizeMap)
      if (allocated(outImgMaskOut32R)) deallocate(outImgMaskOut32R)  
      if (allocated(SumAdminBio)) deallocate(SumAdminBio)
      if (allocated(SumAdminVI)) deallocate(SumAdminVI) 
      if (allocated(outImg32R)) deallocate(outImg32R)
      if (allocated(MaskOut)) deallocate(MaskOut)
      
! Appel fonction matlab pour ecriture Shapefile
      filename='Lib\Bin\BioGenerator_csv2shp.exe'
      call SYSTEM(filename)
        
! Fichier flag
      flagOK=1
      filename='Lib\Tmp\BioGenerator_Flag.txt'
      open(10,file=filename)
      write(10,*) flagOK
      close(10)
      
      write(9,*) 'OK' 
      
 

9999  call date_and_time(VALUES=time2)
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
      
      write(9,*) 'Temps execution   : '//trim(adjustl(output1))//' h '//trim(adjustl(output2))//' mm'
      write(9,*)
      write(9,*) 'Operations terminees.'
      close(9)
      
      filename1='Lib\tmp\BioGenerator_Report.txt'
      filename2='Output\Biomass\Report\'
      call system('copy /Y '//filename1//filename2//'> Lib\Cmd\OutPipe.txt')
      


      if ((quiet.ne.1).and.(autorun.ne.1)) read(*,*)
end program