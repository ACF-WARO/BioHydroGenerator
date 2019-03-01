      program BioGenerator

ccccccccccccccccccccccccccccccccccccccccccccccc
c Erwann Fillol, Action Contre la Faim (2017) c
ccccccccccccccccccccccccccccccccccccccccccccccc
      
      implicit none
      


      integer*4 nblineblock,nbdecadmax,nbblock,
     c          year1,month1,decad1,decaldecad,
     c          yearStop,monthStop,decadStop,
     c          FiltreSizeSigma,nbyearmax,blocksize,
     c          FiltreSize,
     c          DMPMaxJumpFilter,LastStop,
     c          nbpixel,nbline,nbtiffoutmax,
c     c          sizeTiff16S,offTiff16S,
     c          sizeTiff32R,offTiff32R,
c     c          sizehdr32R,
     c          AdmLevelMax, 
     c          sizeBuffMax,factorSizeMNT,
     c          nbAdminmax
     
      real*4    SeuilNDV,SigmaNDV,CoefDMP,
     c          SeuilDMPlow,SeuilNBlow,DMPMaxJump,
     c          pi,e,dMax,
     c          Lat0,Lon0,PixelSD,
     c          convMNT,PixelSKm

      
c Definition fenetre
      parameter (nbpixel=7841)
      parameter (nbline=3630)
      
c Parameters Geo
      parameter (Lat0=27.379464)
      parameter (Lon0=-18.004464)
      parameter (PixelSD=0.00892857)
      parameter (PixelSKm=0.9728653504)      
      
c Definition parametres fichiers sorties
c      parameter (sizeTiff16S=56955085)      
c      parameter (offTiff16S=14528)
      parameter (sizeTiff32R=113880745)      
      parameter (offTiff32R=14528)      
c      parameter (sizehdr32R=334)

c Parametres de definition allocation memoire      
c      parameter (nblineblock=11)
      parameter (nblineblock=11)
      parameter (nbyearmax=25)
c      parameter (nbyearmax=17)     
      parameter (nbdecadmax=36*nbyearmax)
      parameter (blocksize=nblineblock*nbpixel)
      parameter (nbblock=nbline/nblineblock)
      parameter (nbtiffoutmax=nbyearmax*4+4)
      parameter (nbAdminmax=50000)

c Parametres Decoupes Admin
      parameter (AdmLevelMax=9)
      
c Parametres Water Access
      parameter (sizeBuffMax=200)
      parameter (factorSizeMNT=5)
      parameter (dMax=90.0)
      parameter (convMNT=1./3.)

c Parametres maths
      parameter (e=2.718281828)
      parameter (pi=3.141592654)

c Parametre Transformation DMP en Biomasse
      parameter (coefDMP=365.25/36)

c Parametres de la 1ere decade
      parameter (year1=1998)
      parameter (month1=4)
      parameter (decad1=1)
      parameter (decaldecad=9)
      
c Parametres de la Dernière decade (LastStop : 1=oui, 0=non)
      parameter (LastStop=0)
      parameter (yearStop=2000)
      parameter (monthStop=09)
      parameter (decadStop=21)

c Parametres Filtrage temporel
      parameter (SeuilNDV=0.28)
      parameter (FiltreSize=4)
      parameter (SigmaNDV=3.0)
      parameter (FiltreSizeSigma=4)
      parameter (SeuilDMPlow=20)
      parameter (SeuilNBlow=5)
      parameter (DMPMaxJumpFilter=1)      
      parameter (DMPMaxJump=50)
      


      integer*1     buff8S(2),buff8R(4),
     c              out8(blocksize),iError,
c     c              Tiff16S(sizeTiff16S),
     c              Tiff32R(sizeTiff32R),
c     c              hdr32R(sizehdr32R),
     
c     c    slope(nbpixel*factorSizeMNT,nbline*factorSizeMNT),
     c              Forage(nbpixel,nbline),
     c              outImg8(nbpixel,nbline)
     

      integer*2     out16(blocksize),buff16S

      integer*4     i,j,year,month,decad,FE,poso,o,posd,
     c              nbdecadtot,decadyear(nbdecadmax),
     c              yearlast,monthlast,decadlast,
     c              decadmonth(nbdecadmax),decaddecad(nbdecadmax),
     c              decadm,dd,decadd,decadl,decaddeb,loop,
     c              ib,jb,yearback,
     c              decadCumul(36),buff32S, 
     c              nbTot,years(nbyearmax),nbyear,yeart,percent,
     c              flagFiltre,nbLoopFiltre,sizeFiltre,warn,decadback,
     c              decaddebp,decadfinp,nbDecadSomme,decadlastp,
     c              PondUseFlag,nbtiff,tiffout,
     c              typetiff(nbtiffoutmax),filttiff(nbtiffoutmax),

     c              time1(8),time2(8),
     
     c              OutRaw,KeepOld,
     
     c              is(-sizeBuffmax:sizeBuffmax),
     c              DecadPond(nbdecadmax),
c     c              iSlope,jSlope,nbpas,pas,sl,
     c              nbpond(blocksize),
     c              PondAccFlag,sizeBuff,ForageFlag,ForageFlagOld,
     c              nbdecadpond,test1,test2,testPond,testForage,
     c              sizeBuffOld,nbForage,SWB,
     c              nbdecadSWB,Adm,nbID,flag,nbmissSWB,
     c              nbfullYearSWB,nbdecadyearfi,yearfi,
     
     c              Admin(nbpixel,nbline),AdminID(nbAdminmax),nbAdmin,
     c              AdminIDimin(nbAdminmax),AdminIDimax(nbAdminmax),
     c              AdminIDjmin(nbAdminmax),AdminIDjmax(nbAdminmax),
     c              IDi,DegOpt,OutOpt,
     c              nbdecadtotOld,yearlastOld,monthlastOld,decadlastOld,
     c              PondUseFlagOld,PondAccFlagOld,ForageFlagOld2   

      real*4        DMPin(blocksize,nbdecadmax),
     c              NDVin(blocksize,nbdecadmax),
     
     c              CumulDMP(blocksize,36),
     c              NDVtype(blocksize,36),DMPtype(blocksize,36),   
     c              BiomassBlk(blocksize,nbyearmax),
     
     
     c              SigmaBlk(blocksize),TrendBlk(blocksize),
     c              TrendBlkP(blocksize),
     c              PondDMPuse(blocksize),PondDMPacc(blocksize),
     c              PondDMPForage(blocksize),     
     c              R2Blk(blocksize),buff32R,     
     c              outR(blocksize),DiffNDV,
     c              F1,F0,sDMPmin,SeuilCumulLastYear,
     c              NDV1,NDV2,DMP1,DMP2,Andv,Bndv,Admp,Bdmp,
     c              aNDVi,aNDVm,sumDMPType(blocksize),
     c              sumDMP(blocksize),AnomDMP(blocksize),
     c              AnomSigma(blocksize),
     c              SigmaBlkLastp(blocksize),
     c              sDMPtype,sDMPtypeTot,sDMPin,
     c              outImgR(nbpixel,nbline),
     c              outImgRs(nbpixel,nbline),
     c              SigmaR(nbpixel,nbline),
     c              AnomalyR(nbpixel,nbline),
     c              BioMeanR(nbpixel,nbline),     
     c              StackNDV(nbyearmax),StackDMP(nbyearmax),
     c              NDVTypeF(36),DMPTypeF(36),
     c              VIalpha,VIfact,VI(blocksize),
     c              sumback,nbback,sumBnDenVI,sumBnNumVI,
     c              AnVI(blocksize,nbyearmax),
     c              sumDMPlastp(blocksize,nbyearmax),
     
c     c              dslope(256),dtot,fMNT,pasi,pasj,
     c              FBackGround(nbpixel,nbline),
     c              PixelSizeMap(nbpixel,nbline),
     c              PondForage(nbpixel,nbline),
     c              Pond(nbpixel,nbline),
     c              Pondtype(blocksize),PBack,P,
     c              f,P0,P1,factan,d,RWater,
     c              eS(-sizeBuffmax:sizeBuffmax,
     c                 -sizeBuffmax:sizeBuffmax),
     c              fOld,P0Old,P1Old,factanOld,Lon,Lat,
     
     c              AdminIDsurf(nbAdminmax),
     c              surfBio,surfVI,
     
     c              F0Old,F1Old,RWaterOld2,P0Old2,P1Old2,
     
     c              diffH
     
      real*8        Moy,Sigma,Somme,nb,
     c              S2X,S2Y,SXY,sumX,sumY,      
     c              SumAdminBio(nbAdminmax,0:nbyearmax),
     c              SumAdminVI(nbAdminmax,0:nbyearmax)
     
      character*100 filename,filename1,filename2,filename3,
     c              filenameHDR32R,
     c              filetiffin(nbtiffoutmax),
     c              filetiffou(nbtiffoutmax),
     c              filetiffcl(nbtiffoutmax),
     c              filenameAdmin(0:AdmLevelMax),
     c              AdminIDname(nbAdminmax)
      character*3   back3
      character*2   back2
      character*1   back1
      character*4   numer4(0:9999),ext,decadnamep(72),
     c              decadnameMMDD(36)
      character*2   numer2(0:99)
      character*1   numer1(0:9)
      character*8   decadname,decadnameYYYYMMDD(nbdecadmax),
     c              decadwarn(100),decadnameold,back8
      logical       filexist
      equivalence   (buff8S,buff16S),
     c              (buff8R,buff32R),
     c              (SigmaR,outImgR),
     c              (AnomalyR,outImgRs),
     c              (BioMeanR,outImgRs),
     c              (FBackGround,PixelSizeMap)
     
     
     
      call date_and_time(VALUES=time1)
     
      filename='Lib/tmp/BioGenerator_Report.txt'
      open(9,file=filename)
     
      call system('cls')
      write(*,*) '*********************************'
      write(*,*) '*     BioGenerator 2 (v4.0)     *'
      write(*,*) '* Action Contre la Faim (ACF-E) *'
      write(*,*) '*        Erwann Fillol (2017)   *'
      write(*,*) '*        erwann.fillol@gmail.com*'
      write(*,*) '*********************************'
      write(*,*)     
      write(*,'(a,$)') ' Initialisation   '

      write(9,*) '*********************************'
      write(9,*) '*     BioGenerator 2 (v4.0)     *'
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

      do i=0,99
         numer2(i)=
     c   char(int(i/10)-10*int(i/100)+48)//
     c   char(int(i/1)-10*int(i/10)+48)
      enddo
      
      do i=0,9
         numer1(i)=
     c   char(int(i/1)-10*int(i/10)+48)
      enddo      
      
      factan=-9999.
      buff16S=0
      buff32R=0.
      
c Lecture Fichiers Auxillaires
      filenameHDR32R='Lib\Ancillary\HDR\32R.hdr'
c      filename='Lib/Ancillary/HDR/32R.hdr'
c      open(10,file=filename,access='direct',status='old',
c     crecl=sizehdr32R)
c      read(10,rec=1) hdr32R
c      close(10)
      
c      filename='Lib/Tiff/SuSa16S.tif'
c      open(10,file=filename,access='direct',
c     crecl=sizeTiff16S)
c      read(10,rec=1) Tiff16S
c      close(10)
      
      filename='Lib/Ancillary/Tiff/SuSa32R.tif'
      open(10,file=filename,access='direct',
     crecl=sizeTiff32R)
      read(10,rec=1) Tiff32R
      close(10)      
      

      back8=char(8)//char(8)//char(8)//char(8)//
     c      char(8)//char(8)//char(8)//char(8)
      back3=char(8)//char(8)//char(8)
      back2=char(8)//char(8)
      back1=char(8)

c Lecture fichier parametrage
      filename='Param/BioGenerator_Param.txt'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         open(10,file=filename,status='old',
     c   form='formatted',err=98,iostat=iError)
         read(10,*)
c Lissage
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
c Seuil Production Filtrage Anomalie
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
c Parametre Usable
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
c Parametre Indice Vulnerabilite
         read(10,*) VIalpha
         if (VIalpha.lt.0.) then
            VIalpha=0.
         endif
         if (VIalpha.gt.100.) then
            VIalpha=100.
         endif      
c Parametre Accessible
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
c Parametre Sorties
         read(10,*) OutOpt
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
      


      
      
c Recherche fichier Admin et verification ouverture fichiers sortie
      nbAdmin=-1
      do Adm=0,AdmLevelMax
         if (Adm.le.2) then
            filenameAdmin(Adm)='Lib/Ancillary/Img/ADM_'//
     c      numer1(Adm)//'.img'
         else
            filenameAdmin(Adm)='Lib/Ancillary/Img/GEO_'//
     c      numer1(Adm)//'.img'
         endif
         inquire(file=filenameAdmin(Adm),exist=filexist)
         if (filexist) then
            nbAdmin=nbAdmin+1
         endif
      enddo
      do Adm=0,nbAdmin
         if (Adm.le.2) then
            filename='Data/Out/Biomass/Report/Biomass_ADM_'//
     c      numer1(Adm)//'.csv'
         else
            filename='Data/Out/Biomass/Report/Biomass_GEO_'//
     c      numer1(Adm)//'.csv'
         endif
         open(10,file=filename,form='formatted',err=99,iostat=iError)        
         write(10,*) 'Test'
         close(10)
         if (Adm.le.2) then
            filename='Data/Out/Biomass/Report/VI_ADM_'//
     c      numer1(Adm)//'.csv'
         else
            filename='Data/Out/Biomass/Report/VI_GEO_'//
     c      numer1(Adm)//'.csv'
         endif
         open(10,file=filename,form='formatted',err=99,iostat=iError)        
         write(10,*) 'Test'
         close(10)         
99       if (iError.ne.0) then
            write(*,*) ': Erreur'
            write(*,*) 'Fichier ouvert    : ',filename
            write(*,*) 'Veuillez Fermer et Relancer.'
            
            write(9,*) ': Erreur'
            write(9,*) 'Fichier ouvert    : ',filename
            write(9,*) 'Veuillez Fermer et Relancer.'
            goto 9999
         endif
      enddo    
      
c Effacage anciens fichiers
      if (KeepOld.ne.1) then
         call system('del /Q /S Data\Out\Biomass\* > '// 
     c   'Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Out\Raw\Biomass\Anomaly\* > '//
     c   'Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Out\Raw\Biomass\Biomass\* > '//
     c   'Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Out\Raw\Biomass\Cumul\* > '//
     c   'Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Out\Raw\Biomass\Filter\NDVI\* > '//
     c   'Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Out\Raw\Biomass\Type\NDVI\* > '//
     c   'Lib\Cmd\OutPipe.txt')     
         call system('del /Q /S Data\Out\Raw\Biomass\Stat\* > '//
     c   'Lib\Cmd\OutPipe.txt')
         call system('del /Q /S Data\Out\Raw\Biomass\VI\* > '//
     c   'Lib\Cmd\OutPipe.txt')
         if (OutOpt.ne.1) then
            call system(
     c      'del /Q /S Data\Out\Raw\Biomass\Filter\DMP\* > '//
     c      'Lib\Cmd\OutPipe.txt')
            call system(
     c      'del /Q /S Data\Out\Raw\Biomass\Type\DMP\* > '//
     c      'Lib\Cmd\OutPipe.txt')
            call system(
     c      'del /Q /S Data\Out\Raw\Biomass\Ponderate\* > '//
     c      'Lib\Cmd\OutPipe.txt')   
         endif     
      endif      

c Initialisation nom decades integration
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

      year=year1
      month=month1
      decad=decad1
      decadlastp=(month1-1)*3+decad1-1

      nbyear=1
      yeart=year
      years(1)=year
      nbdecadtot=0
      warn=0      
      
c Initialisation des decades debut et fin integration
      if (decadfinp.lt.decaddebp) then
         decadfinp=decadfinp+36
      endif
      nbDecadSomme=decadfinp-decaddebp+1     
      
c Determination du nombre de decades
10    FE=0
      decadname=numer4(year)//numer2(month)//numer2(decad)
      filename='Data/In/DMP/DMP_'//decadname//'.img'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         FE=FE+1
      endif
      filename='Data/In/NDVI/NDVI_'//decadname//'.img'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         FE=FE+1
      endif
      if (FE.eq.2) then     
         decadl=int(decad/10)+1+(month-1)*3
         if ((decadl.eq.decaddebp).and.(year.gt.years(nbyear))) then
            yeart=yeart+1
            nbyear=nbyear+1
            years(nbyear)=yeart
         endif
      
         yearlast=year
         monthlast=month
         decadlast=decad
         decadlastp=decadlastp+1
         nbdecadtot=nbdecadtot+1
         decadyear(nbdecadtot)=year
         decadmonth(nbdecadtot)=month
         decaddecad(nbdecadtot)=decad
         
         if ((year.eq.yearStop).and.(month.eq.monthStop).and.
     c  (decad.eq.decadStop).and.(LastStop.eq.1)) then
            goto 9
         endif
         
         decad=decad+10
         if (decad.eq.31) then
            decad=1
            month=month+1
            if (month.eq.13) then
               month=1
               year=year+1
               decadlastp=0
            endif
         endif

         goto 10
      else
         decadnameold=decadname
         decad=decad+10
         if (decad.eq.31) then
            decad=1
            month=month+1
            if (month.eq.13) then
               month=1
               year=year+1
               decadlastp=0
            endif
         endif
         FE=0
         decadname=numer4(year)//numer2(month)//numer2(decad)
         filename='Data/In/DMP/DMP_'//decadname//'.img'
         inquire(file=filename,exist=filexist)
         if (filexist) then
            FE=FE+1
         endif
         filename='Data/In/NDVI/NDVI_'//decadname//'.img'
         inquire(file=filename,exist=filexist)
         if (filexist) then
            FE=FE+1
         endif
         if (FE.eq.2) then
            warn=warn+1 
            decadwarn(warn)=decadnameold
            
            decadl=int(decad/10)+1+(month-1)*3
            if (decadl.eq.decaddebp) then
               yeart=yeart+1
               nbyear=nbyear+1
               years(nbyear)=yeart
            endif
            
            yearlast=year
            monthlast=month
            decadlast=decad
            decadlastp=decadlastp+1
            nbdecadtot=nbdecadtot+1
            decadyear(nbdecadtot)=year
            decadmonth(nbdecadtot)=month
            decaddecad(nbdecadtot)=decad
            goto 10
         endif
      endif
      

c Lecture anciens paramètres et determination degré optimisation
9     filename='Lib\Param_Old\BioGenerator_Param_Old.txt'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         open(10,file=filename)
         read(10,*) 
     c   nbdecadtotOld,yearlastOld,monthlastOld,decadlastOld
         read(10,*)
     c   PondUseFlagOld,F0Old,F1Old       
         read(10,*)
     c   PondAccFlagOld,RWaterOld2,P0Old2,P1Old2,ForageFlagOld2   
         close(10)
         call system('del /Q '//filename//' > '//
     c   'Lib\Cmd\OutPipe.txt')
      endif
      DegOpt=0
      if ((OutOpt.eq.1).and.(OutRaw.eq.0)) then
         if ((nbdecadtot.eq.nbdecadtotOld).and.
     c      (yearlast.eq.yearlastOld).and.
     c      (monthlast.eq.monthlastOld).and.
     c      (decadlast.eq.decadlastOld)) then
            DegOpt=1
         endif
         if ((DegOpt.eq.1).and.
     c      (PondUseFlag.eq.PondUseFlagOld).and.
     c      (F0.eq.F0Old).and.
     c      (F1.eq.F1Old).and.
     c      (PondAccFlag.eq.PondAccFlagOld).and.
     c      (RWater.eq.RWaterOld2).and.
     c      (P0.eq.P0Old2).and.
     c      (P1.eq.P1Old2).and.
     c      (ForageFlag.eq.ForageFlagOld2)) then
            DegOpt=2
         endif
      endif

      
      
c Initialisation du nom des decades pour lectures des fichiers
      decad=decad1
      month=month1-(decaldecad/3)
      year=year1
      i=1
15    decadnameYYYYMMDD(i)=
     cnumer4(year)//numer2(month)//numer2(decad)
      decad=decad+10
      if (decad.eq.31) then
         decad=1
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
      
17    decad=decad1
      month=month1-(decaldecad/3)
      do i=1,36
         decadnameMMDD(i)=
     c   numer2(month)//numer2(decad)
         decad=decad+10
         if (decad.eq.31) then
            decad=1
            month=month+1
            if (month.eq.13) then
               month=1
            endif
         endif
      enddo


      if (nbdecadtot.ne.0) then
         write(*,*) ': OK'
         write(9,*) ': OK'        
         if (warn.ne.0) then
           if (warn.eq.1) then
              write(*,'(a)') ' Decade manquante  : '//
     c        decadwarn(warn)
              write(9,'(a)') ' Decade manquante  : '//
     c        decadwarn(warn)     
           else
              write(*,'(a,$)') ' Decades manquantes: '
              write(9,'(a,$)') ' Decades manquantes: '
              do loop=1,warn-1
                 write(*,'(a,$)') decadwarn(loop)//'; '
                 write(9,'(a,$)') decadwarn(loop)//'; '
              enddo
              write(*,'(a)') decadwarn(warn)
              write(9,'(a)') decadwarn(warn)
           endif
         endif
         write(*,*)
         write(9,*)
         buff16S=0
      else
         write(*,*) ': Erreur !'
         write(*,*)
         write(*,*) 'Fichiers non-trouves !'
         write(*,*) 'Verifier la configuration'
         write(*,*) 'des fichiers et des repertoires.'
         read(*,*)
         write(9,*) ': Erreur !'
         write(9,*)
         write(9,*) 'Fichiers non-trouves !'
         write(9,*) 'Verifier la configuration'
         write(9,*) 'des fichiers et des repertoires.'         
         stop
      endif
      
      if (flagFiltre.eq.0) then
         write(*,*) 'Filtre spatial    : Nul'
         write(9,*) 'Filtre spatial    : Nul'
      else
         write(*,*) 'Filtre spatial    : '
     c   //numer2(nbLoopFiltre)
     c   //'*'
     c   //numer2(sizeFiltre)
         write(9,*) 'Filtre spatial    : '
     c   //numer2(nbLoopFiltre)
     c   //'*'
     c   //numer2(sizeFiltre)
      endif
      write(*,*) 'Integration       : '
     c   //decadnamep(decaddebp)
     c   //' > '
     c   //decadnamep(decadfinp)
      write(9,*) 'Integration       : '
     c   //decadnamep(decaddebp)
     c   //' > '
     c   //decadnamep(decadfinp)
      write(*,*) 'Seuil Production  : Min =',
     c   nint(sDMPmin),'kg/ha ; Seuil =',
     c   nint(100*SeuilCumulLastYear),back1,'%'
      write(9,*) 'Seuil Production  : Min =',
     c   nint(sDMPmin),'kg/ha ; Seuil =',
     c   nint(100*SeuilCumulLastYear),'%'     
      if (PondUseFlag.eq.0) then
         write(*,*) 'Utilisabilite     : Nul'
         write(9,*) 'Utilisabilite     : Nul'
      else
         write(*,*) 'Utilisabilite     : F0=',
     c   nint(100*F0),back1,'% ; F1=',
     c   nint(100*F1),back1,'%'
         write(9,*) 'Utilisabilite     : F0=',
     c   nint(100*F0),'% ; F1=',
     c   nint(100*F1),'%'     
      endif
      if (PondAccFlag.eq.0) then
         write(*,*) 'Accessibilite     : Nul'
         write(9,*) 'Accessibilite     : Nul'
      else
         write(*,*) 'Accessibilite     : Dmax=',
     c   nint(RWater*100.)/100.,back1,'km ; P0=',
     c   nint(100.*P0),back1,'% ; P1=',
     c   nint(100.*P1),back1,'%'
         write(9,*) 'Accessibilite     : Dmax=',
     c   nint(RWater*100.)/100.,'km ; P0=',
     c   nint(100.*P0),'% ; P1=',
     c   nint(100.*P1),'%'
      endif
      write(*,*) 'VI Vulnerabilite  :',
     c   nint(VIalpha),back1,'%'
      write(9,*) 'VI Vulnerabilite  :',
     c   nint(VIalpha),'%'

      write(*,*) 'Decades           : ',
     cdecadnameYYYYMMDD(1+decaldecad),' > ',
     cdecadnameYYYYMMDD(nbdecadtot+decaldecad)
      write(*,*) 'Nombre de decades :',nbdecadtot-warn
      write(*,*) 'Annees            : '
     c   //numer4(year1)//' > '//numer4(year1+nbyear-1)
      write(*,*)
      
      write(9,*) 'Decades           : ',
     cdecadnameYYYYMMDD(1+decaldecad),' > ',
     cdecadnameYYYYMMDD(nbdecadtot+decaldecad)
      write(9,*) 'Nombre de decades :',nbdecadtot-warn
      write(9,*) 'Annees            : '
     c   //numer4(year1)//' > '//numer4(year1+nbyear-1)
      write(9,*)
      
c      write(*,*) 'Optimisation      :',DegOpt
c      write(*,*)


c Initialisation des decades suplementaires
      DMPin(:,:)=-9999.
      NDVin(:,:)=-9999.
      
      nbdecadtot=nbdecadtot+decaldecad
      
      
     
ccccccccccccccccccc     
c Calcul Pond SWB c
ccccccccccccccccccc
      if ((PondAccFlag.eq.1).and.(DegOpt.lt.2)) then
             
         write(*,'(a,$)') 
     c   ' Access. Eau Init.'  
         write(9,'(a,$)') 
     c   ' Access. Eau Init.'    
         
c Initialisation des variables de buffering
         sizeBuff=nint(1.*RWater/PixelSkm+0.5)
         do jb=-sizeBuff,sizeBuff
            is(jb)=nint((sizeBuff**2-jb**2)**0.5)
         enddo
         f=((1.*RWater/PixelSkm)**2.)/log(100.)
         do jb=-sizeBuff,sizeBuff
            do ib=-sizeBuff,sizeBuff
               eS(ib,jb)=e**(-(ib**2+jb**2)/f)
            enddo
         enddo
         
         
c Initialisation fonction distance/pente     
c         fMNT=convMNT*pi/(dMax*2.)
c         do sl=0,255
c            if (sl*convMNT.le.dMax) then
c               dSlope(sl+1)=(factan/cos(fMNT*sl))/factorSizeMNT
c            else
c               dSlope(sl+1)=dSlope(sl)
c            endif
c         enddo
   
c Lecture MNT
c         filename='Lib/Ancillary/Img/SRTM6.img'
c         open(10,file=filename,access='direct',status='old',
c     c   recl=1*nbpixel*nbline*(factorSizeMNT**2))
c         read(10,rec=1) slope(:,:)
c         close(10)
   
c Lecture FBackGround
         filename='Lib/Ancillary/Img/FBackGround_AI.img'
         open(10,file=filename,access='direct',status='old',
     c   recl=4*nbpixel*nbline)
         read(10,rec=1) FBackGround(:,:)
         close(10)
         FBackGround(:,:)=(P0-P1)*FBackGround(:,:)+P1
         
c Reperage decade
         nbdecadPond=0
         nbdecadSWB=0
         nbmissSWB=0
         nbfullYearSWB=0
         filename='Log/BioGenerator_MissingSWB.txt'
         open(11,file=filename)
         write(11,*) 'Missing_SWB_Dekad : '
         yearfi=0
         testPond=0         
         do decad=1,nbdecadtot
            if (decadyear(decad).ne.yearfi) then
               nbdecadyearfi=0
               yearfi=decadyear(decad)
            endif
            filename='Data/In/SWB/SWB_'//
     c      decadnameYYYYMMDD(decad)//'.img'
            inquire(file=filename,exist=filexist)
            if (filexist) then
               test1=1
               nbdecadSWB=nbdecadSWB+1
               nbdecadyearfi=nbdecadyearfi+1
               if (nbdecadyearfi.eq.36) then
                  nbfullYearSWB=nbfullYearSWB+1
               endif
            else
               test1=0
            endif
            filename1='Data/In/DMP/DMP_'//
     c      decadnameYYYYMMDD(decad)//'.img'
            inquire(file=filename1,exist=filexist)
            if ((filexist).and.(test1.eq.0)) then
               write(11,*) filename
               nbmissSWB=nbmissSWB+1
            endif
            filename='Data/Out/Raw/Water/PondSWB/'//
     c      'PondSWB_'//decadnameYYYYMMDD(decad)//'.old'
            inquire(file=filename,exist=filexist)
            if (filexist) then
               open(10,file=filename)
               read(10,*) sizeBuffOld,P0Old,P1Old,
     c         factanOld,ForageFlagOld
               if ((sizeBuff.eq.sizeBuffOld).and.
     c            (P0Old.eq.P0).and.
     c            (P1Old.eq.P1).and.
     c            (factanOld.eq.factan).and.
     c            (ForageFlagOld.eq.ForageFlag)) then
                  test2=0
               else
                  test2=1
               endif               
            else
               test2=1
            endif
            if ((test1.eq.1).and.(test2.eq.1)) then
               testPond=1
               nbdecadPond=nbdecadPond+1
               DecadPond(nbdecadPond)=decad
            endif
         enddo
         close(11)
         
         if (nbfullYearSWB.gt.0) then
            write(*,*) ': OK'
            write(*,*) 
     c      'Nombre decades SWB:',nbdecadSWB,
     c      '/',nbdecadSWB+nbmissSWB
            write(9,*) ': OK'
            write(9,*) 
     c      'Nombre decades SWB:',nbdecadSWB,
     c      '/',nbdecadSWB+nbmissSWB
         else
            write(*,*) ': Erreur'
            write(*,*) 'Nombre de decades SWB insuffisant !'
            write(*,*) 'Verifier fichier ''MissingSWB.txt'''
            write(9,*) ': Erreur'
            write(9,*) 'Nombre de decades SWB insuffisant !'
            write(9,*) 'Verifier fichier ''MissingSWB.txt'''            
            goto 9999
         endif
            
            
     
c Carte des forages
         if (ForageFlag.eq.1)  then
            Forage(:,:)=0
            nbForage=0
            filename='Param/Bores_List.txt'
            open(10,file=filename)
            read(10,*)
18          read(10,*,iostat=iError) Lon,Lat
            if (iError.eq.0) then 
               i=nint((Lon-Lon0)/pixelSD)+1
               j=nint((Lat0-Lat)/pixelSD)+1
               if ((i.ge.1).and.(i.le.nbpixel).and.
     c             (j.ge.1).and.(j.le.nbline)) then
                  Forage(i,j)=1
                  nbForage=nbForage+1
               endif
               goto 18
            endif
            close(10)
            filename='Data/Out/Raw/Water/Bores/Bores.img'
            inquire(file=filename,exist=filexist)
            if (filexist) then
               open(10,file=filename,access='direct',
     c         recl=4*nbpixel*nbline)
               read(10,rec=1) PondForage
               close(10)
               Test1=0
               do i=1,nbpixel
                  do j=1,nbline
                     if (Forage(i,j).ne.int(PondForage(i,j))) then
                        Test1=1
                     endif
                  enddo
               enddo
               filename='Data/Out/Raw/Water/Bores/Bores.old'
               open(10,file=filename)
               read(10,*) sizeBuffOld,P0Old,P1Old,      
     c         factanOld,ForageFlagOld
               if ((sizeBuff.eq.sizeBuffOld).and.
     c            (P0Old.eq.P0).and.
     c            (P1Old.eq.P1).and.
     c            (factanOld.eq.factan)) then
                  test2=0
               else
                  test2=1
               endif
               if ((test1.eq.1).or.(test2.eq.1)) then
                  testForage=1
               else
                  testForage=0
               endif
            else
               testForage=1
            endif
            if (TestForage.eq.1) then
               nbdecadPond=nbdecadPond+1
               DecadPond(nbdecadPond)=-1
            endif
         else
            nbForage=0
         endif
         write(*,*) 
     c   'Nombre de forages :',nbForage 
         write(9,*) 
     c   'Nombre de forages :',nbForage      
                 
   
c Boucle principale 
         write(*,'(a,a,$)') 
     c   ' Calcul access.    :      ',back2
         write(9,'(a,$)') 
     c   ' Calcul access.    :'     
         do decad=1,nbdecadPond
            if (nbdecadPond.gt.1) then
               percent=nint(100.*(decad-1.)/(nbdecadPond-1.))
            else
               percent=0
            endif
            write(*,'(a,I3.3,a,a,$)') back3,percent,' % (1/2)',back8
            if (DecadPond(decad).ne.-1) then         
               filename='Data/In/SWB/SWB_'//
     c         decadnameYYYYMMDD(DecadPond(decad))//'.img'
               open(10,file=filename,access='direct',status='old',
     c         recl=1*nbpixel*nbline)
               read(10,rec=1) outImg8(:,:)
               close(10)
               filename1='Data/Out/Raw/Water/PondSWB/'//
     c         'PondSWB_'//decadnameYYYYMMDD(DecadPond(decad))//'.img'            
               filename2='Data\Out\Raw\Water\PondSWB\'//
     c         'PondSWB_'//decadnameYYYYMMDD(DecadPond(decad))//'.hdr'
               filename3='Data/Out/Raw/Water/PondSWB/'//
     c         'PondSWB_'//decadnameYYYYMMDD(DecadPond(decad))//'.old'     
            else
               outImg8(:,:)=Forage(:,:)
               filename1='Data/Out/Raw/Water/Bores/Bores.img'            
               filename2='Data/Out/Raw/Water/Bores/Bores.hdr'
               filename3='Data/Out/Raw/Water/Bores/Bores.old'             
            endif
                       
            Pond(:,:)=FBackGround(:,:)
            do j=1+sizeBuff,nbline-sizeBuff
               do i=1+sizeBuff,nbpixel-sizeBuff
                  buff8S(1)=outImg8(i,j)
                  SWB=buff16S
                  if ((SWB.ne.0).and.(SWB.ne.255)) then
                     do jb=-sizeBuff,sizeBuff
                        do ib=-is(jb),is(jb)
c                           d=ib*ib+jb*jb    
c                           iSlope=factorSizeMNT*i
c                           jSlope=factorSizeMNT*j   
c                           nbpas=nint(factorSizeMNT*sqrt(d))
c                           pasi=ib/sqrt(d)
c                           pasj=jb/sqrt(d)
c                           dtot=0.
c                           do pas=0,nbpas
c                              buff8S(1)=slope(iSlope+nint(pas*pasi),
c     c                        jSlope+nint(pas*pasj))
c                              dtot=dtot+dSlope(buff16S+1)
c                           enddo
c                           d=dtot*dtot                           
                           PBack=FBackGround(i+ib,j+jb)						   
                           P=(1.-PBack)*eS(ib,jb)+PBack
                           if (Pond(i+ib,j+jb).lt.P) then
                              Pond(i+ib,j+jb)=P
                           endif
                        enddo
                     enddo
                  endif
               enddo
            enddo
               
            open(10,file=filename1,access='direct',
     c      recl=4*nbpixel*nbline)
            write(10,rec=1) Pond
            close(10)           
            call system('copy '//filenameHDR32R//filename2//
     c      '> Lib\Cmd\OutPipe.txt') 
            open(10,file=filename3)
            write(10,*) sizeBuff,P0,P1,factan,ForageFlag
            close(10)
            
         enddo
         
c Cacul Année Type sur Access 
         do j=1,nbblock
            percent=nint(100.*(j-1.)/(nbblock-1.))
            write(*,'(a,I3.3,a,a,$)') back3,percent,' % (2/2)',back8
            if (testPond.eq.1) then
               do decad=1,36
                  Pondtype=0.
                  nbpond=0
                  do year=1,nbyearmax
                     decadd=decad+(year-1)*36
                     filename='Data/Out/Raw/Water/PondSWB/'//
     c               'PondSWB_'//decadnameYYYYMMDD(decadd)//'.img'
                     inquire(file=filename,exist=filexist)
                     if (filexist) then
                        open(10,file=filename,access='direct',
     c                  recl=4*blocksize)
                        read(10,rec=j) OutR
                        close(10)                     
                        do i=1,blocksize
                           Pondtype(i)=Pondtype(i)+OutR(i)
                           nbpond(i)=nbpond(i)+1
                        enddo         
                     endif
                  enddo
                  Pondtype(:)=Pondtype(:)/nbpond(:)
                  filename='Data/Out/Raw/Water/PondSWBType/'//
     c            'PondSWBtype_'//decadnameMMDD(decad)//'.img'
                  open(10,file=filename,access='direct',
     c            recl=4*blocksize)
                  write(10,rec=j) Pondtype(:)
                  close(10)
               enddo
            endif  
         enddo
         if (testPond.eq.1) then
            do decad=1,36
               filename='Data\Out\Raw\Water\PondSWBType\'//
     c         'PondSWBtype_'//decadnameMMDD(decad)//'.hdr'
               call system('copy '//filenameHDR32R//filename//
     c         '> Lib\Cmd\OutPipe.txt')     
            enddo
         endif            
         write(*,*)
         write(9,*) 'OK'
         write(9,*)
      endif      
               

c Vidage repertoires
      if (DegOpt.lt.1) then
         call system('del /Q Data\Out\Raw\Biomass\Filter\DMP\* > '// 
     c   'Lib\Cmd\OutPipe.txt')
      endif
      if (DegOpt.lt.2) then
         call system('del /Q Data\Out\Raw\Biomass\Ponderate\* > '// 
     c   'Lib\Cmd\OutPipe.txt')      
         call system('del /Q Data\Out\Raw\Biomass\Type\DMP\* > '// 
     c   'Lib\Cmd\OutPipe.txt')
      endif      

cccccccccccccccccccc      
c Calcul Principal c
cccccccccccccccccccc      
      write(*,'(a,a,$)') 
     c          ' Calcul principal  :      ',back2  
      write(9,'(a,$)') 
     c          ' Calcul principal  :'     
c Boucle Principale sur les blocks
      do j=1,nbblock
         percent=nint(100.*(j-1.)/(nbblock-1.))
         write(*,'(a,I3.3,a,a,$)') back3,percent,' %',back2
         
         if (DegOpt.lt.1) then
         
c Chargement des données DMP, NDVI sur le block sur l'ensemble des decades
            DMPin(:,:)=-9999.
            NDVin(:,:)=-9999.
            do decad=1,nbdecadtot
               filename='Data/In/DMP/DMP_'//
     c         decadnameYYYYMMDD(decad)//'.img'
               inquire(file=filename,exist=filexist)
               if (filexist) then
                  open(10,file=filename,access='direct',status='old',
     c            recl=2*blocksize)
                  read(10,rec=j) out16
                  close(10)
               else
                  out16(:)=-1
               endif
               do i=1,blocksize
                  if (out16(i).ge.0) then
                     DMPin(i,decad)=0.01*out16(i)
                  endif
               enddo
   
               filename='Data/In/NDVI/NDVI_'//
     c         decadnameYYYYMMDD(decad)//'.img'
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
                  if (buff16S.ne.0) then
                     NDVin(i,decad)=0.004*buff16S-0.1
                  endif 
               enddo           
      
               do i=1,blocksize
                  if ((NDVin(i,decad).eq.-9999.).or.
     c           (DMPin(i,decad).eq.-9999.)) then
                     NDVin(i,decad)=-9999.
                     DMPin(i,decad)=-9999.
                  endif
               enddo
            enddo
                           
            
   
c Filtre PassBas sur NDV & Filtre sur DMP Low & Filtre sur DMP max jump & Interpolation
            do i=1,blocksize
               do decad=1+FiltreSizeSigma,nbdecadtot-FiltreSizeSigma
                  Moy=0.
                  nb=0
                  do dd=-FiltreSizeSigma,FiltreSizeSigma
                     if ((NDVin(i,decad+dd).ne.-9999.).and.
     c                  (dd.ne.0)) then
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
                     if ((DMPin(i,decad).ne.-9999.).and.
     c            (((DMPin(i,decad+1).ne.-9999.).and.
     c            (DMPin(i,decad)-DMPin(i,decad+1).gt.DMPMaxJump)).or.
     c            ((DMPin(i,decad-1).ne.-9999.).and.
     c            (DMPin(i,decad)-DMPin(i,decad-1).gt.DMPMaxJump)))) 
     c               then
                        DMPin(i,decad)=-9999.
                        NDVin(i,decad)=-9999.
                     endif
                  enddo
               endif
                  
                     
   
               decad=1
11             if (NDVin(i,decad).eq.-9999.) then
                  decad=decad+1
                  if (decad.ge.nbdecadtot) then
                     goto 14
                  else
                     goto 11
                  endif
               endif            
12             if (NDVin(i,decad).ne.-9999.) then
                  decad=decad+1
                  if (decad.ge.nbdecadtot) then
                     goto 14
                  endif
                  goto 12
               endif
               NDV1=NDVin(i,decad-1)
               DMP1=DMPin(i,decad-1)
               dd=0
13             if (NDVin(i,decad+dd).eq.-9999.) then
                  dd=dd+1
                  if (decad+dd.gt.nbdecadtot) then
                     goto 14
                  endif
                  goto 13
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
               goto 12
14          enddo
   
   
   
c Passe du filtre temporel BISE
            do i=1,blocksize
   
               decad=1
               decaddeb=decad
20             if (NDVin(i,decad).eq.-9999.) then
                  decad=decad+1
                  decaddeb=decad
                  if (decad.gt.nbdecadtot-FiltreSize) then
                     goto 60
                  else
                     goto 20
                  endif
               endif
   
30             DiffNDV=1.-NDVin(i,decad+1)/NDVin(i,decad)
               if ((DiffNDV.gt.SeuilNDV).and.
     c            (NDVin(i,decad+1).ne.-9999.)) then
                  aNDVm=-9999.
                  decadm=decad
                  do decadd=decad+2,decad+FiltreSize
                     aNDVi=(NDVin(i,decadd)-NDVin(i,decad))/
     c               (decadd-decad)
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
   
               if (decad+FiltreSize.le.nbdecadtot) then
                  decad=decad+1
                  goto 30
               endif
   
               decad=decaddeb
40             if (NDVin(i,decad).ne.-9999.) then
                  decad=decad+1
                  if (decad.ge.nbdecadtot) then
                     goto 60
                  endif
                  goto 40
               endif
   
               NDV1=NDVin(i,decad-1)
               DMP1=DMPin(i,decad-1)
               dd=0
50             if (NDVin(i,decad+dd).eq.-9999.) then
                  dd=dd+1
                  if (decad+dd.gt.nbdecadtot) then
                     goto 60
                  endif
                  goto 50
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
               goto 40
60          enddo

c Sauvegardage des données filtrées
            do decad=1,nbdecadtot
               if (OutRaw.eq.1) then
                  filename='Data/Out/Raw/Biomass/Filter/NDVI/NDVI_'
     c            //decadnameYYYYMMDD(decad)//'.img'
                  open(10,file=filename,access='direct',
     c            recl=4*blocksize)
                  write(10,rec=j) NDVin(:,decad)
                  close(10)
               endif

               if ((OutOpt.eq.1).or.(OutRaw.eq.1)) then
                  filename='Data/Out/Raw/Biomass/Filter/DMP/DMP_'
     c            //decadnameYYYYMMDD(decad)//'.img'
                  open(10,file=filename,
     c            access='direct',recl=4*blocksize)
                  write(10,rec=j) DMPin(:,decad)
                  close(10)
               endif
            enddo
         endif
         
c lecture données filtrées dans le cas OptDeg=1    
         if (DegOpt.eq.1) then  
            do decad=1,nbdecadtot
               filename='Data/Out/Raw/Biomass/Filter/DMP/DMP_'
     c         //decadnameYYYYMMDD(decad)//'.img'
               open(10,file=filename,access='direct',recl=4*blocksize)
               read(10,rec=j) DMPin(:,decad)
               close(10)
            enddo
         endif            


ccccccccccccccccccc
c Ponderation DMP c
ccccccccccccccccccc

         if (DegOpt.lt.2) then
c Initialisation Facteur Ponderation     
c Ponderation Usable      
            if (PondUseFlag.eq.1) then
               filename='Lib/Ancillary/Img/ConvFactor.img'
               open(10,file=filename,access='direct',status='old',
     c         recl=4*blocksize)
               read(10,rec=j) PondDMPuse
               close(10)
               PondDMPuse(:)=((F1-F0)*PondDMPuse(:)+F0)              
            else
               PondDMPuse(:)=1.
            endif
   
c lecture PondForage sur le block
            if ((ForageFlag.eq.1).and.(PondAccFlag.eq.1)) then
               filename='Data/Out/Raw/Water/Bores/Bores.img'
               open(10,file=filename,access='direct',
     c         recl=4*blocksize)
               read(10,rec=j) PondDMPForage(:)
               close(10)
            else
               PondDMPForage(:)=0.
            endif
            
            do decad=1,nbdecadtot                  
c Ponderation Accessible                  
               if (PondAccFlag.eq.1) then
                  filename='Data/Out/Raw/Water/'//
     c            'PondSWB/PondSWB_'//decadnameYYYYMMDD(decad)//'.img'
                  inquire(file=filename,exist=filexist)
                  if (filexist) then
                     open(10,file=filename,access='direct',status='old',
     c               recl=4*blocksize)
                     read(10,rec=j) PondDMPacc(:)
                     close(10)
                  else
                     decadd=decad-36*int((decad-1.)/36.)
                     filename='Data/Out/Raw/Water/'//
     c               'PondSWBType/PondSWBType_'//
     c               decadnameMMDD(decadd)//'.img'
                     open(10,file=filename,access='direct',status='old',
     c               recl=4*blocksize)
                     read(10,rec=j) PondDMPacc(:)
                     close(10)
                  endif
c Maximisation pour les forages
                  do i=1,blocksize
                     if (PondDMPForage(i).gt.PondDMPacc(i)) then
                        PondDMPacc(i)=PondDMPForage(i)
                     endif
                  enddo
               else
                  PondDMPacc(:)=1.
               endif
               
               do i=1,blocksize            
                  if ((DMPin(i,decad).ne.-9999.).and.
     c            (PondDMPacc(i).ne.-9999.)) then
                     DMPin(i,decad)=
     c               PondDMPuse(i)*PondDMPacc(i)*DMPin(i,decad)
                  else
                     DMPin(i,decad)=-9999.
                  endif
               enddo
            enddo
            
            
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc         
c Calcul de l'année type moyenne Median & Filtrage DMP c  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            DMPtype(:,:)=0.
            do i=1,blocksize  
               do decad=1,36
                  nb=0.
                  do year=1,nbyear
                     decadd=decad+(year-1)*36
                     if (DMPin(i,decadd).ne.-9999.) then
                        nb=nb+1.
                        DMPtype(i,decad)=
     c                  DMPtype(i,decad)+DMPin(i,decadd)
                     endif
                  enddo
                  if (nb.gt.0.) then
                     DMPtype(i,decad)=DMPtype(i,decad)/nb
                  else
                     DMPtype(i,decad)=-9999.
                  endif
               enddo
   
c Lissage Année Type            
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
                     if (DMPtype(i,posd).ne.-9999.) then
                        poso=poso+1
                        StackDMP(poso)=DMPtype(i,posd)
                     endif
                  enddo
                  if (poso.eq.3) then
                     DMPTypeF(decad)=
     c               0.25*(StackDMP(1)+StackDMP(3))+0.5*StackDMP(2)
                  else
                     DMPTypeF(decad)=-9999.   
                  endif
               enddo
               DMPType(i,:)=DMPTypeF(:)              
            enddo
            
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc         
c Calcul de l'année type moyenne Median & Filtrage NDVI c  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            if (OutRaw.eq.1) then
            NDVtype(:,:)=0.
            do i=1,blocksize  
               do decad=1,36
                  nb=0.
                  do year=1,nbyear
                     decadd=decad+(year-1)*36
                     if (NDVin(i,decadd).ne.-9999.) then
                        nb=nb+1.
                        NDVtype(i,decad)=
     c                  NDVtype(i,decad)+NDVin(i,decadd)
                     endif
                  enddo
                  if (nb.gt.0.) then
                     NDVtype(i,decad)=NDVtype(i,decad)/nb
                  else
                     NDVtype(i,decad)=-9999.
                  endif
               enddo
   
c Lissage Année Type            
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
                     NDVTypeF(decad)=
     c               0.25*(StackNDV(1)+StackNDV(3))+0.5*StackNDV(2)
                  else
                     NDVTypeF(decad)=-9999.  
                  endif
               enddo
               NDVType(i,:)=NDVTypeF(:)             
            enddo
            endif            

c Sauvegardage des données pondérées & type
            if ((OutOpt.eq.1).or.(OutRaw.eq.1)) then
               do decad=1,nbdecadtot
                  filename='Data/Out/Raw/Biomass/Ponderate/DMP_'
     c            //decadnameYYYYMMDD(decad)//'.img'
                  open(10,file=filename,
     c            access='direct',recl=4*blocksize)
                  write(10,rec=j) DMPin(:,decad)
                  close(10)
               enddo
               do decad=1,36
                  filename='Data/Out/Raw/'//
     c            'Biomass/Type/DMP/DMP_'//
     c            decadnameMMDD(decad)//'.img'
                  open(10,file=filename,
     c            access='direct',recl=4*blocksize)
                  write(10,rec=j) DMPtype(:,decad)
                  close(10)
               enddo
            endif
            
            if (OutRaw.eq.1) then
               do decad=1,36
                  filename='Data/Out/Raw/'//
     c            'Biomass/Type/NDVI/NDVI_'//
     c            decadnameMMDD(decad)//'.img'
                  open(10,file=filename,access='direct',
     c            recl=4*blocksize)
                  write(10,rec=j) NDVtype(:,decad)
                  close(10)
               enddo
            endif
         endif
         
c lecture données pondérées dans le cas OptDeg=2    
         if (DegOpt.eq.2) then  
            do decad=1,nbdecadtot
               filename='Data/Out/Raw/Biomass/Ponderate/DMP_'
     c         //decadnameYYYYMMDD(decad)//'.img'
               open(10,file=filename,access='direct',recl=4*blocksize)
               read(10,rec=j) DMPin(:,decad)
               close(10)
            enddo
            do decad=1,36
               filename='Data/Out/Raw/'//
     c         'Biomass/Type/DMP/DMP_'//decadnameMMDD(decad)//'.img'
               open(10,file=filename,access='direct',recl=4*blocksize)
               read(10,rec=j) DMPtype(:,decad)
               close(10)
            enddo
         endif              


c Calcul de la somme sur l'année type         
         do i=1,blocksize
            sumDMPType(i)=0.
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

         filename='Data/Out/Raw/'//
     c   'Biomass/Biomass/Biomass_Mean.img'         
         open(10,file=filename,access='direct',recl=4*blocksize)
         write(10,rec=j) sumDMPType
         close(10)

c Calcul de la somme sur chaque année (AnVI année en cours)
         AnVI(:,:)=-9999.
         do year=1,nbyear
            sumDMP(:)=0.
            CumulDMP(:,:)=0.
            decadCumul(:)=-9999
            do i=1,blocksize
c               sumBnDenVI=0.
c               sumBnNumVI=0.
               nb=0.
               do decadl=decaddebp,decadfinp
                  if (decadl.gt.36) then
                     decad=decadl-36
                  else
                     decad=decadl
                  endif
                  decadd=decadl+(year-1)*36
                  decadCumul(decad)=decadd
                  if (DMPin(i,decadd).ne.-9999.) then     
                     nb=nb+1.                   
                     sumDMP(i)=sumDMP(i)+DMPin(i,decadd)
c                     sumBnNumVI=sumBnNumVI+DMPin(i,decadd)                    
c                     if (year.eq.nbyear) then
c                        sumback=0.
c                        nbback=0.
c                        do yearback=year,1,-1
c                           decadback=decadl+(yearback-1)*36
c                           VIfact=(1.-VIalpha/100.)**(year-yearback)
c                           if (DMPin(i,decadback).ne.-9999.) then
c                              sumback=sumback+VIfact*DMPin(i,decadback)
c                              nbback=nbback+VIfact
c                           else
c                              if (DMPtype(i,decad).ne.9999) then
c                                 sumback=sumback+VIfact*DMPtype(i,decad)                        
c                                 nbback=nbback+VIfact
c                              endif
c                           endif
c                        enddo
c                        if (nbback.ne.0.) then
c                           sumBnDenVI=sumBnDenVI+sumback/nbback
c                        endif
c                     endif
                         
                  else
                     if (DMPtype(i,decad).ne.-9999.) then
                        nb=nb+1.
                        sumDMP(i)=sumDMP(i)+
     c                  DMPtype(i,decad)
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
               
c               if (year.eq.nbyear) then
c                  if (sumBnDenVI.ne.0.) then
c                     AnVI(i,year)=sumBnNumVI/sumBnDenVI
c                  else
c                     AnVI(i,year)=-9998.
c                  endif
c               endif
                         
            enddo
            
            BiomassBlk(:,year)=sumDMP(:)

            filename='Data/Out/Raw/Biomass/Biomass/Biomass_'
     c      //numer4(years(year))//'.img'        
            open(10,file=filename,access='direct',recl=4*blocksize)
            write(10,rec=j) sumDMP
            close(10)
            
c Sauvegardage des données Cumulées
            if (OutRaw.eq.1) then
               do decad=1,36
                  if (decadCumul(decad).ne.-9999) then
                     filename='Data/Out/Raw/Biomass/Cumul/Cumul_'
     c               //decadnameYYYYMMDD(decadCumul(decad))//'.img'
                     open(10,file=filename,access='direct',
     c               recl=4*blocksize)
                     write(10,rec=j) CumulDMP(:,decad)
                     close(10)
                  endif
               enddo
            endif

c Mise en mémoire données Cumulées jusqu'à dernière decade année en cours
            sumDMPlastp(:,year)=CumulDMP(:,decadlastp)
         enddo

c Calcul AnVI (sauf année en cours)
c         do year=1,nbyear-1
         do year=1,nbyear
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
c            filename='Data/Out/Raw/Biomass/VI/AnVI_'
c     c      //numer4(years(year))//'.img'        
c            open(10,file=filename,access='direct',recl=4*blocksize)
c            write(10,rec=j) AnVI(:,year) 
c            close(10)             
         enddo            

         do year=1,nbyear
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
            filename='Data/Out/Raw/Biomass/VI/VI_'
     c      //numer4(years(year))//'.img'
            open(10,file=filename,access='direct',recl=4*blocksize)
            write(10,rec=j) VI(:)
            close(10)
         enddo         
         
         
c Calcul de Sigma, Trend et R2
         SigmaBlk(:)=-9999.
         TrendBlk(:)=-9999.
         TrendBlkP(:)=-9999.
         R2Blk(:)=-9999.
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
         filename='Data/Out/Raw/'//
     c   'Biomass/Stat/Sigma.img'       
         open(10,file=filename,access='direct',recl=4*blocksize)
         write(10,rec=j) SigmaBlk(:)
         close(10)         
         filename='Data/Out/Raw/'//
     c   'Biomass/Stat/Trend.img'       
         open(10,file=filename,access='direct',recl=4*blocksize)
         write(10,rec=j) TrendBlk(:)
         close(10)
         filename='Data/Out/Raw/'//
     c   'Biomass/Stat/TrendPercent.img'       
         open(10,file=filename,access='direct',recl=4*blocksize)
         write(10,rec=j) TrendBlkP(:)
         close(10) 
         filename='Data/Out/Raw/'//
     c   'Biomass/Stat/TrendR2.img'       
         open(10,file=filename,access='direct',recl=4*blocksize)
         write(10,rec=j) R2Blk(:)
         close(10)

c Calcul Sigma jusqu'à fin année en cours
         SigmaBlkLastp(:)=-9999.
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
         
         
c Calcul des anomalies % et Sigma pour chaque année
         do year=1,nbyear
            AnomDMP(:)=-9999.
            AnomSigma(:)=-9999.
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
                     nbTot=nbTot+1
                     if (DMPin(i,decadd).ne.-9999.) then
                        sDMPin=sDMPin+DMPin(i,decadd)
                        sDMPtype=sDMPtype+DMPtype(i,decad)
                        nb=nb+1.
                     endif
                  endif                  
               enddo
               
               if (nb.gt.0.) then
                  if (coefDMP*sDMPtype.lt.sDMPmin) then
                     if (sDMPtype.eq.0.) then
                        AnomDMP(i)=-9998.
                        AnomSigma(i)=-9998.
                     else
                        AnomDMP(i)=-9996.
                        AnomSigma(i)=-9996.
                     endif
                  else
                     AnomDMP(i)=100.*sDMPin/sDMPtype             
                     if (year.ne.nbyear) then
                        if (SigmaBlk(i).gt.0.) then
                           AnomSigma(i)=
     c                     coefDMP*(sDMPin-sDMPtype)/SigmaBlk(i)                     
                        else
                           AnomSigma(i)=-9998.
                        endif
                     else
                        if (SigmaBlkLastp(i).gt.0) then
                           AnomSigma(i)=
     c                     coefDMP*(sDMPin-sDMPtype)/SigmaBlkLastp(i)
                        else
                           AnomSigma(i)=-9998.
                        endif                        
                        if ((SDMPTypeTot.gt.0.).and.
     c                     (sDMPtype/sDMPtypeTot.lt.
     c                     SeuilCumulLastYear)) then
                           AnomDMP(i)=-9997.
                           AnomSigma(i)=-9997.
                        endif
                     endif
                  endif
               endif
            enddo

            filename='Data/Out/Raw/Biomass/Anomaly/Anomaly_'
     c      //numer4(years(year))//'.img'
            open(10,file=filename,access='direct',recl=4*blocksize)
            write(10,rec=j) AnomDMP(:)
            close(10)

            filename='Data/Out/Raw/Biomass/Anomaly/AnomalySigma_'
     c      //numer4(years(year))//'.img'
            open(10,file=filename,access='direct',recl=4*blocksize)
            write(10,rec=j) AnomSigma(:)
            close(10)
         enddo            

      enddo          
         
      
cccccccccccccccccccccccccccccccc
c Ecriture des HDR et des META c
cccccccccccccccccccccccccccccccc
      write(*,*)
      write(*,'(a,$)') ' Ecriture          :    '
      write(9,*) 'OK'
      write(9,'(a,$)') ' Ecriture          :'      
      
      if (OutRaw.eq.1) then       
         do decad=1,36
            filename='Data\Out\Raw\'//
     c      'Biomass\Type\NDVI\NDVI_'//decadnameMMDD(decad)//'.hdr'
            call system('copy '//filenameHDR32R//filename//
     c      '> Lib\Cmd\OutPipe.txt')
         enddo

         do decad=1,nbdecadtot
            filename='Data\Out\Raw\Biomass\Filter\NDVI\NDVI_'
     c      //decadnameYYYYMMDD(decad)//'.hdr'
            call system('copy '//filenameHDR32R//filename//
     c      '> Lib\Cmd\OutPipe.txt')       

            filename='Data\Out\Raw\Biomass\Cumul\Cumul_'
     c      //decadnameYYYYMMDD(decad)//'.hdr'
            call system('copy '//filenameHDR32R//filename//
     c      '> Lib\Cmd\OutPipe.txt') 
         enddo
      endif
      
      if ((OutOpt.eq.1).or.(OutRaw.eq.1)) then
         do decad=1,nbdecadtot
            filename='Data\Out\Raw\Biomass\Filter\DMP\DMP_'
     c      //decadnameYYYYMMDD(decad)//'.hdr'
            call system('copy '//filenameHDR32R//filename//
     c      '> Lib\Cmd\OutPipe.txt')

            filename='Data\Out\Raw\Biomass\Ponderate\DMP_'
     c      //decadnameYYYYMMDD(decad)//'.hdr'
            call system('copy '//filenameHDR32R//filename//
     c      '> Lib\Cmd\OutPipe.txt')
         enddo        
         do decad=1,36      
            filename='Data\Out\Raw\'//
     c      'Biomass\Type\DMP\DMP_'//decadnameMMDD(decad)//'.hdr'
            call system('copy '//filenameHDR32R//filename//
     c      '> Lib\Cmd\OutPipe.txt')
         enddo
      endif
      
      if ((testPond.eq.1).and.(PondAccFlag.eq.1)) then
         do decad=1,36
            filename='Data\Out\Raw\Water\PondSWBType\'//
     c      'PondSWBtype_'//decadnameMMDD(decad)//'.hdr'
            call system('copy '//filenameHDR32R//filename//
     c      '> Lib\Cmd\OutPipe.txt')
         enddo
      endif 
      
      filename='Data\Out\Raw\Biomass\Stat\Sigma.hdr'
      call system('copy '//filenameHDR32R//filename//
     c'> Lib\Cmd\OutPipe.txt')

      filename='Data\Out\Raw\Biomass\Stat\Trend.hdr'
      call system('copy '//filenameHDR32R//filename//
     c'> Lib\Cmd\OutPipe.txt')
     
      filename='Data\Out\Raw\Biomass\Stat\TrendPercent.hdr'
      call system('copy '//filenameHDR32R//filename//
     c'> Lib\Cmd\OutPipe.txt')

      filename='Data\Out\Raw\Biomass\Stat\TrendR2.hdr'
      call system('copy '//filenameHDR32R//filename//
     c'> Lib\Cmd\OutPipe.txt')
      
      filename='Data\Out\Raw\Biomass\Biomass\Biomass_Mean.hdr'  
      call system('copy '//filenameHDR32R//filename//
     c'> Lib\Cmd\OutPipe.txt')
      
      do year=1,nbyear      
         filename='Data\Out\Raw\Biomass\Biomass\Biomass_'
     c   //numer4(years(year))//'.hdr'        
         call system('copy '//filenameHDR32R//filename//
     c   '> Lib\Cmd\OutPipe.txt') 
         
         filename='Data\Out\Raw\Biomass\Anomaly\Anomaly_'
     c   //numer4(years(year))//'.hdr'         
         call system('copy '//filenameHDR32R//filename//
     c   '> Lib\Cmd\OutPipe.txt')
         
         filename='Data\Out\Raw\Biomass\Anomaly\AnomalySigma_'
     c   //numer4(years(year))//'.hdr'         
         call system('copy '//filenameHDR32R//filename//
     c   '> Lib\Cmd\OutPipe.txt')

         filename='Data\Out\Raw\Biomass\VI\VI_'
     c   //numer4(years(year))//'.hdr'
         call system('copy '//filenameHDR32R//filename//
     c   '> Lib\Cmd\OutPipe.txt') 

c         filename='Data\Out\Raw\Biomass\AnVI\AnVI_'
c     c   //numer4(years(year))//'.hdr'
c         call system('copy '//filenameHDR32R//filename//
c     c   '> Lib\Cmd\OutPipe.txt')         
      enddo
      
      
cccccccccccccccccccccccccccccccccccc      
c Ecriture Fichiers Stat sur Admin c
cccccccccccccccccccccccccccccccccccc    
      filename='Lib/Ancillary/Img/PixelSizeMap.img'
      open(10,file=filename,access='direct',
     crecl=4*nbpixel*nbline)
      read(10,rec=1) PixelSizeMap
      close(10)
      do Adm=0,nbAdmin
         SumAdminBio(:,:)=0.
         sumAdminVI(:,:)=0.
         filename=filenameAdmin(Adm)
         open(10,file=filename,access='direct',
     c   recl=4*nbpixel*nbline)
         read(10,rec=1) Admin
         close(10)
            
c Ouverture fichier sortie
         if (Adm.le.2) then
            filename1='Data/Out/Biomass/Report/Biomass_ADM_'//
     c      numer1(Adm)//'.csv'
            filename2='Data/Out/Biomass/Report/VI_ADM_'//
     c      numer1(Adm)//'.csv'
            filename3='Lib/Ancillary/Img/ADM_'//
     c      numer1(Adm)//'.txt'
         else
            filename1='Data/Out/Biomass/Report/Biomass_GEO_'//
     c      numer1(Adm)//'.csv'
            filename2='Data/Out/Biomass/Report/VI_GEO_'//
     c      numer1(Adm)//'.csv'
            filename3='Lib/Ancillary/Img/GEO_'//
     c      numer1(Adm)//'.txt'     
         endif
         open(11,file=filename1)
         write(11,*) ';Source:;',filename,';'
         open(12,file=filename2)
         write(12,*) ';Source:;',filename,';'

c Lecture info sur ADM & GEO
         open(10,file=filename3)
         read(10,*) nbID
         if (nbID.gt.0) then
            do IDi=1,nbID
               read(10,*) AdminID(IDi),
     c         AdminIDimin(IDi),AdminIDimax(IDi),
     c         AdminIDjmin(IDi),AdminIDjmax(IDi),
     c         AdminIDsurf(IDi),AdminIDname(IDi)
            enddo
         endif
         close(10)

         write(11,*) ';Nb_entities:;',nbID,';'
         write(11,*) ';Biomass_Product_unit:;[tons];'
         write(11,*) ';'
         write(12,*) ';Nb_entities:;',nbID,';'
         write(12,*) ';Vulnerability_Index:;[No Unit];'
         write(12,*) ';'      
            
c Calcul Biomass & VI
         do year=0,nbyear
            percent=nint(100.*(year+Adm*nbyear)/
     c      (1.*(nbyear*(nbAdmin+1.))))
            write(*,'(a,I3.3,a,a,$)') back3,percent,' % (1/2)',back8
            if (year.eq.0) then
               ext='Mean'
            else
               ext=numer4(years(year))
            endif
            filename='Data/Out/Raw/Biomass/Biomass/Biomass_'//
     c      ext//'.img'
            open(10,file=filename,access='direct',
     c      recl=4*nbpixel*nbline)
            read(10,rec=1) outImgR(:,:)
            close(10)
            
            if (year.ne.0) then
               filename='Data/Out/Raw/Biomass/VI/VI_'//
     c         ext//'.img'
               open(10,file=filename,access='direct',
     c         recl=4*nbpixel*nbline)
               read(10,rec=1) outImgRs(:,:)
               close(10)
            else
               outImgRs(:,:)=-9999.
            endif           
            
            do loop=1,nbID
               surfBio=0.
               surfVI=0.
               do i=AdminIDimin(loop),AdminIDimax(loop)
                  do j=AdminIDjmin(loop),AdminIDjmax(loop)
                     if ((AdminID(loop).eq.Admin(i,j)).and.
     c                  (outImgR(i,j).ge.0.)) then
                        sumAdminBio(loop,year)=sumAdminBio(loop,year)+
     c                  0.001*outImgR(i,j)*PixelSizeMap(i,j)
                        surfBio=surfBio+PixelSizeMap(i,j)
                     endif
                     if ((AdminID(loop).eq.Admin(i,j)).and.
     c                  (abs(outImgRs(i,j)).le.1.)) then
                        sumAdminVI(loop,year)=sumAdminVI(loop,year)+
     c                  outImgRs(i,j)*PixelSizeMap(i,j)
                        surfVI=surfVI+PixelSizeMap(i,j)
                     endif
                  enddo
               enddo
               if (surfBio.gt.0.) then
                  sumAdminBIO(loop,year)=sumAdminBIO(loop,year)
c                  sumAdminBIO(loop,year)=sumAdminBIO(loop,year)/surfBio
               else
                  sumAdminBIO(loop,year)=-9999.0
               endif
               if (surfVI.gt.0.) then   
                  sumAdminVI(loop,year)=sumAdminVI(loop,year)/surfVI
               else
                  sumAdminVI(loop,year)=-9999.
               endif
            enddo
         enddo
         
         write(11,'(a,$)') ';Name;ID;Area[sqkm];Mean;'
         do year=1,nbyear
            write(11,'(a,$)') numer4(years(year))//';'
         enddo
         write(11,*)
         do loop=1,nbID
            write(11,'(a,a,a,I6,a,F10.0,a,$)') 
     c      ';',AdminIDname(loop),';',AdminID(loop),
     c      ';',AdminIDsurf(loop),';'
            do year=0,nbyear
               write(11,'(F18.3,a,$)') sumAdminBio(loop,year),';'
            enddo
            write(11,*)
         enddo
         close(11)
         
         write(12,'(a,$)') ';Name;ID;Area[sqkm];'
         do year=1,nbyear
            write(12,'(a,$)') numer4(years(year))//';'
         enddo
         write(12,*)
         do loop=1,nbID
            write(12,'(a,a,a,I6,a,F10.0,a,$)') 
     c      ';',AdminIDname(loop),';',AdminID(loop),
     c      ';',AdminIDsurf(loop),';'
            do year=1,nbyear
               write(12,'(F18.3,a,$)') sumAdminVI(loop,year),';'
            enddo
            write(12,*)
         enddo
         close(12)            
      enddo       

      
cccccccccccccccccccccccccccccccccccc
c Relecture et re-écriture geotiff c
cccccccccccccccccccccccccccccccccccc

      nbtiff=1
      filetiffin(nbtiff)='Data/Out/Raw/Biomass/Stat/Sigma.img'
      filetiffou(nbtiff)='Data/Out/Biomass/Stat/Sigma.tif'
      filetiffcl(nbtiff)='Data\Out\Raw\Biomass\Stat\Sigma.*'
      typetiff(nbtiff)=1
      filttiff(nbtiff)=1
      
      nbtiff=nbtiff+1
      filetiffin(nbtiff)='Data/Out/Raw/Biomass/Stat/Trend.img'
      filetiffou(nbtiff)='Data/Out/Biomass/Stat/Trend.tif'
      filetiffcl(nbtiff)='Data\Out\Raw\Biomass\Stat\Trend.*'
      typetiff(nbtiff)=1
      filttiff(nbtiff)=1 
      
      nbtiff=nbtiff+1
      filetiffin(nbtiff)='Data/Out/Raw/Biomass/Stat/TrendPercent.img'
      filetiffou(nbtiff)='Data/Out/Biomass/Stat/TrendPercent.tif'
      filetiffcl(nbtiff)='Data\Out\Raw\Biomass\Stat\TrendPercent.*'
      typetiff(nbtiff)=1
      filttiff(nbtiff)=1 

      nbtiff=nbtiff+1
      filetiffin(nbtiff)='Data/Out/Raw/Biomass/Stat/TrendR2.img'
      filetiffou(nbtiff)='Data/Out/Biomass/Stat/TrendR2.tif'
      filetiffcl(nbtiff)='Data\Out\Raw\Biomass\Stat\TrendR2.*'
      typetiff(nbtiff)=1
      filttiff(nbtiff)=1

      nbtiff=nbtiff+1
      filetiffin(nbtiff)='Data/Out/Raw/Biomass/Biomass/Biomass_Mean.img'
      filetiffou(nbtiff)='Data/Out/Biomass/Biomass/Biomass_Mean.tif' 
      filetiffcl(nbtiff)='Data\Out\Raw\Biomass\Biomass\Biomass_Mean.*'
      typetiff(nbtiff)=2
      filttiff(nbtiff)=2       
      
      do year=1,nbyear
         nbtiff=nbtiff+1
         filetiffin(nbtiff)='Data/Out/Raw/Biomass/Biomass/Biomass_'//
     c   numer4(years(year))//'.img'
         filetiffou(nbtiff)='Data/Out/Biomass/Biomass/Biomass_'//
     c   numer4(years(year))//'.tif'
         filetiffcl(nbtiff)='Data\Out\Raw\Biomass\Biomass\Biomass_'//
     c   numer4(years(year))//'.*'      
         typetiff(nbtiff)=2
         filttiff(nbtiff)=2         

         nbtiff=nbtiff+1
         filetiffin(nbtiff)='Data/Out/Raw/Biomass/Anomaly/Anomaly_'//
     c   numer4(years(year))//'.img'
         filetiffou(nbtiff)='Data/Out/Biomass/Anomaly/Anomaly_'//
     c   numer4(years(year))//'.tif'
         filetiffcl(nbtiff)='Data\Out\Raw\Biomass\Anomaly\Anomaly_'//
     c   numer4(years(year))//'.*'     
         typetiff(nbtiff)=3
         filttiff(nbtiff)=3
         
         nbtiff=nbtiff+1
         filetiffin(nbtiff)=
     c   'Data/Out/Raw/Biomass/Anomaly/AnomalySigma_'//
     c   numer4(years(year))//'.img'
         filetiffou(nbtiff)=
     c   'Data/Out/Biomass/Anomaly/AnomalySigma_'//
     c   numer4(years(year))//'.tif'
         filetiffcl(nbtiff)=
     c   'Data\Out\Raw\Biomass\Anomaly\AnomalySigma_'//
     c   numer4(years(year))//'.*'
         typetiff(nbtiff)=4
         filttiff(nbtiff)=3 

         nbtiff=nbtiff+1
         filetiffin(nbtiff)='Data/Out/Raw/Biomass/VI/VI_'//
     c   numer4(years(year))//'.img'
         filetiffou(nbtiff)='Data/Out/Biomass/VI/VI_'//
     c   numer4(years(year))//'.tif'
         filetiffcl(nbtiff)='Data\Out\Raw\Biomass\VI\VI_'//
     c   numer4(years(year))//'.*'     
         typetiff(nbtiff)=5
         filttiff(nbtiff)=3        
      enddo
      
c Init Filtre Spatiale      
      if (flagFiltre.ne.0) then
         do jb=-sizeFiltre,sizeFiltre
            is(jb)=nint((sizeFiltre**2-jb**2)**0.5)
         enddo
      endif

      do tiffout=1,nbtiff
         percent=nint(100.*(tiffout-1.)/(1.*(nbtiff-1.)))
         write(*,'(a,I3.3,a,a,$)') back3,percent,' % (2/2)',back8
      
         filename=filetiffin(tiffout)
         open(10,file=filename,access='direct',
     c   recl=4*nbpixel*nbline)
         read(10,rec=1) outImgR
         close(10)
         
c Filtre Spatiale
         if (flagFiltre.ne.0) then
            OutImgRs(:,:)=OutImgR(:,:)
            do loop=1,nbLoopFiltre
               do i=1+sizeFiltre,nbpixel-sizeFiltre
                  do j=1+sizeFiltre,nbline-sizeFiltre
                     if (OutImgR(i,j).gt.-9990.) then
                        moy=0.
                        nb=0.
                        do jb=-sizeFiltre,sizeFiltre
                           do ib=-is(jb),is(jb)
                              if (OutImgR(i+ib,j+jb).gt.-9990.) then
                                 moy=moy+OutImgR(i+ib,j+jb)
                                 nb=nb+1.
                              endif
                           enddo
                        enddo
                        OutImgRs(i,j)=moy/nb   
                     else
                        OutImgRs(i,j)=OutImgR(i,j)
                     endif
                  enddo
               enddo
               OutImgR(:,:)=OutImgRs(:,:)                   
            enddo
         endif         
         
c Limitation valeurs
c Anomaly %
         if (Typetiff(tiffout).eq.3) then
            do i=1,nbpixel
               do j=1,nbline
                  if (OutImgR(i,j).gt.200.) then
                     OutImgR(i,j)=200.
                  endif
                  if ((OutImgR(i,j).lt.0.).and.
     c            (OutImgR(i,j).gt.-9990.)) then
                     OutImgR(i,j)=0.
                  endif
               enddo
            enddo
         endif 
         
c Anomaly Sigma
         if (Typetiff(tiffout).eq.4) then
            do i=1,nbpixel
               do j=1,nbline
                  if (OutImgR(i,j).gt.10.) then
                     OutImgR(i,j)=10.
                  endif
                  if ((OutImgR(i,j).lt.-10.).and.
     c            (OutImgR(i,j).gt.-9990.)) then
                     OutImgR(i,j)=-10.
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
      
c Ecriture fichier ancien parametres pour optimisation
      filename='Lib/Param_Old/BioGenerator_Param_Old.txt'
      open(10,file=filename)
      write(10,*) 
     c   nbdecadtot-decaldecad,yearlast,monthlast,decadlast
      write(10,*)
     c   PondUseFlag,F0,F1       
      write(10,*)
     c   PondAccFlag,RWater,P0,P1,ForageFlag   
      close(10)
      
C Appel fonction matlab pour ecriture Shapefile
      filename='Lib\Bin\BioGenerator_csv2shp.exe'
      call SYSTEM(filename)
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
      
      filename1='Lib\tmp\BioGenerator_Report.txt'
      filename2='Data\Out\Biomass\Report\'
      call system('move '//filename1//filename2//
     c'> Lib\Cmd\OutPipe.txt')
      
      read(*,*)
      end 