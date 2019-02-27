program WaterAccess
!cccccccccccccccccccccccccccccccccccccccccccccc
! Erwann Fillol, Action Contre la Faim (2017) c
! WaterAccess v1.0 beta                       c
!cccccccccccccccccccccccccccccccccccccccccccccc
! gfortran -static -fopenmp Sources\WaterAccess.f95 -o Lib\Bin\WaterAccess.exe
      
      implicit none
      
      integer*8 &
      sizeBuffmax,nbpixel,nbline,year1,month1,day1,nbdecadmax,nbyearmax,maxmissing,&
      MemAvailableMin,nbThreadMaxMax,nblineblockmin,sizeDilat,sizeDilatMax
      
      real*8 &
      pixelSkm,Lat0,Lon0,PixelSD,e,pi,factorMem,factorThread

! Definition fenetre
      parameter (nbpixel=7841)
      parameter (nbline=3630)      
      
! Parametres Water Access
      parameter (sizeBuffMax=200)
      
! Parameters Geo
      parameter (Lat0=27.379464)
      parameter (Lon0=-18.004464)
      parameter (PixelSD=0.00892857)      
      parameter (PixelSKm=0.9728653504)
      
! Parametre allocation
      parameter (nbyearmax=50)
      parameter (nbdecadmax=nbyearmax*36)
      parameter (nblineblockmin=6)
      
! Parametres decades
      parameter (year1=1998)
      parameter (month1=04)
      parameter (day1=11)
      parameter (maxmissing=3)

! Parametres maths
      parameter (e=2.718281828)
      parameter (pi=3.141592654)

! Parametres Memoire
      parameter (MemAvailableMin=1000000000)
      parameter (factorMem=1./2.)
      parameter (factorThread=4./8.)
      parameter (nbThreadMaxMax=256)

! Paremetre dilatation mask
      parameter (sizeDilat=1)
      parameter (sizeDilatMax=11)
      
      
      integer*8 &
      ForageFlag,decadDeb,decadFin,decadDebOld,decadFinOld,nbdecadtot,nbdecadtotOld,&
      quiet,compress,&
      sizeBuff,sizeBuffOld,year,month,day,nbdecad,nbyear,percent,&
      DecadDD(nbdecadmax),DecadDY(nbdecadmax),DecadYY(nbdecadmax),decadCompute(nbdecadmax),&
      lastexist,nbForage,i,j,ib,jb,test1,test2,ForageTest,&
      compute,nbdecadT,decadT,decadTT(36),&
      nbblock,MemAvailable,allocstat,decad,blocksize,nblineblock,ForageFlagOld,&
      MemAllocNeed,SWB,iError,FinalOk,nbdecaddone,nbThread,nbThreadMax,&
      OMP_GET_MAX_THREADS,OMP_GET_NUM_THREADS,OMP_GET_THREAD_NUM,&
      time1(8),time2(8),nbThreadStart,AllocOk,allocStatTot,&
      nbTremp,nbSuccDetect,nbYearDetect,nbSuccDetectOld,nbYearDetectOld,ActualYear,&
      isb(-sizeDilatMax:sizeDilatMax)
      
      
      real*8 &
      RWater,RWaterOld,P0,P1,P0old,P1old,&
      f,Lon,Lat,PBack,P,nbpond,frac,diffH
      
      character*100 &
      filename,filename0,filename1,filename2,filename3,&
      filenameHDR32R,filenameHDR8U,output1,output2
      
      character*4 &
      decadnameMMDD(36),numer4(0:9999)
      
      character*3 &
      back3
      
      character*2 &
      numer2(0:99),back2
      
      character*1 &
      numer1(0:9),back1
      
      character*8 &
      decadnameYYYYMMDD(nbdecadmax),back8
      
      logical &
      filexist,filexistAll
      
      character*1, dimension(:,:), allocatable ::& !(nbpixel,nbline)
      outImg8
      
      integer*1, dimension(:,:), allocatable ::& !(nbpixel,nbline)
      mask,ActualYearDetect,maskDilat,GLC
      
      integer*4, dimension(:), allocatable ::& !(-sizeBuff:sizeBuff)
      is  

      integer*8, dimension(:,:), allocatable ::& !(nbpixel,nbline)
      Sremp       
      
      real*4, dimension(:,:), allocatable ::& !(nbpixel,nbline)
      outImg32R,FBackGround,Forage,TotYearDetect 
      
      real*4, dimension(:,:), allocatable ::& !(nbpixel,nbline)
      Tremp
      
      real*8, dimension(:,:), allocatable ::& !(nbpixel,nbline)
      Pond,PondType

      real*8, dimension(:,:), allocatable ::& !(-sizeBuff:sizeBuff,-sizeBuff:sizeBuff)
      eS     
      
! Heure départ
      call date_and_time(VALUES=time1)
      
      
! Initialisation

      back8=char(8)//char(8)//char(8)//char(8)//char(8)//char(8)//char(8)//char(8)
      back3=char(8)//char(8)//char(8)
      back2=char(8)//char(8)
      back1=char(8) 

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
      
      decad=1
      month=1
      do i=1,36
         decadnameMMDD(i)=numer2(month)//numer2(decad)
         decad=decad+10
         if (decad.eq.31) then
            decad=1
            month=month+1
            if (month.eq.13) then
               month=1
            endif
         endif
      enddo

! Fenetre de dilatation du mask
      if (sizeDilat.ne.0) then
         do jb=-sizeDilat,sizeDilat
            isb(jb)=nint((sizeDilat**2.-jb**2.)**0.5)
         enddo
      endif

! Flag de validation des résultats      
      FinalOk=0
      filename='Lib\Tmp\WaterAccess_Flag.txt'
      open(10,file=filename)
      write(10,*) FinalOk
      close(10)     
      
! Determination du nombre de thread
      nbThread=1
      nbThreadMax=1
      !$ nbThreadMax=OMP_GET_MAX_THREADS()
      !$ call OMP_SET_NESTED(.TRUE.)
      nbThreadMax=min(nbThreadMax,nbThreadMaxMax)
      nbThreadStart=max(int(factorThread*nbThreadMax),1)

! Lecture Fichiers Auxillaires
      filenameHDR32R='Lib\Ancillary\HDR\32R.hdr'
      filenameHDR8U='Lib\Ancillary\HDR\8U.hdr'
      
      
! WaterAccess Report
      filename='Lib\Tmp\WaterAccess_Report.txt'
      open(9,file=filename)
      
! Lecture param
!     RWater, nbdecadtot, quiet, ForageFlag
      filename='Lib\Tmp\WaterAccess_Param.txt'
      open(10,file=filename)
      read(10,*) RWater,ForageFlag
      read(10,*) P0,P1
      read(10,*) nbSuccDetect,nbYearDetect
      read(10,*) decadDeb,decadFin     
      read(10,*) quiet,compress
      close(10)
      if (decadFin.gt.36) decadFin=decadFin-36
      sizeBuff=nint(1.*RWater/PixelSkm+0.5)
      if (sizeBuff.gt.sizeBuffmax) then
         sizeBuff=sizeBuffmax
      endif

      filename='Lib\Param_Old\WaterAccess_Param_Old.txt'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         open(10,file=filename)
         read(10,*) nbdecadtotOld
         read(10,*) decadDebOld,decadFinOld
         read(10,*) nbSuccDetectOld,nbYearDetectOld
         close(10)
      endif      
      
      
! Verification memoire disponible & Allocation
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
      AllocOk=0

!      MemAllocNeed=&
!      nbpixel*nbline*(4+1+4+8+1)+&
!      ((sizeBuff*2+1)**2)*8+((sizeBuff*2+1)*4)
      
               
!      if (MemAllocNeed.le.factorMem*MemAvailable) then
         allocStatTot=0
         allocate(outImg32R(nbpixel,nbline),stat=allocStat)
         allocStatTot=allocStatTot+allocStat
         allocate(Sremp(nbpixel,nbline),stat=allocStat)
         allocStatTot=allocStatTot+allocStat
         allocate(Tremp(nbpixel,nbline),stat=allocStat)
         allocStatTot=allocStatTot+allocStat
         allocate(ActualYearDetect(nbpixel,nbline),stat=allocStat)
         allocStatTot=allocStatTot+allocStat
         allocate(outImg8(nbpixel,nbline),stat=allocStat)
         allocStatTot=allocStatTot+allocStat
         allocate(mask(nbpixel,nbline),stat=allocStat)
         allocStatTot=allocStatTot+allocStat
         allocate(GLC(nbpixel,nbline),stat=allocStat)
         allocStatTot=allocStatTot+allocStat         
         allocate(maskDilat(nbpixel,nbline),stat=allocStat)
         allocStatTot=allocStatTot+allocStat
         allocate(TotYearDetect(nbpixel,nbline),stat=allocStat)
         allocStatTot=allocStatTot+allocStat
         allocate(eS(-sizeBuff:sizeBuff,-sizeBuff:sizeBuff),stat=allocStat)
         allocStatTot=allocStatTot+allocStat
         allocate(is(-sizeBuff:sizeBuff),stat=allocStat)  
         allocStatTot=allocStatTot+allocStat         
         if (allocStatTot.eq.0) then
            AllocOk=1
         endif
!      endif
      
      if (AllocOk.ne.1) then
         FinalOk=0
         goto 9999
      endif
      
 
! Fichier sortie info 
!      if (quiet.ne.1) then
!      write(*,'(a,$)') ' Access. Eau Init.'
!      endif      
!      write(9,'(a,$)') ' Access. Eau Init.'
  
         
! Initialisation des variables de buffering
      do jb=-sizeBuff,sizeBuff
         is(jb)=nint((sizeBuff**2-jb**2)**0.5)
      enddo
      
      f=((1.*RWater/PixelSkm)**2.)/log(100.)
      do jb=-sizeBuff,sizeBuff
         do ib=-sizeBuff,sizeBuff
            eS(ib,jb)=e**(-(ib**2+jb**2)/f)
         enddo
      enddo

! Lecture GLC2000 pour mares perennes & lacs
      filename='Lib\Ancillary\Img\GLC2000.img'
      open(10,file=filename,access='direct',status='old',recl=1*nbpixel*nbline)
      read(10,rec=1) outImg8
      close(10)
      GLC(:,:)=ichar(outImg8(:,:))       
      
      
! Reperage decade
      decad=0
      year=year1
      month=month1
      day=day1
      
          
10    filename='Data\Raw\In\SWB\SWB_'//numer4(year)//numer2(month)//numer2(day)//'.img'
      inquire(file=filename,exist=filexist)
      if (filexist) then
         decad=decad+1
         decadnameYYYYMMDD(decad)=numer4(year)//numer2(month)//numer2(day)
         DecadDD(decad)=year*10000+month*100+day
         DecadDY(decad)=(month-1)*3+(day-1)/10+1
         DecadYY(decad)=year
         lastexist=maxmissing
      endif
      lastexist=lastexist-1
      if (lastexist.gt.0) then
         day=day+10
         if (day.gt.21) then
            day=1
            month=month+1
            if (month.gt.12) then
               month=1
               year=year+1
            endif
         endif
         goto 10
      endif
      nbdecad=decad
      nbyear=year-year1+1
      
      
      
! Carte des forages
      if (ForageFlag.eq.1)  then
         Forage(:,:)=0
         nbForage=0
         ForageTest=1
         filename='Param\Bores_List.txt'
         open(10,file=filename)
         read(10,*)
18       read(10,*,iostat=iError) Lon,Lat
         if (iError.eq.0) then 
            i=nint((Lon-Lon0)/pixelSD)+1
            j=nint((Lat0-Lat)/pixelSD)+1
            if ((i.ge.1).and.(i.le.nbpixel).and.&
                (j.ge.1).and.(j.le.nbline)) then
               Forage(i,j)=1
               nbForage=nbForage+1
            endif
            goto 18
         endif
         close(10)
         filename='Data\Raw\Water\Bores\Bores.img'
         inquire(file=filename,exist=filexist)
         if (filexist) then
            open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
            read(10,rec=1) outImg32R
            close(10)
            do i=1,nbpixel
               do j=1,nbline
                  if (Forage(i,j).eq.outImg32R(i,j)) then
                     ForageTest=0
                  endif
               enddo
            enddo
         endif
      else
         ForageTest=0
         nbForage=0
      endif

 
      write(output1,*) nbdecad
      write(output1,*) 'Nombre decades SWB: '//trim(adjustl(output1)) 
      if (quiet.ne.1) write(*,'(a)') output1
      write(9,'(a)') output1   
      if (ForageFlag.eq.1)  then
         write(output1,*) nbForage
         write(output1,*) 'Nombre de forages : '//trim(adjustl(output1))
         if (quiet.ne.1) write(*,'(a)') output1
         write(9,'(a)') output1
      endif
      write(*,*)
      output1=' '
      write(9,'(a)') ' '
      
      
      
    
      
!ccccccccccccccccccc      
!c Calcul MareYear c
!ccccccccccccccccccc
      if (quiet.ne.1) then
         write(*,'(a,a,$)') ' Calcul du masque  :      ',back2
      endif
      write(9,'(a,$)') ' Calcul du masque  :'
      
      compute=1
      filexistAll=(.true.)
      filename='Data\Raw\Water\Mask\WaterYear.img'
      inquire(file=filename,exist=filexist)
      filexistAll=(filexistAll.and.filexist)
      filename='Data\Raw\Water\Mask\TotYearDetect.img'
      inquire(file=filename,exist=filexist)
      filexistAll=(filexistAll.and.filexist)
      filename='Data\Raw\Water\Mask\Mask.img'
      inquire(file=filename,exist=filexist)
      filexistAll=(filexistAll.and.filexist)      
      if ((nbdecadtot.eq.nbdecadtotOld).and.&
         (decadDeb.eq.decadDebOld).and.(decadFin.eq.decadFinOld).and.&
         (nbSuccDetect.eq.nbSuccDetectOld).and.(nbYearDetect.eq.nbYearDetectOld).and.&
         (filexistAll)) then
         filename='Data\Raw\Water\Mask\Mask.img'
         open(10,file=filename,access='direct',recl=1*nbpixel*nbline)
         read(10,rec=1) mask
         close(10)
         compute=0
         if (quiet.ne.1) then
            percent=100
            write(*,'(a,I3.3,a)') back3,percent,' %'
         endif
      else
         nbTremp=0
         Tremp(:,:)=0.
         Sremp(:,:)=0
         mask(:,:)=0
         nbdecaddone=0
         ActualYear=DecadYY(1)
         ActualYearDetect(:,:)=0
         TotYearDetect(:,:)=0
         do decad=1,nbdecad
            if (ActualYear.ne.DecadYY(decad)) then
               ActualYear=DecadYY(decad)
               TotYearDetect(:,:)=TotYearDetect(:,:)+ActualYearDetect(:,:)
            endif
            nbdecaddone=nbdecaddone+1
            percent=nint(100.*(nbdecaddone-1.)/(nbdecad-1.))
            if (quiet.ne.1) then
            write(*,'(a,I3.3,a,a,$)') back3,percent,' %',back2
            endif
   
            if (((decadDeb.le.decadFin).and.(DecadDY(decad).ge.decadDeb).and.(DecadDY(decad).le.decadFin)).or.&
               ((decadDeb.gt.decadFin).and.((DecadDY(decad).ge.decadDeb).or.(DecadDY(decad).le.decadFin)))) then
               
               filename='Data\Raw\In\SWB\SWB_'//decadnameYYYYMMDD(decad)//'.img'
               inquire(file=filename,exist=filexist)
               if (filexist) then
                  open(10,file=filename,access='direct',status='old',recl=1*nbpixel*nbline)
                  read(10,rec=1) outImg8
                  close(10)
                  
                  nbTremp=nbTremp+1
                  do i=1,nbpixel
                     do j=1,nbline
                        SWB=ichar(outImg8(i,j))
                        if (((SWB.ne.0).and.(SWB.ne.255)).or.(GLC(i,j).eq.26)) then
                           Tremp(i,j)=Tremp(i,j)+1
                           Sremp(i,j)=int(Sremp(i,j)/2)+2**(nbSuccDetect-1)
                           if (Sremp(i,j).eq.+2**nbSuccDetect-1) then
                              ActualYearDetect(i,j)=1
                           endif
                        endif
                     enddo
                  enddo
               endif
            endif
         enddo
         
! Calcul du mask et de Tremp
         if (nbTremp.ne.0) then
            do i=1,nbpixel
               do j=1,nbline
                  if (TotYearDetect(i,j).ge.nbYearDetect) then
                     mask(i,j)=1
                     Tremp(i,j)=Tremp(i,j)/nbTremp
                  else
                     Tremp(i,j)=0.
                  endif
               enddo
            enddo
         else
            Tremp(:,:)=-9999.
         endif
         
! Dilatation du mask
         maskDilat(:,:)=mask(:,:)
         if (sizeDilat.ne.0) then
            do i=1+sizeDilat,nbpixel-sizeDilat
               do j=1+sizeDilat,nbline-sizeDilat
                  if (mask(i,j).ne.0) then
                     do jb=-sizeDilat,sizeDilat
                        do ib=-isb(jb),isb(jb)
                           maskDilat(i+ib,j+jb)=1
                        enddo
                     enddo
                  endif
               enddo
            enddo
         endif
            
         
! Sauvegarde
         outImg32R(:,:)=Tremp(:,:)
         filename='Data\Raw\Water\Mask\WaterYear.img'
         open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
         write(10,rec=1) outImg32R
         close(10)
         filename='Data\Raw\Water\Mask\WaterYear.hdr'
         call system('copy '//filenameHDR32R//filename//'> Lib\Cmd\OutPipe.txt')
         
         filename='Data\Raw\Water\Mask\TotYearDetect.img'
         open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
         write(10,rec=1) TotYearDetect
         close(10)
         filename='Data\Raw\Water\Mask\TotYearDetect.hdr'
         call system('copy '//filenameHDR32R//filename//'> Lib\Cmd\OutPipe.txt')      
         
         filename='Data\Raw\Water\Mask\Mask.img'
         open(10,file=filename,access='direct',recl=1*nbpixel*nbline)
         write(10,rec=1) mask
         close(10)
         filename='Data\Raw\Water\Mask\Mask.hdr'
         call system('copy '//filenameHDR8U//filename//'> Lib\Cmd\OutPipe.txt') 

         filename='Data\Raw\Water\Mask\MaskDilat.img'
         open(10,file=filename,access='direct',recl=1*nbpixel*nbline)
         write(10,rec=1) maskDilat
         close(10)
         filename='Data\Raw\Water\Mask\MaskDilat.hdr'
         call system('copy '//filenameHDR8U//filename//'> Lib\Cmd\OutPipe.txt')           
         
         if (quiet.ne.1) then
         write(*,*)
         endif

      endif
      write(9,*) 'OK'
      
! Vidage mémoire
      if (allocated(Sremp)) deallocate(Sremp) 
      if (allocated(Tremp)) deallocate(Tremp) 
      if (allocated(ActualYearDetect)) deallocate(ActualYearDetect)
      if (allocated(TotYearDetect)) deallocate(TotYearDetect)
      if (allocated(maskDilat)) deallocate(maskDilat) 

      

      if (quiet.ne.1) then 
      write(*,'(a,a,$)') ' Calcul access.    :      ',back2
      endif
      write(9,'(a,$)')   ' Calcul access.    :'   
 
!ccccccccccccccccccccc
!c Boucle principale c
!ccccccccccccccccccccc

! Verification memoire disponible & Allocation avec optimation nbThread
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
      AllocOk=0
      do nbThread=nbThreadStart,1,-1       
         MemAllocNeed=&
         nbpixel*nbline*(&
         nbThread*(1+8)+&
         (4+1+4))
               
   
         if (MemAllocNeed.le.factorMem*MemAvailable) then         
            allocStatTot=0
            allocate(Pond(nbpixel,nbline),stat=allocStat)  
            allocStatTot=allocStatTot+allocStat
            allocate(FBackGround(nbpixel,nbline),stat=allocStat)
            allocStatTot=allocStatTot+allocStat
            if (allocStatTot.eq.0) then
               AllocOk=1
               exit
            endif
         endif
      enddo
      
      if (AllocOk.ne.1) then
         FinalOk=0
         goto 9999
      endif

      
! Lecture FBackGround
      filename='Lib\Ancillary\Img\FBackGround_AI.img'
      open(10,file=filename,access='direct',status='old',recl=4*nbpixel*nbline)
      read(10,rec=1) outImg32R
      close(10)
      FBackGround(:,:)=0.01*((P0-P1)*outImg32R(:,:)+P1)
      
! Boucle principale
      nbdecaddone=0
      !$OMP PARALLEL DO NUM_THREADS(nbThread) DEFAULT(NONE) &
      !$OMP& PRIVATE(compute,filename,filename0,filename1,filename2,filename3,outImg8,Pond,i,j,ib,jb,SWB,&
      !$OMP& PBack,P,filexist,RWaterOld,P0Old,P1Old,ForageFlagOld) &
      !$OMP& SHARED(nbdecad,nbdecaddone,percent,quiet,back3,back8,DecadDD,DecadDY,decadDeb,decadFin,&
      !$OMP& decadnameYYYYMMDD,Forage,sizeBuff,is,eS,P0,P1,ForageFlag,FBackGround,filenameHDR32R,&
      !$OMP& RWater,ForageTest,decadCompute,mask,outImg32R,GLC)
      do decad=1,nbdecad
      
         !$OMP CRITICAL
         nbdecaddone=nbdecaddone+1
         percent=nint(100.*(nbdecaddone-1.)/(nbdecad-1.))
         if (quiet.ne.1) then
         write(*,'(a,I3.3,a,a,$)') back3,percent,' % (1/2)',back8
         endif
         !$OMP END CRITICAL
         
         compute=0
         decadCompute(decad)=0
         if (((decadDeb.le.decadFin).and.(DecadDY(decad).ge.decadDeb).and.(DecadDY(decad).le.decadFin)).or.&
            ((decadDeb.gt.decadFin).and.((DecadDY(decad).ge.decadDeb).or.(DecadDY(decad).le.decadFin)))) then
            filename0='Data\Raw\In\SWB\SWB_'//decadnameYYYYMMDD(decad)//'.img'
            filename1='Data\Raw\Water\WaterAccess\WaterAccess_'//decadnameYYYYMMDD(decad)//'.img'            
            filename2='Data\Raw\Water\WaterAccess\WaterAccess_'//decadnameYYYYMMDD(decad)//'.hdr'
            filename3='Data\Raw\Water\WaterAccess\WaterAccess_'//decadnameYYYYMMDD(decad)//'.old'
            !$OMP CRITICAL
            inquire(file=filename1,exist=filexist)
            if (filexist) then
               open(10,file=filename3)
               read(10,*) RWaterOld,P0Old,P1Old,ForageFlagOld
               close(10)
            else
               compute=1
            endif
            if ((RWater.ne.RWaterOld).or.(P0.ne.P0Old).or.(P1.ne.P1Old).or.(ForageFlag.ne.ForageFlagOld).or.&
               ((ForageFlag.eq.1).and.(ForageTest.eq.1)).or.(compute.eq.1)) then
               open(10,file=filename0,access='direct',status='old',recl=1*nbpixel*nbline)
               read(10,rec=1) outImg8
               close(10)
               compute=1
            endif
            !$OMP END CRITICAL
                    
            if (compute.eq.1) then
               decadCompute(decad)=1
               Pond(:,:)=FBackGround(:,:)
               do j=1+sizeBuff,nbline-sizeBuff
                  do i=1+sizeBuff,nbpixel-sizeBuff
                     SWB=ichar(outImg8(i,j))
                     if (((((SWB.ne.0).and.(SWB.ne.255)).or.(GLC(i,j).eq.26)).and.(mask(i,j).eq.1)).or.&
                        ((ForageFlag.eq.1).and.(Forage(i,j).eq.1))) then
                        do jb=-sizeBuff,sizeBuff
                           do ib=-is(jb),is(jb)
                              PBack=FBackGround(i+ib,j+jb)						   
                              P=(1.-PBack)*eS(ib,jb)+PBack
                              Pond(i+ib,j+jb)=max(P,Pond(i+ib,j+jb))
                           enddo
                        enddo
                     endif
                  enddo
               enddo
               
               !$OMP CRITICAL
               outImg32R(:,:)=Pond(:,:)
               open(10,file=filename1,access='direct',recl=4*nbpixel*nbline)
               write(10,rec=1) outImg32R
               close(10)           
               call system('copy '//filenameHDR32R//filename2//'> Lib\Cmd\OutPipe.txt') 
               open(10,file=filename3)
               write(10,*) RWater,P0,P1,ForageFlag
               close(10)
               !$OMP END CRITICAL
            endif
         endif            
      enddo
      
! Ecriture fichier Bores.img si necessaire
      if ((ForageFlag.eq.1).and.(ForageTest.eq.1))  then 
         filename='Data\Raw\Water\Bores\Bores.img'
         open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
         write(10,rec=1) Forage
         close(10)        
         filename='Data\Raw\Water\Bores\Bores.hdr'
         call system('copy '//filenameHDR32R//filename//'> Lib\Cmd\OutPipe.txt') 
      endif     
      
      if (allocated(Forage)) deallocate(Forage)      
      if (allocated(outImg8)) deallocate(outImg8)
      if (allocated(FBackGround)) deallocate(FBackGround)
      if (allocated(eS)) deallocate(eS)
      if (allocated(is)) deallocate(is)
      if (allocated(mask)) deallocate(mask)
      if (allocated(Pond)) deallocate(Pond)   
      if (allocated(GLC)) deallocate(GLC)      

99    continue

      AllocOk=0
      allocate(PondType(nbpixel,nbline),stat=allocStat)
      if (allocStat.eq.0) then
         AllocOk=1
      endif
      if (AllocOk.ne.1) then
         FinalOk=0
         goto 9999
      endif  
    
    
! Cacul Année Type sur Access
! Reperage decades Type concernées
      do decadT=1,36
         percent=nint(100.*(decadT-1.)/(36-1.))
         if (quiet.ne.1) then
         write(*,'(a,I3.3,a,a,$)') back3,percent,' % (2/2)',back8
         endif
         
         if (((decadDeb.le.decadFin).and.(DecadT.ge.decadDeb).and.(DecadT.le.decadFin)).or.&
            ((decadDeb.gt.decadFin).and.((DecadT.ge.decadDeb).or.(DecadT.le.decadFin)))) then
            
! Reperage des decades necessaires
            Compute=1
            filename='Data\Raw\Water\WaterAccessType\WaterAccessType_'//decadnameMMDD(decadT)//'.img'
            inquire(file=filename,exist=filexist)
            if (filexist) then
               Compute=0
               do decad=1,nbdecad
                  if (DecadDY(decad).eq.decadT) then
                     filename='Data\Raw\Water\WaterAccess\WaterAccess_'//decadnameYYYYMMDD(decad)//'.img'
                     inquire(file=filename,exist=filexist)
                     if (filexist) then
                        if (decadCompute(decad).eq.1) then
                           Compute=1
                           exit
                        endif
                     endif
                  endif
               enddo
            endif

            
            if (Compute.eq.1) then
!               filename='Data\Raw\Water\WaterAccessType\WaterAccessType_'//decadnameMMDD(decadT)//'.lst'
!               open(11,file=filename)
               PondType(:,:)=0.
               nbpond=0.
               do decad=1,nbdecad
                  if (DecadDY(decad).eq.decadT) then
                     filename='Data\Raw\Water\WaterAccess\WaterAccess_'//decadnameYYYYMMDD(decad)//'.img'
                     inquire(file=filename,exist=filexist)
                     if (filexist) then
                        open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
                        read(10,rec=1) outImg32R
                        close(10)                     
                        PondType(:,:)=PondType(:,:)+outImg32R(:,:)
                        nbpond=nbpond+1.    
!                        write(11,*) decadnameYYYYMMDD(decad)
                     endif
                  endif
               enddo
!               close(11)
               if (nbpond.ne.0.) then
                  outImg32R(:,:)=PondType(:,:)/nbpond
               else
                  outImg32R(:,:)=-9999.
               endif
               filename='Data\Raw\Water\WaterAccessType\WaterAccessType_'//decadnameMMDD(decadT)//'.img'
               open(10,file=filename,access='direct',recl=4*nbpixel*nbline)
               write(10,rec=1) outImg32R
               close(10)
   
               filename='Data\Raw\Water\WaterAccessType\WaterAccessType_'//decadnameMMDD(decadT)//'.hdr'
               call system('copy '//filenameHDR32R//filename//'> Lib\Cmd\OutPipe.txt')
            endif               
         endif
      enddo
        
      
! Sortie
      if (quiet.ne.1) then         
      write(*,*)
      endif
      
      write(9,*) 'OK'
      close(9)
      
      FinalOk=1
      
      
9999  continue

! Vidage mémoire total
      if (allocated(Sremp)) deallocate(Sremp) 
      if (allocated(Tremp)) deallocate(Tremp) 
      if (allocated(ActualYearDetect)) deallocate(ActualYearDetect)
      if (allocated(TotYearDetect)) deallocate(TotYearDetect)
      if (allocated(maskDilat)) deallocate(maskDilat) 
      if (allocated(Forage)) deallocate(Forage)      
      if (allocated(outImg8)) deallocate(outImg8)
      if (allocated(FBackGround)) deallocate(FBackGround)
      if (allocated(eS)) deallocate(eS)
      if (allocated(is)) deallocate(is)
      if (allocated(mask)) deallocate(mask)
      if (allocated(Pond)) deallocate(Pond)   
      if (allocated(GLC)) deallocate(GLC) 
      if (allocated(PondType)) deallocate(PondType)    
      if (allocated(outImg32R)) deallocate(outImg32R)  

      if (AllocOk.eq.0) then
         if (quiet.ne.1) then
         write(*,*) 'Erreur'
         write(*,*) 'Allocation memoire impossible.'
         endif
            
         write(9,*) 'Erreur'
         write(9,*) 'Allocation memoire impossible.'    
      endif  


!      call date_and_time(VALUES=time2)
!      diffH=time2(5)-time1(5)+(1./60.)*(time2(6)-time1(6)+(1./60.)*(time2(7)-time1(7)))
!      if (time1(3).ne.time2(3)) then
!         diffH=diffH+24.
!      endif  
!      write(output1,*) int(diffH)
!      write(output2,*) int(60.*(diffH-int(diffH)))      
!      if (quiet.ne.1) then
!      write(*,*) 
!      write(*,*) 'Temps execution   : '//trim(adjustl(output1))//' h '//trim(adjustl(output2))//' mm'
!      write(*,*)
!      write(*,'(a,$)') ' Operations terminees.'
!      endif
      
      filename='Lib\Param_Old\WaterAccess_Param_Old.txt'
      open(10,file=filename)
      write(10,*) nbdecadtot
      write(10,*) decadDeb,decadFin
      write(10,*) nbSuccDetect,nbYearDetect
      close(10)
      
      filename='Lib\Tmp\WaterAccess_Flag.txt'
      open(10,file=filename)
      write(10,*) FinalOk
      close(10)
      

      ! if (allocated(PondForage)) deallocate(PondForage)






     
     



end program