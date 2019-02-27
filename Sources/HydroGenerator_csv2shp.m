%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HydroGenerator_csv2shp           %
% v1.1 juin 2018                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



clear all

% Repertoires entrée et sortie
pathD=[pwd '\'];
% pathD=['E:\ACF3\BioHydroPond_v3_dev\'];
pathShpIn=[pathD 'Lib\Shape\Shape_Out\'];
pathCsvIn=[pathD 'Output\Water\Report\'];
pathShpOut=[pathD 'Output\Water\Shape\'];
pathShpPrj=[pathD 'Lib\Shape\Shape_Prj\'];
pathOldParam=[pathD 'Lib\Param_Old\'];
pathHideParam=[pathD 'Lib\Bin\'];
pathF=[pathD 'Data\Raw\In\SWB\'];
pathM=[pathD 'Lib\Ancillary\Img\'];
pathR=[pathD 'Data\Raw\Water\Mask\'];

% Parametre
admMax=6;

% Paramètre de remplissage des tables attributaire shapefile
fileName=[pathHideParam 'Common_Param.txt'];
fid=fopen(fileName,'r');
Param=textscan(fid,'%s');
fclose(fid);
fillUpTable=str2double(Param{1}{1});
fillUpTableLastYear=str2double(Param{1}{2});
fillUpTableValue=str2double(Param{1}{3});

% Paramètre taille image
xsize=7841;
ysize=3630;
sizeFile=xsize*ysize;

% Paramètre Geo
Lat0=27.37946430;
Lon0=-18.00446430;
PixelSD=0.00892857143;

% Lecture Param_Old pour récupération Decad_first & Decad_last
fileName=[pathOldParam 'HydroGenerator_Param_Old.txt'];
fid=fopen(fileName,'r');
Param=textscan(fid,'%s');
fclose(fid);
nbDecad=str2double(Param{1}{1});
yearLast=str2double(Param{1}{2});
monthLast=str2double(Param{1}{3});
dayLast=str2double(Param{1}{4});
yearFirst=str2double(Param{1}{5});
monthFirst=str2double(Param{1}{6});
dayFirst=str2double(Param{1}{7});
yearStart=str2double(Param{1}{8});
yearStop=str2double(Param{1}{9});
decaddebp=str2double(Param{1}{10});
decadfinp=str2double(Param{1}{11});
quiet=str2double(Param{1}{17});

% Lecture PondMonitor_List
fileName=[pathOldParam 'PondMonitor_List.txt'];
fid=fopen(fileName,'r');
PondList=textscan(fid,'%d %d %d %d %d %d %d %s');
fclose(fid);
nbPond=size(PondList{1},1);

% Verification période d'intégration
decYearInt=0;
if decadfinp > 36
    decadfinp=decadfinp-36;
end
if decadfinp < decaddebp
    decYearInt=1;
end

% Creation champs Lat et Lon
for j=1:ysize
    LatC(j)=Lat0-PixelSD*(j-1);
end
Lat=single(repmat(LatC,xsize,1));
for i=1:xsize
    LonR(i)=Lon0+PixelSD*(i-1);
end
Lon=single(repmat(LonR',1,ysize));

% Table decadYearName
decadYear=0;
for month=1:12
    for day=1:10:21
        decadYear=decadYear+1;
        decadYearName{decadYear}=[num2str(month,'%02d') num2str(day,'%02d')];
    end
end

% Lecture masque+info ADM & GEO
for adm=0:admMax
    if adm <= 2
        shpName=['ADM_' num2str(adm)];
    end
    if adm >= 3 && adm <=5
        shpName=['GEO_' num2str(adm)];
    end
    if adm == 6
        shpName=['WATER'];
    end
    fid=fopen([pathM shpName '.img'],'r');
    maskAdm(adm+1,:,:)=int32(reshape(fread(fid,xsize*ysize,'int32'),[xsize ysize]));
    fclose(fid);
    fid=fopen([pathM shpName '.txt'],'r');
    scan=textscan(fid,'%d',1);
    nbAdm(adm+1)=scan{1};
    scan=textscan(fid,'%d %d %d %d %d %f %s');
    infoAdm{adm+1}=scan;
    fclose(fid);
end

% Lecture Shape.prj
shpName=[pathShpPrj 'Shape.prj'];
fid=fopen(shpName,'r');
scan=textscan(fid,'%s');
shapePrj=scan{1}{1};
fclose(fid);

% Lecture PixelSizeMap
fileName=[pathM 'PixelSizeMap.img'];
fid=fopen(fileName,'r');
PixelSizeMap=single(reshape(fread(fid,xsize*ysize,'single'),[xsize ysize]));
fclose(fid);

% Lecture GLC2000 pour eau permanente
% fileName=[pathM 'GLC2000.img'];
% fid=fopen(fileName,'r');
% GLC=uint8(reshape(fread(fid,xsize*ysize,'uint8'),[xsize ysize]));
% fclose(fid);

% Lecture MASK pour filtage eau
fileName=[pathR 'Mask.img'];
fid=fopen(fileName,'r');
Mask=uint8(reshape(fread(fid,xsize*ysize,'uint8'),[xsize ysize]));
fclose(fid);

% Relecture SWB
decadCount=0;
for year=yearFirst:yearLast
    month1=1;
    month2=12;
    if year == yearFirst
        month1=monthFirst;
    end
    if year == yearLast
        month2=monthLast;
    end
    for month=month1:month2
        day1=1;
        day2=21;
        if year == yearFirst && month == monthFirst
            day1=dayFirst;
        end
        if year == yearLast && month == monthLast
            day2=dayLast;
        end
        for day=day1:10:day2
            decadCount=decadCount+1;
            decadYear=(day-1)/10+1+(month-1)*3;
            
            if quiet~=1
                pc=round(100.*(decadCount-1)/(nbDecad-1));
                fprintf('\b\b\b\b\b\b\b\b\b\b\b%03d %% (3/4)',pc);
            end
            
            fileName=[pathF 'SWB_' num2str(year,'%04d') num2str(month,'%02d') num2str(day,'%02d') '.img'];
            fid=fopen(fileName,'r');
            if fid~=-1
                inUint8=uint8(reshape(fread(fid,xsize*ysize,'uint8'),[xsize ysize]));
                fclose(fid);
                SWBin=zeros(xsize,ysize,'single');
                SWBin(inUint8==70 | inUint8==150 | inUint8==220)=1.;
                SWBin(inUint8==251)=-9999.;
%                SWBin(Mask==0)=0.;   % Le Mask n'est calculée que sur la période d'intégration
%                SWBin(GLC==26)=1.;
            else
                SWBin=-9999.*ones(xsize,ysize,'single');
            end
            
            SWBout{admMax+2}{1}(decadCount)=year;
            SWBout{admMax+2}{2}(decadCount)=month;
            SWBout{admMax+2}{3}(decadCount)=day;
            SWBout{admMax+2}{4}(decadCount)=decadYear;
            
            for adm=0:6
                nbid=size(infoAdm{adm+1}{1},1);
                for k=1:nbid
                    id=infoAdm{adm+1}{1}(k);
                    imin=infoAdm{adm+1}{2}(k);
                    imax=infoAdm{adm+1}{3}(k);
                    jmin=infoAdm{adm+1}{4}(k);
                    jmax=infoAdm{adm+1}{5}(k);
                    
                    mask=reshape(maskAdm(adm+1,imin:imax,jmin:jmax),(imax-imin+1)*(jmax-jmin+1),1);
                    swb=reshape(SWBin(imin:imax,jmin:jmax),(imax-imin+1)*(jmax-jmin+1),1);
                    SizeMap=reshape(PixelSizeMap(imin:imax,jmin:jmax),(imax-imin+1)*(jmax-jmin+1),1);
                    ind=find(mask==id & swb~=-9999.);
                    if size(ind,1)~=0
                        SWBout{adm+1}{k}(decadCount)=sum(swb(ind).*SizeMap(ind))/100.; % /100 pour hectares vers km2
                    else
                        SWBout{adm+1}{k}(decadCount)=-9999.;
                    end
                end
            end
            
            for Pond=1:nbPond
                idPond=PondList{1}(Pond);
                imin=PondList{2}(Pond);
                imax=PondList{3}(Pond);
                jmin=PondList{4}(Pond);
                jmax=PondList{5}(Pond);
                adm=PondList{6}(Pond);
                admIdPond=PondList{7}(Pond);
                namePond=PondList{8}(Pond);
                
                SWBPondout{Pond}{1}(decadCount)=year;
                SWBPondout{Pond}{2}(decadCount)=month;
                SWBPondout{Pond}{3}(decadCount)=day;
                SWBPondout{Pond}{4}(decadCount)=decadYear;
    
                
                if admIdPond~=0 && adm>=0 && adm<=5
                    k=find(admIdPond == infoAdm{adm+1}{1}(:));                    
                    if ~isempty(k)
                        imin=max(imin,infoAdm{adm+1}{2}(k));
                        imax=min(imax,infoAdm{adm+1}{3}(k));
                        jmin=max(jmin,infoAdm{adm+1}{4}(k));
                        jmax=min(jmax,infoAdm{adm+1}{5}(k));
                        mask=(reshape(maskAdm(adm+1,imin:imax,jmin:jmax),(imax-imin+1)*(jmax-jmin+1),1) == admIdPond);
                    else
                        mask=false;
                    end                
                else
                    mask=(ones((imax-imin+1)*(jmax-jmin+1),1) == 1);
                end
                swb=reshape(SWBin(imin:imax,jmin:jmax),(imax-imin+1)*(jmax-jmin+1),1);
                SizeMap=reshape(PixelSizeMap(imin:imax,jmin:jmax),(imax-imin+1)*(jmax-jmin+1),1);
                LatP=reshape(Lat(imin:imax,jmin:jmax),(imax-imin+1)*(jmax-jmin+1),1);
                LonP=reshape(Lon(imin:imax,jmin:jmax),(imax-imin+1)*(jmax-jmin+1),1);
                
                ind=find(mask & swb~=-9999.);                
                if size(ind,1)~=0
                    SWBPondout{Pond}{7}(decadCount)=sum(swb(ind).*SizeMap(ind))/100.; % /100 pour hectares vers km2
                else
                    SWBPondout{Pond}{7}(decadCount)=-9999.;
                end
                
                ind=find(mask & swb==1.);
                if size(ind,1)~=0
                    SWBPondout{Pond}{5}(decadCount)=mean(LatP(ind));
                    SWBPondout{Pond}{6}(decadCount)=mean(LonP(ind));
                else
                    SWBPondout{Pond}{5}(decadCount)=-9999.;
                    SWBPondout{Pond}{6}(decadCount)=-9999.;
                end
            end
        end
    end
end


% Ecriture fichier .csv Pond
for Pond=1:nbPond
    ind=find(SWBPondout{Pond}{5} ~= -9999. & SWBPondout{Pond}{6} ~= -9999.);
    if size(ind,1)~=0
        LatP=mean(SWBPondout{Pond}{5}(ind));
        LonP=mean(SWBPondout{Pond}{6}(ind));
    else
        LatP=-9999.;
        LonP=-9999.;
    end
    ind=find(SWBPondout{Pond}{7} ~= -9999.);
    if size(ind,1)~=0
        MaxP=max(SWBPondout{Pond}{7}(ind));
        MinP=mean(SWBPondout{Pond}{7}(ind));
    else
        MaxP=0.;
        MinP=0.;
    end    
    
    fileName=[pathCsvIn 'Water_Pond_' num2str(PondList{1}(Pond),'%04d') '.csv'];
    fid=fopen(fileName,'w');
    fprintf(fid,[';Name:;' PondList{8}{Pond} ';\n']);
    fprintf(fid,[';Lat[ddN]:;' num2str(LatP,'%.3f') ';\n']);
    fprintf(fid,[';Lon[ddN]:;' num2str(LonP,'%.3f') ';\n']);
    fprintf(fid,[';Max[sqkm]:;' num2str(MaxP,'%.2f') ';\n']);
    fprintf(fid,[';Min[sqkm]:;' num2str(MinP,'%.2f') ';\n']);
    fprintf(fid,';\n');
    fprintf(fid,';\n');
    
    str=';Dekad;Date;Mean;Min;Max;Std;';
    for year=yearStart:yearStop
        str=[str num2str(year,'%04d') ';'];
    end
    str=[str '\n'];
    fprintf(fid,str);
    
    for decad=1:36
        month=floor((decad-1.)/3.)+1;
        day=(decad-3*(month-1)-1)*10+1;
        
        ind=find(SWBPondout{Pond}{4}(:)==decad);        
        PondMean=mean(SWBPondout{Pond}{7}(ind));
        PondStd=std(SWBPondout{Pond}{7}(ind));
        PondMin=min(SWBPondout{Pond}{7}(ind));
        PondMax=max(SWBPondout{Pond}{7}(ind));
        
        fprintf(fid,[';' num2str(decad,'%d') ';' num2str(day,'%02d') '/' num2str(month,'%02d') ';' num2str(PondMean,'%.2f') ';' num2str(PondMin,'%.2f') ';' num2str(PondMax,'%.2f') ';' num2str(PondStd,'%.2f')]);
        
        for year=yearStart:yearStop
            ind=find(SWBPondout{Pond}{4}(:)==decad & SWBPondout{Pond}{1}(:)==year);
            if ~isempty(ind)
                PondVal=num2str(SWBPondout{Pond}{7}(ind),'%.2f');
            else
                PondVal='';
            end
            fprintf(fid,[';' PondVal]);
        end
        fprintf(fid,[';\n']);
    end
    fprintf(fid,';\n');
    fprintf(fid,';\n');
    fprintf(fid,';\n');
    
    fprintf(fid,';Date;Area[sqkm];\n');
    for ind=1:numel(SWBPondout{Pond}{7})
        year=SWBPondout{Pond}{1}(ind);
        month=SWBPondout{Pond}{2}(ind);
        day=SWBPondout{Pond}{3}(ind);
        PondVal=num2str(SWBPondout{Pond}{7}(ind),'%.2f');
        fprintf(fid,[';' num2str(day,'%02d') '/' num2str(month,'%02d') '/' num2str(year,'%04d') ';' PondVal ';\n']);
    end
      
    fclose(fid);
end
    


for adm=0:6
    if quiet~=1
        pc=round(100.*adm/6.);
        fprintf('\b\b\b\b\b\b\b\b\b\b\b%03d %% (4/4)',pc);
    end    
    
% Calcul statistiques Profil SWB
    nbid=size(infoAdm{adm+1}{1},1);
    for k=1:nbid
                
        % Calcul Stat sum,max,min et sigma
        SWBval=SWBout{adm+1}{k}(1:size(SWBout{admMax+2}{1},2));
        SWBdecadYear=SWBout{admMax+2}{4}(1:size(SWBout{admMax+2}{1},2));
        SWBgood=find(SWBval~=-9999.);
        SWBval=SWBval(SWBgood);
        SWBdecadYear=SWBdecadYear(SWBgood);
        
        for decadYear=1:36
            f=find(SWBdecadYear==decadYear);
            if ~isempty(f)
                SWBmea{adm+1}{k}(decadYear)=mean(SWBval(f));
                SWBmax{adm+1}{k}(decadYear)=max(SWBval(f));
                SWBmin{adm+1}{k}(decadYear)=min(SWBval(f));
                SWBstd{adm+1}{k}(decadYear)=std(SWBval(f),1);
            else
                SWBmea{adm+1}{k}(decadYear)=-9999.;
                SWBmax{adm+1}{k}(decadYear)=-9999.;
                SWBmin{adm+1}{k}(decadYear)=-9999.;
                SWBstd{adm+1}{k}(decadYear)=-9999.;
            end
        end
    end



% Calcul statistiques précence d'eau sur Admin sur période intégration
    nbid=size(infoAdm{adm+1}{1},1);
    SWBadmmoy=zeros(nbid,1);
    SWBadmnb=zeros(nbid,1);
    clear SWBadmyear
    for year=yearStart:yearStop
        SWBsg=zeros(nbid,1);
        SWBnb=zeros(nbid,1);
        for decad=1:nbDecad
            if SWBout{admMax+2}{1}(decad)>=year && SWBout{admMax+2}{4}(decad)>=decaddebp && SWBout{admMax+2}{1}(decad)<=year+decYearInt && SWBout{admMax+2}{4}(decad)<=decadfinp
                for k=1:nbid
                    if SWBout{adm+1}{k}(decad) ~= -9999.
                        SWBsg(k)=SWBsg(k)+SWBout{adm+1}{k}(decad);
                        SWBnb(k)=SWBnb(k)+1;
                    end
                end
            end
        end        
        for k=1:nbid
            if SWBnb(k)~=0
                SWBadmyear(k,year-yearStart+1)=SWBsg(k)/SWBnb(k);
                SWBadmmoy(k)=SWBadmmoy(k)+SWBadmyear(k,year-yearStart+1);
                SWBadmnb(k)=SWBadmnb(k)+1;
            else
                SWBadmyear(k,year-yearStart+1)=-9999.;
            end
        end             
    end
    
    % Calcul moyenne et anomalie
    for k=1:nbid
        if SWBadmnb(k)~=0
            SWBadmmoy(k)=SWBadmmoy(k)/SWBadmnb(k);
            for year=yearStart:yearStop
                if SWBadmyear(k,year-yearStart+1)~=-9999. && SWBadmmoy(k)~=0.
                    SWBadmyear(k,year-yearStart+1)=100.*SWBadmyear(k,year-yearStart+1)/SWBadmmoy(k);
                else
                    SWBadmyear(k,year-yearStart+1)=-1.;
                end
            end
        else
            SWBadmmoy(k)=-1.;
        end
    end
                     
    % Ecriture CSV
    if adm <= 5
        if adm <= 2
            shpName=['ADM_' num2str(adm)];
        end
        if adm >= 3 && adm <=5
            shpName=['GEO_' num2str(adm)];
        end
        fid=fopen([pathCsvIn 'Water_' shpName '.csv'],'w');
        fprintf(fid,[';Source:;' shpName ';\n']);
        fprintf(fid,[';Nb_entities:;' num2str(nbid) ';\n']);
        fprintf(fid,[';Water Anomaly;[%%];\n']);
        fprintf(fid,[';\n']);
        
        str=[';Name;BIOHYDROID;Area[sqkm];Mean[sqkm];'];
        for year=yearStart:yearStop
            str=[str num2str(year) ';'];
        end
        str=[str '\n'];
        fprintf(fid,str);
        
        for k=1:nbid
            str=[';' infoAdm{adm+1}{7}{k} ';' num2str(infoAdm{adm+1}{1}(k)) ';' num2str(infoAdm{adm+1}{6}(k),'%.0f') ';' num2str(SWBadmmoy(k),'%.2f') ';'];
            for year=yearStart:yearStop
                str=[str num2str(SWBadmyear(k,year-yearStart+1),'%.2f') ';'];
            end
            str=[str '\n'];
            fprintf(fid,str);
        end
        fclose(fid);
    end








    % Ecriture Shapefile
    if adm <= 2
        shpName=['ADM_' num2str(adm)];
    end
    if adm >= 3 && adm <=5
        shpName=['GEO_' num2str(adm)];
    end
    if adm == 6
        shpName=['WATER'];
    end
    
    % test existence fichier
    fid=fopen([pathShpIn shpName '.shp']);
    if fid~=-1
        fclose(fid);
        
        % Lecture du shape
        ShapeIn = shaperead([pathShpIn shpName '.shp']);
        
        
%%%%%%% Partie annuelle %%%%%%%
        fid=fopen([pathCsvIn 'Access_' shpName '.csv']);
        if fid~=-1
            fclose(fid);
            
            % Lecture du fichier statistiques issu de HydroGenerator
            CsvIn = dlmread([pathCsvIn 'Access_' shpName '.csv'],';',5,2);
            
            % Nombre d'année
            firstyear=yearStart;
            lastyear=yearStop;
            
            % Extraction vecteur ID de ShapeIn et
            % initialisation des nouveaux
            % champs de sortie
            clear IDin
            for k=1:size(ShapeIn,1)
                IDin(k)=ShapeIn(k).IDBIOHYDRO;
                ShapeIn(k).('YEAR_FIRST')=firstyear;
                ShapeIn(k).('YEAR_LAST')=lastyear;
                ShapeIn(k).('A_MEAN')=-1;                
                for year=firstyear:lastyear
                    ShapeIn(k).(['A_ANO_' num2str(year)])=-1;
                end
                if fillUpTable==1
                    for year=lastyear+1:fillUpTableLastYear
                        ShapeIn(k).(['A_ANO_' num2str(year)])=fillUpTableValue;
                    end
                end
                ShapeIn(k).('W_MEAN')=-1;
                for year=firstyear:lastyear
                    ShapeIn(k).(['W_ANO_' num2str(year)])=-1;
                end
                if fillUpTable==1
                    for year=lastyear+1:fillUpTableLastYear
                        ShapeIn(k).(['W_ANO_' num2str(year)])=fillUpTableValue;
                    end
                end                
            end
            
            % Ecriture des nouveaux champs
            for m=1:size(CsvIn,1)
                ID=CsvIn(m,1);
                [l,k]=find(IDin==ID);
                kb=find(infoAdm{adm+1}{1}(:)==ID);
                if ~isempty(k)
                    ShapeIn(k).('A_MEAN')=CsvIn(m,3);
                    for year=firstyear:lastyear
                        ShapeIn(k).(['A_ANO_' num2str(year)])=CsvIn(m,year-firstyear+4);
                    end
                    ShapeIn(k).('W_MEAN')=SWBadmmoy(kb);
                    for year=firstyear:lastyear
                        ShapeIn(k).(['W_ANO_' num2str(year)])=SWBadmyear(kb,year-yearStart+1);
                    end                    
                end
            end
        end
        
%%%%%%% Partie Decadaire (SWB) %%%%%%%             
        % Initialisation table attributaire
        clear IDin
        for k=1:size(ShapeIn,1)
            IDin(k)=ShapeIn(k).IDBIOHYDRO;
            
            for decadYear=1:36
                ShapeIn(k).(['SWB_MEA_' num2str(decadYear,'%02d')])=-9999.;
            end
            for decadYear=1:36
                ShapeIn(k).(['SWB_MIN_' num2str(decadYear,'%02d')])=-9999.;
            end
            for decadYear=1:36
                ShapeIn(k).(['SWB_MAX_' num2str(decadYear,'%02d')])=-9999.;
            end            
            for decadYear=1:36
                ShapeIn(k).(['SWB_STD_' num2str(decadYear,'%02d')])=-9999.;
            end
            
            ShapeIn(k).('SWB_FIRST')=-9999;
            ShapeIn(k).('SWB_LAST')=-9999;
            
            for decad=1:size(SWBout{admMax+2}{1},2)
                ShapeIn(k).(['SWB_' num2str(SWBout{admMax+2}{1}(decad),'%04d') num2str((SWBout{admMax+2}{2}(decad)-1)*3+(SWBout{admMax+2}{3}(decad)-1)/10+1,'%02d')])=-9999.;
            end
            
            if fillUpTable==1
                for year=yearLast:fillUpTableLastYear
                    if year==yearLast
                        decad1=(monthLast-1)*3+(dayLast-1)/10+1+1;
                        decad2=36;
                    else
                        decad1=1;
                        decad2=36;
                    end
                    for decad=decad1:decad2
                        ShapeIn(k).(['SWB_' num2str(year,'%04d') num2str(decad,'%02d')])=fillUpTableValue;
                    end
                end
            end           
        end
        
        % Ecriture table attributaire
        nbid=size(infoAdm{adm+1}{1},1);
        for i=1:nbid
            ID=infoAdm{adm+1}{1}(i);
            [l,k]=find(IDin==ID);
            if ~isempty(k)
                k=k(1);
                for decadYear=1:36
                    ShapeIn(k).(['SWB_MEA_' num2str(decadYear,'%02d')])=double(SWBmea{adm+1}{i}(decadYear));
                end
                for decadYear=1:36
                    ShapeIn(k).(['SWB_MIN_' num2str(decadYear,'%02d')])=double(SWBmin{adm+1}{i}(decadYear));
                end
                for decadYear=1:36
                    ShapeIn(k).(['SWB_MAX_' num2str(decadYear,'%02d')])=double(SWBmax{adm+1}{i}(decadYear));
                end
                for decadYear=1:36
                    ShapeIn(k).(['SWB_STD_' num2str(decadYear,'%02d')])=double(SWBstd{adm+1}{i}(decadYear));
                end                
                
                ShapeIn(k).('SWB_FIRST')=yearFirst*100 + (monthFirst-1)*3 + (dayFirst-1)/10+1;
                ShapeIn(k).('SWB_LAST')=yearLast*100 + (monthLast-1)*3 + (dayLast-1)/10+1;
                
                for decad=1:size(SWBout{admMax+2}{1},2)
                    SWBval=double(SWBout{adm+1}{i}(decad));
                    ShapeIn(k).(['SWB_' num2str(SWBout{admMax+2}{1}(decad),'%04d') num2str((SWBout{admMax+2}{2}(decad)-1)*3+(SWBout{admMax+2}{3}(decad)-1)/10+1,'%02d')])=SWBval;
                end
            end
        end
        
        % Ecriture du fichier Shape de sortie
        shapewrite(ShapeIn,[pathShpOut 'HYDRO_' shpName '.shp']);
        % Ecriture du fichier de projection
        fileName=[pathShpOut 'HYDRO_' shpName '.prj'];
        fid=fopen(fileName,'w');
        fprintf(fid,shapePrj);
        fclose(fid);        
    end
end