%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BioGenerator_csv2shp             %
% v1.1 beta juin 2018              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


clear all

% Repertoires entrée et sortie
% cd(['..\..');
pathD=[pwd '\'];
% pathD=['E:\ACF3\BioHydroPond_v3_dev\'];
pathShpIn=[pathD 'Lib\Shape\Shape_Out\'];
pathCsvIn=[pathD 'Output\Biomass\Report\'];
pathShpOut=[pathD 'Output\Biomass\Shape\'];
pathShpPrj=[pathD 'Lib\Shape\Shape_Prj\'];
pathOldParam=[pathD 'Lib\Param_Old\'];
pathHideParam=[pathD 'Lib\Bin\'];
pathF=[pathD 'Data\Raw\Biomass\Ponderate\'];
pathM=[pathD 'Lib\Ancillary\Img\'];


% Paramètre taille image
xsize=7841;
ysize=3630;
sizeFile=xsize*ysize;

% Paramètre de remplissage des tables attributaire shapefile
fileName=[pathHideParam 'Common_Param.txt'];
fid=fopen(fileName,'r');
Param=textscan(fid,'%s');
fclose(fid);
fillUpTable=str2double(Param{1}{1});
fillUpTableyearLast=str2double(Param{1}{2});
fillUpTableValue=str2double(Param{1}{3});

% Declaration masque ADM & GEO
% maskAdm=int32([0:5 ; 1:xsize ; 1:ysize]);

% Lecture Param_Old pour récupération Decad_first & Decad_last
fileName=[pathOldParam 'BioGenerator_Param_Old.txt'];
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
Quiet=str2double(Param{1}{17});
Profil=str2double(Param{1}{18});

% Table decadYearName
decadYear=0;
for month=1:12
    for day=1:10:21
        decadYear=decadYear+1;
        decadYearName{decadYear}=[num2str(month,'%02d') num2str(day,'%02d')];
    end
end

% Lecture masque+info ADM & GEO
for adm=0:5
    if adm <= 2
        fileName=['ADM_' num2str(adm)];
    else
        fileName=['GEO_' num2str(adm)];
    end
    fid=fopen([pathM fileName '.img'],'r');
    maskAdm(adm+1,:,:)=int32(reshape(fread(fid,xsize*ysize,'int32'),[xsize ysize]));
    fclose(fid);
    fid=fopen([pathM fileName '.txt'],'r');
    scan=textscan(fid,'%d',1);
    nbAdm(adm+1)=scan{1};
    scan=textscan(fid,'%d %d %d %d %d %f %s');
    infoAdm{adm+1}=scan;
    fclose(fid);
end

% Lecture Shape.prj
fileName=[pathShpPrj 'Shape.prj'];
fid=fopen(fileName,'r');
scan=textscan(fid,'%s');
shapePrj=scan{1}{1};
fclose(fid);

%%%%%%%%%%%%%%%%%%%%%%%%
% Profil DMP Decadaire %
%%%%%%%%%%%%%%%%%%%%%%%%
if Profil==1
    % Lecture PixelSizeMap
    fileName=[pathM 'PixelSizeMap.img'];
    fid=fopen(fileName,'r');
    PixelSizeMap=single(reshape(fread(fid,xsize*ysize,'single'),[xsize ysize]));
    fclose(fid);
    
    % Relecture DMP_Filtre
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
                pc=round(100.*(decadCount-1)/(nbDecad-1));
                if Quiet~=1
                    fprintf('\b\b\b%03d %% (3/3)\b\b\b\b\b\b\b\b',pc);
                end
                
                fileName=[pathF 'DMP_' num2str(year,'%04d') num2str(month,'%02d') num2str(day,'%02d') '.img'];
                fid=fopen(fileName,'r');
                if fid~=-1
                    DMPFin=single(reshape(fread(fid,xsize*ysize,'single'),[xsize ysize]));
                    fclose(fid);
                else
                    DMPFin=single(-9999.*ones(xsize,ysize,'single'));
                end
                
                DMPout{7}{1}(decadCount)=year;
                DMPout{7}{2}(decadCount)=month;
                DMPout{7}{3}(decadCount)=day;
                DMPout{7}{4}(decadCount)=decadYear;
                
                for adm=0:5
                    nbid=size(infoAdm{adm+1}{1},1);
                    for k=1:nbid
                        id=infoAdm{adm+1}{1}(k);
                        imin=infoAdm{adm+1}{2}(k);
                        imax=infoAdm{adm+1}{3}(k);
                        jmin=infoAdm{adm+1}{4}(k);
                        jmax=infoAdm{adm+1}{5}(k);
                        
                        mask=reshape(maskAdm(adm+1,imin:imax,jmin:jmax),(imax-imin+1)*(jmax-jmin+1),1);
                        dmp=reshape(DMPFin(imin:imax,jmin:jmax),(imax-imin+1)*(jmax-jmin+1),1);
                        SizeMap=reshape(PixelSizeMap(imin:imax,jmin:jmax),(imax-imin+1)*(jmax-jmin+1),1);
                        ind=find(mask==id & dmp~=-9999.);
                        if size(ind,1)~=0
                            DMPout{adm+1}{k}(decadCount)=sum(dmp(ind).*SizeMap(ind))/sum(SizeMap(ind));
                        else
                            DMPout{adm+1}{k}(decadCount)=-9999.;
                        end
                    end
                end
            end
        end
    end
    
    
    
    % Calcul statistiques
    for adm=0:5
        nbid=size(infoAdm{adm+1}{1},1);
        for k=1:nbid
            
            
            % Calcul Stat sum,max,min et sigma
            DMPval=DMPout{adm+1}{k}(1:size(DMPout{7}{1},2));
            DMPdecadYear=DMPout{7}{4}(1:size(DMPout{7}{1},2));
            DMPgood=find(DMPval~=-9999.);
            DMPval=DMPval(DMPgood);
            DMPdecadYear=DMPdecadYear(DMPgood);
            
            for decadYear=1:36
                f=find(DMPdecadYear==decadYear);
                if ~isempty(f)
                    DMPmea{adm+1}{k}(decadYear)=mean(DMPval(f));
                    DMPmax{adm+1}{k}(decadYear)=max(DMPval(f));
                    DMPmin{adm+1}{k}(decadYear)=min(DMPval(f));
                    DMPstd{adm+1}{k}(decadYear)=std(DMPval(f),1);
                else
                    DMPmea{adm+1}{k}(decadYear)=-9999.;
                    DMPmax{adm+1}{k}(decadYear)=-9999.;
                    DMPmin{adm+1}{k}(decadYear)=-9999.;
                    DMPstd{adm+1}{k}(decadYear)=-9999.;
                end
            end
        end
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ecriture fichier Shapefile %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for adm=0:5
    if adm <= 2
        shpName=['ADM_' num2str(adm)];
    else
        shpName=['GEO_' num2str(adm)];
    end
    
    % test existence fichier
    fid=fopen([pathShpIn shpName '.shp']);
    if fid~=-1
        fclose(fid);
        
        % Lecture du shape
        ShapeIn = shaperead([pathShpIn shpName '.shp']);
        
        
        
        %%%%%%% Partie ANNUELLE (BIO & VI) %%%%%%%
        % Anomalie sur Biomass
        % Test existence fichier
        fid=fopen([pathCsvIn 'Biomass_' shpName '.csv']);
        if fid~=-1
            fclose(fid);
            
            % Lecture du fichier statistiques issu de BioGenerator
            CsvInBio = dlmread([pathCsvIn 'Biomass_' shpName '.csv'],';',5,2);
            CsvInVI = dlmread([pathCsvIn 'VI_' shpName '.csv'],';',5,2);
            
            % Extraction vecteur ID de ShapeIn et
            % initialisation des nouveaux
            % champs de sortie
            clear IDin
            for k=1:size(ShapeIn,1)
                IDin(k)=ShapeIn(k).IDBIOHYDRO;
                ShapeIn(k).('YEAR_FIRST')=yearFirst;
                ShapeIn(k).('YEAR_LAST')=yearLast;
                ShapeIn(k).('BIO_MEAN')=-9999.;
                for year=yearFirst:yearLast
                    ShapeIn(k).(['BIO_' num2str(year)])=-9999.;
                end
                if fillUpTable==1
                    for year=yearLast+1:fillUpTableyearLast
                        ShapeIn(k).(['BIO_' num2str(year)])=fillUpTableValue;
                    end
                end
                for year=yearFirst:yearLast
                    ShapeIn(k).(['VI_' num2str(year)])=-9999.;
                end
                if fillUpTable==1
                    for year=yearLast+1:fillUpTableyearLast
                        ShapeIn(k).(['VI_' num2str(year)])=fillUpTableValue;
                    end
                end
                %         ShapeIn1(k).('R2')=-1;
                %         ShapeIn1(k).('Trend')=-1;
                %         ShapeIn1(k).('Sigma')=-1;
            end
            
            % Ecriture des nouveaux champs
            for m=1:size(CsvInBio,1)
                ID=CsvInBio(m,1);
                [l,k]=find(IDin==ID);
                if ~isempty(k)
                    ShapeIn(k).('BIO_MEAN')=CsvInBio(m,3);
                    for year=yearFirst:yearLast
                        ShapeIn(k).(['BIO_' num2str(year)])=CsvInBio(m,year-yearFirst+4);
                        ShapeIn(k).(['VI_' num2str(year)])=CsvInVI(m,year-yearFirst+3);
                    end
                    %             ShapeIn(k).('R2')=CsvIn(m,yearLast-yearFirst+4+1);
                    %             ShapeIn(k).('Trend')=CsvIn(m,yearLast-yearFirst+4+2);
                    %             ShapeIn(k).('Sigma')=CsvIn(m,yearLast-yearFirst+4+3);
                end
            end
        end
        
        
        %%%%%%% Partie Decadaire (DMP) %%%%%%%
        if Profil==1
            % Initialisation table attributaire
            clear IDin
            for k=1:size(ShapeIn,1)
                IDin(k)=ShapeIn(k).IDBIOHYDRO;
                
                for decadYear=1:36
                    ShapeIn(k).(['DMP_MEA_' num2str(decadYear,'%02d')])=-9999.;
                end
                for decadYear=1:36
                    ShapeIn(k).(['DMP_MIN_' num2str(decadYear,'%02d')])=-9999.;
                end
                for decadYear=1:36
                    ShapeIn(k).(['DMP_MAX_' num2str(decadYear,'%02d')])=-9999.;
                end
                for decadYear=1:36
                    ShapeIn(k).(['DMP_STD_' num2str(decadYear,'%02d')])=-9999.;
                end
                
                ShapeIn(k).('DMP_FIRST')=-9999;
                ShapeIn(k).('DMP_LAST')=-9999;
                
                for decad=1:size(DMPout{7}{1},2)
                    ShapeIn(k).(['DMP_' num2str(DMPout{7}{1}(decad),'%04d') num2str((DMPout{7}{2}(decad)-1)*3+(DMPout{7}{3}(decad)-1)/10+1,'%02d')])=-9999.;
                end
                
                if fillUpTable==1
                    for year=yearLast:fillUpTableyearLast
                        if year==yearLast
                            decad1=(monthLast-1)*3+(dayLast-1)/10+1+1;
                            decad2=36;
                        else
                            decad1=1;
                            decad2=36;
                        end
                        for decad=decad1:decad2
                            ShapeIn(k).(['DMP_' num2str(year,'%04d') num2str(decad,'%02d')])=fillUpTableValue;
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
                        ShapeIn(k).(['DMP_MEA_' num2str(decadYear,'%02d')])=double(DMPmea{adm+1}{i}(decadYear));
                    end
                    for decadYear=1:36
                        ShapeIn(k).(['DMP_MIN_' num2str(decadYear,'%02d')])=double(DMPmin{adm+1}{i}(decadYear));
                    end
                    for decadYear=1:36
                        ShapeIn(k).(['DMP_MAX_' num2str(decadYear,'%02d')])=double(DMPmax{adm+1}{i}(decadYear));
                    end
                    for decadYear=1:36
                        ShapeIn(k).(['DMP_STD_' num2str(decadYear,'%02d')])=double(DMPstd{adm+1}{i}(decadYear));
                    end
                    
                    ShapeIn(k).('DMP_FIRST')=yearFirst*100 + (monthFirst-1)*3 + (dayFirst-1)/10+1;
                    ShapeIn(k).('DMP_LAST')=yearLast*100 + (monthLast-1)*3 + (dayFirst-1)/10+1;
                    
                    for decad=1:size(DMPout{7}{1},2)
                        ShapeIn(k).(['DMP_' num2str(DMPout{7}{1}(decad),'%04d') num2str((DMPout{7}{2}(decad)-1)*3+(DMPout{7}{3}(decad)-1)/10+1,'%02d')])=double(DMPout{adm+1}{i}(decad));
                    end
                end
            end
        end
        
        % Ecriture du fichier Shape de sortie
        shapewrite(ShapeIn,[pathShpOut 'BIO_' shpName '.shp']);
        % Ecriture du fichier de projection
        fileName=[pathShpOut 'BIO_' shpName '.prj'];
        fid=fopen(fileName,'w');
        fprintf(fid,shapePrj);
        fclose(fid);
    end
end
    
    
    
