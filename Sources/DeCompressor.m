%cccccccccccccccccccccccccccccccccccccccccccccc
% Erwann Fillol, Action Contre la Faim (2018) c
%cccccccccccccccccccccccccccccccccccccccccccccc


clear all

% warning off MATLAB:imagesci:Tiff:libraryWarning
warning('off','all');

% Repertoires entrée et sortie 
pathD=[pwd '\'];
%pathD=['E:\ACF3\BioHydroPond_v3_dev\'];
pathDL=[pathD 'Data\Download\'];
pathIn=[pathD 'Data\Raw\In\'];
pathT=[pathD 'Lib\Tmp\'];
pathL=[pathD 'Lib\Log\'];
pathB=[pathD 'Lib\Bin\'];
pathShpIn=[pathD 'Shape\'];
pathShpCp=[pathD 'Lib\Shape\Shape_Copy\'];
pathShpRs=[pathD 'Lib\Ancillary\Img\'];
pathShpOut=[pathD 'Lib\Shape\Shape_Out\'];
pathShpPrj=[pathD 'Lib\Shape\Shape_Prj\'];
pathParam=[pathD 'Param\'];
pathLogin=[pathD 'Lib\Login\'];

% Parametre
fileName=[pathParam 'DeCompressor_Param.txt'];
fid=fopen(fileName);
Param=textscan(fid,'%s');
fclose(fid);
for line=2:size(Param{1},1)
    rline=Param{1}(line,1);
    if strcmp(rline{1},'Forcage')
        force=(str2double(Param{1}(line-1,1)) == 1);
    end
%     if strcmp(rline{1},'Mode')
%         quiet=str2double(Param{1}(line-1,1));
%     end    
end
quiet=0;

% Mode autorun
fileName=[pathT 'AutoRun_Mode.txt'];
fid=fopen(fileName);
if fid~=-1
    d=textscan(fid,'%d');
    fclose(fid);
    autorun=d{1};
    [status,cmdout]=system(['del ' pathT 'AutoRun_Mode.txt']);
else
    autorun=0;
end
    

if quiet~=1
    if autorun~=1
        %system('cls');
        clc
    end
    fprintf (' **********************************\n');
    fprintf (' *        DeCompressor (v2.1)     *\n');
    fprintf (' * Action Contre la Faim (ACF-E)  *\n');
    fprintf (' *        Erwann Fillol (2018)    *\n');
    fprintf (' *        erwann.fillol@gmail.com *\n');
    fprintf (' **********************************\n');
    fprintf ('\n');
    fprintf (' Initialisation    : ');
end

% Date et heure
clockS=clock;



passPhrase='walaran';
% stringIn='ACF_Pass';
% for i=1:size(stringIn,2)
%     charIn=double(stringIn(i));
%     j=mod((i-1),size(passPhrase,2))+1;
%     charOut=(charIn-31)+double(passPhrase(j));
%     while charOut>95
%         charOut=charOut-95;
%     end
%     charOut=charOut+31;
%     stringIn(i)=char(charOut);
% end
% disp(stringIn);

% Lecture de sortie Flag
fileName=[pathT 'DeCompressor_Flag.txt'];
fid=fopen(fileName,'r');
if fid~=-1 
    sFlag=fscanf(fid,'%d');
    fclose(fid);
else
    sFlag=zeros(4);
end

% Info ftp
fileName=[pathLogin 'VITO_FTP.txt'];
fid=fopen(fileName);
infoFTP=textscan(fid,'%s');
fclose(fid);
c=infoFTP{1}(1);
hostFTP=c{1};
c=infoFTP{1}(2);
usernameFTP=c{1};
c=infoFTP{1}(3);
strIn=c{1};
for i=1:size(strIn,2)
    charOut=double(strIn(i));
    j=mod((i-1),size(passPhrase,2))+1;
    charIn=(charOut-31)-double(passPhrase(j));
    while charIn<1
        charIn=charIn+95;
    end
    charIn=charIn+31;
    strIn(i)=char(charIn);
end
passwordFTP=strIn;

% Info DataPool
fileName=[pathLogin 'Copernicus_Datapool.txt'];
fid=fopen(fileName);
infoDP=textscan(fid,'%s');
fclose(fid);
c=infoDP{1}(1);
adressDataPool=c{1};
c=infoDP{1}(2);
usernameDataPool=c{1};
c=infoDP{1}(3);
strIn=c{1};
for i=1:size(strIn,2)
    charOut=double(strIn(i));
    j=mod((i-1),size(passPhrase,2))+1;
    charIn=(charOut-31)-double(passPhrase(j));
    while charIn<1
        charIn=charIn+95;
    end
    charIn=charIn+31;
    strIn(i)=char(charIn);
end
passwordDataPool=strIn;

% Mois DataPool
monthName=cellstr([
    'January  ';
    'February ';
    'March    ';
    'April    ';
    'May      ';
    'June     ';
    'July     ';
    'August   ';
    'September';
    'October  ';
    'November ';
    'December ']);

% Info départ FTP & DataPool
yearMin=1998;
monthMin=1;
dayMin=1;

% Info nombre essais téléchargement
nbtry=4;

% TIFF SWB et Paramètres découpage
SWBtiffSizeX=10081;
SWBtiffSizeY=8961;
xmin=1345;
ymin=1415;
xsize=7841;
ysize=3630;
sizeFile=xsize*ysize;

% Fenetre SHAPE
overSample=1;
latUL = 27.37946430;
lonUL = -18.00446430;
res =  8.9285714300e-003/overSample;
M = ysize;
N = xsize;
latLR = latUL-(M-1)*res;
lonLR = lonUL+(N-1)*res;
latlim = [latLR latUL];
lonlim = [lonUL lonLR];

% Fichier Shape .prj
filename=[pathShpPrj 'Shape.prj'];
fid=fopen(filename,'r');
prj=textscan(fid,'%s_n');
fclose(fid);

% Init
sizeOldFilename=0;
nbUnzip(1:3)=0;

% Fichier log
fileNameLog=[pathL 'DeCompressor_Log.txt'];
fidLog=fopen(fileNameLog,'a');
fprintf(fidLog,'\n');
fprintf(fidLog,'%02d/%02d/%04d %02d:%02d:%02d\n',clockS(3),clockS(2),clockS(1),clockS(4),clockS(5),floor(clockS(6)));

% Lecture PixelSizeMap
filename=[pathShpRs 'PixelSizeMap.img'];
fid=fopen(filename,'r');
PixelSizeMap=single((reshape(fread(fid,xsize*ysize,'single'),[xsize ysize]))');
fclose(fid);

if quiet~=1
    fprintf('OK\n');
    fprintf('\n');
end



%%%%%%%%%%%%%%%%%%%%%%%%
%      SHAPE           %
%%%%%%%%%%%%%%%%%%%%%%%%
nbShpDetected=0;
sizeOldFilename=0;

%Fichier Log
fprintf(fidLog,'**********\n');
fprintf(fidLog,'Shapefile Debut\n');

for adm=0:7
    if adm <= 2
        shpName=['ADM_' num2str(adm)];
    end
    if adm > 2 && adm <=5
        shpName=['GEO_' num2str(adm)];
    end
    if adm == 6
        shpName=['WATER'];
    end 
    if adm == 7
        shpName=['MASK'];
    end      
    
    ShapeIn = shaperead([pathShpIn shpName '.shp']);
    if exist([pathShpCp shpName '.shp'],'file')
        ShapeCp = shaperead([pathShpCp shpName '.shp']);
    else
        ShapeCp = 0;
    end
    
    if ~isequalwithequalnans(ShapeIn,ShapeCp) || force
        if quiet~=1
            for ind=1:sizeOldFilename
                fprintf('\b');
            end
            sizeOldFilename=fprintf(' Nouveau Shape     : %s (   %%)',shpName);
        end
        
        fprintf(fidLog,['Shape converti : ' shpName '\n']);

        nbShpDetected=nbShpDetected+1;
        
        nbField=size(ShapeIn,1);
        
        ShapeCp=ShapeIn;
        
% Creation champs valeur ids
        clear ids
        for k=1:nbField
            ids(k)=k;
        end


% Creation champs valeur names : Récupération du champs NAME s'il existe,
% conversion en str, comblage des valeurs manquantes
        clear names
        if isfield(ShapeIn,'NAME')
            for k=1:nbField
                if isa(ShapeIn(k).NAME,'char')
                    names(k).n=ShapeIn(k).NAME;
                else
                    name=num2str(ShapeIn(k).NAME);
                    if ~isempty(name)
                        names(k).n=name;
                    else
                        names(k).n=['Name_' num2str(ids(k))];
                    end
                end
            end
            ShapeIn=rmfield(ShapeIn,'NAME');
        else
            for k=1:nbField
                names(k).n=['Name_' num2str(ids(k))];
            end
        end        
              
% Creation du Raster et fichier annexe
%         R = georasterref('RasterSize', [M N], ...
%         'ColumnsStartFrom', 'north', 'Latlim', latlim, ...
%         'Lonlim', lonlim);
%         Zout = zeros(R.RasterSize);
        Zout=int32(zeros(M,N));
        nbFieldO=0;
        idmax=0;
        for k=1:nbField
            pc=round(100.*(k-1)/nbField);
            if quiet~=1
                fprintf('\b\b\b\b\b\b(%03d%%)',pc);
            end
            lat=ShapeIn(k).Y;
            lon=ShapeIn(k).X;

            % Ré-écriture champs IDBIOHYDRO et NAME
            id=ids(k);
            name=names(k).n;
            ShapeIn(k).IDBIOHYDRO=id;
            ShapeIn(k).NAME=name;
            ShapeIn(k).AREA=0.;
            
            ind=find(~isnan(lat) & ~isnan(lon));
            if size(ind,2)>0
%                 lat=lat(ind);
%                 lon=lon(ind);
                
%           disp([int2str(z) '/' int2str(size(ShapeIn,1)) ' id:' int2str(id) ' nb:' int2str(size(lat,2))])
                
%             Z=vec2mtx(lat,lon,Zout,R,'filled');
                decLon=+res/2;
                decLat=+res/2;
                
                imin=floor((min(lon)-(lonUL+decLon))/res+1);
                lonmin=(lonUL+decLon)+(imin-1)*res;
                imax=ceil((max(lon)-(lonUL+decLon))/res+1);
                lonmax=(lonUL+decLon)+(imax-1)*res;
                
                jmin=floor(((latUL+decLat)-max(lat))/res+1);
                latmax=(latUL+decLat)-(jmin-1)*res;
                jmax=ceil(((latUL+decLat)-min(lat))/res+1);
                latmin=(latUL+decLat)-(jmax-1)*res;
                
%             fprintf('%d %f %f %f %f %d %d %d %d\n',id,lonmin,lonmax,latmin,latmax,imin,imax,jmin,jmax);
                
                
                Z=vec2mtx(lat,lon,1/res,[latmin,latmax],[lonmin,lonmax],'filled');
                [i,j]=find(Z~=2);
                j=j+imin;
                i=jmax-i;
                clear gind
                gind=find(i>=1 & i<=M & j>=1 & j<=N);
  
                if ~isempty(gind)
                    jb=j(gind);
                    ib=i(gind);
                    Zout(sub2ind(size(Zout),ib,jb))=int32(id);
                    nbFieldO=nbFieldO+1;
                    idField(nbFieldO)=id;
                    nameField{nbFieldO}=name;
                end
                
                clear Z j i gind ib jb
            end
        end
        clear stock
        nbFieldOO=0;
        for k=1:nbFieldO
            pc=round(100.*(k-1)/nbFieldO);
            if quiet~=1
                fprintf('\b\b\b\b\b\b(%03d%%)',pc);
            end
            id=idField(k);
            name=nameField{k};
            name=strrep(name,' ','_');
            ind=find(Zout==id);
            if ~isempty(ind)
                [i j]=ind2sub([M N],ind);
                nbFieldOO=nbFieldOO+1;
                
                stock(nbFieldOO,1)=id;
                stock(nbFieldOO,2)=min(j);
                stock(nbFieldOO,3)=max(j);
                stock(nbFieldOO,4)=min(i);
                stock(nbFieldOO,5)=max(i);
                
                area=double(sum(PixelSizeMap(ind)))/100.;
                stock(nbFieldOO,6)=area;
                ShapeIn(k).AREA=area;
                
                stockName{nbFieldOO}=name;
            end
            clear i j
        end
        
        % Sauvegarde fichier Shape Modifié
        shapewrite(ShapeIn,[pathShpOut shpName '.shp']);
        % Fichier .prj
        fid=fopen([pathShpOut shpName '.prj'],'w');
        fprintf(fid,prj{1}{1});
        fclose(fid);

        [stock,sortind]=sortrows(stock,1);

        fid=fopen([pathShpRs shpName '.txt'],'w');
        fprintf(fid,'%d\n',nbFieldOO);
        for k=1:nbFieldOO
            fprintf(fid,'%d %d %d %d %d %f %s\n',stock(k,1),stock(k,2),stock(k,3),stock(k,4),stock(k,5),stock(k,6),stockName{sortind(k)});
        end
        fclose(fid);

        fid=fopen([pathShpRs shpName '.img'],'w');
        fwrite(fid,Zout','int32');
        fclose(fid);
        clear Zout

        fid=fopen([pathShpRs shpName '.hdr'],'w');
        fprintf(fid,'ENVI \n');
        fprintf(fid,'samples = %d \n',xsize);
        fprintf(fid,'lines = %d \n',ysize);
        fprintf(fid,'bands = 1 \n');
        fprintf(fid,'file type = ENVI standard \n');
        fprintf(fid,'data type = 3 \n');
        fprintf(fid,'map info = {Geographic Lat/Lon, 1.0000, 1.0000, %12.8f, %12.8f, %.10e, %.10e, WGS-84, units=Degrees} \n',lonUL,latUL,res,res);
        fclose(fid);
        
        % Copie du fichier shape pour comparaison future
        copyfile([pathShpIn shpName '.shp'],[pathShpCp shpName '.shp']);
        copyfile([pathShpIn shpName '.dbf'],[pathShpCp shpName '.dbf']);
        copyfile([pathShpIn shpName '.shx'],[pathShpCp shpName '.shx']);
    end
end
if quiet~=1
    for ind=1:sizeOldFilename
        fprintf('\b');
    end
end
sizeOldFilename=0;
fprintf(fidLog,'Shapefile Fin\n');


%%%%%%%%%%%%%%%%%%%%%%%%
%      DMP & NDVI      %
%%%%%%%%%%%%%%%%%%%%%%%%
for varind=1:2
    if varind==1
        variable='DMP';
        sizeFileFac=2;
    end
    if varind==2
        variable='NDVI';
        sizeFileFac=1;
    end    
    
    %Fichier Log
    fprintf(fidLog,'**********\n');
    fprintf(fidLog,[variable ' Debut\n']);
    
    %Initialisation yearMax, monthMax, dayMax
    yearMax=yearMin;
    monthMax=monthMin;
    dayMax=dayMin;
    
    %Dir sur Download et In
    dDL=dir([pathDL variable '\']);
    dIn=dir([pathIn variable '\']);
    cd([pathDL variable '\']);
    
    %Dir du Download, récupération yearMax, monthMax, dayMax
    for iDL=3:size(dDL,1)
        nameDL=dDL(iDL,1).name;
        [token,remain]=strtok(nameDL,'_');
        [toke1,remain]=strtok(nameDL,'.');
        if strcmp(token,variable) && strcmp(remain,'.zip')
            [token,remain]=strtok(fliplr(nameDL),'_');
            [token,remain]=strtok(fliplr(token),'.');
            yearDL=str2double(token(1:4));
            monthDL=str2double(token(5:6));
            dayDL=str2double(token(7:8));
            if yearDL > yearMax;
                yearMax=yearDL;
                monthMax=monthDL;
                dayMax=dayDL;
            end
            if yearDL == yearMax && monthDL > monthMax
                monthMax=monthDL;
                dayMax=dayDL;
            end
            if yearDL == yearMax && monthDL == monthMax && dayDL > dayMax
                dayMax=dayDL;
            end
        end
    end
    
    %Dir du FTP, récupération yearMax, monthMax, dayMax
    ftpfail=false;
    try
        fFTP=ftp(hostFTP,usernameFTP,passwordFTP);
        sfFTP=struct(fFTP);  
        sfFTP.jobject.enterLocalPassiveMode();
        cd(fFTP,['ACF/' variable '/']);
        dFTP=dir(fFTP);
        close(fFTP);
    catch exception
        ftpfail=true;
    end
    if ~ftpfail
        for iFTP=1:size(dFTP,1)
            nameFTP=dFTP(iFTP,1).name;
            sizeFTP=dFTP(iFTP,1).bytes;
            dateFTP=dFTP(iFTP,1).datenum;
            [token,remain]=strtok(fliplr(nameFTP),'_');
            [token,remain]=strtok(fliplr(token),'.');
            yearFTP=str2double(token(1:4));
            monthFTP=str2double(token(5:6));
            dayFTP=str2double(token(7:8));
            if yearFTP > yearMax;
                yearMax=yearFTP;
                monthMax=monthFTP;
                dayMax=dayFTP;
            end
            if yearFTP == yearMax && monthFTP > monthMax
                monthMax=monthFTP;
                dayMax=dayFTP;
            end
            if yearFTP == yearMax && monthFTP == monthMax && dayFTP > dayMax
                dayMax=dayFTP;
            end
        end
    else
        dFTP(1).name='0';
        dFTP(1).bytes=0;
        dFTP(1).datenum=0;
    end
    
    %Boucle sur l'ensemble de décades
    for year=yearMin:yearMax
        if year==yearMin
            month1=monthMin;
        else
            month1=1;
        end
        if year==yearMax
            month2=monthMax;
        else
            month2=12;
        end
        for month=month1:month2
            if year==yearMin && month==monthMin
                day1=dayMin;
            else
                day1=1;
            end
            if year==yearMax && month==monthMax
                day2=dayMax;
            else
                day2=21;
            end
            for day=day1:10:day2
                %Construction nom du fichier sans extension
                fileName=[variable '_' num2str(year,'%04d') num2str(month,'%02d') num2str(day,'%02d')];
                
                %Repérage de ce qui existe FTP et taille
                existFTP=false;
                sizeFTP=-1;
                dateFTP=-1;
                for iFTP=1:size(dFTP,1)
                    nameFTP=dFTP(iFTP,1).name;
                    if strcmp(nameFTP,[fileName '.zip']);
                        sizeFTP=dFTP(iFTP,1).bytes;
                        dateFTP=dFTP(iFTP,1).datenum;
                        if size(sizeFTP)>0
                            if sizeFTP>0
                                existFTP=true;
                            end
                        end
                    end
                end
                
                %Repérage de ce qui existe Download et taille
                %Effacement si taille différente du FTP
                existDL=false;
                sizeDL=-1;
                for iDL=1:size(dDL,1)
                    nameDL=dDL(iDL,1).name;
                    if strcmp(nameDL,[fileName '.zip']);
                        sizeDL=dDL(iDL,1).bytes;
                        if existFTP && sizeFTP ~= sizeDL
                            delete([pathDL variable '\' fileName '.zip']);
                        else
                            existDL=true;
                        end
                    end
                end                    

                %Repérage de ce qui existe In et taille
                %Effacement si taille non conforme
                existIn=false;
                sizeIn=-1;
                for iIn=1:size(dIn,1)
                    nameIn=dIn(iIn,1).name;
                    if strcmp(nameIn,[fileName '.img']);
                        sizeIn=dIn(iIn,1).bytes;
                        if sizeIn~=sizeFileFac*sizeFile
                            delete([pathIn variable '\' fileName '.img']);
                            delete([pathIn variable '\' fileName '.hdr']);
                        else
                            existIn=true;
                        end
                    end
                end
                
                reforce=force;
                for tryget=1:nbtry
                    %Téléchargement et verification taille
                    if existFTP && (~existDL || reforce)
                        if quiet~=1
                            for ind=1:sizeOldFilename
                                fprintf('\b');
                            end
                            sizeOldFilename=fprintf(' Telechargement    : %s          ',fileName);
                        end
                        ftpfail=false;
                        try
                            fFTP=ftp(hostFTP,usernameFTP,passwordFTP);
                            sfFTP=struct(fFTP);
                            sfFTP.jobject.enterLocalPassiveMode();
                            cd(fFTP,['ACF/' variable '/']);
                            mget(fFTP,[fileName '.zip']);
                            close(fFTP);
                        catch exception
                            ftpfail=true;
                        end
                        
                        if ~ftpfail
                            dDL=dir([pathDL variable '\']);
                            existDL=false;
                            sizeDL=-1;
                            for iDL=1:size(dDL,1)
                                nameDL=dDL(iDL,1).name;
                                if strcmp(nameDL,[fileName '.zip']);
                                    sizeDL=dDL(iDL,1).bytes;
                                    if existFTP && sizeFTP ~= sizeDL
                                        delete([pathDL variable '\' fileName '.zip']);
                                        dDL=dir([pathDL variable '\']);
                                        fprintf(fidLog,[fileName ' telechargement echec tentative ' num2str(tryget) '\n']);
                                    else
                                        existDL=true;
                                        fprintf(fidLog,[fileName ' telechargement reussi tentative ' num2str(tryget) '\n']);
                                        if existIn
                                            reforce=true;
                                        end
                                    end
                                end
                            end
                        end
                    end
                    
                    %Decompression et verification taille
                    if existDL && (~existIn || reforce)
                        if quiet~=1
                            for ind=1:sizeOldFilename
                                fprintf('\b');
                            end
                            sizeOldFilename=fprintf(' Decompression     : %s          ',fileName);
                        end
                        zipfail=false;
                        try
                            unzip([pathDL variable '\' fileName '.zip'],pathT);
                        catch exception
                            zipfail=true;
                        end
                        if ~zipfail
                            dTmp=dir(pathT);
                            for iTmp=1:size(dTmp,1)
                                nameTmp=dTmp(iTmp).name;
                                sizeTmp=dTmp(iTmp).bytes;
                                if strcmp(nameTmp,[fileName '.img']) && sizeTmp == sizeFileFac*sizeFile
                                    movefile([pathT fileName '.img'],[pathIn variable '\']);
                                    movefile([pathT fileName '.hdr'],[pathIn variable '\']);
                                    delete([pathT fileName '.doc']);
                                    existIn=true;
                                    fprintf(fidLog,[fileName ' decompression reussie tentative ' num2str(tryget) '\n']);
                                    nbUnzip(varind)=nbUnzip(varind)+1;
                                    reforce=false;
                                end
                                if strcmp(nameTmp,[fileName '.img']) && sizeTmp ~= sizeFileFac*sizeFile
                                    delete([pathT fileName '.img']);
                                    delete([pathT fileName '.hdr']);
                                    delete([pathT fileName '.doc']);
                                    delete([pathDL variable '\' fileName '.zip']);
                                    existIn=false;
                                    existDL=false;
                                    fprintf(fidLog,[fileName ' dimensions incoherentes tentative ' num2str(tryget) '\n']);
                                end                                
                            end
                        else
                            delete([pathDL variable '\' fileName '.zip']);
                            existDL=false;
                            fprintf(fidLog,[fileName ' decompression echec tentative ' num2str(tryget) '\n']);
                        end
                    end
                end
            end
        end
    end
    fprintf(fidLog,[variable ' Fin\n']);
end
                 




%%%%%%%%%%%%%%%%%
%      SWB      %
%%%%%%%%%%%%%%%%%
varind=3;
sizeFileFac=1;
variable='SWB';

%Fichier Log
fprintf(fidLog,'**********\n');
fprintf(fidLog,[variable ' Debut\n']);

%Telechargement
cd([pathDL variable '\']);

%Initialisation yearMax, monthMax, dayMax
yearMax=yearMin;
monthMax=monthMin;
dayMax=dayMin;

%Dir sur Download et In
dDL=dir([pathDL variable '\']);
dIn=dir([pathIn variable '\']);
cd([pathDL variable '\']);

%Dir du Download, récupération yearMax, monthMax, dayMax
for iDL=3:size(dDL,1)
    nameDL=dDL(iDL,1).name;
    [token,remain]=strtok(nameDL,'_');
    [toke1,remain]=strtok(nameDL,'.');
    if strcmp(token,variable) && strcmp(remain,'.zip')
        [token,remain]=strtok(fliplr(nameDL),'_');
        [token,remain]=strtok(fliplr(token),'.');
        yearDL=str2double(token(1:4));
        monthDL=str2double(token(5:6));
        dayDL=str2double(token(7:8));
        if yearDL > yearMax;
            yearMax=yearDL;
            monthMax=monthDL;
            dayMax=dayDL;
        end
        if yearDL == yearMax && monthDL > monthMax
            monthMax=monthDL;
            dayMax=dayDL;
        end
        if yearDL == yearMax && monthDL == monthMax && dayDL > dayMax
            dayMax=dayDL;
        end
    end
end

%Test existance Datapool
%Annee Max
getCurl=0;
for tryget=1:nbtry
    if getCurl==0
        system([pathB 'curl.exe -s -l -o ' pathT 'OutputCurl.html --user ' usernameDataPool ':' passwordDataPool ' ' adressDataPool '///////////datapool/Water/Water_Bodies/WB_Africa_V1/?cov=cont']);
        fid=fopen([pathT 'OutputCurl.html']);
        if fid~=-1
            OutputCurl=textscan(fid,'%s');
            fclose(fid);
            for line=1:size(OutputCurl{1},1);
                if str2double(OutputCurl{1}(line,1)) == yearMax+1
                    yearMax = yearMax+1;
                    monthMax=1;
                    dayMax=1;
                end
            end
            getCurl=1;
        end
    end
end
%Mois Max
year=yearMax;
for tryget=1:nbtry
    if getCurl==1
        system([pathB 'curl.exe -s -l -o ' pathT 'OutputCurl.html --user ' usernameDataPool ':' passwordDataPool ' ' adressDataPool '///////////datapool/Water/Water_Bodies/WB_Africa_V1/' num2str(yearMax) '/?cov=cont']);
        fid=fopen([pathT 'OutputCurl.html']);
        if fid~=-1
            OutputCurl=textscan(fid,'%s');
            fclose(fid);
            for month=1:12
                for line=1:size(OutputCurl{1},1)-1;
                    rline=OutputCurl{1}(line,1);
                    if strcmp(rline{1},monthName{month}) && str2double(OutputCurl{1}(line+1,1)) == year && month > monthMax
                        monthMax=month;
                        dayMax=1;
                    end
                end
            end
            getCurl=2;
        end
    end
end
%Decad Max
month=monthMax;
year=yearMax;
fid=-1;
for tryget=1:nbtry
    if getCurl==2
        system([pathB 'curl.exe -s -l -o ' pathT 'OutputCurl.html --user ' usernameDataPool ':' passwordDataPool ' ' adressDataPool '///////////datapool/Water/Water_Bodies/WB_Africa_V1/' num2str(yearMax) '/' num2str(monthMax) '/?cov=cont']);
        fid=fopen([pathT 'OutputCurl.html']);
        if fid~=-1
            OutputCurl=textscan(fid,'%s');
            fclose(fid);
            for day=1:10:21
                for line=1:size(OutputCurl{1},1)-2
                    rline=OutputCurl{1}(line+1,1);
                    if str2double(OutputCurl{1}(line,1)) == day && strcmp(rline{1},monthName{month}) && str2double(OutputCurl{1}(line+2,1)) == year && day > dayMax
                        dayMax=day;
                    end
                end
            end
            getCurl=3;
        end
    end
end

%Telechargement & Decompression
delete([pathDL variable '\robots.*']);

%Dir sur Download et In
dDL=dir([pathDL variable '\']);
dIn=dir([pathIn variable '\']);
for year=yearMin:yearMax
    if year==yearMin
        month1=monthMin;
    else
        month1=1;
    end
    if year==yearMax
        month2=monthMax;
    else
        month2=12;
    end
    for month=month1:month2
        if year==yearMin && month==monthMin
            day1=dayMin;
        else
            day1=1;
        end
        if year==yearMax && month==monthMax
            day2=dayMax;
        else
            day2=21;
        end
        for day=day1:10:day2
            %Construction nom du fichier sans extension
            fileName=[variable '_' num2str(year,'%04d') num2str(month,'%02d') num2str(day,'%02d')];
            
            %Repérage de ce qui existe Download
            existDL=false;
            for iDL=3:size(dDL,1);
                remain=dDL(iDL).name;
                [token, remain]=strtok(remain,'_');
                product=token;
                [token, remain]=strtok(remain,'_');
                product=[product '_' token];
                [token, remain]=strtok(remain,'_');
                product=[product '_' token];
                [token, remain]=strtok(remain,'_');
                date=token;
                yearDL=str2double(date(1:4));
                monthDL=str2double(date(5:6));
                dayDL=str2double(date(7:8));
                [token, remain]=strtok(remain,'_');
                continent=token;
                [token, remain]=strtok(remain,'_');
                sat=token;
                [token, remain]=strtok(fliplr(remain),'.');
                extension=fliplr(token);
                [token, remain]=strtok(fliplr(remain),'_');
                [token, remain]=strtok(fliplr(token),'.');
                version=[fliplr(remain) fliplr(token)];
                if yearDL == year && monthDL == month && dayDL == day
                    existDL=true;
                end
            end
            
            % Test existence In et taille
            existIn=false;
            sizeIn=-1;
            for iIn=1:size(dIn,1)
                nameIn=dIn(iIn,1).name;
                if strcmp(nameIn,[fileName '.img']);
                    sizeIn=dIn(iIn,1).bytes;
                    if sizeIn~=sizeFileFac*sizeFile
                        delete([pathIn variable '\' fileName '.img']);
                        delete([pathIn variable '\' fileName '.hdr']);
                    else
                        existIn=true;
                    end
                end
            end
            
            
            tryone=0;
            reforce=force;
            for tryget=1:nbtry
                
                %Téléchargement
                if ~existDL || reforce
                    system([pathB 'curl.exe -s -l -o ' pathT 'OutputCurl.html --user ' usernameDataPool ':' passwordDataPool ' ' adressDataPool '///////////datapool/Water/Water_Bodies/WB_Africa_V1/' num2str(year) '/' num2str(month) '/?cov=cont']);
                    fid=fopen([pathT 'OutputCurl.html']);
                    if fid~=-1
                        OutputCurl=textscan(fid,'%s');
                        fclose(fid);
                        delete([pathT 'OutputCurl.html']);
                        for line=1:size(OutputCurl{1},1)-2;
                            rline=OutputCurl{1}(line+1,1);
                            if str2double(OutputCurl{1}(line,1)) == day && strcmp(rline{1},monthName{month}) && str2double(OutputCurl{1}(line+2,1)) == year
                                if quiet~=1
                                    for ind=1:sizeOldFilename
                                        fprintf('\b');
                                    end
                                    sizeOldFilename=fprintf(' Telechargement    : %s          ',fileName);
                                end
                                
                                system([pathB 'curl.exe -s -l -o ' pathT 'OutputCurl.html --user ' usernameDataPool ':' passwordDataPool ' ' adressDataPool '///////////datapool/Water/Water_Bodies/WB_Africa_V1/' num2str(year) '/' num2str(month) '/' num2str(day) '/?cov=cont']);
                                fid=fopen([pathT 'OutputCurl.html']);
                                if fid~=-1
                                    OutputCurlb=textscan(fid,'%s');
                                    fclose(fid);
                                    delete([pathT 'OutputCurl.html']);
                                    for lineb=1:size(OutputCurlb{1},1);
                                        rline=OutputCurlb{1}(lineb,1);
                                        [token1,remain]=strtok(rline,'_');
                                        [token,remain]=strtok(remain,'_');
                                        [token2,remain]=strtok(remain,'_');
                                        if strcmp(token1,'WB') && strcmp(token2,'AFRI')
                                            namesat=rline{1};
                                            
                                            system([pathB 'WGET64.EXE --tries=1 --timestamping --timeout=180 -r -nd -q --accept=*.zip --http-user=' usernameDataPool ' --http-passwd=' passwordDataPool ' ' adressDataPool '//////datapool/Water/Water_Bodies/WB_Africa_V1/' num2str(year) '/' num2str(month) '/' num2str(day) '/' namesat '/?cov=cont']);
                                            delete([pathDL variable '\robots.*']);
                                            existDL=true;
                                            fprintf(fidLog,[fileName ' telechargement tentative ' num2str(tryget) '\n']);
                                            
                                        end
                                    end
                                end
                                
                                if existIn
                                    reforce=true;
                                end
                            end
                        end
                    end
                end
                
                %Decompression et verification taille
                if existDL && (~existIn || reforce)
                    if quiet~=1
                        for ind=1:sizeOldFilename
                            fprintf('\b');
                        end
                        sizeOldFilename=fprintf(' Decompression     : %s          ',fileName);
                    end
                    
                    dDL=dir([pathDL variable '\']);
                    existDL=false;
                    for iDL=3:size(dDL);
                        fileNameDL=dDL(iDL).name;
                        remain=fileNameDL;
                        [token, remain]=strtok(remain,'_');
                        product=token;
                        [token, remain]=strtok(remain,'_');
                        product=[product '_' token];
                        [token, remain]=strtok(remain,'_');
                        product=[product '_' token];
                        [token, remain]=strtok(remain,'_');
                        dateDL=token;
                        yearDL=str2double(dateDL(1:4));
                        monthDL=str2double(dateDL(5:6));
                        dayDL=str2double(dateDL(7:8));
                        [token, remain]=strtok(remain,'_');
                        continent=token;
                        [token, remain]=strtok(remain,'_');
                        sat=token;
                        [token, remain]=strtok(fliplr(remain),'.');
                        extension=fliplr(token);
                        [token, remain]=strtok(fliplr(remain),'_');
                        [token, remain]=strtok(fliplr(token),'.');
                        version=[fliplr(remain) fliplr(token)];
                        if yearDL == year && monthDL == month && dayDL == day
                            existDL=true;
                            zipfail=false;
                            try
                                unzip([pathDL variable '\' fileNameDL],pathT);
                            catch exception
                                zipfail=true;
                            end
                            % test taille fichier sortie
                            if ~zipfail
                                fileTiff=[pathT num2str(year,'%04d') num2str(month,'%02d') num2str(day,'%02d') '\' 'g2_BIOPAR_WB-SWB_' dateDL '_' continent '_' sat '_' version '.tiff'];
                                
                                tifffail=false;
                                try
                                    t=Tiff(fileTiff,'r');
                                    imageData=(t.read())';
                                    t.close();
                                catch exception
                                    tifffail=true;
                                end
                                
                                sizefail=true;
                                if ~tifffail
                                    if size(imageData,1)~=SWBtiffSizeX || size(imageData,2)~=SWBtiffSizeY
                                        sizefail=true;
                                        delete([pathDL variable '\' fileNameDL]);
                                        existDL=false;
                                    else
                                        sizefail=false;
                                    end
                                end
                                
                                
                                if ~sizefail
                                    imageDataCut=imageData(xmin:xmin+xsize-1,ymin:ymin+ysize-1);
                                    
                                    fid=fopen([pathIn variable '\' fileName '.img'],'w');
                                    fwrite(fid,imageDataCut,'uint8');
                                    fclose(fid);
                                    
                                    fid=fopen([pathIn variable '\' fileName '.hdr'],'w');
                                    fprintf(fid,'ENVI \n');
                                    fprintf(fid,'samples = %d \n',xsize);
                                    fprintf(fid,'lines = %d \n',ysize);
                                    fprintf(fid,'bands = 1 \n');
                                    fprintf(fid,'file type = ENVI standard \n');
                                    fprintf(fid,'data type = 1 \n');
                                    fprintf(fid,'map info = {Geographic Lat/Lon, 1.0000, 1.0000, -18.00446430, 27.37946430, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees} \n');
                                    fclose(fid);
                                    
                                    existIn=true;
                                    existDL=true;
                                    tryone=tryget;
                                    
                                    nbUnzip(varind)=nbUnzip(varind)+1;
                                    rmdir([pathT '\' num2str(year,'%04d') num2str(month,'%02d') num2str(day,'%02d')],'s');
                                    fprintf(fidLog,[fileName ' decompression reussie tentative ' num2str(tryget) '\n']);
                                    reforce=false;
                                end
                                
                            end
                            
                            if zipfail
                                delete([pathDL variable '\' fileNameDL]);
                                existDL=false;
                                fprintf(fidLog,[fileName ' echec decompression tentative ' num2str(tryget) '\n']);
                            end
                            
                            if ~zipfail && tifffail
                                delete([pathDL variable '\' fileNameDL]);
                                existDL=false;
                                fprintf(fidLog,[fileName ' echec lecture tiff tentative ' num2str(tryget) '\n']);
                            end
                            
                            if ~zipfail && ~tifffail && sizefail
                                delete([pathDL variable '\' fileNameDL]);
                                existDL=false;
                                fprintf(fidLog,[fileName ' dimensions incoherentes tentative ' num2str(tryget) '\n']);
                            end
                                
                            dirname=[pathT num2str(year,'%04d') num2str(month,'%02d') num2str(day,'%02d')];
                            if isdir(dirname)
                                rmdir(dirname,'s');
                            end
                        end
                    end
                end
            end
        end
    end
end           
fprintf(fidLog,[variable ' Fin\n']);

if quiet~=1
    for ind=1:sizeOldFilename
        fprintf('\b');
    end
    fprintf(' SHAPE convertis   : %d                \n',nbShpDetected);
    fprintf(' DMP decompresses  : %d\n',nbUnzip(1));
    fprintf(' NDVI decompresses : %d\n',nbUnzip(2));
    fprintf(' SWB decompresses  : %d\n',nbUnzip(3));
end


fprintf(fidLog,'*********\n');
fprintf(fidLog,'LOG Fin\n');
fprintf(fidLog,'*********\n');
fclose(fidLog);

% Fichier de sortie Flag
% [status, message, messageid] = rmdir([pathT '\*','s']);
nbShpDetected=nbShpDetected+sFlag(1);
for i=1:3
    nbUnzip(i)=nbUnzip(i)+sFlag(i+1);
end
fileName=[pathT 'DeCompressor_Flag.txt'];
fid=fopen(fileName,'w');
fprintf(fid,[' ' num2str(nbShpDetected,'%d') ' ' num2str(nbUnzip(1),'%d') ' ' num2str(nbUnzip(2),'%d') ' ' num2str(nbUnzip(3),'%d') '\n']);
fclose(fid);

cd(pathD);

if quiet~=1
    fprintf('\n');
    fprintf(' Operations terminees.');
end

if quiet==0 && autorun==0
    input('');
end
fprintf('\n');

