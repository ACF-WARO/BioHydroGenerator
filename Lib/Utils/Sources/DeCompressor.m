clc
clear all

warning off MATLAB:imagesci:Tiff:libraryWarning


fprintf ('*********************************\n');
fprintf ('*        Decompressor 1.0       *\n');
fprintf ('* Action Contre la Faim (ACF-E) *\n');
fprintf ('*        Erwann Fillol (2017)   *\n');
fprintf ('*        erwann.fillol@gmail.com*\n');
fprintf ('*********************************\n');
fprintf ('\n');
fprintf (' Initialisation    : ');


% Date et heure
clockS=clock;

% Repertoires entrée et sortie
pathD=[pwd '/'];
%pathD=['D:/ACF3/BioHydroPond_v2/'];
pathI=[pathD 'Data/Download/'];
pathO=[pathD 'Data/In/'];
pathT=[pathD 'Lib/Tmp/'];
pathL=[pathD 'Log/'];
pathShpIn=[pathD 'Shape/'];
pathShpCp=[pathD 'Lib/Shape_Copy/'];
pathShpRs=[pathD 'Lib/Ancillary/Img/'];
pathShpOut=[pathD 'Lib/Shape_Out/'];

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

% Init
sizeOldFilename=0;
nbDMP=0;
nbNDVI=0;
nbSWB=0;

% Fichier log
fileNameLog=[pathL 'DataLog.txt'];
fidLog=fopen(fileNameLog,'a');
fprintf(fidLog,'\n');
fprintf(fidLog,'%02d/%02d/%04d %02d:%02d:%02d\n',clockS(3),clockS(2),clockS(1),clockS(4),clockS(5),floor(clockS(6)));

% Lecture PixelSizeMap
filename=[pathShpRs 'PixelSizeMap.img'];
fid=fopen(filename,'r');
PixelSizeMap=(reshape(fread(fid,xsize*ysize,'single'),[xsize ysize]))';
fclose(fid);

fprintf('OK\n');
fprintf('\n');

%%%%%%%%%%%%%%%%%%%%%%%%
%      SHAPE           %
%%%%%%%%%%%%%%%%%%%%%%%%
nbShpDecteted=0;
sizeOldFilename=0;
for adm=0:5
    if adm <= 2
        shpName=['ADM_' num2str(adm)];
    else
        shpName=['GEO_' num2str(adm)];
    end
    
    ShapeIn = shaperead([pathShpIn shpName '.shp']);
    if exist([pathShpCp shpName '.shp'],'file')
        ShapeCp = shaperead([pathShpCp shpName '.shp']);
    else
        ShapeCp = 0;
    end
    
    if ~isequalwithequalnans(ShapeIn,ShapeCp)
        for ind=1:sizeOldFilename
            fprintf('\b');
        end
        fprintf(' Nouveau Shape     : %s (   %%)',shpName);
        sizeOldFilename=33;
        nbShpDecteted=nbShpDecteted+1;
        
        nbField=size(ShapeIn,1);
        
        ShapeCp=ShapeIn;
        
% Creation champs valeur ids : Récupération du champs ID s'il existe,
% conversion en nombre si en str, comblage des valeurs manquantes
        clear ids
        if isfield(ShapeIn,'ID')
            for k=1:nbField
                if isa(ShapeIn(k).ID,'numeric')
                    ids(k)=ShapeIn(k).ID;
                else
                    id=str2num(ShapeIn(k).ID);
                    if ~isempty(id)
                        ids(k)=id;
                    else
                        ids(k)=-9999;
                    end
                end
            end
            idmax=max(ids)+1;
            for k=1:nbField
                if ids(k)==-9999
                    ids(k)=idmax;
                    idmax=idmax+1;
                end
            end
            ShapeIn=rmfield(ShapeIn,'ID');
        else
            for k=1:nbField
                ids(k)=k;
            end
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
        Zout=zeros(M,N);
        nbFieldO=0;
        idmax=0;
        for k=1:nbField
            pc=round(100.*(k-1)/(nbField-1));
            fprintf('\b\b\b\b\b\b(%03d%%)',pc);
            lat=ShapeIn(k).Y;
            lon=ShapeIn(k).X;

            % Ré-écriture champs ID et NAME
            id=ids(k);
            name=names(k).n;
            ShapeIn(k).ID=id;
            ShapeIn(k).NAME=name;
            
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
                
                clear Z j i
                Z=vec2mtx(lat,lon,1/res,[latmin,latmax],[lonmin,lonmax],'filled');
                [i,j]=find(Z~=2);
                j=j+imin;
                i=jmax-i;
                clear gind
                gind=find(i>=1 & i<=M & j>=1 & j<=N);
  
                if ~isempty(gind)
                    jb=j(gind);
                    ib=i(gind);
                    Zout(sub2ind(size(Zout),ib,jb))=id;
                    nbFieldO=nbFieldO+1;
                    idField(nbFieldO)=id;
                    nameField{nbFieldO}=name;
                end
            end
        end
        clear stock
        nbFieldOO=0;
        for k=1:nbFieldO
            pc=round(100.*(k-1)/(nbFieldO-1));
            fprintf('\b\b\b\b\b\b(%03d%%)',pc);
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
                stock(nbFieldOO,6)=sum(PixelSizeMap(ind))/100.;
                stockName{nbFieldOO}=name;
            end
        end
        
        % Sauvegarde fichier Shape Modifié
        shapewrite(ShapeIn,[pathShpOut shpName '.shp']);

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
for ind=1:sizeOldFilename
    fprintf('\b');
end     
sizeOldFilename=0;        



%%%%%%%%%%%%%%%%%
%      DMP      %
%%%%%%%%%%%%%%%%%
variable='DMP';
pathIn=[pathI variable '/'];
pathOut=[pathO variable '/'];

%Fichier Log
fprintf(fidLog,'**********\n');
fprintf(fidLog,'DMP Debut\n');

%Test taille fichiers sorties
listFileOut=dir(pathOut);
for index=3:size(listFileOut)
    fileName=listFileOut(index).name;
    fileSize=listFileOut(index).bytes;
    remain=fileName;
    [token,remain]=strtok(remain,'.');
    extention=remain;
    if strcmp(extention,'.img');
        if fileSize~=2*sizeFile
            delete([pathOut token '*']);
            fprintf(fidLog,[token ' supprime car la taille ne correspond pas\n']);
        end
    end
end

%Decompression
listFileIn=dir(pathIn);
listFileOut=dir(pathOut);
for indexIn=3:size(listFileIn);
    fileNameIn=listFileIn(indexIn).name;
    remain=fileNameIn;
    [token,remain]=strtok(remain,'.');
    extention=remain;    
    fileName=token;
    fileTest=false;
    for indexOut=3:size(listFileOut,1)
        fileNameOut=listFileOut(indexOut).name;
        if strcmp([fileName '.img'],fileNameOut)
            fileTest=true;
        end
    end
    if ~fileTest
    	zipfail=false;
        try
            unzip([pathIn fileNameIn],pathT);
        catch exception
            zipfail=true;
        end
            
        % test taille fichier sortie
        if ~zipfail
            listFileTmp=dir(pathT);
            for indexTmp=3:size(listFileTmp,1)
                fileNameTmp=listFileTmp(indexTmp).name;
                fileSizeTmp=listFileTmp(indexTmp).bytes;
                if strcmp(fileNameTmp,[fileName '.img'])
                    if fileSizeTmp==2*sizeFile
                        nbDMP=nbDMP+1;
                        for ind=1:sizeOldFilename
                            fprintf('\b');
                        end
                        fprintf(' Fichier           : %s          ',fileName);
                        sizeOldFilename=size(fileName,2)+31;
                        movefile([pathT fileName '.img'],pathOut);
                        movefile([pathT fileName '.hdr'],pathOut);
                        fprintf(fidLog,[fileName ' decompresse et copie\n']);
                    else
                        fprintf(fidLog,[fileName ' taille de l''archive incorrecte\n']);
                    end
                    delete([pathT fileName '*']);
                end
            end
        else
            fprintf(fidLog,[fileName ' archive corrompue\n']);
        end
    end
end
fprintf(fidLog,'DMP Fin\n');
for ind=1:sizeOldFilename
    fprintf('\b');
end
sizeOldFilename=0;



%%%%%%%%%%%%%%%%%
%      NDVI     %
%%%%%%%%%%%%%%%%%
variable='NDVI';
pathIn=[pathI variable '/'];
pathOut=[pathO variable '/'];

%Fichier Log
fprintf(fidLog,'**********\n');
fprintf(fidLog,'NDVI Debut\n');

%Test taille fichiers sorties
listFileOut=dir(pathOut);
for index=3:size(listFileOut)
    fileName=listFileOut(index).name;
    fileSize=listFileOut(index).bytes;
    remain=fileName;
    [token,remain]=strtok(remain,'.');
    extention=remain;
    if strcmp(extention,'.img');
        if fileSize~=sizeFile
            delete([pathOut token '*']);
            fprintf(fidLog,[token ' supprime car la taille ne correspond pas\n']);
        end
    end
end

%Decompression
listFileIn=dir(pathIn);
listFileOut=dir(pathOut);
for indexIn=3:size(listFileIn);
    fileNameIn=listFileIn(indexIn).name;
    remain=fileNameIn;
    [token,remain]=strtok(remain,'.');
    extention=remain;    
    fileName=token;
    fileTest=false;
    for indexOut=3:size(listFileOut,1)
        fileNameOut=listFileOut(indexOut).name;
        if strcmp([fileName '.img'],fileNameOut)
            fileTest=true;
        end
    end
    if ~fileTest
    	zipfail=false;
        try
            unzip([pathIn fileNameIn],pathT);
        catch exception
            zipfail=true;
        end
            
        % test taille fichier sortie
        if ~zipfail
            listFileTmp=dir(pathT);
            for indexTmp=3:size(listFileTmp,1)
                fileNameTmp=listFileTmp(indexTmp).name;
                fileSizeTmp=listFileTmp(indexTmp).bytes;
                if strcmp(fileNameTmp,[fileName '.img'])
                    if fileSizeTmp==sizeFile
                        nbNDVI=nbNDVI+1;
                        for ind=1:sizeOldFilename
                            fprintf('\b');
                        end
                        fprintf(' Fichier           : %s          ',fileName);
                        sizeOldFilename=size(fileName,2)+31;
                        movefile([pathT fileName '.img'],pathOut);
                        movefile([pathT fileName '.hdr'],pathOut);
                        fprintf(fidLog,[fileName ' decompresse et copie\n']);
                    else
                        fprintf(fidLog,[fileName ' taille de l''archive incorrecte\n']);
                    end
                    delete([pathT fileName '*']);
                end
            end
        else
            fprintf(fidLog,[fileName ' archive corrompue\n']);
        end
    end
end
fprintf(fidLog,'NDVI Fin\n');
for ind=1:sizeOldFilename
    fprintf('\b');
end
sizeOldFilename=0;




%%%%%%%%%%%%%%%%%
%      SWB      %
%%%%%%%%%%%%%%%%%
variable='SWB';
pathIn=[pathI variable '/'];
pathOut=[pathO variable '/'];

%Fichier Log
fprintf(fidLog,'**********\n');
fprintf(fidLog,'SWB Debut\n');

%Test taille fichiers sorties
listFileOut=dir(pathOut);
for index=3:size(listFileOut)
    fileName=listFileOut(index).name;
    fileSize=listFileOut(index).bytes;
    remain=fileName;
    [token,remain]=strtok(remain,'.');
    extention=remain;
    if strcmp(extention,'.img');
        if fileSize~=sizeFile
            delete([pathOut token '*']);
            fprintf(fidLog,[token ' supprime car la taille ne correspond pas\n']);
        end
    end
end

%Decompression
listFileIn=dir(pathIn);
listFileOut=dir(pathOut);
for indexIn=3:size(listFileIn);
    fileNameIn=listFileIn(indexIn).name;
    remain=fileNameIn;
    [token, remain]=strtok(remain,'_');
    product=token;
    [token, remain]=strtok(remain,'_');
    date=token;
    year=str2double(date(1:4));
    month=str2double(date(5:6));
    day=str2double(date(7:8));
    [token, remain]=strtok(remain,'_');
    continent=token;
    [token, remain]=strtok(remain,'_');
    sat=token;
    [token, remain]=strtok(remain,'_');
    version=token;
    fileName=['SWB_' num2str(year,'%04d') num2str(month,'%02d') num2str(day,'%02d')];
    fileTest=false;
    for indexOut=3:size(listFileOut,1)
        fileNameOut=listFileOut(indexOut).name;
        if strcmp([fileName '.img'],fileNameOut)
            fileTest=true;
        end
    end
    if strcmpi(product,'WB') && strcmpi(continent,'AFRI') && ~fileTest
        
        listFileSub=dir([pathIn fileNameIn]);
        for indexFileSub = 3:size(listFileSub,1)
            fileNameSub=listFileSub(indexFileSub).name;
            remain=fileNameSub;
            while size(remain,2)~=0
                [token,remain]=strtok(remain,'.');
            end
            extention=token;
            if strcmpi(extention,'ZIP');
                
                zipfail=false;
                try
                    unzip([pathIn fileNameIn '/' fileNameSub],pathT);
                catch exception
                    zipfail=true;
                end
                
                % test taille fichier sortie
                if ~zipfail
                    fileTiff=[pathT num2str(year,'%04d') num2str(month,'%02d') num2str(day,'%02d') '/' 'g2_BIOPAR_WB-SWB_' date '_' continent '_' sat '_' version '.tiff'];
                    tifffail=false;
                    try
                        t=Tiff(fileTiff,'r');
                        imageData=(t.read())';
                        t.close();
                        if size(imageData,1)~=SWBtiffSizeX || size(imageData,2)~=SWBtiffSizeY
                            tifffail=true;
                        end
                    catch exception
                        tifffail=true;
                    end
                    
                    
                    if ~tifffail

                        
                        imageDataCut=imageData(xmin:xmin+xsize-1,ymin:ymin+ysize-1);
                        
                        fileNameOut=[pathOut fileName]; 
                        
                        fid=fopen([fileNameOut '.img'],'w');
                        fwrite(fid,imageDataCut,'uint8');
                        fclose(fid);
                        
                        fid=fopen([fileNameOut '.hdr'],'w');
                        fprintf(fid,'ENVI \n');
                        fprintf(fid,'samples = %d \n',xsize);
                        fprintf(fid,'lines = %d \n',ysize);
                        fprintf(fid,'bands = 1 \n');
                        fprintf(fid,'file type = ENVI standard \n');
                        fprintf(fid,'data type = 1 \n');
                        fprintf(fid,'map info = {Geographic Lat/Lon, 1.0000, 1.0000, -18.00446430, 27.37946430, 8.9285714300e-003, 8.9285714300e-003, WGS-84, units=Degrees} \n');
                        fclose(fid);
                        
                        nbSWB=nbSWB+1;
                        for ind=1:sizeOldFilename
                            fprintf('\b');
                        end
                        fprintf(' Fichier           : %s          ',fileName);
                        sizeOldFilename=size(fileName,2)+31;
                        fprintf(fidLog,[fileName ' decompresse et copie\n']);
                        
                        rmdir([pathT '/' num2str(year,'%04d') num2str(month,'%02d') num2str(day,'%02d')],'s');

                    else
                        fprintf(fidLog,[fileName ' archive corrompue\n']);
                    end
                else
                    fprintf(fidLog,[fileName ' archive corrompue\n']);
                end
            end
        end
    end
end
fprintf(fidLog,'SWB Fin\n');
for ind=1:sizeOldFilename
    fprintf('\b');
end

fprintf(' SHAPE convertis   : %d                \n',nbShpDecteted);   
fprintf(' DMP decompresses  : %d\n',nbDMP);
fprintf(' NDVI decompresses : %d\n',nbNDVI);
fprintf(' SWB decompresses  : %d\n',nbSWB);


fprintf(fidLog,'*********\n');
fprintf(fidLog,'LOG Fin\n');
fprintf(fidLog,'*********\n');
fclose(fidLog);

[status, message, messageid] = rmdir([pathT '/*','s']);

fprintf('\n');
input('Operations terminees.');


