%cccccccccccccccccccccccccccccccccccccccccccccc
% Erwann Fillol, Action Contre la Faim (2018) c
% v1.2                              aout 2018 c
%cccccccccccccccccccccccccccccccccccccccccccccc

clear all

% warning off MATLAB:imagesci:Tiff:libraryWarning
warning('off','all');

% Repertoires entrée et sortie
pathD=[pwd '\'];
% pathD=['E:\ACF3\BioHydroPond_v3_dev\'];
pathDL=[pathD 'Data\Download\'];
pathIn=[pathD 'Data\Raw\In\'];
pathT=[pathD 'Lib\Tmp\'];
pathL=[pathD 'Lib\Log\'];
pathB=[pathD 'Lib\Bin\'];
pathO=[pathD 'Lib\Param_Old\'];
pathParam=[pathD 'Param\'];

% Parametre AutoRun
fileName=[pathParam 'AutoRun_Param.txt'];
fid=fopen(fileName);
Param=textscan(fid,'%s');
fclose(fid);
for line=2:size(Param{1},1)
    rline=Param{1}(line,1);
    if strcmp(rline{1},'BioGenerator')
        ParamRunBio=str2double(Param{1}(line-2,1));
        ParamRunHydro=str2double(Param{1}(line-1,1));
    end
    if strcmp(rline{1},'FilePush')
        ParamFilePush=str2double(Param{1}(line-1,1));
    end     
end
quiet=0;

% Lecture Liste Push si besoin
if ParamFilePush == 1
    fileName=[pathParam 'File_Push.txt'];
    fid=fopen(fileName);
    PushList=textscan(fid,'%s');
    fclose(fid);
    FilePushNb=0;
    for line=1:2:size(PushList{1},1)
        FilePushNb=FilePushNb+1;
        FilePushList(FilePushNb,1)=PushList{1}(line,1);
        FilePushList(FilePushNb,2)=PushList{1}(line+1,1);
    end
end
    

% Parametre BioGenerator (Pour Access)
fileName=[pathParam 'BioGenerator_Param.txt'];
fid=fopen(fileName);
Param=textscan(fid,'%s');
fclose(fid);
for line=2:size(Param{1},1)
    rline=Param{1}(line,1);
    if strcmp(rline{1},'Access_Flag')
        ParamBioAccess=str2double(Param{1}(line-5,1));
    end       
end


cd(pathD);


RunBio=1;
RunHydro=1;
RunPush=0;
decadLastWait=0;
decadLastBio=-1;
decadLastHydro=-1;
dayLastBio=0;
monthLastBio=0;
yearLastBio=0;
dayLastHydro=0;
monthLastHydro=0;
yearLastHydro=0;
decadLastDMP=0;
decadLastSWB=0;
year=-1;
month=-1;
day=-1;
hour=-1;
minute=-1;
second=-1;
firstRun=1;
waitClock=1;

while 0==0
    
    if quiet~=1
        clc
        fprintf (' **********************************\n');
        fprintf (' *        AutoRun (v1.2)          *\n');
        fprintf (' * Action Contre la Faim (ACF-E)  *\n');
        fprintf (' *        Erwann Fillol (2018)    *\n');
        fprintf (' *        erwann.fillol@gmail.com *\n');
        fprintf (' **********************************\n');
        fprintf ('\n');
    end
    
    
    if quiet~=1
        if ParamRunBio==1
            fprintf ([' BioGenerator derniere decade integree   : ' num2str(dayLastBio,'%02d') '/' num2str(monthLastBio,'%02d') '/' num2str(yearLastBio,'%04d') '\n']);
        end
        if ParamRunHydro==1
            fprintf ([' HydroGenerator derniere decade integree : ' num2str(dayLastHydro,'%02d') '/' num2str(monthLastHydro,'%02d') '/' num2str(yearLastHydro,'%04d') '\n']);
        end
        fprintf ('\n');
        fprintf ('                                    ');
    end
    
    while waitClock==1
        clockS=clock;
        year=clockS(1);
        month=clockS(2);
        day=clockS(3);
        hour=clockS(4);
        minute=clockS(5);
        second=floor(clockS(6));
        if quiet~=1
            fprintf ('\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b');
            fprintf ([' Date : ' num2str(day,'%02d') '/' num2str(month,'%02d') '/' num2str(year,'%04d') '  Heure : ' num2str(hour,'%02d') ':' num2str(minute,'%02d') ':' num2str(second,'%02d')]);
            pause(1);
        end
        
        % Dernière décade attendue
        monthLastWait=month;
        yearLastWait=year;
        if day<11
            dayLastWait=21;
            monthLastWait=monthLastWait-1;
            if monthLastWait<1
                monthLastWait=12;
                yearLastWait=year-1;
            end
        end
        if day>10 && day<21
            dayLastWait=01;
        end
        if day>21
            dayLastWait=11;
        end
        decadLastWait=yearLastWait*10000+monthLastWait*100+dayLastWait;
        
        if firstRun==1 || ...
           (decadLastWait~=decadLastDMP && minute==0 && second==0) || (decadLastWait~=decadLastSWB && minute==0 && second==0) || ...
           (decadLastDMP~=decadLastBio && ParamRunBio==1) || ...
           (decadLastSWB~=decadLastHydro && ParamRunHydro==1) || ...
           (hour==0 && minute==0 && second==0)
            waitClock=0;
        end

    end
    
    
    
    
    % Téléchargement
    fprintf('\n');
    fprintf('\n');
    
    fprintf(' Etat : Lancement DeCompressor\n');
    fprintf('\n');
    fileName=[pathT 'AutoRun_Mode.txt'];
    fid=fopen(fileName,'w');
    fprintf(fid,'1');
    fclose(fid);
    [status]=system(['DeCompressor.exe']);
    %input('');
    fprintf('\n');
    fileName=[pathT 'DeCompressor_Flag.txt'];
    fid=fopen(fileName,'r');
    if fid~=-1
        DeCompressor_Flag=fscanf(fid,'%d');
        fclose(fid);
    else
        DeCompressor_Flag=zeros(4);
    end
    
    % Dir sur DMP
    yearLastDMP=0;
    monthLastDMP=0;
    dayLastDMP=0;
    dIn=dir([pathIn 'DMP\']);
    for iIn=1:size(dIn,1)
        nameIn=dIn(iIn,1).name;
        sizeIn=dIn(iIn,1).bytes;
        dateIn=dIn(iIn,1).datenum;
        [token,remain]=strtok(fliplr(nameIn),'_');
        [token,remain]=strtok(fliplr(token),'.');
        if strcmp(remain,'.img')
            yearIn=str2double(token(1:4));
            monthIn=str2double(token(5:6));
            dayIn=str2double(token(7:8));
            if yearIn > yearLastDMP;
                yearLastDMP=yearIn;
                monthLastDMP=monthIn;
                dayLastDMP=dayIn;
            end
            if yearIn == yearLastDMP && monthIn > monthLastDMP
                monthLastDMP=monthIn;
                dayLastDMP=dayIn;
            end
            if yearIn == yearLastDMP && monthIn == monthLastDMP && dayIn > dayLastDMP
                dayLastDMP=dayIn;
            end
        end
    end
    decadLastDMP=yearLastDMP*10000+monthLastDMP*100+dayLastDMP;
    
    % Dir sur SWB
    yearLastSWB=0;
    monthLastSWB=0;
    dayLastSWB=0;    
    dIn=dir([pathIn 'SWB\']);
    for iIn=1:size(dIn,1)
        nameIn=dIn(iIn,1).name;
        sizeIn=dIn(iIn,1).bytes;
        dateIn=dIn(iIn,1).datenum;
        [token,remain]=strtok(fliplr(nameIn),'_');
        [token,remain]=strtok(fliplr(token),'.');
        if strcmp(remain,'.img')
            yearIn=str2double(token(1:4));
            monthIn=str2double(token(5:6));
            dayIn=str2double(token(7:8));
            if yearIn > yearLastSWB;
                yearLastSWB=yearIn;
                monthLastSWB=monthIn;
                dayLastSWB=dayIn;
            end
            if yearIn == yearLastSWB && monthIn > monthLastSWB
                monthLastSWB=monthIn;
                dayLastSWB=dayIn;
            end
            if yearIn == yearLastSWB && monthIn == monthLastSWB && dayIn > dayLastSWB
                dayLastSWB=dayIn;
            end
        end
    end
    decadLastSWB=yearLastSWB*10000+monthLastSWB*100+dayLastSWB;
    
    
    % Test Flag et lancement
    if DeCompressor_Flag(1)~=0 || DeCompressor_Flag(2)~=0 || DeCompressor_Flag(3)~=0 || (DeCompressor_Flag(4)~=0 && ParamBioAccess==1) || decadLastBio~=decadLastDMP
        RunBio=1;
    end
    if DeCompressor_Flag(1)~=0 || DeCompressor_Flag(4)~=0 || decadLastHydro~=decadLastSWB 
        RunHydro=1;
    end
    % Effacement Flag
    delete([pathT 'DeCompressor_Flag.txt']);
    
    
    if RunBio==1 && ParamRunBio==1
        fprintf('\n');
        fprintf(' Etat : Lancement BioGenerator\n');
        fprintf('\n');
        fileName=[pathT 'AutoRun_Mode.txt'];
        fid=fopen(fileName,'w');
        fprintf(fid,'1');
        fclose(fid);
        [status]=system(['BioGenerator.exe']);
        %input('');
        fprintf('\n');
        RunBio=0;
        RunPush=1;
        % Dernière décade BioGenerator
        fileName=[pathO 'BioGenerator_Param_Old.txt'];
        fid=fopen(fileName,'r');
        if fid~=-1
            t=fscanf(fid,'%d');
            fclose(fid);
            yearLastBio=t(2);
            monthLastBio=t(3);
            dayLastBio=t(4);
            decadLastBio=yearLastBio*10000+monthLastBio*100+dayLastBio;
        else
            yearLastBio=0;
            monthLastBio=0;
            dayLastBio=0;
            decadLastBio=0;
        end
    end
    
    if RunHydro==1 && ParamRunHydro==1
        fprintf('\n');
        fprintf(' Etat : Lancement HydroGenerator\n');
        fprintf('\n');
        fileName=[pathT 'AutoRun_Mode.txt'];
        fid=fopen(fileName,'w');
        fprintf(fid,'1');
        fclose(fid);        
        [status]=system(['HydroGenerator.exe']);
        %input('');
        fprintf('\n');
        RunHydro=0;
        RunPush=1;
        % Dernière décade HydroGenerator
        fileName=[pathO 'HydroGenerator_Param_Old.txt'];
        fid=fopen(fileName,'r');
        if fid~=-1
            t=fscanf(fid,'%d');
            fclose(fid);
            yearLastHydro=t(2);
            monthLastHydro=t(3);
            dayLastHydro=t(4);
            decadLastHydro=yearLastHydro*10000+monthLastHydro*100+dayLastHydro;
        else
            yearLastHydro=0;
            monthLastHydro=0;
            dayLastHydro=0;
            decadLastHydro=0;
        end
    end
    
    if RunPush==1 && ParamFilePush==1
        for line=1:FilePushNb
            file1=FilePushList(line,1);
            file2=FilePushList(line,2);
            [status,cmdout]=system(['copy ' file1{1} ' ' file2{1}]);
        end
        RunPush=0;
    end
    
    if quiet~=1
        pause(1);
    end
    
    firstRun=0;
    waitClock=1;
    
    
end










