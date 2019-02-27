%cccccccccccccccccccccccccccccccccccccccccccccc
% Erwann Fillol, Action Contre la Faim (2017) c
%cccccccccccccccccccccccccccccccccccccccccccccc

clc
clear all

fprintf (' **********************************\n');
fprintf (' *      AccessPassWord (v1.0 beta)*\n');
fprintf (' * Action Contre la Faim (ACF-E)  *\n');
fprintf (' *        Erwann Fillol (2017)    *\n');
fprintf (' *        erwann.fillol@gmail.com *\n');
fprintf (' **********************************\n');
fprintf ('\n');
fprintf (' Note : Veuillez lire attentive la notice\n');
fprintf ('        avant utilisation.\n');
fprintf ('        Les modifications de mots de passe\n');
fprintf ('        ne sont pas réversibles.\n');
fprintf ('\n');
fprintf ('\n');

% Repertoires entrée et sortie
cd('..\..');
pathD=[pwd '\'];
% pathD=['D:\ACF3\BioHydroPond_v3_dev\'];
pathLogin=[pathD 'Lib\Login\'];


% Passphrase
passPhrase='walaran';


% Info ftp
fileName=[pathLogin 'VITO_FTP.txt'];
fid=fopen(fileName);
infoFTP=textscan(fid,'%s');
fclose(fid);
c=infoFTP{1}(1);
hostFTP=c{1};
c=infoFTP{1}(2);
usernameFTP=c{1};
fprintf (' Connexion FTP VITO :\n');
fprintf (['   Adresse : ' hostFTP '\n']);
fprintf (['   Login   : ' usernameFTP '\n']);
fprintf (['   Pass    : ***********\n']);
fprintf ('\n');
text=input(' Modification (O/N) : ','s');

change=0;

if strncmpi(text,'O',1)
    validation='N';
    change=change+1;
    while ~strncmpi(validation,'O',1)
        hostFTP=input('   Adresse : ','s');
        usernameFTP=input ('   Login   : ','s');
        passFTP=input('   Pass    : ','s');
        validation=input(' Validation (O/N) : ','s');
    end
    
    stringIn=passFTP;
    for i=1:size(stringIn,2)
        charIn=double(stringIn(i));
        j=mod((i-1),size(passPhrase,2))+1;
        charOut=(charIn-31)+double(passPhrase(j));
        while charOut>95
            charOut=charOut-95;
        end
        charOut=charOut+31;
        stringIn(i)=char(charOut);
    end
    passFTP=stringIn;
    
    fileName=[pathLogin 'VITO_FTP.txt'];
    fid=fopen(fileName,'w');
    fprintf(fid,hostFTP);
    fprintf(fid,'\n');
    fprintf(fid,usernameFTP);
    fprintf(fid,'\n');
    fprintf(fid,passFTP);
    fclose(fid);
end


fprintf('\n');
fprintf('\n');


% Datapool
fileName=[pathLogin 'Copernicus_Datapool.txt'];
fid=fopen(fileName);
infoDP=textscan(fid,'%s');
fclose(fid);
c=infoDP{1}(1);
adressDataPool=c{1};
c=infoDP{1}(2);
usernameDataPool=c{1};
fprintf (' Connexion Copernicus Datapool :\n');
fprintf (['   Adresse : ' adressDataPool '\n']);
fprintf (['   Login   : ' usernameDataPool '\n']);
fprintf (['   Pass    : ***********\n']);
fprintf ('\n');
text=input(' Modification (O/N) : ','s');

if strncmpi(text,'O',1)
    validation='N';
    change=change+1;
    while ~strncmpi(validation,'O',1)
        adressDataPool=input('   Adresse : ','s');
        usernameDataPool=input ('   Login   : ','s');
        passDataPool=input('   Pass    : ','s');
        validation=input(' Validation (O/N) : ','s');
    end
    
    stringIn=passDataPool;
    for i=1:size(stringIn,2)
        charIn=double(stringIn(i));
        j=mod((i-1),size(passPhrase,2))+1;
        charOut=(charIn-31)+double(passPhrase(j));
        while charOut>95
            charOut=charOut-95;
        end
        charOut=charOut+31;
        stringIn(i)=char(charOut);
    end
    passDataPool=stringIn;
    
    fileName=[pathLogin 'Copernicus_Datapool.txt'];
    fid=fopen(fileName,'w');
    fprintf(fid,adressDataPool);
    fprintf(fid,'\n');
    fprintf(fid,usernameDataPool);
    fprintf(fid,'\n');
    fprintf(fid,passDataPool);
    fclose(fid);
end

fprintf('\n');
if change~=0
    input(' Modifications enregistrées.');
else
    input(' Aucune modification enregistrée.');
end

    
    
   