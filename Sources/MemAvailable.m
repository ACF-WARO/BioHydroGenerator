%cccccccccccccccccccccccccccccccccccccccccccccc
% Erwann Fillol, Action Contre la Faim (2017) c
%cccccccccccccccccccccccccccccccccccccccccccccc

clear all

% Repertoires entrée et sortie
% cd('../..');
pathD=[pwd '\'];
% pathD=['D:/ACF3/BioHydroPond_v3_dev/'];
pathTmp=[pathD 'Lib\Tmp\'];

[user,sys]=memory;

Mem=sys.PhysicalMemory.Available;

fileName=[pathTmp 'MemAvailable.txt'];
fid=fopen(fileName,'w+');
fprintf(fid,'%d',floor(Mem));
fclose(fid);
