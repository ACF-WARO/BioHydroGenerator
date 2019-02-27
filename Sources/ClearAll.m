%cccccccccccccccccccccccccccccccccccccccccccccc
% Erwann Fillol, Action Contre la Faim (2017) c
%cccccccccccccccccccccccccccccccccccccccccccccc

clc
clear all

% warning off MATLAB:imagesci:Tiff:libraryWarning
warning('off','all');

% Repertoires entrée et sortie 
pathD=[pwd '\'];
% pathD=['D:\ACF3\BioHydroPond_v3_dev\'];

% Temps de pause
pauseT=0;

fprintf (' **********************************\n');
fprintf (' *        ClearAll (v1.0)         *\n');
fprintf (' * Action Contre la Faim (ACF-E)  *\n');
fprintf (' *        Erwann Fillol (2017)    *\n');
fprintf (' *        erwann.fillol@gmail.com *\n');
fprintf (' **********************************\n');
fprintf ('\n');
fprintf (' Note : Veuillez lire attentivement la notice\n');
fprintf ('        avant utilisation.\n');
fprintf ('        Les modifications ne sont pas réversibles.\n');
fprintf ('\n');
text=input(' Vidage des répertoires (O/N) : ','s');
fprintf ('\n');

if strncmpi(text,'O',1)
    
% Data\Raw\Biomass    
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\Biomass\Anomaly\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\Biomass\Biomass\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\Biomass\Cumul\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\Biomass\Filter\DMP\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\Biomass\Filter\NDVI\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\Biomass\Ponderate\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\Biomass\Stat\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\Biomass\Type\DMP\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\Biomass\Type\NDVI\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\Biomass\VI\*.*']);
    pause(pauseT);
    
% Data\Raw\Water
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\Water\Mask\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\Water\WaterAccess\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\Water\WaterAccessAnomaly\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\Water\WaterAccessType\*.*']);
    pause(pauseT);
    
% Data\Raw\In
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\In\DMP\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\In\NDVI\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Data\Raw\In\SWB\*.*']);
    pause(pauseT);
    
% Lib\Ancillary
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Lib\Ancillary\Copy\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Lib\Ancillary\CP\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Lib\Ancillary\Img\ADM*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Lib\Ancillary\Img\GEO*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Lib\Ancillary\Img\WATER*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Lib\Ancillary\Img\MASK*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Lib\Ancillary\Tmp\*.*']);
    pause(pauseT);
    
% Lib\Log
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Lib\Log\*.*']);
    pause(pauseT);
    
% Lib\Tmp
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Lib\Tmp\*.*']);   
    pause(pauseT);
    
% Lib\Param_Old
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Lib\Param_Old\*.*']);     
    pause(pauseT);
    
% Lib\Shape
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Lib\Shape\Shape_Copy\*.*']);         
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Lib\Shape\Shape_Out\*.*']);
    pause(pauseT);
    
% Output
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Output\Biomass\Anomaly\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Output\Biomass\Biomass\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Output\Biomass\Report\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Output\Biomass\Shape\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Output\Biomass\Stat\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Output\Biomass\VI\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Output\Water\Access\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Output\Water\Map\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Output\Water\Report\*.*']);
    pause(pauseT);
    [status,cmdout] = system(['del /Q /S /F ' pathD 'Output\Water\Shape\*.*']);
    
    
    input(' Fichiers supprimés.');
else
    input(' Aucun fichier supprimé.');
end
    
    
    
    
    
