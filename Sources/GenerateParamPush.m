clear all

% warning off MATLAB:imagesci:Tiff:libraryWarning
warning('off','all');

% Repertoires entrée et sortie
pathD=[pwd '\'];
pathD=['E:\ACF3\BioHydroPond_v3_dev\'];
pathTmp=[pathD 'Lib\Tmp\'];

fileName=[pathTmp 'ParamPush.txt'];
fid=fopen(fileName,'w+');
for year=2018:-1:1998
    fprintf(fid,['Output\\Biomass\\Biomass\\Biomass_' num2str(year) '.tif Y:\\data_dir\\data\\Biomass\\SAH_BiomassBiomass' num2str(year) '_ef_v0\\SAH_BiomassBiomass' num2str(year) '_ef_v0.geotiff\n']);
    fprintf(fid,['Output\\Biomass\\Biomass\\Biomass_' num2str(year) '.tif Y:\\data_dir\\MetaDownload\\BiomassValue' num2str(year) '.tif\n']);
end
fclose(fid);