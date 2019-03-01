% clear all

% Repertoires entrée et sortie
% cd('../..');
pathD=[pwd '/'];
% pathD=['D:/ACF3/BioHydroPond_v2/'];
pathShpIn=[pathD 'Lib/Shape_Out/'];
pathCsvIn=[pathD 'Data/Out/Water/Report/'];
pathShpOut=[pathD 'Data/Out/Water/Shape/'];

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
        
        fid=fopen([pathCsvIn 'Access_' shpName '.csv']);
        if fid~=-1
            fclose(fid);
            
            % Lecture du fichier statistiques issu de PondMonitor
            CsvIn = dlmread([pathCsvIn 'Access_' shpName '.csv'],';',5,2);
            
            % Nombre d'année
            lastyear=1998+size(CsvIn,2)-5;
            
            % Extraction vecteur ID de ShapeIn et
            % initialisation des nouveaux
            % champs de sortie
            clear IDin
            for k=1:size(ShapeIn,1)
                IDin(k)=ShapeIn(k).ID;
                ShapeIn(k).('AREA')=-1;
                ShapeIn(k).('ACCESS_MEAN')=-1;
                for year=1998:lastyear
                    ShapeIn(k).(['ANOM_' num2str(year)])=-1;
                end
            end
            
            % Ecriture des nouveaux champs
            for m=1:size(CsvIn,1)
                ID=CsvIn(m,1);
                [l,k]=find(IDin==ID);
                if ~isempty(k)
                    ShapeIn(k).('AREA')=CsvIn(m,2);
                    ShapeIn(k).('ACCESS_MEAN')=CsvIn(m,3);
                    for year=1998:lastyear
                        ShapeIn(k).(['ANOM_' num2str(year)])=CsvIn(m,year-1998+4);
                    end
                end
            end
            
            % Ecriture du fichier Shape de sortie
            shapewrite(ShapeIn,[pathShpOut 'Access_' shpName '.shp']);
        end
    end
end