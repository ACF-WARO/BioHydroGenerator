clear all

% Repertoires entrée et sortie
% cd('../..');
pathD=[pwd '/'];
% pathD=['D:/ACF3/BioHydroPond_v2/'];
pathShpIn=[pathD 'Lib/Shape_Out/'];
pathCsvIn=[pathD 'Data/Out/Biomass/Report/'];
pathShpOut=[pathD 'Data/Out/Biomass/Shape/'];

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
        
        % Anomalie sur Biomass
        % Test existence fichier
        fid=fopen([pathCsvIn 'Biomass_' shpName '.csv']);
        if fid~=-1
            fclose(fid);
            
            % Lecture du fichier statistiques issu de BioGenerator
            CsvInBio = dlmread([pathCsvIn 'Biomass_' shpName '.csv'],';',5,2);
            CsvInVI = dlmread([pathCsvIn 'VI_' shpName '.csv'],';',5,2);
            
            % Nombre d'année
            lastyear=1998+size(CsvInBio,2)-5;
            
            % Extraction vecteur ID de ShapeIn et
            % initialisation des nouveaux
            % champs de sortie
            clear IDin
            for k=1:size(ShapeIn,1)
                IDin(k)=ShapeIn(k).ID;
                ShapeIn(k).('AREA')=-9999.;
                ShapeIn(k).('MEAN')=-9999.;
                for year=1998:lastyear
                    ShapeIn(k).(['BIO_' num2str(year)])=-9999.;
                end
                for year=1998:lastyear
                    ShapeIn(k).(['VI_' num2str(year)])=-9999.;
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
                    ShapeIn(k).('AREA')=CsvInBio(m,2);
                    ShapeIn(k).('MEAN')=CsvInBio(m,3);
                    for year=1998:lastyear
                        ShapeIn(k).(['BIO_' num2str(year)])=CsvInBio(m,year-1998+4);
                        ShapeIn(k).(['VI_' num2str(year)])=CsvInVI(m,year-1998+3);
                    end       
                    %             ShapeIn(k).('R2')=CsvIn(m,lastyear-1998+4+1);
                    %             ShapeIn(k).('Trend')=CsvIn(m,lastyear-1998+4+2);
                    %             ShapeIn(k).('Sigma')=CsvIn(m,lastyear-1998+4+3);
                end
            end
            
            % Ecriture du fichier Shape de sortie
            shapewrite(ShapeIn,[pathShpOut 'BIO_' shpName '.shp']);
        end
    end
end