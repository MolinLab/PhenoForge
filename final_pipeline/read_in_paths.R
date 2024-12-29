
#Eingabe read in paths:

water_readings2<-read_csv2("C:/Users/HabisohnC/Desktop/Folder_StandDerDinge/Round1/metafiles/water_readings_correctDate_1.CSV") #auch dieser Pfad wird in CL übergeben werden

path_allfiles<-"C:/Users/HabisohnC/Desktop/Folder_StandDerDinge/Round1/files/NAS_XLSX_Round1/XLSX"

tempandothermeta<-read_excel("C:/Users/HabisohnC/Desktop/WhaetVIZ_metadata/Temperature_andOther_GC.xlsx")


Bonitur_1<-read_excel("C:/Users/HabisohnC/Desktop/WhaetVIZ_metadata/WheatVIZ_ClimateChamber_2024_Masterfile.xlsx")

weizenernte_full<-read_excel("C:/Users/HabisohnC/Desktop/Folder_StandDerDinge/Round1/metafiles/Weizenernte.xlsx")


#Eingabe Beginn und Ende Trockenstressphase

begin<-as.Date("2024-02-21", format="%Y-%m-%d")
end<-as.Date("2024-01-03", format="%Y-%m-%d")


