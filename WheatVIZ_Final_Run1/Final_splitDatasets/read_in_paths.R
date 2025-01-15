
#Eingabe read in paths:

water_readings2<-
  read_csv2("C:/Users/HabisohnC/Desktop/DigAB/data_Round1/metafiles/water_readings_correctDate_1.CSV")

path_allfiles<-"C:/Users/HabisohnC/Desktop/DigAB/data_Round1/NAS_XLSX_Round1"

tempandothermeta<-read_excel("C:/Users/HabisohnC/Desktop/DigAB/data_Round1/metafiles/Temperature_andOther_GC.xlsx")


Bonitur_1<-read_excel("C:/Users/HabisohnC/Desktop/DigAB/data_Round1/metafiles/WheatVIZ_ClimateChamber_2024_Masterfile.xlsx")

weizenernte_full<-read_excel("C:/Users/HabisohnC/Desktop/DigAB/data_Round1/metafiles/Weizenernte.xlsx")


#Eingabe Beginn und Ende Trockenstressphase

stress_begin<-"2024-02-12"
stress_end<-"2024-03-01"