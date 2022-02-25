# rclone is a program to download data directly from cloud services
# use `rclone config` to set credentials etc
sync:
	rclone sync --drive-shared-with-me "drive:Carceral Ecologies--LAB SHARED/" drive

wget:
	cd data && wget -nc https://echo.epa.gov/files/echodownloads/SDWA_downloads.zip
	cd data && wget -nc https://echo.epa.gov/files/echodownloads/SDWA_latest_downloads.zip
	cd data && wget -nc https://echo.epa.gov/files/echodownloads/npdes_downloads.zip
	cd data && wget -nc https://echo.epa.gov/files/echodownloads/case_downloads.zip
	cd data && wget -nc --no-check-certificate https://gaftp.epa.gov/EJSCREEN/2015/EJSCREEN_20150505.csv.zip
	cd data && wget -nc --no-check-certificate https://gaftp.epa.gov/EJSCREEN/2016/EJSCREEN_V3_USPR_090216_CSV.zip
	cd data && wget -nc --no-check-certificate https://gaftp.epa.gov/EJSCREEN/2017/EJSCREEN_2017_USPR_Public.csv
	cd data && wget -nc --no-check-certificate https://gaftp.epa.gov/EJSCREEN/2018/EJSCREEN_2018_USPR_csv.zip
	cd data && wget -nc --no-check-certificate https://gaftp.epa.gov/EJSCREEN/2019/EJSCREEN_2019_USPR.csv.zip
	cd data && wget -nc --no-check-certificate https://gaftp.epa.gov/EJSCREEN/2020/EJSCREEN_2020_USPR.csv.zip
