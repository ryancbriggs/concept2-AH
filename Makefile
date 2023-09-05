report/erg.pdf: report/erg.qmd \
	logs/erg.Rout \
	logs/ah_analysis.Rout 
	Rscript -e "quarto::quarto_render(here::here('report/erg.qmd'))"

logs/load.Rout: code/load.R 
	R CMD BATCH code/load.R logs/load.Rout 

logs/erg.Rout: code/erg.R \
	logs/load.Rout \
	text/today.txt \
	data/*.csv 
	R CMD BATCH code/erg.R logs/erg.Rout

logs/ah_xml.Rout: code/ah_xml.R \
	logs/load.Rout \
	data/export.zip
	R CMD BATCH code/ah_xml.R logs/ah_xml.Rout

logs/ah_analysis.Rout: code/ah_analysis.R \
	logs/load.Rout \
	logs/ah_xml.Rout \
	data/ah.feather
	R CMD BATCH code/ah_analysis.R logs/ah_analysis.Rout

