*********************************************************************************************************
************************ The `Brown Penalty' Project: Degryse-Doornik-Mamonov ***************************
************************ Start date: May 2023 ***********************************************************
*********************************************************************************************************

*data preparation
clear all
clear matrix
clear mata
clear results
set more off
capture log close log_file
set mem 10000
set matsize 11000
set maxvar 20000
set linesize 255

** DEFINE LOCATION
	** Mikhail 
	// /*
	global path             "C:\Users\peste\Dropbox\Brazil_The_Brown_Penalty_project"
	global do_file_DDM      "$path\Do_Files"
	global dta_file_DDM     "$path\Dta_Files"
	global output_file_DDM  "$path\Output_Check"
	global raw_datasets_DDM "$path\Raw_Datasets" 

	// */
	
	** BERNARDUS
	/*
		global path "\\SBCDF060\depep01$"
			
		*define base folders
		global source_file  = "$path\Bernardus\DDM\Source_files"
		global do_file = "$path\Bernardus\DDM\Do_files"
		global dta_file = "$path\Bernardus\DDM\Dta_files"
		global output_check = "$path\Bernardus\DDM\Output_check"
		global raw_dataset = "$path\DATA\Dta_files"


		*access ado files

		adopath + "$path/ado"
		adopath ++ "$path/ado"
		adopath + "//SBCDF060/ADO"
		adopath ++ "//SBCDF060/ADO"
		adopath + "//SBCDF776/ado"
		adopath ++ "//SBCDF776/ado"
		adopath + "//SBCDF060/depep01$/ado"
		adopath ++ "//SBCDF060/depep01$/ado"
		adopath + "//SBCDF060/depep01$/ado-776e"
		adopath ++ "//SBCDF060/depep01$/ado-776e"


		*create log file
		cd $output_check
		global logfile_name = "$path\Bernardus\DDM\Output_check\"+string( d(`c(current_date)'), "%dCCYYNNDD" )+"_Merge_CTF-APP_to_SCR.smcl"
		global Table = "$path\Bernardus\DDM\Output_check\"+string( d(`c(current_date)'), "%dCCYYNNDD" )+"_results_Merge_CTF-APP_to_SCR.xls"
		log using $logfile_name ,append
	*/
	

	
******************************************************************************************************************************************
************************** STEP 1: Prepare the CTF/APP-registrations data for merging to the SCR 
******************************************************************************************************************************************

		 
	cd $dta_file_DDM						
	use "CTFAPP firms_1000firms", clear
	set more off
		
		// A) Convert CNPJ to CNPJ_eightdigit
			gen 	CNPJ_eightdigit = substr(CNPJ,1,8)
			replace CNPJ 			= CNPJ_eightdigit
			drop 	CNPJ_eightdigit
				
				
				/*
				// B) Retain small part of the dataset (pilot) to write and test the code on my own computer
					
					bysort CNPJ: gen Firmcounter = _n
					gen NewFirm = Firmcounter == 1
							
					gen Sum = sum(NewFirm)
					keep if Sum <= 1000 // retain the first 1000 firms
							
						*drop Firmcounter
						drop NewFirm
						drop Sum
				*/
				
		// C) Generate time variables				
				
			* Start date *
			gen StartDay = day(StartDate)
			gen StartMonth = month(StartDate)
			gen StartYear = year(StartDate)
								
			gen StartQuarter = .
				replace StartQuarter = 1 if (StartMonth == 1 | StartMonth == 2 | StartMonth == 3)
				replace StartQuarter = 2 if (StartMonth == 4 | StartMonth == 5 | StartMonth == 6)
				replace StartQuarter = 3 if (StartMonth == 7 | StartMonth == 8 | StartMonth == 9)
				replace StartQuarter = 4 if (StartMonth == 10 | StartMonth == 11 | StartMonth == 12)
								
								
				* End date *
			gen EndDay = day(EndDate)
			gen EndMonth = month(EndDate)
			gen EndYear = year(EndDate)
								
			gen EndQuarter = .
				replace EndQuarter = 1 if (EndMonth == 1 | EndMonth == 2 | EndMonth == 3)
				replace EndQuarter = 2 if (EndMonth == 4 | EndMonth == 5 | EndMonth == 6)
				replace EndQuarter = 3 if (EndMonth == 7 | EndMonth == 8 | EndMonth == 9)
				replace EndQuarter = 4 if (EndMonth == 10 | EndMonth == 11 | EndMonth == 12)
								
			gen Ongoing = (EndDate == .)
			gen OneMonth = (StartYear == EndYear & StartMonth == EndMonth)
			
			
		// D) Inspect registration Status
			
			tab RegistrationStatus
				
				/* Five possible values: 
					- Ativa (64%)  																						== "Active"
					- Encerrado (22%)																					== "Terminated"
					- Suspenso para AveriguaçÃµes - dados cadastrais inconsistentes e_ou recadastramento (13%)			== "Suspended for investigations - inconsistent registration data and/or re-registration"
							  (i.e. Averiguaçoes)																		
					- Cadastramento indevido (1%)  																	    == "Improper registration" 
					- Suspenso para averiguaçÃµes (0.2%)																== "Suspended for investigation"
							  (i.e. averiguaçoes)			
				*/
			
			tab RegistrationStatus Ongoing // Note that Ongoing is my own definition: i.e. dummy = 1 if EndDate is missing 
				/* Strange: While the "Ongoing" projects are mostly flagged as "active" in registration status, a substantial number of activities with a specified activity-EndDate is still flagged "active"
				   Logical: The projects flagged as "terminated" are virtually always no longer "ongoing" according to my definition
				
				   Unclear: What to do with 14% that is "suspended"?  
				   --> I suggest we try the analysis once with all registrations, once without the suspended ones*/
						
			*drop if RegistrationStatus != "Ativa" & RegistrationStatus != "Encerrado"

				
		// E) Generate a wide-format data structure for the four Polution Potential possibilities 
		//    + indicate the start and endquarter in this structure 
		//    + delete those newely created vars that correspond to those months that do not exist in the data
				
			tab PollutionPotential Ongoing 
			gen HighPP = (PollutionPotential == "Alto")
			gen MediumPP = (PollutionPotential == "Médio")
			gen LowPP = (PollutionPotential == "Pequeno")
			gen MissingPP = (PollutionPotential == "...")
							
				
			forvalues i = 1989(1)2021	{
				forvalues n = 1(1)12 {
							
						gen HighPP`i'M`n' = 0
							replace HighPP`i'M`n' = 1    if StartYear == `i' & StartMonth == `n' & PollutionPotential == "Alto"
							replace HighPP`i'M`n' = 2    if EndYear == `i'   & EndMonth == `n'   & PollutionPotential == "Alto" & OneMonth != 1
							sum HighPP`i'M`n'
								scalar HighPP`i'M`n'_mean = r(mean)
								if HighPP`i'M`n'_mean == 0 {
									drop HighPP`i'M`n'
									}
							
						gen MediumPP`i'M`n' = 0
							replace MediumPP`i'M`n' = 1  if StartYear == `i' & StartMonth == `n' & PollutionPotential == "Médio"
							replace MediumPP`i'M`n' = 2  if EndYear == `i'   & EndMonth == `n'   & PollutionPotential == "Médio" & OneMonth != 1 
							sum MediumPP`i'M`n'
								scalar MediumPP`i'M`n'_mean = r(mean)
								if MediumPP`i'M`n'_mean == 0 {
									drop MediumPP`i'M`n'
									}
									
						gen LowPP`i'M`n' = 0
							replace LowPP`i'M`n' = 1     if StartYear == `i' & StartMonth == `n' & PollutionPotential == "Pequeno"
							replace LowPP`i'M`n' = 2     if EndYear == `i'   & EndMonth == `n'   & PollutionPotential == "Pequeno" & OneMonth != 1
							sum LowPP`i'M`n'
								scalar LowPP`i'M`n'_mean = r(mean)
								if LowPP`i'M`n'_mean == 0 {
									drop LowPP`i'M`n'
									}
									
						gen MissingPP`i'M`n' = 0
							replace MissingPP`i'M`n' = 1 if StartYear == `i' & StartMonth == `n' & PollutionPotential == "..."
							replace MissingPP`i'M`n' = 2 if EndYear == `i'   & EndMonth == `n'   & PollutionPotential == "..." & OneMonth != 1
							sum MissingPP`i'M`n'
								scalar MissingPP`i'M`n'_mean = r(mean)
								if MissingPP`i'M`n'_mean == 0 {
									drop MissingPP`i'M`n'
									}
				}
			}
							
			/*
				// These months are not present in the data
					drop HighPP1989M1 MediumPP1989M1 LowPP1989M1 MissingPP1989M1 HighPP1989M2 MediumPP1989M2 LowPP1989M2 MissingPP1989M2 HighPP1989M3 MediumPP1989M3 LowPP1989M3 MissingPP1989M3 HighPP1989M4 MediumPP1989M4 LowPP1989M4 MissingPP1989M4 HighPP1989M5 MediumPP1989M5 LowPP1989M5 MissingPP1989M5 HighPP1989M6 MediumPP1989M6 LowPP1989M6 MissingPP1989M6  HighPP2021M10 MediumPP2021M10 LowPP2021M10 MissingPP2021M10 HighPP2021M11 MediumPP2021M11 LowPP2021M11 MissingPP2021M11 HighPP2021M12 MediumPP2021M12 LowPP2021M12 MissingPP2021M12 // not in data
			*/
			
		// F) Based on start and end quarter of an activity, fill up the periods in between 
				
				rename HighPP    PPHigh // I rename these variables only to use the "High*" shortcut to refer to "all variables starting with High" --> i.e. <High*>
				rename MediumPP  PPMedium
				rename LowPP     PPLow
				rename MissingPP PPMissing

			// Order below: first ALL high PP's, then ALL medium, then ALL low, then ALL missing 
			global InitialData      = "CNPJ CNPJ FirmName StartDate EndDate CategoryCode CategoryDescription ActivityCode ActivityDescription PollutionPotential Municipality State Latitude Longitude RegistrationStatus"
			global CreatedDataShort = "PPMissing StartDay StartMonth StartYear StartQuarter EndDay EndMonth EndYear EndQuarter Ongoing OneMonth PPHigh PPMedium PPLow PPMissing"
			global CreatedDataWide  = "High* Medium* Low* Missing*"
			
			order $InitialData $CreatedDataShort $CreatedDataWide
			* This order is important because the code below will fill up the periods by filling up adjacent variables
			
					
						
			* High PP * --- Note: routines below must be executed jointly with the ORDER routine above!
				unab vars : High* // this is why i renamed the variables right above
				tokenize `vars' 

					local nvars : word count `vars' 
						
				forval j = 2/`nvars' { 
						local k = `j' - 1 
						replace ``j'' = 1 if ``k'' == 1 & ``j'' != 2 & /*Ongoing != 1 &*/  OneMonth != 1
						} 
					
						
			* Medium PP * --- Note: routines below must be executed jointly with the ORDER routine above!
				unab vars : Medium*
				tokenize `vars' 

					local nvars : word count `vars' 
						
				forval j = 2/`nvars' { 
						local k = `j' - 1 
						replace ``j'' = 1 if ``k'' == 1 & ``j'' != 2 & /*Ongoing != 1 &*/ OneMonth != 1
						} 
					
						
			* Low PP * --- Note: routines below must be executed jointly with the ORDER routine above!
				unab vars : Low*
				tokenize `vars' 

					local nvars : word count `vars' 
						
				forval j = 2/`nvars' { 
						local k = `j' - 1 
						replace ``j'' = 1 if ``k'' == 1 & ``j'' != 2 & /*Ongoing != 1 &*/ OneMonth != 1
						} 
						
						
			* Missing PP * --- Note: routines below must be executed jointly with the ORDER routine above!
				unab vars : Missing*
				tokenize `vars' 

					local nvars : word count `vars' 
						
				forval j = 2/`nvars' { 
						local k = `j' - 1 
						replace ``j'' = 1 if ``k'' == 1 & ``j'' != 2 & /*Ongoing != 1 &*/ OneMonth != 1
						} 
					
					
		// G) Change the value of the end-quarter (which was indicated with 2 in step E) to 1. --- Note: Manual execution required below in the ``global'' routine
			
			foreach var of varlist _all {
				if `=substr("`var'",1,6)=="HighPP"' | `=substr("`var'",1,8)=="MediumPP"' | `=substr("`var'",1,5)=="LowPP"' | `=substr("`var'",1,9)=="MissingPP"' {
					replace `var' = 1 if `var' == 2
					}
				}
			
						
		// H) Keep one observation per firm AFTER making sure that the information from each of its activities is kept (using the TemporarySum variable)
			
			/*
				// drop below is specific to the monthly regressions (as I only need years 2014-2015-2016)
				drop  HighPP1989M7 HighPP1989M8 HighPP1989M9 HighPP1989M10 HighPP1989M11 HighPP1989M12 HighPP1990M1 HighPP1990M2 HighPP1990M3 HighPP1990M4 HighPP1990M5 HighPP1990M6 HighPP1990M7 HighPP1990M8 HighPP1990M9 HighPP1990M10 HighPP1990M11 HighPP1990M12 HighPP1991M1 HighPP1991M2 HighPP1991M3 HighPP1991M4 HighPP1991M5 HighPP1991M6 HighPP1991M7 HighPP1991M8 HighPP1991M9 HighPP1991M10 HighPP1991M11 HighPP1991M12 HighPP1992M1 HighPP1992M2 HighPP1992M3 HighPP1992M4 HighPP1992M5 HighPP1992M6 HighPP1992M7 HighPP1992M8 HighPP1992M9 HighPP1992M10 HighPP1992M11 HighPP1992M12 HighPP1993M1 HighPP1993M2 HighPP1993M3 HighPP1993M4 HighPP1993M5 HighPP1993M6 HighPP1993M7 HighPP1993M8 HighPP1993M9 HighPP1993M10 HighPP1993M11 HighPP1993M12 HighPP1994M1 HighPP1994M2 HighPP1994M3 HighPP1994M4 HighPP1994M5 HighPP1994M6 HighPP1994M7 HighPP1994M8 HighPP1994M9 HighPP1994M10 HighPP1994M11 HighPP1994M12 HighPP1995M1 HighPP1995M2 HighPP1995M3 HighPP1995M4 HighPP1995M5 HighPP1995M6 HighPP1995M7 HighPP1995M8 HighPP1995M9 HighPP1995M10 HighPP1995M11 HighPP1995M12 HighPP1996M1 HighPP1996M2 HighPP1996M3 HighPP1996M4 HighPP1996M5 HighPP1996M6 HighPP1996M7 HighPP1996M8 HighPP1996M9 HighPP1996M10 HighPP1996M11 HighPP1996M12 HighPP1997M1 HighPP1997M2 HighPP1997M3 HighPP1997M4 HighPP1997M5 HighPP1997M6 HighPP1997M7 HighPP1997M8 HighPP1997M9 HighPP1997M10 HighPP1997M11 HighPP1997M12 HighPP1998M1 HighPP1998M2 HighPP1998M3 HighPP1998M4 HighPP1998M5 HighPP1998M6 HighPP1998M7 HighPP1998M8 HighPP1998M9 HighPP1998M10 HighPP1998M11 HighPP1998M12 HighPP1999M1 HighPP1999M2 HighPP1999M3 HighPP1999M4 HighPP1999M5 HighPP1999M6 HighPP1999M7 HighPP1999M8 HighPP1999M9 HighPP1999M10 HighPP1999M11 HighPP1999M12 HighPP2000M1 HighPP2000M2 HighPP2000M3 HighPP2000M4 HighPP2000M5 HighPP2000M6 HighPP2000M7 HighPP2000M8 HighPP2000M9 HighPP2000M10 HighPP2000M11 HighPP2000M12 HighPP2001M1 HighPP2001M2 HighPP2001M3 HighPP2001M4 HighPP2001M5 HighPP2001M6 HighPP2001M7 HighPP2001M8 HighPP2001M9 HighPP2001M10 HighPP2001M11 HighPP2001M12 HighPP2002M1 HighPP2002M2 HighPP2002M3 HighPP2002M4 HighPP2002M5 HighPP2002M6 HighPP2002M7 HighPP2002M8 HighPP2002M9 HighPP2002M10 HighPP2002M11 HighPP2002M12 HighPP2003M1 HighPP2003M2 HighPP2003M3 HighPP2003M4 HighPP2003M5 HighPP2003M6 HighPP2003M7 HighPP2003M8 HighPP2003M9 HighPP2003M10 HighPP2003M11 HighPP2003M12 HighPP2004M1 HighPP2004M2 HighPP2004M3 HighPP2004M4 HighPP2004M5 HighPP2004M6 HighPP2004M7 HighPP2004M8 HighPP2004M9 HighPP2004M10 HighPP2004M11 HighPP2004M12 HighPP2005M1 HighPP2005M2 HighPP2005M3 HighPP2005M4 HighPP2005M5 HighPP2005M6 HighPP2005M7 HighPP2005M8 HighPP2005M9 HighPP2005M10 HighPP2005M11 HighPP2005M12 HighPP2006M1 HighPP2006M2 HighPP2006M3 HighPP2006M4 HighPP2006M5 HighPP2006M6 HighPP2006M7 HighPP2006M8 HighPP2006M9 HighPP2006M10 HighPP2006M11 HighPP2006M12 HighPP2007M1 HighPP2007M2 HighPP2007M3 HighPP2007M4 HighPP2007M5 HighPP2007M6 HighPP2007M7 HighPP2007M8 HighPP2007M9 HighPP2007M10 HighPP2007M11 HighPP2007M12 HighPP2008M1 HighPP2008M2 HighPP2008M3 HighPP2008M4 HighPP2008M5 HighPP2008M6 HighPP2008M7 HighPP2008M8 HighPP2008M9 HighPP2008M10 HighPP2008M11 HighPP2008M12 HighPP2009M1 HighPP2009M2 HighPP2009M3 HighPP2009M4 HighPP2009M5 HighPP2009M6 HighPP2009M7 HighPP2009M8 HighPP2009M9 HighPP2009M10 HighPP2009M11 HighPP2009M12 HighPP2010M1 HighPP2010M2 HighPP2010M3 HighPP2010M4 HighPP2010M5 HighPP2010M6 HighPP2010M7 HighPP2010M8 HighPP2010M9 HighPP2010M10 HighPP2010M11 HighPP2010M12 HighPP2011M1 HighPP2011M2 HighPP2011M3 HighPP2011M4 HighPP2011M5 HighPP2011M6 HighPP2011M7 HighPP2011M8 HighPP2011M9 HighPP2011M10 HighPP2011M11 HighPP2011M12 HighPP2012M1 HighPP2012M2 HighPP2012M3 HighPP2012M4 HighPP2012M5 HighPP2012M6 HighPP2012M7 HighPP2012M8 HighPP2012M9 HighPP2012M10 HighPP2012M11 HighPP2012M12 HighPP2013M1 HighPP2013M2 HighPP2013M3 HighPP2013M4 HighPP2013M5 HighPP2013M6 HighPP2013M7 HighPP2013M8 HighPP2013M9 HighPP2013M10 HighPP2013M11 HighPP2013M12 HighPP2017M1 HighPP2017M2 HighPP2017M3 HighPP2017M4 HighPP2017M5 HighPP2017M6 HighPP2017M7 HighPP2017M8 HighPP2017M9 HighPP2017M10 HighPP2017M11 HighPP2017M12 HighPP2018M1 HighPP2018M2 HighPP2018M3 HighPP2018M4 HighPP2018M5 HighPP2018M6 HighPP2018M7 HighPP2018M8 HighPP2018M9 HighPP2018M10 HighPP2018M11 HighPP2018M12 HighPP2019M1 HighPP2019M2 HighPP2019M3 HighPP2019M4 HighPP2019M5 HighPP2019M6 HighPP2019M7 HighPP2019M8 HighPP2019M9 HighPP2019M10 HighPP2019M11 HighPP2019M12 HighPP2020M1 HighPP2020M2 HighPP2020M3 HighPP2020M4 HighPP2020M5 HighPP2020M6 HighPP2020M7 HighPP2020M8 HighPP2020M9 HighPP2020M10 HighPP2020M11 HighPP2020M12 HighPP2021M1 HighPP2021M2 HighPP2021M3 HighPP2021M4 HighPP2021M5 HighPP2021M6 HighPP2021M7 HighPP2021M8 HighPP2021M9 MediumPP1989M7 MediumPP1989M8 MediumPP1989M9 MediumPP1989M10 MediumPP1989M11 MediumPP1989M12 MediumPP1990M1 MediumPP1990M2 MediumPP1990M3 MediumPP1990M4 MediumPP1990M5 MediumPP1990M6 MediumPP1990M7 MediumPP1990M8 MediumPP1990M9 MediumPP1990M10 MediumPP1990M11 MediumPP1990M12 MediumPP1991M1 MediumPP1991M2 MediumPP1991M3 MediumPP1991M4 MediumPP1991M5 MediumPP1991M6 MediumPP1991M7 MediumPP1991M8 MediumPP1991M9 MediumPP1991M10 MediumPP1991M11 MediumPP1991M12 MediumPP1992M1 MediumPP1992M2 MediumPP1992M3 MediumPP1992M4 MediumPP1992M5 MediumPP1992M6 MediumPP1992M7 MediumPP1992M8 MediumPP1992M9 MediumPP1992M10 MediumPP1992M11 MediumPP1992M12 MediumPP1993M1 MediumPP1993M2 MediumPP1993M3 MediumPP1993M4 MediumPP1993M5 MediumPP1993M6 MediumPP1993M7 MediumPP1993M8 MediumPP1993M9 MediumPP1993M10 MediumPP1993M11 MediumPP1993M12 MediumPP1994M1 MediumPP1994M2 MediumPP1994M3 MediumPP1994M4 MediumPP1994M5 MediumPP1994M6 MediumPP1994M7 MediumPP1994M8 MediumPP1994M9 MediumPP1994M10 MediumPP1994M11 MediumPP1994M12 MediumPP1995M1 MediumPP1995M2 MediumPP1995M3 MediumPP1995M4 MediumPP1995M5 MediumPP1995M6 MediumPP1995M7 MediumPP1995M8 MediumPP1995M9 MediumPP1995M10 MediumPP1995M11 MediumPP1995M12 MediumPP1996M1 MediumPP1996M2 MediumPP1996M3 MediumPP1996M4 MediumPP1996M5 MediumPP1996M6 MediumPP1996M7 MediumPP1996M8 MediumPP1996M9 MediumPP1996M10 MediumPP1996M11 MediumPP1996M12 MediumPP1997M1 MediumPP1997M2 MediumPP1997M3 MediumPP1997M4 MediumPP1997M5 MediumPP1997M6 MediumPP1997M7 MediumPP1997M8 MediumPP1997M9 MediumPP1997M10 MediumPP1997M11 MediumPP1997M12 MediumPP1998M1 MediumPP1998M2 MediumPP1998M3 MediumPP1998M4 MediumPP1998M5 MediumPP1998M6 MediumPP1998M7 MediumPP1998M8 MediumPP1998M9 MediumPP1998M10 MediumPP1998M11 MediumPP1998M12 MediumPP1999M1 MediumPP1999M2 MediumPP1999M3 MediumPP1999M4 MediumPP1999M5 MediumPP1999M6 MediumPP1999M7 MediumPP1999M8 MediumPP1999M9 MediumPP1999M10 MediumPP1999M11 MediumPP1999M12 MediumPP2000M1 MediumPP2000M2 MediumPP2000M3 MediumPP2000M4 MediumPP2000M5 MediumPP2000M6 MediumPP2000M7 MediumPP2000M8 MediumPP2000M9 MediumPP2000M10 MediumPP2000M11 MediumPP2000M12 MediumPP2001M1 MediumPP2001M2 MediumPP2001M3 MediumPP2001M4 MediumPP2001M5 MediumPP2001M6 MediumPP2001M7 MediumPP2001M8 MediumPP2001M9 MediumPP2001M10 MediumPP2001M11 MediumPP2001M12 MediumPP2002M1 MediumPP2002M2 MediumPP2002M3 MediumPP2002M4 MediumPP2002M5 MediumPP2002M6 MediumPP2002M7 MediumPP2002M8 MediumPP2002M9 MediumPP2002M10 MediumPP2002M11 MediumPP2002M12 MediumPP2003M1 MediumPP2003M2 MediumPP2003M3 MediumPP2003M4 MediumPP2003M5 MediumPP2003M6 MediumPP2003M7 MediumPP2003M8 MediumPP2003M9 MediumPP2003M10 MediumPP2003M11 MediumPP2003M12 MediumPP2004M1 MediumPP2004M2 MediumPP2004M3 MediumPP2004M4 MediumPP2004M5 MediumPP2004M6 MediumPP2004M7 MediumPP2004M8 MediumPP2004M9 MediumPP2004M10 MediumPP2004M11 MediumPP2004M12 MediumPP2005M1 MediumPP2005M2 MediumPP2005M3 MediumPP2005M4 MediumPP2005M5 MediumPP2005M6 MediumPP2005M7 MediumPP2005M8 MediumPP2005M9 MediumPP2005M10 MediumPP2005M11 MediumPP2005M12 MediumPP2006M1 MediumPP2006M2 MediumPP2006M3 MediumPP2006M4 MediumPP2006M5 MediumPP2006M6 MediumPP2006M7 MediumPP2006M8 MediumPP2006M9 MediumPP2006M10 MediumPP2006M11 MediumPP2006M12 MediumPP2007M1 MediumPP2007M2 MediumPP2007M3 MediumPP2007M4 MediumPP2007M5 MediumPP2007M6 MediumPP2007M7 MediumPP2007M8 MediumPP2007M9 MediumPP2007M10 MediumPP2007M11 MediumPP2007M12 MediumPP2008M1 MediumPP2008M2 MediumPP2008M3 MediumPP2008M4 MediumPP2008M5 MediumPP2008M6 MediumPP2008M7 MediumPP2008M8 MediumPP2008M9 MediumPP2008M10 MediumPP2008M11 MediumPP2008M12 MediumPP2009M1 MediumPP2009M2 MediumPP2009M3 MediumPP2009M4 MediumPP2009M5 MediumPP2009M6 MediumPP2009M7 MediumPP2009M8 MediumPP2009M9 MediumPP2009M10 MediumPP2009M11 MediumPP2009M12 MediumPP2010M1 MediumPP2010M2 MediumPP2010M3 MediumPP2010M4 MediumPP2010M5 MediumPP2010M6 MediumPP2010M7 MediumPP2010M8 MediumPP2010M9 MediumPP2010M10 MediumPP2010M11 MediumPP2010M12 MediumPP2011M1 MediumPP2011M2 MediumPP2011M3 MediumPP2011M4 MediumPP2011M5 MediumPP2011M6 MediumPP2011M7 MediumPP2011M8 MediumPP2011M9 MediumPP2011M10 MediumPP2011M11 MediumPP2011M12 MediumPP2012M1 MediumPP2012M2 MediumPP2012M3 MediumPP2012M4 MediumPP2012M5 MediumPP2012M6 MediumPP2012M7 MediumPP2012M8 MediumPP2012M9 MediumPP2012M10 MediumPP2012M11 MediumPP2012M12 MediumPP2013M1 MediumPP2013M2 MediumPP2013M3 MediumPP2013M4 MediumPP2013M5 MediumPP2013M6 MediumPP2013M7 MediumPP2013M8 MediumPP2013M9 MediumPP2013M10 MediumPP2013M11 MediumPP2013M12 MediumPP2017M1 MediumPP2017M2 MediumPP2017M3 MediumPP2017M4 MediumPP2017M5 MediumPP2017M6 MediumPP2017M7 MediumPP2017M8 MediumPP2017M9 MediumPP2017M10 MediumPP2017M11 MediumPP2017M12 MediumPP2018M1 MediumPP2018M2 MediumPP2018M3 MediumPP2018M4 MediumPP2018M5 MediumPP2018M6 MediumPP2018M7 MediumPP2018M8 MediumPP2018M9 MediumPP2018M10 MediumPP2018M11 MediumPP2018M12 MediumPP2019M1 MediumPP2019M2 MediumPP2019M3 MediumPP2019M4 MediumPP2019M5 MediumPP2019M6 MediumPP2019M7 MediumPP2019M8 MediumPP2019M9 MediumPP2019M10 MediumPP2019M11 MediumPP2019M12 MediumPP2020M1 MediumPP2020M2 MediumPP2020M3 MediumPP2020M4 MediumPP2020M5 MediumPP2020M6 MediumPP2020M7 MediumPP2020M8 MediumPP2020M9 MediumPP2020M10 MediumPP2020M11 MediumPP2020M12 MediumPP2021M1 MediumPP2021M2 MediumPP2021M3 MediumPP2021M4 MediumPP2021M5 MediumPP2021M6 MediumPP2021M7 MediumPP2021M8 MediumPP2021M9 LowPP1989M7 LowPP1989M8 LowPP1989M9 LowPP1989M10 LowPP1989M11 LowPP1989M12 LowPP1990M1 LowPP1990M2 LowPP1990M3 LowPP1990M4 LowPP1990M5 LowPP1990M6 LowPP1990M7 LowPP1990M8 LowPP1990M9 LowPP1990M10 LowPP1990M11 LowPP1990M12 LowPP1991M1 LowPP1991M2 LowPP1991M3 LowPP1991M4 LowPP1991M5 LowPP1991M6 LowPP1991M7 LowPP1991M8 LowPP1991M9 LowPP1991M10 LowPP1991M11 LowPP1991M12 LowPP1992M1 LowPP1992M2 LowPP1992M3 LowPP1992M4 LowPP1992M5 LowPP1992M6 LowPP1992M7 LowPP1992M8 LowPP1992M9 LowPP1992M10 LowPP1992M11 LowPP1992M12 LowPP1993M1 LowPP1993M2 LowPP1993M3 LowPP1993M4 LowPP1993M5 LowPP1993M6 LowPP1993M7 LowPP1993M8 LowPP1993M9 LowPP1993M10 LowPP1993M11 LowPP1993M12 LowPP1994M1 LowPP1994M2 LowPP1994M3 LowPP1994M4 LowPP1994M5 LowPP1994M6 LowPP1994M7 LowPP1994M8 LowPP1994M9 LowPP1994M10 LowPP1994M11 LowPP1994M12 LowPP1995M1 LowPP1995M2 LowPP1995M3 LowPP1995M4 LowPP1995M5 LowPP1995M6 LowPP1995M7 LowPP1995M8 LowPP1995M9 LowPP1995M10 LowPP1995M11 LowPP1995M12 LowPP1996M1 LowPP1996M2 LowPP1996M3 LowPP1996M4 LowPP1996M5 LowPP1996M6 LowPP1996M7 LowPP1996M8 LowPP1996M9 LowPP1996M10 LowPP1996M11 LowPP1996M12 LowPP1997M1 LowPP1997M2 LowPP1997M3 LowPP1997M4 LowPP1997M5 LowPP1997M6 LowPP1997M7 LowPP1997M8 LowPP1997M9 LowPP1997M10 LowPP1997M11 LowPP1997M12 LowPP1998M1 LowPP1998M2 LowPP1998M3 LowPP1998M4 LowPP1998M5 LowPP1998M6 LowPP1998M7 LowPP1998M8 LowPP1998M9 LowPP1998M10 LowPP1998M11 LowPP1998M12 LowPP1999M1 LowPP1999M2 LowPP1999M3 LowPP1999M4 LowPP1999M5 LowPP1999M6 LowPP1999M7 LowPP1999M8 LowPP1999M9 LowPP1999M10 LowPP1999M11 LowPP1999M12 LowPP2000M1 LowPP2000M2 LowPP2000M3 LowPP2000M4 LowPP2000M5 LowPP2000M6 LowPP2000M7 LowPP2000M8 LowPP2000M9 LowPP2000M10 LowPP2000M11 LowPP2000M12 LowPP2001M1 LowPP2001M2 LowPP2001M3 LowPP2001M4 LowPP2001M5 LowPP2001M6 LowPP2001M7 LowPP2001M8 LowPP2001M9 LowPP2001M10 LowPP2001M11 LowPP2001M12 LowPP2002M1 LowPP2002M2 LowPP2002M3 LowPP2002M4 LowPP2002M5 LowPP2002M6 LowPP2002M7 LowPP2002M8 LowPP2002M9 LowPP2002M10 LowPP2002M11 LowPP2002M12 LowPP2003M1 LowPP2003M2 LowPP2003M3 LowPP2003M4 LowPP2003M5 LowPP2003M6 LowPP2003M7 LowPP2003M8 LowPP2003M9 LowPP2003M10 LowPP2003M11 LowPP2003M12 LowPP2004M1 LowPP2004M2 LowPP2004M3 LowPP2004M4 LowPP2004M5 LowPP2004M6 LowPP2004M7 LowPP2004M8 LowPP2004M9 LowPP2004M10 LowPP2004M11 LowPP2004M12 LowPP2005M1 LowPP2005M2 LowPP2005M3 LowPP2005M4 LowPP2005M5 LowPP2005M6 LowPP2005M7 LowPP2005M8 LowPP2005M9 LowPP2005M10 LowPP2005M11 LowPP2005M12 LowPP2006M1 LowPP2006M2 LowPP2006M3 LowPP2006M4 LowPP2006M5 LowPP2006M6 LowPP2006M7 LowPP2006M8 LowPP2006M9 LowPP2006M10 LowPP2006M11 LowPP2006M12 LowPP2007M1 LowPP2007M2 LowPP2007M3 LowPP2007M4 LowPP2007M5 LowPP2007M6 LowPP2007M7 LowPP2007M8 LowPP2007M9 LowPP2007M10 LowPP2007M11 LowPP2007M12 LowPP2008M1 LowPP2008M2 LowPP2008M3 LowPP2008M4 LowPP2008M5 LowPP2008M6 LowPP2008M7 LowPP2008M8 LowPP2008M9 LowPP2008M10 LowPP2008M11 LowPP2008M12 LowPP2009M1 LowPP2009M2 LowPP2009M3 LowPP2009M4 LowPP2009M5 LowPP2009M6 LowPP2009M7 LowPP2009M8 LowPP2009M9 LowPP2009M10 LowPP2009M11 LowPP2009M12 LowPP2010M1 LowPP2010M2 LowPP2010M3 LowPP2010M4 LowPP2010M5 LowPP2010M6 LowPP2010M7 LowPP2010M8 LowPP2010M9 LowPP2010M10 LowPP2010M11 LowPP2010M12 LowPP2011M1 LowPP2011M2 LowPP2011M3 LowPP2011M4 LowPP2011M5 LowPP2011M6 LowPP2011M7 LowPP2011M8 LowPP2011M9 LowPP2011M10 LowPP2011M11 LowPP2011M12 LowPP2012M1 LowPP2012M2 LowPP2012M3 LowPP2012M4 LowPP2012M5 LowPP2012M6 LowPP2012M7 LowPP2012M8 LowPP2012M9 LowPP2012M10 LowPP2012M11 LowPP2012M12 LowPP2013M1 LowPP2013M2 LowPP2013M3 LowPP2013M4 LowPP2013M5 LowPP2013M6 LowPP2013M7 LowPP2013M8 LowPP2013M9 LowPP2013M10 LowPP2013M11 LowPP2013M12 LowPP2017M1 LowPP2017M2 LowPP2017M3 LowPP2017M4 LowPP2017M5 LowPP2017M6 LowPP2017M7 LowPP2017M8 LowPP2017M9 LowPP2017M10 LowPP2017M11 LowPP2017M12 LowPP2018M1 LowPP2018M2 LowPP2018M3 LowPP2018M4 LowPP2018M5 LowPP2018M6 LowPP2018M7 LowPP2018M8 LowPP2018M9 LowPP2018M10 LowPP2018M11 LowPP2018M12 LowPP2019M1 LowPP2019M2 LowPP2019M3 LowPP2019M4 LowPP2019M5 LowPP2019M6 LowPP2019M7 LowPP2019M8 LowPP2019M9 LowPP2019M10 LowPP2019M11 LowPP2019M12 LowPP2020M1 LowPP2020M2 LowPP2020M3 LowPP2020M4 LowPP2020M5 LowPP2020M6 LowPP2020M7 LowPP2020M8 LowPP2020M9 LowPP2020M10 LowPP2020M11 LowPP2020M12 LowPP2021M1 LowPP2021M2 LowPP2021M3 LowPP2021M4 LowPP2021M5 LowPP2021M6 LowPP2021M7 LowPP2021M8 LowPP2021M9 MissingPP1989M7 MissingPP1989M8 MissingPP1989M9 MissingPP1989M10 MissingPP1989M11 MissingPP1989M12 MissingPP1990M1 MissingPP1990M2 MissingPP1990M3 MissingPP1990M4 MissingPP1990M5 MissingPP1990M6 MissingPP1990M7 MissingPP1990M8 MissingPP1990M9 MissingPP1990M10 MissingPP1990M11 MissingPP1990M12 MissingPP1991M1 MissingPP1991M2 MissingPP1991M3 MissingPP1991M4 MissingPP1991M5 MissingPP1991M6 MissingPP1991M7 MissingPP1991M8 MissingPP1991M9 MissingPP1991M10 MissingPP1991M11 MissingPP1991M12 MissingPP1992M1 MissingPP1992M2 MissingPP1992M3 MissingPP1992M4 MissingPP1992M5 MissingPP1992M6 MissingPP1992M7 MissingPP1992M8 MissingPP1992M9 MissingPP1992M10 MissingPP1992M11 MissingPP1992M12 MissingPP1993M1 MissingPP1993M2 MissingPP1993M3 MissingPP1993M4 MissingPP1993M5 MissingPP1993M6 MissingPP1993M7 MissingPP1993M8 MissingPP1993M9 MissingPP1993M10 MissingPP1993M11 MissingPP1993M12 MissingPP1994M1 MissingPP1994M2 MissingPP1994M3 MissingPP1994M4 MissingPP1994M5 MissingPP1994M6 MissingPP1994M7 MissingPP1994M8 MissingPP1994M9 MissingPP1994M10 MissingPP1994M11 MissingPP1994M12 MissingPP1995M1 MissingPP1995M2 MissingPP1995M3 MissingPP1995M4 MissingPP1995M5 MissingPP1995M6 MissingPP1995M7 MissingPP1995M8 MissingPP1995M9 MissingPP1995M10 MissingPP1995M11 MissingPP1995M12 MissingPP1996M1 MissingPP1996M2 MissingPP1996M3 MissingPP1996M4 MissingPP1996M5 MissingPP1996M6 MissingPP1996M7 MissingPP1996M8 MissingPP1996M9 MissingPP1996M10 MissingPP1996M11 MissingPP1996M12 MissingPP1997M1 MissingPP1997M2 MissingPP1997M3 MissingPP1997M4 MissingPP1997M5 MissingPP1997M6 MissingPP1997M7 MissingPP1997M8 MissingPP1997M9 MissingPP1997M10 MissingPP1997M11 MissingPP1997M12 MissingPP1998M1 MissingPP1998M2 MissingPP1998M3 MissingPP1998M4 MissingPP1998M5 MissingPP1998M6 MissingPP1998M7 MissingPP1998M8 MissingPP1998M9 MissingPP1998M10 MissingPP1998M11 MissingPP1998M12 MissingPP1999M1 MissingPP1999M2 MissingPP1999M3 MissingPP1999M4 MissingPP1999M5 MissingPP1999M6 MissingPP1999M7 MissingPP1999M8 MissingPP1999M9 MissingPP1999M10 MissingPP1999M11 MissingPP1999M12 MissingPP2000M1 MissingPP2000M2 MissingPP2000M3 MissingPP2000M4 MissingPP2000M5 MissingPP2000M6 MissingPP2000M7 MissingPP2000M8 MissingPP2000M9 MissingPP2000M10 MissingPP2000M11 MissingPP2000M12 MissingPP2001M1 MissingPP2001M2 MissingPP2001M3 MissingPP2001M4 MissingPP2001M5 MissingPP2001M6 MissingPP2001M7 MissingPP2001M8 MissingPP2001M9 MissingPP2001M10 MissingPP2001M11 MissingPP2001M12 MissingPP2002M1 MissingPP2002M2 MissingPP2002M3 MissingPP2002M4 MissingPP2002M5 MissingPP2002M6 MissingPP2002M7 MissingPP2002M8 MissingPP2002M9 MissingPP2002M10 MissingPP2002M11 MissingPP2002M12 MissingPP2003M1 MissingPP2003M2 MissingPP2003M3 MissingPP2003M4 MissingPP2003M5 MissingPP2003M6 MissingPP2003M7 MissingPP2003M8 MissingPP2003M9 MissingPP2003M10 MissingPP2003M11 MissingPP2003M12 MissingPP2004M1 MissingPP2004M2 MissingPP2004M3 MissingPP2004M4 MissingPP2004M5 MissingPP2004M6 MissingPP2004M7 MissingPP2004M8 MissingPP2004M9 MissingPP2004M10 MissingPP2004M11 MissingPP2004M12 MissingPP2005M1 MissingPP2005M2 MissingPP2005M3 MissingPP2005M4 MissingPP2005M5 MissingPP2005M6 MissingPP2005M7 MissingPP2005M8 MissingPP2005M9 MissingPP2005M10 MissingPP2005M11 MissingPP2005M12 MissingPP2006M1 MissingPP2006M2 MissingPP2006M3 MissingPP2006M4 MissingPP2006M5 MissingPP2006M6 MissingPP2006M7 MissingPP2006M8 MissingPP2006M9 MissingPP2006M10 MissingPP2006M11 MissingPP2006M12 MissingPP2007M1 MissingPP2007M2 MissingPP2007M3 MissingPP2007M4 MissingPP2007M5 MissingPP2007M6 MissingPP2007M7 MissingPP2007M8 MissingPP2007M9 MissingPP2007M10 MissingPP2007M11 MissingPP2007M12 MissingPP2008M1 MissingPP2008M2 MissingPP2008M3 MissingPP2008M4 MissingPP2008M5 MissingPP2008M6 MissingPP2008M7 MissingPP2008M8 MissingPP2008M9 MissingPP2008M10 MissingPP2008M11 MissingPP2008M12 MissingPP2009M1 MissingPP2009M2 MissingPP2009M3 MissingPP2009M4 MissingPP2009M5 MissingPP2009M6 MissingPP2009M7 MissingPP2009M8 MissingPP2009M9 MissingPP2009M10 MissingPP2009M11 MissingPP2009M12 MissingPP2010M1 MissingPP2010M2 MissingPP2010M3 MissingPP2010M4 MissingPP2010M5 MissingPP2010M6 MissingPP2010M7 MissingPP2010M8 MissingPP2010M9 MissingPP2010M10 MissingPP2010M11 MissingPP2010M12 MissingPP2011M1 MissingPP2011M2 MissingPP2011M3 MissingPP2011M4 MissingPP2011M5 MissingPP2011M6 MissingPP2011M7 MissingPP2011M8 MissingPP2011M9 MissingPP2011M10 MissingPP2011M11 MissingPP2011M12 MissingPP2012M1 MissingPP2012M2 MissingPP2012M3 MissingPP2012M4 MissingPP2012M5 MissingPP2012M6 MissingPP2012M7 MissingPP2012M8 MissingPP2012M9 MissingPP2012M10 MissingPP2012M11 MissingPP2012M12 MissingPP2013M1 MissingPP2013M2 MissingPP2013M3 MissingPP2013M4 MissingPP2013M5 MissingPP2013M6 MissingPP2013M7 MissingPP2013M8 MissingPP2013M9 MissingPP2013M10 MissingPP2013M11 MissingPP2013M12 MissingPP2017M1 MissingPP2017M2 MissingPP2017M3 MissingPP2017M4 MissingPP2017M5 MissingPP2017M6 MissingPP2017M7 MissingPP2017M8 MissingPP2017M9 MissingPP2017M10 MissingPP2017M11 MissingPP2017M12 MissingPP2018M1 MissingPP2018M2 MissingPP2018M3 MissingPP2018M4 MissingPP2018M5 MissingPP2018M6 MissingPP2018M7 MissingPP2018M8 MissingPP2018M9 MissingPP2018M10 MissingPP2018M11 MissingPP2018M12 MissingPP2019M1 MissingPP2019M2 MissingPP2019M3 MissingPP2019M4 MissingPP2019M5 MissingPP2019M6 MissingPP2019M7 MissingPP2019M8 MissingPP2019M9 MissingPP2019M10 MissingPP2019M11 MissingPP2019M12 MissingPP2020M1 MissingPP2020M2 MissingPP2020M3 MissingPP2020M4 MissingPP2020M5 MissingPP2020M6 MissingPP2020M7 MissingPP2020M8 MissingPP2020M9 MissingPP2020M10 MissingPP2020M11 MissingPP2020M12 MissingPP2021M1 MissingPP2021M2 MissingPP2021M3 MissingPP2021M4 MissingPP2021M5 MissingPP2021M6 MissingPP2021M7 MissingPP2021M8 MissingPP2021M9
			*/
			
			
			/* SHORTCUT CODE (less data-demanding)
			foreach i in HighPP1989Q3 HighPP1989Q4 HighPP1990Q1 HighPP1990Q2 HighPP1990Q3 HighPP1990Q4 HighPP1991Q1 HighPP1991Q2 HighPP1991Q3 HighPP1991Q4 HighPP1992Q1 HighPP1992Q2 HighPP1992Q3 HighPP1992Q4 HighPP1993Q1 HighPP1993Q2 HighPP1993Q3 HighPP1993Q4 HighPP1994Q1 HighPP1994Q2 HighPP1994Q3 HighPP1994Q4 HighPP1995Q1 HighPP1995Q2 HighPP1995Q3 HighPP1995Q4 HighPP1996Q1 HighPP1996Q2 HighPP1996Q3 HighPP1996Q4 HighPP1997Q1 HighPP1997Q2 HighPP1997Q3 HighPP1997Q4 HighPP1998Q1 HighPP1998Q2 HighPP1998Q3 HighPP1998Q4 HighPP1999Q1 HighPP1999Q2 HighPP1999Q3 HighPP1999Q4 HighPP2000Q1 HighPP2000Q2 HighPP2000Q3 HighPP2000Q4 HighPP2001Q1 HighPP2001Q2 HighPP2001Q3 HighPP2001Q4 HighPP2002Q1 HighPP2002Q2 HighPP2002Q3 HighPP2002Q4 HighPP2003Q1 HighPP2003Q2 HighPP2003Q3 HighPP2003Q4 HighPP2004Q1 HighPP2004Q2 HighPP2004Q3 HighPP2004Q4 HighPP2005Q1 HighPP2005Q2 HighPP2005Q3 HighPP2005Q4 HighPP2006Q1 HighPP2006Q2 HighPP2006Q3 HighPP2006Q4 HighPP2007Q1 HighPP2007Q2 HighPP2007Q3 HighPP2007Q4 HighPP2008Q1 HighPP2008Q2 HighPP2008Q3 HighPP2008Q4 HighPP2009Q1 HighPP2009Q2 HighPP2009Q3 HighPP2009Q4 HighPP2010Q1 HighPP2010Q2 HighPP2010Q3 HighPP2010Q4 HighPP2011Q1 HighPP2011Q2 HighPP2011Q3 HighPP2011Q4 HighPP2012Q1 HighPP2012Q2 HighPP2012Q3 HighPP2012Q4 HighPP2013Q1 HighPP2013Q2 HighPP2013Q3 HighPP2013Q4 HighPP2014Q1 HighPP2014Q2 HighPP2014Q3 HighPP2014Q4 HighPP2015Q1 HighPP2015Q2 HighPP2015Q3 HighPP2015Q4 HighPP2016Q1 HighPP2016Q2 HighPP2016Q3 HighPP2016Q4 HighPP2017Q1 HighPP2017Q2 HighPP2017Q3 HighPP2017Q4 HighPP2018Q1 HighPP2018Q2 HighPP2018Q3 HighPP2018Q4 HighPP2019Q1 HighPP2019Q2 HighPP2019Q3 HighPP2019Q4 HighPP2020Q1 HighPP2020Q2 HighPP2020Q3 HighPP2020Q4 HighPP2021Q1 HighPP2021Q2 HighPP2021Q3 MediumPP1989Q3 MediumPP1989Q4 MediumPP1990Q1 MediumPP1990Q2 MediumPP1990Q3 MediumPP1990Q4 MediumPP1991Q1 MediumPP1991Q2 MediumPP1991Q3 MediumPP1991Q4 MediumPP1992Q1 MediumPP1992Q2 MediumPP1992Q3 MediumPP1992Q4 MediumPP1993Q1 MediumPP1993Q2 MediumPP1993Q3 MediumPP1993Q4 MediumPP1994Q1 MediumPP1994Q2 MediumPP1994Q3 MediumPP1994Q4 MediumPP1995Q1 MediumPP1995Q2 MediumPP1995Q3 MediumPP1995Q4 MediumPP1996Q1 MediumPP1996Q2 MediumPP1996Q3 MediumPP1996Q4 MediumPP1997Q1 MediumPP1997Q2 MediumPP1997Q3 MediumPP1997Q4 MediumPP1998Q1 MediumPP1998Q2 MediumPP1998Q3 MediumPP1998Q4 MediumPP1999Q1 MediumPP1999Q2 MediumPP1999Q3 MediumPP1999Q4 MediumPP2000Q1 MediumPP2000Q2 MediumPP2000Q3 MediumPP2000Q4 MediumPP2001Q1 MediumPP2001Q2 MediumPP2001Q3 MediumPP2001Q4 MediumPP2002Q1 MediumPP2002Q2 MediumPP2002Q3 MediumPP2002Q4 MediumPP2003Q1 MediumPP2003Q2 MediumPP2003Q3 MediumPP2003Q4 MediumPP2004Q1 MediumPP2004Q2 MediumPP2004Q3 MediumPP2004Q4 MediumPP2005Q1 MediumPP2005Q2 MediumPP2005Q3 MediumPP2005Q4 MediumPP2006Q1 MediumPP2006Q2 MediumPP2006Q3 MediumPP2006Q4 MediumPP2007Q1 MediumPP2007Q2 MediumPP2007Q3 MediumPP2007Q4 MediumPP2008Q1 MediumPP2008Q2 MediumPP2008Q3 MediumPP2008Q4 MediumPP2009Q1 MediumPP2009Q2 MediumPP2009Q3 MediumPP2009Q4 MediumPP2010Q1 MediumPP2010Q2 MediumPP2010Q3 MediumPP2010Q4 MediumPP2011Q1 MediumPP2011Q2 MediumPP2011Q3 MediumPP2011Q4 MediumPP2012Q1 MediumPP2012Q2 MediumPP2012Q3 MediumPP2012Q4 MediumPP2013Q1 MediumPP2013Q2 MediumPP2013Q3 MediumPP2013Q4 MediumPP2014Q1 MediumPP2014Q2 MediumPP2014Q3 MediumPP2014Q4 MediumPP2015Q1 MediumPP2015Q2 MediumPP2015Q3 MediumPP2015Q4 MediumPP2016Q1 MediumPP2016Q2 MediumPP2016Q3 MediumPP2016Q4 MediumPP2017Q1 MediumPP2017Q2 MediumPP2017Q3 MediumPP2017Q4 MediumPP2018Q1 MediumPP2018Q2 MediumPP2018Q3 MediumPP2018Q4 MediumPP2019Q1 MediumPP2019Q2 MediumPP2019Q3 MediumPP2019Q4 MediumPP2020Q1 MediumPP2020Q2 MediumPP2020Q3 MediumPP2020Q4 MediumPP2021Q1 MediumPP2021Q2 MediumPP2021Q3 LowPP1989Q3 LowPP1989Q4 LowPP1990Q1 LowPP1990Q2 LowPP1990Q3 LowPP1990Q4 LowPP1991Q1 LowPP1991Q2 LowPP1991Q3 LowPP1991Q4 LowPP1992Q1 LowPP1992Q2 LowPP1992Q3 LowPP1992Q4 LowPP1993Q1 LowPP1993Q2 LowPP1993Q3 LowPP1993Q4 LowPP1994Q1 LowPP1994Q2 LowPP1994Q3 LowPP1994Q4 LowPP1995Q1 LowPP1995Q2 LowPP1995Q3 LowPP1995Q4 LowPP1996Q1 LowPP1996Q2 LowPP1996Q3 LowPP1996Q4 LowPP1997Q1 LowPP1997Q2 LowPP1997Q3 LowPP1997Q4 LowPP1998Q1 LowPP1998Q2 LowPP1998Q3 LowPP1998Q4 LowPP1999Q1 LowPP1999Q2 LowPP1999Q3 LowPP1999Q4 LowPP2000Q1 LowPP2000Q2 LowPP2000Q3 LowPP2000Q4 LowPP2001Q1 LowPP2001Q2 LowPP2001Q3 LowPP2001Q4 LowPP2002Q1 LowPP2002Q2 LowPP2002Q3 LowPP2002Q4 LowPP2003Q1 LowPP2003Q2 LowPP2003Q3 LowPP2003Q4 LowPP2004Q1 LowPP2004Q2 LowPP2004Q3 LowPP2004Q4 LowPP2005Q1 LowPP2005Q2 LowPP2005Q3 LowPP2005Q4 LowPP2006Q1 LowPP2006Q2 LowPP2006Q3 LowPP2006Q4 LowPP2007Q1 LowPP2007Q2 LowPP2007Q3 LowPP2007Q4 LowPP2008Q1 LowPP2008Q2 LowPP2008Q3 LowPP2008Q4 LowPP2009Q1 LowPP2009Q2 LowPP2009Q3 LowPP2009Q4 LowPP2010Q1 LowPP2010Q2 LowPP2010Q3 LowPP2010Q4 LowPP2011Q1 LowPP2011Q2 LowPP2011Q3 LowPP2011Q4 LowPP2012Q1 LowPP2012Q2 LowPP2012Q3 LowPP2012Q4 LowPP2013Q1 LowPP2013Q2 LowPP2013Q3 LowPP2013Q4 LowPP2014Q1 LowPP2014Q2 LowPP2014Q3 LowPP2014Q4 LowPP2015Q1 LowPP2015Q2 LowPP2015Q3 LowPP2015Q4 LowPP2016Q1 LowPP2016Q2 LowPP2016Q3 LowPP2016Q4 LowPP2017Q1 LowPP2017Q2 LowPP2017Q3 LowPP2017Q4 LowPP2018Q1 LowPP2018Q2 LowPP2018Q3 LowPP2018Q4 LowPP2019Q1 LowPP2019Q2 LowPP2019Q3 LowPP2019Q4 LowPP2020Q1 LowPP2020Q2 LowPP2020Q3 LowPP2020Q4 LowPP2021Q1 LowPP2021Q2 LowPP2021Q3 MissingPP1989Q3 MissingPP1989Q4 MissingPP1990Q1 MissingPP1990Q2 MissingPP1990Q3 MissingPP1990Q4 MissingPP1991Q1 MissingPP1991Q2 MissingPP1991Q3 MissingPP1991Q4 MissingPP1992Q1 MissingPP1992Q2 MissingPP1992Q3 MissingPP1992Q4 MissingPP1993Q1 MissingPP1993Q2 MissingPP1993Q3 MissingPP1993Q4 MissingPP1994Q1 MissingPP1994Q2 MissingPP1994Q3 MissingPP1994Q4 MissingPP1995Q1 MissingPP1995Q2 MissingPP1995Q3 MissingPP1995Q4 MissingPP1996Q1 MissingPP1996Q2 MissingPP1996Q3 MissingPP1996Q4 MissingPP1997Q1 MissingPP1997Q2 MissingPP1997Q3 MissingPP1997Q4 MissingPP1998Q1 MissingPP1998Q2 MissingPP1998Q3 MissingPP1998Q4 MissingPP1999Q1 MissingPP1999Q2 MissingPP1999Q3 MissingPP1999Q4 MissingPP2000Q1 MissingPP2000Q2 MissingPP2000Q3 MissingPP2000Q4 MissingPP2001Q1 MissingPP2001Q2 MissingPP2001Q3 MissingPP2001Q4 MissingPP2002Q1 MissingPP2002Q2 MissingPP2002Q3 MissingPP2002Q4 MissingPP2003Q1 MissingPP2003Q2 MissingPP2003Q3 MissingPP2003Q4 MissingPP2004Q1 MissingPP2004Q2 MissingPP2004Q3 MissingPP2004Q4 MissingPP2005Q1 MissingPP2005Q2 MissingPP2005Q3 MissingPP2005Q4 MissingPP2006Q1 MissingPP2006Q2 MissingPP2006Q3 MissingPP2006Q4 MissingPP2007Q1 MissingPP2007Q2 MissingPP2007Q3 MissingPP2007Q4 MissingPP2008Q1 MissingPP2008Q2 MissingPP2008Q3 MissingPP2008Q4 MissingPP2009Q1 MissingPP2009Q2 MissingPP2009Q3 MissingPP2009Q4 MissingPP2010Q1 MissingPP2010Q2 MissingPP2010Q3 MissingPP2010Q4 MissingPP2011Q1 MissingPP2011Q2 MissingPP2011Q3 MissingPP2011Q4 MissingPP2012Q1 MissingPP2012Q2 MissingPP2012Q3 MissingPP2012Q4 MissingPP2013Q1 MissingPP2013Q2 MissingPP2013Q3 MissingPP2013Q4 MissingPP2014Q1 MissingPP2014Q2 MissingPP2014Q3 MissingPP2014Q4 MissingPP2015Q1 MissingPP2015Q2 MissingPP2015Q3 MissingPP2015Q4 MissingPP2016Q1 MissingPP2016Q2 MissingPP2016Q3 MissingPP2016Q4 MissingPP2017Q1 MissingPP2017Q2 MissingPP2017Q3 MissingPP2017Q4 MissingPP2018Q1 MissingPP2018Q2 MissingPP2018Q3 MissingPP2018Q4 MissingPP2019Q1 MissingPP2019Q2 MissingPP2019Q3 MissingPP2019Q4 MissingPP2020Q1 MissingPP2020Q2 MissingPP2020Q3 MissingPP2020Q4 MissingPP2021Q1 MissingPP2021Q2 MissingPP2021Q3 {
					bysort CNPJ: egen TemporarySum = sum(`i')
					replace `i' = 1 if TemporarySum >= 1
					drop TemporarySum
					}
			*/							
									
				
			* ALTERNATIVELY: in code below we retain an additional variable per Quarter*PP combination in order to indicates the total nr of activities with equal PP in a given quarter (		
				foreach i of global PP_WideVar_remained {
						bysort CNPJ: egen NRofAct`i' = sum(`i')
						replace `i' = 1 if NRofAct`i' >= 1
						}
											
									/* TEST CODE: 	To check if the code above worked: there shouldn't be any withing CNPJ*quarter variation anymore
									
									foreach i of global PP_WideVar_remained {		
											count if `i' == 1
											bysort CNPJ (`i'): gen X = (`i'[1] != `i'[_N])
												tab X
												drop X
												}
									*/
											
			bysort CNPJ: gen counter = _n
			keep if counter == 1 // retain 1 observation per firm
				drop counter
			
			
		// I)Drop activity specific information (which only pertains to the first observation per CNPJ, so it's basically random information)
				* Note that the structure of the data does retain information on pollution potential 
			
			drop StartDate EndDate CategoryCode CategoryDescription ActivityCode ActivityDescription PollutionPotential RegistrationStatus StartDay StartMonth StartYear StartQuarter EndDay EndMonth EndYear EndQuarter Ongoing OneMonth PPHigh PPMedium PPLow PPMissing				
		
			
		// J) Reshape to long format	
		
			reshape long HighPP MediumPP LowPP MissingPP NRofActHighPP NRofActMediumPP NRofActLowPP NRofActMissingPP, i(CNPJ) j(Time_ID_string) string 
			
			gen Year = substr(Time_ID_string, 1, 4)
			gen Month = substr(Time_ID_string, 6, 2)
			
				* Generate the same time-format as in the SCR*
					replace Month = "01" if Month == "1"
					replace Month = "02" if Month == "2"
					replace Month = "03" if Month == "3"
					replace Month = "04" if Month == "4"
					replace Month = "05" if Month == "5"
					replace Month = "06" if Month == "6"
					replace Month = "07" if Month == "7"
					replace Month = "08" if Month == "8"
					replace Month = "09" if Month == "9"
				
					gen time_id = Year + Month
						destring time_id, replace
						destring Year, replace
						
					gen firm_id = CNPJ	
					
			order CNPJ firm_id time_id Time_ID Year Month Time_ID_string HighPP MediumPP LowPP MissingPP NRofActHighPP NRofActMediumPP NRofActLowPP NRofActMissingPP FirmName Municipality State Latitude Longitude			
			
			destring firm_id, replace
	
	
	cd $dta_file		
	save "CTFAPP_sample_monthly_DDM", replace
	
	use "CTFAPP_sample_monthly_DDM", clear
	
		// drop if Year < 2014 | Year > 2016
		
		*** Drop inactive firms
		egen TotalNRofAct = rowtotal(NRofActHighPP NRofActMediumPP NRofActLowPP NRofActMissingPP)
		gen Registered = (TotalNRofAct > 0)
		
		bysort firm: egen SumofRegistrations = sum(Registered)
		drop if SumofRegistrations == 0
		
		drop TotalNRofAct Registered SumofRegistrations
	
	save "CTFAPP_sample_monthly_active_DDM", replace
		
	

	
******************************************************************************************************************************************
************************** STEP 2: Merging CTF/APP registrations to the SCR 
******************************************************************************************************************************************
	
	scalar StartYear = 2011
	scalar EndYear   = 2011
	
	//Prepare the credit register files
	forvalues year = `=scalar(StartYear)'/`=scalar(EndYear)' {
		
		local file_dates 01 02 03 04 05 06 07 08 09 10 11 12
		
		foreach x of local file_dates {
	
		
	cd $raw_datasets_DDM
	use "SCR_FAKE_`year'`x'.dta", clear
	*destring firm_id, replace force
	drop time_id
	gen Year = `year'
	gen Month = "`x'" 
	egen time_id = concat(Year Month)
	destring time_id, replace
	
		*Retain observations from firms that have at least one activity registered --- ? How are we sure that firm_id in the SCR and in the CTF/APP are the same?
		cd $dta_file_DDM	
		merge m:1 firm_id time_id using "CTFAPP_sample_monthly_active_DDM", keepusing(HighPP MediumPP LowPP MissingPP NRofActHighPP NRofActMediumPP NRofActLowPP NRofActMissingPP FirmName Municipality State Latitude Longitude) 
			keep if _merge==3
			drop _merge

		*Dummy for collateral
		* gen dummy_collateral = (collateral_fid!=0 | collateral_nfid!=0 | collateral_value!=0) 			// This works for the SCR in >= 2014 ... ? In SCR-2011 there are no ~_fid and ~_nfid
		gen dummy_collateral = (collateral_type!=0 | collateral_evaluation_date!=0 | collateral_value!=0) 	// This works for the SCR in = 2011
  
		*Maintain one line per loan at each database --- ? This effectively eliminates duplicates... but why collateral value differs within a given set of duplicates?
		bysort loan_id bank_id: gen first = _n==1
		keep if first ==1
		capture rename loan_resource loan_origin
		keep time_id bank_id firm_id loan_id conglomerate_id loan_outstanding loan_unreleased loan_credit_line loan_arrears_90_days loan_arrears_90_180_days loan_arrears_over_180_days loan_losses loan_type loan_origin loan_rating loan_base_rate loan_index_rate loan_start_date loan_end_date firm_size firm_nature firm_control firm_industry firm_location firm_bank_start_date firm_start_date dummy_collateral HighPP MediumPP LowPP MissingPP NRofActHighPP NRofActMediumPP NRofActLowPP NRofActMissingPP FirmName Municipality State Latitude Longitude

		*Debt exposure
		gen amount = loan_outstanding

		*Keep only private firms (dropping public firms using firm_control and firm_nature)
		gen gov_owned = (firm_control!=1 | firm_nature<205)
		bysort firm_id: egen max_gov_owned = max(gov_owned)
		*drop if max_gov_owned==1
		*drop gov_owned max_gov_owned

		*Drop financial institutions (dropping using firm_industry)
		destring firm_industry, replace force
		gen finance = (firm_industry>6400 & firm_industry<6799)
		bysort firm_id: egen max_finance = max(finance)
		drop if max_finance==1
		drop finance max_finance

		*Drop firms if there is a presence of interbank loans --- ? What does this mean, given that you drop financial institutions just above?
		gen interbank_loans = (loan_type==1401)
		bysort firm_id: egen max_interbank_loans = max(interbank_loans)
		drop if max_interbank_loans==1
		drop interbank_loans max_interbank_loans

		*Dummy for loans that used bank's private resources only
		gen dummy_origin = (loan_origin<200)
		
		* Lines below are probably important for the project with Maxim but not for ours
		/*
			*Credit Unions are not allowed to lend certain types of credit, such as real estate, exportation loans. Therefore we keep loan types common to Banks and Credit Unions (~94% of the loans)
			gen allowed_type=(loan_type<500 | (loan_type>800 & loan_type<900) | (loan_type>1300 & loan_type<1400) | (loan_type>1900 & loan_type<2000) )
			*keep if allowed_type==1
			*drop allowed_type
		*/
		
		*Date variables in Stata format
			replace loan_start_date = dofc(loan_start_date)
			replace loan_start_date =. if loan_start_date<=-21549 // -21549= "01jan1901"do "C:\Users\u0118269\AppData\Local\Temp\STD01000000.tmp"
			format loan_start_date %td
			
			replace loan_end_date = dofc(loan_end_date)
			replace loan_end_date =. if loan_end_date <=-21549 // -21549= "01jan1901"
			format loan_end_date %td
			
			replace firm_start_date = dofc(firm_start_date)
			replace firm_start_date =. if firm_start_date<=-21549 // -21549= "01jan1901"
			format firm_start_date %td
			
			replace firm_bank_start_date = dofc(firm_bank_start_date)
			replace firm_bank_start_date =. if firm_bank_start_date<=-21549 // -21549= "01jan1901"
			format firm_bank_start_date %td

		**Dummy for new credit in a rigorous approach 
			*firm_start_date<=firm_bank_start_date<=loan_start_date<loan_end_date & loan_start_date_year==time_id_year & loan_start_date_quarter==time_id_quarter
			
			*Year, quarter and date given the variable time_id
			tostring time_id, gen(time_id_string)
			gen time_id_year  = substr(time_id_string, 1, 4)
			gen time_id_month = substr(time_id_string, 5,2) // used to be gen time_id_quarter
			destring time_id_year, replace
			destring time_id_month, replace
			
			gen time_id_quarter = .
				replace time_id_quarter= 1 if (time_id_month == 1 | time_id_month == 2 | time_id_month == 3)
				replace time_id_quarter= 2 if (time_id_month == 4 | time_id_month == 5 | time_id_month == 6)
				replace time_id_quarter= 3 if (time_id_month == 7 | time_id_month == 8 | time_id_month == 9)
				replace time_id_quarter= 4 if (time_id_month == 10 | time_id_month == 11 | time_id_month == 12)
			
			gen time_id_YMD = ""
				replace time_id_YMD = time_id_string + "31" if time_id_month == 1 | time_id_month == 3 | time_id_month == 5 | time_id_month == 7 | time_id_month == 8 | time_id_month == 10 | time_id_month == 12 // 31 days if March and December
				replace time_id_YMD = time_id_string + "30" if time_id_month == 4 | time_id_month == 6 | time_id_month == 9 | time_id_month == 11 
				replace time_id_YMD = time_id_string + "28" if time_id_month == 2
			
			gen time_id_date=date(time_id_YMD, "YMD")
			format time_id_date %td
			drop time_id_string time_id_YMD

			*Year and quarter given the variable loan_start_date
			gen loan_start_date_year  = year(loan_start_date)
			gen loan_start_date_month = month(loan_start_date)
			gen loan_start_date_quarter = .
				replace loan_start_date_quarter = 1 if (loan_start_date_month == 1 | loan_start_date_month == 2 | loan_start_date_month == 3)
				replace loan_start_date_quarter = 2 if (loan_start_date_month == 4 | loan_start_date_month == 5 | loan_start_date_month == 6)
				replace loan_start_date_quarter = 3 if (loan_start_date_month == 7 | loan_start_date_month == 8 | loan_start_date_month == 9)
				replace loan_start_date_quarter = 4 if (loan_start_date_month == 10 | loan_start_date_month == 11 | loan_start_date_month == 12)

			*Dummy for new credit
			gen dummy_new = (firm_start_date<=firm_bank_start_date & firm_bank_start_date<=loan_start_date & loan_start_date<loan_end_date & ///
							 loan_start_date_year==time_id_year & loan_start_date_quarter==time_id_quarter & loan_start_date_month == time_id_month)
			drop time_id_year time_id_quarter loan_start_date_year loan_start_date_quarter

		*Contractual maturity
		gen 	maturity_contract = loan_end_date - loan_start_date
		replace maturity_contract = . if maturity_contract<1 | maturity_contract>12775 //max possible in Brazil not above 35 years * 365 days
		replace maturity_contract = maturity_contract/365
		
		*Remaining maturity
		gen 	maturity_remain = loan_end_date - time_id_date
		replace maturity_remain = . if maturity_remain<0 | maturity_remain>12775
		replace maturity_remain = maturity_remain/365
		
		*Proxy for state/province location --- Why only 2 first numbers? Mixing, say, 1100 and 11000. And it's a numeric var---no substr taken
		
		gen 	 firm_state = firm_location // substr(firm_location, 1,2)
		/*
		destring firm_state, replace force
		replace  firm_state = . if firm_state<0
		drop     firm_location
		*/
		
		*Interest rate
		gen 	interest = loan_base_rate
		replace interest = . if interest<0 | interest>1000 //Interest below 0% or above 1000% a year is most probably a mistake.  
		

		**Rating
			*Rating - linear transformation
			*"AA" is the best rating and "HH" is the worst, following Resolution 2,682/1999 of the Central Bank of Brazil.
			**label def rating_def 10 "AA" 9 "A " 8 "B " 7 "C " 6 "D " 5 "E " 4 "F " 3 "G " 2 "H " 1 "HH"
			**encode loan_rating, gen(rating) label(rating_def)
			gen     cod_rating = 5  if loan_rating=="AA"
			replace cod_rating = 10 if loan_rating=="A "
			replace cod_rating = 15 if loan_rating=="B "
			replace cod_rating = 20 if loan_rating=="C "
			replace cod_rating = 25 if loan_rating=="D "
			replace cod_rating = 30 if loan_rating=="E "
			replace cod_rating = 35 if loan_rating=="F "
			replace cod_rating = 40 if loan_rating=="G "
			replace cod_rating = 45 if loan_rating=="H "
			replace cod_rating = 50 if loan_rating=="HH"
			//encode  cod_rating, gen(rating)
			//label list rating
			
			
			*Rating - provision transformation, following Resolution 2,682/1999 of the Central Bank of Brazil.
			*Provision is zero for loans rated at "AA" and provision is 100% for loans rated "H" and "HH".
			*For the label command, we use --> percent * 10  
			gen     provision = 0    if cod_rating==5
			replace provision = 0.5  if cod_rating==10
			replace provision = 1.0  if cod_rating==15
			replace provision = 3.0  if cod_rating==20
			replace provision = 10.0 if cod_rating==25
			replace provision = 30.0 if cod_rating==30
			replace provision = 50.0 if cod_rating==35
			replace provision = 70.0 if cod_rating==40
			replace provision = 100  if cod_rating==45
			replace provision = 100  if cod_rating==50
			replace provision = .    if cod_rating==-2
			replace provision = .    if cod_rating==1
			
			

		*Dummy for default greater than 90 days
		gen dummy_default = ( ( loan_arrears_90_180_days + loan_arrears_over_180_days)>0 )

		
		**Variables at the firm-bank-time level
			*Defaulted loan
			bysort bank_id firm_id: egen default = max(dummy_default)
			*drop if dummy_default==1
				
				*Defaulted firm
				bysort firm_id: egen default_firm_level = max(dummy_default)
				drop dummy_default

			*Total exposure
			bysort bank_id firm_id: egen amount_bank = total(amount)
			gen ln_amount = ln(1+amount_bank)
		
			*Total amount of new credit
			bysort bank_id firm_id dummy_new: egen amount_new = total(amount)
			replace amount_new = 0 if dummy_new==0
			gen  ln_amount_new = ln(1+amount_new)
			drop dummy_new

			*Weighted risk
			gen provision_100 = amount/amount_bank*(provision/100)
			bysort bank_id firm_id: egen risk = total(provision_100)
			replace risk = 1 if risk>1
			drop provision_100 provision				

			*Weighted contractual maturity
			gen amount_m = amount if maturity_contract!=.
			bysort bank_id firm_id: egen amount_maturity = total(amount_m)
			gen maturity_ratio = amount/amount_maturity*(maturity_contract)
			bysort bank_id firm_id: egen contract_maturity = total(maturity_ratio)
			drop maturity_ratio maturity_contract amount_m amount_maturity

			*Weighted remaining maturity
			gen amount_m = amount if maturity_remain!=.
			bysort bank_id firm_id: egen amount_maturity=total(amount_m)
			gen maturity_ratio=amount/amount_maturity*(maturity_remain)
			bysort bank_id firm_id: egen remain_maturity=total(maturity_ratio)
			drop maturity_ratio maturity_remain amount_m amount_maturity

			*Weighted interest
			gen amount_i = amount if interest!=.
			bysort bank_id firm_id: egen amount_interest = total(amount_i)
			gen interest_ratio = amount/amount_interest*(interest)
			bysort bank_id firm_id: egen total_interest = total(interest_ratio)
			replace total_interest=100 if total_interest>100 //capping interest rate at a maximum of 100% since we had a max of 792% in our previous descriptive statistics, which seems implausible.
			drop interest_ratio interest amount_i amount_interest

			*Weighted collateral
			gen collateral_ratio = amount/amount_bank*(dummy_collateral)
			bysort bank_id firm_id: egen collateral = total(collateral_ratio)
			replace collateral = 1 if collateral>1
			replace collateral = 0 if collateral<0
			drop collateral_ratio dummy_collateral

			*Number of loans
			bysort bank_id firm_id loan_id: gen n_loans =_n==1
			bysort bank_id firm_id: replace n_loans = sum(n_loans)
			bysort bank_id firm_id: replace n_loans = n_loans[_N]

			*Number of banking relationships (firm-level)
			bysort bank_id firm_id: gen n_relationships = _n==1
				bysort firm_id: replace n_relationships = sum(n_relationships)
				bysort firm_id: replace n_relationships = n_relationships[_N]

			*Market share (= share of a firm's credit that is serviced by the bank in question --> bank level variable)
			bysort firm_id: egen amount_firm = total(amount)
			gen market_share = amount_bank/amount_firm
			replace market_share=1 if market_share>1
			drop amount_firm amount

			*Dummy for primary_bank
			bysort firm_id: egen max_share = max(market_share)
			gen primary_bank = (max_share==market_share)
			drop max_share

			*Relationship duration
			replace firm_bank_start_date = . if firm_start_date>firm_bank_start_date
			bysort bank_id firm_id: egen mean_start = mean(firm_bank_start_date)
			bysort bank_id firm_id: egen max_start  = min(firm_bank_start_date)
			gen rel_duration     = (time_id_date - mean_start)/365
			gen rel_duration_max = (time_id_date - max_start)/365
			replace rel_duration = . if rel_duration<0
			replace rel_duration_max = . if rel_duration_max<0

				*Dummy for oldest bank (firm-level)
				bysort firm_id: egen min_start     = min(mean_start)
				bysort firm_id: egen min_start_max = min(max_start)
				gen oldest_bank     = (min_start == mean_start)
				gen oldest_bank_max = (min_start_max == max_start)
				drop mean_start min_start firm_bank_start_date
				drop max_start min_start_max 

				*Firm age (firm-level) 
				bysort firm_id: egen mean_start = mean(firm_start_date) // firm_start_date = first entry into financial system
				gen     firm_age = (time_id_date - mean_start)/365
				replace firm_age = . if firm_age<0
				drop mean_start firm_start_date
				
				*Firm size (firm-level)
				bysort firm_id: replace firm_size = . if firm_size < 1
				count if firm_size == .
				bysort firm_id: egen mean_firm_size = mean(firm_size)
				replace mean_firm_size = 1 if mean_firm_size < 1.5
				replace mean_firm_size = 2 if mean_firm_size > 1.4 & mean_firm_size < 2.5
				replace mean_firm_size = 3 if mean_firm_size > 2.4 & mean_firm_size < 3.5
				replace mean_firm_size = 4 if mean_firm_size > 3.4
				drop firm_size
				rename mean_firm_size firm_size

		*Collapse to firm-bank-time level
		bysort firm_id bank_id: gen first = _n==1
		keep if first == 1
		keep time_id bank_id firm_id conglomerate_id firm_industry firm_state default default_firm_level risk ln_amount ln_amount_new amount_bank amount_new contract_maturity remain_maturity total_interest collateral n_loans n_relationships market_share primary_bank rel_duration rel_duration_max oldest_bank oldest_bank_max firm_age firm_size firm_nature HighPP MediumPP LowPP MissingPP NRofActHighPP NRofActMediumPP NRofActLowPP NRofActMissingPP FirmName Municipality State Latitude Longitude gov_owned max_gov_owned 

		*Sum variables
		sum

		*Compress file
		compress

		*Save file
		cd $dta_file_DDM
		save "CTFAPP_SCR_monthly_`year'`x'", replace
	}
	}
	*
	
	
	
	// Append the credit register files
		
		scalar StartYear = 2011
		scalar EndYear   = 2011
		scalar StartMonth = "01"
		
		cd $dta_file_DDM
		use "CTFAPP_SCR_monthly_`=scalar(StartYear)'`=scalar(StartMonth)'.dta", clear
			
		forvalues year = `=scalar(StartYear)'/`=scalar(EndYear)' {
			local file_dates 02 03 04 05 06 07 08 09 10 11 12
			foreach x of local file_dates {
			
				cd $dta_file_DDM
				append using "CTFAPP_SCR_monthly_`year'`x'.dta"

			}
			}
		
		cd $dta_file_DDM
		save "CTFAPP_SCR_monthly_`=scalar(StartYear)'to`=scalar(EndYear)'", replace


		
		
		
*******************
*******************
************************** STEP 3: Generating additional regression variables
	

	
			/* MAXIM directory to sample dataset
		cd $dta_file_SDD_Y\SCR	
		use "CTFAPP_SCR_sample_final.dta", clear
			*/
	
		egen TotalNRofAct = rowtotal(NRofActHighPP NRofActMediumPP NRofActLowPP NRofActMissingPP)
		
		
			// Share of certain type of activity over all activities
		gen ShareHighPP = NRofActHighPP/TotalNRofAct * 100
		gen ShareMediumPP = NRofActMediumPP/TotalNRofAct * 100
		gen ShareLowPP = NRofActLowPP/TotalNRofAct * 100
		gen ShareMissingPP = NRofActMissingPP/TotalNRofAct * 100
			
			replace ShareHighPP = 0 if missing(ShareHighPP)
			replace ShareMediumPP = 0 if missing(ShareMediumPP)
			replace ShareLowPP = 0 if missing(ShareLowPP)
			replace ShareMissingPP = 0 if missing(ShareMissingPP)
	
							/*
						// relative changes in the share (can also fix this in the regressions)
					bysort CNPJ: gen DeltaShareHighPP = (ShareHighPP[_n]-ShareHighPP[_n-1])/ShareHighPP[_n-1]
					bysort CNPJ: gen DeltaShareMediumPP = (ShareMediumPP[_n]-ShareMediumPP[_n-1])/ShareMediumPP[_n-1]
					bysort CNPJ: gen DeltaShareLowPP = (ShareLowPP[_n]-ShareLowPP[_n-1])/ShareLowPP[_n-1]
					bysort CNPJ: gen DeltaShareMissingPP = (ShareMissingPP[_n]-ShareMissingPP[_n-1])/ShareMissingPP[_n-1] 
					
						replace DeltaShareHighPP = 0 if missing(DeltaShareHighPP)
						replace DeltaShareMediumPP = 0 if missing(DeltaShareMediumPP)
						replace DeltaShareLowPP = 0 if missing(DeltaShareLowPP)
						replace DeltaShareMissingPP = 0 if missing(DeltaShareMissingPP)
							*/
							
		gen PostParis = (time_id >= 201600)
	*/
	
*******************
*******************
************************** STEP 4: First suggestions for regressions (only with DeltaShareHighPP as main explanatory variable)
											/* 
			**********************************
			* Generate the prototype dataset *					
			**********************************		
				
				cd $dta_file_SDD_Y\SCR	
				use "CTFAPP_SCR_sample.dta", clear
									
					// Generate fictitious time_id
						replace time_id = . 
						// Manual adjustment necessary here! See below
							// ==> I copied a new sequence of time_id's into the data sample in order to have sufficient consequent periods to construct growth rates
							// the resulting dataset is saved below
						
						*save "CTFAPP_SCR_sample_new.dta", replace
						use "CTFAPP_SCR_sample_new.dta", clear
							
					// Extend the dataset
							forvalues j = 1/100 {
								append using "CTFAPP_SCR_sample_new.dta"
							}
							
							*save "CTFAPP_SCR_sample_extended.dta", replace 
							*use "CTFAPP_SCR_sample_extended.dta", clear
					
					// Generate fictitious firm_id, bank_id, and firm_industry
						br firm_id bank_id firm_industry 
						drop firm_id bank_id firm_industry 
						
							gen firm_id = 10*uniform()
							replace firm_id = round(firm_id)
								replace firm_id = 1 if firm_id == 0
							
							gen bank_id = 10*uniform()
							replace bank_id = round(bank_id)
								replace bank_id = 1 if bank_id == 0 
								
							gen firm_industry = 10*uniform()
							replace firm_industry = round(firm_industry)
								replace firm_industry = 1 if firm_industry == 0 
					
					// Retain one observation per F*B*Y like in the original data sample
						bysort time_id firm_id bank_id: gen counter = _n 
						keep if counter == 1
							drop counter
						
				save "CTFAPP_SCR_sample_final.dta", replace
				
					****************************							
									
									
									
		/* MAXIM directory to sample data
	cd $dta_file_SDD_Y\SCR	
	use "CTFAPP_SCR_sample_final.dta", clear
		*/
			*/						
	cd $dta_file	
	use "CTFAPP_SCR_monthly", clear
	
			*keep if time_id == 201412 | time_id == 201503 | time_id == 201506 | time_id == 201509
	
			gen PostParis = (time_id >= 201600)
			
			gen PRSA = 0
				replace PRSA = 1 if bank_id == 7237373
				replace PRSA = 1 if bank_id == 33657248
				replace PRSA = 1 if conglomerate_id == "C0010045"
				replace PRSA = 1 if conglomerate_id == "C0010069"
				replace PRSA = 1 if conglomerate_id == "C0030173"
				replace PRSA = 1 if conglomerate_id == "C0030379"
				replace PRSA = 1 if conglomerate_id == "C0030403"
				replace PRSA = 1 if conglomerate_id == "C0049906"
				replace PRSA = 1 if conglomerate_id == "C0049944"
				replace PRSA = 1 if conglomerate_id == "C0051011"
				replace PRSA = 1 if conglomerate_id == "C0051626"
				
					/* MAXIM code
					gen PRSA = (firm_size == 1)
					gen PostPRSA = (time_id >= 200506)
					*/
				
				* The first batch of PRSA banks has to comply starting from 28 February 2015; second batch from 31 July 2015 
			gen PostPRSA = (time_id >= 201503) 
			

									
		// Generate growth rate versions of Y variables
			egen panelid = group(firm_id bank_id)
			xtset panelid time_id
			
				sort panelid time_id
				*br time_id panelid firm_id bank_id amount_bank ln_amount
			
			*gen g_amount_bank = D.amount_bank / L.amount_bank // relative change
			*gen g_ln_amount = D.ln_amount // log approximation of relative change 
				// difference operator somehow doesn't work ...
			
			bysort panelid (time_id): gen g_amount_bank = (amount_bank[_n] - amount_bank[_n-1]) / amount_bank[_n-1]
			bysort panelid (time_id): gen g_ln_amount = (ln_amount[_n] - ln_amount[_n-1])
			
			bysort panelid (time_id): gen g_amount_new = (amount_new[_n] - amount_new[_n-1]) / amount_new[_n-1]
			bysort panelid (time_id): gen g_ln_amount_new = (ln_amount_new[_n] - ln_amount_new[_n-1])
			
		
		// Keep only observations from 2011
			*drop if time_id < 201103
			
			
		// Bank heterogeneity variables		
			cd $raw2
			merge m:1 bank_id time_id using "Unicad.dta", keepusing (bank_type bank_macrosegment bank_control bank_situation)
			drop if _merge==2 // we can drop observations that are found in the using file (UNICAD) but not in the master file (SCR)
			drop _merge	
					
										/* For MAXIM to be able to run test the code
									rename gov_owned gov_owned_firm

									gen gov_owned = (firm_size == 1)
									gen foreign = (firm_size == 2)
									gen priv_dom = (firm_size == 3)
										*/
				
				rename gov_owned gov_owned_firm
			*Dummy for public bank (Control is from federal or provincial government or Development Banks, Development Agencies, Banco do Brasil, Saving Bank - CEF, and the National Development Bank - BNDES) 
				gen gov_owned = (bank_control==1 | bank_control==2 | bank_type=="D" | bank_type=="K" | bank_type=="L" | bank_type=="M" | bank_type=="N")
			*Dummy for foreign bank
				gen foreign=(bank_control==4 & (bank_type=="B" | bank_type=="U" | bank_type=="I") )
			*Dummy for private domestic bank
				gen priv_dom=((bank_control==3 | bank_control==5 | bank_control==.) & (bank_type=="B" | bank_type=="U" | bank_type=="I") )

				drop if gov_owned == 0 & foreign == 0 & priv_dom == 0
		
		
		// Firm heterogeneity: size - fixed at start of sample period!
			
			preserve 
				keep firm_id time_id firm_size
				
				bysort firm_id time_id: gen counter = _n
					keep if counter == 1
					drop counter
					
				xtset firm_id time_id
				
				gen firm_size_lagged = firm_size[_n-1]
				
				cd $dta_file	
					save  "firm_size_lagged.dta", replace
			restore 
			
			merge m:1 time_id firm_id using  "firm_size_lagged.dta", keepusing(firm_size_lagged)
						
			
			
		// Generate main interaction variables
				
				* Pollution potential dummy
			gen pollution_potential = (HighPP == 1 | MediumPP == 1 | LowPP == 1)
			gen pollution_potential2 = (HighPP == 1 | MediumPP == 1)
			
				* Paris interactions
			gen PPxPostParis = pollution_potential * PostParis
			gen PPxPostParis2 = pollution_potential2 * PostParis
			
				gen HighPPxPostParis = HighPP * PostParis
				gen MediumPPxPostParis = MediumPP * PostParis
				gen LowPPxPostParis = LowPP * PostParis
				
							* PRSA interactions
						gen PPxPostPRSA = pollution_potential * PostPRSA
						gen PPxPostPRSA2 = pollution_potential2 * PostPRSA
					
							gen HighPPxPostPRSA = HighPP * PostPRSA
							gen MediumPPxPostPRSA = MediumPP * PostPRSA
							gen LowPPxPostPRSA = LowPP * PostPRSA
						
				* Bank heterogeneity interactions
			gen PPxGov_owned = pollution_potential * gov_owned
			gen PPxForeign = pollution_potential * foreign
			gen PPxPriv_dom = pollution_potential * priv_dom
			
			gen PPxGov_owned2 = pollution_potential2 * gov_owned
			gen PPxForeign2 = pollution_potential2 * foreign
			gen PPxPriv_dom2 = pollution_potential2 * priv_dom
			
				gen HighPPxGov_owned = HighPP * gov_owned
				gen MediumPPxGov_owned = MediumPP * gov_owned
				gen LowPPxGov_owned = LowPP * gov_owned
				
				gen HighPPxForeign = HighPP * foreign
				gen MediumPPxForeign = MediumPP * foreign
				gen LowPPxForeign = LowPP * foreign
				
				gen HighPPxPriv_dom = HighPP * priv_dom
				gen MediumPPxPriv_dom = MediumPP * priv_dom
				gen LowPPxPriv_dom = LowPP * priv_dom
				
							
							* Bank heterogeneity interactions
						gen PPxPRSA = pollution_potential * PRSA
						gen PPxPRSA2 = pollution_potential2 * PRSA
						
							gen HighPPxPRSA = HighPP * PRSA
							gen MediumPPxPRSA = MediumPP * PRSA
							gen LowPPxPRSA = LowPP * PRSA
							
							
			// For triple interaction
			gen Gov_ownedxPostParis = gov_owned * PostParis 
			gen ForeignxPostParis = foreign * PostParis
			gen Priv_domxPostParis = priv_dom * PostParis
			
			gen PPxGov_ownedxPostParis = pollution_potential * gov_owned * PostParis 
			gen PPxForeignxPostParis = pollution_potential * foreign * PostParis
			gen PPxPriv_domxPostParis = pollution_potential * priv_dom * PostParis
				* no intensity triple int yet
				
			gen PPxGov_ownedxPostParis2 = pollution_potential2 * gov_owned * PostParis 
			gen PPxForeignxPostParis2 = pollution_potential2 * foreign * PostParis
			gen PPxPriv_domxPostParis2 = pollution_potential2 * priv_dom * PostParis
				* no intensity triple int yet
				
						
									// For triple interaction
						gen PRSAxPostPRSA = gov_owned * PostPRSA 
						
						gen PPxPRSAxPostPRSA = pollution_potential * PRSA * PostPRSA 
						gen PPxPRSAxPostPRSA2 = pollution_potential2 * PRSA * PostPRSA 
							* no intensity triple int yet
						
						gen HighPPxPRSAxPostPRSA = HighPP * PRSA * PostPRSA
						gen MediumPPxPRSAxPostPRSA = MediumPP * PRSA * PostPRSA
						gen LowPPxPRSAxPostPRSA = LowPP * PRSA * PostPRSA 
	
		
					// Check the amount of firm-level variation over time of "polution_potential"
									
								preserve // 
									bysort firm_id time_id: gen counter = _n
									keep if counter == 1 
									bysort firm_id: gen total = _N
									
									bysort firm_id (time_id): gen change = (pollution_potential[_n] != pollution_potential[_n-1])
									bysort firm_id: egen totalchange = total(change)
									gen variation = totalchange / total
										sum totalchange, det
										sum variation, det
								restore
									
		
		
		// Generate globals to facilitate regression commands
			global controls risk /*remain_maturity*/ total_interest collateral primary_bank /*rel_duration_max*/ n_relationships default_firm_level i.firm_size_lagged
			
			global treatmentintensity  HighPP MediumPP LowPP
				global treatmentintensity2 HighPP MediumPP
			*global treatmentintensitycontinuous ShareHighPP ShareMediumPP ShareLowPP
			
			global paris PostParis PPxPostParis	
				global paris2 PostParis PPxPostParis2
				
							global PRSA PostPRSA PPxPostPRSA	
								global PRSA2 PostPRSA PPxPostPRSA2
			
			
			global paris_intensity PostParis HighPPxPostParis MediumPPxPostParis LowPPxPostParis
				global paris_intensity2 PostParis HighPPxPostParis MediumPPxPostParis // = without "LOW"
				
							global PRSA_intensity PostPRSA HighPPxPostPRSA MediumPPxPostPRSA LowPPxPostPRSA
								global PRSA_intensity2 PostPRSA HighPPxPostPRSA MediumPPxPostPRSA // = without "LOW"
			
			
			global bank_heterogeneity gov_owned foreign /*priv_dom*/ PPxGov_owned PPxForeign /*PPxPriv_dom*/ 
				global bank_heterogeneity2 gov_owned foreign /*priv_dom*/ PPxGov_owned2 PPxForeign2 /*PPxPriv_dom2*/
				
							global PRSA_heterogeneity PRSA PPxPRSA 
								global PRSA_heterogeneity2 PRSA PPxPRSA
															
				
			global bank_heterogeneity_intensity gov_owned foreign priv_dom HighPPxGov_owned MediumPPxGov_owned LowPPxGov_owned HighPPxForeign MediumPPxForeign LowPPxForeign HighPPxPriv_dom MediumPPxPriv_dom LowPPxPriv_dom
				global bank_heterogeneity_intensity2 gov_owned foreign priv_dom HighPPxGov_owned MediumPPxGov_owned  HighPPxForeign MediumPPxForeign  HighPPxPriv_dom MediumPPxPriv_dom // = without "LOW"
			
							global PRSA_heterogeneity_intensity PRSA HighPPxPRSA MediumPPxPRSA LowPPxPRSA
								global PRSA_heterogeneity_intensity2 PRSA HighPPxPRSA MediumPPxPRSA 			
			
			global TRIPLE /*pollution_potential*/ gov_owned foreign /*priv_dom*/ PostParis PPxGov_owned PPxForeign /*PPxPriv_dom*/ Gov_ownedxPostParis ForeignxPostParis /*Priv_domxPostParis*/ PPxPostParis PPxGov_ownedxPostParis PPxForeignxPostParis /*PPxPriv_domxPostParis*/
				global TRIPLE2 /*pollution_potential2*/ gov_owned foreign /*priv_dom*/ PostParis PPxGov_owned2 PPxForeign2 /*PPxPriv_dom2*/ Gov_ownedxPostParis ForeignxPostParis /*Priv_domxPostParis*/ PPxPostParis2 PPxGov_ownedxPostParis2 PPxForeignxPostParis2 /*PPxPriv_domxPostParis2*/	
				
							global PRSA_TRIPLE pollution_potential PRSA PostPRSA PPxPRSA PRSAxPostPRSA PPxPostPRSA PPxPRSAxPostPRSA
								global PRSA_TRIPLE2 pollution_potential2 PRSA PostPRSA PPxPRSA2 PRSAxPostPRSA PPxPostPRSA2 PPxPRSAxPostPRSA2
							
							
							global PRSA_TRIPLE_INTENSITY HighPP MediumPP LowPP PRSA PostPRSA  PRSAxPostPRSA HighPPxPRSA MediumPPxPRSA LowPPxPRSA HighPPxPostPRSA MediumPPxPostPRSA LowPPxPostPRSA HighPPxPRSAxPostPRSA MediumPPxPRSAxPostPRSA LowPPxPRSAxPostPRSA
								global PRSA_TRIPLE_INTENSITY2 HighPP MediumPP PRSA PostPRSA  PRSAxPostPRSA HighPPxPRSA MediumPPxPRSA HighPPxPostPRSA MediumPPxPostPRSA HighPPxPRSAxPostPRSA MediumPPxPRSAxPostPRSA // without LOW
		// Regression analysis
		// ssc install reghdfe
		set more off
				
			/*	ROUND 1
			
				// Pollution potential dummy
			foreach i in amount_bank ln_amount g_amount_bank g_ln_amount /*amount_new ln_amount_new g_amount_new g_ln_amount_new*/ {
				reg `i' pollution_potential
				reghdfe `i' pollution_potential, absorb(i.firm_id i.bank_id i.time_id)
				reg `i' pollution_potential $controls
				reghdfe `i' pollution_potential $controls, absorb(i.firm_id i.bank_id i.time_id) 
					display "--- NEW DEPENDENT VAR ---"
				}
				
				
				// Pollution potential intensity
			foreach i in amount_bank ln_amount g_amount_bank g_ln_amount /*amount_new ln_amount_new g_amount_new g_ln_amount_new*/ {
				reg `i' $treatmentintensity
				reghdfe `i' $treatmentintensity, absorb(i.firm_id i.bank_id i.time_id) 
				reg `i' $treatmentintensity $controls 
				reghdfe `i' $treatmentintensity $controls, absorb(i.firm_id i.bank_id i.time_id) 
					display "--- NEW DEPENDENT VAR ---"
				}
				
				
				// Pollution potential x Paris
			foreach i in amount_bank ln_amount g_amount_bank g_ln_amount /*amount_new ln_amount_new g_amount_new g_ln_amount_new*/ {
				reg `i' pollution_potential $paris
				reghdfe `i' pollution_potential $paris, absorb(i.firm_id i.bank_id i.time_id) 
				reg `i' pollution_potential $paris $controls
				reghdfe `i' pollution_potential $paris $controls, absorb(i.firm_id i.bank_id i.time_id) 
					display "--- NEW DEPENDENT VAR ---"
				}
				
				
				// Pollution intensity x Paris
			foreach i in amount_bank ln_amount g_amount_bank g_ln_amount /*amount_new ln_amount_new g_amount_new g_ln_amount_new*/ {
				reg `i' $treatmentintensity $paris_intensity
				reghdfe `i' $treatmentintensity $paris_intensity, absorb(i.firm_id i.bank_id i.time_id) 
				reg `i' $treatmentintensity $paris_intensity $controls
				reghdfe `i' $treatmentintensity $paris_intensity $controls, absorb(i.firm_id i.bank_id i.time_id)
					display "--- NEW DEPENDENT VAR ---"
				}
	

			// ROUND 2
			
		drop if time_id < 201103
		
			// Generate totals
		gen PP_factorvar = "Not registered"
			replace PP_factorvar = "Missing PP" if MissingPP == 1
			replace PP_factorvar = "Low PP" if LowPP == 1 
			replace PP_factorvar = "Medium PP" if MediumPP == 1
			replace PP_factorvar = "High PP" if HighPP == 1
			// Note that it may occur that one firm has two types of activity. Because of the sequence in the code, the firm is classified according to it's highest PP.						
		
		drop if PP_factorvar == "Missing PP"
		set more off
		cd $output_check
				
			
			// Standard proposal regressions
		foreach i in ln_amount g_ln_amount {
			reghdfe `i' pollution_potential $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
			reghdfe `i' pollution_potential $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
			
			reghdfe `i' $treatmentintensity $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
			reghdfe `i' $treatmentintensity $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
			
			reghdfe `i' pollution_potential $paris $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
			reghdfe `i' pollution_potential $paris $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
			
			reghdfe `i' $treatmentintensity $paris_intensity $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
			reghdfe `i' $treatmentintensity	$paris_intensity $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				display "--- NEW DEPENDENT VAR ---"
			}

			
		
			// Bank heterogeneity 
		foreach i in ln_amount g_ln_amount {	
			reghdfe `i' pollution_potential $bank_heterogeneity $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
			reghdfe `i' pollution_potential $bank_heterogeneity $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
			
			reghdfe `i' $treatmentintensity $bank_heterogeneity_intensity $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
			reghdfe `i' $treatmentintensity	$bank_heterogeneity_intensity $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				display "--- NEW DEPENDENT VAR ---"
			}		
		
		
	
		drop if PP_factorvar == "Not registered"
			
			
			
			// Standard proposal regressions - different omitted category
		foreach i in ln_amount g_ln_amount {
			reghdfe `i' pollution_potential2 $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
			reghdfe `i' pollution_potential2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
			
			reghdfe `i' $treatmentintensity2 $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
			reghdfe `i' $treatmentintensity2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
			
			reghdfe `i' pollution_potential2 $paris2 $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
			reghdfe `i' pollution_potential2 $paris2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
			
			reghdfe `i' $treatmentintensity2 $paris_intensity2 $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
			reghdfe `i' $treatmentintensity2 $paris_intensity2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				display "--- NEW DEPENDENT VAR ---"
			}
		
		
	
				
			
		// ROUND 3	
			
		drop if time_id < 201103
		set more off
		
			// Generate totals
		gen PP_factorvar = "Not registered"
			replace PP_factorvar = "Missing PP" if MissingPP == 1
			replace PP_factorvar = "Low PP" if LowPP == 1 
			replace PP_factorvar = "Medium PP" if MediumPP == 1
			replace PP_factorvar = "High PP" if HighPP == 1
			// Note that it may occur that one firm has two types of activity. Because of the sequence in the code, the firm is classified according to it's highest PP.						
		
		drop if PP_factorvar == "Missing PP"
		set more off	
		cd $output_check
		cd
			
			// Standard proposal regressions
		foreach i in ln_amount g_ln_amount {	
			reghdfe `i' pollution_potential $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard.xls, replace ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
			
			reghdfe `i' $treatmentintensity $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard.xls, append ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
				
			reghdfe `i' pollution_potential $paris $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard.xls, append ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
			
			reghdfe `i' $treatmentintensity	$paris_intensity $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard.xls, append ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
				
				display "--- NEW DEPENDENT VAR ---"
			}
			
			
			
			// Bank heterogeneity
		foreach i in ln_amount g_ln_amount {
			reghdfe `i' pollution_potential $bank_heterogeneity $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Bank_heterogeneity.xls, replace ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
			
			reghdfe `i' $treatmentintensity	$bank_heterogeneity_intensity $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Bank_heterogeneity.xls, append ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
				
			reghdfe `i' pollution_potential $TRIPLE $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Bank_heterogeneity.xls, append ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)	
			
				display "--- NEW DEPENDENT VAR ---"
			}
							
		
			
			drop if PP_factorvar == "Not registered"	
			
			
			
			// Standard proposal regressions - different omitted category
		foreach i in ln_amount g_ln_amount {	
			reghdfe `i' pollution_potential2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_DOC.xls, replace ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
			
			reghdfe `i' $treatmentintensity2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_DOC.xls, append ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
		
			reghdfe `i' pollution_potential2 $paris2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_DOC.xls, append ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
			
			reghdfe `i' $treatmentintensity2 $paris_intensity2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_DOC.xls, append ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
				
				display "--- NEW DEPENDENT VAR ---"
			}
			
			
			
			// Bank heterogeneity
		foreach i in ln_amount g_ln_amount {
			reghdfe `i' pollution_potential2 $bank_heterogeneity2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Bank_heterogeneity_DOC.xls, replace ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
			
			reghdfe `i' $treatmentintensity2 $bank_heterogeneity_intensity2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Bank_heterogeneity_DOC.xls, append ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
				
			reghdfe `i' pollution_potential2 $TRIPLE2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Bank_heterogeneity_DOC.xls, append ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)					
				
				display "--- NEW DEPENDENT VAR ---"
			}			
			
			
			
		// ROUND 4	
			
		drop if time_id < 201103
		
			// Generate totals
		gen PP_factorvar = "Not registered"
			replace PP_factorvar = "Missing PP" if MissingPP == 1
			replace PP_factorvar = "Low PP" if LowPP == 1 
			replace PP_factorvar = "Medium PP" if MediumPP == 1
			replace PP_factorvar = "High PP" if HighPP == 1
			// Note that it may occur that one firm has two types of activity. Because of the sequence in the code, the firm is classified according to it's highest PP.						
		
		drop if PP_factorvar == "Missing PP"
		cd $output_check	
			/* Maxim directory below
			cd $output_file_SDD_Y 
			*/
			
			set more off
			// Standard proposal regressions without firm FE
		foreach i in ln_amount g_ln_amount {	
			reghdfe `i' pollution_potential $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
				outreg2 using Standard_`i'.xls, replace ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, NO, Time FE, YES)
			
			reghdfe `i' pollution_potential $paris $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
				outreg2 using Standard_`i'.xls, append ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, NO, Time FE, YES)
				
			reghdfe `i' $treatmentintensity $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
				outreg2 using Standard_`i'.xls, append ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, NO, Time FE, YES)				
			
			reghdfe `i' $treatmentintensity	$paris_intensity $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
				outreg2 using Standard_`i'.xls, append ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, NO, Time FE, YES)
				
				display "--- NEW DEPENDENT VAR ---"
			}		
		
		
		
			// Standard proposal regressions with firm FE
		foreach i in ln_amount g_ln_amount {	
			reghdfe `i' pollution_potential $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_`i'_FirmFE.xls, replace ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
			
			reghdfe `i' pollution_potential $paris $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_`i'_FirmFE.xls, append ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
				
			reghdfe `i' $treatmentintensity $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_`i'_FirmFE.xls, append ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)				
			
			reghdfe `i' $treatmentintensity	$paris_intensity $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_`i'_FirmFE.xls, append ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
				
				display "--- NEW DEPENDENT VAR ---"
			}
			
			
			
			drop if PP_factorvar == "Not registered"	
			
			
			// Standard proposal regressions without firm FE - Different omitted category
		foreach i in ln_amount g_ln_amount {	
			reghdfe `i' pollution_potential2 $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
				outreg2 using Standard_DOC_`i'.xls, replace ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, NO, Time FE, YES)
			
			reghdfe `i' pollution_potential2 $paris2 $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
				outreg2 using Standard_DOC_`i'.xls, append ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, NO, Time FE, YES)
				
			reghdfe `i' $treatmentintensity2 $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
				outreg2 using Standard_DOC_`i'.xls, append ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, NO, Time FE, YES)				
			
			reghdfe `i' $treatmentintensity2 $paris_intensity2 $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
				outreg2 using Standard_DOC_`i'.xls, append ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, NO, Time FE, YES)
				
				display "--- NEW DEPENDENT VAR ---"
			}		
		
		
		
			// Standard proposal regressions with firm FE - Different omitted category
		foreach i in ln_amount g_ln_amount {	
			reghdfe `i' pollution_potential2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_DOC_`i'_FirmFE.xls, replace ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
			
			reghdfe `i' pollution_potential2 $paris2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_DOC_`i'_FirmFE.xls, append ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
				
			reghdfe `i' $treatmentintensity2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_DOC_`i'_FirmFE.xls, append ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)				
			
			reghdfe `i' $treatmentintensity2 $paris_intensity2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_DOC_`i'_FirmFE.xls, append ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
				
				display "--- NEW DEPENDENT VAR ---"
			}							
			
	*/			
		
		
		// ROUND 5	
			
		*drop if time_id < 201103
		
		drop if time_id >= 201508
		drop if time_id <= 201409
		
			// Generate totals
		gen PP_factorvar = "Not registered"
			replace PP_factorvar = "Missing PP" if MissingPP == 1
			replace PP_factorvar = "Low PP" if LowPP == 1 
			replace PP_factorvar = "Medium PP" if MediumPP == 1
			replace PP_factorvar = "High PP" if HighPP == 1
			// Note that it may occur that one firm has two types of activity. Because of the sequence in the code, the firm is classified according to it's highest PP.						
		
		drop if PP_factorvar == "Missing PP"
		cd $output_check	
			/* Maxim directory below
			cd $output_file_SDD_Y 
			*/
		
		egen firmXtime = group(firm_id time_id)
		replace firm_id = firmXtime
		
			set more off
			// Standard proposal regressions without firm FE
		foreach i in ln_amount g_ln_amount g_amount_bank {	
			reghdfe `i' $PRSA_TRIPLE $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
				outreg2 using Standard_`i'.xls, replace title(Lending to polluters relative to non-polluters - without firm FE) ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, NO, Time FE, YES)
			
			reghdfe `i' $PRSA_TRIPLE_INTENSITY $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
				outreg2 using Standard_`i'.xls, append title(Lending to polluters relative to non-polluters - without firm FE) ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, NO, Time FE, YES)
				
				display "--- NEW DEPENDENT VAR ---"
			}		
		
		
		
			// Standard proposal regressions with firm FE
		foreach i in ln_amount g_ln_amount g_amount_bank {	
			reghdfe `i' $PRSA_TRIPLE $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_`i'_FirmFE.xls, replace title(Lending to polluters relative to non-polluters - with firm FE) ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
			
			reghdfe `i' $PRSA_TRIPLE_INTENSITY $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_`i'_FirmFE.xls, append title(Lending to polluters relative to non-polluters - with firm FE) ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
				
				display "--- NEW DEPENDENT VAR ---"
			}
			
			
			
			drop if PP_factorvar == "Not registered"	
			
			
			// Standard proposal regressions without firm FE - Different omitted category
		foreach i in ln_amount g_ln_amount g_amount_bank {	
			reghdfe `i' $PRSA_TRIPLE2 $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
				outreg2 using Standard_DOC_`i'.xls, replace title(Lending to polluters relative to LowPP - without firm FE) ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, NO, Time FE, YES)
			
			reghdfe `i' $PRSA_TRIPLE_INTENSITY2 $controls, absorb(i.bank_id i.time_id) vce(cluster firm_id)
				outreg2 using Standard_DOC_`i'.xls, append title(Lending to polluters relative to LowPP - without firm FE) ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, NO, Time FE, YES)
				
				display "--- NEW DEPENDENT VAR ---"
			}		
		
		
		
			// Standard proposal regressions with firm FE - Different omitted category
		foreach i in ln_amount g_ln_amount g_amount_bank {	
			reghdfe `i' $PRSA_TRIPLE2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_DOC_`i'_FirmFE.xls, replace title(Lending to polluters relative to LowPP - with firm FE) ctitle(PP) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
			
			reghdfe `i' $PRSA_TRIPLE_INTENSITY2 $controls, absorb(i.bank_id i.time_id i.firm_id) vce(cluster firm_id)
				outreg2 using Standard_DOC_`i'_FirmFE.xls, append title(Lending to polluters relative to LowPP - with firm FE) ctitle(Intensity) adjr2 addtext (Bank FE, YES, Firm FE, YES, Time FE, YES)
				
				display "--- NEW DEPENDENT VAR ---"
			}							
			
				log close		
		
		
		
		
			
		
		
		
		
		
	
	
	
	
	
	