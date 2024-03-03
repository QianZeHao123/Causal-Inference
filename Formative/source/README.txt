**************************************
*
* This is the replication archive for:
*
* "Messages Designed to Increase Perceived Electoral Closeness Increase Turnout"
* Daniel R. Biggers, David J. Hendry, and Gregory A. Huber
* American Politics Research
* 
* This version: 1.0, October 2023
*
**************************************


This replication archive contains data and Stata code necessary to replicate
the analyses presented in both the published article and the supporting
information. 

----------------------------------------------------------------------------

In the main directory are the following files:

APRCloseElections_Final_Publication_Replication_Dataset.dta
	A Stata dataset that contains the information necessary to 
	conduct all analyses in the main text and online appendix

AnalyzeFinalAPRReplicationArchive.do
	A Stata .do file that draws in the dataset described above and
	performs the same analyses as those presented in the paper and 
	supporting information. The file produces tables saved in the 
	"tables" subdirectory, Figure 1 saved in the "figures"
	subdirectory, and a logfile save in the logs subdirectory.

Closeness_SubProgramPRTestRegression.do
	A Stata .do file that contains the subprogram used by the main do
	file to produce many of the model results.