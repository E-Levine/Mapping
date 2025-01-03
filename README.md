# Mapping
Code for various mapping projects using R.
When using this repo, insure all large raw data files are stored in a folder named 'Data'. 'Data' folders will not sync, but any 'Output', 'Summary', or other folders will.

HSI <br>
*Templates for creating HSI scoring. <br>
*In development to utilize survey code for creation of editable HSI.

LOCATION SPECIFIC CODE <br>
Assorted code files with location specific information. For tracking purposes, file names should include date used/completed in "YYYY MM DD" format. 

SCALLOP SURVEY <br>
Code for scallop surveys which use a different gridding system than the HSI and Oyster survey files. 

SURVEY <br>
*Templates for <br> 
1)Updating grids, <br>
2)Updating HSI evaluation, <br>
3) MGID checks and assignements of survey stations, <br>
4)Selection of stations <br>
5)Selected stations mapping <br>
6)Mapping of survey results and basic summary data based on grid cells and oyster polygons<br>
*Code for mapping completed randomized survey stations within the Oyster Mapping folders.

WATER QUALITY DATA <br>
*Code for selecting and compiling water quality data for use with mapping. Requires state grid and estuary area layers. 

**Updates** <br>
 6/5/24 - #4 Updated for proper selection/exclusion of monitoring stations. <br>
 11/18/24 - gitignore 'Data' folders. <br>
 1/3/25 - WQ folders created. README updated.