Option Explicit

Const vbNormal = 1

DIM objXL, objWB, objR
DIM Title, Text, tmp, i, file, name, savename

Set objXL = WScript.CreateObject ("Excel.Application")
objXL.Visible = false
DIM objFSO
Set objFSO = CreateObject("scripting.filesystemobject")
Dim myFolder, inputFolder, outputFolder, objFile
inputFolder = "/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/data_sales"
outputFolder = "/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/data_sales"
Const fileExt = ".xlsx"

set myFolder = objFSO.getfolder("/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/data_sales")
	For Each objFile In myFolder.Files
		If LCase(objFSO.GetExtensionName(objFile)) = "csv" Then
		savename = objFSO.BuildPath(outputFolder, objFSO.GetBaseName(objFile) & FileExt)
		Set objWb = objX1.WorkBooks.Open(objFile)
		objWb.Activate

		OBJXL.DISPLAYALERTS = FALSE

		objx1.ActiveWorktook.SaveAs savename, &HFFFFEFD1
		End If
	Next
objX1.Quit()
Set objXL = Nothing
Set objWB = Nothing
Set objR = Nothing
