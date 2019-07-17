Attribute VB_Name = "fdot_iter"


Sub fdot_iter()
Attribute fdot_iter.VB_ProcData.VB_Invoke_Func = " \n14"
'
' Macro4 Macro
'

'
    Sheets("sheet1").Select
    Range("A1").Select


    N = 20
    Range("HT5").Select
    ActiveCell.FormulaR1C1 = "0.1"
    Range("HT5").Select
    Selection.AutoFill Destination:=Range("HT5:HT3631")
    Range("HT5:HT3631").Select
    
    For i = 1 To N / 2
    Columns("IA:IA").Select
    Selection.Copy
    Columns("HT:HT").Select
    Selection.PasteSpecial Paste:=xlPasteValues, Operation:=xlNone, SkipBlanks _
        :=False, Transpose:=False
    Next
    
    Application.CutCopyMode = False
    
    Range("JG5").Select
    ActiveCell.FormulaR1C1 = "0.1"
    Range("JG5").Select
    Selection.AutoFill Destination:=Range("JG5:JG3631")
    Range("JG5:JG3631").Select
    
    For i = 1 To 4 * N
    Columns("JJ:JJ").Select
    Selection.Copy
    Columns("JG:JG").Select
    Selection.PasteSpecial Paste:=xlPasteValues, Operation:=xlNone, SkipBlanks _
        :=False, Transpose:=False
    Next
        
    Application.CutCopyMode = False

    Range("KK5").Select
    ActiveCell.FormulaR1C1 = "0.1"
    Range("KK5").Select
    Selection.AutoFill Destination:=Range("KK5:KK3631")
    Range("KK5:KK3631").Select
    
    For i = 1 To N * 4
    Columns("KR:KR").Select
    Selection.Copy
    Columns("KK:KK").Select
    Selection.PasteSpecial Paste:=xlPasteValues, Operation:=xlNone, SkipBlanks _
        :=False, Transpose:=False
    Next
    
    Application.CutCopyMode = False
    Range("a1").Select
    
    
    Sheets("sensitivity").Select
    Range("W23").Select
End Sub
