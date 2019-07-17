Attribute VB_Name = "error_calc3"
Function correlation_(x_range As Range, y_range As Range, caserange_ As Range, case_ As Integer) As Double

    xbar = 0
    ybar = 0
    nsize = x_range.Rows.Count
    x_no = 0
    
    For i = 1 To nsize
    If caserange_.Cells(i, 1) = case_ Then
        x = x_range.Cells(i, 1)
        y = y_range.Cells(i, 1)
        
        x_no = x_no + 1
        xbar = xbar + x
        ybar = ybar + y
    End If
    Next
    
    xbar = xbar / x_no
    ybar = ybar / x_no
    
    temp1 = 0: temp2 = 0: temp3 = 0

    For i = 1 To nsize
    If caserange_.Cells(i, 1) = case_ Then
        x = x_range.Cells(i, 1)
        y = y_range.Cells(i, 1)
        
        temp1 = temp1 + (x - xbar) ^ 2
        temp2 = temp2 + (y - ybar) ^ 2
        temp3 = temp3 + (x - xbar) * (y - ybar)
    End If
    Next
    
    correlation_ = temp3 / Sqr(temp1 * temp2)

End Function


