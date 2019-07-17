Attribute VB_Name = "error_calc"
Function error5(rng1 As Range, rng2 As Range, rng_cri As Range, case_ As Integer, type_ As String) As Double
    nsize = rng1.Rows.Count
    ntot = 0
    xbar = 0
    ybar = 0
    xmin = 1000
    xmax = -1000
    ymin = xmin
    ymax = xmax
    For i = 1 To nsize
        If rng_cri.Cells(i, 1) = 2 Or rng_cri.Cells(i, 1) = 3 Or rng_cri.Cells(i, 1) = 4 Or rng_cri.Cells(i, 1) = 5 Then
        If rng_cri.Cells(i, 1) = case_ Or case_ = 2345 Then
            ntot = ntot + 1
            x = rng1.Cells(i, 1)
            y = rng2.Cells(i, 1)
            xbar = xbar + x
            ybar = ybar + y
            xmin = WorksheetFunction.Min(x, xmin)
            xmax = WorksheetFunction.max(x, xmax)
            ymin = WorksheetFunction.Min(y, ymin)
            ymax = WorksheetFunction.max(y, ymax)
            
            
            
        End If
        End If
    Next
    xbar = xbar / ntot
    ybar = ybar / ntot
    
    const1 = 0
    const2 = 0
    const3 = 0
    
    For i = 1 To nsize
        If rng_cri.Cells(i, 1) = 2 Or rng_cri.Cells(i, 1) = 3 Or rng_cri.Cells(i, 1) = 4 Or rng_cri.Cells(i, 1) = 5 Then
        If rng_cri.Cells(i, 1) = case_ Or case_ = 2345 Then
            x = rng1.Cells(i, 1)
            y = rng2.Cells(i, 1)
            const1 = const1 + (x - xbar) * (y - ybar)
            const2 = const2 + (x - xbar) ^ 2
            const3 = const3 + (y - ybar) ^ 2
        End If
        End If
    Next
    
    If const2 * const3 > 0 Then
        cc = const1 / Sqr(const2 * const3)
    Else
        cc = 0
    End If
    
    If type_ = "cc" Then
        error5 = Round(cc, 2) '1 = correlation
    ElseIf type_ = "count" Then  '2 = count
        error5 = ntot
    ElseIf type_ = "xbar" Then '3=average of x
        error5 = xbar
    ElseIf type_ = "ybar" Then '4 = average of y
        error5 = ybar
    ElseIf type_ = "ymax" Then
        error5 = ymax
    ElseIf type_ = "ymin" Then
        error5 = ymin
    ElseIf type_ = "xmax" Then
        error5 = xmax
    ElseIf type_ = "xmin" Then
        error5 = xmin
    End If

End Function

Function error2(rng1 As Range, rng2 As Range, type_ As Integer) As Double
    ' Returns the Pearson correlation of two seperate column ranges (equal in length)
    
    ntot = rng1.Rows.Count
    
    type_0 = 0   'average y   [0]
    type_1 = 0   'SSE [1]
    type_2 = 0   'MSE [2]
    type_3 = 0   'RMSE [3]
    type_4 = 0   'MAE [4]
    type_5 = 0   'DR [5]
    type_55 = 0  'COF [55]
    type_6 = 0   'R^2 [6]
    type_7 = 0   'Ia [7]
    type_8 = 0   'CC pearson [8]
    type_9 = 0   'PAE [9]
    type_91 = 0  'U [91]
    type_92 = 0  'count [92]
    
    type_10 = 0 'min yhat-y  [10]
    type_11 = 0 'q1 yhat-y    [11]
    type_12 = 0 'median yhat-y  [12]
    type_13 = 0 'q3 yhat-y       [13]
    type_14 = 0 'max yhat-y      [14]
    type_15 = 0 'average yhat-y   [15]
    
    type_21 = 0   'percent yhat<0.8*y, [21]
    type_22 = 0   'percent yhat>1.2*y, [22]
    type_23 = 0   'percent yhat<y,     [23]
    type_24 = 0   'percent 0.9*y<yhat<1.5*y [24]
    type_25 = 0 'percent between +-20% [25]
    
    
    'type 21, 22 23
    For j = 1 To ntot
    
    
        aa = rng1.Cells(j, 1).Value
        bb = rng2.Cells(j, 1).Value
        
        If bb < 0.8 * aa Then
            type_21 = type_21 + 1
        End If
        If bb > 1.2 * aa Then
            type_22 = type_22 + 1
        End If
        If bb < aa Then
            type_23 = type_23 + 1
        End If
        If bb < 1.5 * aa And bb > 0.9 * aa Then
            type_24 = type_24 + 1
        End If
        
        
    
    Next
    
    If type_ = 21 Then
        error2 = type_21 / ntot * 100
    End If
    If type_ = 22 Then
        error2 = type_22 / ntot * 100
    End If
    If type_ = 23 Then
        error2 = type_23 / ntot * 100
    End If
    If type_ = 24 Then
        error2 = type_24 / ntot * 100
    End If
    If type_ = 25 Then
        error2 = 100 - (type_21 + type_22) / ntot * 100
    End If
    
    '[10] to [15]
    'Dim rngerr As Range
    'rngerr = rng1
    For j = 1 To ntot
        'rngerr.Cells(j, 1).Value = rng2.Cells(j, 1).Value - rng1.Cells(j, 1).Value
    Next
    
    Dim residual() As Double
    ReDim residual(ntot - 1)
    
    For j = 1 To ntot
        residual(j - 1) = rng2.Cells(j, 1).Value - rng1.Cells(j, 1).Value
    Next
    
    'min yhat-y [10]
    If type_ = 10 Then
        error2 = Application.WorksheetFunction.Min(residual)
    End If
    
    'q1 yhat-y [10]
    If type_ = 11 Then
        error2 = Application.WorksheetFunction.Quartile(residual, 1)
    End If
    
    'median yhat-y [10]
    If type_ = 12 Then
        error2 = Application.WorksheetFunction.Median(residual)
    End If
    
    'q2 yhat-y [10]
    If type_ = 13 Then
        error2 = Application.WorksheetFunction.Quartile(residual, 3)
    End If
    
    'max yhat-y [10]
    If type_ = 14 Then
        error2 = Application.WorksheetFunction.max(residual)
    End If
    
    'average yhat-y [15]
    If type_ = 15 Then
        error2 = Application.WorksheetFunction.Average(residual)
    End If
    
    
    
    'average y   [0]
    For j = 1 To ntot
        aa = rng1.Cells(j, 1).Value
        type_0 = type_0 + aa
    Next
    
    type_0 = type_0 / ntot
    
    If type_ = 0 Then
        error2 = type_0
    End If
    
    'SSE    [1]
    For j = 1 To ntot
        aa = (rng2.Cells(j, 1).Value - rng1.Cells(j, 1).Value) ^ 2
        type_1 = type_1 + aa
    Next
    
    If type_ = 1 Then
        error2 = type_1
    End If
    
    'MSE    [2]
    type_2 = type_1 / ntot
    
    If type_ = 2 Then
        error2 = type_2
    End If
    
    
    'RMSE   [3]
    type_3 = Sqr(type_1 / (ntot))
    
    If type_ = 3 Then
        error2 = type_3
    End If
    
    'MAE    [4]
    For j = 1 To ntot
        aa = Abs(rng2.Cells(j, 1).Value - rng1.Cells(j, 1).Value)
        type_4 = type_4 + aa
    Next
    
    type_4 = type_4 / ntot
    
    
    If type_ = 4 Then
        error2 = type_4
    End If
    
    'PAE    [9]
    For j = 1 To ntot
        If rng1.Cells(j, 1) > 0 Then
            aa = Abs(rng2.Cells(j, 1).Value - rng1.Cells(j, 1).Value) / rng1.Cells(j, 1).Value
            type_9 = type_9 + aa
        End If
    Next
    
    type_9 = type_9 / ntot * 100
    
    
    If type_ = 9 Then
        error2 = type_9
    End If
    
    'DR     [5]
    For j = 1 To ntot
        If rng1.Cells(j, 1) > 0 Then
            aa = Abs(rng2.Cells(j, 1).Value / rng1.Cells(j, 1).Value)
        Else
            aa = 1
        End If
        type_5 = type_5 + aa
    Next
    type_5 = type_5 / ntot
    
    If type_ = 5 Then
        error2 = type_5
    End If
    
    'COV [55]
    For j = 1 To ntot
        If rng1.Cells(j, 1) > 0 Then
            aa = Abs(rng2.Cells(j, 1).Value / rng1.Cells(j, 1).Value)
        Else
            aa = 1
        End If
        type_55 = type_55 + (aa - type_5) ^ 2
    Next
    type_55 = Sqr(type_55 / (ntot - 1))
    type_55 = type_55 / type_5
    
    If type_ = 55 Then
        error2 = type_55
    End If
    
    'R^2   [6]
    For j = 1 To ntot
        aa = (rng1.Cells(j, 1).Value - type_0) ^ 2
        type_6 = type_6 + aa
    Next
    
    If type_6 > 0 Then
    type_6 = 1 - type_1 / type_6
    End If
    
    If type_ = 6 Then
        error2 = type_6
    End If
    
    'Ia    [7]
    For j = 1 To ntot
        aa = (Abs(rng1.Cells(j, 1).Value - type_0) + Abs(rng2.Cells(j, 1).Value - type_0)) ^ 2
        type_7 = type_7 + aa
    Next
    If type_7 > 0 Then
    type_7 = 1 - type_1 / type_7
    End If
    
    If type_ = 7 Then
        error2 = type_7
    End If
    
    'U    [91]
    For j = 1 To ntot
        aa = (rng1.Cells(j, 1).Value) ^ 2
        type_91 = type_91 + aa
    Next
    If type_91 > 0 Then
    type_91 = Sqr(type_1 / type_91)
    End If
    
    If type_ = 91 Then
        error2 = type_91
    End If
    
    'CC [8]
    If type_ = 8 Then
        error2 = Application.WorksheetFunction.Correl(rng1, rng2)
    End If


End Function


Function nondim_R2(rng1 As Range, rng2 As Range, rng3 As Range) As Double

    
    ntot = rng1.Rows.Count
    
    type_0 = 0   'average y   [0]
    type_1 = 0   'SSE [1]
    type_6 = 0   'R^2 [6]
  
   
    'average y   [0]
    For j = 1 To ntot
        aa = rng1.Cells(j, 1).Value / rng3.Cells(j, 1).Value
        type_0 = type_0 + aa
    Next
    
    type_0 = type_0 / ntot


    'SSE    [1]
    For j = 1 To ntot
        aa = (rng2.Cells(j, 1).Value / rng3.Cells(j, 1).Value - rng1.Cells(j, 1).Value / rng3.Cells(j, 1).Value) ^ 2
        type_1 = type_1 + aa
    Next


    
    'R^2   [6]
    For j = 1 To ntot
        aa = (rng1.Cells(j, 1).Value / rng3.Cells(j, 1).Value - type_0) ^ 2
        type_6 = type_6 + aa
    Next

    If type_6 > 0 Then
    type_6 = 1 - type_1 / type_6
    End If
    
nondim_R2 = type_6




End Function






