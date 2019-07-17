Attribute VB_Name = "error_calc2"
Function calc_err(type_, case_, traintest_, time_, case_rng As Range, train_rng As Range, test_rng As Range, time_rng As Range, obs_ As Range, pred_ As Range) As Double

Const N = 537
count_ = 0
ReDim array_(1 To N)





For i = 1 To N
array_(i) = 0

If (case_rng.Cells(i, 1).Value = case_) Or (case_rng.Cells(i, 1).Value >= 2 And case_rng.Cells(i, 1).Value <= 5 And case_ = 2345) Then
If (train_rng.Cells(i, 1).Value = 1 And traintest_ = 1) Or (test_rng.Cells(i, 1).Value = 1 And traintest_ = 2) Or (train_rng.Cells(i, 1).Value + test_rng.Cells(i, 1).Value = 1 And traintest_ = 3) Then
If (time_ = 1) Or (time_rng.Cells(i, 1).Value = 1 And time_ = 2) Then

array_(i) = 1
count_ = count_ + 1
End If
End If
End If

Next

xbar = 0
err_ = 0
std_ = 0

For i = 1 To N
If array_(i) = 1 Then
xbar = xbar + obs_.Cells(i, 1).Value
err_ = err_ + (pred_.Cells(i, 1).Value - obs_.Cells(i, 1).Value) ^ 2
End If
Next

xbar = xbar / count_
For i = 1 To N
If array_(i) = 1 Then
std_ = std_ + (obs_.Cells(i, 1).Value - xbar) ^ 2
End If
Next

'type_ = 1 for R2
If type_ = 1 Then
'calc_err = 1 - err_ / std_    "R2
calc_err = Sqr(err_ / count_) 'RMSE
End If

perc_above_20 = 0
perc_below_20 = 0
For i = 1 To N
If array_(i) = 1 Then
    If pred_.Cells(i, 1).Value <= obs_.Cells(i, 1).Value * 0.8 Then
        perc_below_20 = perc_below_20 + 1
    End If
    If pred_.Cells(i, 1).Value >= obs_.Cells(i, 1).Value * 1.2 Then
        perc_above_20 = perc_above_20 + 1
    End If
End If
Next

'type_ = 2 for perc_under_minus20
If type_ = 2 Then
calc_err = (perc_below_20 + perc_above_20) / count_ * 100
End If

perc_err = 0

For i = 1 To N
If array_(i) = 1 Then
    If obs_.Cells(i, 1).Value > 0 Then
        perc_err = perc_err + Abs(pred_.Cells(i, 1).Value - obs_.Cells(i, 1).Value) / obs_.Cells(i, 1).Value
    End If
End If
Next

'type_ = 3 for average_percent_absolute_error
If type_ = 3 Then
calc_err = perc_err / count_ * 100
End If


End Function



Function boxplot_(y_obs As Range, y_pred As Range, type__ As String)
Dim yy As Variant
yy = y_obs
nsize = y_obs.Rows.Count

For i = 1 To nsize
yy(i, 1) = y_pred.Cells(i, 1) - y_obs.Cells(i, 1)
Next

If type__ = "median" Then
    boxplot_ = WorksheetFunction.Median(yy)
ElseIf type__ = "min" Then
    boxplot_ = WorksheetFunction.Min(yy)
ElseIf type__ = "max" Then
    boxplot_ = WorksheetFunction.max(yy)
ElseIf type__ = "q1" Then
    boxplot_ = WorksheetFunction.Quartile(yy, 1)
ElseIf type__ = "q3" Then
    boxplot_ = WorksheetFunction.Quartile(yy, 3)
End If
'boxplot_ = yy(7, 1)

End Function
