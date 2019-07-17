Attribute VB_Name = "regressPolyF"
'public function regressPolyFF_ALL(



Public Function regressPolyFF(x1, x2, x3, x4, x5, x6, case_, complexity_)

ymin_new = 0
ymax_new = 0

If case_ = 21 Then
    ymin_new = 0.35
    ymax_new = 1.4
ElseIf case_ = 31 Then
    ymin_new = 0.33
    ymax_new = 1.56
ElseIf case_ = 32 Then
    ymin_new = 0.28
    ymax_new = 1.14
ElseIf case_ = 41 Then
    ymin_new = 0#
    ymax_new = 1.79
ElseIf case_ = 42 Then
    ymin_new = 0#
    ymax_new = 1.21
ElseIf case_ = 43 Then
    ymin_new = 0.14
    ymax_new = 2.04
ElseIf case_ = 51 Then
    ymin_new = 0
    ymax_new = 1.24
ElseIf case_ = 52 Then
    ymin_new = 0.12
    ymax_new = 1.28
End If


'x1  x2  x3  x4  x5  x6
'bpc/y   bcol/bpc    bpg/bpc T'/y    f1/bcol h'1/y
'z=be/b*
'b*=average exposed

'using only training data

'1 = without eliminating variables
'2 = with eliminating variables
'3 = polynomial one

If case_ = 21 Then
    If complexity_ = 1 Then
        z = 1.56 - 0.08 * x1 - 2.52 * x3 + 0.76 * x4
    ElseIf complexity_ = 2 Then 'rmse=0.18
        z = 1.41 - 2.32 * x3 + 0.79 * x4
    ElseIf complexity_ = 3 Then 'rmse=0.09 for 2, rmse=0.07 for 3
        z = 0.174 + -1.13 * x3 + 3.761 * x3 ^ 2 + 8.893 * x4 + -17.355 * x3 * x4 + -0.951 * x4 ^ 2
        'z = 13.448 + -99.072 * x3 + 238.363 * x3 ^ 2 + -183.807 * x3 ^ 3 + -23.177 * x4 + 164.978 * x3 * x4 + -243.178 * x3 ^ 2 * x4 + -16.435 * x4 ^ 2 + 29.646 * x3 * x4 ^ 2 + 1.776 * x4 ^ 3
    End If

ElseIf case_ = 31 Then
    If complexity_ = 1 Then
        z = 0.94 + 0.1 * x1 + 0.59 * x2 - 0.5 * x3 - 0.1 * x4 - 0.2 * x5 - 0.53 * x6
    ElseIf complexity_ = 2 Then 'rmse=0.16
        z = 0.64 + 0.8 * x2 - 0.56 * x6
    ElseIf complexity_ = 3 Then 'rmse=0.096 for 3, rmse=0.074 for 4
        z = 0.906 + 2.497 * x2 + -19.444 * x2 ^ 2 + 29.271 * x2 ^ 3 + -6.391 * x6 + 49.798 * x2 * x6 + -63.784 * x2 ^ 2 * x6 + -9.812 * x6 ^ 2 + -11.858 * x2 * x6 ^ 2 + 20.308 * x6 ^ 3
        'z = -342.915 + 2801.111 * x2 + -6806.677 * x2 ^ 2 + 5217.791 * x2 ^ 3 + -61.08 * x2 ^ 4 + 931.259 * x6 + -6247.179 * x2 * x6 + 12163.068 * x2 ^ 2 * x6 + -7600.866 * x2 ^ 3 * x6 + -887.102 * x6 ^ 2 + 3804.135 * x2 * x6 ^ 2 + -3188.156 * x2 ^ 2 * x6 ^ 2 + 440.042 * x6 ^ 3 + -990.773 * x2 * x6 ^ 3 + -83.103 * x6 ^ 4
    End If

ElseIf case_ = 32 Then
    If complexity_ = 1 Then
        z = 0.49 + 0.11 * x1 + 0.49 * x2 - 0.11 * x3 - 0.15 * x4 + 0.26 * x5 - 0.13 * x6
    ElseIf complexity_ = 2 Then
        z = 0.21 + 0.69 * x2 + 0.45 * x5 'rmse=0.11
    ElseIf complexity_ = 3 Then
        z = 0.21 + 0.69 * x2 + 0.45 * x5 'nothing new
    End If

    
ElseIf case_ = 41 Then

    If complexity_ = 1 Then
        z = 1.45 + 0.35 * x1 + 0.39 * x2 - 0.81 * x3 - 1.68 * x4 - 0.49 * x5
    ElseIf complexity_ = 2 Then 'rmse=0.26
        z = 1.38 + 0.43 * x1 - 1.81 * x4 - 0.58 * x5
    ElseIf complexity_ = 3 Then 'rmse=0.16 for 2, 0.10 for 3
        z = 1.755 + -0.64 * x1 + -0.785 * x1 ^ 2 + 0.83 * x4 + 4.29 * x1 * x4 + -6.076 * x4 ^ 2 + -2.655 * x5 + 2.118 * x1 * x5 + -3.792 * x4 * x5 + 1.092 * x5 ^ 2
        'z = 2.558 + -35.79 * x1 + 19.689 * x1 ^ 2 + -16.826 * x1 ^ 3 + 61.286 * x4 + -4.538 * x1 * x4 + 97.516 * x1 ^ 2 * x4 + -60.747 * x4 ^ 2 + -183.261 * x1 * x4 ^ 2 + 122.959 * x4 ^ 3 + 16.549 * x5 + 67.593 * x1 * x5 + -12.889 * x1 ^ 2 * x5 + -147.552 * x4 * x5 + -92.897 * x1 * x4 * x5 + 205.727 * x4 ^ 2 * x5 + -44.493 * x5 ^ 2 + -2.052 * x1 * x5 ^ 2 + 56.397 * x4 * x5 ^ 2 + 17.872 * x5 ^ 3
    End If
    
ElseIf case_ = 42 Then
    If complexity_ = 1 Then
        z = -0.41 + 0.04 * x1 + 1.79 * x2 - 0.83 * x3 + 0.02 * x4 + 0.8 * x5 + 1.04 * x6
    ElseIf complexity_ = 2 Then 'rmse=0.15
        z = -1.01 + 2.18 * x2 + 1.2 * x5
    ElseIf complexity_ = 3 Then 'rmse=0.1
        z = 17.561 + -39.196 * x2 + 22.159 * x2 ^ 2 + -36.135 * x5 + 41.816 * x2 * x5 + 18.32 * x5 ^ 2 + 27.788 * x6 + -17.427 * x2 * x6 + -24.686 * x5 * x6 + -30.129 * x6 ^ 2
    End If

ElseIf case_ = 43 Then
    If complexity_ = 1 Then
        z = 1.93 - 0.06 * x1 + 1.13 * x2 - 3.89 * x3 - 0.28 * x4 + 0.25 * x5 + 0.04 * x6
    ElseIf complexity_ = 2 Then 'rmse=0.28
        z = 2.28 + 0.6 * x2 - 4.18 * x3
    ElseIf complexity_ = 3 Then 'rmse=0.17, 0.05 for 3
        z = 33.757 + 8.675 * x2 + -31.873 * x2 ^ 2 + -3.648 * x2 ^ 3 + -331.805 * x3 + 50.98 * x2 * x3 + 86.627 * x2 ^ 2 * x3 + 1014.702 * x3 ^ 2 + -173.955 * x2 * x3 ^ 2 + -974.056 * x3 ^ 3
        'z = -90.478 + 5.015 * x2 + -45.346 * x2 ^ 2 + 7.671 * x2 ^ 3 + 740.286 * x3 + 63.356 * x2 * x3 + 91.157 * x2 ^ 2 * x3 + -2034.164 * x3 ^ 2 + -205.467 * x2 * x3 ^ 2 + 1934.914 * x3 ^ 3 + 32.676 * x4 + 68.777 * x2 * x4 + -29.065 * x2 ^ 2 * x4 + -216.027 * x3 * x4 + -41.453 * x2 * x3 * x4 + 189.53 * x3 ^ 2 * x4 + -6.509 * x4 ^ 2 + -6.77 * x2 * x4 ^ 2 + 40.538 * x3 * x4 ^ 2 + -1.576 * x4 ^ 3
    End If

    
ElseIf case_ = 51 Then
    If complexity_ = 1 Then
        z = 0.76 + 0.11 * x1 + 1.29 * x2 - 1.66 * x3 - 1 * x4 + 0.32 * x5 + 0.9 * x6
    ElseIf complexity_ = 2 Then 'rmse=0.24
        z = 0.22 + 0.75 * x2 - 0.57 * x4 + 0.4 * x5
    ElseIf complexity_ = 3 Then 'rmse=0.1, 0.02
        z = -6.783 + 45.047 * x2 + -45.855 * x2 ^ 2 + -15.395 * x4 + 30.902 * x2 * x4 + -4.532 * x4 ^ 2 + 5.489 * x5 + -30.826 * x2 * x5 + 7.83 * x4 * x5 + 2.015 * x5 ^ 2
        'z = -125.36 + 1061.675 * x2 + -2060.74 * x2 ^ 2 + 1110.787 * x2 ^ 3 + -452.047 * x4 + 130.305 * x2 * x4 + 790.453 * x2 ^ 2 * x4 + 1477.167 * x4 ^ 2 + -2232.173 * x2 * x4 ^ 2 + -2.754 * x4 ^ 3 + 128.62 * x5 + -775.956 * x2 * x5 + 804.095 * x2 ^ 2 * x5 + 193.488 * x4 * x5 + 386.664 * x2 * x4 * x5 + -728.013 * x4 ^ 2 * x5 + NA * x5 ^ 2 + NA * x2 * x5 ^ 2 + NA * x4 * x5 ^ 2 + NA * x5 ^ 3
    End If
    
ElseIf case_ = 52 Then
    If complexity_ = 1 Then
        z = 0.04 - 0.2 * x1 + 1.31 * x2 - 0.45 * x3 - 0.93 * x4 + 0.47 * x5 - 0.36 * x6
    ElseIf complexity_ = 2 Then 'rmse=0.18
        z = 0.37 + 0.58 * x2 - 1.14 * x4 - 0.28 * x6
    ElseIf complexity_ = 3 Then 'rmse=0.13 ,0.07
        z = -0.522 + 2.938 * x2 + 4.184 * x2 ^ 2 + 3.971 * x4 + -15.191 * x2 * x4 + -0.236 * x4 ^ 2 + -0.537 * x6 + 9.114 * x2 * x6 + -3.538 * x4 * x6 + 3.219 * x6 ^ 2
        'z = -55.49 + 222.237 * x2 + -48.741 * x2 ^ 2 + -350.008 * x2 ^ 3 + 438.015 * x4 + -1373.836 * x2 * x4 + 1781.205 * x2 ^ 2 * x4 + -611.463 * x4 ^ 2 + -52.674 * x2 * x4 ^ 2 + -753.773 * x4 ^ 3 + -93.855 * x6 + 519.697 * x2 * x6 + -649.26 * x2 ^ 2 * x6 + 393.096 * x4 * x6 + 272.233 * x2 * x4 * x6 + -1759.633 * x4 ^ 2 * x6 + 30.807 * x6 ^ 2 + -10.244 * x2 * x6 ^ 2 + 160.994 * x4 * x6 ^ 2 + 21.484 * x6 ^ 3
    End If
End If

z = WorksheetFunction.Min(WorksheetFunction.max(z, ymin_new), ymax_new)

regressPolyFF = z

End Function
