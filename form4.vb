'it does accept tab delimited text box copy paste

Imports System.IO.Stream

Public Class Form1

    Private Sub Form1_Load(ByVal sender As System.Object,
                           ByVal e As System.EventArgs) Handles MyBase.Load
        '
        ' Fill in the data grid with a List
        '
        Dim list = New List(Of Test)
        For i = 1 To 1500
            list.Add(New Test("", ""))

        Next

        DataGridView1.DataSource = list


        DataGridView1.RowHeadersWidth = 60

        For i = 0 To DataGridView1.Rows.Count - 1
            DataGridView1.Rows(i).HeaderCell.Value = CStr(i + 1)
        Next


        For i = 1 To 18 - 1
            Dim col As New DataGridViewTextBoxColumn
            col.DataPropertyName = "PropertyName"
            col.HeaderText = "SomeText"
            col.Name = "colWhateverName"
            DataGridView1.Columns.Add(col)
        Next

        For i = 0 To DataGridView1.Columns.Count - 1
            DataGridView1.Columns(i).Width = 60
        Next

        DataGridView1.Columns(0).HeaderText = "Database"
        DataGridView1.Columns(1).HeaderText = "Live bed/Clear water"
        DataGridView1.Columns(2).HeaderText = "Field/Lab"
        DataGridView1.Columns(3).HeaderText = "Database index"
        DataGridView1.Columns(4).HeaderText = "Reference"
        DataGridView1.Columns(5).HeaderText = "Model & Run"
        DataGridView1.Columns(6).HeaderText = "Channel width B (m)"
        DataGridView1.Columns(7).HeaderText = "Depth y (m)"
        DataGridView1.Columns(8).HeaderText = "Width b (m)"
        DataGridView1.Columns(9).HeaderText = "Length L (m)"
        DataGridView1.Columns(10).HeaderText = "angle (degree)"
        DataGridView1.Columns(11).HeaderText = "Velocity V (m/s)"
        DataGridView1.Columns(12).HeaderText = "Sediment d50 (mm)"
        DataGridView1.Columns(13).HeaderText = "Nose shape"
        DataGridView1.Columns(14).HeaderText = "Ks,shape"
        DataGridView1.Columns(15).HeaderText = "Time (h)"
        DataGridView1.Columns(16).HeaderText = "Sigma_g"
        DataGridView1.Columns(17).HeaderText = "Observed scour depth ys (m)"
        DataGridView1.Columns(18).HeaderText = "NeuralNet ys (m)"

        DataGridView1.Columns(18).ReadOnly = True 'make it read only
        DataGridView1.Columns(18).DefaultCellStyle.BackColor = Color.LightGray 'make it gray
        'DataGridView1.Rows(1).Cells(10).Value = 10.256
        DataGridView1.AutoResizeColumns()
    End Sub

    Private Sub DataGridView1_KeyDown(sender As Object, e As KeyEventArgs) Handles DataGridView1.KeyDown
        If DataGridView1.RowCount > 0 Then
            If e.Control And (e.KeyCode = Keys.C) Then
                Dim d As DataObject = DataGridView1.GetClipboardContent()
                Clipboard.SetDataObject(d)
                e.Handled = True
            ElseIf (e.Control And e.KeyCode = Keys.V) Then
                PasteUnboundRecords(DataGridView1)
            End If
        End If
    End Sub

    Private Sub PasteUnboundRecords(ByVal dgv As DataGridView)
        Try
            Dim rowLines As String() = Clipboard.GetText(TextDataFormat.Text).Split(New String(0) {vbCr & vbLf}, StringSplitOptions.None)
            Dim currentRowIndex As Integer = (If(dgv.CurrentRow IsNot Nothing, dgv.CurrentRow.Index, 0))
            Dim currentColumnIndex As Integer = (If(dgv.CurrentCell IsNot Nothing, dgv.CurrentCell.ColumnIndex, 0))
            Dim currentColumnCount As Integer = dgv.ColumnCount

            For rowLine As Integer = 0 To rowLines.Length - 1
                If rowLine = rowLines.Length - 1 AndAlso String.IsNullOrEmpty(rowLines(rowLine)) Then
                    Exit For
                End If

                Dim columnsData As String() = rowLines(rowLine).Split(New String(0) {vbTab}, StringSplitOptions.None)
                If (currentColumnIndex + columnsData.Length) > dgv.ColumnCount Then
                    For columnCreationCounter As Integer = 0 To ((currentColumnIndex + columnsData.Length) - currentColumnCount) - 1
                        If columnCreationCounter = rowLines.Length - 1 Then
                            Exit For
                        End If
                    Next
                End If
                If dgv.Rows.Count > (currentRowIndex + rowLine) Then
                    For columnsDataIndex As Integer = 0 To columnsData.Length - 1
                        If currentColumnIndex + columnsDataIndex <= dgv.ColumnCount - 1 Then
                            dgv.Rows(currentRowIndex + rowLine).Cells(currentColumnIndex + columnsDataIndex).Value = columnsData(columnsDataIndex)
                        End If
                    Next
                Else
                    Dim pasteCells As String() = New String(dgv.ColumnCount - 1) {}
                    For cellStartCounter As Integer = currentColumnIndex To dgv.ColumnCount - 1
                        If columnsData.Length > (cellStartCounter - currentColumnIndex) Then
                            pasteCells(cellStartCounter) = columnsData(cellStartCounter - currentColumnIndex)
                        End If
                    Next
                End If
            Next
        Catch ex As Exception

        End Try
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim y, b, L, angle, V, d50, Ks, t, sigma As Double
        y = 0
        b = 0
        L = 0
        angle = 0
        V = 0
        d50 = 0
        Ks = 0
        t = 0
        sigma = 0

        Dim cell_no, row_now As Integer

        row_now = 0

        Dim err1, err2, err3, err4, err5, err6, err7, err8, err9, err10 As Boolean


        err1 = Double.TryParse(DataGridView1.Rows(row_now).Cells(7).Value, y)
        err2 = Double.TryParse(DataGridView1.Rows(row_now).Cells(8).Value, b)
        err3 = Double.TryParse(DataGridView1.Rows(row_now).Cells(9).Value, L)
        err4 = Double.TryParse(DataGridView1.Rows(row_now).Cells(10).Value, angle)
        err5 = Double.TryParse(DataGridView1.Rows(row_now).Cells(11).Value, V)
        err6 = Double.TryParse(DataGridView1.Rows(row_now).Cells(12).Value, d50)
        err7 = Double.TryParse(DataGridView1.Rows(row_now).Cells(14).Value, Ks)
        err8 = Double.TryParse(DataGridView1.Rows(row_now).Cells(15).Value, t)
        err9 = Double.TryParse(DataGridView1.Rows(row_now).Cells(16).Value, sigma)

        err10 = err1 And err2 And err3 And err4 And err5 And err6 And err7 And err8 And err9


        Dim z As Double
        z = y ^ 2 + t + sigma / (d50 + 1) - V + L + b
        If err10 Then
            DataGridView1.Rows(row_now).Cells(18).Value = z
        Else
            DataGridView1.Rows(row_now).Cells(18).Value = "NonnumericInput"
        End If

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        'clear
        For i = 0 To DataGridView1.Rows.Count - 1
            For j = 0 To DataGridView1.Columns.Count - 1
                DataGridView1.Rows(i).Cells(j).Value = ""
            Next
        Next

        DataGridView1.CurrentCell = DataGridView1.Rows(0).Cells(0)
        DataGridView1.BeginEdit(True)
        DataGridView1.EndEdit()



    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Dim myStream As IO.Stream = Nothing
        Dim openFileDialog1 As New OpenFileDialog()

        openFileDialog1.InitialDirectory = "c:\"
        openFileDialog1.Filter = "txt files (*.txt)|*.txt|All files (*.*)|*.*"
        openFileDialog1.FilterIndex = 1
        openFileDialog1.RestoreDirectory = True
        Dim bytes(10) As Byte
        Dim row_no_old, cell_no_old As Integer
        Dim row_no_new, cell_no_new As Integer
        row_no_old = 0
        cell_no_old = 0
        row_no_new = 0
        cell_no_new = 0
        Dim s As Integer
        Dim str1 As String
        Dim kk As Integer
        str1 = ""
        If openFileDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
            Try
                myStream = openFileDialog1.OpenFile()
                If (myStream IsNot Nothing) Then

                    ' Insert code to read the stream here.
                    myStream.Position = 0

                    'ignore first line
                    For kk = 0 To myStream.Length - 1 - myStream.Position
                        s = myStream.ReadByte()
                        If s = 10 Then 'tab
                            Exit For
                        End If
                    Next

                    'kk = 0
                    For j = kk To myStream.Length() - 1
                        str1 = ""
                        For i = 0 To myStream.Length - 1 - myStream.Position
                            s = myStream.ReadByte()
                            If s = 9 Then 'tab
                                cell_no_new = cell_no_old + 1
                                Exit For
                            ElseIf s = 10 Then 'enter
                                row_no_new = row_no_old + 1
                                cell_no_new = 0
                                Exit For

                            Else
                                str1 = str1 & Chr(s)
                            End If
                        Next
                        DataGridView1.Rows(row_no_old).Cells(cell_no_old).Value = str1
                        cell_no_old = cell_no_new
                        row_no_old = row_no_new
                    Next


                End If
            Catch Ex As Exception
                MessageBox.Show("Cannot read file from disk. Original error: " & Ex.Message)
            Finally
                ' Check this again, since we need to make sure we didn't throw an exception on open.
                If (myStream IsNot Nothing) Then
                    myStream.Close()
                End If
            End Try
        End If
        DataGridView1.AutoResizeColumns()
    End Sub

    Private Sub Label1_Click(sender As Object, e As EventArgs) Handles Label1.Click

    End Sub
End Class
Public Class Test

    Public Sub New(ByVal name As String, ByVal cost As String)
        _name = name
        _cost = cost
    End Sub

    Private _name As String
    Public Property Name() As String
        Get
            Return _name
        End Get
        Set(ByVal value As String)
            _name = value
        End Set
    End Property

    Private _cost As String
    Public Property Cost() As String
        Get
            Return _cost
        End Get
        Set(ByVal value As String)
            _cost = value
        End Set
    End Property

End Class