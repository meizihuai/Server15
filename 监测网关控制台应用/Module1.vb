
Imports System.Collections.Generic
Imports System.Linq
Imports System.Text
Imports System.IO
Imports System.Math
Imports System.Net.Sockets
Imports System.Net
Imports System.Net.HttpListener
Imports System.Data
Imports System.Data.Sql
Imports System.Data.SqlClient
Imports System.Threading
Imports System.Threading.Thread
Imports OfficeOpenXml
Imports System
Imports System.Int32
Imports System.BitConverter
Imports Newtonsoft
Imports Newtonsoft.Json
Imports Newtonsoft.Json.Linq
Imports System.Xml
Imports System.Web
Imports System.Web.HttpUtility
Imports System.IO.Compression

Module Module1

    Dim title As String = "Server14 V1.3.7"
    Dim StartTime As Date
    Dim logLock As Object
    Dim isAutoScrollLog As Boolean = True
    Dim WithEvents TZBQServer As TZBQServer
    Dim WithEvents TSSServer As TSSServer
    Dim WithEvents TSS_GateWayServer As TSS_GateWayServer
    Dim WithEvents MZH_GateWayServer As MZHGateWayServer
    Dim WithEvents myRMTPServer As RMTPServer
    Dim TSSPort As Integer = 3204
    Dim TZBQPort As Integer = 8899
    Dim TSS_GateWayPort As Integer = 3205
    Dim MZHGateWayPort As Integer = 3206
    Dim RMTPPort As Integer = 5150

    WithEvents MainHttpListener As MainHttpListener
    Dim MainHttpListenerUrl As String = "http://+:8080/"
    Dim HTTPMsg_IpUrl As String = "http://123.207.31.37:"
    Dim IISFileUrl As String = "http://123.207.31.37:8082/"
    Public taskTSSHelperList As List(Of TaskTSS)
    Public taskTSSHelperListLock As Object
    Public taskTZBQHelperList As List(Of TaskTZBQ)
    Public taskTZBQHelperListLock As Object
    Sub Main()
        Console.Title = title
        StartTime = Now
        ini()
    End Sub
    Private Sub ini()
        'Dim itp As String = "123.207.31.37"
        'MsgBox(ip2Address(itp))
        myServerInfoLock = New Object
        logLock = New Object
        DeviceListLock = New Object
        iniMyServerInfo()
        UseDSGFreq = myServerInfo.UseDSGFreq
        ConnectSQL = String.Format("server={0};DataBase={1};User Id={2};Pwd={3}", New String() {myServerInfo.SQLServerIP, myServerInfo.SQLDataBaseName,
                                                                                           myServerInfo.SQLUid, myServerInfo.SQLPwd})

        If myServerInfo.TSSPort <> 0 Then TSSPort = myServerInfo.TSSPort
        If myServerInfo.TZBQPort <> 0 Then TZBQPort = myServerInfo.TZBQPort
        If myServerInfo.RMTPPort <> 0 Then RMTPPort = myServerInfo.RMTPPort
        HTTPMsg_IpUrl = myServerInfo.HTTPMsg_IpUrl
        IISFileUrl = myServerInfo.IISFileUrl

        DeviceList = New List(Of deviceStu)
        log(title)
        log("初始化……")

        CheckDir(Directory.GetCurrentDirectory() & "\Task")
        CheckDir(Directory.GetCurrentDirectory() & "\Taskimages")
        CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports")
        CheckDir(Directory.GetCurrentDirectory() & "\Task\logs")
        CheckDir(Directory.GetCurrentDirectory() & "\JobFiles")
        Dim d As New DirectoryInfo(Directory.GetCurrentDirectory() & "\Taskimages")
        For Each f In d.GetFiles
            Dim fPath As String = f.FullName
            Try
                File.Delete(fPath)
            Catch ex As Exception

            End Try
        Next

        DeviceListLock = New Object
        DeviceList = New List(Of deviceStu)
        dik = New Dictionary(Of String, String)
        dik_Lock = New Object
        FirstRunCheckUnCLosedTask()
        TZBQServer = New TZBQServer(TZBQPort)
        TZBQServer.StartServer()

        TSSServer = New TSSServer(TSSPort)
        TSSServer.StartServer()

        TSS_GateWayServer = New TSS_GateWayServer(TSS_GateWayPort)
        TSS_GateWayServer.StartServer()

        myRMTPServer = New RMTPServer(RMTPPort)
        myRMTPServer.StartServer()


        MZH_GateWayServer = New MZHGateWayServer(MZHGateWayPort)
        MZH_GateWayServer.StartServer()
        MainHttpListenerUrl = "http://+:" & myServerInfo.MainHTTPPort & "/"
        log("MainHttpListenerUrl=" & MainHttpListenerUrl)
        log("UseDSGFreq=" & myServerInfo.UseDSGFreq)

        MainHttpListener = New MainHttpListener(MainHttpListenerUrl, HTTPMsg_IpUrl)
        AddHandler MainHttpListener.Raiselog, AddressOf MainHTTPSevLog
        MainHttpListener.Start()

        If (myServerInfo.isMain = False) Then
            Dim mainHTTPUrl As String = myServerInfo.MainServerIP
            If mainHTTPUrl <> "" Then
                '连接主服务器
                log("本服务器作为从服务，连接主服务器(" & mainHTTPUrl & ":" & myServerInfo.MainServerPort & ")……")
                Try
                    Dim ip As IPAddress = IPAddress.Parse(mainHTTPUrl)
                    Dim ipport As IPEndPoint = New IPEndPoint(ip, myServerInfo.MainServerPort)
                    MZHHandle = New MZH_TSS_Handle(mainHTTPUrl, myServerInfo.MainServerPort, myServerInfo.MZHDeviceID)
                    MZHHandle.TSS_Handle_Start()
                    flag_MzhHandle = True
                Catch ex As Exception
                    flag_MzhHandle = False
                    log("主服务器的远端IP地址或者端口设置有误")
                End Try
                ' Dim dcf As New MZH_TSS_Handle()
            Else
                flag_MzhHandle = False
                log("本服务器作为从服务但不连接主服务")
            End If
        Else
            '作为主服务器
            flag_MzhHandle = False
            log("本服务器作为从主服务")
            DailyFreqHelper.StartWork()
        End If

    End Sub
    Private Sub FirstRunCheckUnCLosedTask()
        Try
            log("检查Task……")
            taskTSSHelperListLock = New Object
            taskTZBQHelperListLock = New Object

            taskTSSHelperList = New List(Of TaskTSS)
            taskTZBQHelperList = New List(Of TaskTZBQ)
            Dim sql As String = "select * from UserTaskTable where isClosed=0 and EndTime<'" & Now.ToString("yyyy-MM-dd HH:mm:ss") & "'"
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) Then
                log("没有未完成Task")
                Return
            End If
            If dt.Rows.Count = 0 Then
                log("没有未完成Task")
                Return
            End If
            log("有未完成Task，开始安排Cls执行")
            For Each row As DataRow In dt.Rows
                Try
                    Dim taskName As String = row("TaskName")
                    Dim deviceID As String = row("DeviceID")
                    Dim taskNickName As String = row("TaskNickName")
                    Dim taskDeviceKind As String = row("TaskDeviceKind")
                    If taskDeviceKind.ToLower = "tss" Then
                        ' MsgBox(taskName)
                        Dim cls As New TaskTSS("", row)
                        cls.MakeTaskReport()
                    End If
                    If taskDeviceKind.ToLower = "tzbq" Then
                        Dim cls As New TaskTZBQ("", row, False)
                        cls.MakeTaskReport()
                    End If
                Catch ex As Exception

                End Try
            Next
        Catch ex As Exception

        End Try

    End Sub
    Private Sub MainHTTPSevLog(ByVal str As String)
        log(str)
    End Sub
    Public Sub AddWarnMsg2WarnList(ByVal deviceMsg As String)
        Try
            MainHttpListener.AddWarnMsg2List(deviceMsg)
        Catch ex As Exception
            MsgBox(ex.ToString)
        End Try
    End Sub
    Public Function GetMyDevList(userName As String) As String
        If userName = "" Then Return ""
        Dim sql As String = "select * from userTable where usr='" & userName & "'"
        Try
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) = False Then
                If dt.Rows.Count > 0 Then
                    Dim row As DataRow = dt.Rows(0)
                    Dim device As String = row("device")
                    Return device
                Else
                    Return ""
                End If
            Else
                Return ""
            End If
        Catch ex As Exception
            Return ""
        End Try
    End Function
#Region "配置"
    Private Sub iniMyServerInfo()
        CheckDir(Directory.GetCurrentDirectory() & "\MyServerInfo")
        Dim path As String = Directory.GetCurrentDirectory() & "\MyServerInfo\config.ini"
        If File.Exists(path) = False Then
            DefaultMyServerInfo()
            SaveMyServerInfo()
            Exit Sub
        End If
        ReadMyServerInfo()
    End Sub
    Private Sub DefaultMyServerInfo()
        myServerInfo.MainHTTPPort = 8080
        myServerInfo.TZBQPort = 8899
        myServerInfo.TSSPort = 3204
        myServerInfo.RMTPPort = RMTPPort
        myServerInfo.HTTPMsg_IpUrl = "http://123.207.31.37:"
        myServerInfo.isSaveDevMsg = True
        myServerInfo.SQLDataBaseName = "DianCiXinXiFuWu"
        myServerInfo.SQLServerIP = "localhost"
        myServerInfo.SQLUid = "root"
        myServerInfo.SQLPwd = "Mei19931129"
        myServerInfo.isMain = True
        myServerInfo.MZHDeviceID = "ING_180501"
        myServerInfo.MainServerPort = 3206
        myServerInfo.MainServerIP = "123.207.31.37"
        myServerInfo.UseDSGFreq = True
        myServerInfo.IISFileUrl = "http://123.207.31.37:8082/"
        myServerInfo2UI()
    End Sub
    Public Sub myServerInfo2UI()

    End Sub
    Public Sub SaveMyServerInfo()
        Try
            CheckDir(Directory.GetCurrentDirectory() & "\MyServerInfo")
            Dim path As String = Directory.GetCurrentDirectory() & "\MyServerInfo\config.ini"
            If File.Exists(path) Then File.Delete(path)
            Dim str As String = JsonConvert.SerializeObject(myServerInfo)
            Dim sw As New StreamWriter(path)
            sw.Write(str)
            sw.Close()
        Catch ex As Exception

        End Try
    End Sub
    Private Sub ReadMyServerInfo()
        Try
            Dim path As String = Directory.GetCurrentDirectory() & "\MyServerInfo\config.ini"
            Dim sr As New StreamReader(path)
            Dim str As String = sr.ReadToEnd
            sr.Close()
            myServerInfo = JsonConvert.DeserializeObject(str, GetType(myServerInfoStu))
            If myServerInfo.MainHTTPPort = 0 Then
                myServerInfo.MainHTTPPort = 8080
            End If
            myServerInfo2UI()
        Catch ex As Exception
            DefaultMyServerInfo()
        End Try
    End Sub

#End Region
    Dim swLock As New Object
    Public Sub log(ByVal str As String)
        'Return
        ThLog(str)
        'Dim th As New Thread(AddressOf ThLog)
        'th.Start(str)
    End Sub
    Private Sub ThLog(str As String)
        str = Now.ToString("[yyyy-MM-dd HH:mm:ss] ") & str
        Console.WriteLine(str)
        Try
            If Directory.Exists("logs") = False Then
                Directory.CreateDirectory("logs")
            End If

            SyncLock swLock
                Dim path As String = "logs\" & Now.ToString("yyyy_MM_dd") & ".txt"
                Dim sw As New StreamWriter(path, True, Encoding.Default)
                sw.WriteLine(str)
                sw.Close()
            End SyncLock
        Catch ex As Exception

        End Try
    End Sub

    Private Sub TZBQRefrushDeviceList() Handles TZBQServer.RefrushDeviceList

    End Sub
    Private Sub TZBQSevLog(ByVal mainMsg As String, ByVal UserName As String, ByVal Port As String) Handles TZBQServer.RaiseLog
        log(mainMsg)
    End Sub
    Private Sub TSSRefrushDeviceList() Handles TSSServer.RefrushDeviceList

    End Sub
    Private Sub TSSSevLog(ByVal mainMsg As String, ByVal UserName As String, ByVal Port As String) Handles TSSServer.RaiseLog
        log(mainMsg)
    End Sub
    Private Sub TSS_GateWaySevLog(ByVal mainMsg As String, ByVal UserName As String, ByVal Port As String) Handles TSS_GateWayServer.RaiseLog
        log(mainMsg)
    End Sub
    Private Sub MZH_GateWaySevLog(ByVal mainMsg As String, ByVal UserName As String, ByVal Port As String) Handles MZH_GateWayServer.RaiseLog
        log(mainMsg)
    End Sub
    Private Sub TSSSevHttpLog(ByVal mainMsg As String) Handles TSSServer.RaiseHttplog
        'log2(mainMsg)
    End Sub
    Public Function Compress(ByVal data() As Byte) As Byte()
        Dim stream As MemoryStream = New MemoryStream
        Dim gZip As New GZipStream(stream, CompressionMode.Compress)
        gZip.Write(data, 0, data.Length)
        gZip.Close()
        Return stream.ToArray
    End Function
    Public Function Decompress(ByVal data() As Byte) As Byte()
        Dim stream As MemoryStream = New MemoryStream
        Dim gZip As New GZipStream(New MemoryStream(data), CompressionMode.Decompress)
        Dim n As Integer = 0
        While True
            Dim by(409600) As Byte
            n = gZip.Read(by, 0, by.Length)
            If n = 0 Then Exit While
            stream.Write(by, 0, n)
        End While
        gZip.Close()
        Return stream.ToArray
    End Function
    Public Function PPSJValues2DSGBase(value() As Double) As String
        If IsNothing(value) Then Return ""
        Dim count As Long = value.Count
        Dim by(4 * count - 1) As Byte
        For i = 0 To count - 1
            Dim k As Single = value(i)
            Dim b() As Byte = BitConverter.GetBytes(k)
            For j = 0 To 3
                by(i * 4 + j) = b(j)
            Next
        Next
        'For Each v In value
        '    Dim k As Single = v
        '    Dim b() As Byte = BitConverter.GetBytes(k)
        '    If IsNothing(by) Then
        '        by = b
        '    Else
        '        by = by.Concat(b).ToArray
        '    End If
        'Next
        Dim nb() As Byte = Compress(by)
        Dim base64 As String = Convert.ToBase64String(nb)
        Return base64
    End Function
    Public Function DSGBase2PPSJValues(base64 As String) As Double()
        If base64 = "" Then Return Nothing
        Try
            Dim nb() As Byte = Convert.FromBase64String(base64)
            Dim jb() As Byte = Decompress(nb)
            Dim list As New List(Of Double)
            For i = 0 To jb.Length - 1 Step 4
                Dim k As Single = BitConverter.ToSingle(jb, i)
                Dim d As Double = k
                list.Add(d)
            Next
            Return list.ToArray
        Catch ex As Exception

        End Try
        Return Nothing
    End Function
    Public Function CanUpdate(ByVal oldVersion As String, ByVal newVersion As String) As Boolean
        Dim oldv As New Version(oldVersion)
        Dim newv As New Version(newVersion)
        If oldv < newv Then
            Return True
        Else
            Return False
        End If

    End Function

End Module
