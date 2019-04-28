Imports System
Imports System.IO
Imports System.Text
Imports System.Net
Imports System.Net.HttpListener
Imports System.Threading.Thread
Imports System.Data
Imports System.Data.Sql
Imports System.Data.SqlClient
Imports System.Linq
Imports System.Net.Sockets
Imports System.Threading
Imports System.Web
Imports System.Web.HttpUtility
Imports Newtonsoft
Imports Newtonsoft.Json
Imports Novacode
Imports Novacode.DocX
Imports System.Windows.Forms.DataVisualization
Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports Newtonsoft.Json.Linq
Imports System.Deployment
Imports System.Threading.Tasks

Public Class DeviceTSS
    Public isLiSanSaoMiao As Boolean = False
    Public lsInfo As LisanInfo
    Public lsInfoLock As New Object

    Private myRunkind As String = ""
    Private maxSize As Integer = 81920
    Private isNeedAutoStopDevice As Boolean = True
    Private autoStopDeviceTime As Date = Now.AddMinutes(1)
    Public isWorking As Boolean = False
    Private myControlerVersion As String = "null"
    Private workingTask As New NormalTaskStu
    Private legalSigNal As List(Of Double)
    Private isWarnWZ As Boolean = False
    Private WarnInfoList As List(Of WarnInfo)
    Private DSGDeviceKind As String
    Private minWarnNum As Integer = 5
    Private WarnFucha As Double = 5
    Private warnDaikuan As Double = 5
    Private isTZBQ_JCPD As Boolean = False
    Private TZBQ_JCPD_IndexList As List(Of Integer)
    Private TZBQ_JCPDString As String = ""
    Public myDeviceInfo As New DeviceStu
    Private myRealID As String
    Private myHttpListener As HttpListener
    Private webSocketListener As WebSocketServer
    Private myHttpUrl As String
    Private webSocketUrl As String
    Private myClientSocket As New System.Net.Sockets.Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
    Private oldbytes() As Byte
    Private IP As String
    Private Port As String
    Private iLen As Integer
    Public HttpMsgList As List(Of String)
    Private HttpMsgList_Object As Object
    Private HTTPClient As List(Of HttpListenerContext)
    Private HTTPClient_Object As Object
    Private broadcastThread As Thread
    Private loopGetHeartBeatThread As Thread
    Private flagNeedGetGPSLoop As Boolean = False
    Private DHDeviceControlStr As String = "one"
    Private TaskWorkerThread As Thread
    Public Event OnHandleJsonMsg(js As String)
    Public Event RefrushDeviceList()
    Public Event RaiseLog(ByVal mainMsg As String, ByVal UserName As String, ByVal IP As String)
    Dim DirPath As String = Directory.GetCurrentDirectory() & "\DeviceTaskIni"
    Public Event RaiseHttplog(ByVal str As String)
    Private is701Device As Boolean = False
    Private freqGisBusLineId As String
    Private myFreqInfo As json_PPSJ
    Private flagHaveMyFreqInfo As Boolean = False

    Private myRunLocation As RunLocation
    Private myRunLocationLock As New Object
    Private lastCarFreqGisTime As Date = Now
    Dim isHandledLogin As Boolean = False

    Private Sub Httplog(ByVal str As String)
        ' Console.WriteLine(str)
        RaiseEvent RaiseHttplog(str)
    End Sub

    Public Sub New(ByVal ClientSocket As Socket)
        myClientSocket = ClientSocket
        IP = myClientSocket.RemoteEndPoint.ToString.Split(":")(0)
        Port = myClientSocket.RemoteEndPoint.ToString.Split(":")(1)

    End Sub
    Public Sub Start()
        serverThreadProc()
        log("TSS设备连接，服务器主动发送握手命令……")
        'log(
        ' SendMsgToTSSDevByString(&H0, "task", "netswitch", "<netswitch:sw=in;>", Nothing)
        ' Sleep(3000)
        'Dim b(9) As Byte
        'For i = 0 To b.Length - 1
        '    b(i) = 1
        'Next
        'myClientSocket.Send(b)
        SendMsgToTSSDevByString(&H0, "DevMsg", "WSZL", "", Nothing)
        '  Sleep(3000)
        'If Not isHandledLogin Then
        '    Dim th As New Thread(Sub()
        '                             Sleep(7000)
        '                             If Not isHandledLogin Then
        '                                 CloseSocket()
        '                             End If
        '                         End Sub)
        '    th.Start()
        'End If
    End Sub

    Private Sub log(ByVal MainMsg As String)
        'Form1.log(MainMsg, "TSS_Server", "+:" & _Port)
        ' Module1.log(MainMsg)
        RaiseEvent RaiseLog(MainMsg, "TSS_" & myDeviceInfo.DeviceID, IP & ":" & Port)
    End Sub

    Private Function GetDSGDeviceKind(id As String) As String
        Dim DH As String = "DSG_DH"
        Dim DL As String = "DSG-DL"
        Dim ONE As String = "DSG-ONE"
        Dim TEK As String = "Tek"
        If id.Length > DH.Length Then
            If id.Substring(0, DH.Length) = DH Then
                Return "DH"
            End If
        End If
        If id.Length > DL.Length Then
            If id.Substring(0, DL.Length) = DL Then
                Return "DL"
            End If
        End If
        If id.Length > ONE.Length Then
            If id.Substring(0, ONE.Length) = ONE Then
                Return "ONE"
            End If
        End If
        If id.Length > TEK.Length Then
            If id.Substring(0, TEK.Length) = TEK Then
                Return "TEK"
            End If
        End If
    End Function
    Private Sub LoopGetHeartBeat()
        Dim times As Integer = 10
        While True
            Try
                SendMsgToTSSDevByString(&H0, "task", "getheartbeat", "", Nothing)
            Catch ex As Exception

            End Try
            Try
                If times = 10 Then
                    times = 0
                    If myRunkind = "bus" Then
                        SendOrderFreqToDevice(30, 6000, 25, 8, "2to1", "bus")
                    End If
                End If
            Catch ex As Exception

            End Try
            times = times + 1
            Sleep(3 * 1000)
        End While
    End Sub
    Private Sub HandleLogin(ByVal id As String, runKind As String)
        If isHandledLogin Then Return
        isHandledLogin = True
        log("TSS设备上报登陆命令，id=" & id & ",名称=" & GetDeviceNickNameByID(id))
        If id.Substring(0, 2) = "01" Then
            is701Device = True
        End If
        myRealID = id
        DSGDeviceKind = GetDSGDeviceKind(id)
        Dim NickName As String = id
        Dim DeviceName As String = "TSS_" & id
        DeviceName = id
        Dim dbLng As String = ""
        Dim dbLat As String = ""
        Dim myAddress As String = ""
        myAddress = ip2Address(IP)
        runKind = "" '2019-03-25修改，运行模式靠数据库 维护
        If runKind = "" Then
            Dim sql As String = "select runKind from deviceTable where DeviceId='" & id & "'"
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) = False Then
                If dt.Rows.Count > 0 Then
                    If IsNothing(dt.Rows(0)(0)) = False Then
                        runKind = dt.Rows(0)(0).ToString
                    End If
                End If
            End If
        End If
        If runKind = "bus" Or runKind = "car" Then
            flagNeedGetGPSLoop = True
        Else
            flagNeedGetGPSLoop = False
        End If
        myRunkind = runKind
        Dim myLA As LocationAddressInfo
        ' myAddress = GetAddressByLngLat()
        Try
            Dim sql As String = String.Format("select * from deviceTable where DeviceID='{0}' and Kind='{1}'", New String() {id, "TSS"})
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) = False Then
                If dt.Rows.Count > 0 Then
                    Dim row As DataRow = dt.Rows(0)
                    DeviceName = row("DeviceNickName")
                    NickName = id
                    dbLng = row("Lng")
                    dbLat = row("Lat")
                    Dim dbAddress As String = row("Address").ToString
                    Dim flagNeedGetAddress As Boolean = True
                    If IsNothing(dbAddress) = False Then
                        If dbAddress <> "" Then
                            myAddress = dbAddress
                            flagNeedGetAddress = False
                        End If
                    End If
                    If flagNeedGetAddress Then
                        If IsNothing(dbLng) = False And IsNothing(dbLng) = False Then
                            If dbLng <> "" And dbLat <> "" Then
                                If IsNumeric(dbLng) And IsNumeric(dbLat) Then
                                    Try
                                        Dim la As LocationAddressInfo = GetAddressByLngLat(dbLng, dbLat)
                                        myLA = la
                                        Dim addressTmp As String = la.DetailAddress
                                        If IsNothing(addressTmp) = False Then
                                            If addressTmp <> "" Then
                                                myAddress = addressTmp
                                            End If
                                        End If
                                    Catch ex As Exception

                                    End Try
                                End If
                            End If
                        End If
                    End If
                    Dim sqlTmp As String = "update deviceTable set OnlineTime='{0}',IP='{1}',Port='{2}',Province='{3}',city='{4}',District='{5}',LocationAddress='{6}' where DeviceID='{7}'"
                    sqlTmp = String.Format(sqlTmp, New String() {Now.ToString("yyyy-MM-dd HH:mm:ss"), IP, Port, myLA.Province, myLA.City, myLA.District, myLA.DetailAddress, id})
                    SQLCmd(sqlTmp)
                Else
                    Dim insertSql As String = "insert into deviceTable (deviceid,deviceNickName,kind,OnlineTime,IP,port) values('{0}','{1}','{2}','{3}','{4}','{5}')"
                    insertSql = String.Format(insertSql, New String() {id, id, "TSS", Now.ToString("yyyy-MM-dd HH:mm:ss"), IP, Port, runKind})
                    SQLCmd(insertSql)
                End If
            Else
                Dim insertSql As String = "insert into deviceTable (deviceid,deviceNickName,kind,OnlineTime,IP,port) values('{0}','{1}','{2}','{3}','{4}','{5}')"
                insertSql = String.Format(insertSql, New String() {id, id, "TSS", Now.ToString("yyyy-MM-dd HH:mm:ss"), IP, Port, runKind})
                SQLCmd(insertSql)
            End If
        Catch ex As Exception

        End Try
        If runKind = "bus" Then
            Try
                Dim sql As String = String.Format("select * from BusLineTable where DeviceID='{0}'", New String() {id})
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        'Dim sqlTmp As String = "update BusLineTable set time='{0}' where DeviceID='{1}'"
                        'sqlTmp = String.Format(sqlTmp, New String() {Now.ToString("yyyy-MM-dd HH:mm:ss"), id})
                        'SQLCmd(sqlTmp)
                    Else
                        Dim insertSql As String = "insert into BusLineTable (lineId,lineName,busNo,plateNumber,deviceId,deviceName,lng,lat,location,time,shutdownTime,shutdownVoltage) values('{0}','{1}','{2}','{3}','{4}','{5}','{6}','{7}','{8}','{9}','',0)"
                        insertSql = String.Format(insertSql, New String() {0, "线路_" & id, "", "", id, DeviceName, "", "", "", Now.ToString("yyyy-MM-dd HH:mm:ss")})
                        SQLCmd(insertSql)
                    End If
                Else
                    Dim insertSql As String = "insert into BusLineTable (lineId,lineName,busNo,plateNumber,deviceId,deviceName,lng,lat,location,time,shutdownTime,shutdownVoltage) values('{0}','{1}','{2}','{3}','{4}','{5}','{6}','{7}','{8}','{9}','',0)"
                    insertSql = String.Format(insertSql, New String() {0, "线路_" & id, "", "", id, DeviceName, "", "", "", Now.ToString("yyyy-MM-dd HH:mm:ss")})
                    SQLCmd(insertSql)
                End If
            Catch ex As Exception

            End Try
            Try
                Dim sql As String = String.Format("select * from BusLineTable where DeviceID='{0}'", New String() {id})
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        Dim row As DataRow = dt.Rows(0)
                        freqGisBusLineId = row("lineId")
                    End If
                End If
            Catch ex As Exception

            End Try
        End If
        Dim newMyAddressTmp As String = ""
        If IsNothing(myAddress) = False Then
            Dim st() As String = myAddress.Split(",")
            For Each itm In st
                If itm <> "" Then
                    newMyAddressTmp = newMyAddressTmp & itm & ","
                End If
            Next
            If IsNothing(newMyAddressTmp) = False Then
                If newMyAddressTmp.Length > 2 Then
                    newMyAddressTmp = newMyAddressTmp.Substring(0, newMyAddressTmp.Length - 1)
                End If
            End If
        End If
        myDeviceInfo = New DeviceStu()
        myDeviceInfo.DeviceID = NickName
        myDeviceInfo.Address = newMyAddressTmp
        myHttpUrl = TSSHttpListenerPath & id & "/"
        webSocketUrl = TSSWebSocketListenerPath & id & "/"
        myDeviceInfo.HTTPMsgUrl = myHttpUrl
        myDeviceInfo.WebSocketUrl = webSocketUrl
        myDeviceInfo.Kind = "TSS"
        myDeviceInfo.OnlineTime = Now.ToString("yyyy-MM-dd HH:mm:ss")
        myDeviceInfo.Statu = "normal"
        myDeviceInfo.cls = Me
        myDeviceInfo.IP = IP
        myDeviceInfo.Port = Port
        myDeviceInfo.RunKind = runKind
        myDeviceInfo.Name = "TSS_" & id
        myDeviceInfo.Name = DeviceName
        myDeviceInfo.Func = "null"
        myDeviceInfo.Lng = dbLng
        myDeviceInfo.Lat = dbLat
        myDeviceInfo.NetSwitch = 0
        If IsNothing(DeviceListLock) Then DeviceListLock = New Object
        If IsNothing(DeviceList) Then DeviceList = New List(Of DeviceStu)
        Dim isFind As Boolean = False
        Dim isJXX As Boolean = False
        Dim JXXItm As DeviceStu
        SyncLock DeviceListLock
            For i = 0 To DeviceList.Count - 1
                Dim itm As DeviceStu = DeviceList(i)
                If itm.DeviceID = NickName Then
                    log("TSS DeviceID=" & NickName & "已登录,正在处理同ID登录，New IP=" & IP & ":" & Port & ",oldIP=" & getIpAndPortByDeviceID(id))
                    Try
                        If itm.IP = IP And itm.Port = Port Then
                            isFind = True
                            log("TSS old DeviceID=" & itm.DeviceID & ",重复登录，无需处理" & ",old位置=" & itm.Address & "oldip=(" & itm.IP & ":" & itm.Port & ")")
                            Exit For
                        Else
                            isJXX = True
                            JXXItm = itm
                            Exit For
                        End If
                    Catch ex As Exception

                    End Try
                    Exit For
                End If
            Next
        End SyncLock
        If isJXX Then
            Dim cls As DeviceTSS = CType(JXXItm.cls, DeviceTSS)
            cls.CloseALL(True, "(挤下线)")
            AddMyLog("重新上线", "成功")
            log("重新上线TSS设备登录，deviceID=" & id)
            StartHttp(cls)
            If flagNeedGetGPSLoop Then
                Try
                    log("TSSDeviceId=" & id & "runKind=" & runKind & " 开启频繁获取心跳包线程")
                    loopGetHeartBeatThread = New Thread(AddressOf LoopGetHeartBeat)
                    loopGetHeartBeatThread.Start()
                Catch ex As Exception

                End Try
            End If
            RaiseEvent RefrushDeviceList()
            isFind = True
        End If
        If isFind = False Then
            AddMyLog("上线", "成功")
            log("TSS设备登录，deviceID=" & NickName)
            StartHttp(Nothing)
            RaiseEvent RefrushDeviceList()
            CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & id)
            CheckDir(Directory.GetCurrentDirectory() & "\Task\logs\" & id)
            GetMyTask()
            If flagNeedGetGPSLoop Then
                Try
                    log("TSSDeviceId=" & id & "runKind=" & runKind & " 开启频繁获取心跳包线程")
                    loopGetHeartBeatThread = New Thread(AddressOf LoopGetHeartBeat)
                    loopGetHeartBeatThread.Start()
                Catch ex As Exception

                End Try
            End If
        End If

    End Sub
    Public Sub GetMyTask()
        Try
            log("获取我的任务……")
            Dim dt As DataTable = SQLGetDT("select * from UserTaskTable Where DeviceID='" & myDeviceInfo.DeviceID & "' and EndTime>'" & Now.ToString("yyyy-MM-dd HH:mm:ss") & "' order by StartTime")
            If dt.Rows.Count = 0 Then
                SyncLock DeviceListLock
                    For i = 0 To DeviceList.Count - 1
                        Dim itm As DeviceStu = DeviceList(i)
                        If itm.DeviceID = myDeviceInfo.DeviceID Then
                            itm.Statu = "free"
                            DeviceList(i) = itm
                            Exit For
                        End If
                    Next
                End SyncLock
                log("没有任务")
                AddMyLog("查询任务", "成功,无待执行任务")
                Exit Sub
            End If
            Dim row As DataRow = dt.Rows(0)
            If isWorking = False Then
                log("有任务，开始处理……")
                AddMyLog("查询任务", "成功,有待执行任务，并开始处理")
                SyncLock DeviceListLock
                    For i = 0 To DeviceList.Count - 1
                        Dim itm As DeviceStu = DeviceList(i)
                        If itm.DeviceID = myDeviceInfo.DeviceID Then
                            itm.Statu = "working"
                            DeviceList(i) = itm
                            Exit For
                        End If
                    Next
                End SyncLock
                TaskWorkerThread = New Thread(AddressOf TaskWorker)
                TaskWorkerThread.Start(row)
            Else
                log("有任务，正在执行无需处理")
                AddMyLog("查询到有待执行任务，尝试执行", "失败，因为设备正在执行任务，等待现有任务执行完毕")
            End If
        Catch ex As Exception

        End Try
    End Sub

    Private Function GetOrderList(ByVal pds() As String) As List(Of Double)
        Dim ds As New List(Of Double)
        For Each sh In pds
            If IsNumeric(sh) = False Then
                Return Nothing
            Else
                Dim v As Double = Val(sh)
                ds.Add(v)
            End If
        Next
        For i = 0 To ds.Count - 1
            Dim a As Double = ds(i)
            For j = i + 1 To ds.Count - 1
                Dim b As Double = ds(j)
                If a > b Then
                    ds(i) = b
                    ds(j) = a
                End If
            Next
        Next
        Return ds
    End Function
    Private Sub TaskWorker(ByVal row As DataRow)
        Dim task As New NormalTaskStu
        task.StartTime = row("StartTime")
        task.EndTime = row("EndTime")
        task.TimeStep = row("TimeStep")
        task.TaskName = row("TaskName")
        task.TaskNickName = row("TaskNickName")
        task.TaskCode = row("TaskCode")
        task.PushWeChartToUserName = row("PushWeChatUserName")
        task.PushEmailToUserName = row("PushEmailUserName")
        task.UserName = row("UserName")
        task.DeviceID = row("DeviceID")
        task.DeviceName = row("DeviceName")
        workingTask = task
        Dim SleepInt As Integer = task.TimeStep * 10000
        Dim TssBscanOrder As tssOrder_stu
        Dim DataType As String = ""
        Dim FuncType As String = ""
        Dim CanShuQu As String = ""
        If task.TaskName = "可用评估" Then
            Dim code As String = task.TaskCode
            code = code.Replace("[", "").Replace("]", "")
            Dim pds() As String = code.Split(",")
            Dim pdlist As List(Of Double) = GetOrderList(pds)
            If IsNothing(pdlist) Then
                Return
            End If
            If pdlist.Count = 0 Then Return
            Dim freqbegin As Double
            Dim freqend As Double
            Dim freqstep As Double
            If pdlist.Count = 1 Then
                freqbegin = pdlist(0) - 25
                freqend = pdlist(0) + 25
                freqstep = 25
                TZBQ_JCPD_IndexList = New List(Of Integer)
                TZBQ_JCPD_IndexList.Add(0)
            Else
                freqbegin = pdlist(0)
                freqend = pdlist(pdlist.Count - 1)
                freqstep = pdlist(1) - pdlist(0)
                TZBQ_JCPD_IndexList = New List(Of Integer)
                For Each pd In pdlist
                    Dim int As Integer = -1
                    For i = freqbegin To freqend Step freqstep
                        int = int + 1
                        If pd = i Then
                            TZBQ_JCPD_IndexList.Add(int)
                            Exit For
                        End If
                    Next
                Next
            End If
            Dim ifbw As String = 40000
            Dim abw As String = freqstep
            Dim bits As String = 16
            Dim gcmode As String = "fagc"
            Dim gcvalue As String = 8
            Dim returndata As String = "fft"
            Dim returninter As String = 0
            Dim detetor As String = "real"
            'CanShuQu = "<bscan: tasknum=1" &
            '         ";freqbegin=" & freqbegin &
            '         ";freqend=" & freqend &
            '          ";freqstep=" & freqstep &
            '          ";ifbw=" & ifbw &
            '           ";abw=" & abw &
            '           ";bits=" & bits &
            '           ";gcmode=" & gcmode &
            '           ";gcvalue=" & gcvalue &
            '          ";returndata=" & returndata &
            '          ";detetor=" & detetor &
            '           ";returninter =" & returninter & ">"
            If gcvalue <> 8 And gcvalue <> 16 Then
                gcvalue = 8
            End If
            myFreqInfo = New json_PPSJ
            myFreqInfo.freqStart = freqbegin
            myFreqInfo.freqStep = freqstep
            myFreqInfo.freqEnd = freqend
            myFreqInfo.dataCount = ((freqend - freqbegin) / (freqstep * 0.001)) + 1
            flagHaveMyFreqInfo = True
            CanShuQu = "<bscan: tasknum=1" &
                             ";freqbegin=" & freqbegin &
                             ";freqend=" & freqend &
                              ";freqstep=" & freqstep &
                              ";ifbw=" & ifbw &
                               ";abw=" & abw &
                               ";rbw=" & abw &
                               ";bits=" & bits &
                               ";gcmode=" & gcmode &
                               ";gcvalue=" & gcvalue &
                              ";returndata=" & returndata &
                              ";detetor=" & detetor &
                               ";returninter=" & returninter
            If DSGDeviceKind = "DH" Then
                CanShuQu = CanShuQu & ";device=" & DHDeviceControlStr & ";>"
            Else
                CanShuQu = CanShuQu & ">"
            End If
            DataType = "task"
            FuncType = "bscan"
            isTZBQ_JCPD = True
            TZBQ_JCPDString = ""
            TZBQ_JCPDString = "<TZBQ:SSSJ," & myDeviceInfo.DeviceID & "," & pdlist.Count
            For i = 0 To pdlist.Count - 1
                TZBQ_JCPDString = TZBQ_JCPDString & ",{" & i & "}"
            Next
            For i = 0 To pdlist.Count - 1
                TZBQ_JCPDString = TZBQ_JCPDString & "," & pdlist(i)
            Next
            TZBQ_JCPDString = TZBQ_JCPDString + ">"
            TZBQ_JCPDString = getcrcstr(TZBQ_JCPDString)
        End If
        If task.TaskName = "频谱监测" Or task.TaskName = "台站监督" Or task.TaskName = "频谱取样" Or task.TaskName = "占用统计" Or task.TaskName = "黑广播捕获" Or task.TaskName = "违章捕获" Then
            Try
                TssBscanOrder = JsonConvert.DeserializeObject(task.TaskCode, GetType(tssOrder_stu))
                If TssBscanOrder.task = "bscan" Then
                    Dim freqbegin As Double = TssBscanOrder.freqStart
                    Dim freqend As Double = TssBscanOrder.freqEnd
                    Dim freqstep As Double = TssBscanOrder.freqStep
                    myFreqInfo = New json_PPSJ
                    myFreqInfo.freqStart = freqbegin
                    myFreqInfo.freqStep = freqstep
                    myFreqInfo.freqEnd = freqend
                    myFreqInfo.dataCount = ((freqend - freqbegin) / (freqstep * 0.001)) + 1
                    flagHaveMyFreqInfo = True
                    Dim ifbw As String = 40000
                    Dim abw As String = freqstep
                    Dim bits As String = 16
                    Dim gcmode As String = "fagc"
                    Dim gcvalue As String = 8
                    Dim returndata As String = "fft"
                    Dim returninter As String = 1
                    Dim detetor As String = "real"
                    If gcvalue <> 8 And gcvalue <> 16 Then
                        gcvalue = 8
                    End If
                    CanShuQu = "<bscan: tasknum=1" &
                             ";freqbegin=" & freqbegin &
                             ";freqend=" & freqend &
                              ";freqstep=" & freqstep &
                              ";ifbw=" & ifbw &
                               ";abw=" & abw &
                               ";rbw=" & abw &
                               ";bits=" & bits &
                               ";gcmode=" & gcmode &
                               ";gcvalue=" & gcvalue &
                              ";returndata=" & returndata &
                              ";detetor=" & detetor &
                               ";returninter=" & returninter
                    If DSGDeviceKind = "DH" Then
                        CanShuQu = CanShuQu & ";device=" & DHDeviceControlStr & ";>"
                    Else
                        CanShuQu = CanShuQu & ">"
                    End If
                    DataType = "task"
                    FuncType = "bscan"
                End If
            Catch ex As Exception

            End Try
        End If
        If task.TaskName = "黑广播捕获" Or task.TaskName = "违章捕获" Then
            isWarnWZ = True
            WarnInfoList = New List(Of WarnInfo)
            TssBscanOrder = JsonConvert.DeserializeObject(task.TaskCode, GetType(tssOrder_stu))
            minWarnNum = TssBscanOrder.WarnNum
            If IsNothing(TssBscanOrder.Legal) = False Then
                legalSigNal = TssBscanOrder.Legal
                warnDaikuan = TssBscanOrder.Daikuan
                WarnFucha = TssBscanOrder.Fucha
                If warnDaikuan Mod 2 = 0 Then warnDaikuan = warnDaikuan + 1
                If warnDaikuan < 3 Then warnDaikuan = 3
                If WarnFucha <= 0 Then WarnFucha = 1
            End If
        End If
        Sleep(1000)
        While True
            isWorking = True
            If Now >= task.EndTime Then
                Dim cls As TaskTSS
                SyncLock taskTSSHelperListLock
                    For Each itm As TaskTSS In taskTSSHelperList
                        If itm.myDeviceId = myDeviceInfo.DeviceID Then
                            cls = itm
                            Exit For
                        End If
                    Next
                End SyncLock
                If IsNothing(cls) Then
                    cls = New TaskTSS(myDeviceInfo.DeviceID, task)
                    cls.MakeTaskReport()
                Else
                    cls.MakeTaskReport()
                End If

                Exit While
            End If
            If Now >= task.StartTime Then
                If DSGDeviceKind = "DH" Then
                    SendStop2Device()
                    Sleep(1000)
                End If

                SendMsgToTSSDevByString(&H0, DataType, FuncType, CanShuQu, Nothing)
            End If
            Sleep(SleepInt)
        End While
        isWorking = False
        isTZBQ_JCPD = False
        isWarnWZ = False

        WarnInfoList = Nothing
        GetMyTask()
    End Sub




    Public Function GetAvg(ByVal y() As Double) As Double
        Dim sum As Double
        For Each d In y
            sum = sum + d
        Next
        Return sum / (y.Count)
    End Function
    Public Function XinHaoFenLi(ByVal xx() As Double, ByVal yy() As Double, ByVal Du As Integer, ByVal fucha As Double) As Double(,)
        Dim rx As New List(Of Double)
        Dim ry As New List(Of Double)
        Try
            If xx.Count <> yy.Count Then Return Nothing
            If Du Mod 2 = 0 Then Return Nothing
            Dim avg As Double = -80
            avg = GetAvg(yy)
            Dim jieti As Integer = (Du - 1) / 2
            For i = jieti To xx.Count - 1 - jieti
                Dim isxinhao As Boolean = True
                If (yy(i) < avg) Then
                    isxinhao = False
                    Continue For
                End If
                For j = i - jieti To i - 1
                    If yy(j) >= yy(j + 1) Then
                        isxinhao = False
                        Exit For
                    End If
                Next
                For j = i To i + jieti - 1
                    If yy(j) <= yy(j + 1) Then
                        isxinhao = False
                        Exit For
                    End If
                Next
                If yy(i) - fucha < yy(i + jieti) Then
                    isxinhao = False
                End If
                If yy(i) - fucha < yy(i - jieti) Then
                    isxinhao = False
                End If
                If isxinhao Then
                    rx.Add(xx(i))
                    ry.Add(yy(i))
                End If
            Next
            Dim result(rx.Count - 1, 1) As Double
            For i = 0 To rx.Count - 1
                result(i, 0) = rx(i)
                result(i, 1) = ry(i)
            Next
            Return result
        Catch ex As Exception
            '‘MsgBox(ex.ToString)
        End Try
    End Function

    Private Function DSGDecompare(kk As json_PPSJ) As json_PPSJ
        If kk.isDSGFreq Then
            kk.value = DSGBase2PPSJValues(kk.DSGFreqBase64)
        End If
        Return kk
    End Function


    Public Sub AddMyLog(ByVal log As String, ByVal result As String)
        AddDeviceLog(myDeviceInfo.DeviceID, myDeviceInfo.Name, myDeviceInfo.Address, log, result, "在线")
    End Sub
    Private Function getIpAndPortByDeviceID(ByVal id As String) As String
        For Each sh In DeviceList
            If sh.DeviceID = id Then
                Return sh.IP & ":" & sh.Port
            End If
        Next
    End Function
    Public Sub CloseSocket()
        Try
            Sleep(2000)
            If IsNothing(myDeviceInfo) = False Then
                AddMyLog("断线(TCP连接断开)", "")
            End If

            myClientSocket.Close()

        Catch ex As Exception
            If IsNothing(myDeviceInfo) = False Then
                log("断开前者TSS_socket,DeviceID=" & myDeviceInfo.DeviceID & ">ERR>" & ex.Message)
            End If
        End Try
    End Sub
    Public Sub CloseALL(ByVal isOld As Boolean, closeMsg As String)

        log("TSS_CloseALL-->[" & IP & ":" & Port & "]")
        AddMyLog("断线" & closeMsg, "")
        Dim oldstr As String = ""
        If isOld Then oldstr = "前者"
        Try
            If IsNothing(broadcastThread) = False Then
                broadcastThread.Abort()
                log("关闭" & oldstr & "消息广播线程成功")
            End If
        Catch ex As Exception
            log("关闭" & oldstr & "消息广播线程失败" & ex.Message)
        End Try
        Try
            If IsNothing(TaskWorkerThread) = False Then
                TaskWorkerThread.Abort()
                log("关闭" & oldstr & "任务线程成功")
            End If
        Catch ex As Exception
            log("关闭" & oldstr & "任务线程失败" & ex.Message)
        End Try
        Try
            If IsNothing(myHttpListener) = False Then
                myHttpListener.Abort()
                log("关闭" & oldstr & "myHttpListener成功")
            End If
        Catch ex As Exception
            log("关闭" & oldstr & "myHttpListener失败" & ex.ToString)
        End Try
        If flagNeedGetGPSLoop Then
            Try
                If IsNothing(loopGetHeartBeatThread) = False Then
                    loopGetHeartBeatThread.Abort()
                    log("关闭" & oldstr & "循环取TSS经纬度线程成功")
                End If
            Catch ex As Exception
                log("关闭" & oldstr & "循环取网关经纬度线程失败！" & ex.Message)
            End Try
        End If
        Try
            If IsNothing(webSocketListener) = False Then
                webSocketListener.Close()
                log("关闭" & oldstr & "webSocketListener成功")
            End If
        Catch ex As Exception
            log("关闭" & oldstr & "webSocketListener成功" & ex.Message)
        End Try
        Try
            If IsNothing(DeviceListLock) Then DeviceListLock = New Object
            If IsNothing(DeviceList) Then DeviceList = New List(Of DeviceStu)
            SyncLock DeviceListLock
                For i = DeviceList.Count - 1 To 0 Step -1
                    Dim itm As DeviceStu = DeviceList(i)
                    If itm.IP = IP And itm.Port = Port Then
                        DeviceList.RemoveAt(i)
                        Dim str As String = "DeviceList已移除" & oldstr & "DeviceID=" & itm.DeviceID & ",位置=" & itm.Address & "(" & IP & ":" & Port & ")"
                        log(str)
                        Exit For
                    End If
                Next
            End SyncLock

            RaiseEvent RefrushDeviceList()
        Catch ex As Exception

        End Try
        If isOld = False Then
            CloseSocket()
        End If

        'Dim th As New Thread(AddressOf th_CloseDeviceSocket)
        'th.Start(myClientSocket)
    End Sub
    Private Sub th_CloseDeviceSocket(ByVal obj As Object)
        Try
            Dim htp As Socket = CType(obj, Socket)
            log("准备断开前者TSS_Socket,DeviceID=" & myDeviceInfo.DeviceID)
            ' Threading.Thread.Sleep(10000)
            htp.Close()
            log("已断开前者TSS_Socket,DeviceID=" & myDeviceInfo.DeviceID)
        Catch ex As Exception
            log("断开前者TSS_socket,DeviceID=" & myDeviceInfo.DeviceID & ">ERR>" & ex.Message)
        End Try
    End Sub
    Private Sub handleTm(ByVal by() As Byte)
        HeartWatcher = 1
        Dim tm As tssMsg = byte2tssmsg(by)
        ' Dim base As String = Convert.ToBase64String(by)
        Dim id As String = tm.deviceID
        Task.Run(Sub()
                     If isHandledLogin AndAlso IsNothing(myDeviceInfo) = False AndAlso myDeviceInfo.DeviceID <> "" Then
                         id = myDeviceInfo.DeviceID
                     End If
                     Dim nowTime As Date = Now
                     Dim isToday As Boolean = False
                     Dim isMonth As Boolean = False
                     Dim isYear As Boolean = False
                     Dim sql As String = $"select LastDateTime from deviceTable where deviceId='{id}'"
                     Dim lastDateTime As String = SQLInfo(sql)
                     If lastDateTime.Length > 10 Then
                         Dim lastDate As Date = nowTime
                         Date.TryParse(lastDateTime, lastDate)
                         isYear = (lastDate.Year = nowTime.Year)
                         If isYear Then
                             isMonth = (lastDate.Month = nowTime.Month)
                             If isMonth Then
                                 isToday = (lastDate.Day = nowTime.Day)
                             End If
                         End If
                     End If
                     lastDateTime = nowTime.ToString("yyyy-MM-dd HH:mm:ss")
                     Dim thisLength As Double = by.Length
                     thisLength = thisLength / 1024
                     thisLength = Math.Round(thisLength, 2)
                     If isToday = False Then SQLCmd($"update deviceTable set todayFlow=0 where deviceId='{id}'")
                     If isMonth = False Then SQLCmd($"update deviceTable set MonthFlow=0 where deviceId='{id}'")
                     If isYear = False Then SQLCmd($"update deviceTable set YearFlow=0 where deviceId='{id}'")
                     SQLCmd($"update deviceTable set LastDateTime='{lastDateTime}', YearFlow=YearFlow+{thisLength},MonthFlow=MonthFlow+{thisLength},todayFlow=todayFlow+{thisLength} where deviceId='{id}'")
                 End Sub)

        ' log("<接收TSS_Dev> id=" & id & ",datatype=" & tm.datatype & "," & "functype=" & tm.functype)
        'If tm.deviceID = "0120170003" Then
        '    log("TSS->" & tm.deviceID & "->")
        '    log("           buffer.length=" & by.Length)
        '    log("           tm.dataType=" & tm.datatype)
        '    log("           tm.funcType=" & tm.functype)
        '    log("           tm.parm=" & tm.canshuqu)
        'End If
        If tm.datatype = "ACK" Then
            Dim msg As String = tm.canshuqu
            If InStr(msg, "cmd=WSZL") Then

            End If
        End If
        If InStr(tm.functype, "logon") Then
            HandleLogin(id, tm.canshuqu)
            Sleep(1000)
            If is701Device = False Then SendMsgToTSSDevByString(&H0, "task", "getheartbeat", "", Nothing)
            If is701Device Then SendMsgToTSSDevByString(&H0, "task", "logon", "", Nothing)
            Return
            '  Sleep(1000)
            ' SendMsgToTSSDevByString(&H0, "task", "logon", "", Nothing)
        End If
        If tm.datatype = "link" Then
            If tm.functype = "logon" Then
                HandleLogin(id, tm.canshuqu)
                Sleep(1000)
                If is701Device = False Then SendMsgToTSSDevByString(&H0, "task", "getheartbeat", "", Nothing)
                If is701Device Then SendMsgToTSSDevByString(&H0, "task", "logon", "", Nothing)
                Return
                ' Sleep(1000)
                ' SendMsgToTSSDevByString(&H0, "task", "logon", "", Nothing)
            End If
        End If
        If tm.datatype = "data" Then
            If tm.functype = "version" Then
                myControlerVersion = tm.canshuqu
            End If
        End If
        'If tm.datatype = "links" Then
        '    If tm.functype = "logonon" Then
        '        HandleLogin(id)
        '        Sleep(1000)
        '        SendMsgToTSSDevByString(&H0, "task", "getheartbeat", "", Nothing)
        '    End If
        'End If
        If tm.datatype = "DevData" Then
            If tm.functype = "State" Then
                If isWorking = False Then
                    log("收到" & myDeviceInfo.DeviceID & "上报设备状态")
                    handleDeviceState(tm, IP, Port)
                    Return
                End If
            End If
        End If
        If tm.datatype = "spect_conti" Then
            If tm.functype = "bscan" Then
                If is701Device Then
                    device701FreqReSendCount = 0
                End If
                handlePinPuFenXi(tm.deviceID, tm.shujuqu, "")
                Return
            End If
        End If
        If tm.datatype.Length > 11 And tm.functype.Length > 5 Then
            If tm.datatype.Substring(0, 11) = "spect_conti" Then
                If tm.functype.Substring(0, 5) = "bscan" Then
                    If is701Device Then
                        device701FreqReSendCount = 0
                    End If
                    handlePinPuFenXi(tm.deviceID, tm.shujuqu, "")

                    Return
                End If
            End If
        End If

        If tm.datatype = "audio" Then
            If tm.functype = "ifscan_wav" Then
                If is701Device Then
                    device701FreqReSendCount = 0
                End If
                HandleAudio(tm.shujuqu)
                Return
            End If
        End If
        If tm.datatype.ToLower() = "data" Then
            If tm.functype.ToLower() = "heartbeat" Then
                HandleHeartBeat(tm)
                'If myDeviceInfo.DeviceID = "DSG_DH0047" Then
                ' log("47收到设备心跳包" & myDeviceInfo.DeviceID & "--->" & tm.canshuqu)
                'End If
                'log("收到网关设备心跳包" & myDeviceInfo.DeviceID & "--->" & tm.canshuqu)
            End If
        End If
        If tm.datatype.ToLower() = "data" Then
            If tm.functype.ToLower() = "screenimage" Then
                Dim buffer() As Byte = tm.shujuqu
                If IsNothing(buffer) = False Then
                    Dim jsonmsg As JSON_Msg
                    jsonmsg.func = "ScreenImage"
                    jsonmsg.msg = Convert.ToBase64String(buffer)
                    Dim deviceMsg As String = JsonConvert.SerializeObject(jsonmsg)
                    AddDeviceMsg(deviceMsg)
                End If
            End If
        End If

    End Sub
    Private Sub HandleHeartBeat(tm As tssMsg)
        Dim str As String = tm.canshuqu
        str = str.Replace("<", "").Replace(">", "")
        Dim st() As String = str.Split(":")
        If st(0) <> "heartbeat" Then Return
        Dim sk() As String = st(1).Split(",")
        Dim sk1() As String = st(1).Split(";")
        Dim flagIsUseFenHao As Boolean = False
        If IsNothing(sk) = False Then
            If IsNothing(sk1) = False Then
                If sk1.Length > sk.Length Then
                    flagIsUseFenHao = True
                    sk = sk1
                End If
            End If
        Else
            flagIsUseFenHao = True
            sk = sk1
        End If

        If IsNothing(sk) Then
            log("heartBeat sk is null")
            Return
        End If
        If IsNothing(DeviceListLock) Then DeviceListLock = New Object
        SyncLock DeviceListLock
            If IsNothing(DeviceList) Then DeviceList = New List(Of DeviceStu)
            For i = 0 To DeviceList.Count - 1
                Dim sh As DeviceStu = DeviceList(i)
                If sh.DeviceID = myDeviceInfo.DeviceID Then
                    Dim deviceGPSReporterLng As Double
                    Dim deviceGPSReporterLat As Double
                    Dim flagHaveLng As Boolean = False
                    Dim flagHaveLat As Boolean = False
                    Dim flagGpsOK As Boolean = False
                    Dim flagHaveGPSStatus As Boolean = False
                    Try
                        For Each skt In sk
                            If Not flagIsUseFenHao Then skt = skt.Replace(";", "")
                            If skt = "" Then Continue For
                            If skt.Contains("=") = False Then Continue For
                            Dim k As String = skt.Split("=")(0)
                            Dim v As String = skt.Split("=")(1)
                            If k = "deviceid" Then sh.NetDeviceID = v
                            If k = "swnet" Then
                                If v = "in" Then sh.NetSwitch = 1
                                If v = "out" Then sh.NetSwitch = 2
                            End If
                            If k = "longitude" Then
                                flagHaveGPSStatus = True
                                If IsNothing(v) = False Then
                                    If v <> "" Then
                                        If IsNumeric(v) Then
                                            flagHaveLng = True
                                            deviceGPSReporterLat = v  '2018-12-09 设备上报的经纬度是反的，故迎合设备
                                            ' deviceGPSReporterLng = v
                                        End If
                                    End If
                                End If
                            End If
                            If k = "latitude" Then
                                flagHaveGPSStatus = True
                                If IsNothing(v) = False Then
                                    If v <> "" Then
                                        If IsNumeric(v) Then
                                            flagHaveLat = True
                                            ' deviceGPSReporterLat = v
                                            deviceGPSReporterLng = v  '2018-12-09 设备上报的经纬度是反的，故迎合设备
                                        End If
                                    End If
                                End If
                            End If

                            flagGpsOK = True
                            'If k = "lgps_status" Then
                            '    flagHaveGPSStatus = True
                            '    If IsNothing(v) = False Then
                            '        If v <> "" Then
                            '            If IsNumeric(v) Then
                            '                If v = 1 Then
                            '                    flagGpsOK = True
                            '                End If
                            '            End If
                            '        End If
                            '    End If
                            'End If
                        Next
                    Catch ex As Exception
                        ' If myDeviceInfo.DeviceID = "DSG-GW0100" Then log(ex.ToString)
                    End Try
                    Try
                        If flagGpsOK And flagHaveLat And flagHaveLng Then
                            If deviceGPSReporterLng > 0 And deviceGPSReporterLat > 0 Then
                                If deviceGPSReporterLat > deviceGPSReporterLng Then
                                    ' log("经纬度需要转置，deviceId=" & myDeviceInfo.DeviceID)
                                    Dim t As Double = deviceGPSReporterLat
                                    deviceGPSReporterLat = deviceGPSReporterLng
                                    deviceGPSReporterLng = t
                                End If
                                Dim cinfo As CoordInfo = GPS2BDS(deviceGPSReporterLng, deviceGPSReporterLat)
                                deviceGPSReporterLng = cinfo.x
                                deviceGPSReporterLat = cinfo.y
                                sh.Lng = deviceGPSReporterLng
                                sh.Lat = deviceGPSReporterLat

                                SyncLock myRunLocationLock
                                    If IsNothing(myRunLocation) Then myRunLocation = New RunLocation
                                    myRunLocation.lng = sh.Lng
                                    myRunLocation.lat = sh.Lat
                                    myRunLocation.lat = sh.Lat
                                    myRunLocation.time = Now.ToString("yyyy-MM-dd HH:mm:ss")
                                End SyncLock
                                Try
                                    Dim sqlTmp As String = "update deviceTable set Lng='{0}',Lat='{1}' where DeviceID='{2}'"
                                    sqlTmp = String.Format(sqlTmp, New String() {sh.Lng, sh.Lat, sh.DeviceID})
                                    SQLCmd(sqlTmp)
                                Catch ex As Exception

                                End Try
                            End If
                            '   log("收到TSS设备上报正确经纬度！id=" & myDeviceInfo.DeviceID & "," & deviceGPSReporterLng & "," & deviceGPSReporterLat)
                        Else
                            If flagHaveGPSStatus Then
                                log("收到TSS设备上报经纬度字段，id=" & myDeviceInfo.DeviceID)
                            End If
                        End If
                    Catch ex As Exception

                    End Try
                    DeviceList(i) = sh
                    Exit For
                End If
            Next
            RaiseEvent RefrushDeviceList()
        End SyncLock
    End Sub

    Structure json_Audio
        Dim freq As Double
        Dim samplingRate As Long
        Dim audioBit As Integer
        Dim channelNum As Integer
        Dim audioBase64 As String
        Sub New(_freq As Double, _samplingRate As Long, _audioBit As Integer, _channelNum As Integer, _audioBase64 As String)
            freq = _freq
            samplingRate = _samplingRate
            audioBit = _audioBit
            channelNum = _channelNum
            audioBase64 = _audioBase64
        End Sub
    End Structure
    Dim oldAudioInfo As New json_Audio(0, 0, 0, 0, "")
    Dim audioBuffer() As Byte
    Dim audioCollectInt As Integer = 0
    Dim audioCollectMaxInt As Integer = 20
    Dim audioCollectLock As New Object
    Private Sub HandleAudioWebSocket(ByVal wavBuffer() As Byte)
        Try
            Dim wav_buffer(wavBuffer.Count - 45) As Byte
            Dim caiyanglv As Integer = BitConverter.ToInt32(wavBuffer, 30)
            Dim yinpinweishu As Integer = wavBuffer(34)
            Dim lenOfPerSec As Integer = yinpinweishu * 1000
            If yinpinweishu = 8 Then lenOfPerSec = 8000
            If yinpinweishu = 16 Then lenOfPerSec = 84787
            Dim tongdaoshu As Integer = wavBuffer(35)
            Dim xinhaopinlv As Double = BitConverter.ToDouble(wavBuffer, 2)
            Array.Copy(wavBuffer, 44, wav_buffer, 0, wav_buffer.Count)
            Dim isSend As Boolean = True
            If isSend Then
                Dim AudioJosn As json_Audio
                AudioJosn.freq = xinhaopinlv
                AudioJosn.samplingRate = caiyanglv
                AudioJosn.audioBit = yinpinweishu
                AudioJosn.channelNum = tongdaoshu
                AudioJosn.audioBase64 = Convert.ToBase64String(wav_buffer)
                Dim deviceMsg As String = JsonConvert.SerializeObject(AudioJosn)
                SendAudioToAllUser(deviceMsg)
            End If
        Catch ex As Exception

        End Try
    End Sub
    Private Sub HandleAudio(ByVal wavBuffer() As Byte)
        Try
            'Dim th As New Thread(AddressOf HandleAudioWebSocket)
            'th.Start(wavBuffer)
            Dim wav_buffer(wavBuffer.Count - 45) As Byte
            Dim caiyanglv As Integer = BitConverter.ToInt32(wavBuffer, 30)
            Dim yinpinweishu As Integer = wavBuffer(34)
            Dim lenOfPerSec As Integer = yinpinweishu * 1000
            If yinpinweishu = 8 Then lenOfPerSec = 8000
            If yinpinweishu = 16 Then lenOfPerSec = 84787
            Dim tongdaoshu As Integer = wavBuffer(35)
            Dim xinhaopinlv As Double = BitConverter.ToDouble(wavBuffer, 2)
            If oldAudioInfo.freq <> xinhaopinlv Or oldAudioInfo.samplingRate <> caiyanglv Or oldAudioInfo.audioBit <> yinpinweishu Or oldAudioInfo.channelNum <> tongdaoshu Then
                SyncLock audioCollectLock
                    audioCollectInt = 0
                    audioBuffer = Nothing
                    oldAudioInfo.freq = xinhaopinlv
                    oldAudioInfo.samplingRate = caiyanglv
                    oldAudioInfo.audioBit = yinpinweishu
                    oldAudioInfo.channelNum = tongdaoshu
                End SyncLock
            End If
            Array.Copy(wavBuffer, 44, wav_buffer, 0, wav_buffer.Count)
            Dim isSend As Boolean = False
            Dim wavByte() As Byte
            SyncLock audioCollectLock
                If IsNothing(audioBuffer) Then
                    audioBuffer = wav_buffer
                Else
                    audioBuffer = audioBuffer.Concat(wav_buffer).ToArray
                End If
                audioCollectInt = audioCollectInt + 1
                If audioCollectInt >= audioCollectMaxInt Then
                    wavByte = addWavHead(audioBuffer, caiyanglv, yinpinweishu, tongdaoshu)
                    audioCollectInt = 0
                    audioBuffer = Nothing
                    If IsNothing(wavByte) = False Then
                        isSend = True
                    End If
                End If
            End SyncLock
            If isSend Then
                Dim AudioJosn As json_Audio
                AudioJosn.freq = xinhaopinlv
                AudioJosn.samplingRate = caiyanglv
                AudioJosn.audioBit = yinpinweishu
                AudioJosn.channelNum = tongdaoshu
                AudioJosn.audioBase64 = Convert.ToBase64String(wavByte)
                Dim jsonmsg As JSON_Msg
                jsonmsg.func = "ifscan_wav"
                jsonmsg.msg = JsonConvert.SerializeObject(AudioJosn)
                Dim deviceMsg As String = JsonConvert.SerializeObject(jsonmsg)
                AddDeviceMsg(deviceMsg)
            End If
        Catch ex As Exception

        End Try
    End Sub


    Structure DSGFreqDataStu
        Dim freqStart As Double
        Dim freqStep As Double
        Dim deviceID As String
        Dim dataCount As Integer
        Dim xx() As Double
        Dim yy() As Double
        Dim DSGFreqModuleId As Integer
    End Structure
    Dim DSGFreqModule As DSGFreqDataStu
    Dim isHaveDSGFreqModule As Boolean = False
    Structure DSGFreqStu
        Dim index As Integer
        Dim value As Double
        Sub New(_index As Integer, _value As Double)
            index = _index
            value = _value
        End Sub
    End Structure
    Private Function GetNewDSGFreqModuleId() As Integer
        Dim m As Integer = 9999
        Dim n As Integer = 1000
        Dim k As Integer = Int(Rnd() * (m - n + 1)) + n
        Return k
    End Function
    Structure JSON_Msg
        Dim func As String
        Dim msg As String
    End Structure
    Structure WarnInfo
        Dim Freq As Double
        Dim Value As Double
        Dim MinWarnNum As Integer
        Dim RealWarnNum As Integer
        Sub New(ByVal _Freq As Double, ByVal _Value As Double, ByVal _MinWarNum As Integer)
            Freq = _Freq
            Value = _Value
            MinWarnNum = _MinWarNum
        End Sub
    End Structure
    Private Function GetLen(str As String) As String
        Dim len As Long = str.Length
        If len < 1024 Then
            Return len & "b"
        End If
        If len < 1024 * 1024 Then
            Dim d As Double = len / 1024
            Return d.ToString("0.00") & "kb"
        End If
        Dim d2 As Double = len / （1024 * 1024)
        Return d2.ToString("0.00") & "mb"
    End Function
    Private Function CheckLocation(lng As String, lat As String) As Boolean
        If IsNothing(lng) Or IsNothing(lat) Then Return False
        If lng = "" Or lat = "" Then Return False
        If IsNumeric(lng) = False Or IsNumeric(lat) = False Then Return False
        If Val(lng) <= 0 Or Val(lat) <= 0 Then Return False
        Return True
    End Function
    Public Sub SetmyRunLocation(runlocation As RunLocation)
        If IsNothing(runlocation) = False Then
            SyncLock myRunLocationLock
                Me.myRunLocation = runlocation
                myDeviceInfo.Lng = runlocation.lng
                myDeviceInfo.Lat = runlocation.lat
                Dim sqlTmp As String = "update deviceTable set Lng='{0}',Lat='{1}' where DeviceID='{2}'"
                sqlTmp = String.Format(sqlTmp, New String() {runlocation.lng, runlocation.lat, myDeviceInfo.DeviceID})
                SQLCmd(sqlTmp)
            End SyncLock
        End If
    End Sub
    Private Sub handlePinPuFenXi(ByVal deviceID As String, ByVal PinPuShuJu() As Byte, canshuqu As String)
        Try
            '  log($"TSS设备，{deviceID}，上报频谱数据")
            Dim p As ppsj = shuju2ppsj(PinPuShuJu)
            Dim qishi As Double = p.qishipinlv
            Dim bujin As Double = p.bujin

            Dim weishu As Integer = p.weishu
            Dim geshu As Integer = 0
            Dim jieshu As Double
            Dim xx() As Double, yy() As Double

            Dim jifenleixing As Integer = p.jifenleixing
            If jifenleixing <> 0 Then Exit Sub
            If weishu = 8 Then
                geshu = p.pinpuzongshu
                ReDim xx(geshu - 1), yy(geshu - 1)
                For i = 0 To geshu - 1
                    yy(i) = p.pinpushuju(i) - 127 - 80
                    xx(i) = qishi + i * bujin
                    yy(i) = yy(i).ToString("0.0")
                Next
            End If

            If weishu = 16 Then
                geshu = p.pinpuzongshu / 2
                ReDim xx(geshu - 1), yy(geshu - 1)
                Dim isYouXiao As Boolean = True
                For i = 0 To geshu - 1
                    yy(i) = BitConverter.ToInt16(p.pinpushuju, i * 2) * 0.1
                    If yy(i) >= 0 Then
                        isYouXiao = False
                        Exit For
                    End If
                    yy(i) = yy(i).ToString("0.0")
                    xx(i) = qishi + i * bujin
                Next
                If isYouXiao = False Then
                    Exit Sub
                End If
            End If
            If IsNothing(xx) Or IsNothing(yy) Then Return
            If xx.Count <> yy.Count Then Return
            If xx.Count = 0 Or yy.Count = 0 Then Return


            Task.Run(Sub()
                         SyncLock lsInfoLock
                             If isLiSanSaoMiao Then
                                 HandleLisanFreq(xx, yy)
                             End If
                         End SyncLock
                     End Sub)


            If flagHaveMyFreqInfo Then
                If myFreqInfo.freqStart = xx(0) Then
                    If Not myFreqInfo.freqEnd = xx(xx.Count - 1) Then
                        Dim tmpFreqStep As Single = myFreqInfo.freqStep
                        Dim bujinFreqStep As Single = bujin * 1000
                        Dim bool As Boolean = (tmpFreqStep = bujinFreqStep)
                        If bool Then
                            If myFreqInfo.dataCount - geshu = 1 Then
                                geshu = myFreqInfo.dataCount
                                Dim xTmp(geshu - 1), yTmp(geshu - 1) As Double
                                For i = 0 To xx.Count - 1
                                    xTmp(i) = xx(i）
                                    yTmp(i) = yy(i)
                                Next
                                If xTmp.Count > 2 Then
                                    xTmp(xTmp.Count - 1) = myFreqInfo.freqEnd
                                    yTmp(yTmp.Count - 1) = yTmp(yTmp.Count - 2)
                                End If
                                xx = xTmp
                                yy = yTmp
                            End If
                        End If
                    End If
                End If
            End If
            If IsNothing(xx) Then Exit Sub
            If IsNothing(yy) Then Exit Sub
            jieshu = xx(xx.Count - 1)

            If isTZBQ_JCPD = False Then
                Dim originalFreqJson As json_PPSJ
                Dim jsonPP As New json_PPSJ
                jsonPP.freqStart = qishi
                jsonPP.freqStep = bujin
                jsonPP.freqEnd = jieshu
                jsonPP.dataCount = geshu
                jsonPP.deviceID = myDeviceInfo.DeviceID
                jsonPP.value = yy
                jsonPP.runLocation = Nothing

                originalFreqJson = jsonPP.Copy

                DailyFreqHelper.OnDeviceFreq(jsonPP.Copy, myDeviceInfo)
                Dim dataCount As Integer = yy.Count
                Dim isReduceCount As Boolean = True
                If isReduceCount Then
                    Dim maxCount As Integer = 5000
                    If dataCount > maxCount Then
                        Dim realValue() As Double = yy
                        Dim xlist As New List(Of Double)
                        Dim ylist As New List(Of Double)
                        Dim st As Integer = Math.Ceiling(dataCount / maxCount)
                        jsonPP.freqStep = bujin * st
                        For i = 0 To dataCount - 1 Step st
                            xlist.Add(qishi + i * bujin)
                            ylist.Add(realValue(i))
                        Next
                        If xlist(xlist.Count - 1) <> qishi + (dataCount - 1) * bujin Then
                            xlist.Add(qishi + (dataCount - 1) * bujin)
                            ylist.Add(yy(yy.Length - 1))
                        End If
                        '  xlist(xlist.Count - 1) = xx(xx.Count - 1)
                        xx = xlist.ToArray
                        yy = ylist.ToArray
                        jsonPP.dataCount = yy.Count
                        jsonPP.value = yy
                        'jsonPP.isDSGFreq = True
                        'jsonPP.isDSGFreqModule = True
                        'jsonPP.DSGFreqModuleId = DSGFreqModule.DSGFreqModuleId
                    End If
                End If
                If UseDSGFreq Then
                    jsonPP.isDSGFreq = True
                    jsonPP.DSGFreqBase64 = PPSJValues2DSGBase(jsonPP.value)
                    jsonPP.value = Nothing
                    originalFreqJson.isDSGFreq = True
                    originalFreqJson.DSGFreqBase64 = PPSJValues2DSGBase(originalFreqJson.value)
                    originalFreqJson.value = Nothing
                End If

                If myDeviceInfo.RunKind = "bus" Or myDeviceInfo.RunKind = "car" Then
                    If IsNothing(myRunLocation) = False Then
                        Try
                            Try
                                ''更新公交实时经纬度
                                Dim rlc As RunLocation = myRunLocation
                                rlc.time = Now.ToString("yyyy-MM-dd HH:mm:ss")
                                Dim isRefrushGPS As Boolean = CheckLocation(rlc.lng, rlc.lat)
                                jsonPP.runLocation = rlc
                                originalFreqJson.runLocation = rlc
                                Dim sqlTmp As String = ""
                                If isRefrushGPS Then
                                    If myDeviceInfo.RunKind = "bus" Then
                                        sqlTmp = "update BusLineTable set time='{0}',lng='{1}',lat='{2}' where DeviceID='{3}'"
                                        sqlTmp = String.Format(sqlTmp, New String() {Now.ToString("yyyy-MM-dd HH:mm:ss"), rlc.lng, rlc.lat, myDeviceInfo.DeviceID})
                                    End If
                                Else
                                    If myDeviceInfo.RunKind = "bus" Then
                                        sqlTmp = "update BusLineTable set time='{0}' where DeviceID='{1}'"
                                        sqlTmp = String.Format(sqlTmp, New String() {Now.ToString("yyyy-MM-dd HH:mm:ss"), myDeviceInfo.DeviceID})
                                    End If
                                End If
                                SQLCmd(sqlTmp)
                            Catch ex As Exception

                            End Try
                            ''储存公交车频谱地图
                            Dim isSaveBusFreq As Boolean = True
                            If myDeviceInfo.RunKind = "car" Then
                                Dim nDate As Date = Now
                                Dim ts As TimeSpan = nDate - lastCarFreqGisTime
                                If ts.Seconds < 10 Then
                                    isSaveBusFreq = False
                                Else
                                    lastCarFreqGisTime = Now
                                End If
                            End If
                            If isSaveBusFreq Then
                                If IsNothing(jsonPP.runLocation) = False Then
                                    If jsonPP.runLocation.lng > 0 And jsonPP.runLocation.lat > 0 Then

                                        Dim tmpJsonPP As json_PPSJ = jsonPP
                                        Dim sql As String = "insert into freqGisTable values('0','{0}','{1}','{2}','{3}','{4}','{5}','{6}','{7}','{8}','{9}','{10}','{11}','{12}')"
                                        sql = "insert into freqGisTable (type,lineid,date,time,freqStart,freqEnd,freqStep,pointCount,freqJson,lng,lat,freqJsonLen,isDSGFreq,grid,deviceId,runKind,originalFreqJson) values({0})"
                                        Dim params As String = "'{0}','{1}','{2}','{3}','{4}','{5}','{6}','{7}','{8}','{9}','{10}','{11}','{12}','{13}','{14}','{15}','{16}'"
                                        Dim strList As New List(Of String)
                                        strList.Add(1)
                                        If freqGisBusLineId = "" Then freqGisBusLineId = 0
                                        strList.Add(freqGisBusLineId)
                                        strList.Add(Now.ToString("yyyy-MM-dd"))
                                        strList.Add(Now.ToString("yyyy-MM-dd HH:mm:ss"))
                                        strList.Add(tmpJsonPP.freqStart)
                                        strList.Add(jieshu)
                                        strList.Add(tmpJsonPP.freqStep)
                                        strList.Add(tmpJsonPP.dataCount)

                                        'Dim isUseDSGFreq As Boolean = True
                                        'If isUseDSGFreq Then
                                        '    If UseDSGFreq Then
                                        '        tmpJsonPP.isDSGFreq = True
                                        '        tmpJsonPP.DSGFreqBase64 = PPSJValues2DSGBase(tmpJsonPP.value)
                                        '        tmpJsonPP.value = Nothing
                                        '    Else
                                        '        isUseDSGFreq = False
                                        '    End If
                                        'End If
                                        Dim cinfo As CoordInfo = BDS2GPS(tmpJsonPP.runLocation.lng, tmpJsonPP.runLocation.lat)
                                        Dim gird As GridInfo = GetGridBySQL(cinfo.x, cinfo.y)
                                        jsonPP.grid = gird
                                        tmpJsonPP.grid = gird
                                        originalFreqJson.grid = gird
                                        Dim jsonTmpJsonPP As String = JsonConvert.SerializeObject(tmpJsonPP)
                                        strList.Add(jsonTmpJsonPP)
                                        strList.Add(tmpJsonPP.runLocation.lng)
                                        strList.Add(tmpJsonPP.runLocation.lat)
                                        strList.Add(GetLen(jsonTmpJsonPP))
                                        If UseDSGFreq Then
                                            strList.Add(1)
                                        Else
                                            strList.Add(0)
                                        End If


                                        strList.Add(gird.id)
                                        strList.Add(myDeviceInfo.DeviceID)
                                        strList.Add(myDeviceInfo.RunKind)
                                        strList.Add(JsonConvert.SerializeObject(originalFreqJson))
                                        params = String.Format(params, strList.ToArray())
                                        sql = String.Format(sql, params)
                                        Dim result As String = SQLCmd(sql)
                                        ' log(sql)
                                        log(result)
                                    End If
                                End If
                            End If
                        Catch ex As Exception
                            log(ex.ToString)
                        End Try
                    End If
                End If
                Dim tmpMsg As String = JsonConvert.SerializeObject(jsonPP)
                Dim jsonmsg As JSON_Msg
                jsonmsg.func = "bscan"
                jsonmsg.msg = tmpMsg
                Dim deviceMsg As String = JsonConvert.SerializeObject(jsonmsg)
                AddDeviceMsg(deviceMsg)

                If isWorking Then
                    If isWarnWZ Then
                        Try
                            If IsNothing(WarnInfoList) = False Then
                                Dim sigNal(,) As Double = XinHaoFenLi(xx, yy, warnDaikuan, WarnFucha)
                                If IsNothing(sigNal) = False Then
                                    For i = 0 To sigNal.GetLength(0) - 1
                                        Dim pinlv As Double = sigNal(i, 0)
                                        Dim changqiang As Double = sigNal(i, 1)
                                        Dim isIn As Boolean = False
                                        For j = WarnInfoList.Count - 1 To 0 Step -1
                                            Dim s As WarnInfo = WarnInfoList(j)
                                            If s.Freq = pinlv Then
                                                isIn = True
                                                If s.Value = 0 Then s.Value = changqiang
                                                If s.Value < changqiang Then
                                                    s.Value = changqiang
                                                End If
                                                s.RealWarnNum = s.RealWarnNum + 1
                                                Dim minNum As Integer = s.MinWarnNum
                                                If s.RealWarnNum >= s.MinWarnNum Then
                                                    'log("s.RealWarnNum=" & s.RealWarnNum)
                                                    'log(" s.MinWarnNum=" & s.MinWarnNum)
                                                    s.RealWarnNum = 0
                                                    '生成预警字段
                                                    Dim pl As Double = pinlv
                                                    Dim cq As Double = s.Value
                                                    Dim id As String = myDeviceInfo.DeviceID
                                                    Dim lng As String = myDeviceInfo.Lng
                                                    Dim lat As String = myDeviceInfo.Lat
                                                    Dim time1 As String = Now.ToString("yyyy-MM-dd")
                                                    Dim time2 As String = Now.ToString("HH:mm:ss")
                                                    Dim d As Double = cq
                                                    Dim myValue As Double = -65
                                                    Dim t As Integer = Math.Abs(100 * ((d - myValue) / myValue))
                                                    If IsNothing(workingTask) = False Then
                                                        Dim deviceTZBQMsg As String = ""
                                                        If workingTask.TaskName = "黑广播捕获" Then
                                                            deviceTZBQMsg = "<TZBQ:WARN," & id & ",HGB," & lng & "," & lat & "," & time1 & "," & time2 & "," & pl & "," & d & "," & t & ">"
                                                        End If
                                                        If workingTask.TaskName = "违章捕获" Then
                                                            deviceTZBQMsg = "<TZBQ:WARN," & id & ",WZ," & lng & "," & lat & "," & time1 & "," & time2 & "," & pl & "," & d & "," & t & ">"
                                                        End If
                                                        If deviceTZBQMsg <> "" Then
                                                            deviceTZBQMsg = getcrcstr(deviceTZBQMsg)
                                                            If workingTask.TaskName = "违章捕获" Then
                                                                If s.Value > -55 Then
                                                                    AddWarnMsg2WarnList(deviceTZBQMsg)
                                                                End If
                                                            Else
                                                                AddWarnMsg2WarnList(deviceTZBQMsg)
                                                            End If


                                                            Dim warnKind As String = GetWarnKind(deviceTZBQMsg)
                                                            Dim sql As String = "insert into WarnTable "
                                                            sql = sql & "(MsgTime,DeviceID,DeviceKind,Func,DeviceMsg,Lng,Lat,Address,WarnKind) values "
                                                            sql = sql & "('{0}','{1}','{2}','{3}','{4}','{5}','{6}','{7}','{8}')"
                                                            Dim para As New List(Of String)
                                                            para.Add(Now.ToString("yyyy-MM-dd HH:mm:ss"))
                                                            para.Add(myDeviceInfo.DeviceID)
                                                            para.Add("TZBQ")
                                                            para.Add("WARN")
                                                            para.Add(deviceTZBQMsg)
                                                            para.Add(myDeviceInfo.Lng)
                                                            para.Add(myDeviceInfo.Lat)
                                                            para.Add(myDeviceInfo.Address)
                                                            para.Add(warnKind)
                                                            sql = String.Format(sql, para.ToArray)
                                                            Dim result As String = SQLCmd(sql)
                                                        End If
                                                    End If
                                                    WarnInfoList.RemoveAt(j)
                                                    'log(" RemoveAt=" & j)
                                                Else
                                                    WarnInfoList(j) = s
                                                End If
                                                Exit For
                                            End If
                                        Next
                                        If isIn = False Then
                                            If IsInLegalSignal(pinlv) = False Then
                                                WarnInfoList.Add(New WarnInfo(pinlv, 0, minWarnNum))
                                            End If
                                        End If
                                    Next
                                End If
                            End If
                        Catch ex As Exception
                            log(ex.ToString)
                            '   MsgBox(ex.ToString)
                        End Try
                    End If
                    RecordDeviceMsg(myDeviceInfo.DeviceID, "TSS", jsonmsg.func, deviceMsg)
                End If
            Else
                If TZBQ_JCPDString <> "" Then
                    Dim cqList As New List(Of Double)
                    If yy.Count >= TZBQ_JCPD_IndexList.Count Then
                        For Each index In TZBQ_JCPD_IndexList
                            cqList.Add(yy(index))
                        Next
                        Try
                            Dim st(cqList.Count - 1) As String
                            For i = 0 To cqList.Count - 1
                                st(i) = cqList(i)
                            Next
                            Dim nString As String = String.Format(TZBQ_JCPDString, st)
                            Dim jsonmsg As JSON_Msg
                            jsonmsg.func = "bscan"
                            jsonmsg.msg = nString
                            Dim deviceMsg As String = JsonConvert.SerializeObject(jsonmsg)
                            AddDeviceMsg(deviceMsg)
                            'AddMainBroderCastDeviceMsg(jsMsg)
                        Catch ex As Exception

                        End Try
                    End If
                End If
            End If
        Catch ex As Exception
            log(ex.ToString)
        End Try
    End Sub
    Private Sub HandleLisanFreq(xx() As Double, yy() As Double)
        '  log("lsInfo.pointlist step=1")
        If IsNothing(xx) Or IsNothing(yy) Then Return
        If xx.Length <> yy.Length Then Return
        Dim maxhfdbm As Double = -70
        ' log("lsInfo.pointlist step=2")
        SyncLock lsInfoLock
            If isLiSanSaoMiao = False Then Return
            If IsNothing(lsInfo) Then Return
            If xx(0) <> lsInfo.freqStart Then Return
            'log("lsInfo.pointlist step=3")

            If xx(xx.Length - 1).ToString() = (lsInfo.freqEnd - 0.025).ToString() Or xx(xx.Length - 1).ToString() = lsInfo.freqEnd.ToString() Then

            Else
                '   log($"lsInfo.pointlist step=3.5,最后一个={xx(xx.Length - 1) },lsInfo.freqEnd={lsInfo.freqEnd}")
                Return
            End If
            If IsNothing(lsInfo.pointlist) Then
                Return
            Else
                'log($"lsInfo.pointlist step=3.6,lsInfo.pointlist is null")
            End If
            '  log("lsInfo.pointlist step=4")
            lsInfo.watchTime = lsInfo.watchTime + 1
            Dim runIndex As Integer = 0
            For Each itm In lsInfo.pointlist
                Dim freq As Double = itm.freq
                Dim cz As Double = Math.Abs(xx(runIndex) - freq)
                itm.dbm = yy(0)
                For i = runIndex + 1 To xx.Length - 1
                    Dim tcz As Double = Math.Abs(xx(i) - freq)
                    If tcz <= cz Then
                        cz = tcz
                        itm.dbm = yy(i)
                    Else
                        runIndex = i - 1
                        Exit For
                    End If
                    If i = xx.Length - 1 Then
                        runIndex = i
                    End If
                Next
                If itm.dbm_avg = 0 Then itm.dbm_avg = itm.dbm
                If itm.dbm_max = 0 Then itm.dbm_max = itm.dbm
                If itm.dbm_min = 0 Then itm.dbm_min = itm.dbm
                If itm.dbm_max < itm.dbm Then itm.dbm_max = itm.dbm
                If itm.dbm_min > itm.dbm Then itm.dbm_min = itm.dbm
                itm.sigNalInfo = "正常"
                itm.sigNalStatus = "正常"
                itm.dbm_avg = Math.Round((itm.dbm + itm.dbm_avg) * 0.5, 2)
                If itm.dbm > maxhfdbm Then
                    itm.overCount = itm.overCount + 1
                    itm.overPercent = 100 * itm.overPercent / lsInfo.watchTime
                End If
                itm.isFree = (itm.overPercent < 30)
            Next
            Dim jsonmsg As JSON_Msg
            jsonmsg.func = "lisan"
            jsonmsg.msg = JsonConvert.SerializeObject(lsInfo)
            Dim deviceMsg As String = JsonConvert.SerializeObject(jsonmsg)
            AddDeviceMsg(deviceMsg)
            ' log("lsInfo.pointlist step=5")
        End SyncLock



    End Sub
    Private Function IsInLegalSignal(ByVal v As Double) As Boolean
        If IsNothing(legalSigNal) Then Return False
        If legalSigNal.Count = 0 Then Return False
        For Each itm In legalSigNal
            If itm = v Then
                Return True
            End If
        Next
        Return False
    End Function
    Private Sub handleDeviceState(ByVal tm As tssMsg, ByVal ip As String, ByVal port As String)

        Dim list As New List(Of String)
        Try
            Dim myDevicestring As String = GetMyDevList("shadmin")
            If myDevicestring <> "" Then
                If myDevicestring <> "all" Then
                    list = myDevicestring.Split(",").ToList()
                End If
            End If
        Catch ex As Exception

        End Try

        Dim id As String = tm.deviceID
        If IsNothing(list) = False Then
            If list.Count > 0 Then
                If list.Contains(id) Then
                    log("deviceid=" & id & "属于上海系设备，上报经纬度不予受理")
                    Return
                End If
            End If
        End If
        If IsNothing(DeviceListLock) Then DeviceListLock = New Object
        SyncLock DeviceListLock
            If IsNothing(DeviceList) Then DeviceList = New List(Of DeviceStu)
            For i = 0 To DeviceList.Count - 1
                Dim sh As DeviceStu = DeviceList(i)
                If sh.DeviceID = myDeviceInfo.DeviceID Then
                    'sh.IP = ip
                    'sh.Port = port
                    'sh.Address = ip2Address(ip)
                    Dim msg As String = tm.canshuqu

                    If msg.Split(":")(0) = "<info" Then
                        msg = msg.Replace(">", "")
                        msg = msg.Split(":")(1)
                        Dim st() As String = msg.Split(";")
                        If sh.Lng = "" Then sh.Lng = st(0).Split("=")(1)
                        If sh.Lat = "" Then sh.Lat = st(1).Split("=")(1)
                        sh.Func = st(3).Split("=")(1)
                    End If
                    DeviceList(i) = sh
                    Try
                        Dim sqlTmp As String = "update deviceTable set Lng='{0}',Lat='{1}' where DeviceID='{2}'"
                        sqlTmp = String.Format(sqlTmp, New String() {sh.Lng, sh.Lat, sh.DeviceID})
                        SQLCmd(sqlTmp)
                    Catch ex As Exception

                    End Try
                    Exit For
                End If
            Next
            RaiseEvent RefrushDeviceList()
        End SyncLock

    End Sub
#Region "socket处理"
    Dim flag_isHead As Boolean = True
    Dim body_len As Long = 0
    Dim head_len As Integer = 103
    Dim realReadLen As Integer = 103
    Public Sub serverThreadProc()
        Try
            Dim sb As New SocketAndBuffer
            sb.Socket = myClientSocket
            If flag_isHead Then
                sb.Socket.BeginReceive(sb.Buffer, 0, head_len, SocketFlags.None, AddressOf ReceiveCallBack, sb)
            Else
                sb.Socket.BeginReceive(sb.Buffer, 0, body_len, SocketFlags.None, AddressOf ReceiveCallBack, sb)
            End If
        Catch ex As Exception

        End Try
    End Sub
    Private Sub ReceiveCallBack(ByVal ar As IAsyncResult)
        Dim sb As SocketAndBuffer
        sb = CType(ar.AsyncState, SocketAndBuffer)
        Dim stepp As Integer = 0
        Try
            If sb.Socket.Connected Then
                iLen = sb.Socket.EndReceive(ar)
                'log("iLen=" & iLen & ",flag_isHead=" & flag_isHead)
                If iLen > 0 Then
                    Dim iBuffer(iLen - 1) As Byte
                    Array.Copy(sb.Buffer, 0, iBuffer, 0, iLen)
                    If flag_isHead Then
                        If iLen = head_len Then
                            '说明头取完整了
                            If IsNothing(byteSub) Then
                                byteSub = iBuffer
                            Else
                                byteSub = byteSub.Concat(iBuffer).ToArray
                            End If
                            Dim lenofMsg As Integer = BitConverter.ToInt32(byteSub, 10)
                            If lenofMsg = byteSub.Length Then
                                ''功德圆满，获取到了一个整块的TM
                                flag_isHead = True
                                head_len = realReadLen
                                Dim nb() As Byte = byteSub
                                byteSub = Nothing
                                stepp = 1
                                sb.Socket.BeginReceive(sb.Buffer, 0, head_len, SocketFlags.None, AddressOf ReceiveCallBack, sb)
                                Try
                                    handleTm(nb)
                                Catch ex As Exception
                                    log("handleTm-->Err->" & ex.ToString)
                                End Try
                                Return
                            End If
                            If lenofMsg < byteSub.Length Then
                                'MsgBox("lenofMsg<byteSub.Length")
                                If (Not myClientSocket Is Nothing) Then
                                    CloseALL(False, "ReceiveCallBack lenofMsg<byteSub.Length")
                                End If
                                Return
                            End If
                            If lenofMsg > byteSub.Length Then
                                '数据包显示 本包 数量比较大，头是已经取完了，body还有数据待取
                                Dim num As Integer = lenofMsg - byteSub.Length
                                flag_isHead = False
                                body_len = num


                                Dim revSize As Integer = num
                                If revSize > maxSize Then
                                    revSize = maxSize
                                End If
                                'MsgBox("数据包显示 本包 数量比较大，头是已经取完了，body还有数据待取 num=" & num)
                                sb.Socket.BeginReceive(sb.Buffer, 0, revSize, SocketFlags.None, AddressOf ReceiveCallBack, sb)
                                Return
                            End If
                        End If
                        If iLen < head_len Then
                            '说明头还没有取完，接着取头
                            If IsNothing(byteSub) Then
                                byteSub = iBuffer
                            Else
                                byteSub = byteSub.Concat(iBuffer).ToArray
                            End If
                            flag_isHead = True
                            head_len = head_len - iLen
                            Dim str As String = ""
                            'MsgBox("'说明头还没有取完，接着取头 head_len=" & head_len & ",iLen=" & iLen)
                            'head_len变为剩下待取头量
                            stepp = 3
                            sb.Socket.BeginReceive(sb.Buffer, 0, head_len, SocketFlags.None, AddressOf ReceiveCallBack, sb)
                            Return
                        End If
                    Else
                        If iLen = body_len Then
                            '说明剩下的body部分取完整了
                            If IsNothing(byteSub) Then
                                byteSub = iBuffer
                            Else
                                byteSub = byteSub.Concat(iBuffer).ToArray
                            End If
                            ''功德圆满，获取到了一个整块的TM
                            Dim nb() As Byte = byteSub
                            byteSub = Nothing
                            flag_isHead = True
                            head_len = realReadLen
                            stepp = 4
                            sb.Socket.BeginReceive(sb.Buffer, 0, head_len, SocketFlags.None, AddressOf ReceiveCallBack, sb)
                            Try
                                handleTm(nb)
                            Catch ex As Exception
                                log("handleTm-->Err->" & ex.ToString)
                            End Try
                            Return
                        End If
                        If iLen < body_len Then
                            '说明剩下的body部分还没有取完整
                            If IsNothing(byteSub) Then
                                byteSub = iBuffer
                            Else
                                byteSub = byteSub.Concat(iBuffer).ToArray
                            End If
                            flag_isHead = False
                            'body_len变为剩下待取body量
                            body_len = body_len - iLen
                            stepp = 5
                            Dim revSize As Integer = body_len
                            If revSize > maxSize Then
                                revSize = maxSize
                            End If
                            sb.Socket.BeginReceive(sb.Buffer, 0, revSize, SocketFlags.None, AddressOf ReceiveCallBack, sb)
                            Return
                        End If
                    End If
                Else
                    If (Not myClientSocket Is Nothing) Then
                        CloseALL(False, "ReceiveCallBack iLen=0")
                    End If
                End If
            End If
        Catch ex As Exception
            '  MsgBox(ex.ToString)
            If (Not myClientSocket Is Nothing) Then
                'Dim str As String = "setpp=" & stepp & vbCrLf & ex.ToString
                'File.WriteAllText("d:\ReceiveCallBackError.txt", str)
                CloseALL(False, "ReceiveCallBack error " & ex.Message)

            End If
        End Try
    End Sub
    Dim byteSub() As Byte

    'Private Sub HandleBuffer(ByVal buffer() As Byte) '2018年08月06日改进  已弃用
    '    Try
    '        If IsNothing(byteSub) Then
    '            byteSub = buffer
    '        Else
    '            byteSub = byteSub.Concat(buffer).ToArray
    '        End If
    '        If byteSub.Length < 102 Then
    '            Return
    '        End If
    '        Dim readindex As Integer = 0
    '        Dim totalnum As Integer = byteSub.Length
    '        While True
    '            If readindex >= totalnum Then
    '                log("readindex>=totalnum,& readIndex=" & readindex & ",totalnum" & totalnum)
    '                Return
    '            End If
    '            If checkFlag(byteSub, readindex, totalnum - readindex) Then
    '                byteSub = tobytes(byteSub, readindex, totalnum - readindex)
    '                readindex = -1
    '                totalnum = byteSub.Length
    '                If byteSub.Length < 102 Then
    '                    Return
    '                Else
    '                    Dim tm As tssMsg = byte2tssmsgHead(byteSub)
    '                    Dim lenofmsg As Integer = tm.lenofmsg
    '                    If byteSub.Length < lenofmsg Then
    '                        Return
    '                    End If
    '                    If byteSub.Length = lenofmsg Then
    '                        Dim th As New Thread(AddressOf handleTm)
    '                        th.Start(byteSub)
    '                        byteSub = Nothing
    '                        Return
    '                    End If
    '                    If byteSub.Length > lenofmsg Then
    '                        log("byteSub.Length=" & byteSub.Length)
    '                        log("lenofmsg=" & lenofmsg)
    '                        log("byteSub.Length > lenofmsg")
    '                        Dim bk() As Byte = tobytes(byteSub, 0, lenofmsg)
    '                        Dim th As New Thread(AddressOf handleTm)
    '                        th.Start(bk)
    '                        byteSub = tobytes(byteSub, lenofmsg, byteSub.Length - lenofmsg)
    '                        readindex = -1
    '                        totalnum = byteSub.Count
    '                    End If
    '                End If
    '            Else
    '                log("checkFlag=false,readindex=" & readindex & ",totalnum=" & totalnum)
    '            End If
    '            readindex = readindex + 1
    '            log("readindex=" & readindex)
    '        End While
    '    Catch ex As Exception
    '        MsgBox("err3" & vbCrLf & ex.ToString)
    '    End Try
    'End Sub
    'Private Sub HandleBuffer(ByVal buffer() As Byte) '旧版稳定版，但是效率太低  暂且保留

    '    Try
    '        If IsNothing(byteSub) Then
    '            byteSub = buffer
    '        Else
    '            byteSub = byteSub.Concat(buffer).ToArray
    '        End If
    '        If byteSub.Length < 102 Then
    '            Return
    '        End If
    '        Dim readindex As Integer = 0
    '        Dim totalnum As Integer = byteSub.Length
    '        While True
    '            If readindex >= totalnum Then Exit While
    '            Try
    '                If checkFlag(byteSub, readindex, totalnum - readindex) = True Then
    '                    byteSub = tobytes(byteSub, readindex, totalnum - readindex)
    '                    readindex = -1
    '                    totalnum = byteSub.Length
    '                    If byteSub.Length < 102 Then
    '                        Exit Sub
    '                    Else
    '                        Try
    '                            Dim tm As tssMsg = byte2tssmsgHead(byteSub)
    '                            If IsNothing(tm) = False Then
    '                                Dim lenofmsg As Integer = tm.lenofmsg
    '                                If byteSub.Length < lenofmsg Then
    '                                    Exit Sub
    '                                End If
    '                                If byteSub.Length = lenofmsg Then
    '                                    handleTm(byteSub)
    '                                    byteSub = Nothing
    '                                    Exit Sub
    '                                End If
    '                                If byteSub.Length > lenofmsg Then
    '                                    Dim bk() As Byte = tobytes(byteSub, 0, lenofmsg)
    '                                    handleTm(bk)
    '                                    byteSub = tobytes(byteSub, lenofmsg, byteSub.Length - lenofmsg)
    '                                    readindex = -1
    '                                    totalnum = byteSub.Count
    '                                End If
    '                            Else
    '                                byteSub = tobytes(byteSub, 102, byteSub.Length - 102)
    '                                readindex = -1
    '                                totalnum = byteSub.Count
    '                            End If
    '                        Catch ex As Exception
    '                            ''MsgBox("err1" & vbCrLf & ex.ToString)
    '                            ' RaiseEvent Exception(ex)
    '                        End Try
    '                    End If
    '                End If
    '            Catch ex As Exception
    '                '  'MsgBox("err2" & vbCrLf & ex.ToString)
    '            End Try
    '            readindex = readindex + 1
    '            If readindex >= totalnum Then Exit While
    '        End While
    '    Catch ex As Exception
    '        ' 'MsgBox("err3" & vbCrLf & ex.ToString)
    '    End Try
    'End Sub

    Private Class SocketAndBuffer
        Public Socket As System.Net.Sockets.Socket
        Public Buffer(819200) As Byte
    End Class
#End Region
    Private Sub StartHttp(ByVal cls As DeviceTSS)
        Try
            log("TSS_" & myDeviceInfo.DeviceID & ",尝试开启HTTP服务……")
            myHttpListener = New HttpListener
            myHttpListener.Prefixes.Add(myHttpUrl)
            myHttpListener.Start()
            log("TSS_" & myDeviceInfo.DeviceID & ",已开启HTTP服务")
            log("TSS_" & myDeviceInfo.DeviceID & ",尝试开启线程……")
            broadcastThread = New Thread(AddressOf sub_broadcastDeviceMsg)
            broadcastThread.Start()
            log("TSS_" & myDeviceInfo.DeviceID & ",已开启线程")
            If IsNothing(DeviceListLock) Then DeviceListLock = New Object
            SyncLock DeviceListLock
                DeviceList.Add(myDeviceInfo)
            End SyncLock
            log("DeviceList已记录DeviceID=" & myDeviceInfo.DeviceID & ",位置=" & myDeviceInfo.Address)
            myHttpListener.BeginGetContext(New AsyncCallback(AddressOf GetContextCallBack), myHttpListener)
        Catch ex As Exception
            log("TSS_" & myDeviceInfo.DeviceID & ",初始化HTTP等服务失败，ERR-->" & ex.Message)
        End Try
        ' StartWebSocket()
        If IsNothing(cls) = False Then
            cls.CloseSocket()
        End If
    End Sub
    Private Sub SendAudioToAllUser(msg As String)
        If IsNothing(webSocketListener) = False Then
            Try
                webSocketListener.SendMsgToAllAsync(msg)
            Catch ex As Exception

            End Try
        End If
    End Sub
    Private Sub StartWebSocket()
        Try
            log("TSS_" & myDeviceInfo.DeviceID & ",尝试开启webSocket服务……")
            webSocketListener = New WebSocketServer(webSocketUrl)
            webSocketListener.start()
            log("TSS_" & myDeviceInfo.DeviceID & ",已开启webSocket服务")
        Catch ex As Exception
            log("TSS_" & myDeviceInfo.DeviceID & ",初始化webSocket服务失败，ERR-->" & ex.Message)
        End Try
    End Sub
    Private Sub AddDeviceMsg(ByVal deviceMsg As String)
        Try
            RaiseEvent OnHandleJsonMsg(deviceMsg)
            If IsNothing(HttpMsgList_Object) Then HttpMsgList_Object = New Object
            SyncLock HttpMsgList_Object
                If IsNothing(HttpMsgList) Then HttpMsgList = New List(Of String)
                HttpMsgList.Add(deviceMsg)
            End SyncLock
            If flag_MzhHandle Then
                Dim mbcm As New MainBroderCastMsgStu(myDeviceInfo.DeviceID, deviceMsg, "TSS")
                Dim jsMsg As String = JsonConvert.SerializeObject(mbcm)
                If IsNothing(MZHHandle) = False Then
                    MZHHandle.HandleMyDeviceMsg(jsMsg)
                End If
            End If
        Catch ex As Exception

        End Try
    End Sub
    Private Sub GetContextCallBack(ByVal ar As IAsyncResult)
        Try
            myHttpListener = ar.AsyncState
            Dim context As HttpListenerContext = myHttpListener.EndGetContext(ar)
            myHttpListener.BeginGetContext(New AsyncCallback(AddressOf GetContextCallBack), myHttpListener)
            If IsNothing(context) = False Then
                handleHttpRequest(context)
            End If
        Catch ex As Exception

        End Try
    End Sub
    Public Function SendOrderFreqToDevice(freqStart As Double, freqEnd As Double, freqStep As Double, gcValue As Double, DHDevice As String, Optional fromInfo As String = "") As String
        isNeedAutoStopDevice = True
        If fromInfo = "normalOrder" Then
            '命令来自前台下发任务
            autoStopDeviceTime = Now.AddMinutes(10)
            'autoStopDeviceTime = Now.AddSeconds(10)
        End If
        If fromInfo = "daily" Then
            '命令来自每日一谱
            autoStopDeviceTime = Now.AddMinutes(1)
        End If
        If fromInfo = "bus" Then
            '命令来自公交系统
            autoStopDeviceTime = Now.AddMinutes(10)
        End If

        SendStop2Device()
        Sleep(1000)
        Dim ifbw As String = 40000
        Dim abw As String = freqStep
        Dim bits As String = 16
        Dim gcmode As String = "fagc"
        Dim returndata As String = "fft"
        Dim returninter As String = 1
        Dim detetor As String = "real"
        If gcValue <> 8 And gcValue <> 16 Then
            gcValue = 8
        End If
        Dim msg As String = "<bscan: tasknum=1" &
             ";freqbegin=" & freqStart &
             ";freqend=" & freqEnd &
              ";freqstep=" & freqStep &
              ";ifbw=" & ifbw &
               ";abw=" & abw &
               ";rbw=" & abw &
               ";bits=" & bits &
               ";gcmode=" & gcmode &
               ";gcvalue=" & gcValue &
              ";returndata=" & returndata &
              ";detetor=" & detetor &
               ";returninter=" & returninter
        If DSGDeviceKind = "DH" Then
            If DHDevice = "2to1" Then
                DHDeviceControlStr = DHDevice
            End If
            msg = msg & ";device=" & DHDeviceControlStr & ";>"
        Else
            msg = msg & ">"
        End If
        ' File.WriteAllText("d:\rrrrrrr.txt", msg)
        Dim result As String = SendMsgToTSSDevByString(&H0, "task", "bscan", msg, Nothing)
        'If myDeviceInfo.Name = "长安上沙站" Then
        '    log(msg)
        '    log(result)
        'End If
        If result.StartsWith("result=success") Then
            myFreqInfo = New json_PPSJ
            myFreqInfo.freqStart = freqStart
            myFreqInfo.freqStep = freqStep
            myFreqInfo.freqEnd = freqEnd
            myFreqInfo.dataCount = ((freqEnd - freqStart) / (freqStep * 0.001)) + 1
            flagHaveMyFreqInfo = True
        End If

        Return result
    End Function
    Private Sub handleHttpRequest(ByVal context As HttpListenerContext)
        Try
            Dim GETSTR As String = UrlDecode(context.Request.Url.PathAndQuery)
            Dim func As String = GetParaValue(GETSTR, "func")
            Dim dataMsg As String = GetParaValue(GETSTR, "datamsg")
            'Httplog(" --------------------------------")
            'Httplog("服务:TSS/" & myDeviceInfo.DeviceID & "/")
            'Httplog("请求:" & GETSTR)
            'Httplog("func:" & func)
            'Httplog("dataMsg:" & dataMsg)
            If func = "GetDevMsg" Then     '获取设备消息
                If IsNothing(HTTPClient) Then HTTPClient = New List(Of HttpListenerContext)
                If IsNothing(HTTPClient_Object) Then HTTPClient_Object = New Object
                SyncLock HTTPClient_Object
                    HTTPClient.Add(context)
                End SyncLock
                Exit Sub
            End If
            Dim token As String = GetParaValue(GETSTR, "token")
            If islogined(token) = False Then
                response(context, "result=fail;msg=Please login")
                Exit Sub
            End If
            If func = "SetDeviceNickID" Then
                Dim nickid As String = GetParaValue(GETSTR, "nickid")
                If nickid = "" Then
                    response(context, "result=fail;msg=NickID不允许为空;errMsg=" & "null" & ";advise=")
                    Exit Sub
                End If
                If nickid = myDeviceInfo.DeviceID Then
                    response(context, "result=fail;msg=前后名称相同;errMsg=" & "null" & ";advise=")
                    Exit Sub
                End If
                Try
                    Dim sql As String = String.Format("select * from deviceTable where DeviceNickName='{0}'", New String() {nickid})
                    Dim dt As DataTable = SQLGetDT(sql)
                    If IsNothing(dt) = False Then
                        If dt.Rows.Count > 0 Then
                            response(context, "result=fail;msg=该名称已存在;errMsg=" & "null" & ";advise=")
                            Exit Sub
                        End If
                    End If
                Catch ex As Exception

                End Try
                Try
                    Dim sql As String = String.Format("update deviceTable set DeviceNickName='{0}' where DeviceID='{1}'", New String() {nickid, myRealID})
                    Dim result As String = SQLCmd(sql)
                    AddMyLog("设置设备备注为" & nickid, "成功")
                    response(context, "result=success;msg=设置备注成功")
                    Sleep(500)
                    CloseALLByChaoShi()
                    Exit Sub
                Catch ex As Exception
                    If nickid = myDeviceInfo.DeviceID Then
                        response(context, "result=fail;msg=;errMsg=" & ex.Message & ";advise=")
                        Exit Sub
                    End If
                End Try
            End If
            If func = "tssOrder" Then

                If isWorking Then
                    AddMyLog("接收一般命令", "失败,设备正在执行任务")
                    response(context, "result=fail;msg=设备正在执行任务;errMsg=" & "null" & ";advise=")
                    Exit Sub
                End If
                If myDeviceInfo.RunKind = "bus" Then
                    AddMyLog("接收一般命令", "失败,该设备是公交监测系统，日常监测任务不可改变")
                    response(context, "result=fail;msg=该设备是公交监测系统，日常监测任务不可改变;errMsg=" & "null" & ";advise=")
                    Exit Sub
                End If

                Dim msg As String = dataMsg
                Try
                    Dim tss As tssOrder_stu = JsonConvert.DeserializeObject(msg, GetType(tssOrder_stu))
                    If tss.task = "bscan" Then
                        isLiSanSaoMiao = False
                        AddMyLog("接收一般命令,频谱扫描", "成功")
                        Dim freqbegin As Double = tss.freqStart
                        Dim freqend As Double = tss.freqEnd
                        Dim freqstep As Double = tss.freqStep
                        Dim orderResult As String = SendOrderFreqToDevice(freqbegin, freqend, freqstep, tss.gcValue, tss.DHDevice, "normalOrder")
                        response(context, orderResult)
                    End If
                    If tss.task = "stop" Then
                        isLiSanSaoMiao = False
                        AddMyLog("接收一般命令,停止工作", "成功")
                        response(context, SendMsgToTSSDevByString(&H0, "task", "taskctrl", "<taskctrl:taskstate=stop;>", Nothing))
                    End If
                    If tss.task = "ifscan_wav" Then
                        isLiSanSaoMiao = False
                        isNeedAutoStopDevice = True
                        autoStopDeviceTime = Now.AddMinutes(10)
                        SendStop2Device()
                        Sleep(1000)
                        AddMyLog("接收一般命令,音频解调", "成功")
                        Dim Text As String = tss.freqStart
                        msg = "<ifscan_wav:tasknum=1;freq=" & Text & ";mod_fre=" & Text & ";freqstep=0.15625;ifbw=200;demodmode=FM;demodbw=200;rfwmode=nm;gcmode=fagc;gcvalue=0;returndata=audio;encodetype=pcm;bits=8;returninter=1;detector=real;rfwmode=nm;>"
                        'sendMsgToDev(&H0, "task", "ifscan_wav", msg, Nothing)
                        response(context, SendMsgToTSSDevByString(&H0, "task", "ifscan_wav", msg, Nothing))
                    End If
                    If tss.task = "lisan" Then
                        Try
                            log($"TSS设备，{myDeviceInfo.DeviceID}，收到离散扫描命令")
                            Dim list As List(Of Double) = JsonConvert.DeserializeObject(Of List(Of Double))(tss.data)
                            If IsNothing(list) Then
                                response(context, JsonConvert.SerializeObject(New NormalResponse(False, "离散扫描数据格式有误")))
                                Return
                            End If
                            list.Sort()
                            Dim freqstart As Double = list(0)
                            Dim freqEnd As Double = list(list.Count - 1)
                            If freqstart < 30 Then
                                response(context, JsonConvert.SerializeObject(New NormalResponse(False, "频点值不能小于30")))
                                Return
                            End If
                            If freqEnd > 6000 Then
                                response(context, JsonConvert.SerializeObject(New NormalResponse(False, "频点值不能大于6000")))
                                Return
                            End If
                            Dim orderResult As String = SendOrderFreqToDevice(freqstart, freqEnd, 25, 8, "2to1", "normalOrder")
                            If orderResult.StartsWith("result=success") Then
                                SyncLock lsInfoLock
                                    isLiSanSaoMiao = True
                                    lsInfo = New LisanInfo
                                    lsInfo.freqStart = freqstart
                                    lsInfo.freqEnd = freqEnd
                                    lsInfo.startTime = Now
                                    lsInfo.watchTime = 0
                                    lsInfo.pointlist = New List(Of LisanPointInfo)
                                    For Each d In list
                                        Dim p As New LisanPointInfo
                                        p.freq = d
                                        p.isFree = True
                                        lsInfo.pointlist.Add(p)
                                    Next
                                End SyncLock
                                log("TSS设备，" & myDeviceInfo.DeviceID & " 开始离散扫描," & JsonConvert.SerializeObject(lsInfo))
                                AddMyLog("接收一般命令,离散扫描", "成功")
                                response(context, JsonConvert.SerializeObject(New NormalResponse(True, "success")))
                            Else
                                response(context, JsonConvert.SerializeObject(New NormalResponse(False, orderResult)))
                            End If

                        Catch ex As Exception

                        End Try
                    End If
                Catch ex As Exception
                    response(context, "result=fail;msg=null;errMsg=" & ex.Message & ";advise=null")
                End Try
                Exit Sub
            End If
            If func = "tssOrderByCode" Then
                Dim funcType As String = GetParaValue(GETSTR, "funcType")
                Dim dataType As String = GetParaValue(GETSTR, "dataType")
                Dim paraMsg As String = GetParaValue(GETSTR, "paraMsg")
                Dim result As String = SendMsgToTSSDevByString(&H0, dataType, funcType, paraMsg, Nothing)
                'GETSTR
                AddMyLog("接收一般命令,dataType=" & dataType & ",funcType=" & funcType, "成功")
                response(context, result)
                Exit Sub
            End If
            If func = "netswitchin" Then
                AddMyLog("接收一般命令,切换到内网", "成功")
                response(context, SendMsgToTSSDevByString(&H0, "task", "netswitch", "<netswitch:sw=in;>", Nothing))
                Exit Sub
            End If
            If func = "netswitchout" Then
                AddMyLog("接收一般命令,切换到外网", "成功")
                response(context, SendMsgToTSSDevByString(&H0, "task", "netswitch", "<netswitch:sw=out;>", Nothing))
                Exit Sub
            End If
            If func = "ReStartWindows" Then
                AddMyLog("接收一般命令,重启工控机", "成功")
                response(context, SendMsgToTSSDevByString(&H0, "task", "ReStartWindows", "", Nothing))
                Exit Sub
            End If
            If func = "ShutdownWindows" Then
                AddMyLog("接收一般命令,关闭工控机", "成功")
                response(context, SendMsgToTSSDevByString(&H0, "task", "ShutdownWindows", "", Nothing))
                Exit Sub
            End If
            If func = "GetControlerVersion" Then
                response(context, myControlerVersion)
                Return
            End If

            If func = "SetDeviceLngAndLat" Then
                Dim lng As String = GetParaValue(GETSTR, "lng")
                Dim lat As String = GetParaValue(GETSTR, "lat")
                Dim deviceid As String = myDeviceInfo.DeviceID
                If lng = "" Or lat = "" Then
                    response(context, "result=fail;msg=经纬度不允许为空"）
                    Return
                End If
                Dim sql As String = "update deviceTable set Lng='{0}' , Lat='{1}' where DeviceID='{2}'"
                sql = String.Format(sql, New String() {lng, lat, deviceid})

                Dim result As String = SQLCmd(sql)
                If result = "success" Then
                    response(context, "result=success;msg="）
                    Sleep(500)
                    CloseALLByChaoShi()
                    Return
                Else
                    response(context, "result=fail;msg=" & result）
                    Return
                End If
            End If
            If func = "GetScreenImage" Then
                AddMyLog("接收一般命令,捕捉控制系统桌面", "成功")
                response(context, SendMsgToTSSDevByString(&H0, "task", "GetScreenImage", "", Nothing))
                Exit Sub
            End If
            If func = "mouseClick" Then
                response(context, SendMsgToTSSDevByString(&H0, "task", "mouseClick", dataMsg, Nothing))
                Exit Sub
            End If
        Catch ex As Exception

        End Try
        response(context, "")
    End Sub
    Public Sub SendStop2Device(Optional fromInfo As String = "")
        Dim logstr As String = "发送停止命令"
        If fromInfo <> "" Then
            logstr = $"[{fromInfo}]发送停止命令"
            AddMyLog(logstr, "成功")
        End If

        SendMsgToTSSDevByString(&H0, "task", "taskctrl", "<taskctrl:taskstate=stop;>", Nothing)
    End Sub
    Public Function GetisWorking() As Boolean
        Return isWorking
    End Function
    Public Function SendRMTPFreqOrder2Device(freqbegin As Double, freqEnd As Double, freqStep As Double, ifbw As Double) As String
        If isWorking Then
            Return "设备正忙"
        End If
        SendStop2Device()
        Sleep(1000)
        AddMyLog("接收RMTP命令,频谱扫描", "成功")
        myFreqInfo = New json_PPSJ
        myFreqInfo.freqStart = freqbegin
        myFreqInfo.freqStep = freqStep
        myFreqInfo.freqEnd = freqEnd
        myFreqInfo.dataCount = ((freqEnd - freqbegin) / (freqStep * 0.001)) + 1
        flagHaveMyFreqInfo = True
        ifbw = 40000
        Dim abw As String = freqStep
        Dim bits As String = 16
        Dim gcmode As String = "fagc"
        Dim gcvalue As Double = 8
        Dim returndata As String = "fft"
        Dim returninter As String = 1
        Dim detetor As String = "real"
        If gcvalue <> 8 And gcvalue <> 16 Then
            gcvalue = 8
        End If
        Dim msg As String = "<bscan: tasknum=1" &
             ";freqbegin=" & freqbegin &
             ";freqend=" & freqEnd &
              ";freqstep=" & freqStep &
              ";ifbw=" & ifbw &
               ";abw=" & abw &
               ";rbw=" & abw &
               ";bits=" & bits &
               ";gcmode=" & gcmode &
               ";gcvalue=" & gcvalue &
              ";returndata=" & returndata &
              ";detetor=" & detetor &
               ";returninter=" & returninter
        If DSGDeviceKind = "DH" Then
            msg = msg & ";device=" & "one" & ";>"
        Else
            msg = msg & ">"
        End If
        SendMsgToTSSDevByString(&H0, "task", "bscan", msg, Nothing)
        Return "ok"
    End Function
    Dim heartTm As tssMsg
    Public Function SendMsgToTSSDevByString(ByVal a As String, ByVal b As String, ByVal c As String, ByVal d As String, ByVal e As Byte()) As String
        '&H0, "DevMsg", "Test", "<info:func=;>", Nothing
        Try
            If a = "" Then a = &H0
            If b = "" Then Return "result=fail;msg=tssMsg Parse file;errMsg=null;advise=check out your parameter"
            If c = "" Then Return "result=fail;msg=tssMsg Parse file;errMsg=null;advise=check out your parameter"
            Dim tm As tssMsg = msg2TssMsg(a, b, c, d, e)
            If isWorking = False Then
                If b.ToLower() = "task" Then
                    heartTm = tm
                End If
            End If
            Dim by() As Byte = tssmsg2byte(tm)
            If IsNothing(by) Then
                log("TSSSer.SendData>ERR>Byte[]为空")
                Return "result=fail;msg=tssMsg Parse file;errMsg=null;advise=check out your parameter"
            End If
            'log("<发送TSS_Dev> id=" & myDeviceInfo.DeviceID & "," & b & "," & c & "," & d)
            myClientSocket.Send(by)
            ' log("Success")
            Return "result=success;msg=success;errMsg=null;advise=good"
        Catch ex As Exception
            Return "result=fail;msg=null;errMsg=" & ex.Message & ";advise=null"
        End Try
    End Function
    Dim HeartWatcher As Integer = 0
    Dim device701FreqReSendCount As Integer = 2
    Private Sub sub_broadcastDeviceMsg()
        Dim num As Integer = 0
        Dim SleepHeartWatch As Integer = 0
        While True
            Try
                If IsNothing(HttpMsgList_Object) = False Then
                    If IsNothing(HttpMsgList) = False Then
                        If HttpMsgList.Count > 0 Then
                            If IsNothing(HTTPClient) = False Then
                                If IsNothing(HTTPClient_Object) = False Then
                                    If HTTPClient.Count > 0 Then
                                        Dim msg As String = ""
                                        Dim MsgCount As Integer = 0
                                        Dim ClientCount As Integer = 0
                                        SyncLock HttpMsgList_Object
                                            msg = JsonConvert.SerializeObject(HttpMsgList)
                                            MsgCount = HttpMsgList.Count
                                            HttpMsgList.Clear()
                                        End SyncLock
                                        SyncLock HTTPClient_Object
                                            For Each client In HTTPClient
                                                response(client, msg)
                                            Next
                                            ClientCount = HTTPClient.Count
                                            HTTPClient.Clear()
                                        End SyncLock
                                        'log("MsgCount=" & MsgCount & ",ClientCount=" & ClientCount)
                                    Else
                                        SyncLock HttpMsgList_Object
                                            HttpMsgList.Clear()
                                        End SyncLock
                                    End If
                                Else
                                    SyncLock HttpMsgList_Object
                                        HttpMsgList.Clear()
                                    End SyncLock
                                End If
                            Else
                                SyncLock HttpMsgList_Object
                                    HttpMsgList.Clear()
                                End SyncLock
                            End If
                        End If
                    End If
                End If
            Catch ex As Exception
                log(ex.ToString)
            End Try

            Try
                num = num + 1
                If num = 20 Then
                    num = 0
                    If is701Device Then
                        Try
                            'log("701设备发心跳握手包,DeviceID=" & myDeviceInfo.DeviceID)
                            Dim isHanelded As Boolean = False
                            If isWorking = False Then
                                If is701Device Then
                                    If heartTm.datatype.ToLower = "task" Then
                                        SendMsgToTSSDevByString(heartTm.ctrl, heartTm.datatype, heartTm.functype, heartTm.canshuqu, heartTm.shujuqu)
                                        isHanelded = True
                                    End If
                                End If
                            End If
                            ' SendMsgToTSSDevByString(&H0, "DevMsg", "WSZL", "", Nothing)
                            If isHanelded = False Then
                                Dim by(9) As Byte
                                For i = 0 To by.Length - 1
                                    by(i) = 1
                                Next
                                myClientSocket.Send(by)
                            End If

                        Catch ex As Exception

                        End Try
                    Else
                        SendMsgToTSSDevByString(&H0, "DevMsg", "Test", "", Nothing)

                    End If
                End If
            Catch ex As Exception

            End Try
            If is701Device Then
                Try
                    device701FreqReSendCount = device701FreqReSendCount + 1
                    If device701FreqReSendCount > 1 Then
                        device701FreqReSendCount = 0
                        If isWorking = False Then
                            If heartTm.datatype.ToLower = "task" Then
                                SendMsgToTSSDevByString(heartTm.ctrl, heartTm.datatype, heartTm.functype, heartTm.canshuqu, heartTm.shujuqu)
                            End If
                        End If
                    End If
                Catch ex As Exception

                End Try
            End If

            Try
                SleepHeartWatch = SleepHeartWatch + 1
                If SleepHeartWatch = 45 Then
                    SleepHeartWatch = 0
                    If HeartWatcher = 0 Then
                        Try
                            log("TSS_" & myDeviceInfo.DeviceID & "通信超时，将主动断开Socket……")
                            CloseALLByChaoShi()
                        Catch ex As Exception

                        End Try
                    Else
                        HeartWatcher = 0
                    End If
                End If
            Catch ex As Exception

            End Try
            Try
                Task.Run(Sub()
                             If isWorking = False Then
                                 If isNeedAutoStopDevice Then
                                     If Now >= autoStopDeviceTime Then
                                         SendStop2Device("系统")
                                         autoStopDeviceTime = Now.AddMinutes(1)
                                         isNeedAutoStopDevice = False
                                     End If
                                 End If
                             End If
                         End Sub)
            Catch ex As Exception

            End Try
            Sleep(broadcastSleep)
        End While
    End Sub
    Private Sub CloseALLByChaoShi()
        AddMyLog("断线(超时)", "")
        Dim oldstr As String = "超时"
        Try
            myHttpListener.Abort()
            log("关闭" & oldstr & "myHttpListener成功")
        Catch ex As Exception
            log("关闭" & oldstr & "myHttpListener失败" & ex.ToString)
        End Try
        Try
            If IsNothing(DeviceListLock) Then DeviceListLock = New Object
            If IsNothing(DeviceList) Then DeviceList = New List(Of DeviceStu)
            SyncLock DeviceListLock
                For i = DeviceList.Count - 1 To 0 Step -1
                    Dim itm As DeviceStu = DeviceList(i)
                    If itm.IP = IP And itm.Port = Port Then
                        DeviceList.RemoveAt(i)
                        Dim str As String = "DeviceList已移除" & oldstr & "DeviceID=" & itm.DeviceID & ",位置=" & itm.Address & "(" & IP & ":" & Port & ")"
                        log(str)
                        Exit For
                    End If
                Next
            End SyncLock
            log("刷新DeviceList……")
            RaiseEvent RefrushDeviceList()
        Catch ex As Exception

        End Try
        Try

            myClientSocket.Close()
        Catch ex As Exception
            log("断开超时TSS_socket,DeviceID=" & myDeviceInfo.DeviceID & ">ERR>" & ex.Message)
        End Try
        Try
            broadcastThread.Abort()
            log("关闭" & oldstr & "线程成功")
        Catch ex As Exception
            log("关闭" & oldstr & "线程失败"）
        End Try
        Try
            If IsNothing(TaskWorkerThread) = False Then
                TaskWorkerThread.Abort()
                log("关闭" & oldstr & "任务线程成功")
            End If

        Catch ex As Exception
            log("关闭" & oldstr & "任务线程失败" & ex.ToString)
        End Try
    End Sub
End Class
