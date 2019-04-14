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

Public Class TSS_GateWay
    Private workingTask As New NormalTaskStu
    Private legalSigNal As List(Of Double)
    Private isWarnWZ As Boolean = False
    Private flagIsTekGateWay As Boolean = False
    Private minWarnNum As Integer = 5
    Private WarnFucha As Double = 5
    Private warnDaikuan As Double = 5
    Private isTZBQ_JCPD As Boolean = False
    Private TZBQ_JCPD_IndexList As List(Of Integer)
    Private TZBQ_JCPDString As String = ""
    Private myDeviceInfo As New DeviceStu
    Private myRealID As String
    Private myHttpListener As HttpListener
    Private myHttpUrl As String
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
    Private TaskWorkerThread As Thread
    Private loopGetHeartBeatThread As Thread
    Public Event RefrushDeviceList()
    Public Event RaiseLog(ByVal mainMsg As String, ByVal UserName As String, ByVal IP As String)
    Dim isHandledLogin As Boolean = False
    Dim DirPath As String = Directory.GetCurrentDirectory() & "\DeviceTaskIni"
    Public Event RaiseHttplog(ByVal str As String)

    Private Sub Httplog(ByVal str As String)
        RaiseEvent RaiseHttplog(str)
    End Sub

    Public Sub New(ByVal ClientSocket As Socket)
        myClientSocket = ClientSocket

        IP = myClientSocket.RemoteEndPoint.ToString.Split(":")(0)
        Port = myClientSocket.RemoteEndPoint.ToString.Split(":")(1)

    End Sub
    Public Sub Start()
        serverThreadProc()
        log("TSS_GateWay连接，服务器主动发送握手命令……")
        'log(
        ' SendMsgToTSSDevByString(&H0, "task", "netswitch", "<netswitch:sw=in;>", Nothing)
        Sleep(3000)
        SendMsgToTSSDevByString(&H0, "DevMsg", "WSZL", "", Nothing)
        Sleep(3000)
        If Not isHandledLogin Then
            Dim th As New Thread(Sub()
                                     Sleep(7000)
                                     If Not isHandledLogin Then
                                         CloseSocket()
                                     End If
                                 End Sub)
            th.Start()
        End If
    End Sub
    Private Sub log(ByVal MainMsg As String)
        'Form1.log(MainMsg, "TSS_GateWay_Server", "+:" & _Port)
        RaiseEvent RaiseLog(MainMsg, "TSS_GateWay_" & myDeviceInfo.DeviceID, IP & ":" & Port)
    End Sub

    Private Sub HandleLogin(ByVal id As String)
        If isHandledLogin Then Return
        isHandledLogin = True
        'If id = "57G-SM0001" Then
        '    id = "0320160018"
        'End If
        myRealID = id
        Dim NickName As String = id
        Dim DeviceName As String = id
        Dim dbLng As String = ""
        Dim dbLat As String = ""
        Dim myAddress As String = ip2Address(IP)
        Try
            Dim sql As String = String.Format("select * from deviceTable where DeviceID='{0}'", id)
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) = False Then
                If dt.Rows.Count > 0 Then
                    Dim row As DataRow = dt.Rows(0)
                    DeviceName = row("DeviceNickName")
                    NickName = id
                    dbLng = row("Lng")
                    dbLat = row("Lat")
                    Dim sqlTmp As String = "update deviceTable set OnlineTime='{0}',Address='{1}',IP='{2}',Port='{3}' where DeviceID='{4}'"
                    sqlTmp = String.Format(sqlTmp, New String() {Now.ToString("yyyy-MM-dd HH:mm:ss"), myAddress, IP, Port, id})
                    SQLCmd(sqlTmp)
                Else
                    Dim insertSql As String = "insert into deviceTable (deviceid,deviceNickName,kind,OnlineTime,IP,port) values('{0}','{1}','{2}','{3}','{4}','{5}')"
                    insertSql = String.Format(insertSql, New String() {id, id, "TSSGateWay", Now.ToString("yyyy-MM-dd HH:mm:ss"), IP, Port, ""})
                    SQLCmd(insertSql)
                End If
            Else
                Dim insertSql As String = "insert into deviceTable (deviceid,deviceNickName,kind,OnlineTime,IP,port) values('{0}','{1}','{2}','{3}','{4}','{5}')"
                insertSql = String.Format(insertSql, New String() {id, id, "TSSGateWay", Now.ToString("yyyy-MM-dd HH:mm:ss"), IP, Port, ""})
                SQLCmd(insertSql)
            End If
        Catch ex As Exception

        End Try
        myDeviceInfo = New deviceStu
        myDeviceInfo.DeviceID = NickName
        myDeviceInfo.Address = myAddress
        myHttpUrl = TSSHttpListenerPath & id & "/"
        myDeviceInfo.HTTPMsgUrl = myHttpUrl
        myDeviceInfo.Kind = "TSSGateWay"
        myDeviceInfo.isNetGateWay = True
        myDeviceInfo.OnlineTime = Now.ToString("yyyy-MM-dd HH:mm:ss")
        myDeviceInfo.Statu = "normal"
        myDeviceInfo.cls = Me
        myDeviceInfo.IP = IP
        myDeviceInfo.Port = Port
        myDeviceInfo.Name = id
        myDeviceInfo.Name = DeviceName
        myDeviceInfo.Func = "null"
        myDeviceInfo.Lng = dbLng
        myDeviceInfo.Lat = dbLat
        myDeviceInfo.NetSwitch = 0
        If IsNothing(DeviceListLock) Then DeviceListLock = New Object
        If IsNothing(DeviceList) Then DeviceList = New List(Of deviceStu)
        Dim isFind As Boolean = False
        Dim isJXX As Boolean = False
        Dim JXXItm As deviceStu
        SyncLock DeviceListLock
            For i = 0 To DeviceList.Count - 1
                Dim itm As deviceStu = DeviceList(i)
                If itm.DeviceID = NickName Then
                    log("TSS_GateWay DeviceID=" & NickName & "已登录,正在处理同ID登录，New IP=" & IP & ":" & Port & ",oldIP=" & getIpAndPortByDeviceID(id))
                    Try
                        If itm.IP = IP And itm.Port = Port Then
                            isFind = True
                            log("TSS_GateWay old DeviceID=" & itm.DeviceID & ",重复登录，无需处理" & ",old位置=" & itm.Address & "oldip=(" & itm.IP & ":" & itm.Port & ")")
                            Exit For
                        Else
                            log("处理同ID=" & NickName & ",挤下线")
                            isJXX = True
                            JXXItm = itm
                            Exit For
                        End If
                    Catch ex As Exception
                        log("处理同ID=" & NickName & ",err=" & ex.Message)
                    End Try
                    Exit For
                End If
            Next
        End SyncLock
        flagIsTekGateWay = True
        If isJXX Then '如果是挤下线 
            Dim cls As TSS_GateWay = CType(JXXItm.cls, TSS_GateWay) '获取到前者的对象(类)
            cls.CloseALL(True) '前者关闭，并通知朕可以登陆
            AddMyLog("重新上线", "成功") '增加设备log
            log("重新上线TSS_GateWay登录，deviceID=" & id) '增加设备log
            StartHttp(cls) '朕开启自身的HTTP服务


            If flagIsTekGateWay Then
                Try
                    log("TssGateWayId=" & id & "是Tek配套网关，开启循环或取网关经纬度线程")
                    loopGetHeartBeatThread = New Thread(AddressOf LoopGetHeartBeat)
                    loopGetHeartBeatThread.Start()
                Catch ex As Exception

                End Try
            End If

            RaiseEvent RefrushDeviceList() '通知主线程更新总设备列表
            isFind = True
        End If
        If isFind = False Then
            AddMyLog("上线", "成功")
            log("TSS_GateWay登录，deviceID=" & NickName)
            StartHttp(Nothing)
            If flagIsTekGateWay Then
                Try
                    log("TssGateWayId=" & id & "是Tek配套网关，开启循环或取网关经纬度线程")
                    loopGetHeartBeatThread = New Thread(AddressOf LoopGetHeartBeat)
                    loopGetHeartBeatThread.Start()
                Catch ex As Exception

                End Try
            End If
            RaiseEvent RefrushDeviceList()
            CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & id)
            CheckDir(Directory.GetCurrentDirectory() & "\Task\logs\" & id)

        End If
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
    Private Sub LoopGetHeartBeat()
        While True
            Try
                SendGetHeartBeat()
            Catch ex As Exception

            End Try
            Sleep(3 * 1000)
        End While
    End Sub


    Public Function GetAvg(ByVal y() As Double) As Double
        Dim sum As Double
        For Each d In y
            sum = sum + d
        Next
        Return sum / (y.Count)
    End Function


    Private Sub AddMyLog(ByVal log As String, ByVal result As String)
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
                AddMyLog("断线_连接关闭", "")
            End If

            myClientSocket.Close()
        Catch ex As Exception
            If IsNothing(myDeviceInfo) = False Then
                log("断开前者TSS_GateWay_socket,DeviceID=" & myDeviceInfo.DeviceID & ">ERR>" & ex.Message)
            End If
        End Try
    End Sub
    Public Sub CloseALL(ByVal isOld As Boolean)
        log("TSS_GateWay_CloseALL-->[" & IP & ":" & Port & "]")
        AddMyLog("断线", "")
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
        ' loopGetHeartBeatThread
        If flagIsTekGateWay Then
            Try
                If IsNothing(loopGetHeartBeatThread) = False Then
                    loopGetHeartBeatThread.Abort()
                    log("关闭" & oldstr & "循环取网关经纬度线程成功")
                End If
            Catch ex As Exception
                log("关闭" & oldstr & "循环取网关经纬度线程失败！" & ex.Message)
            End Try
        End If

        Try
            If IsNothing(myHttpListener) = False Then
                myHttpListener.Abort()
                log("关闭" & oldstr & "myHttpListener成功")
            End If
        Catch ex As Exception
            log("关闭" & oldstr & "myHttpListener失败" & ex.ToString)
        End Try
        Try
            If IsNothing(DeviceListLock) Then DeviceListLock = New Object
            If IsNothing(DeviceList) Then DeviceList = New List(Of deviceStu)
            SyncLock DeviceListLock
                For i = DeviceList.Count - 1 To 0 Step -1
                    Dim itm As deviceStu = DeviceList(i)
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
            log("准备断开前者TSS_GateWay_Socket,DeviceID=" & myDeviceInfo.DeviceID)
            ' Threading.Thread.Sleep(10000)
            htp.Close()
            log("已断开前者TSS_GateWay_Socket,DeviceID=" & myDeviceInfo.DeviceID)
        Catch ex As Exception
            log("断开前者TSS_GateWay_socket,DeviceID=" & myDeviceInfo.DeviceID & ">ERR>" & ex.Message)
        End Try
    End Sub
    Private Sub SendGetHeartBeat()
        SendMsgToTSSDevByString(&H0, "task", "getheartbeat", "", Nothing)
    End Sub
    Private Sub handleTm(ByVal by() As Byte)
        HeartWatcher = 1
        Dim tm As tssMsg = byte2tssmsg(by)
        Dim base As String = Convert.ToBase64String(by)
        Dim id As String = tm.deviceID
        ' log("<接收TSS_GateWay_Dev> id=" & id & ",datatype=" & tm.datatype & "," & "functype=" & tm.functype)
        If tm.datatype = "ACK" Then
            Dim msg As String = tm.canshuqu
            If InStr(msg, "cmd=WSZL") Then

            End If
        End If
        If tm.datatype = "link" Then
            If tm.functype = "logon" Then
                HandleLogin(id)
                Sleep(1000)
                SendGetHeartBeat()
            End If
        End If

        If tm.datatype = "DevData" Then
            If tm.functype = "State" Then
                log("收到" & myDeviceInfo.DeviceID & "上报设备状态")
                ' handleDeviceState(tm, IP, Port)
            End If
        End If


        If tm.datatype.ToLower() = "data" Then
            If tm.functype.ToLower() = "heartbeat" Then
                'If myDeviceInfo.DeviceID = "DSG_GW0047" Then
                '    log("47收到网关设备心跳包" & myDeviceInfo.DeviceID & "--->" & tm.canshuqu)
                'End If
                HandleGateWayHeartBeat(tm)
                'If myDeviceInfo.DeviceID = "DSG-GW0001" Then
                '    log("001收到网关设备心跳包" & myDeviceInfo.DeviceID & "--->" & tm.canshuqu)
                'End If
                ' <heartbeat:deviceid = 320160035,swnet=In,swpow=off;>

            End If
        End If
    End Sub
    Private Sub HandleGateWayHeartBeat(tm As tssMsg)
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
        'If myDeviceInfo.DeviceID = "DSG-GW0107" Then
        '    log("107网关上报心跳包," & st(1))
        'End If
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
                    Dim gwsinfo As New GateWayStatusInfo
                    Try
                        For Each skt In sk
                            If Not flagIsUseFenHao Then skt = skt.Replace(";", "")
                            If skt = "" Then Continue For
                            If skt.Contains("=") = False Then Continue For
                            Dim k As String = skt.Split("=")(0)
                            Dim v As String = skt.Split("=")(1)
                            'If myDeviceInfo.DeviceID = "DSG-GW0100" Then
                            '    log(k & "=" & v)
                            'End If
                            ' Console.WriteLine(k & "=" & v)
                            If k = "deviceid" Then sh.NetDeviceID = v
                            If k = "swnet" Then
                                If v = "in" Then sh.NetSwitch = 1
                                If v = "out" Then sh.NetSwitch = 2
                                gwsinfo.net = v
                            End If
                            If k = "swpow" Then
                                gwsinfo.power = v
                            End If
                            If k = "voltage" Then
                                If IsNumeric(v) Then
                                    gwsinfo.voltage = Double.Parse(v)
                                End If
                            End If
                            If k = "longitude" Then
                                flagHaveGPSStatus = True
                                If IsNothing(v) = False Then
                                    If v <> "" Then
                                        If IsNumeric(v) Then
                                            gwsinfo.lon = Val(v)
                                            flagHaveLng = True
                                            ' deviceGPSReporterLat = v  '2018-12-09 设备上报的经纬度是反的，故迎合设备
                                            deviceGPSReporterLng = v
                                        End If
                                    End If
                                End If
                            End If
                            If k = "latitude" Then
                                flagHaveGPSStatus = True
                                If IsNothing(v) = False Then
                                    If v <> "" Then
                                        If IsNumeric(v) Then
                                            gwsinfo.lat = Val(v)
                                            flagHaveLat = True
                                            deviceGPSReporterLat = v
                                            ' deviceGPSReporterLng = v  '2018-12-09 设备上报的经纬度是反的，故迎合设备
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
                                    'log("经纬度需要转置，deviceId=" & myDeviceInfo.DeviceID)
                                    Dim t As Double = deviceGPSReporterLat
                                    deviceGPSReporterLat = deviceGPSReporterLng
                                    deviceGPSReporterLng = t
                                End If
                                Dim cinfo As CoordInfo = GPS2BDS(deviceGPSReporterLng, deviceGPSReporterLat)
                                gwsinfo.lon = cinfo.x
                                gwsinfo.lat = cinfo.y
                                deviceGPSReporterLng = cinfo.x
                                deviceGPSReporterLat = cinfo.y
                                'log("收到网关设备上报正确经纬度！id=" & myDeviceInfo.DeviceID & "," & deviceGPSReporterLng & "," & deviceGPSReporterLat)

                                sh.Lng = gwsinfo.lon
                                sh.Lat = gwsinfo.lat
                                Dim sql As String = "select * from deviceTable where dsgwgDeviceId='" & myDeviceInfo.DeviceID & "'"
                                Dim dt As DataTable = SQLGetDT(sql)
                                If IsNothing(dt) = False AndAlso dt.Rows.Count > 0 Then
                                    Dim row As DataRow = dt.Rows(0)
                                    Dim tekDeviceId As String = row("DeviceID").ToString()
                                    '  log($"网关{myDeviceInfo.DeviceID}网关上报心跳包,对应的使用者DeviceId=" & tekDeviceId)
                                    If IsNothing(tekDeviceId) = False AndAlso IsDBNull(tekDeviceId) = False And tekDeviceId <> "" Then
                                        SyncLock DeviceListLock
                                            For m = 0 To DeviceList.Count - 1
                                                Dim d As DeviceStu = DeviceList(m)
                                                If d.DeviceID = tekDeviceId Then
                                                    Dim cls As DeviceTSS = d.cls
                                                    If IsNothing(cls) = False Then
                                                        cls.SetmyRunLocation(New RunLocation(deviceGPSReporterLng, deviceGPSReporterLat, Now.ToString("yyyy-MM-dd HH:mm:ss")))
                                                    End If
                                                    Exit For
                                                End If
                                            Next
                                        End SyncLock
                                    End If
                                End If
                            End If
                            'Dim sqlTmp As String = "update deviceTable set Lng='{0}',Lat='{1}' where DeviceID='{2}'"
                            'sqlTmp = String.Format(sqlTmp, New String() {sh.Lng, sh.Lat, sh.DeviceID})
                            'SQLCmd(sqlTmp)
                        Else
                            If flagHaveGPSStatus Then
                                ' log("收到网关设备上报经纬度字段，id=" & myDeviceInfo.DeviceID)
                            End If
                        End If
                    Catch ex As Exception

                    End Try
                    sh.DSGWGstatus = gwsinfo
                    DeviceList(i) = sh
                    Exit For
                End If
            Next
            RaiseEvent RefrushDeviceList()
        End SyncLock
    End Sub

    Private Sub handleDeviceState(ByVal tm As tssMsg, ByVal ip As String, ByVal port As String)
        Dim id As String = tm.deviceID
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
                        sh.Lng = st(0).Split("=")(1)
                        sh.Lat = st(1).Split("=")(1)
                        sh.Func = st(3).Split("=")(1)
                    End If
                    DeviceList(i) = sh
                    Try
                        'Dim sqlTmp As String = "update deviceTable set Lng='{0}',Lat='{1}' where DeviceID='{2}'"
                        'sqlTmp = String.Format(sqlTmp, New String() {sh.Lng, sh.Lat, sh.DeviceID})
                        'SQLCmd(sqlTmp)
                    Catch ex As Exception

                    End Try
                    Exit For
                End If
            Next
            RaiseEvent RefrushDeviceList()
        End SyncLock

    End Sub
#Region "socket处理"
    Public Sub serverThreadProc()
        Try
            Dim sb As New SocketAndBuffer
            sb.Socket = myClientSocket
            sb.Socket.BeginReceive(sb.Buffer, 0, sb.Buffer.Length, SocketFlags.None, AddressOf ReceiveCallBack, sb)
        Catch ex As Exception

        End Try
    End Sub
    Dim byteSub() As Byte
    Private Sub HandleBuffer(ByVal buffer() As Byte)
        Try
            If IsNothing(byteSub) Then
                byteSub = buffer
            Else
                byteSub = byteSub.Concat(buffer).ToArray
            End If
            If byteSub.Length < 102 Then
                Exit Sub
            End If

            Dim readindex As Integer = 0
            Dim totalnum As Integer = byteSub.Length
            While True
                If readindex >= totalnum Then Exit While
                Try
                    If checkFlag(byteSub, readindex, totalnum - readindex) = True Then
                        byteSub = tobytes(byteSub, readindex, totalnum - readindex)
                        readindex = -1
                        totalnum = byteSub.Length
                        If byteSub.Length < 102 Then
                            Exit Sub
                        Else
                            Try
                                Dim tm As tssMsg = byte2tssmsgHead(byteSub)
                                If IsNothing(tm) = False Then
                                    Dim lenofmsg As Integer = tm.lenofmsg
                                    If byteSub.Length < lenofmsg Then
                                        Exit Sub
                                    End If
                                    If byteSub.Length = lenofmsg Then
                                        Try
                                            handleTm(byteSub)
                                        Catch ex As Exception

                                        End Try
                                        byteSub = Nothing
                                        Exit Sub
                                    End If
                                    If byteSub.Length > lenofmsg Then
                                        Dim bk() As Byte = tobytes(byteSub, 0, lenofmsg)
                                        Try
                                            handleTm(bk)
                                        Catch ex As Exception

                                        End Try
                                        byteSub = tobytes(byteSub, lenofmsg, byteSub.Length - lenofmsg)
                                        readindex = -1
                                        totalnum = byteSub.Count
                                    End If
                                Else
                                    byteSub = tobytes(byteSub, 102, byteSub.Length - 102)
                                    readindex = -1
                                    totalnum = byteSub.Count
                                End If
                            Catch ex As Exception
                                ' 'MsgBox("err1" & vbCrLf & ex.ToString)
                                ' RaiseEvent Exception(ex)
                            End Try
                        End If
                    End If
                Catch ex As Exception
                    '  'MsgBox("err2" & vbCrLf & ex.ToString)
                End Try
                readindex = readindex + 1
                If readindex >= totalnum Then Exit While
            End While
        Catch ex As Exception
            ' 'MsgBox("err3" & vbCrLf & ex.ToString)
        End Try
    End Sub
    Private Sub ReceiveCallBack(ByVal ar As IAsyncResult)
        Dim sb As SocketAndBuffer
        sb = CType(ar.AsyncState, SocketAndBuffer)
        Try
            If sb.Socket.Connected Then
                iLen = sb.Socket.EndReceive(ar)
                If iLen > 0 Then
                    ReDim oldbytes(iLen - 1)
                    Array.Copy(sb.Buffer, 0, oldbytes, 0, iLen)
                    HandleBuffer(oldbytes)
                    sb.Socket.BeginReceive(sb.Buffer, 0, sb.Buffer.Length, SocketFlags.None, AddressOf ReceiveCallBack, sb)
                Else
                    If (Not myClientSocket Is Nothing) Then
                        CloseALL(False)
                    End If
                End If
            End If
        Catch ex As Exception
            If (Not myClientSocket Is Nothing) Then
                CloseALL(False)
            End If
        End Try
    End Sub
    Private Class SocketAndBuffer
        Public Socket As System.Net.Sockets.Socket
        Public Buffer(8192) As Byte
    End Class
#End Region
    Private Sub StartHttp(ByVal cls As TSS_GateWay)
        Try
            log("TSS_GateWay_" & myDeviceInfo.DeviceID & ",尝试开启HTTP服务……")
            myHttpListener = New HttpListener
            myHttpListener.Prefixes.Add(myHttpUrl)
            myHttpListener.Start()
            log("TSS_GateWay_" & myDeviceInfo.DeviceID & ",已开启HTTP服务")
            log("TSS_GateWay_" & myDeviceInfo.DeviceID & ",尝试开启线程……")
            broadcastThread = New Thread(AddressOf sub_broadcastDeviceMsg)
            broadcastThread.Start()
            log("TSS_GateWay_" & myDeviceInfo.DeviceID & ",已开启线程")
            If IsNothing(DeviceListLock) Then DeviceListLock = New Object
            SyncLock DeviceListLock
                DeviceList.Add(myDeviceInfo)
            End SyncLock
            log("DeviceList已记录DeviceID=" & myDeviceInfo.DeviceID & ",位置=" & myDeviceInfo.Address)
            myHttpListener.BeginGetContext(New AsyncCallback(AddressOf GetContextCallBack), myHttpListener)
        Catch ex As Exception

            log("TSS_GateWay_" & myDeviceInfo.DeviceID & ",初始化HTTP等服务失败，ERR-->" & ex.Message)
        End Try
        If IsNothing(cls) = False Then

            cls.CloseSocket()
        End If
    End Sub
    Private Sub AddDeviceMsg(ByVal deviceMsg As String)
        Try
            If IsNothing(HttpMsgList_Object) Then HttpMsgList_Object = New Object
            SyncLock HttpMsgList_Object
                If IsNothing(HttpMsgList) Then HttpMsgList = New List(Of String)
                HttpMsgList.Add(deviceMsg)
            End SyncLock
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
                AddMyLog("接收一般命令", "失败,设备正在执行任务")
                response(context, "result=fail;msg=设备正在执行任务;errMsg=" & "null" & ";advise=")
                Exit Sub
                Dim msg As String = dataMsg
                Try
                    Dim tss As tssOrder_stu = JsonConvert.DeserializeObject(msg, GetType(tssOrder_stu))
                    If tss.task = "bscan" Then
                        AddMyLog("接收一般命令,频谱扫描", "成功")
                        Dim freqbegin As Double = tss.freqStart
                        Dim freqend As Double = tss.freqEnd
                        Dim freqstep As Double = tss.freqStep
                        Dim ifbw As String = 40000
                        Dim abw As String = freqstep
                        Dim bits As String = 16
                        Dim gcmode As String = "fagc"
                        Dim gcvalue As String = 0
                        Dim returndata As String = "fft"
                        Dim returninter As String = 1
                        Dim detetor As String = "real"
                        msg = "<bscan: tasknum=1" &
                             ";freqbegin=" & freqbegin &
                             ";freqend=" & freqend &
                              ";freqstep=" & freqstep &
                              ";ifbw=" & ifbw &
                               ";abw=" & abw &
                               ";bits=" & bits &
                               ";gcmode=" & gcmode &
                               ";gcvalue=" & gcvalue &
                              ";returndata=" & returndata &
                              ";detetor=" & detetor &
                               ";returninter =" & returninter & ">"
                        response(context, SendMsgToTSSDevByString(&H0, "task", "bscan", msg, Nothing))
                    End If
                    If tss.task = "stop" Then
                        AddMyLog("接收一般命令,停止工作", "成功")
                        response(context, SendMsgToTSSDevByString(&H0, "task", "taskctrl", "<taskctrl:taskstate=stop;>", Nothing))
                    End If
                    If tss.task = "ifscan_wav" Then
                        AddMyLog("接收一般命令,音频解调", "成功")
                        Dim Text As String = tss.freqStart
                        msg = "<ifscan_wav:tasknum=1;freq=" & Text & ";mod_fre=" & Text & ";freqstep=0.15625;ifbw=200;demodmode=FM;demodbw=200;rfwmode=nm;gcmode=fagc;gcvalue=0;returndata=audio;encodetype=pcm;bits=8;returninter=1;detector=real;rfwmode=nm;>"
                        'sendMsgToDev(&H0, "task", "ifscan_wav", msg, Nothing)
                        response(context, SendMsgToTSSDevByString(&H0, "task", "ifscan_wav", msg, Nothing))
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
                Sleep(2000)
                SendGetHeartBeat()
                Exit Sub
            End If
            If func = "netswitchout" Then
                AddMyLog("接收一般命令,切换到外网", "成功")
                response(context, SendMsgToTSSDevByString(&H0, "task", "netswitch", "<netswitch:sw=out;>", Nothing))
                Sleep(2000)
                SendGetHeartBeat()
                Exit Sub
            End If
            If func = "poweron" Then
                AddMyLog("接收一般命令,打开设备电源", "成功")
                response(context, SendMsgToTSSDevByString(&H0, "task", "powswitch", "<powswitch:pow=on;>", Nothing))
                Sleep(2000)
                SendGetHeartBeat()
                Exit Sub
            End If
            If func = "poweroff" Then
                AddMyLog("接收一般命令,关闭设备电源", "成功")
                response(context, SendMsgToTSSDevByString(&H0, "task", "powswitch", "<powswitch:pow=off;>", Nothing))
                Sleep(2000)
                SendGetHeartBeat()
                Exit Sub
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
                'log(sql)
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
        Catch ex As Exception

        End Try
        response(context, "")
    End Sub


    Public Function SendMsgToTSSDevByString(ByVal a As String, ByVal b As String, ByVal c As String, ByVal d As String, ByVal e As Byte()) As String
        '&H0, "DevMsg", "Test", "<info:func=;>", Nothing
        Try
            If a = "" Then a = &H0
            If b = "" Then Return "result=fail;msg=tssMsg Parse file;errMsg=null;advise=check out your parameter"
            If c = "" Then Return "result=fail;msg=tssMsg Parse file;errMsg=null;advise=check out your parameter"
            Dim by() As Byte = tssmsg2byte(msg2TssMsg(a, b, c, d, e))
            If IsNothing(by) Then
                log("TSSSer.SendData>ERR>Byte[]为空")
                Return "result=fail;msg=tssMsg Parse file;errMsg=null;advise=check out your parameter"
            End If
            'log("<发送TSS_GateWay_Dev> id=" & myDeviceInfo.DeviceID & "," & b & "," & c & "," & d)
            myClientSocket.Send(by)
            ' log("Success")
            Return "result=success;msg=success;errMsg=null;advise=good"
        Catch ex As Exception
            Return "result=fail;msg=null;errMsg=" & ex.Message & ";advise=null"
        End Try
    End Function
    Dim HeartWatcher As Integer = 0
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

            End Try
            Try
                num = num + 1
                If num = 30 Then
                    num = 0
                    SendMsgToTSSDevByString(&H0, "DevMsg", "Test", "<info:func=;>", Nothing)
                    'SendMsgToTSSDevByString(&H0, "DevMsg", "WSZL", "", Nothing)
                End If
            Catch ex As Exception

            End Try
            Try
                SleepHeartWatch = SleepHeartWatch + 1
                If SleepHeartWatch = 45 Then
                    SleepHeartWatch = 0
                    If HeartWatcher = 0 Then
                        Try
                            log("TSS_GateWay_" & myDeviceInfo.DeviceID & "通信超时，将主动断开Socket……")
                            CloseALLByChaoShi()
                        Catch ex As Exception

                        End Try
                    Else
                        HeartWatcher = 0
                    End If
                End If
            Catch ex As Exception

            End Try
            Sleep(broadcastSleep)
        End While
    End Sub
    Private Sub CloseALLByChaoShi()
        Dim oldstr As String = "超时"
        Try
            myHttpListener.Abort()
            log("关闭" & oldstr & "myHttpListener成功")
        Catch ex As Exception
            log("关闭" & oldstr & "myHttpListener失败" & ex.ToString)
        End Try
        Try
            If IsNothing(DeviceListLock) Then DeviceListLock = New Object
            If IsNothing(DeviceList) Then DeviceList = New List(Of deviceStu)
            SyncLock DeviceListLock
                For i = DeviceList.Count - 1 To 0 Step -1
                    Dim itm As deviceStu = DeviceList(i)
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
            log("断开超时TSS_GateWay_socket,DeviceID=" & myDeviceInfo.DeviceID & ">ERR>" & ex.Message)
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
