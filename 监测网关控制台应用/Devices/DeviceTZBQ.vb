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
Imports System.Math
Public Class DeviceTZBQ
    Private myDeviceInfo As New DeviceStu
    Private isPOATaskMyMain As Boolean = False
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
    Private myNormalTaskStu As New NormalTaskStu
    Private myYJSModuleEndTime As Date
    Private isMyYJSCreatingModule As Boolean
    Private myYJSModuleStu As yjsModuleStu
    Public Event RefrushDeviceList()
    Public Event RaiseLog(ByVal mainMsg As String, ByVal UserName As String, ByVal IP As String)
    Dim myTask As myTaskStu
    Dim myTaskLock As Object
    Dim DirPath As String = Directory.GetCurrentDirectory()  & "\DeviceTaskIni"
    Public Event RaiseHttplog(ByVal str As String)
    Dim isLogin As Boolean = False
    Structure yjsModuleStu
        Dim pdlist As List(Of Double)
        Dim cqlist As List(Of Double)
    End Structure
    Public Sub Httplog(ByVal str As String)
        RaiseEvent RaiseHttplog(str)
    End Sub
    Structure myTaskStu
        Dim myWorkFunc As String
        Dim myWorkBQ As String
        Dim SSSJPd As String
        Dim SSSJ As Integer
        Dim SSSJNum As Integer
        Sub New(ByVal _myWorkFunc As String, ByVal _myWorkBQ As String, ByVal _SSSJPd As String, ByVal _SSSJNum As String)
            myWorkFunc = _myWorkFunc
            myWorkBQ = _myWorkBQ
            SSSJPd = _SSSJPd
            SSSJNum = _SSSJNum
        End Sub
    End Structure
    Public Sub New(ByVal ClientSocket As Socket)
        myClientSocket = ClientSocket
        myTaskLock = New Object
        IP = myClientSocket.RemoteEndPoint.ToString.Split(":")(0)
        Port = myClientSocket.RemoteEndPoint.ToString.Split(":")(1)
    End Sub
    Public Sub Start()
        serverThreadProc()
        ' SendMsgToTZBQDev(tobyte("<TZBQ:STOP," & 0 & ">"))
        log("TZBQ设备连接，服务器主动发送握手命令," & IP & ":" & Port & "," & ip2Address(IP))
        '   Sleep(3000)
        Dim msg As String = getcrcstr("<TZBQ:WSML,0>")
        SendMsgToTZBQDev(tobyte(msg)) '发送握手命令    
        sssjHandleTime = Now
        'Sleep(3000)
        'If Not isLogin Then
        '    Dim th As New Thread(Sub()
        '                             Sleep(7000)
        '                             If Not isLogin Then
        '                                 CloseSocket()
        '                             End If
        '                         End Sub)
        '    th.Start()
        'End If
        'AddWarnMsg2WarnList("hasaki")
    End Sub
    Private Sub log(ByVal MainMsg As String)
        'Form1.log(MainMsg, "TZBQ_Server", "+:" & _Port)
        RaiseEvent RaiseLog(MainMsg, "TZBQ_" & myDeviceInfo.DeviceID, IP & ":" & Port)
    End Sub
    Private Sub HandleLogin(ByVal id As String)
        myRealID = id
        '查询该Id是否存在设备表中
        log("TZBQ设备登录，id=" & id)
        Dim NickName As String = id
        Dim DeviceName As String = "TZBQ_" & id
        DeviceName = id
        Dim dbLng As String = ""
        Dim dbLat As String = ""
        Dim sql As String = String.Format("select * from deviceTable where DeviceID='{0}' and Kind='{1}'", New String() {id, "TZBQ"})
        Dim dt As DataTable = SQLGetDT(sql)
        Dim myAddress As String = ip2Address(IP)
        Dim myLA As LocationAddressInfo
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
                insertSql = String.Format(insertSql, New String() {id, id, "TZBQ", Now.ToString("yyyy-MM-dd HH:mm:ss"), IP, Port})
                SQLCmd(insertSql)
            End If
        Else
            Dim insertSql As String = "insert into deviceTable (deviceid,deviceNickName,kind,OnlineTime,IP,port) values('{0}','{1}','{2}','{3}','{4}','{5}')"
            insertSql = String.Format(insertSql, New String() {id, id, "TZBQ", Now.ToString("yyyy-MM-dd HH:mm:ss"), IP, Port})
            SQLCmd(insertSql)
        End If
        Dim newMyAddressTmp As String = ""
        If IsNothing(myAddress) = False Then
            Dim st() As String = myAddress.Split(",")
            For Each itm In st
                If itm <> "" Then
                    newMyAddressTmp = newMyAddressTmp & itm & ","
                End If
            Next
            newMyAddressTmp = newMyAddressTmp.Substring(0, newMyAddressTmp.Length - 1)
        End If


        myDeviceInfo = New deviceStu
        myDeviceInfo.DeviceID = NickName
        myDeviceInfo.Address = newMyAddressTmp
        myHttpUrl = TZBQHttpListenerPath & id & "/"
        myDeviceInfo.HTTPMsgUrl = myHttpUrl
        myDeviceInfo.Kind = "TZBQ"
        myDeviceInfo.OnlineTime = Now.ToString("yyyy-MM-dd HH:mm:ss")
        myDeviceInfo.Statu = "normal"
        myDeviceInfo.IP = IP
        myDeviceInfo.Port = Port
        myDeviceInfo.Lng = dbLng
        myDeviceInfo.Lat = dbLat
        myDeviceInfo.cls = Me
        myDeviceInfo.Name = "TZBQ_" & id
        myDeviceInfo.Name = DeviceName
        myDeviceInfo.Func = "null"
        myDeviceInfo.NetSwitch = 0
        Dim isJXX As Boolean = False
        Dim JXXItm As deviceStu
        If IsNothing(DeviceListLock) Then DeviceListLock = New Object
        If IsNothing(DeviceList) Then DeviceList = New List(Of deviceStu)
        Dim isFind As Boolean = False
        SyncLock DeviceListLock
            For i = 0 To DeviceList.Count - 1
                Dim itm As deviceStu = DeviceList(i)
                If itm.DeviceID = NickName Then
                    log("TZBQ DeviceID=" & NickName & "已登录,正在处理同ID登录，New IP=" & IP & ":" & Port)
                    Try
                        If itm.IP = IP And itm.Port = Port Then
                            isFind = True
                            log("属于重复登录，无需处理")
                            Exit For
                        Else
                            isJXX = True
                            JXXItm = itm
                            Exit For
                        End If
                        log("TZBQ DeviceID=" & NickName & "重新登录,ip=" & IP & ":" & Port)
                    Catch ex As Exception

                    End Try
                    Exit For
                End If
            Next
        End SyncLock
        If isJXX Then
            Dim cls As DeviceTZBQ = CType(JXXItm.cls, DeviceTZBQ)
            cls.CloseALL(True)
            AddMyLog("重新上线", "成功")
            log("重新上线TZBQ设备登录，deviceID=" & id)
            StartHttp(cls)
            RaiseEvent RefrushDeviceList()
            isFind = True
        End If
        If isFind = False Then
            AddMyLog("上线", "成功")
            log("TZBQ设备登录，deviceID=" & id)
            StartHttp(Nothing)
            RaiseEvent RefrushDeviceList()
        End If

    End Sub
    Private Sub AddMyLog(ByVal log As String, ByVal result As String)
        AddDeviceLog(myDeviceInfo.DeviceID, myDeviceInfo.Name, myDeviceInfo.Address, log, result, "在线")
    End Sub
    Public Sub CloseSocket()
        Try
            Sleep(2000)

            If IsNothing(myDeviceInfo) = False Then
                AddMyLog("断线(TCP连接断开)", "")
            End If
            myClientSocket.Close()
        Catch ex As Exception
            If IsNothing(myDeviceInfo) = False Then
                log("断开TZBQ_socket,DeviceID=" & myDeviceInfo.DeviceID & ">ERR>" & ex.Message)
            End If
        End Try
    End Sub
    Public Sub CloseALL(ByVal isOld As Boolean)
        log("TZBQ_CloseALL-->[" & IP & ":" & Port & "]")
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
            ' log("TZBQ_" & myDeviceInfo.DeviceID & "断开连接，正在更新DeviceList……")
            SyncLock DeviceListLock
                For i = DeviceList.Count - 1 To 0 Step -1
                    Dim itm As deviceStu = DeviceList(i)
                    If itm.IP = IP And itm.Port = Port Then
                        DeviceList.RemoveAt(i)
                        Dim str As String = "DeviceList已移除" & isOld & "DeviceID=" & itm.DeviceID & ",位置=" & itm.Address & "(" & IP & ":" & Port & ")"
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

    End Sub

    Private Sub HandleBQ(ByVal by() As Byte)
        HeartWatcher = 1
        Dim deviceMsg As String = tostr(by)
        Dim id As String = getidbyBQ(deviceMsg)

        If (InStr(deviceMsg, "TZBQ:") And InStr(deviceMsg, "WSML,OK")) Then
            isLogin = True
            log("收到设备握手命令，deviceID=" & getidbyBQ(deviceMsg))
            HandleLogin(id)
            log("向设备发送状态查询命令……")
            Dim msg As String = getcrcstr("<TZBQ:SBZT,0>")
            SendMsgToTZBQDev(tobyte(msg))
            log("向设备发送状态查询命令成功")
            CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & id)
            CheckDir(Directory.GetCurrentDirectory() & "\Task\logs\" & id)
            GetMyTask()
        End If
        If isLogin = False Then
            ' log("TZBQ Msg id=" & id & " " & deviceMsg)
            Return
        End If
        Dim func As String = getFuncByBQ(deviceMsg)
        If func = "SBZT" Then
            If IsNothing(DeviceListLock) Then DeviceListLock = New Object
            SyncLock DeviceListLock
                Dim isRefrush As Boolean = False
                For i = 0 To DeviceList.Count - 1
                    Dim odt As deviceStu = DeviceList(i)
                    If odt.DeviceID = myDeviceInfo.DeviceID Then
                        Dim zt As String = deviceMsg
                        Dim st() As String = zt.Split(",")
                        If st.Count >= 8 Then
                            Try
                                Dim flag_IsWriteLngLat As Boolean = False
                                If myDeviceInfo.Lng = "" Or myDeviceInfo.Lat = "" Then
                                    myDeviceInfo.Lng = st(2)
                                    myDeviceInfo.Lat = st(3)
                                    odt.Lng = st(2)
                                    odt.Lat = st(3)
                                    Dim sqlTmp As String = "update deviceTable set Lng='{0}',Lat='{1}' where DeviceID='{2}'"
                                    sqlTmp = String.Format(sqlTmp, New String() {myDeviceInfo.Lng, myDeviceInfo.Lat, myDeviceInfo.DeviceID})
                                    SQLCmd(sqlTmp)
                                    DeviceList(i) = odt
                                End If
                            Catch ex As Exception

                            End Try
                            isRefrush = True
                        End If
                    End If

                Next
                If isRefrush Then RaiseEvent RefrushDeviceList()
            End SyncLock
        End If
        If func = "SSSJ" Then
            Dim BQ As String = deviceMsg
            Dim str As String = BQ.Substring(InStr(BQ, "<"), InStr(BQ, ">") - InStr(BQ, "<") - 1)
            Dim st() As String = str.Split(",")
            Dim numOfValue As Integer = st(2)
            If numOfValue = myTask.SSSJNum Then
                deviceMsg = deviceMsg.Replace(">", "," & myTask.SSSJPd & ">")
            End If
        End If
        If func = "WARN" Then
            Dim warnKind As String = GetWarnKind(deviceMsg)
            AddWarnMsg2WarnList(deviceMsg)
            Dim sql As String = "insert into WarnTable "
            sql = sql & "(MsgTime,DeviceID,DeviceKind,Func,DeviceMsg,Lng,Lat,Address,WarnKind) values "
            sql = sql & "('{0}','{1}','{2}','{3}','{4}','{5}','{6}','{7}','{8}')"
            Dim para As New List(Of String)
            para.Add(Now.ToString("yyyy-MM-dd HH:mm:ss"))
            para.Add(myDeviceInfo.DeviceID)
            para.Add("TZBQ")
            para.Add(func)
            para.Add(deviceMsg)
            para.Add(myDeviceInfo.Lng)
            para.Add(myDeviceInfo.Lat)
            para.Add(myDeviceInfo.Address)
            para.Add(warnKind)
            sql = String.Format(sql, para.ToArray)
            Dim result As String = SQLCmd(sql)

        End If
        Dim CBQ As String = CutBQ(deviceMsg)
        AddDeviceMsg(deviceMsg)

        ' AddMainBroderCastDeviceMsg(jsMsg)
        If isWorking Then
            If myNormalTaskStu.TaskName = "事件预警" Or myNormalTaskStu.TaskName = "状态预警" Then
                If func = "SSSJ" Then
                    Dim BQ As String = deviceMsg
                    Dim str As String = BQ.Substring(InStr(BQ, "<"), InStr(BQ, ">") - InStr(BQ, "<") - 1)
                    Dim st() As String = str.Split(",")
                    Dim numOfValue As Integer = st(2)
                    If numOfValue = myTask.SSSJNum Then
                        deviceMsg = deviceMsg.Replace(">", "," & myTask.SSSJPd & ">")
                        Try
                            HandleSSSJ(deviceMsg)
                        Catch ex As Exception
                            log("HandleSSSJ>ERR>ERR" & ex.Message)
                        End Try
                    End If
                End If
            End If
            RecordDeviceMsg(myDeviceInfo.DeviceID, "TZBQ", func, deviceMsg)
        End If

    End Sub
    Dim sssjHandleTime As Date = "2018-01-01 00:00:00"
    Structure ztInfo
        Dim pl As Double
        Dim ModuleValue As Double
        Dim MaxNewTime As Date
        Dim MinNewTime As Date
        Dim KongNewTime As Date
        Dim MaxValue As Double
        Dim MinValue As Double
        Dim KongValue As Double
        Dim MaxTime As Integer
        Dim MinTime As Integer
        Dim KongTime As Integer
        Dim realValue As Double
        Sub New(ByVal _pl As Double, ByVal _MaxValue As Double, ByVal _MinValue As Double, ByVal _KongValue As Double, ByVal _MaxTime As Integer, ByVal _MinTime As Integer, ByVal _KongTime As Integer)
            pl = _pl
            MaxValue = _MaxValue
            MinValue = _MinValue
            KongValue = _KongValue
            MaxTime = _MaxTime
            MinTime = _MinTime
            KongTime = _KongTime
        End Sub
    End Structure
    Dim ztStu As List(Of ztInfo)
    Private Sub HandleSSSJ(ByVal bq As String)

        Dim t1 As TimeSpan = Now - sssjHandleTime
        'Console.WriteLine(t1.TotalSeconds)
        If t1.TotalSeconds <= 1 Then
            Return
        Else
            sssjHandleTime = Now
        End If
        Dim strtmp As String = Now.ToString("HH:mm:ss")
        Dim str As String = bq.Substring(InStr(bq, "<"), InStr(bq, ">") - InStr(bq, "<") - 1)
        Dim st() As String = str.Split(",")
        Dim numOfValue As Integer = st(2)
        Dim SigNalList As New List(Of Double)
        Dim CqList As New List(Of Double)
        If st.Length < 2 + numOfValue + 2 Then Return
        If st.Length > numOfValue * 2 Then
            For i = 3 To 2 + numOfValue
                Dim pd As Double = st(i + numOfValue)
                Dim cq As Double = st(i)
                SigNalList.Add(pd)
                CqList.Add(cq)
            Next
            If isMyYJSCreatingModule Then
                If IsNothing(myYJSModuleStu.pdlist) Then
                    myYJSModuleStu.pdlist = New List(Of Double)
                    myYJSModuleStu.cqlist = New List(Of Double)
                    For i = 0 To SigNalList.Count - 1
                        myYJSModuleStu.pdlist.Add(SigNalList(i))
                        myYJSModuleStu.cqlist.Add(CqList(i))
                    Next
                Else
                    If SigNalList.Count = myYJSModuleStu.pdlist.Count Then
                        For i = 0 To CqList.Count - 1
                            Dim myValue As Double = myYJSModuleStu.cqlist(i)
                            Dim newValue As Double = CqList(i)
                            If myValue < newValue Then
                                myYJSModuleStu.cqlist(i) = newValue
                            End If
                        Next
                    End If
                End If
                If Now > myYJSModuleEndTime Then
                    isMyYJSCreatingModule = False
                    log("建模完成")
                    If IsNothing(ztStu) = False Then
                        log("建模完成，状态预警初始化……")
                        'Try
                        '    Dim json As String = JsonConvert.SerializeObject(ztStu)
                        '    Dim path As String =  Directory.GetCurrentDirectory()  & "\ztStuBefore.txt"
                        '    Dim sw As New StreamWriter(path, False, Encoding.Default)
                        '    sw.Write(json)
                        '    sw.Close()
                        'Catch ex As Exception

                        'End Try
                        If ztStu.Count = myYJSModuleStu.pdlist.Count Then
                            For i = 0 To myYJSModuleStu.pdlist.Count - 1
                                Dim pl As Double = myYJSModuleStu.pdlist(i)
                                Dim value As Double = myYJSModuleStu.cqlist(i)
                                For j = 0 To ztStu.Count - 1
                                    Dim itm As ztInfo = ztStu(j)
                                    If itm.pl = pl Then
                                        itm.ModuleValue = value
                                        itm.MaxValue = (1 - (itm.MaxValue / 100)) * value
                                        itm.MinValue = (1 + (itm.MinValue / 100)) * value
                                        itm.KongNewTime = Now
                                        itm.MaxNewTime = Now
                                        itm.MinNewTime = Now
                                        ztStu(j) = itm
                                        Exit For
                                    End If
                                Next
                            Next
                        End If
                        log("状态预警初始化完毕")
                        'Try
                        '    Dim json As String = JsonConvert.SerializeObject(ztStu)
                        '    Dim path As String =  Directory.GetCurrentDirectory()  & "\ztStu.txt"
                        '    Dim sw As New StreamWriter(path, False, Encoding.Default)
                        '    sw.Write(json)
                        '    sw.Close()
                        'Catch ex As Exception

                        'End Try

                    End If

                End If
            Else
                If IsNothing(myYJSModuleStu.pdlist) Then
                    Return
                End If
                If SigNalList.Count = myYJSModuleStu.pdlist.Count Then
                    If myNormalTaskStu.TaskName = "事件预警" Then
                        For i = 0 To CqList.Count - 1
                            Dim myValue As Double = myYJSModuleStu.cqlist(i)
                            Dim newValue As Double = CqList(i)
                            If newValue - myValue >= 10 Then
                                'MsgBox(myYJSModuleStu.pdlist(i) & "," & myYJSModuleStu.cqlist(i))
                                'st.Add("<TZBQ:WARN," & ID & ",GR,115.96,29.7092,16-11-23,09:28:02,400," & d & "," & t & ",2>" & vbCrLf)
                                Dim id As String = myDeviceInfo.DeviceID
                                Dim lng As String = myDeviceInfo.Lng
                                Dim lat As String = myDeviceInfo.Lat
                                Dim time1 As String = Now.ToString("yyyy-MM-dd")
                                Dim time2 As String = Now.ToString("HH:mm:ss")
                                Dim pl As Double = myYJSModuleStu.pdlist(i)
                                Dim d As Double = newValue
                                Dim t As Integer = Math.Abs(100 * ((d - myValue) / myValue))
                                Dim deviceMsg As String = "<TZBQ:WARN," & id & ",WZ," & lng & "," & lat & "," & time1 & "," & time2 & "," & pl & "," & d & "," & t & ">"
                                deviceMsg = getcrcstr(deviceMsg)
                                AddDeviceMsg(deviceMsg)
                                AddWarnMsg2WarnList(deviceMsg)
                                Dim warnKind As String = GetWarnKind(deviceMsg)
                                Dim sql As String = "insert into WarnTable "
                                sql = sql & "(MsgTime,DeviceID,DeviceKind,Func,DeviceMsg,Lng,Lat,Address,WarnKind) values "
                                sql = sql & "('{0}','{1}','{2}','{3}','{4}','{5}','{6}','{7}','{8}')"
                                Dim para As New List(Of String)
                                para.Add(Now.ToString("yyyy-MM-dd HH:mm:ss"))
                                para.Add(myDeviceInfo.DeviceID)
                                para.Add("TZBQ")
                                para.Add("WARN")
                                para.Add(deviceMsg)
                                para.Add(myDeviceInfo.Lng)
                                para.Add(myDeviceInfo.Lat)
                                para.Add(myDeviceInfo.Address)
                                para.Add(warnKind)
                                sql = String.Format(sql, para.ToArray)
                                Dim result As String = SQLCmd(sql)
                            End If
                        Next
                    End If
                    If myNormalTaskStu.TaskName = "状态预警" Then
                        For i = 0 To CqList.Count - 1
                            Dim newValue As Double = CqList(i)
                            Dim itm As ztInfo = ztStu(i)
                            If newValue < itm.MaxValue Then
                                itm.MaxNewTime = Now
                            End If
                            If newValue > itm.MinValue Then
                                itm.MinNewTime = Now
                            End If
                            If newValue > itm.KongValue Then
                                itm.KongNewTime = Now
                            End If
                            itm.realValue = newValue
                            ztStu(i) = itm
                        Next
                        Dim nTime As Date = Now
                        For i = 0 To ztStu.Count - 1
                            Dim itm As ztInfo = ztStu(i)
                            Dim tMax As Integer = (nTime - itm.MaxNewTime).TotalSeconds
                            Dim tMin As Integer = (nTime - itm.MinNewTime).TotalSeconds
                            Dim tKong As Integer = (nTime - itm.KongNewTime).TotalSeconds
                            Dim id As String = myDeviceInfo.DeviceID
                            Dim lng As String = myDeviceInfo.Lng
                            Dim lat As String = myDeviceInfo.Lat
                            Dim time1 As String = Now.ToString("yyyy-MM-dd")
                            Dim time2 As String = Now.ToString("HH:mm:ss")
                            Dim pl As Double = myYJSModuleStu.pdlist(i)
                            Dim d As Double = itm.realValue
                            Dim deviceMsg As String = ""
                            If tMax >= itm.MaxTime Then
                                itm.MaxNewTime = nTime
                                '超标
                                Dim t As Integer = Math.Abs(100 * ((d - itm.ModuleValue) / itm.ModuleValue))
                                deviceMsg = "<TZBQ:WARN," & id & ",CB," & lng & "," & lat & "," & time1 & "," & time2 & "," & pl & "," & d & "," & t & ">"

                            End If
                            If tMin >= itm.MinTime Then
                                itm.MinNewTime = nTime
                                '故障
                                Dim t As Integer = Math.Abs(100 * ((itm.ModuleValue - d) / itm.ModuleValue))
                                deviceMsg = "<TZBQ:WARN," & id & ",GZ," & lng & "," & lat & "," & time1 & "," & time2 & "," & pl & "," & d & "," & t & ">"

                            End If
                            If tKong >= itm.KongTime Then
                                itm.KongNewTime = nTime
                                '空闲
                                Dim t As Integer = Math.Abs(100 * ((itm.KongValue - d) / itm.KongValue))
                                deviceMsg = "<TZBQ:WARN," & id & ",KX," & lng & "," & lat & "," & time1 & "," & time2 & "," & pl & "," & d & "," & t & ">"

                            End If
                            If deviceMsg = "" Then Continue For
                            deviceMsg = getcrcstr(deviceMsg)
                            AddDeviceMsg(deviceMsg)
                            AddWarnMsg2WarnList(deviceMsg)
                            Dim warnKind As String = GetWarnKind(deviceMsg)
                            Dim sql As String = "insert into WarnTable "
                            sql = sql & "(MsgTime,DeviceID,DeviceKind,Func,DeviceMsg,Lng,Lat,Address,WarnKind) values "
                            sql = sql & "('{0}','{1}','{2}','{3}','{4}','{5}','{6}','{7}','{8}')"
                            Dim para As New List(Of String)
                            para.Add(Now.ToString("yyyy-MM-dd HH:mm:ss"))
                            para.Add(myDeviceInfo.DeviceID)
                            para.Add("TZBQ")
                            para.Add("WARN")
                            para.Add(deviceMsg)
                            para.Add(myDeviceInfo.Lng)
                            para.Add(myDeviceInfo.Lat)
                            para.Add(myDeviceInfo.Address)
                            para.Add(warnKind)
                            sql = String.Format(sql, para.ToArray)
                            Dim result As String = SQLCmd(sql)
                            ztStu(i) = itm
                        Next
                    End If
                End If
            End If
        End If
    End Sub

    Public Sub GetMyTask()
        Try
            log("获取我的任务……")
            Dim dt As DataTable = SQLGetDT("select * from UserTaskTable Where DeviceID='" & myDeviceInfo.DeviceID & "' and EndTime >'" & Now.ToString("yyyy-MM-dd HH:mm:ss") & "' order by StartTime")

            If dt.Rows.Count = 0 Then
                SyncLock DeviceListLock
                    For i = 0 To DeviceList.Count - 1
                        Dim itm As deviceStu = DeviceList(i)
                        If itm.DeviceID = myDeviceInfo.DeviceID Then
                            itm.Statu = "free"
                            DeviceList(i) = itm
                            Exit For
                        End If
                    Next
                End SyncLock
                '没有单个任务，查询所有未完成任务看有无多设备任务
                dt = Nothing
                dt = SQLGetDT("select * from UserTaskTable Where EndTime >'" & Now.ToString("yyyy-MM-dd HH:mm:ss") & "' order by StartTime")
                If IsNothing(dt) = False Then
                    For Each r As DataRow In dt.Rows
                        Dim TaskDeviceID As String = r("DeviceID")
                        Dim list As List(Of String) = JsonConvert.DeserializeObject(TaskDeviceID, GetType(List(Of String)))
                        If IsNothing(list) = False Then
                            For j = 0 To list.Count - 1
                                Dim d As String = list(j)
                                If d = myDeviceInfo.DeviceID Then
                                    '有任务
                                    If isWorking = False Then
                                        If j = 0 Then
                                            isPOATaskMyMain = True
                                            log("POA任务，main=" & d)
                                        Else
                                            isPOATaskMyMain = False
                                        End If
                                        log("有任务，开始处理……")
                                        AddMyLog("查询任务", "成功,有待执行任务，并开始处理")
                                        SyncLock DeviceListLock
                                            For i = 0 To DeviceList.Count - 1
                                                Dim itm As deviceStu = DeviceList(i)
                                                If itm.DeviceID = myDeviceInfo.DeviceID Then
                                                    itm.Statu = "working"
                                                    DeviceList(i) = itm
                                                    Exit For
                                                End If
                                            Next
                                        End SyncLock
                                        TaskWorkerThread = New Thread(AddressOf TaskWorker)
                                        TaskWorkerThread.Start(r)
                                        Exit Sub
                                    Else
                                        AddMyLog("查询到有待执行任务，尝试执行", "失败，因为设备正在执行任务，等待现有任务执行完毕")
                                        log("有任务，正在执行无需处理")
                                        Exit Sub
                                    End If
                                    Exit For
                                End If
                            Next
                        End If
                    Next
                End If
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
                        Dim itm As deviceStu = DeviceList(i)
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
                AddMyLog("查询到有待执行任务，尝试执行", "失败，因为设备正在执行任务，等待现有任务执行完毕")
                log("有任务，正在执行无需处理")
            End If
        Catch ex As Exception
            log("获取我的任务-->err-->" & ex.ToString)
        End Try
    End Sub
    Dim isWorking As Boolean = False
    Structure YuJingStu
        Dim plist As List(Of Double)
        Dim moduleTime As Integer
        Sub New(ByVal _plist As List(Of Double), ByVal _moduleTime As Integer)
            plist = _plist
            moduleTime = _moduleTime
        End Sub
    End Structure
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
        myNormalTaskStu = task
        Dim SleepInt As Integer = task.TimeStep * 10000
        SleepInt = 30000
        Dim BQSList As String
        If task.TaskName = "频点监测" Then
            Dim code As String = task.TaskCode
            code = code.Replace("[", "")
            code = code.Replace("]", "")
            If InStr(code, ",") Then
                Dim pds() As String = code.Split(",")
                Dim pdList As New List(Of Double)
                For Each itm In pds
                    If itm = "" Then Continue For
                    If IsNumeric(itm) Then
                        pdList.Add(Val(itm))
                    End If
                Next
                Dim jcpd As String = "<TZBQ:JCPD," & myRealID & "," & 10 & "," & "Y" & "," & 15 & "," & 15 & "," & 10 & "," & 1 & "," & 1 & "," & pdList.Count
                For i = 0 To pdList.Count - 1
                    jcpd = jcpd & "," & pdList(i)
                Next
                jcpd = jcpd & ">"
                BQSList = jcpd
            End If
        End If
        If task.TaskName = "台站监督" Or task.TaskName = "可用评估" Or task.TaskName = "违章台站" Then
            Dim code As String = task.TaskCode
            code = code.Replace("[", "")
            code = code.Replace("]", "")
            If InStr(code, ",") Then
                Dim pds() As String = code.Split(",")
                Dim pdList As New List(Of Double)
                For Each itm In pds
                    If itm = "" Then Continue For
                    If IsNumeric(itm) Then
                        pdList.Add(Val(itm))
                    End If
                Next
                Dim jcpd As String = "<TZBQ:JCPD," & myRealID & "," & 10 & "," & "Y" & "," & 15 & "," & 15 & "," & 10 & "," & 1 & "," & 1 & "," & pdList.Count
                For i = 0 To pdList.Count - 1
                    jcpd = jcpd & "," & pdList(i)
                Next
                jcpd = jcpd & ">"
                BQSList = jcpd
            End If
        End If
        If task.TaskName = "事件预警" Then
            Dim code As String = task.TaskCode
            Dim yjs As YuJingStu = JsonConvert.DeserializeObject(code, GetType(YuJingStu))
            myYJSModuleEndTime = Now.AddSeconds(yjs.moduleTime)
            isMyYJSCreatingModule = True
            myYJSModuleStu = New yjsModuleStu
            Dim pdList As List(Of Double) = yjs.plist
            Dim jcpd As String = "<TZBQ:JCPD," & myRealID & "," & 10 & "," & "Y" & "," & 15 & "," & 15 & "," & 10 & "," & 1 & "," & 1 & "," & pdList.Count
            For i = 0 To pdList.Count - 1
                jcpd = jcpd & "," & pdList(i)
            Next
            jcpd = jcpd & ">"
            BQSList = jcpd
        End If
        If task.TaskName = "状态预警" Then
            Dim code As String = task.TaskCode
            Dim z As ZTYJ_stu = JsonConvert.DeserializeObject(code, GetType(ZTYJ_stu))
            myYJSModuleEndTime = Now.AddSeconds(z.ModuleTime)
            isMyYJSCreatingModule = True
            myYJSModuleStu = New yjsModuleStu
            Dim pdList As List(Of Double) = z.pdList
            Dim jcpd As String = "<TZBQ:JCPD," & myRealID & "," & 10 & "," & "Y" & "," & 15 & "," & 15 & "," & 10 & "," & 1 & "," & 1 & "," & pdList.Count
            For i = 0 To pdList.Count - 1
                jcpd = jcpd & "," & pdList(i)
            Next
            jcpd = jcpd & ">"
            BQSList = jcpd
            ztStu = New List(Of ztInfo)
            For Each v In pdList
                ztStu.Add(New ztInfo(v, z.MaxPercent, z.MinPercent, z.MinValue, z.HoldSecond, z.HoldSecond, z.MinValueSecond))
            Next
        End If
        If task.TaskName = "POA定位" Then
            Dim code As String = task.TaskCode
            Dim pdList As New List(Of Double)
            pdList.Add(Val(code))
            Dim jcpd As String = "<TZBQ:JCPD," & myRealID & "," & 10 & "," & "Y" & "," & 15 & "," & 15 & "," & 10 & "," & 1 & "," & 1 & "," & pdList.Count
            For i = 0 To pdList.Count - 1
                jcpd = jcpd & "," & pdList(i)
            Next
            jcpd = jcpd & ">"
            BQSList = jcpd

        End If
        Sleep(1000)
        While True
            isWorking = True
            If Now >= task.EndTime Then
                Dim cls As TaskTZBQ
                SyncLock taskTZBQHelperListLock
                    For Each itm As TaskTZBQ In taskTZBQHelperList
                        If itm.myDeviceId = myDeviceInfo.DeviceID Then
                            cls = itm
                            Exit For
                        End If
                    Next
                End SyncLock
                If IsNothing(cls) Then
                    cls = New TaskTZBQ(myDeviceInfo.DeviceID, task, isPOATaskMyMain)
                    cls.MakeTaskReport()
                Else
                    cls.MakeTaskReport()
                End If
                Exit While
            End If
            If Now >= task.StartTime Then
                SendMsgToTZBQById(BQSList)
                If getFuncByBQ(BQSList) = "JCPD" Then
                    Sleep(1000)
                    SendMsgToTZBQById("<TZBQ:SSSJ," & myDeviceInfo.DeviceID & ",1>")
                End If
            End If
            Sleep(SleepInt)
        End While
        isWorking = False
        ztStu = Nothing
        GetMyTask()
    End Sub



    Private Function GetAvg(ByVal d As List(Of Double)) As Double
        If IsNothing(d) Then Return 0
        Dim sum As Double = 0
        For Each itm In d
            sum = sum + itm
        Next
        Return sum / d.Count
    End Function
    Private Function GetPOAMap(ByVal DeviceNameList As List(Of String), ByVal lngList As List(Of Double), ByVal latList As List(Of Double), ByVal valueList As List(Of Double)) As MemoryStream
        Dim wb As New WebClient
        ' Dim imgPath As String = "C:\Users\meizi\Desktop\img.jpg"
        Dim u As String = "http://api.map.baidu.com/staticimage/v2?"
        Dim dik As New Dictionary(Of String, String)
        Dim aLng As Double = GetAvg(lngList)
        Dim aLat As Double = GetAvg(latList)
        Dim markers As String
        Dim markerStyles As String
        For i = 0 To lngList.Count - 1
            Dim lng As Double = lngList(i)
            Dim lat As Double = latList(i)
            markers = markers & "|" & lng & "," & lat
            markerStyles = markerStyles & "|" & "l,A,0xff0000"
        Next
        markers = markers.Substring(1, markers.Length - 1)
        markerStyles = markerStyles.Substring(1, markerStyles.Length - 1)
        Dim labelStyles As String
        For i = 0 To DeviceNameList.Count - 1
            Dim dName As String = DeviceNameList(i)
            labelStyles = labelStyles & "|" & dName & "(" & valueList(i) & "dBm)" & ",1,12,0xffffff,0x000fff,1"
        Next
        labelStyles = labelStyles.Substring(1, labelStyles.Length - 1)
        dik.Add("ak", "5cW7OlxZXThtbkq1Y0u5yNO6")
        dik.Add("center", aLng & "," & aLat)
        dik.Add("width", "600")
        dik.Add("height", "500")
        Dim zoom As Integer = GetZoom(lngList, latList)

        dik.Add("zoom", zoom)
        dik.Add("markers", markers)
        dik.Add("markerStyles", markerStyles)
        dik.Add("labels", markers)
        dik.Add("labelStyles", labelStyles)

        Dim url As String = u & TransforPara2Query(dik)

        Dim stm As Stream = wb.OpenRead(url)
        Dim sr As New StreamReader(stm)
        Dim by(1000000) As Byte
        Dim sumNum As Integer = by.Length
        Dim startIndex As Integer = 0
        Dim realByteNum As Integer = 0
        While sumNum > 0
            Dim n As Integer = stm.Read(by, startIndex, sumNum)
            If n = 0 Then
                Exit While
            End If
            startIndex = startIndex + n
            sumNum = sumNum - n
            realByteNum = realByteNum + n
        End While
        sr.Close()
        stm.Close()
        Dim realB(realByteNum - 1) As Byte
        Array.Copy(by, realB, realByteNum)
        Dim ms As New MemoryStream(realB)
        Return ms
    End Function
    Public Function GetZoom(ByVal lngList As List(Of Double), ByVal latList As List(Of Double)) As Integer
        Dim d As Double = GetMaxDis(lngList, latList)
        If d <= 5 Then Return 15
        If d >= 960 Then Return 3
        If d <= 60 Then Return 14
        If d <= 120 Then Return 13
        If d <= 180 Then Return 12
        If d <= 240 Then Return 11
        If d <= 300 Then Return 11
        If d <= 360 Then Return 11
        If d <= 420 Then Return 11
        If d <= 480 Then Return 11
        If d <= 540 Then Return 10
        If d <= 600 Then Return 9
        If d <= 660 Then Return 8
        If d <= 720 Then Return 7
        If d <= 780 Then Return 6
        If d <= 840 Then Return 5
        If d <= 900 Then Return 4
    End Function
    Public Function GetMaxDis(ByVal lngList As List(Of Double), ByVal latList As List(Of Double)) As Double
        Dim max As Double = 0
        For i = 0 To lngList.Count - 1
            Dim lng1 As Double = lngList(i)
            Dim lat1 As Double = latList(i)
            For j = i + 1 To lngList.Count - 1
                Dim lng2 As Double = lngList(j)
                Dim lat2 As Double = latList(j)
                If i <> j Then
                    Dim dis As Double = GetDistance(lat1, lng1, lat2, lng2)
                    If dis > max Then
                        max = dis
                    End If
                End If
            Next
        Next
        Return max
    End Function
    Private Function rad(ByVal d As Double) As Double
        rad = d * 3.1415926535898 / 180
    End Function
    Private Function GetDistance(ByVal lat1 As Double, ByVal lng1 As Double, ByVal lat2 As Double, ByVal lng2 As Double) As Double
        Dim radlat1 As Double, radlat2 As Double
        Dim a As Double, b As Double, s As Double, Temp As Double
        radlat1 = rad(lat1)
        radlat2 = rad(lat2)
        a = radlat1 - radlat2
        b = rad(lng1) - rad(lng2)
        Temp = Sqrt(Sin(a / 2) ^ 2 + Cos(radlat1) * Cos(radlat2) * Sin(b / 2) ^ 2)
        s = 2 * Atan(Temp / Sqrt(-Temp * Temp + 1))
        s = s * 6378.137
        GetDistance = s

    End Function
    Private Function HandleTaskPOA(ByVal Task As NormalTaskStu) As String
        log("开始制作报告……")
        Dim sb As New StringBuilder
        sb.Append("select * from DeviceMsgTable ")
        Dim str As String = ""
        Dim DeviceList As List(Of String) = JsonConvert.DeserializeObject(Task.DeviceID, GetType(List(Of String)))
        Dim sbTmp As New StringBuilder
        If IsNothing(DeviceList) = False Then
            For Each d In DeviceList
                Dim s As String = "DeviceID='" & d & "'"
                sbTmp.Append(s & " or ")
            Next
        End If
        str = sbTmp.ToString
        str = str.Substring(0, Len(str) - 3)
        sb.Append("where (" & str & ")")
        sb.Append("and Func='{0}' ")
        sb.Append("and MsgTime between '{1}' and '{2}' order by MsgTime")
        Dim para() As String = New String() {"SSSJ", Task.StartTime, Task.EndTime}
        Dim sqlMsg As String = String.Format(sb.ToString, para)
        log(sqlMsg)
        Dim dt As DataTable = SQLGetDT(sqlMsg)
        CheckDir(Directory.GetCurrentDirectory()  & "\Task\Reports\" & myDeviceInfo.DeviceID)
        Dim path As String = Directory.GetCurrentDirectory()  & "\Task\Reports\" & myDeviceInfo.DeviceID & "\" & Task.TaskNickName & "_" &
                                  Date.Parse(Task.StartTime).ToString("yyyyMMddHHmmss") & "_" &
                                  Date.Parse(Task.EndTime).ToString("yyyyMMddHHmmss") & ".docx"
        Dim pdList As New List(Of String)
        pdList.Add(Val(Task.TaskCode))
        Dim PDStr As String = Val(Task.TaskCode)
        Dim doc As DocX = DocX.Create(path)
        doc.InsertParagraph.AppendLine(Task.TaskNickName & "监测报告").Font(New FontFamily("黑体")).FontSize(25)
        doc.InsertParagraph.AppendLine("1.概述").Font(New FontFamily("宋体")).FontSize(20)
        doc.InsertParagraph.AppendLine("     本次监测任务主要监测" & PDStr & ",共计" & pdList.Count & "个频点的POA定位信息").Font(New FontFamily("宋体")).FontSize(15)
        If IsNothing(dt) Then
            log("查询历史设备消息失败或者没有历史设备消息")
            doc.Save()
            Return ""
        Else
            log("查询历史设备消息记录行数=" & dt.Rows.Count)
        End If
        Dim POAInfoList As New List(Of POAFreqInfo)
        For Each r As DataRow In dt.Rows
            Dim time As String = r("MsgTime")
            Dim msg As String = r("DeviceMsg")
            Dim DeviceID As String = r("DeviceID")
            Dim a As Integer = InStr(msg, "<")
            Dim b As Integer = InStr(msg, ">")
            msg = msg.Substring(a, b - a - 1)
            msg = msg.Replace("<", "")
            msg = msg.Replace(">", "")
            msg = msg.Replace("TZBQ:", "")
            Dim st() As String = msg.Split(",")
            Dim SSSJName As String = st(0)
            If SSSJName <> "SSSJ" Then Continue For
            Dim numOfValue As Integer = st(2)
            If st.Length = numOfValue * 2 + 3 Then
                For j = 3 To numOfValue + 2
                    Dim PD As Double = Val(st(j + numOfValue))
                    Dim CQ As Double = Val(st(j))
                    Dim isinmytask As Boolean = False
                    For Each itt In pdList
                        If itt = PD Then
                            isinmytask = True
                            Exit For
                        End If
                    Next
                    If isinmytask = False Then Continue For
                    POAInfoList.Add(New POAFreqInfo(PD, CQ, DeviceID))
                Next
            End If
        Next
        Dim TaskDeviceID As String = Task.DeviceID
        Dim Freq As Double = Val(Task.TaskCode)
        Dim list As List(Of String) = JsonConvert.DeserializeObject(TaskDeviceID, GetType(List(Of String)))
        If IsNothing(list) = False Then

            Dim p As New List(Of POAMaxInfo)
            For Each d In list
                p.Add(New POAMaxInfo(d, Freq, -999))
            Next
            doc.InsertParagraph.AppendLine("2.使用设备信息").Font(New FontFamily("宋体")).FontSize(20)
            Dim tabDeviceID As Table = doc.AddTable(list.Count + 1, 4)
            '序号  设备ID  设备名称  设备类型
            tabDeviceID.Design = TableDesign.TableGrid
            tabDeviceID.Alignment = Alignment.center
            Dim DeviceNamelist As New List(Of String)
            Dim lngList As New List(Of Double)
            Dim latList As New List(Of Double)
            Dim ValueList As New List(Of Double)
            For i = 0 To 3
                tabDeviceID.Rows(0).Cells(i).FillColor = Color.FromArgb(226, 226, 226)
            Next
            tabDeviceID.Rows(0).Cells(0).Paragraphs(0).Append("序号").Bold().FontSize(12)
            tabDeviceID.Rows(0).Cells(1).Paragraphs(0).Append("设备ID").Bold().FontSize(12)
            tabDeviceID.Rows(0).Cells(2).Paragraphs(0).Append("设备名称").Bold().FontSize(12)
            tabDeviceID.Rows(0).Cells(3).Paragraphs(0).Append("设备类型").Bold().FontSize(12)
            If IsNothing(list) = False Then
                Dim DeviceDT As DataTable = SQLGetDT("select * from DeviceTable")
                If IsNothing(DeviceDT) = False Then
                    For i = 0 To p.Count - 1
                        Dim itm As POAMaxInfo = p(i)
                        For Each row As DataRow In DeviceDT.Rows
                            If itm.DeviceID = row("DeviceID") Then
                                itm.DeviceNickName = row("DeviceNickName")
                                p(i) = itm
                                tabDeviceID.Rows(i + 1).Cells(0).Paragraphs(0).Append(i + 1).FontSize(12)
                                tabDeviceID.Rows(i + 1).Cells(1).Paragraphs(0).Append(row("DeviceID")).FontSize(12)
                                tabDeviceID.Rows(i + 1).Cells(2).Paragraphs(0).Append(row("DeviceNickName")).FontSize(12)

                                Dim kind As String = row("Kind")
                                If kind = "TZBQ" Then
                                    tabDeviceID.Rows(i + 1).Cells(3).Paragraphs(0).Append("微型传感器").FontSize(12)
                                End If
                                If kind = "TSS" Then
                                    tabDeviceID.Rows(i + 1).Cells(3).Paragraphs(0).Append("频谱传感器").FontSize(12)
                                End If
                                Exit For
                            End If
                        Next
                    Next
                End If
            End If

            doc.InsertParagraph.InsertTableAfterSelf(tabDeviceID)
            For Each s In POAInfoList
                Dim DeviceID As String = s.DeviceID
                Dim sFreq As Double = s.freq
                Dim sValue As Double = s.cq
                For i = 0 To p.Count - 1
                    Dim itm As POAMaxInfo = p(i)
                    If itm.DeviceID = DeviceID And itm.freq = sFreq Then
                        If sValue > itm.Value Then
                            itm.Value = sValue
                            p(i) = itm
                            Exit For
                        End If
                    End If
                Next
            Next
            For i = 0 To p.Count - 1
                For j = i + 1 To p.Count - 1
                    Dim a As POAMaxInfo = p(i)
                    Dim b As POAMaxInfo = p(j)
                    If b.Value > a.Value Then
                        p(j) = a
                        p(i) = b
                    End If
                Next
            Next
            'doc.InsertParagraph.AppendLine("3.信号直方图").Font(New FontFamily("宋体")).FontSize(20)
            'Dim imgPath_zyd As String = ShiXu2Img_zydFile(list.ToArray, yy_zyd.ToArray)
            'Dim nImg_zyd As Novacode.Image = doc.AddImage(imgPath_zyd)
            'Dim pic_zyd As Novacode.Picture = nImg_zyd.CreatePicture
            'pic_zyd.Width = 600
            'pic_zyd.Height = 200
            'doc.InsertParagraph.AppendPicture(pic_zyd)
            doc.InsertParagraph.AppendLine("3.POA信息表").Font(New FontFamily("宋体")).FontSize(20)
            Dim tab As Table = doc.AddTable(p.Count + 1, 4)
            '序号  频率(MHz)  设备名称  电平最大值(dBm）
            tab.Design = TableDesign.TableGrid
            tab.Alignment = Alignment.center
            For i = 0 To 3
                tab.Rows(0).Cells(i).FillColor = Color.FromArgb(226, 226, 226)
            Next
            tab.Rows(0).Cells(0).Paragraphs(0).Append("序号").Bold().FontSize(12)
            tab.Rows(0).Cells(1).Paragraphs(0).Append("频率(MHz)").Bold().FontSize(12)
            tab.Rows(0).Cells(2).Paragraphs(0).Append("设备名称").Bold().FontSize(12)
            tab.Rows(0).Cells(3).Paragraphs(0).Append("电平最大值(dBm）").Bold().FontSize(12)
            SyncLock DeviceListLock
                For i = 0 To p.Count - 1
                    Dim iInfo As POAMaxInfo = p(i)
                    For Each itm In Module2.DeviceList
                        If iInfo.DeviceID = itm.DeviceID Then
                            DeviceNamelist.Add(itm.Name)
                            lngList.Add(itm.Lng)
                            latList.Add(itm.Lat)
                            ValueList.Add(iInfo.Value)
                            Exit For
                        End If
                    Next
                Next
            End SyncLock
            'log("DeviceNamelist.count=" & DeviceNamelist.Count)
            'log("lngList.count=" & lngList.Count)
            'log("latList.count=" & latList.Count)
            'log("ValueList.count=" & ValueList.Count)
            For i = 0 To p.Count - 1
                Dim itm As POAMaxInfo = p(i)
                tab.Rows(i + 1).Cells(0).Paragraphs(0).Append(i + 1).FontSize(12)
                tab.Rows(i + 1).Cells(1).Paragraphs(0).Append(itm.freq).FontSize(12)
                tab.Rows(i + 1).Cells(2).Paragraphs(0).Append(itm.DeviceNickName).FontSize(12)
                tab.Rows(i + 1).Cells(3).Paragraphs(0).Append(itm.Value).FontSize(12)
            Next
            doc.InsertParagraph.InsertTableAfterSelf(tab)
            doc.InsertParagraph.AppendLine("4.POA地图").Font(New FontFamily("宋体")).FontSize(20)
            Dim ms As MemoryStream = GetPOAMap(DeviceNamelist, lngList, latList, ValueList)
            If IsNothing(ms) = False Then
                Dim nImg As Novacode.Image = doc.AddImage(ms)
                Dim pic As Novacode.Picture = nImg.CreatePicture
                pic.Width = 600
                pic.Height = 500
                doc.InsertParagraph.AppendPicture(pic)
            End If
        End If
        doc.Save()
        doc.Dispose()
        Return path
    End Function
    Structure POAMaxInfo
        Dim DeviceID As String
        Dim DeviceNickName As String
        Dim freq As Double
        Dim Value As Double
        Sub New(ByVal _DeviceID As String, ByVal _freq As Double, ByVal _Value As Double)
            DeviceID = _DeviceID
            ' DeviceNickName = GetDeviceNickNameByID(_DeviceID)
            freq = _freq
            Value = _Value
        End Sub
    End Structure
    Structure POAFreqInfo
        Dim freq As Double
        Dim cq As Double
        Dim DeviceID As String
        Sub New(ByVal _Freq As Double, ByVal _cq As Double, ByVal _DeviceID As Double)
            freq = _Freq
            cq = _cq
            DeviceID = _DeviceID
        End Sub
    End Structure


    Structure JCPD
        Dim msgTime As String
        Dim PD As Double
        Dim CQ As Double
        Sub New(ByVal a As String, ByVal b As Double, ByVal c As Double)
            msgTime = a
            PD = b
            CQ = c
        End Sub
    End Structure


    Dim HeartWatcher As Integer = 0
#Region "socket处理"
    Public Sub serverThreadProc()
        Try
            Dim sb As New SocketAndBuffer
            sb.Socket = myClientSocket
            sb.Socket.BeginReceive(sb.Buffer, 0, sb.Buffer.Length, SocketFlags.None, AddressOf ReceiveCallBack, sb)
        Catch ex As Exception

        End Try
    End Sub
    Dim BQbuffer() As Byte
    Private Sub ReciveBQ(ByVal by() As Byte)
        Try
            'If isLogin = False Then
            '    log("收到TZBQ " & by.Length & "字节 " & tostr(by))
            'End If
            If IsNothing(BQbuffer) Then
                BQbuffer = by
            Else
                BQbuffer = BQbuffer.Concat(by).ToArray
            End If
            If BQbuffer.Length < 7 Then Return
            Dim readIndex As Integer = 0
            While True
                If IsNothing(BQbuffer) Then Exit Sub
                If readIndex >= BQbuffer.Length - 6 Then
                    Return
                End If
                If Encoding.Default.GetString(BQbuffer, readIndex, 6) = "<TZBQ:" Then
                    BQbuffer = tobytes(BQbuffer, readIndex, BQbuffer.Length - readIndex)
                    readIndex = 0
                    For i = readIndex To BQbuffer.Length - 2    'chr(13)  chr(10)
                        Dim k1 As Byte = BQbuffer(i)
                        Dim k2 As Byte = BQbuffer(i + 1)
                        If k1 = 13 And k2 = 10 Then
                            Dim byteSub(i + 3) As Byte
                            Array.Copy(BQbuffer, readIndex, byteSub, 0, i + 2)
                            byteSub(i + 2) = 47
                            byteSub(i + 3) = 48
                            Try
                                HandleBQ(byteSub)
                            Catch ex As Exception
                                log("handleBQ-->Err-->" & ex.ToString)
                            End Try

                            BQbuffer = tobytes(BQbuffer, i + 2, BQbuffer.Length - (i + 2))
                            readIndex = -1
                            Exit For
                        End If
                    Next
                End If
                readIndex = readIndex + 1
            End While
        Catch ex As Exception
            ' 'MsgBox(ex.ToString)
        End Try

    End Sub
    Private Sub ReceiveCallBack(ByVal ar As IAsyncResult)
        Dim sb As SocketAndBuffer
        sb = CType(ar.AsyncState, SocketAndBuffer)
        Dim socket As Socket = sb.Socket
        Try
            If socket.Connected Then
                iLen = socket.EndReceive(ar)
                If iLen > 0 Then
                    ReDim oldbytes(iLen - 1)
                    Array.Copy(sb.Buffer, 0, oldbytes, 0, iLen)
                    sb = New SocketAndBuffer
                    sb.Socket = socket
                    socket.BeginReceive(sb.Buffer, 0, sb.Buffer.Length, SocketFlags.None, AddressOf ReceiveCallBack, sb)
                    ReciveBQ(oldbytes)
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
    Private Sub StartHttp(ByVal cls As DeviceTZBQ)
        Try
            log("TZBQ_" & myDeviceInfo.DeviceID & ",尝试开启HTTP服务……")
            myHttpListener = New HttpListener
            myHttpListener.Prefixes.Add(myHttpUrl)
            myHttpListener.Start()
            log("TZBQ_" & myDeviceInfo.DeviceID & ",已开启HTTP服务")
            log("TZBQ_" & myDeviceInfo.DeviceID & ",尝试开启线程……")
            broadcastThread = New Thread(AddressOf sub_broadcastDeviceMsg)
            broadcastThread.Start()
            log("TZBQ_" & myDeviceInfo.DeviceID & ",已开启线程")
            If IsNothing(DeviceListLock) Then DeviceListLock = New Object
            SyncLock DeviceListLock
                DeviceList.Add(myDeviceInfo)
            End SyncLock
            myHttpListener.BeginGetContext(New AsyncCallback(AddressOf GetContextCallBack), myHttpListener)
        Catch ex As Exception
            log("TZBQ_" & myDeviceInfo.DeviceID & ",初始化HTTP等服务失败，ERR-->" & ex.Message)
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
            Dim mbcm As New MainBroderCastMsgStu(myDeviceInfo.DeviceID, deviceMsg, "TZBQ")
            Dim jsMsg As String = JsonConvert.SerializeObject(mbcm)
            If flag_MzhHandle Then
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
    Private Sub handleHttpRequest(ByVal context As HttpListenerContext)
        Try
            Dim GETSTR As String = UrlDecode(context.Request.Url.PathAndQuery)
            Dim func As String = GetParaValue(GETSTR, "func")
            Dim dataMsg As String = GetParaValue(GETSTR, "datamsg")
            Dim token As String = GetParaValue(GETSTR, "token")

            If func = "GetDevMsg" Then     '获取设备消息
                If IsNothing(HTTPClient) Then HTTPClient = New List(Of HttpListenerContext)
                If IsNothing(HTTPClient_Object) Then HTTPClient_Object = New Object
                SyncLock HTTPClient_Object
                    HTTPClient.Add(context)
                End SyncLock
                Exit Sub
            End If
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
                If myDeviceInfo.DeviceID = "0" Then
                    response(context, "result=fail;msg=不能在0号机基础上更改，0号机是出厂设置新机，请前通过更改设备ID命令修改ID为1 - 99 范围;errMsg=" & "null" & ";advise=")
                    Exit Sub
                End If
                If myDeviceInfo.DeviceID = 0 Then
                    response(context, "result=fail;msg=不能在0号机基础上更改，0号机是出厂设置新机，请前通过更改设备ID命令修改ID为1 - 99 范围;errMsg=" & "null" & ";advise=")
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
            If func = "tzbqOrder" Then
                If isWorking Then
                    AddMyLog("接收一般命令", "失败,设备正在执行任务")
                    response(context, "result=fail;msg=设备正在执行任务;errMsg=" & "null" & ";advise=")
                    Exit Sub
                End If
                Dim msg As String = dataMsg
                Dim resultMsg As String = CutBQ(msg)
                If resultMsg = "" Then
                    response(context, "result=fail;msg=TZBQ string ERR;errMsg=" & "null" & ";advise=Your TZBQ_String Must Contain '<' And '>'")
                Else
                    AddMyLog("接收一般命令", "成功")
                    response(context, SendMsgToTZBQById(resultMsg))
                End If
                Exit Sub
            End If
            If func = "isWorking" Then
                If isWorking Then
                    response(context, "result=fail;msg=true;errMsg=" & "null" & ";advise=")
                Else
                    response(context, "result=success;msg=false;errMsg=" & "null" & ";advise=")
                End If
                Exit Sub
            End If
            If func = "WorkingDetail" Then
                If isWorking Then
                    If IsNothing(myNormalTaskStu) = False Then
                        Dim msg As String = JsonConvert.SerializeObject(myNormalTaskStu)
                        response(context, msg)
                    End If
                Else
                    response(context, "result=fail;msg=设备当前无任务;errMsg=" & "null" & ";advise=")
                End If
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
    Public Function SendMsgToTZBQById(ByVal BQ As String)
        Try
            Dim id As String = myRealID
            Dim ip As String = myDeviceInfo.IP
            Dim result As String = "result=fail;msg=This Device ID is " & id & " disOnline;errMsg=" & "null" & ";advise=null"
            Dim func As String = getFuncByBQ(BQ)
            If func = "JCPD" Then
                If InStr(BQ, "<") Then
                    If InStr(BQ, ">") Then
                        Dim msg As String = BQ.Substring(InStr(BQ, "<"), InStr(BQ, ">") - InStr(BQ, "<") - 1)
                        If InStr(msg, ",") Then
                            Dim st() As String = msg.Split(",")
                            SyncLock myTaskLock
                                myTask.SSSJPd = ""
                                If st.Length >= 11 Then
                                    myTask.SSSJNum = st(9)
                                    For i = 10 To st.Length - 1
                                        myTask.SSSJPd = myTask.SSSJPd & st(i) & ","
                                    Next
                                    If myTask.SSSJPd.Length > 0 Then
                                        myTask.SSSJPd = myTask.SSSJPd.Substring(0, myTask.SSSJPd.Length - 1)
                                    End If
                                End If
                                myTask.myWorkFunc = func
                                myTask.myWorkBQ = BQ
                                'SaveMyTask()
                            End SyncLock
                        End If
                    End If

                End If
            End If
            If func = "PPSJ" Then
                myTask.myWorkFunc = func
                myTask.myWorkBQ = BQ
            End If

            BQ = CutBQ(BQ)
            Dim by() As Byte = tobyte(getcrcstr(BQ))
            result = SendMsgToTZBQDev(by)
            Return result
        Catch ex As Exception

        End Try
    End Function
    Private Sub SaveMyTask()
        Try
            If Directory.Exists(DirPath) = False Then
                Directory.CreateDirectory(DirPath)
            End If
            Dim filePath As String = DirPath & "/" & myDeviceInfo.DeviceID & ".ini"
            Dim taskStr As String = JsonConvert.SerializeObject(myTask)
            Dim SW As New StreamWriter(filePath, False, Encoding.Default)
            SW.Write(taskStr)
            SW.Close()
        Catch ex As Exception

        End Try

    End Sub
    Private Sub ReadMyTask()
        Try
            If Directory.Exists(DirPath) = False Then
                Directory.CreateDirectory(DirPath)
                Exit Sub
            End If
            Dim filePath As String = DirPath & "/" & myDeviceInfo.DeviceID & ".ini"
            If File.Exists(filePath) = False Then Exit Sub
            Dim SW As New StreamReader(filePath, Encoding.Default)
            Dim str As String = SW.ReadToEnd
            SW.Close()
            myTask = JsonConvert.DeserializeObject(str, GetType(myTaskStu))
        Catch ex As Exception

        End Try

    End Sub
    Public Function SendMsgToTZBQDev(ByVal by() As Byte) As String
        Try
            If IsNothing(by) Then
                log("TZBQSer.SendData>ERR>Byte[]为空")
                Return "result=fail;msg=TZBQ string ERR;errMsg=" & "null" & ";advise=Your TZBQ_String Must Contain '<' And '>'"
            End If
            ' log("<发送TZBQ_Dev> id=" & myDeviceInfo.DeviceID & ",msg=" & tostr(by))
            myClientSocket.Send(by)
            'log("Success")
            Return "result=success;msg=success;errMsg=null;advise=good"
        Catch ex As Exception
            Return "result=fail;msg=null;errMsg=" & ex.Message & ";advise=null"
        End Try

    End Function
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
                    SendMsgToTZBQDev(tobyte(getcrcstr("<TZBQ:SBZT,0>")))
                End If
            Catch ex As Exception

            End Try
            Try
                SleepHeartWatch = SleepHeartWatch + 1
                If SleepHeartWatch = 45 Then
                    SleepHeartWatch = 0
                    If HeartWatcher = 0 Then
                        Try
                            log("TZBQ_" & myDeviceInfo.DeviceID & "通信超时，将主动断开Socket……")
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
        AddMyLog("断线(超时)", "")
        Dim oldstr As String = ""
        Dim isOld As Boolean = True
        If isOld Then oldstr = "超时"
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
            broadcastThread.Abort()
            log("关闭" & oldstr & "线程成功")
        Catch ex As Exception
            log("关闭" & oldstr & "线程失败")
        End Try
        Try
            If IsNothing(TaskWorkerThread) = False Then
                TaskWorkerThread.Abort()
                log("关闭" & oldstr & "任务线程成功")
            End If

        Catch ex As Exception
            log("关闭" & oldstr & "任务线程失败" & ex.ToString)
        End Try
        Sleep(500)
        Try

            myClientSocket.Close()
        Catch ex As Exception
            log("断开超时TZBQ_socket,DeviceID=" & myDeviceInfo.DeviceID & ">ERR>" & ex.Message)
        End Try
    End Sub
End Class
