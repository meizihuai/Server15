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
''' <summary>
''' 监测网关服务程序
''' </summary>
Public Class MZHGateWay
    Private workingTask As New NormalTaskStu
    Private legalSigNal As List(Of Double)
    Private isWarnWZ As Boolean = False
    Private WarnInfoList As List(Of WarnInfo)
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
    Public Event RefrushDeviceList()
    Public Event RaiseLog(ByVal mainMsg As String, ByVal UserName As String, ByVal IP As String)
    Dim myTask As myTaskStu
    Dim myTaskLock As Object
    Dim DirPath As String = Directory.GetCurrentDirectory() & "\DeviceTaskIni"
    Public Event RaiseHttplog(ByVal str As String)
    Private CAMListLock As Object
    Private CAMList As List(Of ContextAndMsgID)
    Private Structure ContextAndMsgID
        Dim MsgID As Integer
        Dim Context As HttpListenerContext
        Sub New(_MsgID As Integer, _Context As HttpListenerContext）
            MsgID = _MsgID
            Context = _Context
        End Sub
    End Structure
    Private Sub Httplog(ByVal str As String)
        RaiseEvent RaiseHttplog(str)
    End Sub
    Structure myTaskStu
        Dim myWorkFunc As String
        Dim dataType As String
        Dim funcType As String
        Dim CanShuQu As String
    End Structure
    Public Sub New(ByVal ClientSocket As Socket)
        myClientSocket = ClientSocket
        myTaskLock = New Object
        CAMListLock = New Object
        IP = myClientSocket.RemoteEndPoint.ToString.Split(":")(0)
        Port = myClientSocket.RemoteEndPoint.ToString.Split(":")(1)
        CAMList = New List(Of ContextAndMsgID)
    End Sub
    Public Sub Start()
        serverThreadProc()
        log("ING设备连接，服务器主动发送握手命令……")
        'log(
        'SendMsgToTSSDevByString(&H0, "task", "netswitch", "<netswitch:sw=in;>", Nothing)
        'Sleep(500)
        SendMsgToTSSDevByString(&H0, "DevMsg", "WSZL", "", Nothing)
    End Sub
    Private Sub log(ByVal MainMsg As String)
        'Form1.log(MainMsg, "TSS_Server", "+:" & _Port)
        RaiseEvent RaiseLog(MainMsg, "TSS_" & myDeviceInfo.DeviceID, IP & ":" & Port)
    End Sub
    Private Sub HandleLogin(ByVal id As String)
        'If id = "57G-SM0001" Then
        '    id = "0320160018"
        'End If
        myRealID = id
        Dim NickName As String = id
        Dim DeviceName As String = "ING_" & id
        Dim dbLng As String = ""
        Dim dbLat As String = ""
        Dim myAddress As String = ip2Address(IP)
        Try
            Dim sql As String = String.Format("select * from deviceTable where DeviceID='{0}'", New String() {id})
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
                    insertSql = String.Format(insertSql, New String() {id, id, "ING", Now.ToString("yyyy-MM-dd HH:mm:ss"), IP, Port})
                    SQLCmd(insertSql)
                End If
            Else
                Dim insertSql As String = "insert into deviceTable (deviceid,deviceNickName,kind,OnlineTime,IP,port) values('{0}','{1}','{2}','{3}','{4}','{5}')"
                insertSql = String.Format(insertSql, New String() {id, id, "ING", Now.ToString("yyyy-MM-dd HH:mm:ss"), IP, Port})
                SQLCmd(insertSql)
            End If
        Catch ex As Exception

        End Try
        myDeviceInfo = New deviceStu
        myDeviceInfo.DeviceID = NickName
        myDeviceInfo.Address = myAddress
        myHttpUrl = TSSHttpListenerPath & id & "/"
        myDeviceInfo.HTTPMsgUrl = myHttpUrl
        myDeviceInfo.Kind = "ING"
        myDeviceInfo.OnlineTime = Now.ToString("yyyy-MM-dd HH:mm:ss")
        myDeviceInfo.Statu = "normal"
        myDeviceInfo.cls = Me
        myDeviceInfo.IP = IP
        myDeviceInfo.Port = Port
        myDeviceInfo.Name = "ING_" & id
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
                    log("ING DeviceID=" & NickName & "已登录,正在处理同ID登录，New IP=" & IP & ":" & Port & ",oldIP=" & getIpAndPortByDeviceID(id))
                    Try
                        If itm.IP = IP And itm.Port = Port Then
                            isFind = True
                            log("ING old DeviceID=" & itm.DeviceID & ",重复登录，无需处理" & ",old位置=" & itm.Address & "oldip=(" & itm.IP & ":" & itm.Port & ")")
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
            Dim cls As MZHGateWay = CType(JXXItm.cls, MZHGateWay)
            cls.CloseALL(True)
            AddMyLog("重新上线", "成功")
            log("重新上线ING设备登录，deviceID=" & id)
            StartHttp(cls)
            RaiseEvent RefrushDeviceList()
            isFind = True
        End If
        If isFind = False Then
            AddMyLog("上线", "成功")
            log("ING设备登录，deviceID=" & NickName)
            StartHttp(Nothing)
            RaiseEvent RefrushDeviceList()
            CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & id)
            CheckDir(Directory.GetCurrentDirectory() & "\Task\logs\" & id)
            GetMyTask()
            '    SyncLock myTaskLock
            '        ReadMyTask()
            '        If myTask.myWorkFunc = "bscan" Then
            '            Sleep(2000)
            '            If myTask.dataType <> "" Then
            '                If myTask.funcType <> "" Then
            '                    If myTask.CanShuQu <> "" Then
            '                        SendMsgToTSSDevByString(&H0, myTask.dataType, myTask.funcType, myTask.CanShuQu, Nothing)
            '                    End If
            '                End If
            '            End If
            '        End If
            '    End SyncLock
        End If
    End Sub
    Public Sub GetMyTask()
        Try
            log("获取我的任务……")
            Dim dt As DataTable = SQLGetDT("select * from UserTaskTable Where DeviceID='" & myDeviceInfo.DeviceID & "' and EndTime>'" & Now.ToString("yyyy-MM-dd HH:mm:ss") & "' order by StartTime")
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
                log("有任务，正在执行无需处理")
                AddMyLog("查询到有待执行任务，尝试执行", "失败，因为设备正在执行任务，等待现有任务执行完毕")
            End If
        Catch ex As Exception

        End Try
    End Sub
    Dim isWorking As Boolean = False
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
            Dim gcvalue As String = 0
            Dim returndata As String = "fft"
            Dim returninter As String = 0
            Dim detetor As String = "real"
            CanShuQu = "<bscan: tasknum=1" &
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
        If task.TaskName = "频谱监测" Or task.TaskName = "频谱取样" Or task.TaskName = "占用统计" Or task.TaskName = "黑广播捕获" Or task.TaskName = "违章捕获" Then
            Try
                TssBscanOrder = JsonConvert.DeserializeObject(task.TaskCode, GetType(tssOrder_stu))
                If TssBscanOrder.task = "bscan" Then
                    Dim freqbegin As Double = TssBscanOrder.freqStart
                    Dim freqend As Double = TssBscanOrder.freqEnd
                    Dim freqstep As Double = TssBscanOrder.freqStep
                    Dim ifbw As String = 40000
                    Dim abw As String = freqstep
                    Dim bits As String = 16
                    Dim gcmode As String = "fagc"
                    Dim gcvalue As String = 0
                    Dim returndata As String = "fft"
                    Dim returninter As String = 1
                    Dim detetor As String = "real"
                    CanShuQu = "<bscan: tasknum=1" &
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
                Dim th As New Thread(AddressOf MakeTaskReport)
                th.Start(task)
                Exit While
            End If
            If Now >= task.StartTime Then
                SendMsgToTSSDevByString(&H0, DataType, FuncType, CanShuQu, Nothing)
            End If
            Sleep(SleepInt)
        End While
        isWorking = False
        isTZBQ_JCPD = False
        isWarnWZ = False

        WarnInfoList = Nothing
        'SyncLock DeviceListLock
        '    For i = 0 To DeviceList.Count - 1
        '        Dim itm As deviceStu = DeviceList(i)
        '        If itm.DeviceID = myDeviceInfo.DeviceID Then
        '            itm.Statu = "free"
        '            DeviceList(i) = itm
        '            Exit For
        '        End If
        '    Next
        'End SyncLock
        GetMyTask()
    End Sub
    Private Sub MakeTaskReport(ByVal task As NormalTaskStu)
        Try
            Dim sb As New StringBuilder
            sb.Append("update UserTaskTable Set ")
            sb.Append("OverPercent='99%',")
            sb.Append("ResultReportUrl='正在制作……' ")
            sb.Append("Where TaskNickName='{0}'")
            log(task.DeviceID & "任务完成，正在制作报告，更新数据库状态为正在制作报告……")
            Dim cmdString As String = String.Format(sb.ToString, New String() {task.TaskNickName})
            Dim Result As String = SQLCmd(cmdString)
            log(task.DeviceID & "更新任务数据库返回:" & Result)
            Dim path As String = WriteReport(task)
            sb = New StringBuilder
            sb.Append("update UserTaskTable Set ")
            sb.Append("OverPercent='100%',")
            sb.Append("ResultReportUrl='{0}',")
            sb.Append("ErrMsg='{1}' ")
            sb.Append("Where TaskNickName='{2}'")
            log(task.DeviceID & "更新任务数据库……")
            Dim ReportUrl As String = "http://123.207.31.37:8082/Task/Reports/" & myDeviceInfo.DeviceID & "/" & task.TaskNickName & "_" &
                                      Date.Parse(task.StartTime).ToString("yyyyMMddHHmmss") & "_" &
                                      Date.Parse(task.EndTime).ToString("yyyyMMddHHmmss") & ".docx"

            cmdString = String.Format(sb.ToString, New String() {ReportUrl, "", task.TaskNickName})
            Result = SQLCmd(cmdString)
            log(task.DeviceID & "更新任务数据库返回:" & Result)
            AddJobLog("system", "系统上报", "设备任务完成提示", "[" & task.TaskNickName & "]" & "任务已完成", task.DeviceName, task.DeviceID, task.TaskNickName, task.TaskNickName)
            If path <> "" Then
                log(task.DeviceID & "更新任务数据库返回:" & Result)
                log("报告制作完成")
                log("发送邮件……")
                Dim ht As New Hashtable
                ht.Add(task.TaskNickName & "监测报告.docx", path)
                Dim content As String = "您好，您的监测任务已完成，系统自动推送报告至您的邮箱，请查阅附件，谢谢！"
                log(SendMail(task.PushEmailToUserName, task.TaskNickName & "监测报告", content, ht))
            End If
        Catch ex As Exception

        End Try
    End Sub
    Private Function WriteReport(ByVal task As NormalTaskStu) As String
        Try
            If task.TaskName = "频谱监测" Then
                Return HandleFreqMaxTaskReport(task)
            End If
            If task.TaskName = "频谱取样" Then
                Return HandleSaveFreqStepTaskReport(task)
            End If
            If task.TaskName = "占用统计" Then
                Return HandleZhanYongTongJiTaskReport(task)
            End If
            If task.TaskName = "黑广播捕获" Then
                Return HandleHGBBuHuoTaskReport(task)
            End If
            If task.TaskName = "违章捕获" Then
                Return HandleHGBBuHuoTaskReport(task)
            End If
        Catch ex As Exception
            log("报告制作ERR>>" & ex.Message)
        End Try
        Return ""
    End Function
    Private Function HandleHGBBuHuoTaskReport(ByVal Task As NormalTaskStu) As String '黑广播/违章捕获生成报告
        log("开始制作报告……")
        Dim menXian As Double = -95
        Dim sb As New StringBuilder
        sb.Append("select * from DeviceMsgTable ")
        sb.Append("where DeviceID='{0}' ")
        sb.Append("and Func='{1}' ")
        sb.Append("and MsgTime between '{2}' and '{3}' order by MsgTime")
        Dim para() As String = New String() {myDeviceInfo.DeviceID, "bscan", Task.StartTime, Task.EndTime}
        Dim dt As DataTable = SQLGetDT(String.Format(sb.ToString, para))
        CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceInfo.DeviceID)
        Dim path As String = Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceInfo.DeviceID & "\" & Task.TaskNickName & "_" &
                                  Date.Parse(Task.StartTime).ToString("yyyyMMddHHmmss") & "_" &
                                  Date.Parse(Task.EndTime).ToString("yyyyMMddHHmmss") & ".docx"
        Dim doc As DocX = DocX.Create(path)

        doc.InsertParagraph.AppendLine(Task.TaskNickName & "监测报告").Font(New FontFamily("黑体")).FontSize(25)
        doc.InsertParagraph.AppendLine("1.概述").Font(New FontFamily("宋体")).FontSize(20)
        Try
            Dim code As String = Task.TaskCode
            Dim TssBscanOrder As tssOrder_stu = JsonConvert.DeserializeObject(code, GetType(tssOrder_stu))
            If TssBscanOrder.task = "bscan" Then
                Dim freqStart As Double = TssBscanOrder.freqStart
                Dim freqEnd As Double = TssBscanOrder.freqEnd
                Dim freqStep As Double = (TssBscanOrder.freqStep) / 1000
                ' Dim saveFreqStep As Integer = TssBscanOrder.saveFreqStep
                doc.InsertParagraph.AppendLine("     本次监测任务主要监测[" & freqStart.ToString("0.0000") & "MHz," & freqEnd.ToString("0.0000") & "MHz]频段,在" & Task.StartTime & "到" & Task.EndTime & "期间的信号捕获统计").Font(New FontFamily("宋体")).FontSize(15)
                If IsNothing(dt) Then
                    log("查询历史设备消息失败或者没有历史设备消息")
                    doc.Save()
                    Exit Function
                End If
                doc.InsertParagraph.AppendLine("2.频谱最大值保持图").Font(New FontFamily("宋体")).FontSize(20)
                Dim oldTime As String = ""
                Dim sumCount As Integer = dt.Rows.Count
                Dim BscanList As New List(Of json_PPSJ)
                Dim TimeInt As Integer = Task.TimeStep
                If TimeInt < 5 Then TimeInt = 5
                Dim TimeList As New List(Of String)
                For i = 0 To sumCount - 1
                    Dim row As DataRow = dt.Rows(i)
                    Dim MsgTime As String = row("MsgTime")
                    'Dim time As Date = Date.Parse(MsgTime)
                    'Dim isHandle As Boolean = False
                    'If oldTime = "" Then
                    '    oldTime = time
                    '    isHandle = True
                    'Else
                    '    Dim oTime As Date = Date.Parse(oldTime)
                    '    Dim t As TimeSpan = time - oTime
                    '    If t.Seconds >= TimeInt Then
                    '        oldTime = time
                    '        isHandle = True
                    '    End If
                    'End If
                    'If isHandle = False Then Continue For
                    Dim DeviceMsg As String = row("DeviceMsg")
                    Dim JObj As Object = JObject.Parse(DeviceMsg)
                    Dim func As String = JObj("func").ToString
                    If func = "bscan" Then
                        Dim msg As String = JObj("msg").ToString
                        Dim ppsj As json_PPSJ = JsonConvert.DeserializeObject(msg, GetType(json_PPSJ))
                        If ppsj.freqStart = freqStart Then
                            Dim tmpFreqEnd As Double = freqStart + (ppsj.dataCount - 1) * ppsj.freqStep
                            If tmpFreqEnd = freqEnd Then
                                BscanList.Add(ppsj)
                                TimeList.Add(oldTime)
                            End If
                        End If
                    End If
                Next
                log("BscanList.Count =" & BscanList.Count)
                If BscanList.Count > 0 Then
                    Dim firstBscan As json_PPSJ = BscanList(0)
                    Dim dataCount As Integer = firstBscan.dataCount
                    Dim xx(dataCount - 1) As Double
                    Dim maxYY(dataCount - 1) As Double
                    For i = 0 To dataCount - 1
                        xx(i) = freqStart + i * freqStep
                        If firstBscan.value.Count = dataCount Then
                            maxYY(i) = firstBscan.value(i)
                        End If
                    Next
                    For i = 1 To BscanList.Count - 1
                        Dim itm As json_PPSJ = BscanList(i)
                        If itm.value.Count = dataCount Then
                            For j = 0 To dataCount - 1
                                If maxYY(j) < itm.value(j) Then
                                    maxYY(j) = itm.value(j)
                                End If
                            Next
                        End If
                    Next
                    Dim labelStr As String = "[" & freqStart.ToString("0.0000") & "MHz," & freqEnd.ToString("0.0000") & "MHz]频段,在" & Task.StartTime & "到" & Task.EndTime & "期间最大值保持图"
                    doc.InsertParagraph.AppendLine(" ● " & labelStr).Font(New FontFamily("宋体")).FontSize(12)
                    Dim imgPath As String = ShiXu2ImgFile(xx, maxYY, labelStr, Color.Green)
                    Dim nImg As Novacode.Image = doc.AddImage(imgPath)
                    Dim pic As Novacode.Picture = nImg.CreatePicture
                    pic.Width = 600
                    pic.Height = 200
                    doc.InsertParagraph.AppendPicture(pic)
                    Dim sigNalList As New List(Of sigNalInfo)
                    Dim overThreshol As Double = TssBscanOrder.Threshol
                    If overThreshol >= 0 Then
                        overThreshol = -60
                    End If
                    For Each itm In BscanList
                        Dim myXX() As Double = GetXXByFreqInfo(itm.freqStart, itm.freqStep, itm.dataCount)
                        Dim myYY() As Double = itm.value
                        If myXX.Count = myYY.Count Then
                            Dim du As Integer = 5
                            Dim fucha As Integer = 5
                            Dim sigNal(,) As Double = XinHaoFenLi(myXX, myYY, du, fucha)
                            For i = 0 To sigNal.Length / 2 - 1
                                Dim pinlv As Double = sigNal(i, 0)
                                Dim changqiang As Double = sigNal(i, 1)
                                Dim isIn As Boolean = False
                                For j = 0 To sigNalList.Count - 1
                                    Dim s As sigNalInfo = sigNalList(j)
                                    If s.freq = pinlv Then
                                        isIn = True
                                        If s.max < changqiang Then
                                            s.max = changqiang
                                        End If
                                        If s.min > changqiang Then
                                            s.min = changqiang
                                        End If
                                        If changqiang > overThreshol Then s.OverCount = s.OverCount + 1
                                        s.SumCount = s.SumCount + 1
                                        sigNalList(j) = s
                                        Exit For
                                    End If
                                Next
                                If isIn = False Then
                                    Dim s As sigNalInfo
                                    s.freq = pinlv
                                    s.max = changqiang
                                    s.min = changqiang
                                    If changqiang > overThreshol Then s.OverCount = 1
                                    s.SumCount = 1
                                    sigNalList.Add(s)
                                End If
                            Next
                        End If
                    Next
                    Dim halfCount As Integer = BscanList.Count / 2
                    Dim xx_zyd As New List(Of Single)
                    Dim yy_zyd As New List(Of Single)
                    For j = sigNalList.Count - 1 To 0 Step -1
                        Dim s As sigNalInfo = sigNalList(j)
                        'If s.SumCount < halfCount Or s.OverCount < 10 Then
                        '    sigNalList.RemoveAt(j)
                        'End If
                        If s.OverCount < minWarnNum Then
                            sigNalList.RemoveAt(j)
                        End If
                    Next
                    For i = 0 To sigNalList.Count - 1
                        Dim itm As sigNalInfo = sigNalList(i)
                        xx_zyd.Add(itm.freq.ToString("0.000"))
                        Dim zy As Double = itm.OverCount / itm.SumCount
                        yy_zyd.Add(zy * 100)
                    Next
                    doc.InsertParagraph.AppendLine("3.信号直方图").Font(New FontFamily("宋体")).FontSize(20)
                    Dim imgPath_zyd As String = ShiXu2Img_zydFile(xx_zyd.ToArray, yy_zyd.ToArray)
                    Dim nImg_zyd As Novacode.Image = doc.AddImage(imgPath_zyd)
                    Dim pic_zyd As Novacode.Picture = nImg_zyd.CreatePicture
                    pic_zyd.Width = 600
                    pic_zyd.Height = 200
                    doc.InsertParagraph.AppendPicture(pic_zyd)
                    doc.InsertParagraph.AppendLine("4.信号信息统计表").Font(New FontFamily("宋体")).FontSize(20)
                    Dim tab As Table = doc.AddTable(sigNalList.Count + 1, 8)
                    '序号  频率(MHz)  最大值  最小值  平均值  是否合法  占用度  信号直方图
                    tab.Design = TableDesign.TableGrid
                    tab.Alignment = Alignment.center
                    For i = 0 To 7
                        tab.Rows(0).Cells(i).FillColor = Color.FromArgb(226, 226, 226)
                    Next
                    tab.Rows(0).Cells(0).Paragraphs(0).Append("序号").Bold().FontSize(12)
                    tab.Rows(0).Cells(1).Paragraphs(0).Append("频率(MHz)").Bold().FontSize(12)
                    tab.Rows(0).Cells(2).Paragraphs(0).Append("最大值").Bold().FontSize(12)
                    tab.Rows(0).Cells(3).Paragraphs(0).Append("最小值").Bold().FontSize(12)
                    tab.Rows(0).Cells(4).Paragraphs(0).Append("平均值").Bold().FontSize(12)
                    tab.Rows(0).Cells(5).Paragraphs(0).Append("是否合法").Bold().FontSize(12)
                    tab.Rows(0).Cells(6).Paragraphs(0).Append("占用度").Bold().FontSize(12)
                    tab.Rows(0).Cells(7).Paragraphs(0).Append("信号直方图").Bold().FontSize(12)
                    For i = 0 To sigNalList.Count - 1
                        For j = i + 1 To sigNalList.Count - 1
                            Dim b As sigNalInfo = sigNalList(j)
                            Dim a As sigNalInfo = sigNalList(i)
                            Dim zyda As Double = a.OverCount / a.SumCount
                            Dim zydb As Double = b.OverCount / b.SumCount
                            If zydb > zyda Then
                                sigNalList(i) = b
                                sigNalList(j) = a
                            End If
                        Next
                    Next
                    For i = 0 To sigNalList.Count - 1
                        Dim s As sigNalInfo = sigNalList(i)
                        Dim x As Double = s.freq
                        Dim maxY As Double = s.max
                        Dim minY As Double = s.min
                        Dim overCount As Integer = s.OverCount
                        Dim sum As Integer = s.SumCount
                        Dim avg As Double = (maxY + minY) / 2
                        Dim zy As Double = overCount / sum
                        Dim zyd As String = Format(zy * 100, "0.00") & " %"
                        tab.Rows(i + 1).Cells(0).Paragraphs(0).Append(i + 1).FontSize(12)
                        tab.Rows(i + 1).Cells(1).Paragraphs(0).Append(x.ToString("0.0000")).FontSize(12)
                        tab.Rows(i + 1).Cells(2).Paragraphs(0).Append(maxY.ToString("0.00")).FontSize(12)
                        tab.Rows(i + 1).Cells(3).Paragraphs(0).Append(minY.ToString("0.00")).FontSize(12)
                        tab.Rows(i + 1).Cells(4).Paragraphs(0).Append(avg.ToString("0.00")).FontSize(12)
                        If IsInLegalSignal(x) Then
                            tab.Rows(i + 1).Cells(5).Paragraphs(0).Append("合法").FontSize(12).Color(Color.Blue)
                        Else
                            tab.Rows(i + 1).Cells(5).Paragraphs(0).Append("不合法").FontSize(12).Color(Color.Red)
                        End If
                        tab.Rows(i + 1).Cells(6).Paragraphs(0).Append(Format(zy * 100, "0.00") & " %").FontSize(12)
                        tab.Rows(i + 1).Cells(7).Paragraphs(0).Append(GetPerPic(zy)).FontSize(4)
                    Next
                    doc.InsertParagraph.InsertTableAfterSelf(tab)
                End If
            End If
        Catch ex As Exception
            log("报告制作完成出错：" & ex.Message)
        End Try
        doc.Save()
        doc.Dispose()
        log("报告制作完成")
        Return path
    End Function
    Private Function HandleZhanYongTongJiTaskReport(ByVal Task As NormalTaskStu) As String '占用统计生成报告
        log("开始制作报告……")
        Dim menXian As Double = -95
        Dim sb As New StringBuilder
        sb.Append("select * from DeviceMsgTable ")
        sb.Append("where DeviceID='{0}' ")
        sb.Append("and Func='{1}' ")
        sb.Append("and MsgTime between '{2}' and '{3}' order by MsgTime")
        Dim para() As String = New String() {myDeviceInfo.DeviceID, "bscan", Task.StartTime, Task.EndTime}
        Dim dt As DataTable = SQLGetDT(String.Format(sb.ToString, para))
        CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceInfo.DeviceID)
        Dim path As String = Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceInfo.DeviceID & "\" & Task.TaskNickName & "_" &
                                  Date.Parse(Task.StartTime).ToString("yyyyMMddHHmmss") & "_" &
                                  Date.Parse(Task.EndTime).ToString("yyyyMMddHHmmss") & ".docx"
        Dim doc As DocX = DocX.Create(path)

        doc.InsertParagraph.AppendLine(Task.TaskNickName & "监测报告").Font(New FontFamily("黑体")).FontSize(25)
        doc.InsertParagraph.AppendLine("1.概述").Font(New FontFamily("宋体")).FontSize(20)
        Try
            Dim code As String = Task.TaskCode
            Dim TssBscanOrder As tssOrder_stu = JsonConvert.DeserializeObject(code, GetType(tssOrder_stu))
            If TssBscanOrder.task = "bscan" Then
                Dim freqStart As Double = TssBscanOrder.freqStart
                Dim freqEnd As Double = TssBscanOrder.freqEnd
                Dim freqStep As Double = (TssBscanOrder.freqStep) / 1000
                ' Dim saveFreqStep As Integer = TssBscanOrder.saveFreqStep
                doc.InsertParagraph.AppendLine("     本次监测任务主要监测[" & freqStart.ToString("0.0000") & "MHz," & freqEnd.ToString("0.0000") & "MHz]频段,在" & Task.StartTime & "到" & Task.EndTime & "期间的信号占用统计").Font(New FontFamily("宋体")).FontSize(15)
                If IsNothing(dt) Then
                    log("查询历史设备消息失败或者没有历史设备消息")
                    doc.Save()
                    Exit Function
                End If
                doc.InsertParagraph.AppendLine("2.频谱最大值保持图").Font(New FontFamily("宋体")).FontSize(20)
                Dim oldTime As String = ""
                Dim sumCount As Integer = dt.Rows.Count
                Dim BscanList As New List(Of json_PPSJ)
                Dim TimeInt As Integer = Task.TimeStep
                If TimeInt < 5 Then TimeInt = 5
                Dim TimeList As New List(Of String)
                For i = 0 To sumCount - 1
                    Dim row As DataRow = dt.Rows(i)
                    Dim MsgTime As String = row("MsgTime")
                    'Dim time As Date = Date.Parse(MsgTime)
                    'Dim isHandle As Boolean = False
                    'If oldTime = "" Then
                    '    oldTime = time
                    '    isHandle = True
                    'Else
                    '    Dim oTime As Date = Date.Parse(oldTime)
                    '    Dim t As TimeSpan = time - oTime
                    '    If t.Seconds >= TimeInt Then
                    '        oldTime = time
                    '        isHandle = True
                    '    End If
                    'End If
                    'If isHandle = False Then Continue For
                    Dim DeviceMsg As String = row("DeviceMsg")
                    Dim JObj As Object = JObject.Parse(DeviceMsg)
                    Dim func As String = JObj("func").ToString
                    If func = "bscan" Then
                        Dim msg As String = JObj("msg").ToString
                        Dim ppsj As json_PPSJ = JsonConvert.DeserializeObject(msg, GetType(json_PPSJ))
                        If ppsj.freqStart = freqStart Then
                            Dim tmpFreqEnd As Double = freqStart + (ppsj.dataCount - 1) * ppsj.freqStep
                            If tmpFreqEnd = freqEnd Then
                                BscanList.Add(ppsj)
                                TimeList.Add(oldTime)
                            End If
                        End If
                    End If
                Next
                log("BscanList.Count =" & BscanList.Count)
                If BscanList.Count > 0 Then
                    Dim firstBscan As json_PPSJ = BscanList(0)
                    Dim dataCount As Integer = firstBscan.dataCount
                    Dim xx(dataCount - 1) As Double
                    Dim maxYY(dataCount - 1) As Double
                    For i = 0 To dataCount - 1
                        xx(i) = freqStart + i * freqStep
                        If firstBscan.value.Count = dataCount Then
                            maxYY(i) = firstBscan.value(i)
                        End If
                    Next
                    For i = 1 To BscanList.Count - 1
                        Dim itm As json_PPSJ = BscanList(i)
                        If itm.value.Count = dataCount Then
                            For j = 0 To dataCount - 1
                                If maxYY(j) < itm.value(j) Then
                                    maxYY(j) = itm.value(j)
                                End If
                            Next
                        End If
                    Next
                    Dim labelStr As String = "[" & freqStart.ToString("0.0000") & "MHz," & freqEnd.ToString("0.0000") & "MHz]频段,在" & Task.StartTime & "到" & Task.EndTime & "期间最大值保持图"
                    doc.InsertParagraph.AppendLine(" ● " & labelStr).Font(New FontFamily("宋体")).FontSize(12)
                    Dim imgPath As String = ShiXu2ImgFile(xx, maxYY, labelStr, Color.Green)
                    Dim nImg As Novacode.Image = doc.AddImage(imgPath)
                    Dim pic As Novacode.Picture = nImg.CreatePicture
                    pic.Width = 600
                    pic.Height = 200
                    doc.InsertParagraph.AppendPicture(pic)
                    Dim sigNalList As New List(Of sigNalInfo)
                    Dim overThreshol As Double = TssBscanOrder.Threshol
                    If overThreshol >= 0 Then
                        overThreshol = -60
                    End If
                    For Each itm In BscanList
                        Dim myXX() As Double = GetXXByFreqInfo(itm.freqStart, itm.freqStep, itm.dataCount)
                        Dim myYY() As Double = itm.value
                        If myXX.Count = myYY.Count Then
                            Dim du As Integer = 5
                            Dim fucha As Integer = 5
                            Dim sigNal(,) As Double = XinHaoFenLi(myXX, myYY, du, fucha)
                            For i = 0 To sigNal.Length / 2 - 1
                                Dim pinlv As Double = sigNal(i, 0)
                                Dim changqiang As Double = sigNal(i, 1)
                                Dim isIn As Boolean = False
                                For j = 0 To sigNalList.Count - 1
                                    Dim s As sigNalInfo = sigNalList(j)
                                    If s.freq = pinlv Then
                                        isIn = True
                                        If s.max < changqiang Then
                                            s.max = changqiang
                                        End If
                                        If s.min > changqiang Then
                                            s.min = changqiang
                                        End If
                                        If changqiang > overThreshol Then s.OverCount = s.OverCount + 1
                                        s.SumCount = s.SumCount + 1
                                        sigNalList(j) = s
                                        Exit For
                                    End If
                                Next
                                If isIn = False Then
                                    Dim s As sigNalInfo
                                    s.freq = pinlv
                                    s.max = changqiang
                                    s.min = changqiang
                                    If changqiang > overThreshol Then s.OverCount = 1
                                    s.SumCount = 1
                                    sigNalList.Add(s)
                                End If
                            Next
                        End If
                    Next
                    Dim halfCount As Integer = BscanList.Count / 2
                    Dim xx_zyd As New List(Of Single)
                    Dim yy_zyd As New List(Of Single)
                    For j = sigNalList.Count - 1 To 0 Step -1
                        Dim s As sigNalInfo = sigNalList(j)
                        'If s.SumCount < halfCount Or s.OverCount < 10 Then
                        '    sigNalList.RemoveAt(j)
                        'End If
                        If s.OverCount < 10 Then
                            sigNalList.RemoveAt(j)
                        End If
                    Next
                    For i = 0 To sigNalList.Count - 1
                        Dim itm As sigNalInfo = sigNalList(i)
                        xx_zyd.Add(itm.freq.ToString("0.000"))
                        Dim zy As Double = itm.OverCount / itm.SumCount
                        yy_zyd.Add(zy * 100)
                    Next
                    doc.InsertParagraph.AppendLine("3.信号直方图").Font(New FontFamily("宋体")).FontSize(20)
                    Dim imgPath_zyd As String = ShiXu2Img_zydFile(xx_zyd.ToArray, yy_zyd.ToArray)
                    Dim nImg_zyd As Novacode.Image = doc.AddImage(imgPath_zyd)
                    Dim pic_zyd As Novacode.Picture = nImg_zyd.CreatePicture
                    pic_zyd.Width = 600
                    pic_zyd.Height = 200
                    doc.InsertParagraph.AppendPicture(pic_zyd)
                    doc.InsertParagraph.AppendLine("4.信号信息统计表").Font(New FontFamily("宋体")).FontSize(20)
                    Dim tab As Table = doc.AddTable(sigNalList.Count + 1, 7)
                    '序号  频率(MHz)  是否可用  最大值  最小值  平均值  信号直方图
                    tab.Design = TableDesign.TableGrid
                    tab.Alignment = Alignment.center
                    For i = 0 To 6
                        tab.Rows(0).Cells(i).FillColor = Color.FromArgb(226, 226, 226)
                    Next
                    tab.Rows(0).Cells(0).Paragraphs(0).Append("序号").Bold().FontSize(12)
                    tab.Rows(0).Cells(1).Paragraphs(0).Append("频率(MHz)").Bold().FontSize(12)
                    tab.Rows(0).Cells(2).Paragraphs(0).Append("最大值").Bold().FontSize(12)
                    tab.Rows(0).Cells(3).Paragraphs(0).Append("最小值").Bold().FontSize(12)
                    tab.Rows(0).Cells(4).Paragraphs(0).Append("平均值").Bold().FontSize(12)
                    tab.Rows(0).Cells(5).Paragraphs(0).Append("占用度").Bold().FontSize(12)
                    tab.Rows(0).Cells(6).Paragraphs(0).Append("信号直方图").Bold().FontSize(12)
                    For i = 0 To sigNalList.Count - 1
                        For j = i + 1 To sigNalList.Count - 1
                            Dim b As sigNalInfo = sigNalList(j)
                            Dim a As sigNalInfo = sigNalList(i)
                            Dim zyda As Double = a.OverCount / a.SumCount
                            Dim zydb As Double = b.OverCount / b.SumCount
                            If zydb > zyda Then
                                sigNalList(i) = b
                                sigNalList(j) = a
                            End If
                        Next
                    Next
                    For i = 0 To sigNalList.Count - 1
                        Dim s As sigNalInfo = sigNalList(i)
                        Dim x As Double = s.freq
                        Dim maxY As Double = s.max
                        Dim minY As Double = s.min
                        Dim overCount As Integer = s.OverCount
                        Dim sum As Integer = s.SumCount
                        Dim avg As Double = (maxY + minY) / 2
                        Dim zy As Double = overCount / sum
                        Dim zyd As String = Format(zy * 100, "0.00") & " %"
                        tab.Rows(i + 1).Cells(0).Paragraphs(0).Append(i + 1).FontSize(12)
                        tab.Rows(i + 1).Cells(1).Paragraphs(0).Append(x.ToString("0.0000")).FontSize(12)
                        tab.Rows(i + 1).Cells(2).Paragraphs(0).Append(maxY.ToString("0.00")).FontSize(12)
                        tab.Rows(i + 1).Cells(3).Paragraphs(0).Append(minY.ToString("0.00")).FontSize(12)
                        tab.Rows(i + 1).Cells(4).Paragraphs(0).Append(avg.ToString("0.00")).FontSize(12)
                        tab.Rows(i + 1).Cells(5).Paragraphs(0).Append(Format(zy * 100, "0.00") & " %").FontSize(12)
                        tab.Rows(i + 1).Cells(6).Paragraphs(0).Append(GetPerPic(zy)).FontSize(4)
                    Next
                    doc.InsertParagraph.InsertTableAfterSelf(tab)
                End If
            End If
        Catch ex As Exception
            log("报告制作完成出错：" & ex.Message)
        End Try
        doc.Save()
        doc.Dispose()
        log("报告制作完成")
        Return path
    End Function
    Private Function ShiXu2Img_zydFile(ByVal xx() As Single, ByVal yy() As Single) As String
        Dim chart As New System.Windows.Forms.DataVisualization.Charting.Chart
        chart.Width = 1000
        chart.Height = 300
        chart.BackColor = Color.White
        chart.ChartAreas.Add("时序")
        chart.ChartAreas(0).AxisY.Maximum = 100
        chart.ChartAreas(0).AxisY.Minimum = 0
        chart.ChartAreas(0).AxisX.LineColor = Color.Gray
        chart.ChartAreas(0).AxisY.LineColor = Color.Gray

        chart.ChartAreas(0).AxisX.MajorGrid.LineColor = Color.Gray
        chart.ChartAreas(0).AxisY.MajorGrid.LineColor = Color.Gray

        chart.ChartAreas(0).AxisX.LabelStyle.ForeColor = Color.Black
        chart.ChartAreas(0).AxisY.LabelStyle.ForeColor = Color.Black

        chart.ChartAreas(0).BorderColor = Color.White
        chart.ChartAreas(0).BackColor = Color.White
        chart.ChartAreas(0).AxisX.LabelStyle.IsStaggered = False

        Dim ser As New System.Windows.Forms.DataVisualization.Charting.Series("时序")
        ser.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Column
        ser("PixelPointWidth") = 20
        For i = 0 To yy.Count - 1
            ser.Points.AddXY(xx(i), yy(i))
        Next
        ser.Color = Color.Blue
        ser.IsVisibleInLegend = False
        chart.Series.Clear()
        chart.Series.Add(ser)
        Dim rand As New Random
        Dim int As Integer = rand.Next(100, 999)
        Dim timeFileName As String = Now.ToString("yyyyMMddHHmmss") & int
        Dim tmpPath As String = Directory.GetCurrentDirectory() & "\Taskimages\" & timeFileName & "_old.jpg"
        If File.Exists(tmpPath) Then File.Delete(tmpPath)
        chart.SaveImage(tmpPath, Imaging.ImageFormat.Jpeg)
        Dim img As System.Drawing.Image = System.Drawing.Image.FromFile(tmpPath)
        Dim bmp As New Bitmap(img)
        img.Dispose()
        File.Delete(tmpPath)
        'Dim g As Graphics = Graphics.FromImage(img)
        'g.DrawString(labelStr, New Font("宋体", 12), Brushes.Red, 100, 20)
        'g.Save()
        Dim path As String = Directory.GetCurrentDirectory() & "\Taskimages\" & timeFileName & "_new.jpg"
        If File.Exists(path) Then File.Delete(path)
        bmp.Save(path)
        Return path
    End Function
    Private Function GetPerPic(ByVal v As Double) As String
        If v >= 1 Then v = 1
        If v <= 0 Then v = 0
        If v = 0 Then Return ""
        Dim fk As String = "■"
        Dim value As Integer = v * 100
        Dim result As String = ""
        For i = 1 To value Step 10
            result = result + fk
        Next
        Return result
    End Function
    Structure sigNalInfo
        Dim freq As Double
        Dim max As Double
        Dim min As Double
        Dim OverCount As Integer
        Dim SumCount As Integer
    End Structure
    Public Function GetXXByFreqInfo(ByVal freqStart As Double, ByVal freqStep As Double, ByVal dataCount As Double) As Double()
        Dim xx(dataCount - 1) As Double
        For i = 0 To dataCount - 1
            xx(i) = freqStart + i * freqStep
        Next
        Return xx
    End Function
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
    Private Function HandleSaveFreqStepTaskReport(ByVal Task As NormalTaskStu) As String '频谱取样生成报告
        log("开始制作报告……")
        Dim menXian As Double = -95
        Dim sb As New StringBuilder
        sb.Append("select * from DeviceMsgTable ")
        sb.Append("where DeviceID='{0}' ")
        sb.Append("and Func='{1}' ")
        sb.Append("and MsgTime between '{2}' and '{3}' order by MsgTime")
        Dim para() As String = New String() {myDeviceInfo.DeviceID, "bscan", Task.StartTime, Task.EndTime}
        Dim dt As DataTable = SQLGetDT(String.Format(sb.ToString, para))
        CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceInfo.DeviceID)
        Dim path As String = Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceInfo.DeviceID & "\" & Task.TaskNickName & "_" &
                                  Date.Parse(Task.StartTime).ToString("yyyyMMddHHmmss") & "_" &
                                  Date.Parse(Task.EndTime).ToString("yyyyMMddHHmmss") & ".docx"
        Dim doc As DocX = DocX.Create(path)

        doc.InsertParagraph.AppendLine(Task.TaskNickName & "监测报告").Font(New FontFamily("黑体")).FontSize(25)
        doc.InsertParagraph.AppendLine("1.概述").Font(New FontFamily("宋体")).FontSize(20)
        Try
            Dim code As String = Task.TaskCode
            Dim TssBscanOrder As tssOrder_stu = JsonConvert.DeserializeObject(code, GetType(tssOrder_stu))
            If TssBscanOrder.task = "bscan" Then
                Dim freqStart As Double = TssBscanOrder.freqStart
                Dim freqEnd As Double = TssBscanOrder.freqEnd
                Dim freqStep As Double = (TssBscanOrder.freqStep) / 1000
                Dim saveFreqStep As Integer = TssBscanOrder.saveFreqStep
                doc.InsertParagraph.AppendLine("     本次监测任务主要监测[" & freqStart.ToString("0.0000") & "MHz," & freqEnd.ToString("0.0000") & "MHz]频段,在" & Task.StartTime & "到" & Task.EndTime & "期间的频谱取样").Font(New FontFamily("宋体")).FontSize(15)
                If IsNothing(dt) Then
                    log("查询历史设备消息失败或者没有历史设备消息")
                    doc.Save()
                    Exit Function
                End If
                doc.InsertParagraph.AppendLine("2.最大值保持图").Font(New FontFamily("宋体")).FontSize(20)
                Dim oldTime As String = ""
                Dim sumCount As Integer = dt.Rows.Count
                Dim BscanList As New List(Of json_PPSJ)
                Dim TimeInt As Integer = saveFreqStep
                Dim TimeList As New List(Of String)
                For i = 0 To sumCount - 1
                    Dim row As DataRow = dt.Rows(i)
                    Dim MsgTime As String = row("MsgTime")
                    Dim time As Date = Date.Parse(MsgTime)
                    Dim isHandle As Boolean = False
                    If oldTime = "" Then
                        oldTime = time
                        isHandle = True
                    Else
                        Dim oTime As Date = Date.Parse(oldTime)
                        Dim t As TimeSpan = time - oTime
                        If t.Seconds >= TimeInt Then
                            oldTime = time
                            isHandle = True
                        End If
                    End If
                    If isHandle = False Then Continue For
                    Dim DeviceMsg As String = row("DeviceMsg")
                    Dim JObj As Object = JObject.Parse(DeviceMsg)
                    Dim func As String = JObj("func").ToString
                    If func = "bscan" Then
                        Dim msg As String = JObj("msg").ToString
                        Dim ppsj As json_PPSJ = JsonConvert.DeserializeObject(msg, GetType(json_PPSJ))
                        If ppsj.freqStart = freqStart Then
                            Dim tmpFreqEnd As Double = freqStart + (ppsj.dataCount - 1) * ppsj.freqStep
                            If tmpFreqEnd = freqEnd Then
                                BscanList.Add(ppsj)
                                TimeList.Add(oldTime)
                            End If
                        End If
                    End If
                Next
                log("BscanList.Count =" & BscanList.Count)
                If BscanList.Count > 0 Then
                    Dim firstBscan As json_PPSJ = BscanList(0)
                    Dim dataCount As Integer = firstBscan.dataCount
                    Dim xx(dataCount - 1) As Double
                    Dim maxYY(dataCount - 1) As Double
                    For i = 0 To dataCount - 1
                        xx(i) = freqStart + i * freqStep
                        If firstBscan.value.Count = dataCount Then
                            maxYY(i) = firstBscan.value(i)
                        End If
                    Next
                    For i = 1 To BscanList.Count - 1
                        Dim itm As json_PPSJ = BscanList(i)
                        If itm.value.Count = dataCount Then
                            For j = 0 To dataCount - 1
                                If maxYY(j) < itm.value(j) Then
                                    maxYY(j) = itm.value(j)
                                End If
                            Next
                        End If
                    Next
                    Dim labelStr As String = "[" & freqStart.ToString("0.0000") & "MHz," & freqEnd.ToString("0.0000") & "MHz]频段,在" & Task.StartTime & "到" & Task.EndTime & "期间最大值保持图"
                    doc.InsertParagraph.AppendLine(" ● " & labelStr).Font(New FontFamily("宋体")).FontSize(12)
                    Dim imgPath As String = ShiXu2ImgFile(xx, maxYY, labelStr, Color.Green)
                    Dim nImg As Novacode.Image = doc.AddImage(imgPath)
                    Dim pic As Novacode.Picture = nImg.CreatePicture
                    pic.Width = 600
                    pic.Height = 200
                    doc.InsertParagraph.AppendPicture(pic)
                End If
                doc.InsertParagraph.AppendLine("3.各时间频谱实时图").Font(New FontFamily("宋体")).FontSize(20)
                For i = 0 To BscanList.Count - 1
                    Dim Bscan As json_PPSJ = BscanList(i)
                    Dim dataCount As Integer = Bscan.dataCount
                    Dim xx(dataCount - 1) As Double
                    Dim yy(dataCount - 1) As Double
                    For j = 0 To dataCount - 1
                        xx(j) = freqStart + j * freqStep
                        If Bscan.value.Count = dataCount Then
                            yy(j) = Bscan.value(j)
                        End If
                    Next
                    Dim TimeString As String = TimeList(i)
                    Dim labelStr As String = "[" & freqStart.ToString("0.0000") & "MHz," & freqEnd.ToString("0.0000") & "MHz]频段,在" & TimeString & "的实时频谱图"
                    doc.InsertParagraph.AppendLine(" ● " & labelStr).Font(New FontFamily("宋体")).FontSize(12)
                    Dim imgPath As String = ShiXu2ImgFile(xx, yy, labelStr, Color.Blue)
                    Dim nImg As Novacode.Image = doc.AddImage(imgPath)
                    Dim pic As Novacode.Picture = nImg.CreatePicture
                    pic.Width = 600
                    pic.Height = 200
                    doc.InsertParagraph.AppendPicture(pic)
                Next
            End If
        Catch ex As Exception
            log("报告制作完成出错：" & ex.Message)
        End Try
        doc.Save()
        doc.Dispose()
        log("报告制作完成")
        Return path
    End Function
    Private Function HandleFreqMaxTaskReport(ByVal Task As NormalTaskStu) As String '频谱监测生成报告
        log("开始制作报告……")
        Dim menXian As Double = -95
        Dim sb As New StringBuilder
        sb.Append("select * from DeviceMsgTable ")
        sb.Append("where DeviceID='{0}' ")
        sb.Append("and Func='{1}' ")
        sb.Append("and MsgTime between '{2}' and '{3}' order by MsgTime")
        Dim para() As String = New String() {myDeviceInfo.DeviceID, "bscan", Task.StartTime, Task.EndTime}
        Dim dt As DataTable = SQLGetDT(String.Format(sb.ToString, para))
        CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceInfo.DeviceID)
        Dim path As String = Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceInfo.DeviceID & "\" & Task.TaskNickName & "_" &
                                  Date.Parse(Task.StartTime).ToString("yyyyMMddHHmmss") & "_" &
                                  Date.Parse(Task.EndTime).ToString("yyyyMMddHHmmss") & ".docx"
        Dim doc As DocX = DocX.Create(path)
        doc.InsertParagraph.AppendLine(Task.TaskNickName & "监测报告").Font(New FontFamily("黑体")).FontSize(25)
        doc.InsertParagraph.AppendLine("1.概述").Font(New FontFamily("宋体")).FontSize(20)
        Try
            Dim code As String = Task.TaskCode
            Dim TssBscanOrder As tssOrder_stu = JsonConvert.DeserializeObject(code, GetType(tssOrder_stu))
            If TssBscanOrder.task = "bscan" Then
                Dim freqStart As Double = TssBscanOrder.freqStart
                Dim freqEnd As Double = TssBscanOrder.freqEnd
                Dim freqStep As Double = (TssBscanOrder.freqStep) / 1000
                doc.InsertParagraph.AppendLine("     本次监测任务主要监测[" & freqStart.ToString("0.0000") & "MHz," & freqEnd.ToString("0.0000") & "MHz]频段,在" & Task.StartTime & "到" & Task.EndTime & "期间最大值保持等信息").Font(New FontFamily("宋体")).FontSize(15)
                If IsNothing(dt) Then
                    log("查询历史设备消息失败或者没有历史设备消息")
                    doc.Save()
                    Exit Function
                End If
                doc.InsertParagraph.AppendLine("2.最大值保持图").Font(New FontFamily("宋体")).FontSize(20)
                Dim oldTime As String = ""
                Dim sumCount As Integer = dt.Rows.Count
                Dim BscanList As New List(Of json_PPSJ)
                Dim TimeInt As Integer = Val(Task.TimeStep)
                If TimeInt < 5 Then TimeInt = 5
                For i = 0 To sumCount - 1
                    Dim row As DataRow = dt.Rows(i)
                    Dim MsgTime As String = row("MsgTime")
                    Dim time As Date = Date.Parse(MsgTime)
                    Dim isHandle As Boolean = False
                    If oldTime = "" Then
                        oldTime = time
                        isHandle = True
                    Else
                        Dim oTime As Date = Date.Parse(oldTime)
                        Dim t As TimeSpan = time - oTime
                        If t.Seconds >= TimeInt Then
                            oldTime = time
                            isHandle = True
                        End If
                    End If
                    If isHandle = False Then Continue For
                    Dim DeviceMsg As String = row("DeviceMsg")
                    Dim JObj As Object = JObject.Parse(DeviceMsg)
                    Dim func As String = JObj("func").ToString
                    If func = "bscan" Then
                        Dim msg As String = JObj("msg").ToString
                        Dim ppsj As json_PPSJ = JsonConvert.DeserializeObject(msg, GetType(json_PPSJ))
                        If ppsj.freqStart = freqStart Then
                            BscanList.Add(ppsj)
                            Dim tmpFreqEnd As Double = freqStart + (ppsj.dataCount - 1) * ppsj.freqStep
                            If tmpFreqEnd = freqEnd Then
                                BscanList.Add(ppsj)
                            End If
                        End If
                    End If
                Next
                log("BscanList.Count =" & BscanList.Count)
                If BscanList.Count > 0 Then
                    Dim firstBscan As json_PPSJ = BscanList(0)
                    Dim dataCount As Integer = firstBscan.dataCount
                    Dim xx(dataCount - 1) As Double
                    Dim maxYY(dataCount - 1) As Double
                    For i = 0 To dataCount - 1
                        xx(i) = freqStart + i * freqStep
                        If firstBscan.value.Count = dataCount Then
                            maxYY(i) = firstBscan.value(i)
                        End If
                    Next
                    For i = 1 To BscanList.Count - 1
                        Dim itm As json_PPSJ = BscanList(i)
                        If itm.value.Count = dataCount Then
                            For j = 0 To dataCount - 1
                                If maxYY(j) < itm.value(j) Then
                                    maxYY(j) = itm.value(j)
                                End If
                            Next
                        End If
                    Next
                    Dim labelStr As String = "[" & freqStart.ToString("0.0000") & "MHz," & freqEnd.ToString("0.0000") & "MHz]频段,在" & Task.StartTime & "到" & Task.EndTime & "期间最大值保持图"
                    doc.InsertParagraph.AppendLine(" ● " & labelStr).Font(New FontFamily("宋体")).FontSize(12)
                    Dim imgPath As String = ShiXu2ImgFile(xx, maxYY, labelStr, Color.Green)
                    Dim nImg As Novacode.Image = doc.AddImage(imgPath)
                    Dim pic As Novacode.Picture = nImg.CreatePicture
                    pic.Width = 600
                    pic.Height = 200
                    doc.InsertParagraph.AppendPicture(pic)
                End If
            End If
        Catch ex As Exception

        End Try
        doc.Save()
        doc.Dispose()
        log("报告制作完成")
        Return path
    End Function
    Private Function ShiXu2ImgFile(ByVal xx() As Double, ByVal yy() As Double, ByVal labelStr As String, ByVal Seriescolor As Color) As String

        Dim chart As New System.Windows.Forms.DataVisualization.Charting.Chart
        chart.Width = 1000
        chart.Height = 300
        chart.BackColor = Color.White
        chart.ChartAreas.Add("最大值保持")
        chart.ChartAreas(0).AxisY.Maximum = -30
        chart.ChartAreas(0).AxisY.Minimum = -150
        chart.ChartAreas(0).AxisX.LineColor = Color.Gray
        chart.ChartAreas(0).AxisY.LineColor = Color.Gray

        chart.ChartAreas(0).AxisX.MajorGrid.LineColor = Color.Gray
        chart.ChartAreas(0).AxisY.MajorGrid.LineColor = Color.Gray

        chart.ChartAreas(0).AxisX.LabelStyle.ForeColor = Color.Black
        chart.ChartAreas(0).AxisY.LabelStyle.ForeColor = Color.Black

        chart.ChartAreas(0).BorderColor = Color.White
        chart.ChartAreas(0).BackColor = Color.White
        chart.ChartAreas(0).AxisX.LabelStyle.IsStaggered = False
        chart.ChartAreas(0).AxisY.Interval = 30
        chart.ChartAreas(0).AxisY.IntervalOffset = 30
        Dim ser As New System.Windows.Forms.DataVisualization.Charting.Series("时序")
        ser.ChartType = Charting.SeriesChartType.FastLine
        ser("PointWidth") = 0.5
        For i = 0 To yy.Count - 1
            ser.Points.AddXY(xx(i), yy(i))
        Next
        ser.Color = Seriescolor
        ser.IsVisibleInLegend = False
        chart.Series.Clear()
        chart.Series.Add(ser)
        'Dim ser2 As New System.Windows.Forms.DataVisualization.Charting.Series("门限")
        'ser2.ChartType = Charting.SeriesChartType.FastLine
        'ser2("PointWidth") = 0.5
        'For i = 0 To yy.Count - 1
        '    ser2.Points.AddXY(xx(i), menxian)
        'Next
        'ser2.Color = Color.FromArgb(255, 0, 0)
        'ser2.IsVisibleInLegend = False
        'chart.Series.Add(ser2)


        Dim timeFileName As String = Now.ToString("yyyyMMddHHmmss") & "Bscan"
        Dim tmpPath As String = Directory.GetCurrentDirectory() & "\Taskimages\" & timeFileName & "_old.jpg"
        If File.Exists(tmpPath) Then File.Delete(tmpPath)
        chart.SaveImage(tmpPath, Imaging.ImageFormat.Jpeg)
        Dim img As System.Drawing.Image = System.Drawing.Image.FromFile(tmpPath)
        Dim bmp As New Bitmap(img)
        img.Dispose()
        File.Delete(tmpPath)
        Dim g As Graphics = Graphics.FromImage(bmp)
        g.DrawString(labelStr, New Font("宋体", 12), Brushes.Red, 100, 20)
        g.Save()
        Dim path As String = Directory.GetCurrentDirectory() & "\Taskimages\" & timeFileName & "_new.jpg"
        If File.Exists(path) Then File.Delete(path)
        bmp.Save(path)
        Return path
    End Function
    Private Sub TaskLog(ByVal str As String)
        'Dim sr As New StreamWriter( Directory.GetCurrentDirectory()  & "\Task\TaskLog\" & myDeviceInfo.DeviceID &
    End Sub
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
            AddMyLog("断线_连接关闭", "")
            myClientSocket.Close()
        Catch ex As Exception
            log("断开前者ING_Socket,DeviceID=" & myDeviceInfo.DeviceID & ">ERR>" & ex.Message)
        End Try
    End Sub
    Public Sub CloseALL(ByVal isOld As Boolean)
        log("ING_CloseALL-->[" & IP & ":" & Port & "]")
        AddMyLog("断线", "")
        Dim oldstr As String = ""
        If isOld Then oldstr = "前者"
        Try
            If IsNothing(broadcastThread) = False Then
                broadcastThread.Abort()
                ' log("关闭" & oldstr & "线程成功")
            End If
        Catch ex As Exception
            ' log("关闭" & oldstr & "线程失败" & ex.ToString)
        End Try
        Try
            If IsNothing(TaskWorkerThread) = False Then
                TaskWorkerThread.Abort()
                ' log("关闭" & oldstr & "线程成功")
            End If
        Catch ex As Exception
            ' log("关闭" & oldstr & "线程失败" & ex.ToString)
        End Try
        Try
            If IsNothing(myHttpListener) = False Then
                myHttpListener.Abort()
                'log("关闭" & oldstr & "myHttpListener成功")
            End If
        Catch ex As Exception
            'log("关闭" & oldstr & "myHttpListener失败" & ex.ToString)
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
            log("准备断开前者ING_Socket,DeviceID=" & myDeviceInfo.DeviceID)
            ' Threading.Thread.Sleep(10000)
            htp.Close()
            log("已断开前者ING_Socket,DeviceID=" & myDeviceInfo.DeviceID)
        Catch ex As Exception
            log("断开前者ING_Socket,DeviceID=" & myDeviceInfo.DeviceID & ">ERR>" & ex.Message)
        End Try
    End Sub
    ''' <summary>
    ''' 处理ING设备发过来的消息
    ''' </summary>
    ''' <param name="by"></param>
    Private Sub handleTm(ByVal by() As Byte)  '处理设备发过来的TM消息
        HeartWatcher = 1
        Dim tm As tssMsg = byte2tssmsg(by)
        Dim base As String = Convert.ToBase64String(by)
        Dim id As String = tm.deviceID
        ' log("<接收TSS_Dev> id=" & id & ",datatype=" & tm.datatype & "," & "functype=" & tm.functype)
        If tm.datatype = "ACK" Then
            Dim msg As String = tm.canshuqu
            If InStr(msg, "cmd=WSZL") Then

            End If
        End If
        If tm.datatype = "link" Then
            If tm.functype = "logon" Then
                HandleLogin(id)
                Sleep(1000)
                SendMsgToTSSDevByString(&H0, "task", "getheartbeat", "", Nothing)
            End If
        End If

        If tm.datatype = "DevData" Then
            If tm.functype = "State" Then
                If isWorking = False Then
                    log("收到" & myDeviceInfo.DeviceID & "上报设备状态")
                    handleDeviceState(tm, IP, Port)
                End If
            End If
        End If
        If tm.datatype = "spect_conti" Then
            If tm.functype = "bscan" Then
                handlePinPuFenXi(tm.deviceID, tm.shujuqu)
            End If
        End If
        If tm.datatype = "audio" Then
            If tm.functype = "ifscan_wav" Then
                HandleAudio(tm.shujuqu)
            End If
        End If
        If tm.datatype.ToLower() = "data" Then
            If tm.functype.ToLower() = "heartbeat" Then
                ' log("收到网关设备心跳包" & myDeviceInfo.DeviceID & "--->" & tm.canshuqu)
            End If
        End If
        If tm.datatype.ToLower() = "data" Then
            If tm.functype.ToLower() = "ingmsg" Then
                'log("收到网关设备INGMsg消息   " & JsonConvert.SerializeObject（）)
                HandleINGMsg(TSSShuju2INGMsg(tm))
            End If
        End If
        If tm.datatype.ToLower = "data" Then
            If tm.functype.ToLower() = "devicemsg" Then
                Dim msg As String = Encoding.Default.GetString(tm.shujuqu)
                AddDeviceMsg(msg)
                'Console.WriteLine(msg)
            End If
        End If
    End Sub

    Private Sub HandleINGMsg(INGMsg As INGMsgStu) '设备回复过来的  INGMsg
        If IsNothing(INGMsg) Then Return
        Dim MsgID As Integer = INGMsg.MsgID
        Dim MsgTime As String = INGMsg.MsgTime
        Dim func As String = INGMsg.func
        Dim msg As String = INGMsg.msg
        Dim context As HttpListenerContext = Nothing
        Dim isFind As Boolean = False
        SyncLock CAMListLock
            If IsNothing(CAMList) = False Then
                Dim index As Integer = 0
                For i = 0 To CAMList.Count - 1
                    Dim itm As ContextAndMsgID = CAMList(i)
                    If itm.MsgID = MsgID Then
                        context = itm.Context
                        isFind = True
                        index = i
                        Exit For
                    End If
                Next
                If isFind Then
                    CAMList.RemoveAt(index)
                End If
            End If
        End SyncLock
        If isFind = False Then Return
        If IsNothing(context) Then Return
        response(context, msg)
        Return
    End Sub
    Structure json_Audio
        Dim freq As Double
        Dim audioBase64 As String
    End Structure
    Private Sub HandleAudio(ByVal wavBuffer() As Byte)
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
            Dim wavByte() As Byte = addWavHead(wav_buffer, caiyanglv, yinpinweishu, tongdaoshu)
            If IsNothing(wavByte) Then Exit Sub
            Dim AudioJosn As json_Audio
            AudioJosn.freq = xinhaopinlv
            AudioJosn.audioBase64 = Convert.ToBase64String(wavByte)
            Dim jsonmsg As JSON_Msg
            jsonmsg.func = "ifscan_wav"
            jsonmsg.msg = JsonConvert.SerializeObject(AudioJosn)
            Dim deviceMsg As String = JsonConvert.SerializeObject(jsonmsg)
            AddDeviceMsg(deviceMsg)
        Catch ex As Exception

        End Try
    End Sub
    Structure json_PPSJ
        Dim freqStart As Double
        Dim freqStep As Double
        Dim deviceID As String
        Dim dataCount As Integer
        Dim value() As Double
    End Structure
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
    Private Sub handlePinPuFenXi(ByVal deviceID As String, ByVal PinPuShuJu() As Byte)
        Try
            Dim p As ppsj = shuju2ppsj(PinPuShuJu)
            Dim qishi As Double = p.qishipinlv
            Dim bujin As Double = p.bujin
            Dim weishu As Integer = p.weishu
            Dim geshu As Integer = 0
            Dim xx() As Double, yy() As Double
            Dim jifenleixing As Integer = p.jifenleixing
            If jifenleixing <> 0 Then Exit Sub
            If weishu = 8 Then
                geshu = p.pinpuzongshu
                ReDim xx(geshu - 1), yy(geshu - 1)
                For i = 0 To geshu - 1
                    yy(i) = p.pinpushuju(i) - 127 - 80
                    xx(i) = qishi + i * bujin
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
                    xx(i) = qishi + i * bujin
                Next
                If isYouXiao = False Then

                    Exit Sub
                End If
            End If

            If IsNothing(xx) Then Exit Sub
            If IsNothing(yy) Then Exit Sub
            If isTZBQ_JCPD = False Then
                Dim jsonPP As json_PPSJ
                jsonPP.freqStart = qishi
                jsonPP.freqStep = bujin
                jsonPP.dataCount = geshu
                jsonPP.deviceID = myDeviceInfo.DeviceID
                jsonPP.value = yy
                Dim jsonmsg As JSON_Msg
                jsonmsg.func = "bscan"
                jsonmsg.msg = JsonConvert.SerializeObject(jsonPP)
                Dim deviceMsg As String = JsonConvert.SerializeObject(jsonmsg)
                AddDeviceMsg(deviceMsg)
                If isWorking Then
                    If isWarnWZ Then
                        Try
                            If IsNothing(WarnInfoList) = False Then
                                Dim sigNal(,) As Double = XinHaoFenLi(xx, yy, warnDaikuan, WarnFucha)
                                If IsNothing(sigNal) = False Then
                                    For i = 0 To sigNal.Length / 2 - 1
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
                                                            AddWarnMsg2WarnList(deviceTZBQMsg)
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

                    RecordDeviceMsg(myDeviceInfo.DeviceID, "ING", jsonmsg.func, deviceMsg)
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
                        Catch ex As Exception

                        End Try
                    End If
                End If
            End If
        Catch ex As Exception
            ' log(ex.ToString)
        End Try
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
        Dim id As String = tm.deviceID
        If IsNothing(DeviceListLock) Then DeviceListLock = New Object
        SyncLock DeviceListLock
            If IsNothing(DeviceList) Then DeviceList = New List(Of deviceStu)
            For i = 0 To DeviceList.Count - 1
                Dim sh As deviceStu = DeviceList(i)
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
                                        handleTm(byteSub)
                                        byteSub = Nothing
                                        Exit Sub
                                    End If
                                    If byteSub.Length > lenofmsg Then
                                        Dim bk() As Byte = tobytes(byteSub, 0, lenofmsg)
                                        handleTm(bk)
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
    Private Sub StartHttp(ByVal cls As MZHGateWay)
        Try
            log("MZH_" & myDeviceInfo.DeviceID & ",尝试开启HTTP服务……")
            myHttpListener = New HttpListener
            myHttpListener.Prefixes.Add(myHttpUrl)
            myHttpListener.Start()
            log("MZH_" & myDeviceInfo.DeviceID & ",已开启HTTP服务")
            log("MZH_" & myDeviceInfo.DeviceID & ",尝试开启线程……")
            broadcastThread = New Thread(AddressOf sub_broadcastDeviceMsg)
            broadcastThread.Start()
            log("MZH_" & myDeviceInfo.DeviceID & ",已开启线程")
            If IsNothing(DeviceListLock) Then DeviceListLock = New Object
            SyncLock DeviceListLock
                DeviceList.Add(myDeviceInfo)
            End SyncLock
            log("DeviceList已记录DeviceID=" & myDeviceInfo.DeviceID & ",位置=" & myDeviceInfo.Address)
            myHttpListener.BeginGetContext(New AsyncCallback(AddressOf GetContextCallBack), myHttpListener)
        Catch ex As Exception

            log("MZH_" & myDeviceInfo.DeviceID & ",初始化HTTP等服务失败，ERR-->" & ex.Message)
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
    ''' <summary>
    ''' 处理客户端的HTTP请求
    ''' </summary>
    Private Sub SendINGMsg2Device(INGMsg As INGMsgStu)
        Try
            Dim by() As Byte = INGMsg2TSSShuju(INGMsg)
            SendMsgToTSSDevByString("&H0", "task", "ingmsg", "", by)
        Catch ex As Exception

        End Try
    End Sub
    Private Sub HandleINGMsgStu(INGMsg As INGMsgStu, realMsgID As Integer)
        INGMsg.MsgID = realMsgID
        INGMsg.MsgTime = Now.ToString("yyyy-MM-dd HH:mm:ss")
        SendINGMsg2Device(INGMsg)
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
            If func = "CommandING" Then
                Try
                    Dim INGMsgText As String = GetParaValue(GETSTR, "INGMsgText")
                    If INGMsgText = "" Then
                        response(context, "result=fail;msg=" & "INGMsgText不可为空")
                        Return
                    End If
                    Dim INGMsg As INGMsgStu = JsonConvert.DeserializeObject(INGMsgText, GetType(INGMsgStu))
                    If IsNothing(INGMsg) Then
                        response(context, "result=fail;msg=" & "INGMsgText格式错误")
                        Return
                    End If
                    Dim realMsgID As Integer = 0
                    SyncLock CAMListLock
                        Dim MsgID As Integer = 0
                        While True
                            Dim isfind As Boolean = False
                            For Each sh In CAMList
                                If sh.MsgID = MsgID Then
                                    isfind = True
                                    Exit For
                                End If
                            Next
                            If isfind Then
                                MsgID = MsgID + 1
                            Else
                                Exit While
                            End If
                        End While
                        realMsgID = MsgID
                        Dim k As New ContextAndMsgID(MsgID, context）
                        CAMList.Add(k)
                    End SyncLock
                    HandleINGMsgStu(INGMsg, realMsgID)
                    Return
                Catch ex As Exception
                    response(context, "result=fail;msg=" & ex.Message)
                    Return
                End Try
                response(context, "result=fail;msg=" & "unknow err")
                Return
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
                Exit Sub
            End If
            If func = "netswitchout" Then
                AddMyLog("接收一般命令,切换到外网", "成功")
                response(context, SendMsgToTSSDevByString(&H0, "task", "netswitch", "<netswitch:sw=out;>", Nothing))
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
        response(context, "hasaki")
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
            'log("<发送TSS_Dev> id=" & myDeviceInfo.DeviceID & "," & b & "," & c & "," & d)
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
            log("断开超时ING_Socket,DeviceID=" & myDeviceInfo.DeviceID & ">ERR>" & ex.Message)
        End Try
        Try
            broadcastThread.Abort()
            log("关闭" & oldstr & "线程成功")
        Catch ex As Exception
            log("关闭" & oldstr & "线程失败" & ex.ToString)
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
