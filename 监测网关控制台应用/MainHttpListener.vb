Imports System.Collections.Generic
Imports System.Linq
Imports System.Text
Imports System.IO
Imports System.Math
Imports System.Net.Sockets
Imports System.Net
Imports System.Net.HttpListener
Imports System.Data
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
Imports System.Reflection
Public Class MainHttpListener
    Dim MainHttpListener As HttpListener
    Dim HTTPMsg_IpUrl As String = "http://123.207.31.37:"
    Public Event Raiselog(ByVal str As String)
    Public Event RaiseHttplog(ByVal str As String)
    Public warnMsgList As List(Of String)
    Public warnMsgListLock As New Object
    Public warnContextList As List(Of HttpListenerContext)
    Public warnContextListLock As New Object
    Private myServerURI As String
    Structure jobStu
        Dim JobID As String
        Dim Time As String
        Dim JobFrom As String
        Dim Department As String
        Dim Worker As String
        Dim Title As String
        Dim Content As String
        Dim Status As String
        Dim StartTime As String
        Dim EndTime As String
        Dim Job As String
        Dim AlarmJob As String
        Dim DeviceID As String
        Dim TaskNickName As String
        Dim fileName As String
        Dim fileBase64 As String
    End Structure
    Structure JTStu
        Dim imgName As String
        Dim imgText As String
        Dim userName As String
        Dim base64 As String
    End Structure
    Private Sub log(ByVal str As String)
        RaiseEvent Raiselog(str)
    End Sub
    Private Sub Httplog(ByVal str As String)
        RaiseEvent RaiseHttplog(str)
    End Sub
    Sub New(ByVal url As String, ByVal _HTTPMsg_IpUrl As String)
        Try


            MainHttpListener = New HttpListener
            HTTPMsg_IpUrl = _HTTPMsg_IpUrl
            MainHttpListener.Prefixes.Add(url)
            myServerURI = url
        Catch ex As Exception
            log("MainHttpListener初始化失败，" & ex.Message)
        End Try
    End Sub
    Public Sub Start()
        Try
            MainHttpListener.Start()
            log("MainHttpListener已开启,URL=" & myServerURI)
            MainHttpListener.BeginGetContext(New AsyncCallback(AddressOf GetContextCallBack), MainHttpListener)
            Dim th As New Thread(AddressOf BroadcastWarnMsg)
            th.Start()
            log("BroadcastWarnMsg已开启")

        Catch ex As Exception
            log("MainHttpListener开启失败" & ex.Message)
        End Try

    End Sub

    Private Sub GetContextCallBack(ByVal ar As IAsyncResult)
        Try
            MainHttpListener = ar.AsyncState
            Dim context As HttpListenerContext = MainHttpListener.EndGetContext(ar)
            MainHttpListener.BeginGetContext(New AsyncCallback(AddressOf GetContextCallBack), MainHttpListener)
            HandleHttpContext(context)
        Catch ex As Exception

        End Try
    End Sub
    Structure TSSOrder_stu
        Dim deviceID As String
        Dim task As String
        Dim freqStart As Double
        Dim freqEnd As Double
        Dim freqStep As Double
        Dim saveFreqStep As String
    End Structure
    Private Sub AddMyLog(ByVal log As String, ByVal result As String)
        AddDeviceLog("Server", "服务器", "", log, result, "在线")
    End Sub

    Public Sub AddWarnMsg2List(ByVal warnMsg As String)
        Try
            SyncLock warnMsgListLock
                If IsNothing(warnMsgList) Then warnMsgList = New List(Of String)
                warnMsgList.Add(warnMsg)
            End SyncLock
        Catch ex As Exception
            MsgBox("2" & vbCrLf & ex.ToString)
        End Try

    End Sub
    Public Sub BroadcastWarnMsg()
        Dim num As Integer = 0
        While True
            Try
                Dim isReply As Boolean = False
                Dim msgCount As Integer = 0
                Dim conCount As Integer = 0
                If IsNothing(warnMsgList) = False Then
                    If warnMsgList.Count > 0 Then
                        Dim json As String = ""
                        SyncLock warnMsgListLock
                            json = JsonConvert.SerializeObject(warnMsgList)
                            msgCount = warnMsgList.Count
                            warnMsgList.Clear()
                        End SyncLock
                        If json <> "" Then
                            If IsNothing(warnContextList) = False Then
                                If warnContextList.Count > 0 Then
                                    SyncLock warnContextListLock
                                        conCount = warnContextList.Count
                                        For Each cot In warnContextList
                                            response(cot, json)
                                        Next
                                        warnContextList.Clear()
                                        isReply = True
                                    End SyncLock
                                Else
                                    SyncLock warnMsgListLock
                                        warnMsgList.Clear()
                                    End SyncLock
                                End If
                            Else
                                SyncLock warnMsgListLock
                                    warnMsgList.Clear()
                                End SyncLock
                            End If
                        Else

                        End If
                    End If
                End If
                If isReply = False Then
                    num = num + 1
                    ' log("num=" & num)
                    If num = 60 Then
                        num = 0
                        If IsNothing(warnContextList) = False Then
                            If warnContextList.Count > 0 Then
                                SyncLock warnContextListLock
                                    For Each cot In warnContextList
                                        response(cot, "null")
                                    Next
                                    warnContextList.Clear()
                                End SyncLock
                            End If
                        End If
                    End If
                Else
                    'log("msgCount=" & msgCount)
                    'log("conCount=" & conCount)
                End If
                Sleep(2000)
            Catch ex As Exception

            End Try
        End While
    End Sub
    Structure SH57NetSwitch_Stu
        Dim DeviceID As String
        Dim DeviceName As String
        Dim NetSwitch As Integer
    End Structure

    Private Sub ReStart()
        Dim th As New Thread(AddressOf ReStartRun)
        Sleep(2000)
        th.Start()
    End Sub
    Private Sub ReStartRun()
        Dim path As String = System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName
        Dim p As New Process
        p.StartInfo.FileName = path
        p.Start()
        End
    End Sub
    Private Sub HandleHttpContext(ByVal context As HttpListenerContext)
        If context.Request.HttpMethod.ToLower.ToString = "post" Then
            HandlePost(context)
            Exit Sub
        End If
        Dim GETSTR As String = UrlDecode(context.Request.Url.PathAndQuery)
        Dim func As String = GetParaValue(GETSTR, "func")
        Dim datamsg As String = GetParaValue(GETSTR, "datamsg")
        Dim exename As String = GetParaValue(GETSTR, "exename")
        Dim deviceID As String = GetParaValue(GETSTR, "deviceID")
        Dim group As String = GetParaValue(GETSTR, "group")
        Dim token As String = GetParaValue(GETSTR, "token")
        Dim usr As String = GetParaValue(GETSTR, "usr")
        Dim pwd As String = GetParaValue(GETSTR, "pwd")
        If func = "getUpdateExeLength" Or func = "getUpdate" Then
            HandleGetUpdate(context)
            Exit Sub
        End If
        If func = "autoUpdate" Then
            HandleAutoUpdate(context)
            Return
        End If
        If func = "webtest" Then
            response(context, "true")
            Exit Sub
        End If
        If func = "login" Then
            Dim result As String = CheckLoginFunc(usr, pwd, "CLT")
            If result = "登录成功" Then
                Dim responseMsg As String = ""
                Dim isFind As Boolean = False
                SyncLock dik_Lock
                    For Each itm In dik
                        If itm.Key = usr Then
                            isFind = True
                            Exit For
                        End If
                    Next
                End SyncLock
                If isFind = False Then
                    Dim newtoken As String = GetTokenByUserName(usr, "CLT")
                    SetToken(usr, newtoken)
                    response(context, "result=success;token=" & newtoken)
                Else
                    response(context, "result=success;token=" & GetToken(usr))
                End If
                Exit Sub
            Else
                response(context, "result=fail;msg=" & result)
                Exit Sub
            End If
        End If
        If func = "regist" Then
            Dim rusr As String = GetParaValue(GETSTR, "rusr")
            Dim rpwd As String = GetParaValue(GETSTR, "rpwd")
            Dim rpower As String = GetParaValue(GETSTR, "rpower")
            If rusr = "" Or rpower = "" Or rpwd = "" Then
                response(context, "result=fail;msg=请填写好注册信息")
                Exit Sub
            End If
            If rusr.ToLower = "admin" Then
                response(context, "result=fail;msg=用户名不能为Admin")
                Exit Sub
            End If
            Try
                Dim sql As String = "select * from userTable where usr='" & rusr & "'"
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        response(context, "result=fail;msg=该用户名已被注册")
                        Exit Sub
                    End If
                End If
                sql = "insert into userTable values ('{0}','{1}','{2}','{3}','{4}','{5}','{6}','{7}')"
                Dim kind As String = "CLT"
                Dim email As String = ""
                Dim power As String = rpower
                Dim status As String = "0"
                sql = String.Format(sql, New String() {rusr, rpwd, kind, email, power, status, "all", "all"})
                Dim result As String = SQLCmd(sql)
                If result = "success" Then
                    response(context, "result=success;msg=注册成功，请等待上级审核")
                    Exit Sub
                Else
                    response(context, "result=success;msg=" & result & "")
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "result=fail;msg=null;errmsg=" & ex.Message)
                Exit Sub
            End Try
        End If
        If islogined(token) = False Then
            response(context, "result=fail;msg=Please login")
            Exit Sub
        End If
        ''''''一切其他请求发生在这之后

        If func = "Restart" Then
            Try
                Dim remoteIPPort As String = context.Request.RemoteEndPoint.ToString
                Dim remoteUserName As String = GetUsrByToken(token)
                AddMyLog("重启[" & remoteUserName & "@" & remoteIPPort & "]"， "成功")
                response(context, "result=success;msg=" & "系统将会重新启动")
                ReStart()
                Return
            Catch ex As Exception

            End Try
        End If
        If func = "GetSysConf" Then
            Try

                SyncLock myServerInfoLock
                    If IsNothing(myServerInfo) = False Then
                        Dim json As String = JsonConvert.SerializeObject(myServerInfo)
                        response(context, json)
                        Return
                    Else
                        response(context, "result=fail;msg=" & "myServerInfo=null")
                        Return
                    End If
                End SyncLock
            Catch ex As Exception
                response(context, "result=fail;msg=" & ex.Message)
                Return
            End Try
            response(context, "result=fail;msg=" & "unknow err")
            Return
        End If
        If func = "SetSysConf" Then
            Try
                Dim remoteIPPort As String = context.Request.RemoteEndPoint.ToString
                Dim remoteUserName As String = GetUsrByToken(token)
                AddMyLog("设置参数[" & remoteUserName & "@" & remoteIPPort & "]"， "成功")
                Dim conf As String = GetParaValue(GETSTR, "conf")
                If conf = "" Then
                    response(context, "result=fail;msg=conf字段不能为空，应为json")
                    Return
                End If
                Try
                    Dim tmp As myServerInfoStu = JsonConvert.DeserializeObject(conf, GetType(myServerInfoStu))
                    If IsNothing(tmp) Then
                        response(context, "result=fail;msg=conf json格式不对")
                        Return
                    End If
                    If tmp.HTTPMsg_IpUrl = "" Or
                       tmp.isMain = Nothing Or
                       tmp.isSaveDevMsg = Nothing Or
                       tmp.MainServerIP = "" Or
                       tmp.MainServerPort <= 0 Or
                       tmp.MZHDeviceID = "" Or
                       tmp.SQLDataBaseName = "" Or
                       tmp.SQLPwd = "" Or
                       tmp.SQLServerIP = "" Or
                       tmp.SQLUid = "" Or
                       tmp.TSSPort <= 0 Or
                       tmp.TZBQPort <= 0 Then
                        response(context, "result=fail;msg=conf任何参数均不可为空")
                        Return
                    End If
                    SyncLock myServerInfoLock
                        myServerInfo = Nothing
                        myServerInfo = tmp
                        SaveMyServerInfo()
                        response(context, "result=success;msg=设置成功")
                        Return
                    End SyncLock

                    Return
                Catch ex As Exception
                    response(context, "result=fail;msg=" & ex.Message)
                    Return
                End Try
                response(context, "result=fail;msg=null")
                Return
            Catch ex As Exception

            End Try
        End If
        If func = "GetSH57NetSwitch" Then
            Try
                Dim SH57DList As New List(Of SH57NetSwitch_Stu)
                SyncLock DeviceListLock
                    For Each d In DeviceList
                        Dim dId As String = d.DeviceID
                        Dim dName As String = d.Name
                        Dim NetSwitch As Integer = d.NetSwitch  '0 Unkown   1 in  2 out
                        If InStr(dId, "-") Then
                            Dim st() As String = dId.Split("-")
                            If st(0) = "57S" Then
                                Dim sh As SH57NetSwitch_Stu
                                sh.DeviceID = dId
                                sh.DeviceName = dName
                                sh.NetSwitch = NetSwitch
                                SH57DList.Add(sh)
                            End If
                        End If
                    Next
                End SyncLock
                Dim json As String = JsonConvert.SerializeObject(SH57DList)
                response(context, json)
                Exit Sub
            Catch ex As Exception
                response(context, "result=" & "fail" & ";msg=" & "" & ";errMsg=" & ex.Message & ";advise=null")
                Exit Sub
            End Try
        End If
        If func = "GetMyPower" Then
            Dim userName As String = GetUsrByToken(token)
            Dim sql As String = "select * from userTable where usr='" & userName & "'"
            Try
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        Dim row As DataRow = dt.Rows(0)
                        Dim power As String = row("power")
                        response(context, "result=" & "success" & ";power=" & power)
                        Exit Sub
                    Else
                        response(context, "result=" & "fail" & ";msg=" & "获取权限失败")
                        Exit Sub
                    End If
                Else
                    response(context, "result=" & "fail" & ";msg=" & "获取权限失败")
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "result=" & "fail" & ";msg=" & "" & ";errMsg=" & ex.Message & ";advise=null")
                Exit Sub
            End Try
        End If
        If func = "GetMyUI" Then
            Dim userName As String = GetUsrByToken(token)
            Dim sql As String = "select * from userTable where usr='" & userName & "'"
            Try
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        Dim row As DataRow = dt.Rows(0)
                        Dim ui As String = row("ui")
                        response(context, "result=" & "success" & ";ui=" & ui)
                        Exit Sub
                    Else
                        response(context, "result=" & "fail" & ";msg=" & "获取UI失败")
                        Exit Sub
                    End If
                Else
                    response(context, "result=" & "fail" & ";msg=" & "获取UI失败")
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "result=" & "fail" & ";msg=" & "" & ";errMsg=" & ex.Message & ";advise=null")
                Exit Sub
            End Try
        End If
        If func = "GetMyDevice" Then
            Dim userName As String = GetUsrByToken(token)
            Dim sql As String = "select * from userTable where usr='" & userName & "'"
            Try
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        Dim row As DataRow = dt.Rows(0)
                        Dim device As String = row("device")
                        response(context, "result=" & "success" & ";device=" & device)
                        Exit Sub
                    Else
                        response(context, "result=" & "fail" & ";msg=" & "获取device失败")
                        Exit Sub
                    End If
                Else
                    response(context, "result=" & "fail" & ";msg=" & "获取device失败")
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "result=" & "fail" & ";msg=" & "" & ";errMsg=" & ex.Message & ";advise=null")
                Exit Sub
            End Try
        End If
        If func = "GetAllUser" Then
            Dim userName As String = GetUsrByToken(token)
            Dim power As Integer = GetMyPower(userName)
            If power = 1 Or power = 9 Then
                Try
                    Dim sql As String = "select usr,power,status from userTable where usr!='admin'"
                    Dim dt As DataTable = SQLGetDT(sql)
                    If IsNothing(dt) = False Then
                        If dt.Rows.Count > 0 Then
                            response(context, JsonConvert.SerializeObject(dt))
                            Exit Sub
                        Else
                            response(context, "[]")
                            Exit Sub
                        End If
                    Else
                        response(context, "[]")
                        Exit Sub
                    End If
                Catch ex As Exception
                    response(context, "[]")
                    Exit Sub
                End Try
            Else
                response(context, "[]")
                Exit Sub
            End If
        End If
        If func = "changeUsrInfo" Then
            Dim userName As String = GetUsrByToken(token)
            Dim power As Integer = GetMyPower(userName)
            If power = 2 Then
                response(context, "result=fail;msg=您的权限不够")
                Exit Sub
            End If
            Dim spower As String = GetParaValue(GETSTR, "power")
            Dim susr As String = GetParaValue(GETSTR, "usr")
            Dim status As String = GetParaValue(GETSTR, "status")
            If spower = "2" Then
                If power <> 1 And power <> 9 Then
                    response(context, "result=fail;msg=您的权限不够,只有领导账户和管理员能设置值班员账户")
                    Exit Sub
                End If
            End If
            If spower = "1" Then
                If power <> 9 Then
                    response(context, "result=fail;msg=您的权限不够,只有管理员账户能设置领导账户")
                    Exit Sub
                End If
            End If
            Dim apower As String = GetMyPower(susr)
            If apower = power Then
                response(context, "result=fail;msg=您的权限不够,不能平级设置")
                Exit Sub
            End If
            Try
                Dim sql As String = "update userTable set power='{0}' , status='{1}' where usr='{2}'"
                sql = String.Format(sql, New String() {spower, status, susr})
                Dim result As String = SQLCmd(sql)
                If result = "success" Then
                    AddJobLog(userName, "领导记事", "设置账户", "改变账户" & susr & "相关属性", "", "", "", susr)
                    response(context, "result=success;msg=设置成功")
                    Exit Sub
                Else
                    response(context, "result=fail;msg=" & result)
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "result=fail;msg=;errmsg=" & ex.Message)
                Exit Sub
            End Try
        End If
        If func = "GetJobList" Then
            Try
                Dim sql As String = "select * from JobTable order by Time"
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        response(context, JsonConvert.SerializeObject(dt))
                        Exit Sub
                    Else
                        response(context, "[]")
                        Exit Sub
                    End If
                Else
                    response(context, "[]")
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "[]")
                Exit Sub
            End Try
        End If
        If func = "GetLogList" Then
            Try
                Dim sql As String = "select * from LogTable order by Time"
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        response(context, JsonConvert.SerializeObject(dt))
                        Exit Sub
                    Else
                        response(context, "[]")
                        Exit Sub
                    End If
                Else
                    response(context, "[]")
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "[]")
                Exit Sub
            End Try
        End If
        If func = "GetLogListByDate" Then
            Try
                Dim startTime As String = GetParaValue(GETSTR, "startTime")
                Dim endTime As String = GetParaValue(GETSTR, "endTime")
                Dim startDate As Date
                Dim endDate As Date
                Try
                    startDate = Date.Parse(startTime)
                    endDate = Date.Parse(endTime)
                    If endDate <= startDate Then
                        response(context, "[]")
                        Exit Sub
                    End If
                    startTime = startDate.ToString("yyyy-MM-dd HH:mm:ss")
                    endTime = endDate.ToString("yyyy-MM-dd HH:mm:ss")
                Catch ex As Exception
                    response(context, "[]")
                    Exit Sub
                End Try
                Dim sql As String
                Dim userName As String = GetUsrByToken(token)
                Dim deviceListString As String = GetMyDevList(userName)
                If deviceListString = "" Or deviceListString.ToLower() = "all" Then
                    sql = "select * from LogTable where Time between '{0}' and '{1}' order by Time"
                    sql = String.Format(sql, New String() {startTime, endTime})
                Else
                    sql = "select * from LogTable where (usr='{0}' or usr='{1}') and Time between '{2}' and '{3}' order by Time"
                    sql = String.Format(sql, New String() {userName, "system", startTime, endTime})
                End If

                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        response(context, JsonConvert.SerializeObject(dt))
                        Exit Sub
                    Else
                        response(context, "[]")
                        Exit Sub
                    End If
                Else
                    response(context, "[]")
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "[]")
                Exit Sub
            End Try
        End If
        If func = "DeleteLogByID" Then
            Try
                Dim LogID As String = GetParaValue(GETSTR, "logid")
                If LogID = "" Then
                    response(context, "result=fail;msg=" & "LogID不能为空")
                    Exit Sub
                End If
                Dim sql As String = "Delete from logTable where LogID='" & LogID & "'"
                Dim result As String = SQLCmd(sql)
                If result = "success" Then
                    response(context, "result=success;msg=" & "")
                    Exit Sub
                Else
                    response(context, "result=fail;msg=" & result)
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "result=fail;msg=;errmsg=" & ex.Message)
                Exit Sub
            End Try
        End If
        If func = "DeleteJobByID" Then
            Try
                Dim JobID As String = GetParaValue(GETSTR, "jobid")
                If JobID = "" Then
                    response(context, "result=fail;msg=" & "JobID不能为空")
                    Exit Sub
                End If
                Dim sql As String = "Delete from JobTable where JobID='" & JobID & "'"
                Dim result As String = SQLCmd(sql)
                If result = "success" Then
                    response(context, "result=success;msg=" & "")
                    Exit Sub
                Else
                    response(context, "result=fail;msg=" & result)
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "result=fail;msg=;errmsg=" & ex.Message)
                Exit Sub
            End Try
        End If
        If func = "GetJobByID" Then
            Try
                Dim JobID As String = GetParaValue(GETSTR, "jobid")
                If JobID = "" Then
                    response(context, "[]")
                    Exit Sub
                End If
                Dim sql As String = "select * from JobTable where JobID='" & JobID & "'"
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        response(context, JsonConvert.SerializeObject(dt))
                        Exit Sub
                    Else
                        response(context, "[]")
                        Exit Sub
                    End If
                Else
                    response(context, "[]")
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "[]")
                Exit Sub
            End Try
        End If
        If func = "GetLogByID" Then
            Try
                Dim LogID As String = GetParaValue(GETSTR, "logid")
                If LogID = "" Then
                    response(context, "[]")
                    Exit Sub
                End If
                Dim sql As String = "select * from logTable where LogID='" & LogID & "'"
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        response(context, JsonConvert.SerializeObject(dt))
                        Exit Sub
                    Else
                        response(context, "[]")
                        Exit Sub
                    End If
                Else
                    response(context, "[]")
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "[]")
                Exit Sub
            End Try
        End If
        If func = "GetNewWarn" Then
            SyncLock warnContextListLock
                If IsNothing(warnContextList) Then warnContextList = New List(Of HttpListenerContext)
                warnContextList.Add(context)
            End SyncLock
            Exit Sub
        End If
        If func = "GetHistoryImageList" Then
            Try
                Dim userName As String = GetUsrByToken(token)
                Dim sql As String = "select imgName,imgText,userName from imgTable where userName='" & userName & "'"
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) Then
                    response(context, "[]")
                    Exit Sub
                End If
                If dt.Rows.Count = 0 Then
                    response(context, "[]")
                    Exit Sub
                End If
                Dim json As String = JsonConvert.SerializeObject(dt)
                response(context, json)
                Exit Sub
            Catch ex As Exception
                response(context, "result=" & "fail" & ";msg=" & "" & ";errMsg=" & ex.Message & ";advise=null")
                Exit Sub
            End Try
        End If
        If func = "GetTaskOverPercent" Then
            Dim taskNickName As String = GetParaValue(GETSTR, "taskNickName")
            If taskNickName = "" Then
                response(context, "result=" & "fail" & ";msg=" & "taskNickName不能为空")
                Exit Sub
            End If
            Try
                Dim sql As String = "select * from userTaskTable where TaskNickName='" & taskNickName & "'"
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        Dim row As DataRow = dt.Rows(0)
                        Dim StartTime As String = row("StartTime")
                        Dim EndTime As String = row("EndTime")
                        Dim OverPercent As String = row("OverPercent")
                        If OverPercent = "99%" Or OverPercent = "100%" Then
                            response(context, "result=" & "success" & ";OverPercent=" & OverPercent)
                            Exit Sub
                        Else
                            Dim StartDate As Date = Date.Parse(StartTime)
                            Dim EndDate As Date = Date.Parse(EndTime)
                            Dim nowDate As Date = Now
                            Dim t1 As TimeSpan = EndDate - StartDate
                            Dim t2 As TimeSpan = Now - StartDate
                            Dim d As Double = t2.TotalSeconds / t1.TotalSeconds
                            If StartTime >= Now Then
                                d = 0
                            End If
                            Dim percent As String = (d * 100).ToString(0.0) & "%"
                            response(context, "result=" & "success" & ";OverPercent=" & percent)
                            Exit Sub
                        End If
                    Else
                        response(context, "result=" & "fail" & ";msg=没有该任务记录")
                        Exit Sub
                    End If
                Else
                    response(context, "result=" & "fail" & ";msg=没有该任务记录")
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "result=" & "fail" & ";msg=" & "errmsg=" & ex.Message)
                Exit Sub
            End Try
        End If
        If func = "GetTaskDetailInfo" Then
            Dim taskNickName As String = GetParaValue(GETSTR, "taskNickName")
            If taskNickName = "" Then
                response(context, "result=" & "fail" & ";msg=" & "taskNickName不能为空")
                Exit Sub
            End If
            Try
                Dim sql As String = "select * from userTaskTable where TaskNickName='" & taskNickName & "'"
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        response(context, JsonConvert.SerializeObject(dt))
                        Exit Sub
                    Else
                        response(context, "result=" & "fail" & ";msg=没有该任务记录")
                        Exit Sub
                    End If
                Else
                    response(context, "result=" & "fail" & ";msg=没有该任务记录")
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "result=" & "fail" & ";msg=" & "errmsg=" & ex.Message)
                Exit Sub
            End Try
        End If
        If func = "GetImageBase64ByImgName" Then
            Dim ImgName As String = GetParaValue(GETSTR, "imgname")
            If ImgName = "" Then
                response(context, "result=" & "fail" & ";msg=" & "截图名称不能为空" & ";errMsg=" & "" & ";advise=null")
                Exit Sub
            End If
            Try
                Dim sql As String = String.Format("select base64 from imgTable where imgName='{0}'", ImgName)
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) Then
                    response(context, "[]")
                    Exit Sub
                End If
                If dt.Rows.Count = 0 Then
                    response(context, "[]")
                    Exit Sub
                End If
                Dim base64 As String = dt.Rows(0)(0)
                'Dim json As String = JsonConvert.SerializeObject(dt)
                response(context, base64)
                Exit Sub
            Catch ex As Exception
                response(context, "result=" & "fail" & ";msg=" & "" & ";errMsg=" & ex.Message & ";advise=null")
                Exit Sub
            End Try
        End If
        If func = "DeleteImageBase64ByImgName" Then
            Dim ImgName As String = GetParaValue(GETSTR, "imgname")
            If ImgName = "" Then
                response(context, "result=" & "fail" & ";msg=" & "截图名称不能为空" & ";errMsg=" & "" & ";advise=null")
                Exit Sub
            End If
            Try
                Dim sql As String = String.Format("delete from imgTable where imgName='{0}'", ImgName)
                Dim result As String = SQLCmd(sql)
                If result = "success" Then
                    response(context, "result=" & "success" & ";msg=" & "" & ";errMsg=" & "" & ";advise=null")
                    Return
                Else
                    response(context, "result=" & "fail" & ";msg=" & "" & ";errMsg=" & result & "" & ";advise=null")
                    Return
                End If
            Catch ex As Exception
                response(context, "result=" & "fail" & ";msg=" & "" & ";errMsg=" & ex.Message & ";advise=null")
                Exit Sub
            End Try
        End If
        If func = "GetHistoryAudioList" Then
            Dim list As New List(Of String)
            Dim audioPath As String = Directory.GetCurrentDirectory() & "\TSSaudio"
            If Directory.Exists(audioPath) = False Then
                response(context, "[]")
                Exit Sub
            End If
            Dim d As New DirectoryInfo(audioPath)
            For Each f In d.GetFiles
                list.Add(f.Name)
            Next
            Dim json As String = JsonConvert.SerializeObject(list)
            response(context, json)
            Exit Sub
        End If
        If func = "GetAudioBase64ByAudioFileName" Then
            Try

                Dim AudioFileName As String = GetParaValue(GETSTR, "audiofilename")
                If AudioFileName = "" Then
                    response(context, "result=" & "fail" & ";msg=" & "音频文件名称不能为空" & ";errMsg=" & "" & ";advise=null")
                    Exit Sub
                End If
                Dim audioPath As String = Directory.GetCurrentDirectory() & "\TSSaudio"
                If Directory.Exists(audioPath) = False Then
                    response(context, "null")
                    Exit Sub
                End If
                If File.Exists(audioPath & "\" & AudioFileName) = False Then
                    response(context, "null")
                    Exit Sub
                End If
                Dim stream As New FileStream(audioPath & "\" & AudioFileName, FileMode.Open)
                Dim br As New BinaryReader(stream)
                Dim flen As Double = New FileInfo(audioPath & "\" & AudioFileName).Length
                Dim by() As Byte = br.ReadBytes(flen)
                Dim base64 As String = Convert.ToBase64String(by)
                response(context, base64)
                Exit Sub
            Catch ex As Exception
                response(context, "result=" & "fail" & ";msg=" & "" & ";errMsg=" & ex.Message & ";advise=null")
                Exit Sub
            End Try
        End If
        If func = "getalldevlist" Then '获取在线设备列表
            If IsNothing(DeviceListLock) Then DeviceListLock = New Object
            Dim msg As String
            SyncLock DeviceListLock
                If IsNothing(DeviceList) = False Then
                    Dim d As List(Of deviceStuWithOutHttpListener) = TransForDeviceStu(DeviceList)
                    If IsNothing(d) = False Then
                        Dim userName As String = GetUsrByToken(token)
                        Dim myDevicestring As String = GetMyDevList(userName)
                        If token = "928453310" Then
                            myDevicestring = "all"
                        End If
                        If myDevicestring = "all" Then
                            msg = JsonConvert.SerializeObject(d)
                        Else
                            Dim list As New List(Of String)
                            list = myDevicestring.Split(",").ToList()
                            If IsNothing(list) Then
                                msg = "[]"
                            Else
                                For j = d.Count - 1 To 0 Step -1
                                    Dim itm As deviceStuWithOutHttpListener = d(j)
                                    If list.Contains(itm.DeviceID) = False Then
                                        d.RemoveAt(j)
                                    End If
                                Next
                                msg = JsonConvert.SerializeObject(d)
                            End If
                        End If
                    End If
                End If
            End SyncLock
            response(context, msg)
            Exit Sub
        End If
        If func = "GetAllDBDevlist" Then
            Dim userName As String = GetUsrByToken(token)
            Dim sql As String = "select * from devicetable"
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) = False Then
                If dt.Rows.Count > 0 Then
                    Dim myDevicestring As String = GetMyDevList(userName)
                    If myDevicestring = "all" Then
                        Dim json As String = JsonConvert.SerializeObject(dt)
                        response(context, json)
                        Exit Sub
                    Else
                        Dim list As New List(Of String)
                        list = myDevicestring.Split(",").ToList()
                        If IsNothing(list) Then
                            response(context, "[]")
                            Exit Sub
                        Else
                            For j = dt.Rows.Count - 1 To 0 Step -1
                                Dim row As DataRow = dt.Rows(j)
                                Dim did As String = row("DeviceID").ToString()
                                If list.Contains(did) = False Then
                                    dt.Rows.RemoveAt(j)
                                End If
                            Next
                            Dim json As String = JsonConvert.SerializeObject(dt)
                            response(context, json)
                            Exit Sub
                        End If
                    End If
                Else
                    response(context, "[]")
                    Exit Sub
                End If
            Else
                response(context, "[]")
                Exit Sub
            End If
        End If
        If func = "GetAllDeviceLog" Then
            Dim sql As String = "select * from DeviceLogTable"
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) = False Then
                If dt.Rows.Count > 0 Then
                    Dim json As String = JsonConvert.SerializeObject(dt)
                    response(context, json)
                    Exit Sub
                Else
                    response(context, "[]")
                    Exit Sub
                End If
            Else
                response(context, "[]")
                Exit Sub
            End If
        End If

        If func = "GetDeviceLogByNickNameWithTimeRegion" Then
            Dim nickName As String = GetParaValue(GETSTR, "nickname")
            Dim startTime As String = GetParaValue(GETSTR, "startTime")
            Dim endTime As String = GetParaValue(GETSTR, "endTime")
            If startTime = "" Then
                response(context, "result=" & "fail" & ";msg=" & "startTime is null" & ";errMsg=" & "" & ";advise=null")
                Exit Sub
            End If
            If endTime = "" Then
                response(context, "result=" & "fail" & ";msg=" & "endTime is null" & ";errMsg=" & "" & ";advise=null")
                Exit Sub
            End If
            If nickName = "" Then
                response(context, "result=" & "fail" & ";msg=" & "nickname is null" & ";errMsg=" & "" & ";advise=null")
                Exit Sub
            End If
            Try
                Dim startDate As Date = Date.Parse(startTime)
                startTime = startDate.ToString("yyyy-MM-dd") & " 00:00:00"
                Dim endDate As Date = Date.Parse(endTime)
                endTime = endDate.ToString("yyyy-MM-dd") & " 23:59:59"
                startDate = Date.Parse(startTime)
                endDate = Date.Parse(endTime)
                If endDate <= startDate Then
                    response(context, "result=" & "fail" & ";msg=" & "结束时间需大于起始时间" & ";errMsg=" & "" & ";advise=null")
                    Exit Sub
                End If
                Dim realDeviceID As String = GetDeviceIDByNickName(nickName)
                If realDeviceID = "" Then
                    response(context, "[]")
                    Exit Sub
                End If
                Dim sql As String = "select * from DeviceLogTable where DeviceID='{0}' and Time between '{1}' and '{2}'"
                sql = String.Format(sql, New String() {realDeviceID, startTime, endTime})
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        Dim json As String = JsonConvert.SerializeObject(dt)
                        response(context, json)
                        Exit Sub
                    Else
                        response(context, "[]")
                        Exit Sub
                    End If
                Else
                    response(context, "[]")
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "result=" & "fail" & ";msg=" & "date is not a DateTime" & ";errMsg=" & "" & ";advise=null")
                Exit Sub
            End Try
        End If
        If func = "GetDeviceLogByNickName" Then
            Dim nickName As String = GetParaValue(GETSTR, "nickname")
            Dim sDate As String = GetParaValue(GETSTR, "date")
            If sDate = "" Then
                response(context, "result=" & "fail" & ";msg=" & "date is null" & ";errMsg=" & "" & ";advise=null")
                Exit Sub
            End If
            If nickName = "" Then
                response(context, "result=" & "fail" & ";msg=" & "nickname is null" & ";errMsg=" & "" & ";advise=null")
                Exit Sub
            End If
            Try
                Dim sd As Date = Date.Parse(sDate)
                sDate = sd.ToString("yyyy-MM-dd") & " 00:00:00"
                Dim eDate As String = sd.ToString("yyyy-MM-dd") & " 23:59:59"
                Dim realDeviceID As String = GetDeviceIDByNickName(nickName)
                If realDeviceID = "" Then
                    response(context, "[]")
                    Exit Sub
                End If
                Dim sql As String = "select * from DeviceLogTable where DeviceID='{0}' and Time between '{1}' and '{2}'"
                sql = String.Format(sql, New String() {realDeviceID, sDate, eDate})
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        Dim json As String = JsonConvert.SerializeObject(dt)
                        response(context, json)
                        Exit Sub
                    Else
                        response(context, "[]")
                        Exit Sub
                    End If
                Else
                    response(context, "[]")
                    Exit Sub
                End If
            Catch ex As Exception
                response(context, "result=" & "fail" & ";msg=" & "date is not a DateTime" & ";errMsg=" & "" & ";advise=null")
                Exit Sub
            End Try
        End If
        If func = "GetHttpMsgUrlById" Then
            response(context, GetHttpMsgUrlById(deviceID))
            Exit Sub
        End If
        If func = "GetHttpMsgUrlByDeviceName" Then
            Dim DeviceName As String = GetParaValue(GETSTR, "DeviceName")
            response(context, GetHttpMsgUrlByDeviceName(DeviceName))
            Exit Sub
        End If
        If func = "TSSOrder" Then
            Dim msg As String = datamsg
            Try
                Dim TSS As TSSOrder_stu = JsonConvert.DeserializeObject(msg, GetType(TSSOrder_stu))
                If TSS.task = "bscan" Then
                    Dim freqbegin As Double = TSS.freqStart
                    Dim freqend As Double = TSS.freqEnd
                    Dim freqstep As Double = TSS.freqStep
                    Dim ifbw As String = 40000
                    Dim abw As String = freqstep
                    Dim bits As String = 16
                    Dim gcmode As String = "fagc"
                    Dim gcvalue As String = 0
                    Dim returndata As String = "fft"
                    Dim returninter As String = 2
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
                    response(context, sendMsgToTSSDevById(TSS.deviceID, &H0, "task", "bscan", msg, Nothing))
                End If
                If TSS.task = "stop" Then
                    response(context, sendMsgToTSSDevById(TSS.deviceID, &H0, "task", "taskctrl", "<taskctrl:taskstate=stop;>", Nothing))
                End If
            Catch ex As Exception
                response(context, "result=fail;msg=null;errMsg=" & ex.Message & ";advise=null")
            End Try
            Exit Sub
        End If
        If func = "DeleteMyTask" Then
            usr = GetUsrByToken(token)
            If usr = "" Then
                response(context, "未检测到用户登录信息,请重新登录")
                Return
            End If
            Dim TaskName As String = GetParaValue(GETSTR, "TaskName")
            Dim TaskNickName As String = GetParaValue(GETSTR, "TaskNickName")
            Dim StartTime As String = GetParaValue(GETSTR, "StartTime")
            Dim EndTime As String = GetParaValue(GETSTR, "EndTime")
            'DeviceID
            If deviceID = "" Or TaskName = "" Or TaskNickName = "" Or StartTime = "" Or EndTime = "" Then
                response(context, "信息不完整，请填写完整信息再重新提交")
                Return
            End If
            Try
                Dim endTimeDate As Date = Date.Parse(EndTime)
                If endTimeDate > Now.AddMinutes(-1) Then
                    Dim strTmp As String = "任务时间:{0},现在时间:{1}"
                    strTmp = String.Format(strTmp, endTimeDate.ToString("yyyy-MM-dd HH:mm:ss"), Now.ToString("yyyy-MM-dd HH:mm:ss"))
                    response(context, "result=" & "fail" & ";msg=" & "任务还未结束，无法删除" & ";errMsg=" & strTmp & "" & ";advise=null")
                    Return
                End If
            Catch ex As Exception
                response(context, "result=" & "fail" & ";msg=" & "EndTime is not a DateTime" & ";errMsg=" & "" & ";advise=null")
                Return
            End Try
            Dim sql As String = "delete from UserTaskTable where UserName='{0}' and  TaskName='{1}'  and  TaskNickName='{2}'  and  DeviceID='{3}' and StartTime='{4}' and EndTime='{5}'"
            sql = String.Format(sql, usr, TaskName, TaskNickName, deviceID, StartTime, EndTime)
            Dim result As String = SQLCmd(sql)
            If result = "success" Then
                'AddJobLog(usr, "值班员记事", "删除任务", "删除任务" & TaskNickName, "", "", "", TaskNickName)
                response(context, "result=" & "success" & ";msg=" & "" & ";errMsg=" & "" & ";advise=null")
                Return
            Else
                response(context, "result=" & "fail" & ";msg=" & "" & ";errMsg=" & result & "" & ";advise=null")
                Return
            End If
        End If
        If func = "AddTask" Then
            Try
                Dim TaskJson As String = GetParaValue(GETSTR, "TaskJson")
                Dim task As NormalTaskStu = JsonConvert.DeserializeObject(TaskJson, GetType(NormalTaskStu))
                If GetUsrByToken(token) <> task.UserName Then
                    response(context, "result=" & "fail" & ";msg=Please Use Your Own UserName;errMsg=" & "" & ";advise=null")
                    Exit Sub
                End If
                If task.isMoreDevice = False Then
                    Dim taskDeviceId As String = task.DeviceID
                    SyncLock DeviceListLock
                        For Each itm In DeviceList
                            If itm.DeviceID = taskDeviceId Then
                                task.DeviceName = itm.Name
                                Exit For
                            End If
                        Next
                    End SyncLock
                Else
                    Dim taskDeviceId As String = task.DeviceID
                    Dim list As List(Of String) = JsonConvert.DeserializeObject(taskDeviceId, GetType(List(Of String)))
                    If IsNothing(list) Then
                        response(context, "result=" & "fail" & ";msg=多设备选取有误;errMsg=" & "" & ";advise=null")
                        Exit Sub
                    End If
                    If list.Count = 0 Then
                        response(context, "result=" & "fail" & ";msg=多设备选取有误;errMsg=" & "" & ";advise=null")
                        Exit Sub
                    End If
                    Dim nList As New List(Of String)
                    SyncLock DeviceListLock
                        For Each d In list
                            For Each itm In DeviceList
                                If itm.DeviceID = d Then
                                    nList.Add(itm.Name)
                                    Exit For
                                End If
                            Next
                        Next
                    End SyncLock
                    task.DeviceName = JsonConvert.SerializeObject(nList)
                End If

                Dim taskStartTime As Date
                Dim taskEndTime As Date
                Dim taskNickName As String = task.TaskNickName
                If taskNickName = "" Then
                    response(context, "result=" & "fail" & ";msg=TaskNickName is null;errMsg=" & "" & ";advise=null")
                    Exit Sub
                End If
                Try
                    Dim sqlstrTmp As String = "select * from UserTaskTable where TaskNickName='{0}'"
                    Dim dtTmp As DataTable = SQLGetDT(sqlstrTmp)
                    If IsNothing(dtTmp) = False Then
                        If dtTmp.Rows.Count > 0 Then
                            response(context, "result=" & "fail" & ";msg=TaskNickName已存在;errMsg=" & "" & ";advise=null")
                            Exit Sub
                        End If
                    End If
                Catch ex As Exception

                End Try
                Try
                    taskStartTime = Date.Parse(task.StartTime)
                    taskEndTime = Date.Parse(task.EndTime)
                    If taskEndTime <= taskStartTime Then
                        response(context, "result=" & "fail" & ";msg=结束时间必须大于开始时间;errMsg=" & "" & ";advise=null")
                        Exit Sub
                    End If
                    If taskEndTime <= Now.AddMinutes(1) Then
                        response(context, "result=" & "fail" & ";msg=结束时间必须大于现在时间;errMsg=" & "" & ";advise=null")
                        Exit Sub
                    End If
                Catch ex As Exception
                    response(context, "result=" & "fail" & ";msg=;errMsg=" & "任务时间转换错误" & ";advise=null")
                    Exit Sub
                End Try
                If task.TimeStep < 5 Then
                    response(context, "result=" & "fail" & ";msg=TimeSpan必须大于等于5;errMsg=" & "" & ";advise=null")
                    Exit Sub
                End If
                If task.TaskName = "可用评估" Then
                    Dim isTSS As Boolean = False
                    SyncLock DeviceListLock
                        For i = 0 To DeviceList.Count - 1
                            Dim itm As deviceStu = DeviceList(i)
                            If itm.DeviceID = task.DeviceID Then
                                If itm.Kind = "TSS" Then
                                    isTSS = True
                                    Exit For
                                End If
                            End If
                        Next
                    End SyncLock
                    If isTSS Then
                        Dim code As String = task.TaskCode
                        code = code.Replace("[", "").Replace("]", "")
                        If InStr(code, ",") Then
                            Dim pds() As String = code.Split(",")
                            If CheckTSS_JCPD(pds) = False Then
                                response(context, "result=" & "fail" & ";msg=频点值间隔应等距")
                                Exit Sub
                            End If
                        Else
                            response(context, "result=" & "fail" & ";msg=TaskCode应为[a,b,c,d,e]")
                            Exit Sub
                        End If
                    End If
                End If
                If task.isMoreDevice = False Then
                    Dim CheckTime As String = String.Format("select * from UserTaskTable where " &
                                                       "DeviceID='{0}' and " &
                                                       "((StartTime between '{1}' and '{2}' or EndTime between '{1}' and '{2}') " &
                                                       "or (StartTime<='{1}' and EndTime>='{2}'))", New String() {task.DeviceID, task.StartTime, task.EndTime})
                    Dim dt As DataTable = SQLGetDT(CheckTime)
                    If IsNothing(dt) = False Then
                        If dt.Rows.Count > 0 Then
                            response(context, "result=" & "fail" & ";msg=该时段设备忙;errMsg=" & "" & ";advise=null")
                            Exit Sub
                        End If
                    End If
                Else
                    Dim taskDeviceId As String = task.DeviceID
                    Dim list As List(Of String) = JsonConvert.DeserializeObject(taskDeviceId, GetType(List(Of String)))
                    If IsNothing(list) = False Then
                        For Each d In list
                            Dim CheckTime As String = String.Format("select * from UserTaskTable where " &
                                                      "DeviceID='{0}' and " &
                                                      "((StartTime between '{1}' and '{2}' or EndTime between '{1}' and '{2}') " &
                                                      "or (StartTime<='{1}' and EndTime>='{2}'))", New String() {d, task.StartTime, task.EndTime})
                            Dim dt As DataTable = SQLGetDT(CheckTime)
                            If IsNothing(dt) = False Then
                                If dt.Rows.Count > 0 Then
                                    response(context, "result=" & "fail" & ";msg=该时段设备忙;errMsg=" & "" & ";advise=null")
                                    Exit Sub
                                End If
                            End If
                        Next
                    End If
                End If
                Dim TaskBg As String = task.TaskBg
                Dim TaskDeviceKind As String = GetTaskDeviceKind(task.DeviceID)
                Dim SQLString As String = "insert into UserTaskTable " &
                                     "(UserName,TaskName,TaskNickName,DeviceID,DeviceName,StartTime,EndTime,TimeStep,PushWeChatUserName,PushEmailUserName,TaskCode,OverPercent,ResultReportUrl,ErrMsg,TaskBg,isClosed,TaskDeviceKind)" &
                                      "values" &
                                      "('{0}','{1}','{2}','{3}','{4}','{5}','{6}','{7}','{8}','{9}','{10}','{11}','{12}','{13}','{14}',0,'{15}')"
                Dim para() As String = New String() {task.UserName, task.TaskName, task.TaskNickName, task.DeviceID, task.DeviceName,
                                                     task.StartTime, task.EndTime, task.TimeStep, task.PushWeChartToUserName,
                                                     task.PushEmailToUserName, task.TaskCode, "", "", "", TaskBg, TaskDeviceKind}
                Dim CmdString As String = String.Format(SQLString, para)
                Dim res As String = SQLCmd(CmdString)
                If res = "success" Then
                    Dim sb As New StringBuilder
                    If task.TaskBg <> "" Then
                        sb.AppendLine("任务背景:" & task.TaskBg & ";")
                    End If
                    sb.AppendLine("任务类型:" & task.TaskName & ";")
                    sb.AppendLine("任务备注:" & task.TaskNickName & ";")
                    sb.AppendLine("开始时间:" & task.StartTime & ";")
                    sb.AppendLine("结束时间:" & task.EndTime & ";")
                    AddJobLog(GetUsrByToken(token), "值班员记事", "下发任务", sb.ToString, task.DeviceName, task.DeviceID, taskNickName, taskNickName)
                    response(context, "result=" & res & ";msg=null;errMsg=null;advise=null")
                    If task.isMoreDevice = False Then
                        SyncLock DeviceListLock
                            For i = 0 To DeviceList.Count - 1
                                Dim itm As deviceStu = DeviceList(i)
                                If itm.DeviceID = task.DeviceID Then
                                    If itm.Kind = "TZBQ" Then
                                        Dim devTZBQ As DeviceTZBQ = CType(itm.cls, DeviceTZBQ)
                                        If IsNothing(devTZBQ) = False Then
                                            devTZBQ.GetMyTask()
                                        End If
                                    End If
                                End If
                                If itm.DeviceID = task.DeviceID Then
                                    If itm.Kind = "TSS" Then
                                        Dim devTSS As DeviceTSS = CType(itm.cls, DeviceTSS)
                                        If IsNothing(devTSS) = False Then
                                            devTSS.GetMyTask()
                                        End If
                                    End If
                                End If
                            Next
                        End SyncLock
                    Else
                        SyncLock DeviceListLock
                            Dim TaskDeviceID As String = task.DeviceID
                            Dim list As List(Of String) = JsonConvert.DeserializeObject(TaskDeviceID, GetType(List(Of String)))
                            If IsNothing(list) = False Then
                                For Each d In list
                                    For i = 0 To DeviceList.Count - 1
                                        Dim itm As deviceStu = DeviceList(i)
                                        If itm.DeviceID = d Then
                                            If itm.Kind = "TZBQ" Then
                                                Dim devTZBQ As DeviceTZBQ = CType(itm.cls, DeviceTZBQ)
                                                If IsNothing(devTZBQ) = False Then
                                                    devTZBQ.GetMyTask()
                                                End If
                                            End If
                                        End If
                                        If itm.DeviceID = d Then
                                            If itm.Kind = "TSS" Then
                                                Dim devTSS As DeviceTSS = CType(itm.cls, DeviceTSS)
                                                If IsNothing(devTSS) = False Then
                                                    devTSS.GetMyTask()
                                                End If
                                            End If
                                        End If
                                    Next
                                Next

                            End If

                        End SyncLock
                    End If
                Else
                    response(context, "result=" & "fail" & ";msg=null;errMsg=" & res & ";advise=null")
                End If
                Exit Sub

            Catch ex As Exception
                response(context, "result=fail;msg=null;errMsg=" & ex.Message & ";advise=null")
            End Try
            Exit Sub
        End If
        If func = "GetTaskInfoByTaskNickName" Then
            Try
                Dim TaskNickName As String = GetParaValue(GETSTR, "TaskNickName")
                If TaskNickName = "" Then
                    response(context, "[]")
                    Exit Sub
                End If
                Dim dt As DataTable = SQLGetDT("select * from UserTaskTable where TaskNickName='" & TaskNickName & "'")
                If IsNothing(dt) = False Then
                    If dt.Rows.Count > 0 Then
                        Dim json As String = JsonConvert.SerializeObject(dt)
                        response(context, json)
                        Exit Sub
                    Else
                        response(context, "[]")
                        Exit Sub
                    End If
                Else
                    response(context, "[]")
                    Exit Sub
                End If

            Catch ex As Exception
                response(context, "[]")
                Exit Sub
            End Try
            Exit Sub
        End If
        If func = "GetDeviceInfoByDeviceID" Then
            Try
                If deviceID = "" Then
                    response(context, "result=fail;msg=deviceID不能为空")
                    Exit Sub
                End If
                Dim json As String = ""
                SyncLock DeviceListLock
                    For Each d In DeviceList
                        If d.DeviceID = deviceID Then
                            json = JsonConvert.SerializeObject(d)
                            Exit For
                        End If
                    Next
                End SyncLock
                If json <> "" Then
                    response(context, json)
                    Exit Sub
                Else
                    response(context, "result=fail;msg=该设备ID不在线或者不存在")
                    Exit Sub
                End If
            Catch ex As Exception

            End Try
        End If
        If func = "GetDeviceInfoByDeviceName" Then
            Try
                Dim DeviceName As String = GetParaValue(GETSTR, "DeviceName")
                If DeviceName = "" Then
                    response(context, "result=fail;msg=DeviceName不能为空")
                    Exit Sub
                End If
                Dim json As String = ""
                SyncLock DeviceListLock
                    For Each d In DeviceList
                        If d.Name = DeviceName Then
                            json = JsonConvert.SerializeObject(d)
                            Exit For
                        End If
                    Next
                End SyncLock
                If json <> "" Then
                    response(context, json)
                    Exit Sub
                Else
                    response(context, "result=fail;msg=该设备名称不在线或者不存在")
                    Exit Sub
                End If
            Catch ex As Exception

            End Try
        End If
        If func = "GetMyTask" Then
            Try
                Dim userName As String = GetUsrByToken(token)
                Dim deviceListString = GetMyDevList(userName)
                If deviceListString = "" Or deviceListString.ToLower() = "all" Then
                    Dim dt As DataTable = SQLGetDT("select * from UserTaskTable order by StartTime")
                    Dim json As String = JsonConvert.SerializeObject(dt)
                    response(context, json)
                    Return
                End If
                If deviceListString <> "" Then
                    Dim sql As String = "select * from UserTaskTable where UserName='{0}' order by StartTime"
                    sql = String.Format(sql, userName)
                    Dim dt As DataTable = SQLGetDT(sql)
                    Dim json As String = JsonConvert.SerializeObject(dt)
                    response(context, json)
                    Return
                End If
                'Dim dt As DataTable = SQLGetDT("select * from UserTaskTable Where UserName='" & userName & "' order by StartTime")

            Catch ex As Exception
                response(context, "result=fail;msg=null;errMsg=" & ex.Message & ";advise=null")
            End Try
            Exit Sub
        End If
        If func = "GetAllTask" Then
            Try
                Dim dt As DataTable = SQLGetDT("select * from UserTaskTable order by StartTime")
                Dim json As String = JsonConvert.SerializeObject(dt)
                response(context, json)
            Catch ex As Exception
                response(context, "result=fail;msg=null;errMsg=" & ex.Message & ";advise=null")
            End Try
            Exit Sub
        End If
        If func = "GetOtherDeviceList" Then
            Try
                Dim dt As DataTable = SQLGetDT("select * from OtherDeviceTable order by DeviceKind")
                Dim json As String = JsonConvert.SerializeObject(dt)
                response(context, json)
            Catch ex As Exception
                response(context, "result=fail;msg=null;errMsg=" & ex.Message & ";advise=null")
            End Try
            Exit Sub
        End If
        If func = "GetDeviceMsg" Then
            Try
                Dim msgTime As String = GetParaValue(GETSTR, "msgTime")
                Dim msgfunc As String = GetParaValue(GETSTR, "msgfunc")
                If msgTime = "" Then
                    response(context, "msgTime is null")
                    Exit Sub
                End If
                Try
                    Dim t As Date = Date.Parse(msgTime)
                Catch ex As Exception
                    response(context, "result=fail;msg=msgTime is not a dateTime")
                    Exit Sub
                End Try
                Dim sql As String = String.Format("select * from DeviceMsgtable where DeviceID='{0}' and MsgTime ='{1}' order by MsgTime", New String() {deviceID, msgTime})
                If msgfunc = "SSSJ" Then
                    sql = String.Format("select * from DeviceMsgtable where DeviceID='{0}' and MsgTime ='{1}' order by MsgTime LIMIT 1", New String() {deviceID, msgTime})
                End If
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) Then
                    response(context, "[]")
                    Exit Sub
                End If
                If dt.Rows.Count = 0 Then
                    response(context, "[]")
                    Exit Sub
                End If
                Dim json As String = JsonConvert.SerializeObject(dt)
                response(context, json)
                Exit Sub
            Catch ex As Exception
                'response(context, ex.Message)
                'Exit Sub
            End Try
        End If
        If func = "GetDeviceMsgTimeList" Then
            Try
                Dim startTime As String = GetParaValue(GETSTR, "startTime")
                Dim endTime As String = GetParaValue(GETSTR, "endTime")
                Dim msgfunc As String = GetParaValue(GETSTR, "msgfunc")
                If startTime = "" Or endTime = "" Or deviceID = "" Then
                    response(context, "startTime or endTime or deviceID is null")
                    Exit Sub
                End If
                Dim sql As String = String.Format("select MsgTime from DeviceMsgtable where DeviceID='{0}' and MsgTime between '{1}' and '{2}' order by MsgTime", New String() {deviceID, startTime, endTime})
                If msgfunc <> "" Then
                    sql = String.Format("select  MsgTime from DeviceMsgtable where DeviceID='{0}' and Func='{1}' and MsgTime between '{2}' and '{3}'  order by MsgTime", New String() {deviceID, msgfunc, startTime, endTime})
                End If
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) Then
                    response(context, "[]")
                    Exit Sub
                End If
                If dt.Rows.Count = 0 Then
                    response(context, "[]")
                    Exit Sub
                End If
                Dim msg As String = ""
                For Each row As DataRow In dt.Rows
                    msg = msg & "," & row(0)
                Next
                If msg <> "" Then
                    msg = msg.Substring(1, msg.Length - 1)
                End If
                response(context, msg)
                Exit Sub
            Catch ex As Exception
                'response(context, ex.Message)
                'Exit Sub
            End Try
        End If
        If func = "GetDeviceMsgTimeListInfo" Then
            Try
                Dim startTime As String = GetParaValue(GETSTR, "startTime")
                Dim endTime As String = GetParaValue(GETSTR, "endTime")
                Dim msgfunc As String = GetParaValue(GETSTR, "msgfunc")
                If startTime = "" Or endTime = "" Or deviceID = "" Then
                    response(context, "startTime or endTime or deviceID is null")
                    Exit Sub
                End If
                Dim sql As String = String.Format("select count(*) MsgTime from DeviceMsgtable where DeviceID='{0}' and MsgTime between '{1}' and '{2}' order by MsgTime", New String() {deviceID, startTime, endTime})
                If msgfunc <> "" Then
                    sql = String.Format("select count(*) MsgTime from DeviceMsgtable where DeviceID='{0}' and Func='{1}' and MsgTime between '{2}' and '{3}'  order by MsgTime", New String() {deviceID, msgfunc, startTime, endTime})
                End If
                Dim str As String = SQLInfo(sql)
                response(context, str)
                Exit Sub
            Catch ex As Exception
                'response(context, ex.Message)
                'Exit Sub
            End Try
        End If
        If func = "GetDeviceMsgTimeListByIndex" Then
            Try
                Dim startTime As String = GetParaValue(GETSTR, "startTime")
                Dim endTime As String = GetParaValue(GETSTR, "endTime")
                Dim msgfunc As String = GetParaValue(GETSTR, "msgfunc")
                Dim startIndex As String = GetParaValue(GETSTR, "startIndex")
                Dim count As String = GetParaValue(GETSTR, "count")
                If startTime = "" Or endTime = "" Or deviceID = "" Or startIndex = "" Or count = "" Then
                    response(context, "startTime or endTime or deviceID is null")
                    Exit Sub
                End If
                Dim startIndex_int As Integer = Val(startIndex)
                Dim count_int As Integer = Val(count)
                Dim sql As String = String.Format("select  MsgTime from DeviceMsgtable where DeviceID='{0}' and MsgTime between '{1}' and '{2}' order by MsgTime limit " & startIndex_int + count_int, New String() {deviceID, startTime, endTime})
                If msgfunc <> "" Then
                    sql = String.Format("select MsgTime from DeviceMsgtable where DeviceID='{0}' and Func='{1}' and MsgTime between '{2}' and '{3}'  order by MsgTime limit " & startIndex_int + count_int, New String() {deviceID, msgfunc, startTime, endTime})
                End If
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) Then
                    response(context, "[]")
                    Exit Sub
                End If
                If dt.Rows.Count = 0 Then
                    response(context, "[]")
                    Exit Sub
                End If
                For j = startIndex_int - 1 To 0 Step -1
                    If dt.Rows.Count >= j Then
                        dt.Rows.RemoveAt(j)
                    End If
                Next
                Dim msg As String = ""
                For Each row As DataRow In dt.Rows
                    msg = msg & "," & row(0)
                Next
                If msg <> "" Then
                    msg = msg.Substring(1, msg.Length - 1)
                End If

                response(context, msg)
                Exit Sub
            Catch ex As Exception
                'response(context, ex.Message)
                'Exit Sub
            End Try
        End If
        If func = "GetWarn" Then
            Dim sql As String = ""
            Try
                Dim startTime As String = GetParaValue(GETSTR, "startTime")
                Dim endTime As String = GetParaValue(GETSTR, "endTime")
                Dim msgfunc As String = GetParaValue(GETSTR, "msgfunc")
                Dim startIndex As String = GetParaValue(GETSTR, "startIndex")
                Dim count As String = GetParaValue(GETSTR, "count")
                If startTime = "" Or endTime = "" Or startIndex = "" Or count = "" Then
                    response(context, "startTime or endTime or count is null")
                    Exit Sub
                End If
                Dim startIndex_int As Integer = Val(startIndex)
                Dim count_int As Integer = Val(count)

                If deviceID = "" Then
                    If msgfunc = "" Then
                        Sql = String.Format("select * from WarnTable where  MsgTime between '{0}' and '{1}' order by MsgTime limit " & startIndex_int + count_int, New String() {startTime, endTime})
                    Else
                        Sql = String.Format("select * from WarnTable where Func='{0}' and  MsgTime between '{1}' and '{2}' order by MsgTime limit " & startIndex_int + count_int, New String() {msgfunc, startTime, endTime})
                    End If
                Else
                    If msgfunc = "" Then
                        Sql = String.Format("select  * from WarnTable where DeviceID='{0}' and  MsgTime between '{1}' and '{2}' order by MsgTime limit " & startIndex_int + count_int, New String() {deviceID, startTime, endTime})
                    Else
                        Sql = String.Format("select  * from WarnTable where DeviceID='{0}' and Func='{1}' and  MsgTime between '{2}' and '{3}' order by MsgTime limit " & startIndex_int + count_int, New String() {deviceID, msgfunc, startTime, endTime})
                    End If
                End If

                Dim dt As DataTable = SQLGetDT(Sql)
                If IsNothing(dt) Then
                    response(context, "[]")
                    Exit Sub
                End If
                If dt.Rows.Count = 0 Then
                    response(context, "[]")
                    Exit Sub
                End If
                For j = startIndex_int - 1 To 0 Step -1
                    If dt.Rows.Count >= j Then
                        dt.Rows.RemoveAt(j)
                    End If
                Next
                ''''
                Dim userName As String = GetUsrByToken(token)
                Dim myDevicestring As String = GetMyDevList(userName)
                If myDevicestring = "all" Then
                    Dim json As String = JsonConvert.SerializeObject(dt)
                    response(context, json)
                    Exit Sub
                Else
                    Dim list As New List(Of String)
                    list = myDevicestring.Split(",").ToList()
                    If IsNothing(list) Then
                        response(context, "[]")
                        Exit Sub
                    Else
                        For j = dt.Rows.Count - 1 To 0 Step -1
                            Dim row As DataRow = dt.Rows(j)
                            Dim did As String = row("DeviceID").ToString()
                            If list.Contains(did) = False Then
                                dt.Rows.RemoveAt(j)
                            End If
                        Next
                        Dim json As String = JsonConvert.SerializeObject(dt)
                        response(context, json)
                        Exit Sub
                    End If
                End If
            Catch ex As Exception
                response(context, New NormalResponse(False, ex.ToString, sql, ""))
            End Try
        End If
        If func = "GetIsHaveMsg" Then
            Dim startTime As String = GetParaValue(GETSTR, "startTime")
            Dim endTime As String = GetParaValue(GETSTR, "endTime")
            Dim sDate, eDate As Date
            Try
                sDate = Date.Parse(startTime)
                eDate = Date.Parse(endTime)
                If eDate < sDate Then
                    Dim msg As String = "result=fail;msg=startTime can not later than endTime"
                    response(context, msg)
                    Exit Sub
                End If
                Dim list As New List(Of Boolean)
                While sDate <= eDate
                    Dim t1Date As Date = Date.Parse(sDate.ToString("yyyy-MM-dd") & " 00:00:00")
                    Dim t2Date As Date = Date.Parse(sDate.ToString("yyyy-MM-dd") & " 23:59:59")
                    Dim sql As String = "select msgTime from DeviceMsgTable where DeviceID='{0}' and msgTime between '{1}' and '{2}' order by msgTime limit 1"
                    sql = String.Format(sql, deviceID, t1Date.ToString("yyyy-MM-dd HH:mm:ss"), t2Date.ToString("yyyy-MM-dd HH:mm:ss"))
                    'log(sql)
                    Dim dt As DataTable = SQLGetDT(sql)
                    If IsNothing(dt) Then
                        list.Add(False)
                    Else
                        If dt.Rows.Count = 0 Then
                            list.Add(False)
                        Else
                            list.Add(True)
                        End If
                    End If
                    sDate = sDate.AddDays(1)
                End While
                Dim json As String = JsonConvert.SerializeObject(list)
                response(context, json)
                Exit Sub
            Catch ex As Exception
                Dim msg As String = "result=fail;msg=startTime or endTime is not a dateTime"
                response(context, msg)
                Exit Sub
            End Try
        End If
        If func = "GetFreqPointTimeSeq" Then
            Dim startTime As String = GetParaValue(GETSTR, "startTime")
            Dim endTime As String = GetParaValue(GETSTR, "endTime")
            Dim freq As String = GetParaValue(GETSTR, "freq")
            Dim sDate, eDate As Date
            Try
                sDate = Date.Parse(startTime)
                eDate = Date.Parse(endTime)
                If eDate < sDate Then
                    Dim msg As String = "result=fail;msg=startTime can not later than endTime"
                    response(context, msg)
                    Exit Sub
                End If
                If freq = "" Then
                    Dim msg As String = "result=fail;msg=freq is null"
                    response(context, msg)
                    Exit Sub
                End If
                If IsNumeric(freq) = False Then
                    Dim msg As String = "result=fail;msg=freq is not a number"
                    response(context, msg)
                    Exit Sub
                End If
                If freq <= 0 Then
                    Dim msg As String = "result=fail;msg=freq<=0"
                    response(context, msg)
                    Exit Sub
                End If
                Dim sql As String = "select msgTime,DeviceMsg from DeviceMsgTable where DeviceID='{0}' and func='bscan' and msgTime between '{1}' and '{2}' order by msgTime"
                sql = String.Format(sql, deviceID, startTime, endTime)
                'log(sql)
                Dim dt As DataTable = SQLGetDT(sql)
                If IsNothing(dt) = False Then
                    Dim list As New List(Of Double)
                    For Each row As DataRow In dt.Rows
                        Dim time As String = row("MsgTime")
                        Dim deviceMsg As String = row("DeviceMsg")
                        Dim v As Double = GetFreqValueByFreqJson(deviceMsg, freq)
                        list.Add(v)
                    Next
                    Dim json As String = JsonConvert.SerializeObject(list)
                    response(context, json)
                    Exit Sub
                End If
            Catch ex As Exception
                Dim msg As String = "result=fail;msg=startTime or endTime is not a dateTime;errmsg=" & ex.Message
                response(context, msg)
                Exit Sub
            End Try
        End If
        If func = "GetBusLines" Then
            HandleGetBusLines(context)
            Return
        End If
        Dim isHandleOver As Boolean = False
        Try
            'Dim n As New HTTPHandle()
            'Dim t As Type = n.GetType()
            'Dim obj As Object = Activator.CreateInstance(t)
            'Dim mf As MethodInfo = t.GetMethod("Handle_" & func)
            'Dim ok As Object = mf.Invoke(obj, New Object() {context})
            'Dim result As Boolean = CType(ok, Boolean)
            'isHandleOver = result
            Dim n As New HTTPHandle()
            Dim t As Type = n.GetType()
            Dim obj As Object = Activator.CreateInstance(t)
            Dim mf As MethodInfo = t.GetMethod("Handle_" & func)
            Dim ok As Object = mf.Invoke(obj, New Object() {context})
            Dim np As NormalResponse = CType(ok, NormalResponse)
            If IsNothing(np) = False Then
                isHandleOver = True
                response(context, np)
                Return
            End If
        Catch ex As Exception

        End Try
        If isHandleOver Then Return
        response(context, "Please Send Some QueryString With in The Url ^_^" & ";Your Url=" & GETSTR)
    End Sub

    Private Function GetTaskDeviceKind(taskDeviceId As String) As String
        If taskDeviceId = "" Then Return ""
        If taskDeviceId.Substring(0, 1) = "[" Then
            If taskDeviceId.Substring(taskDeviceId.Length - 1, 1) = "]" Then
                Try
                    Dim list As List(Of String) = JsonConvert.DeserializeObject(taskDeviceId, GetType(List(Of String)))
                    If IsNothing(list) = False Then
                        If list.Count > 0 Then taskDeviceId = list(0)
                    End If
                Catch ex As Exception

                End Try
            End If
        End If
        SyncLock DeviceListLock
            If IsNothing(DeviceList) = False Then
                For Each d In DeviceList
                    If d.DeviceID = taskDeviceId Then
                        Return d.Kind
                        Exit For
                    End If
                Next
            End If
        End SyncLock
        Return ""
    End Function
    Structure PostStu
        Dim func As String
        Dim msg As String
        Dim token As String
    End Structure
    Structure emailStu
        Dim address As String
        Dim title As String
        Dim body As String
        Dim isHaveFile As Boolean
        Dim TaskNickName As String
    End Structure
    Private Sub HandlePost(ByVal context As HttpListenerContext)
        Dim sr As New StreamReader(context.Request.InputStream, Encoding.UTF8)
        Dim body As String = sr.ReadToEnd
        Try
            Dim ps As PostStu = JsonConvert.DeserializeObject(body, GetType(PostStu))
            Dim func As String = ps.func
            Dim msg As String = ps.msg
            Dim token As String = ps.token
            If islogined(token) = False Then
                response(context, "result=fail;msg=Please login")
                Exit Sub
            End If
            Dim usr As String = GetUsrByToken(token)
            If func = "sendEmail" Then
                Try
                    Dim s As emailStu = JsonConvert.DeserializeObject(msg, GetType(emailStu))
                    If IsNothing(s) Then
                        response(context, "result=fail;msg=格式有误")
                        Return
                    End If
                    Dim addressTmp As String = s.address
                    Dim titleTmp As String = s.title
                    Dim bodyTmp As String = s.body
                    Dim TaskNickName As String = s.TaskNickName
                    Dim ht As New Hashtable
                    If s.isHaveFile Then
                        Dim sql As String = "select ResultReportUrl from userTaskTable where TaskNickName='{0}'"
                        sql = String.Format(sql, TaskNickName)
                        Dim dt As DataTable = SQLGetDT(sql)
                        If IsNothing(dt) = False Then
                            If dt.Rows.Count > 0 Then
                                Dim url As String = dt.Rows(0)("ResultReportUrl").ToString()
                                Dim filePath As String = url.Replace("http://123.207.31.37:8082", "")
                                Dim fileName As String = Directory.GetCurrentDirectory() & filePath
                                If File.Exists(fileName) Then
                                    ht.Add(TaskNickName & "监测报告.docx", fileName)
                                End If
                            End If
                        End If
                    End If
                    If ht.Count <> 0 Then
                        Dim msgTmp As String = SendMail(addressTmp, titleTmp, bodyTmp, ht)
                        If msgTmp = "发送邮件成功" Then
                            response(context, "result=success;msg=发送邮件成功")
                            Return
                        Else
                            response(context, "result=fail;msg=发送邮件失败")
                            Return
                        End If
                    Else
                        Dim msgTmp As String = SendMail(addressTmp, titleTmp, bodyTmp, Nothing)
                        If msgTmp = "发送邮件成功" Then
                            response(context, "result=success;msg=发送邮件成功")
                            Return
                        Else
                            response(context, "result=fail;msg=发送邮件失败")
                            Return
                        End If
                    End If
                Catch ex As Exception
                    response(context, "result=fail;msg=格式有误")
                    Return
                End Try
                response(context, "result=fail;msg=发送邮件失败")
                Return
            End If
            If func = "SetSysConf" Then
                Try
                    Dim conf As String = ps.msg
                    If conf = "" Then
                        response(context, "result=fail;msg=conf字段不能为空，应为json")
                        Return
                    End If
                    Try
                        Dim tmp As myServerInfoStu = JsonConvert.DeserializeObject(conf, GetType(myServerInfoStu))
                        If IsNothing(tmp) Then
                            response(context, "result=fail;msg=conf json格式不对")
                            Return
                        End If
                        If tmp.HTTPMsg_IpUrl = "" Then
                            response(context, "result=fail;msg=HTTPMsg_IpUrl不可为空“)
                            Return
                        End If
                        'If tmp.isMain = "" Then
                        '    response(context, "result=fail;msg=isMain不可为空“)
                        '    Return
                        'End If
                        'If tmp.isSaveDevMsg = "" Then
                        '    response(context, "result=fail;msg=isSaveDevMsg不可为空“)
                        '    Return
                        'End If
                        If tmp.MainServerIP = "" Then
                            response(context, "result=fail;msg=MainServerIP不可为空“)
                            Return
                        End If
                        If tmp.MainServerPort <= 0 Then
                            response(context, "result=fail;msg=MainServerPort不可<=0“)
                            Return
                        End If
                        If tmp.MZHDeviceID = "" Then
                            response(context, "result=fail;msg=MZHDeviceID不可为空“)
                            Return
                        End If
                        If tmp.SQLDataBaseName = "" Then
                            response(context, "result=fail;msg=SQLDataBaseName不可为空“)
                            Return
                        End If
                        If tmp.SQLPwd = "" Then
                            response(context, "result=fail;msg=SQLPwd不可为空“)
                            Return
                        End If

                        If tmp.SQLServerIP = "" Then
                            response(context, "result=fail;msg=SQLServerIP不可为空“)
                            Return
                        End If
                        If tmp.SQLUid = "" Then
                            response(context, "result=fail;msg=SQLUid不可为空“)
                            Return
                        End If
                        If tmp.TZBQPort <= 0 Then
                            response(context, "result=fail;msg=TZBQPort不可<=0“)
                            Return
                        End If
                        If tmp.TSSPort <= 0 Then
                            response(context, "result=fail;msg=TSSPort不可<=0“)
                            Return
                        End If
                        'If tmp.HTTPMsg_IpUrl = "" Or
                        '   tmp.isMain = Nothing Or
                        '   tmp.isSaveDevMsg = Nothing Or
                        '   tmp.MainServerIP = "" Or
                        '   tmp.MainServerPort <= 0 Or
                        '   tmp.MZHDeviceID = "" Or
                        '   tmp.SQLDataBaseName = "" Or
                        '   tmp.SQLPwd = "" Or
                        '   tmp.SQLServerIP = "" Or
                        '   tmp.SQLUid = "" Or
                        '   tmp.TSSPort <= 0 Or
                        '   tmp.TZBQPort <= 0 Then
                        '    response(context, "result=fail;msg=conf任何参数均不可为空;your conf parsed will be=" & JsonConvert.SerializeObject(tmp))
                        '    Return
                        'End If
                        SyncLock myServerInfoLock
                            myServerInfo = Nothing
                            myServerInfo = tmp
                            SaveMyServerInfo()
                            response(context, "result=success;msg=设置成功")
                            Return
                        End SyncLock

                        Return
                    Catch ex As Exception
                        response(context, "result=fail;msg=" & ex.Message)
                        Return
                    End Try
                    response(context, "result=fail;msg=null")
                    Return
                Catch ex As Exception

                End Try
            End If
            If func = "UploadRunImage" Then
                Try
                    Dim jr As JTStu = JsonConvert.DeserializeObject(msg, GetType(JTStu))
                    If IsNothing(jr) Then
                        response(context, "result=" & "fail" & ";msg=" & "截图信息为空，请重试" & ";errMsg=" & "" & ";advise=null")
                        Exit Sub
                    End If
                    Dim imgName As String = jr.imgName
                    Dim imgText As String = jr.imgText
                    Dim userName As String = jr.userName
                    Dim tUserName As String = GetUsrByToken(token)
                    If userName <> tUserName Then
                        response(context, "result=fail;msg=用户名与登录信息不匹配")
                        Exit Sub
                    End If
                    Dim base64 As String = jr.base64
                    Dim sql As String = "insert into imgTable values('{0}','{1}','{2}','{3}')"
                    sql = String.Format(sql, New String() {imgName, imgText, userName, base64})
                    Dim result As String = SQLCmd(sql)
                    If result = "success" Then
                        AddJobLog(userName, "值班员记事", "上传截图", "[" & imgName & "]" & imgText, "", "", "", imgName)
                        response(context, "result=" & "success" & ";msg=" & "" & ";errMsg=" & "" & ";advise=null")
                        Return
                    Else
                        response(context, "result=" & "fail" & ";msg=" & "" & ";errMsg=" & result & "" & ";advise=null")
                        Return
                    End If
                Catch ex As Exception
                    response(context, "result=" & "fail" & ";msg=" & "截图json格式有误，请重试" & ";errMsg=" & "" & ";advise=null")
                    Exit Sub
                End Try
            End If
            If func = "UpdateJobInfo" Then
                Try
                    Dim jobs As jobStu = JsonConvert.DeserializeObject(msg, GetType(jobStu))
                    If IsNothing(jobs) Then
                        response(context, "result=" & "fail" & ";msg=" & "json格式有误" & ";errMsg=" & "" & ";advise=null")
                        Exit Sub
                    End If
                    Dim sql As String = "update JobTable set Title='{0}'," &
                                        "Content='{1}'," &
                                        "JobFrom='{2}'," &
                                        "Status='{3}'," &
                                        "StartTime='{4}'," &
                                        "EndTime='{5}'," &
                                        "Job='{6}'," &
                                        "AlarmJob='{7}'," &
                                        "DeviceID='{8}'," &
                                        "TaskNickName='{9}' " & "where JobID='{10}'"
                    sql = String.Format(sql, New String() {jobs.Title, jobs.Content, jobs.JobFrom, jobs.Status, jobs.StartTime, jobs.EndTime, jobs.Job, jobs.AlarmJob, jobs.DeviceID, jobs.TaskNickName, jobs.JobID})
                    Dim result As String = SQLCmd(sql)
                    If result = "success" Then

                        response(context, "result=" & "success" & ";msg=" & "")
                        Exit Sub
                    Else
                        response(context, "result=" & "fail" & ";msg=" & result)
                        Exit Sub
                    End If
                Catch ex As Exception
                    response(context, "result=" & "fail" & ";msg=" & "json格式有误" & ";errMsg=" & ex.Message & ";advise=null")
                    Exit Sub
                End Try
            End If
            If func = "UpdateLogInfo" Then
                Try
                    Dim s As logstu = JsonConvert.DeserializeObject(msg, GetType(logstu))
                    If IsNothing(s) Then
                        response(context, "result=" & "fail" & ";msg=" & "json格式有误" & ";errMsg=" & "" & ";advise=null")
                        Exit Sub
                    End If
                    If s.LogID = "" Then
                        response(context, "result=" & "fail" & ";msg=" & "LogID不能为空" & ";errMsg=" & "" & ";advise=null")
                        Exit Sub
                    End If
                    Dim sql As String = "update LogTable set Kind='{0}'," &
                                        "Cata='{1}'," &
                                        "Content='{2}'," &
                                        "DeviceNickName='{3}'," &
                                        "DeviceID='{4}'," &
                                        "TaskNickName='{5}' " & "where LogID='{6}'"
                    sql = String.Format(sql, New String() {s.Kind, s.Cata, s.Content, s.DeviceNickName, s.DeviceID, s.TaskNickName, s.LogID})
                    Dim result As String = SQLCmd(sql)
                    If result = "success" Then
                        response(context, "result=" & "success" & ";msg=" & "")
                        Exit Sub
                    Else
                        response(context, "result=" & "fail" & ";msg=" & result)
                        Exit Sub
                    End If
                Catch ex As Exception
                    response(context, "result=" & "fail" & ";msg=" & "json格式错误" & ";errMsg=" & ex.Message & ";advise=null")
                    Exit Sub
                End Try
            End If
            If func = "AddLog" Then
                Try
                    Dim userName As String = GetUsrByToken(token)
                    Dim s As logstu = JsonConvert.DeserializeObject(msg, GetType(logstu))
                    If IsNothing(s) Then
                        response(context, "result=" & "fail" & ";msg=" & "json格式有误" & ";errMsg=" & "" & ";advise=null")
                        Exit Sub
                    End If
                    Dim sql As String = "insert into LogTable (Time,Usr,Kind,Content,DeviceNickName,DeviceID,TaskNickName,Cata) values('{0}','{1}','{2}','{3}','{4}','{5}','{6}','{7}')"
                    'Dim DeviceNickName As String = s.DeviceNickName
                    'Dim DeviceID As String = s.DeviceID
                    'SyncLock DeviceListLock
                    '    If IsNothing(device
                    'End SyncLock
                    sql = String.Format(sql, New String() {s.Time, userName, s.Kind, s.Content, s.DeviceNickName, s.DeviceID, s.TaskNickName, s.Cata})
                    Dim result As String = SQLCmd(sql)
                    If result = "success" Then
                        response(context, "result=" & "success" & ";msg=" & "录入成功")
                        Exit Sub
                    Else
                        response(context, "result=" & "fail" & ";msg=" & "" & ";errMsg=" & result)
                        Exit Sub
                    End If
                Catch ex As Exception
                    response(context, "result=" & "fail" & ";msg=" & "POST格式有误" & ";errMsg=" & "" & ";advise=null")
                    Exit Sub
                End Try
            End If
            If func = "AddJob" Then
                Try
                    Dim userName As String = GetUsrByToken(token)
                    Dim jobs As jobStu = JsonConvert.DeserializeObject(msg, GetType(jobStu))
                    If IsNothing(jobs) Then
                        response(context, "result=" & "fail" & ";msg=" & "json格式有误" & ";errMsg=" & "" & ";advise=null")
                        Exit Sub
                    End If
                    Dim sql As String = "select Title from JobTable where Title='" & jobs.Title & "'"
                    Dim dt As DataTable = SQLGetDT(sql)
                    If IsNothing(dt) = False Then
                        If dt.Rows.Count > 0 Then
                            response(context, "result=" & "fail" & ";msg=" & "该标题已存在")
                            Exit Sub
                        End If
                    End If
                    sql = "insert into JobTable (Time,JobFrom,Department,Worker,Title,Content,Status,StartTime,EndTime,Job,AlarmJob,DeviceID,TaskNickName,FileUrl,Submiter) values('{0}','{1}','{2}','{3}','{4}','{5}','{6}','{7}','{8}','{9}','{10}','{11}','{12}','{13}','{14}')"
                    Dim fileUrl As String = ""
                    If jobs.fileName <> "" Then
                        If jobs.fileBase64 <> "" Then
                            If jobs.fileBase64.Length <= 5 * 1024 * 1024 Then
                                Dim fileName As String = GetNewFileName(jobs.fileName)
                                Dim filePath As String = Directory.GetCurrentDirectory() & "\JobFiles\" & fileName
                                Try
                                    Dim by() As Byte = Convert.FromBase64String(jobs.fileBase64)
                                    If IsNothing(by) Then
                                        response(context, "result=" & "fail" & ";msg=" & "附件传输错误")
                                        Exit Sub
                                    End If
                                    Dim stream As New FileStream(filePath, FileMode.Create)
                                    Dim bw As New BinaryWriter(stream)
                                    bw.Write(by)
                                    bw.Close()
                                    stream.Close()
                                Catch ex As Exception
                                    response(context, "result=" & "fail" & ";msg=" & "附件储存错误")
                                    Exit Sub
                                End Try
                                fileUrl = "http://123.207.31.37:8082/JobFiles/" & fileName
                            Else
                                response(context, "result=" & "fail" & ";msg=" & "附件不能超过5M")
                                Exit Sub
                            End If
                        End If
                    End If
                    sql = String.Format(sql, New String() {jobs.Time, jobs.JobFrom, jobs.Department, jobs.Worker, jobs.Title, jobs.Content, jobs.Status, jobs.StartTime, jobs.EndTime, jobs.Job, jobs.AlarmJob, jobs.DeviceID, jobs.TaskNickName, fileUrl, userName})
                    Dim result As String = SQLCmd(sql)
                    If result = "success" Then
                        response(context, "result=" & "success" & ";msg=" & "录入成功")
                        Exit Sub
                    Else
                        response(context, "result=" & "fail" & ";msg=" & "" & ";errMsg=" & result)
                        Exit Sub
                    End If
                Catch ex As Exception
                    'MsgBox(ex.ToString)
                    response(context, "result=" & "fail" & ";msg=" & "json格式有误" & ";errMsg=" & ex.Message & ";advise=null")
                    Exit Sub
                End Try
            End If
            Dim isHandleOver As Boolean = False
            Try
                Dim n As New HTTPHandle()
                Dim t As Type = n.GetType()
                Dim obj As Object = Activator.CreateInstance(t)
                Dim mf As MethodInfo = t.GetMethod("Handle_" & func)
                Dim ok As Object = mf.Invoke(obj, New Object() {usr, msg})
                Dim np As NormalResponse = CType(ok, NormalResponse)
                If IsNothing(np) = False Then
                    isHandleOver = True
                    response(context, np)
                    Return
                End If
            Catch ex As Exception

            End Try
            If isHandleOver Then Return
        Catch ex As Exception
            response(context, "result=" & "fail" & ";msg=" & "POST格式有误" & ";errMsg=" & "" & ";advise=null")
            Exit Sub
        End Try
        response(context, "^_^")
    End Sub
    Private Sub HandleGetBusLines(context As HttpListenerContext)
        Try
            Dim sql As String = "select * from BusLineTable"
            Dim dt As DataTable = SQLGetDT(sql)
            If dt.Rows.Count = 0 Then response(context, "[]")
            Dim json As String = JsonConvert.SerializeObject(dt)
            response(context, json)
        Catch ex As Exception
            response(context, "[]")
        End Try
    End Sub

    Private Function GetNewFileName(ByVal fName As String) As String
        Dim filePath As String = Directory.GetCurrentDirectory() & "\JobFiles"
        CheckDir(filePath)
        Dim int As Integer = 1
        Dim nName As String = fName
        While True
            Dim nfilePath As String = filePath + "\" + nName
            If File.Exists(nfilePath) Then
                Dim f As New FileInfo("c:\" & fName)
                Dim ext As String = f.Extension
                Dim realName As String = f.Name.Replace(ext, "")
                nName = realName & "(" & int & ")" & ext
                int = int + 1
            Else
                Return nName
            End If
        End While
    End Function
    Structure jFreq
        Dim freqStart As Decimal
        Dim freqStep As Decimal
        Dim dataCount As Integer
        Dim value() As Decimal
    End Structure
    Private Function GetFreqValueByFreqJson(ByVal fj As String, ByVal v As Decimal) As Single
        Dim jp As JObject = JsonConvert.DeserializeObject(fj, GetType(JObject))
        Dim msg As String = jp("msg")
        Dim jf As jFreq = JsonConvert.DeserializeObject(msg, GetType(jFreq))
        Dim freqStart As Decimal = jf.freqStart
        Dim freqStep As Single = jf.freqStep
        Dim count As Integer = jf.dataCount
        Dim freqEnd As Decimal = freqStart + freqStep * (count - 1)
        Dim value() As Decimal = jf.value
        If count <> value.Count Then Return 0
        If v < freqStart Then Return 0
        If v > freqEnd Then Return 0
        If v = freqStart Then Return value(0)
        If v = freqEnd Then Return value(count - 1)

        If (v - freqStart) Mod freqStep <> 0 Then
            Return 0
        End If
        Dim index As Integer = (v - freqStart) / freqStep
        If index < count Then
            Return value(index)
        End If
    End Function
    Structure GetDeviceMsgTimeListInfo
        Dim count As Long
        Dim txtLength As Long
        Sub New(ByVal _count As Single, ByVal _txtLength As Single)
            count = _count
            txtLength = _txtLength
        End Sub
    End Structure
    Private Function GetHttpMsgUrlById(ByVal id As String) As String
        If IsNothing(DeviceListLock) Then DeviceListLock = New Object
        Dim result As String = ""
        If id = "" Then Return ""
        SyncLock DeviceListLock
            If IsNothing(DeviceList) = False Then
                For Each d In DeviceList
                    If d.DeviceID = id Then
                        result = d.HTTPMsgUrl
                        Exit For
                    End If
                Next
            End If
        End SyncLock
        If result = "" Then Return result
        result = result.Replace("http://+:", HTTPMsg_IpUrl)
        Return result
    End Function
    Private Function GetHttpMsgUrlByDeviceName(ByVal DeviceName As String) As String
        If IsNothing(DeviceListLock) Then DeviceListLock = New Object
        Dim result As String = ""
        If DeviceName = "" Then Return ""
        SyncLock DeviceListLock
            If IsNothing(DeviceList) = False Then
                For Each d In DeviceList
                    If d.Name = DeviceName Then
                        result = d.HTTPMsgUrl
                        Exit For
                    End If
                Next
            End If
        End SyncLock
        If result = "" Then Return result
        result = result.Replace("http://+:", HTTPMsg_IpUrl)
        Return result
    End Function
    Private Function sendMsgToTSSDevById(ByVal id As String, ByVal a As String, ByVal b As String, ByVal c As String, ByVal d As String, ByVal e As Byte()) As String
        'Return TSSServer.SendMsgToTSSDevByID(id, a, b, c, d, e)
    End Function
    Private Sub HandleAutoUpdate(ByVal Context As HttpListenerContext)
        Dim GETSTR As String = UrlDecode(Context.Request.Url.PathAndQuery)
        Dim exename As String = GetParaValue(GETSTR, "exename")
        Dim updateFunc As String = GetParaValue(GETSTR, "updateFunc")
        Dim version As String = GetParaValue(GETSTR, "version")
        If updateFunc = "getexelen" Then
            If exename = "" Then
                response(Context, "result=fail;msg=没有可用升级")
                Return
            End If
            Dim filePath As String = "update/" & exename.Replace(".exe", "") & "/" & exename
            If File.Exists(filePath) = False Then
                response(Context, "result=fail;msg=没有可用升级")
                Return
            End If
            Dim finfo As New FileInfo(filePath)
            Dim len As Long = finfo.Length
            response(Context, "result=success;msg=" & len)
            Return
        End If
        If updateFunc = "downloadexe" Then
            If exename = "" Then
                response(Context, "result=fail;msg=没有可用升级")
                Return
            End If
            Dim filePath As String = "update/" & exename.Replace(".exe", "") & "/" & exename
            If File.Exists(filePath) = False Then
                response(Context, "result=fail;msg=没有可用升级")
                Return
            End If
            Dim stream As New FileStream(filePath, FileMode.Open)
            Dim br As New BinaryReader(stream)
            Dim f As New FileInfo(filePath)
            Dim length As Double = f.Length
            Dim buffer() As Byte = br.ReadBytes(length)
            br.Close()
            stream.Close()
            If IsNothing(buffer) Then
                response(Context, "result=fail;msg=没有可用升级")
            Else
                Context.Response.OutputStream.Write(buffer, 0, buffer.Length)
                Context.Response.OutputStream.Close()
                Return
            End If
        End If
        If updateFunc = "getupdate" Then
            If version = "" Then
                response(Context, "result=fail;msg=没有可用升级")
                Return
            End If
            If exename = "" Then
                response(Context, "result=fail;msg=没有可用升级")
                Return
            End If
            Dim filePath As String = "update/" & exename.Replace(".exe", "") & "/" & exename
            If File.Exists(filePath) = False Then
                response(Context, "result=fail;msg=没有可用升级")
                Return
            End If
            Dim versionInfoPath As String = "update/" & exename.Replace(".exe", "") & "/" & "version.txt"
            Dim sr As New StreamReader(versionInfoPath, Encoding.Default)
            Dim newVersion As String = sr.ReadToEnd
            sr.Close()
            Try
                Dim iscanupdate As Boolean = CanUpdate(version, newVersion)
                If iscanupdate = False Then
                    response(Context, "result=fail;msg=没有可用升级")
                    Return
                Else
                    response(Context, "result=success;msg=有可用更新")
                    Return
                End If
            Catch ex As Exception

            End Try
        End If
    End Sub
    Private Sub HandleGetUpdate(ByVal Context As HttpListenerContext)
        Dim GETSTR As String = UrlDecode(Context.Request.Url.PathAndQuery)
        Dim func As String = GetParaValue(GETSTR, "func")
        Dim datamsg As String = GetParaValue(GETSTR, "datamsg")
        Dim exename As String = GetParaValue(GETSTR, "exename")
        Dim deviceID As String = GetParaValue(GETSTR, "deviceID")
        Dim group As String = GetParaValue(GETSTR, "group")
        If func = "getUpdateExeLength" Then
            If exename = "" Then
                response(Context, "please addQueryString 'exename'")
            Else
                If exename = "DianCiXinXiFuWu.exe" Then
                    Dim path As String = Directory.GetCurrentDirectory() & "\update\电磁信息服务\电磁信息云服务.exe"
                    If File.Exists(path) Then
                        Dim f As New FileInfo(path)
                        Dim length As Double = f.Length

                        response(Context, length)
                    Else
                        response(Context, "没有可用更新")
                    End If
                Else
                    Dim path As String = Directory.GetCurrentDirectory() & "\update\电磁信息服务\" & exename
                    '   log("exename=" & exename)
                    If File.Exists(path) Then
                        Dim f As New FileInfo(path)
                        Dim length As Double = f.Length

                        response(Context, length)
                    Else
                        response(Context, "没有可用更新")
                    End If
                End If
            End If
            Exit Sub
        End If
        If func = "getUpdate" Then
            If exename = "" Then
                response(Context, "please addQueryString 'exename'")
            Else
                If exename = "DianCiXinXiFuWu.exe" Then
                    Dim path As String = Directory.GetCurrentDirectory() & "\update\电磁信息服务\电磁信息云服务.exe"
                    If File.Exists(path) Then
                        Dim stream As New FileStream(path, FileMode.Open)
                        Dim br As New BinaryReader(stream)
                        Dim f As New FileInfo(path)
                        Dim length As Double = f.Length
                        Dim buffer() As Byte = br.ReadBytes(length)
                        br.Close()
                        stream.Close()
                        If IsNothing(buffer) Then
                            response(Context, "没有可用更新")
                        Else
                            Context.Response.OutputStream.Write(buffer, 0, buffer.Length)
                            Context.Response.OutputStream.Close()
                        End If
                    Else
                        response(Context, "没有可用更新")
                    End If
                Else
                    Dim path As String = Directory.GetCurrentDirectory() & "\update\电磁信息服务\" & exename
                    If File.Exists(path) Then
                        Dim stream As New FileStream(path, FileMode.Open)
                        Dim br As New BinaryReader(stream)
                        Dim f As New FileInfo(path)
                        Dim length As Double = f.Length
                        Dim buffer() As Byte = br.ReadBytes(length)
                        br.Close()
                        stream.Close()
                        If IsNothing(buffer) Then
                            response(Context, "没有可用更新")
                        Else
                            Context.Response.OutputStream.Write(buffer, 0, buffer.Length)
                            Context.Response.OutputStream.Close()
                        End If
                    Else
                        response(Context, "没有可用更新")
                    End If
                End If

            End If
            Exit Sub
        End If
    End Sub
    Private Function CheckTSS_JCPD(ByVal pds() As String) As Boolean
        If pds.Count = 0 Then Return False
        Dim ds As New List(Of Double)
        For Each sh In pds
            If IsNumeric(sh) = False Then
                Return False
            Else
                Dim v As Double = Val(sh)
                ds.Add(v)
            End If
        Next
        If ds.Count = 1 Then Return True
        If ds.Count = 2 Then Return True
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
        Dim stepd As Double = ds(1) - ds(0)
        For i = 2 To ds.Count - 1
            Dim ns As Double = ds(i) - ds(i - 1)
            If ns <> stepd Then
                Return False
            End If
        Next
        Return True
    End Function

End Class

