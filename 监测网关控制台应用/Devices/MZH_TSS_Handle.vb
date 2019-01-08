Imports System
Imports System.IO
Imports System.Text
Imports System.Threading
Imports System.Threading.Thread
Imports Newtonsoft
Imports Newtonsoft.Json
Imports Newtonsoft.Json.Linq
Imports System.Net
Imports System.Net.Sockets
Imports System.Runtime.InteropServices
''' <summary>
''' 监测网关主要处理程序
''' </summary>

Public Class MZH_TSS_Handle
    Private serverIP As String
    Private serverPort As Integer
    Private deviceID As String
    Private tcpcl As TcpClient
    Private flag_isConnected As Boolean = False
    Private flag_isConnecting As Boolean = False

    Sub New(_serverIP As String, _serverPort As Integer, _deviceID As String)
        serverIP = _serverIP
        serverPort = _serverPort
        deviceID = _deviceID
    End Sub
    Public Sub TSS_Handle_Start()
        flag_MzhHandle = True
        Dim th As New Thread(AddressOf ConnectToSever)
        th.Start()
    End Sub
    Private Sub handleTM(ByVal buffer() As Byte)  ''''处理服务器发过来的命令
        If IsNothing(buffer) Then Exit Sub
        Dim tm As tssMsg = byte2tssmsg(buffer)
        If IsNothing(tm) Then Exit Sub
        If tm.datatype = "DevMsg" And tm.functype = "Test" And tm.canshuqu = "<info:func=;>" Then
            'log("收到心跳命令")
            sendMsg2TSSServer("DevMsg", "Test", "", Nothing)
        End If
        If tm.datatype = "task" And tm.functype = "bscan" Then   '频谱
            DeviceStop()
            Sleep(1000)
            Dim str As String = tm.canshuqu
            Dim freqbegin As String = getValueByCanShuQu(str, "freqbegin")
            Dim freqend As String = getValueByCanShuQu(str, "freqend")
            Dim freqstep As String = getValueByCanShuQu(str, "abw")
            Dim Bscan_freqBegin As Double = Val(freqbegin)
            Dim Bscan_freqEnd As Double = Val(freqend)
            Dim Bscan_freqStep As Double = Val(freqstep)
            log("收到频谱命令，Bscan_freqBegin=" & Bscan_freqBegin & ",Bscan_freqEnd=" & Bscan_freqEnd & ",Bscan_freqStep=" & Bscan_freqStep)
        End If
        If tm.datatype = "task" And tm.functype = "ifscan_wav" Then
            'STOPDevice()
            'Dim str As String = tm.canshuqu
            'Dim freq As String = getValueByCanShuQu(str, "freq")
            'If freq = "" Then
            '    freq = getValueByCanShuQu(str, "mod_fre")
            'End If
            'setMode(1)
            'Sleep(100)
            'setShePinCanShu(Val(freq), Val(freq))
            'Sleep(100)
            'setShiYuCanShu(Val(freq))
            'Sleep(100)
            'startDataTransfor()
        End If
        If tm.datatype = "task" And tm.functype = "taskctrl" And tm.canshuqu = "<taskctrl:taskstate=stop;>" Then
            log("收到设备停止命令")
            DeviceStop()
        End If
        If tm.datatype = "task" And tm.functype = "ingmsg" Then
            Dim im As INGMsgStu = TSSShuju2INGMsg(tm)
            If IsNothing(im) Then Return
            Try
                HandleINGMsg(im)
            Catch ex As Exception

            End Try
        End If
    End Sub
    Structure bscanStu
        Dim deviceID As String
        Dim task As String
        Dim freqStart As Double
        Dim freqEnd As Double
        Dim freqStep As Double
    End Structure
    Private Sub HandleINGMsg(INGMsg As INGMsgStu) ''''处理服务器发过来  INGMsg  的命令
        Dim MsgID As String = INGMsg.MsgID
        Dim MsgTime As String = INGMsg.MsgTime
        Dim func As String = INGMsg.func
        Dim msg As String = INGMsg.msg
        If func = "GetDevList" Then
            Dim json As String = ""
            SyncLock DeviceListLock
                If IsNothing(DeviceList) = False Then
                    Dim newlist As List(Of deviceStuWithOutHttpListener) = TransForDeviceStu(DeviceList)
                    json = JsonConvert.SerializeObject(newlist)
                End If
            End SyncLock
            Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, json)
            sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
            Return
        End If
        If func = "stop" Then
            Try
                If IsNothing(DeviceList) = False Then
                    SyncLock DeviceListLock
                        For Each d In DeviceList
                            If d.Kind = "TZBQ" Then
                                Dim ordreMsg As String = "<TZBQ:STOP," & d.DeviceID & ">"
                                SendOrderMsgToDevice(d.DeviceID, ordreMsg)
                            End If
                            If d.Kind = "TSS" Then
                                Dim p As bscanStu
                                p.task = "stop"
                                p.deviceID = d.DeviceID
                                Dim ordreMsg As String = JsonConvert.SerializeObject(p)
                                SendOrderMsgToDevice(d.DeviceID, ordreMsg)
                            End If
                        Next
                    End SyncLock
                End If
            Catch ex As Exception

            End Try
            Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=success;msg=")
            sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
            Return
        End If
        If func = "tzbqOrder" Then
            Try
                Dim ido As INGDeviceOrder = JsonConvert.DeserializeObject(msg, GetType(INGDeviceOrder))
                If IsNothing(ido) Then
                    Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=INGDeviceOrder格式不正确")
                    sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
                    Return
                End If
                If IsNothing(DeviceList) Then
                    Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=当前无服务设备在线")
                    sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
                    Return
                End If
                If DeviceList.Count = 0 Then
                    Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=当前无服务设备在线")
                    sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
                    Return
                End If
                Dim deviceid As String = ""
                Dim selectKind As String = ""
                SyncLock DeviceListLock
                    If deviceid = "" Then
                        For Each d In DeviceList
                            If d.Kind = "TZBQ" Then
                                deviceid = d.DeviceID
                                selectKind = d.Kind
                                Exit For
                            End If
                        Next
                    End If
                End SyncLock
                If deviceid = "" Then
                    Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=当前无服务设备在线")
                    sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
                    Return
                End If
                Dim rpINGMsg2 As INGMsgStu = New INGMsgStu(MsgID, func, SendOrderMsgToDevice(deviceid, ido.orderJson))
                sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg2))
                Return
            Catch ex As Exception

            End Try
        End If
        If func = "bscan" Then
            Try
                Dim ido As INGDeviceOrder = JsonConvert.DeserializeObject(msg, GetType(INGDeviceOrder))
                If IsNothing(ido) Then
                    Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=INGDeviceOrder格式不正确")
                    sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
                    Return
                End If
                Dim ts As bscanStu = JsonConvert.DeserializeObject(ido.orderJson, GetType(bscanStu))
                If IsNothing(ts) Then
                    Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=tssOrder_stu格式不正确")
                    sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
                    Return
                End If
                If IsNothing(DeviceList) Then
                    Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=当前无服务设备在线")
                    sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
                    Return
                End If
                If DeviceList.Count = 0 Then
                    Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=当前无服务设备在线")
                    sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
                    Return
                End If
                Dim deviceid As String = ""
                Dim selectKind As String = ""
                SyncLock DeviceListLock
                    For Each d In DeviceList
                        If d.Kind = "TSS" Then
                            deviceid = d.DeviceID
                            selectKind = d.Kind
                            Exit For
                        End If
                    Next
                    If deviceid = "" Then
                        For Each d In DeviceList
                            If d.Kind = "TZBQ" Then
                                deviceid = d.DeviceID
                                selectKind = d.Kind
                                Exit For
                            End If
                        Next
                    End If
                End SyncLock

                If deviceid = "" Then
                    Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=当前无服务设备在线")
                    sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
                    Return
                End If
                If selectKind = "TSS" Then
                    Dim orderJson As String = JsonConvert.SerializeObject(ts)

                    Dim rpINGMsg2 As INGMsgStu = New INGMsgStu(MsgID, func, SendOrderMsgToDevice(deviceid, orderJson))
                    sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg2))
                    Return
                End If
                If selectKind = "TZBQ" Then
                    Dim orderJson As String = "<TZBQ:SMBH," & deviceid & "," & ts.freqStart & "," & ts.freqEnd & "," & (ts.freqStep) / 1000 & "," & 1 & "," & 0 & "," & 0 & "," & 1 & "," & 1 & ">"
                    Dim rpINGMsg2 As INGMsgStu = New INGMsgStu(MsgID, func, SendOrderMsgToDevice(deviceid, orderJson))
                    sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg2))
                    Return
                End If
                Dim rsINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=未指定设备类型")
                sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rsINGMsg))
                Return
            Catch ex As Exception
                Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=tssOrder_stu格式不正确")
                sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
                Return
            End Try
        End If
        If func = "DeviceOrder" Then
            Try
                Dim ido As INGDeviceOrder = JsonConvert.DeserializeObject(msg, GetType(INGDeviceOrder))
                If IsNothing(ido) Then
                    Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=INGDeviceOrder格式不正确")
                    sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
                    Return
                End If
                Dim deviceID As String = ido.DeviceID
                Dim orderJson As String = ido.orderJson
                If deviceID = "" Then
                    Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=INGDeviceOrder_deviceID不可为空")
                    sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
                    Return
                End If
                If orderJson = "" Then
                    Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=INGDeviceOrder_orderJson不可为空")
                    sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
                    Return
                End If
                Dim rpINGMsg2 As INGMsgStu = New INGMsgStu(MsgID, func, SendOrderMsgToDevice(deviceID, orderJson))
                sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg2))
                Return
            Catch ex As Exception
                Dim rpINGMsg As INGMsgStu = New INGMsgStu(MsgID, func, "result=fail;msg=INGDeviceOrder格式不正确")
                sendMsg2TSSServer("data", "ingmsg", "", INGMsg2TSSShuju(rpINGMsg))
                Return
            End Try
        End If
    End Sub
    Private Function SendOrderMsgToDevice(deviceID As String, orderMsg As String) As String
        Dim flag_isin As Boolean = False
        Dim httpUrl As String = ""
        Dim deviceKind As String
        SyncLock DeviceListLock
            If IsNothing(DeviceList) = False Then
                For Each d In DeviceList
                    If d.DeviceID = deviceID Then
                        flag_isin = True
                        httpUrl = d.HTTPMsgUrl
                        deviceKind = d.Kind
                        Exit For
                    End If
                Next
            End If
        End SyncLock
        If flag_isin = False Then Return "result=fail;msg=设备不在线"
        httpUrl = httpUrl.Replace("http://+", "http://127.0.0.1")
        If deviceKind.ToLower() = "tss" Then
            Dim str As String = "?func=tssOrder&datamsg=" & orderMsg & "&token=" & "928453310"
            Dim result As String = GetH(httpUrl, str)
            Return result
        End If
        If deviceKind.ToLower() = "tzbq" Then
            Dim str As String = "?func=tzbqOrder&datamsg=" & orderMsg & "&token=" & “928453310”
            Dim result As String = GetH(httpUrl, str)
            Return result
        End If
        Return “result=fail;msg=查询到设备类型非微型传感器或频谱传感器，无法下达设备命令”
    End Function
    Structure INGDeviceOrder
        Dim DeviceID As String
        Dim orderJson As String
    End Structure
    Private Sub DeviceStop()

    End Sub
    Private Sub log(str As String)
        Module1.log(str)
    End Sub

    Private Sub SendFreq(ByVal freqbegin As Double, ByVal FreqStep As Double, ByVal xx() As Double, ByVal yy() As Double)
        FreqStep = FreqStep / 1000
        If flag_isConnected = False Then Return
        If xx.Length <> yy.Length Then Return
        Dim ppsj(yy.Length * 2 - 1) As Byte
        For i = 0 To yy.Length - 1
            Dim value As Double = yy(i) * 10
            Dim va As Int16 = Math.Floor(value)
            Dim by() As Byte = BitConverter.GetBytes(va)
            Array.Copy(by, 0, ppsj, i * 2, by.Length)
        Next

        Dim ResultBuffer(45 + ppsj.Length - 1) As Byte
        Array.Copy(BitConverter.GetBytes(CType(0, Int16)), 0, ResultBuffer, 0, 2)
        Array.Copy(BitConverter.GetBytes(CType(0, Int16)), 0, ResultBuffer, 2, 2)
        Array.Copy(BitConverter.GetBytes(CType(freqbegin, Double)), 0, ResultBuffer, 4, 8)
        Array.Copy(BitConverter.GetBytes(CType(FreqStep, Double)), 0, ResultBuffer, 12, 8)
        Array.Copy(BitConverter.GetBytes(CType(8000, Int32)), 0, ResultBuffer, 20, 4)
        Array.Copy(BitConverter.GetBytes(CType(0, Int16)), 0, ResultBuffer, 24, 2)
        Array.Copy(BitConverter.GetBytes(CType(0, Int16)), 0, ResultBuffer, 26, 2)
        Array.Copy(BitConverter.GetBytes(CType(0, Int16)), 0, ResultBuffer, 28, 2)
        ResultBuffer(30) = 16
        Array.Copy(BitConverter.GetBytes(CType(ppsj.Length, Int32)), 0, ResultBuffer, 37, 4)
        Array.Copy(BitConverter.GetBytes(CType(ppsj.Length, Int32)), 0, ResultBuffer, 41, 4)
        Array.Copy(ppsj, 0, ResultBuffer, 45, ppsj.Length)
        '  MsgBox(ppsj.Length)

        sendMsg2TSSServer("spect_conti", "bscan", "", ResultBuffer)
        '  log("ppsj")
    End Sub
    Public Sub HandleMyDeviceMsg(deviceMsg As String)
        sendMsg2TSSServer("data", "devicemsg", "", Encoding.Default.GetBytes(deviceMsg))
    End Sub
#Region "连接到TSS服务器"
    Private Sub ConnectToSever()
        If flag_isConnected Then Return
        If flag_isConnecting Then Return
        If IsNothing(tcpcl) = False Then
            Try
                If tcpcl.Connected Then tcpcl.Close()
            Catch ex As Exception

            End Try
        End If
        Dim ipendpoint = New IPEndPoint(IPAddress.Parse(serverIP), serverPort)
        Dim num As Integer = 0
        While True
            num = num + 1
            If num >= 4 Then
                'return
            End If
            Try
                If flag_isConnected = False Then
                    tcpcl = New TcpClient
                    log("正在连接服务器……")
                    flag_isConnecting = True
                    tcpcl.Connect(ipendpoint)
                End If
                If tcpcl.Connected Then
                    flag_isConnected = True
                    log("已连接服务器")
                    flag_isConnecting = False
                    Dim th As New Thread(AddressOf reciveMsg)
                    th.Start()
                    Sleep(100)
                    login()
                    Return
                End If
                Sleep(1000)
            Catch ex As Exception
            End Try
        End While
    End Sub
    Private Function msg2TssMsg(ByVal ctrl As Byte, ByVal datatype As String, ByVal functype As String, ByVal canshu As String, ByVal shuju() As Byte) As tssMsg
        Dim tm As New tssMsg
        tm.flag = "$RADIOINF"
        tm.ctrl = ctrl
        tm.datatype = datatype
        tm.functype = functype
        tm.canshuquchangdu = 0
        tm.shujuquchangdu = 0
        tm.deviceID = deviceID
        Dim numofmsg As Integer = 102
        If canshu <> "" Then
            tm.canshuqu = canshu
            Dim b() As Byte = tobyte(tm.canshuqu)
            tm.canshuquchangdu = b.Count
            numofmsg = numofmsg + b.Count
        End If
        If IsNothing(shuju) = False Then
            ReDim tm.shujuqu(shuju.Count - 1)
            Array.Copy(shuju, tm.shujuqu, shuju.Count)
            tm.shujuquchangdu = shuju.Count
            numofmsg = numofmsg + shuju.Count
        End If
        tm.lenofmsg = numofmsg
        Return tm
    End Function
    Private Sub sendMsg2TSSServer(ByVal dataType As String, ByVal functionTye As String, ByVal canshuqu As String, ByVal shuju() As Byte)
        SendMsg(Me.msg2TssMsg(&H0, dataType, functionTye, canshuqu, shuju))
    End Sub
    Private Sub login()
        Try
            sendMsg2TSSServer("link", "logon", "", Nothing)
            Sleep(1000)
            sendMsg2TSSServer("link", "logon", "", Nothing)
        Catch ex As Exception

        End Try
    End Sub

    Private Sub SendMsg(ByVal tm As tssMsg)
        Try
            If flag_isConnected = False Then
                Return
            End If
            Dim by() As Byte = tssmsg2byte(tm)
            If IsNothing(by) Then
                log("TSSSer.SendData>ERR>Byte[]为空")
                Exit Sub
            End If
            tcpcl.GetStream.Write(by, 0, by.Length)
            tcpcl.GetStream.Flush()
        Catch ex As Exception
            log("SendMsg>ERR>" & ex.ToString)
            If InStr(ex.Message, "连接") Then
                flag_isConnected = False
                Dim th As New Thread(AddressOf ConnectToSever)
                th.Start()
            End If
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
                                        handleTM(byteSub)
                                        byteSub = Nothing
                                        Exit Sub
                                    End If
                                    If byteSub.Length > lenofmsg Then
                                        Dim bk() As Byte = tobytes(byteSub, 0, lenofmsg)
                                        handleTM(bk)
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
    Private Sub reciveMsg()
        While True
            Try

                Dim buffer(8192) As Byte
                Dim num As Integer = tcpcl.GetStream.Read(buffer, 0, buffer.Length)
                ' log("收到服务器" & num & "字节")
                If num = 0 Then
                    flag_isConnected = False
                    Dim th As New Thread(AddressOf ConnectToSever)
                    th.Start()
                    Exit Sub
                End If
                Try
                    Dim by(num - 1) As Byte
                    Array.Copy(buffer, 0, by, 0, by.Length)
                    HandleBuffer(by)
                Catch ex As Exception

                End Try
            Catch ex As Exception
                log("reciveMsg错误," & ex.Message)
                flag_isConnected = False
                Dim th As New Thread(AddressOf ConnectToSever)
                th.Start()
                Exit Sub
            End Try
        End While
    End Sub
#End Region
End Class
