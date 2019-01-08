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
Public Class RMTPClient
    Private RMTPVersion As String = "01.01"
    Private IP As String
    Private Port As Integer
    Private flagIsFirstPackage As Boolean = True
    Private flagIsLogin As Boolean = False
    Private bufferLen As Integer = 8192
    Private myClientSocket As New System.Net.Sockets.Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
    Private flagIsLinkToDevice As Boolean = False
    Private linkedDeviceStu As DeviceStu = Nothing
    Private WithEvents linkedDeviceCls As DeviceTSS
    Private selectedFunction As String
    Private orderFreqStart As Double
    Private orderFreqEnd As Double
    Private orderFreqStep As Double
    Private orderParaMsg As String
    Public Sub New(ByVal ClientSocket As Socket)
        myClientSocket = ClientSocket
        IP = myClientSocket.RemoteEndPoint.ToString.Split(":")(0)
        Port = myClientSocket.RemoteEndPoint.ToString.Split(":")(1)
    End Sub
    Public Sub Start()
        serverThreadProc()
        SendRMTPMsgInfo2Client(New RMTPMsgInfo("VERSION", RMTPVersion))
        'log("RMTPClient连接，服务器主动发送握手命令……")
    End Sub
    Public Sub CloseAll()
        Try
            Try
                HandleRSTOP("", False)
            Catch ex As Exception

            End Try
            If IsNothing(myClientSocket) = False Then
                Try
                    If myClientSocket.Connected Then myClientSocket.Close()
                Catch ex As Exception

                End Try
            End If
            myClientSocket = Nothing
        Catch ex As Exception

        End Try
    End Sub


    Private Sub HandleRMTPClientMsg(msg As String)
        'log("New RMTP Msg [" & msg & "]")
        Dim ri As New RMTPMsgInfo(msg)
        'log("   .func=" & ri.func)
        'log("   .msg=" & ri.body)

        If IsNothing(ri) Then
            CloseAll()
            Return
        End If
        If ri.func = "" Then
            CloseAll()
            Return
        End If
        If flagIsFirstPackage Then
            flagIsFirstPackage = False
        Else
            If Not flagIsLogin Then
                CloseAll()
                Return
            End If
        End If
        Dim func As String = ri.func
        Dim body As String = ri.body
        Try
            Select Case func
                Case "VERIF"
                    flagSendData2Client = False
                    HandleLogin(body)
                Case "FSCAN"  '频段扫描
                    flagSendData2Client = False
                    HandleFscan(body)
                Case "PARAM"  '客户端下发参数
                    flagSendData2Client = False
                    HandleParam(body)
                Case "RSTOP"  '客户端下发参数
                    flagSendData2Client = False
                    HandleRSTOP(body, True)
                Case "GPSLA"
                    HandleGetGPS(body)
            End Select
        Catch ex As Exception
            '  MsgBox(ex.ToString)
        End Try
    End Sub
    Private Sub HandleGetGPS(body As String)
        SendData2Client(RMTPDataType.执行结果信息, "RESULT:SUCCESSED;Info:")
    End Sub
    Private Sub HandleRSTOP(msg As String, stopDevice As Boolean)
        flagIsLinkToDevice = False
        linkedDeviceStu = Nothing
        If IsNothing(linkedDeviceCls) = False Then
            Try
                If stopDevice Then
                    linkedDeviceCls.AddMyLog("接收RMTP命令,停止工作", "成功")
                    linkedDeviceCls.SendStop2Device()
                End If
                RemoveHandler linkedDeviceCls.OnHandleJsonMsg, AddressOf OnHandleJsonMsg
            Catch ex As Exception

            End Try
        End If
        selectedFunction = Nothing
        orderFreqStart = Nothing
        orderFreqEnd = Nothing
        orderFreqStep = Nothing
        orderParaMsg = Nothing
    End Sub
    Private Sub HandleLogin(msg As String)
        Dim dik As Dictionary(Of String, String) = GetRMTPParaDik(msg)
        If IsNothing(dik) Then Return
        Dim method As String = ""
        Dim usr As String = ""
        Dim pwd As String = ""
        If dik.ContainsKey("METHOD") Then method = dik("METHOD")
        If dik.ContainsKey("USER") Then usr = dik("USER")
        If dik.ContainsKey("PASSWD") Then pwd = dik("PASSWD")
        If method <> "01" Or usr = "" Or pwd = "" Then
            SendRMTPMsgInfo2Client(New RMTPMsgInfo("VERIF", "+REFUSE"))
            CloseAll()
        End If
        If CheckLoginFunc(usr, pwd, "CLT") = "登录成功" Then
            'log("RMTP user " & usr & " login success!")
            flagIsLogin = True
            SendRMTPMsgInfo2Client(New RMTPMsgInfo("VERIF", "+ok"))
            Return
        End If
        SendRMTPMsgInfo2Client(New RMTPMsgInfo("VERIF", "+REFUSE"))
        CloseAll()
    End Sub
    Private Sub HandleFscan(msg As String)
        If msg = "" Then
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:FAILURE;Info:参数不能为空")
            CloseAll()
            Return
        End If
        Dim st() As String = msg.Split(":")
        If st.Length < 3 Then
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:FAILURE;Info:参数格式不正确")
            CloseAll()
            Return
        End If
        Dim stationid As String = st(0)
        Dim centerid As String = st(1)
        Dim pricode As String = st(2)
        If stationid = "" Then
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:FAILURE;Info:stationid不可为空")
            CloseAll()
            Return
        End If
        If IsNothing(DeviceListLock) Then DeviceListLock = New Object
        Dim flagIsFind As Boolean = False
        Dim ds As DeviceStu
        SyncLock DeviceListLock
            If IsNothing(DeviceList) = False Then
                For Each itm In DeviceList
                    If itm.DeviceID = stationid Then
                        ds = itm
                        flagIsFind = True
                        Exit For
                    End If
                Next
            End If
        End SyncLock
        If flagIsFind = False Then
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:FAILURE;Info:站点不在线!")
            CloseAll()
            Return
        End If
        If IsNothing(ds) Then
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:FAILURE;Info:连接站点失败!")
            CloseAll()
            Return
        End If
        If ds.Kind.ToLower <> "tss" Then
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:FAILURE;Info:连接站点失败!")
            CloseAll()
            Return
        End If
        flagIsLinkToDevice = True
        linkedDeviceStu = ds
        linkedDeviceCls = CType(ds.cls, DeviceTSS)
        If linkedDeviceCls.GetisWorking Then
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:FAILURE;Info:设备正在执行任务!")
        Else
            flagSendOrderHead = True
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:SUCCESSED;Info:好!")
        End If
        AddHandler linkedDeviceCls.OnHandleJsonMsg, AddressOf OnHandleJsonMsg
        selectedFunction = "Fscan"
    End Sub
    Private Sub HandleParam(msg As String)
        If selectedFunction <> "Fscan" Then
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:FAILURE;Info:参数有误，当前命令并非Fscan!")
            CloseAll()
            Return
        End If
        Dim dik As Dictionary(Of String, String) = GetRMTPParaDik(msg)
        If IsNothing(dik) Then
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:FAILURE;Info:参数有误，无法形成参数键值对!")
            CloseAll()
            Return
        End If
        Dim startfreq, stopfreq, stepfreq, ifbw As String
        If dik.ContainsKey("startfreq") Then startfreq = dik("startfreq")
        If dik.ContainsKey("stopfreq") Then stopfreq = dik("stopfreq")
        If dik.ContainsKey("step") Then stepfreq = dik("step")
        If dik.ContainsKey("ifbw") Then ifbw = dik("ifbw")

        If startfreq = "" Or stopfreq = "" Or stepfreq = "" Then
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:FAILURE;Info:参数有误，startfreq/stopfreq/stepfreq为空!")
            CloseAll()
            Return
        End If
        orderFreqStart = Val(startfreq)
        orderFreqEnd = Val(stopfreq)
        orderFreqStep = Val(stepfreq)
        If orderFreqStart = 0 Or orderFreqEnd = 0 Or orderFreqStep = 0 Then
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:FAILURE;Info:参数设置有误!startfreq/stopfreq/stepfreq为0!")
            CloseAll()
            Return
        End If
        If orderFreqEnd <= orderFreqStart Then
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:FAILURE;Info:起始频率必须小于终止频率!")
            CloseAll()
            Return
        End If

        orderParaMsg = "DEVICE=" & linkedDeviceStu.DeviceID & "," & msg
        Dim result As String = linkedDeviceCls.SendRMTPFreqOrder2Device(orderFreqStart, orderFreqEnd, orderFreqStep, 40000)
        If result = "ok" Then
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:SUCCESSED;Info:ok")
            SendData2Client(RMTPDataType.设备参数信息, orderParaMsg)
            flagSendData2Client = True
        Else
            SendData2Client(RMTPDataType.执行结果信息, "RESULT:FAILURE;Info:" & result)
        End If
    End Sub
    Dim flagSendData2Client As Boolean = False
    Private Sub OnHandleJsonMsg(js As String)
        If flagIsLinkToDevice = False Then
            RemoveHandler linkedDeviceCls.OnHandleJsonMsg, AddressOf OnHandleJsonMsg
            Return
        End If
        ' log("flagSendData2Client=" & flagSendData2Client)
        If flagSendData2Client = False Then Return
        Dim obj As JSON_Msg = JsonConvert.DeserializeObject(js, GetType(JSON_Msg))
        Dim func As String = obj.func
        Dim msg As String = obj.msg
        If func = "bscan" Then
            Dim p As json_PPSJ = JsonConvert.DeserializeObject(msg, GetType(json_PPSJ))
            handlePinPuFenXi(p)
        End If
    End Sub
    Dim flagSendOrderHead As Boolean = True
    Private Sub handlePinPuFenXi(ByVal p As json_PPSJ)

        Dim headBuffer() As Byte = GetDataHeadByPPSJ(p)
        If IsNothing(headBuffer) Then Return
        If flagSendOrderHead Then
            ''先返回数据描述头
            'log("freq-->返回数据描述头")
            SendData2Client(RMTPDataType.数据描述头, headBuffer)
            flagSendOrderHead = False
        End If
        Dim bodyBuffer() As Byte = GetDataBodyByPPSJ(p)
        ' log("freq-->准备返回业务数据...")
        If IsNothing(bodyBuffer) Then Return
        ' log("返回频谱数据，data.length=" & bodyBuffer.Length)
        SendData2Client(RMTPDataType.业务数据, bodyBuffer)
        ' log("freq-->返回业务数据")
    End Sub
    Private Function GetDataHeadByPPSJ(p As json_PPSJ) As Byte()
        Dim headBuffer(30) As Byte
        headBuffer(0) = 1
        Dim nFscanSegment As Int16 = 1
        Dim by() As Byte = BitConverter.GetBytes(nFscanSegment)
        headBuffer(1) = by(0)
        headBuffer(2) = by(1)
        Dim freqStart As Long = p.freqStart * 1000 * 1000
        Dim freqEnd As Long = p.freqEnd * 1000 * 1000
        Dim freqStep As Long = p.freqStep * 1000 * 1000
        Dim by1() As Byte = BitConverter.GetBytes(freqStart)
        Dim by2() As Byte = BitConverter.GetBytes(freqEnd)
        Dim by3() As Byte = BitConverter.GetBytes(freqStep)
        Array.Copy(by1, 0, headBuffer, 3, 8)
        Array.Copy(by2, 0, headBuffer, 11, 8)
        Array.Copy(by3, 0, headBuffer, 19, 8)
        Dim nPoints As Integer = p.dataCount
        Array.Copy(BitConverter.GetBytes(nPoints), 0, headBuffer, 27, 4)
        Return headBuffer
    End Function
    Private Function GetDataBodyByPPSJ(p As json_PPSJ) As Byte()
        Dim nPoints As Integer = p.dataCount
        Dim buffer(nPoints * 2 + 8 - 1) As Byte
        Array.Copy(BitConverter.GetBytes(nPoints), 0, buffer, 0, 4)
        Dim freqIndex As Integer = 0
        Array.Copy(BitConverter.GetBytes(freqIndex), 0, buffer, 4, 4)
        Dim yy() As Double = p.value
        Dim isDSGFreq As Boolean = False
        If p.isDSGFreq Then
            isDSGFreq = True
            yy = DSGBase2PPSJValues(p.DSGFreqBase64)
        End If
        '106.989
        If IsNothing(yy) Then Return Nothing
        If yy.Count <> nPoints Then Return Nothing
        Dim add As Double = 107  '106.989
        For i = 0 To nPoints - 1
            Dim itmValue As Double = yy(i) + add
            Dim d As Short = Math.Floor(itmValue * 100)
            'If d >= 0 Then Return Nothing
            If d < -32768 Then Return Nothing
            Dim int As Int16 = d
            Array.Copy(BitConverter.GetBytes(int), 0, buffer, 8 + i * 2, 2)
        Next
        Return buffer
    End Function
    Structure JSON_Msg
        Dim func As String
        Dim msg As String
    End Structure
    Structure json_PPSJ
        Dim freqStart As Double
        Dim freqStep As Double
        Dim freqEnd As Double
        Dim deviceID As String
        Dim dataCount As Integer
        Dim runLocation As runLocation
        Dim value() As Double
        Dim isDSGFreq As Boolean
        Dim DSGFreqBase64 As String
    End Structure
    Structure runLocation
        Dim lng As String
        Dim lat As String
        Dim time As String
        Sub New(ByVal _lng As String, ByVal _lat As String, ByVal _time As String)
            lng = _lng
            lat = _lat
            time = _time
        End Sub
    End Structure
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

#Region "RMTPHelper"
    Private Sub SendMsg2Client(msg As String)
        If msg = "" Then Return
        If msg.StartsWith("RMTP:") = False Then
            msg = "RMTP:" & msg
        End If
        If msg.EndsWith(vbLf) = False Then
            msg = msg & vbLf
        End If
        SendRMTPMsg(msg)
    End Sub
    Enum RMTPDataType
        业务数据 = 0
        未使用1 = 1
        音频数据 = 2
        经纬度信息 = 3
        执行结果信息 = 4
        未使用2 = 5
        数据描述头 = 6
        设备参数信息 = 7
        单信道中频音频数据 = 8
    End Enum
    Private Sub SendRMTPMsgInfo2Client(ri As RMTPMsgInfo)
        If IsNothing(ri) Then Return
        Dim msg As String = RMTPMsgInfo2msg(ri)
        SendMsg2Client(msg)
    End Sub
    Private Sub SendData2Client(dataType As RMTPDataType, msg As String)
        If IsNothing(msg) Then msg = "+err"
        If msg = "" Then msg = "+err"
        Dim by() As Byte = Encoding.Default.GetBytes(msg)
        SendData2Client(dataType, by)
    End Sub
    Private Sub SendData2Client(dataType As RMTPDataType, data() As Byte)
        Dim headBuffer(15) As Byte
        If (data.Length + headBuffer.Length > 32768) Then Return 'int16最大数为 32768
        headBuffer(0) = &HEE : headBuffer(1) = &HEE : headBuffer(2) = &HEE : headBuffer(3) = &HEE
        Dim allLen As Int16 = data.Length + headBuffer.Length
        Dim realAllLen As Int16 = allLen - 4
        Dim by() As Byte = BitConverter.GetBytes(realAllLen)
        headBuffer(4) = by(0)
        headBuffer(5) = by(1)
        Dim year As Int16 = Now.Year
        by = BitConverter.GetBytes(year)
        headBuffer(6) = by(0)
        headBuffer(7) = by(1)
        headBuffer(8) = Now.Month
        headBuffer(9) = Now.Day
        headBuffer(10) = Now.Hour
        headBuffer(11) = Now.Minute
        headBuffer(12) = Now.Second
        Dim ms As Int16 = Now.Millisecond
        by = BitConverter.GetBytes(ms)
        headBuffer(13) = by(0)
        headBuffer(14) = by(1)
        Dim nDataType As Integer = dataType
        headBuffer(15) = nDataType
        Dim realBuffer(allLen - 1) As Byte
        Array.Copy(headBuffer, 0, realBuffer, 0, headBuffer.Length)
        Array.Copy(data, 0, realBuffer, headBuffer.Length, data.Length)
        SendByte(realBuffer)
        'Dim str As String = ""
        'For i = 0 To realBuffer.Length - 1
        '    str = str & realBuffer(i).ToString("X") & " "
        'Next
        'log("")
    End Sub
    Structure RMTPMsgInfo
        Dim func As String
        Dim body As String
        Sub New(rmtpMsg As String)
            If rmtpMsg = "" Then Return
            If rmtpMsg.StartsWith("RMTP:") = False Then Return
            rmtpMsg = rmtpMsg.Replace(vbLf, "").Replace(vbCr, "").Replace("RMTP:", "")
            Dim st() As String = rmtpMsg.Split(":")
            If st.Length < 2 Then
                func = rmtpMsg
                Return
            End If
            func = st(0)
            For i = 1 To st.Length - 1
                body = body & st(i) & ":"
            Next
            body = body.Substring(0, body.Length - 1)
        End Sub
        Sub New(_func As String, _body As String)
            func = _func
            body = _body
        End Sub
    End Structure
    Private Function RMTPMsgInfo2msg(ri As RMTPMsgInfo) As String
        If IsNothing(ri) Then Return ""
        If ri.func = "" Then Return ""
        Dim msg As String = "RMTP:" & ri.func & ":" & ri.body
        Return msg
    End Function
    Private Function GetRMTPParaDik(msg As String) As Dictionary(Of String, String)
        Dim dik As New Dictionary(Of String, String)
        Dim st() As String = msg.Split(",")
        For Each itm In st
            If InStr(itm, "=") Then
                Dim sh() As String = itm.Split("=")
                If sh.Length = 2 Then
                    Dim key As String = sh(0)
                    Dim value As String = sh(1)
                    key = key.Replace(",", "")
                    value = value.Replace(",", "")
                    dik(key) = value
                End If
            End If
        Next
        Return dik
    End Function
#End Region
#Region "socket处理"
    Private Sub SendRMTPMsg(ByVal Msg As String)
        Dim b() As Byte = Encoding.Default.GetBytes(Msg)
        SendByte(b)
    End Sub
    Private Sub SendByte(ByVal by() As Byte)
        Try
            If IsNothing(by) Then Return
            If IsNothing(myClientSocket) Then Return
            If myClientSocket.Connected = False Then Return
            myClientSocket.Send(by)
        Catch ex As Exception
            'log("发送失败,已断开连接," & ex.Message)
        End Try
    End Sub
    Structure SocketAndBuffer
        Dim Socket As System.Net.Sockets.Socket
        Dim Buffer() As Byte
        Sub New(_socket As Socket, _buffer() As Byte)
            Socket = _socket
            Buffer = _buffer
        End Sub
    End Structure
    Private Sub serverThreadProc()
        Try
            Dim buffer(bufferLen - 1) As Byte
            Dim sb As New SocketAndBuffer(myClientSocket, buffer)
            sb.Socket.BeginReceive(sb.Buffer, 0, bufferLen, SocketFlags.None, AddressOf ReceiveCallBack, sb)
        Catch ex As Exception

        End Try
    End Sub

    Private Sub ReceiveCallBack(ByVal ar As IAsyncResult)
        Dim sb As SocketAndBuffer
        sb = CType(ar.AsyncState, SocketAndBuffer)
        Try
            If IsNothing(sb.Socket) Then Return
            If Not sb.Socket.Connected Then
                CloseAll()
                Return
            End If
            Dim iLen As Integer = sb.Socket.EndReceive(ar)
            If iLen = 0 Then
                CloseAll()
                Return
            End If
            Dim iBuffer(iLen - 1) As Byte
            Array.Copy(sb.Buffer, 0, iBuffer, 0, iLen)
            Dim str As String = Encoding.Default.GetString(iBuffer)
            If str.StartsWith("RMTP:") Then
                sb.Socket.BeginReceive(sb.Buffer, 0, bufferLen, SocketFlags.None, AddressOf ReceiveCallBack, sb)
                Try
                    HandleRMTPClientMsg(str.Replace(vbLf, ""))
                Catch ex As Exception

                End Try
            Else
                ' 'log("not RMTP")
                CloseAll()
            End If
        Catch ex As Exception
            ' 'log(ex.Message)
            CloseAll()
            Return
        End Try
    End Sub
#End Region
End Class
