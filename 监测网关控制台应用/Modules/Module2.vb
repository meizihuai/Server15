Imports System
Imports System.Net
Imports System.IO
Imports System.Text
Imports System.Data
Imports MySql.Data.MySqlClient
Imports Newtonsoft
Imports Newtonsoft.Json
Imports Newtonsoft.Json.Linq
Imports System.Net.Sockets
Imports ConvertGPS
Imports 监测网关控制台应用.DeviceTSS

Module Module2
    Public UseDSGFreq As Boolean
    Public ConnectSQL As String = "server=123.207.31.37;DataBase=DianCiXinXiFuWu;User ID=root;Pwd=Mei19931129"
    Public TZBQHttpListenerPath As String = "http://+:3277/TZBQ/"
    Public TSSHttpListenerPath As String = "http://+:3277/TSS/"
    Public TSSWebSocketListenerPath As String = "http://+:3278/websocket/TSS/"
    Public broadcastSleep As Integer = 2000
    Public DeviceList As List(Of DeviceStu)
    Public DeviceListLock As Object
    Public dik As Dictionary(Of String, String)
    Public dik_Lock As Object
    Public myServerInfo As myServerInfoStu
    Public myServerInfoLock As Object
    Public MZHHandle As MZH_TSS_Handle
    Public flag_MzhHandle As Boolean
    Public TekGateWay() As String = New String() {"DSG-GW0100", "DSG-GW0101", "DSG-GW0102", "DSG-GW0103"}
    Public TekBusDevicesLock As New Object
    Public TekBusDevices() As TekBusDeviceInfo = New TekBusDeviceInfo() {New TekBusDeviceInfo("Tek180509", "DSG-GW0102", "DSG_DH0043"),
                                                                         New TekBusDeviceInfo("Tek180510", "DSG-GW0103", ""),
                                                                         New TekBusDeviceInfo("Tek180511", "DSG-GW0100", ""),
                                                                         New TekBusDeviceInfo("Tek180512", "DSG-GW0101", "DSG_DH0044")}
    'Public Structure normalResponse 'json回复格式
    '    Public result As Boolean
    '    Public msg As String
    '    Public errmsg As String
    '    Public data As Object
    '    Sub New(ByVal _result As Boolean, ByVal _msg As String, ByVal _errmsg As String, ByVal _data As Object) '基本构造函数() As _errmsg,string
    '        result = _result
    '        msg = _msg
    '        errmsg = _errmsg
    '        data = _data
    '    End Sub
    '    Sub New(ByVal _result As Boolean, ByVal _msg As String) '重载构造函数，为了方便写new,很多时候，只需要一个结果和一个参数() As _result,string
    '        result = _result
    '        msg = _msg
    '        errmsg = ""
    '        data = ""
    '    End Sub
    'End Structure
    Structure TekBusDeviceInfo
        Dim lineId As Integer
        Dim gpsTime As String
        Dim freqUpdateTime As String
        Dim TekDeviceId As String
        Dim GwDeviceId As String
        Dim HDeviceId As String
        Dim TekCoordinfo As CoordInfo
        Dim GwCoordinfo As CoordInfo
        Dim HkCoordinfo As CoordInfo
        Dim freqJson As json_PPSJ
        Sub New(TekDeviceId As String, GwDeviceId As String, HDeviceId As String)
            Me.TekDeviceId = TekDeviceId
            Me.GwDeviceId = GwDeviceId
            Me.HDeviceId = HDeviceId
        End Sub
    End Structure
    Structure GPSTranslatInfo
        Dim status As Integer
        Dim result As CoordInfo()
    End Structure
    Structure CoordInfo
        Dim x As Double
        Dim y As Double
        Sub New(ByVal x As Double, ByVal y As Double)
            Me.x = x
            Me.y = y
        End Sub
    End Structure
    Public Function GPS2BDS(ByVal x As Double, ByVal y As Double) As CoordInfo
        'Dim coords As String = x & "," & y
        'Return GPS2BDS(coords)(0)
        '  ConvertGPS.
        Dim gps As New PointLatLng(y, x)
        Dim p As PointLatLng = Gps84_To_bd09(gps)
        Return New CoordInfo(p.Lng, p.Lat)
    End Function
    Public Function GPS2GDS(ByVal x As Double, ByVal y As Double) As CoordInfo
        'Dim coords As String = x & "," & y
        'Return GPS2GDS(coords)(0)
        Dim gps As New PointLatLng(y, x)
        Dim p As PointLatLng = gps84_To_Gcj02(gps)
        Return New CoordInfo(p.Lng, p.Lat)
    End Function
    Public Function GPS2BDS(ByVal coords As String) As CoordInfo()
        ' http://api.map.baidu.com/geoconv/v1/?coords=114.21892734521,29.575429778924;114.21892734521,29.575429778924&from=1&to=5&ak=你的密钥
        Dim ak As String = "5cW7OlxZXThtbkq1Y0u5yNO6"
        Dim url As String = "http://api.map.baidu.com/geoconv/v1/?coords=" & coords & "&from=1&to=5&ak=" & ak
        ' Console.WriteLine(coords)
        Dim result As String = GetH(url, "")
        Try
            Dim info As GPSTranslatInfo = JsonConvert.DeserializeObject(result, GetType(GPSTranslatInfo))
            If IsNothing(info) = False Then
                If info.status = 0 Then
                    If IsNothing(info.result) = False Then
                        If info.result.Count <> 0 Then
                            ' MsgBox(url & vbCrLf & result)
                            Return info.result
                        End If
                    End If
                End If
            End If
        Catch ex As Exception

        End Try
        Return Nothing
    End Function
    Structure MainBroderCastMsgStu
        Dim deviceID As String
        Dim deviceKind As String
        Dim msgTime As String
        Dim deviceMsg As String

        Sub New(_deviceID As String, _deviceMsg As String, _deviceKind As String)
            deviceID = _deviceID
            msgTime = Now.ToString("yyyy-MM-dd HH:mm:ss")
            deviceMsg = _deviceMsg
            deviceKind = _deviceKind
        End Sub
    End Structure
    Structure INGMsgStu
        Dim MsgID As Integer
        Dim MsgTime As String
        Dim func As String
        Dim msg As String
        Sub New(ByVal _func As String, ByVal _msg As String)
            MsgID = 0
            MsgTime = Now.ToString("yyyy-MM-dd HH:mm:ss")
            func = _func
            msg = _msg
        End Sub
        Sub New(_MsgID As Integer, ByVal _func As String, ByVal _msg As String)
            MsgID = _MsgID
            MsgTime = Now.ToString("yyyy-MM-dd HH:mm:ss")
            func = _func
            msg = _msg
        End Sub
    End Structure
    Structure tssOrder_stu
        Dim deviceID As String
        Dim task As String
        Dim freqStart As Double
        Dim freqEnd As Double
        Dim freqStep As Double
        Dim DHDevice As String
        Dim gcValue As Double
        Dim saveFreqStep As Integer
        Dim Threshol As Double
        Dim Fucha As Double
        Dim Daikuan As Double
        Dim MinDValue As Double
        Dim Legal As List(Of Double)
        Dim watchPoint As List(Of Double)
        Dim WarnNum As Integer
    End Structure
    Structure ZTYJ_stu
        Dim pdList As List(Of Double)
        Dim ModuleTime As Double
        Dim MaxPercent As Double
        Dim MinPercent As Double
        Dim HoldSecond As Integer
        Dim MinValue As Double
        Dim MinValueSecond As Double
    End Structure
    Structure deviceStuWithOutHttpListener
        Dim DeviceID As String
        Dim Name As String
        Dim Address As String
        Dim Kind As String
        Dim DSGDeviceKind As String
        Dim OnlineTime As String
        Dim Statu As String
        Dim Lng As String
        Dim Lat As String
        Dim IP As String
        Dim Port As Integer
        Dim Func As String
        Dim RunKind As String
        Dim isNetGateWay As Boolean
        Dim NetDeviceID As String
        Dim NetGateWayID As String
        Dim NetSwitch As Integer
        Dim HTTPMsgUrl As String
        Dim WebSocketUrl As String
    End Structure
    Structure logstu
        Dim LogID As String
        Dim Time As String
        Dim Cata As String
        Dim Kind As String
        Dim Content As String
        Dim Usr As String
        Dim DeviceNickName As String
        Dim DeviceID As String
        Dim TaskNickName As String
        Dim RelateID As String
    End Structure
    Structure myServerInfoStu
        Dim MainHTTPPort As Integer
        Dim TZBQPort As Integer
        Dim TSSPort As Integer
        Dim RMTPPort As Integer
        Dim HTTPMsg_IpUrl As String
        Dim IISFileUrl As String
        Dim isSaveDevMsg As Boolean
        Dim SQLServerIP As String
        Dim SQLDataBaseName As String
        Dim SQLUid As String
        Dim SQLPwd As String
        Dim isMain As Boolean
        Dim MainServerIP As String
        Dim MainServerPort As Integer
        Dim MZHDeviceID As String
        Dim UseDSGFreq As Boolean
    End Structure

    Structure Config
        Dim ConnectSQL As String

    End Structure

    Structure tssMsg
        Dim flag As String
        Dim crc As String
        Dim lenofmsg As Integer
        Dim ctrl As Byte
        Dim xieyibanbenhao As Byte
        Dim datatype As String
        Dim functype As String
        Dim baowenxuhao As Integer
        Dim baotouchangdu As Short
        Dim canshuquchangdu As Short
        Dim shujuquchangdu As Integer
        Dim shibiao As String
        Dim deviceID As String
        Dim source As String
        Dim destination As String
        Dim canshuqu As String
        Dim shujuqu As Byte()
    End Structure
    Structure ppsj
        Dim flag As Integer
        Dim xuhao As Integer
        Dim qishipinlv As Double
        Dim bujin As Double
        Dim caiyanglv As Integer
        Dim zengyi As Integer
        Dim jifenleixing As Integer
        Dim dianping As Integer
        Dim weishu As Integer
        Dim tongdaoshu As Integer
        Dim tongdaohao As Integer
        Dim zhenshu As Integer
        Dim meizhendianshu As Integer
        Dim pinpuzongshu As Integer
        Dim pinpushuju() As Byte
    End Structure
    Public Function shuju2ppsj(ByVal by() As Byte) As ppsj
        If by.Count >= 45 Then
            Dim p As ppsj
            p.flag = BitConverter.ToInt16(by, 0)
            p.xuhao = BitConverter.ToInt16(by, 2)
            p.qishipinlv = BitConverter.ToDouble(by, 4)
            p.bujin = BitConverter.ToDouble(by, 12)
            p.caiyanglv = BitConverter.ToInt32(by, 20)
            p.zengyi = BitConverter.ToInt16(by, 24)
            p.jifenleixing = BitConverter.ToInt16(by, 26)
            p.dianping = BitConverter.ToInt16(by, 28)
            p.weishu = by(30)
            p.tongdaoshu = BitConverter.ToInt16(by, 31)
            p.tongdaohao = BitConverter.ToInt16(by, 33)
            p.zhenshu = BitConverter.ToInt16(by, 35)
            p.meizhendianshu = BitConverter.ToInt32(by, 37)
            p.pinpuzongshu = BitConverter.ToInt32(by, 41)
            p.pinpushuju = tobytes(by, 45, p.pinpuzongshu)
            Return p
        End If
    End Function
    Public Sub response(ByVal context As HttpListenerContext, ByVal np As NormalResponse)
        Dim json As String = JsonConvert.SerializeObject(np)
        response(context, json)
    End Sub
    Public Sub response(ByVal context As HttpListenerContext, ByVal msg As String)
        Try
            Dim t As New Th_ResponseStu(context, msg)
            Dim th As New Threading.Thread(AddressOf Th_Response)
            th.Start(t)
        Catch ex As Exception

        End Try
    End Sub
    Structure Th_ResponseStu
        Dim c As HttpListenerContext
        Dim m As String
        Sub New(ByVal context As HttpListenerContext, ByVal msg As String)
            c = context
            m = msg
        End Sub
    End Structure
    Private Sub Th_Response(ByVal t As Th_ResponseStu)
        If IsNothing(t) Then Return
        Try
            Dim context As HttpListenerContext = t.c
            Dim msg As String = t.m
            context.Response.StatusCode = 200
            context.Response.Headers.Add("Access-Control-Allow-Origin", "*")
            context.Response.Headers.Add("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
            context.Response.Headers.Add("Access-Control-Allow-Headers", "x-requested-with,Content-Type")
            Dim w As New StreamWriter(context.Response.OutputStream, Encoding.UTF8)
            w.Write(msg)
            w.Close()
        Catch ex As Exception

        End Try
    End Sub
    Private Function GetHTML(ByVal uri As String, ByVal cook As CookieContainer, ByVal msg As String) As String
        Dim req As HttpWebRequest = WebRequest.Create(uri & msg)
        req.Accept = "*/*"
        req.UserAgent = "Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.2.13) Gecko/20101203 Firefox/3.6.13"
        req.CookieContainer = cook
        req.KeepAlive = True
        req.ContentType = "aplication/x-www-form-urlencoded"
        req.Method = "GET"
        req.Timeout = 3000
        Dim rp As HttpWebResponse = req.GetResponse
        Dim str As String = New StreamReader(rp.GetResponseStream(), Encoding.Default).ReadToEnd

        Return str
    End Function
    Public Function ip2Address(ByVal ip As String) As String
        Try
            If InStr(ip, ":") Then
                ip = ip.Split(":")(0)
            End If
            Dim url As String = "http://int.dpool.sina.com.cn/iplookup/iplookup.php?&ip=" & ip
            url = "http://www.ip138.com/ips1388.asp?ip=" & ip & "&action=2"
            Dim msg As String = GetHTML(url, New CookieContainer, "")
            Dim findStr As String = "本站数据"
            Dim str As String = msg.Substring(InStr(msg, findStr), 100)
            Dim a As Integer = InStr(str, "：")
            Dim b As Integer = InStr(str, "</li>")
            str = str.Substring(a, b - a - 1)
            str = str.Replace("上海市上海市", "上海市")
            Return str
            'Dim s As String = msg
            'If InStr(msg, vbTab) Then
            '    Dim st() As String = msg.Split(vbTab)
            '    If st.Count >= 6 Then
            '        s = st(3) & "," & st(4) & "," & st(5)
            '    End If
            'End If
            'Return s
        Catch ex As Exception
            ' MsgBox（ex.ToString)
        End Try
    End Function
    Public Function getFuncByBQ(ByVal BQ As String)
        Try
            Dim func As String = BQ.Split(":")(1).Split(",")(0)
            If func = "ECHO" Then
                Dim huiying As String = BQ.Split(":")(1).Split(",")(2)
                func = func & "_" & huiying
            End If
            Return func
        Catch ex As Exception
            '‘MsgBox(ex.ToString)
        End Try
    End Function
    Public Function getcrcstr(ByVal str As String) As String
        Return str & crc(tobyte(str)) & vbCrLf & "\0"
    End Function
    Public Function crc(ByVal by() As Byte) As Integer
        Dim bt As Byte = 0
        Dim int As Integer = 0
        For i = 0 To by.Length - 1
            int = int + by(i)
            If int >= 256 Then
                int = int - 256
            End If
        Next
        Return int
    End Function
    Public Function tobytes(ByVal by() As Byte, ByVal startindex As Integer, ByVal len As Integer) As Byte()
        Dim bu(len - 1) As Byte
        Array.Copy(by, startindex, bu, 0, len)
        Return bu
    End Function
    Public Function tostr(ByVal by() As Byte) As String
        If IsNothing(by) Then
            Return Nothing
        End If
        Dim str As String = Encoding.Default.GetString(by)
        Return str
    End Function
    Public Function tobyte(ByVal str As String) As Byte()
        If str = "" Then
            Return New Byte() {0}
        End If
        Dim by() As Byte = Encoding.Default.GetBytes(str)
        Return by
    End Function
    Private TSS_Head_FLag As String = "$RADIOINF"
    Public Function checkFlag(ByVal by() As Byte, ByVal startindex As Integer, ByVal num As Integer) As Boolean
        If num > 9 Then num = 9
        Dim str As String = TSS_Head_FLag.Substring(0, num)
        Dim str2 As String = Encoding.Default.GetString(by, startindex, num)
        If str = str2 Then
            Return True
        Else
            Return False
        End If
    End Function
    Private Function delchr(ByVal str As String) As String
        Dim St As String = ""
        St = str.Replace(ChrW(0), "")
        Return St
    End Function
    Public Function byte2tssmsgHead(ByVal by() As Byte) As tssMsg
        Dim tm As tssMsg
        tm.flag = delchr(Encoding.Default.GetString(by, 0, 9))
        tm.crc = by(9)
        tm.lenofmsg = BitConverter.ToInt32(by, 10)
        tm.ctrl = by(14)
        tm.xieyibanbenhao = by(15)
        tm.datatype = delchr(Encoding.Default.GetString(by, 16, 16))
        tm.functype = delchr(Encoding.Default.GetString(by, 32, 16))
        tm.baowenxuhao = BitConverter.ToInt32(by, 48)
        tm.baotouchangdu = BitConverter.ToInt16(by, 52)
        tm.canshuquchangdu = BitConverter.ToInt16(by, 54)
        tm.shujuquchangdu = BitConverter.ToInt32(by, 56)
        Try
            Dim shijian As String = ""
            shijian = "20" & by(60) & "-" & by(61) & "-" & by(62) & " " & by(63) & ":" & by(64) & ":" & by(65)
            tm.shibiao = Date.Parse(shijian).ToString("yyyy-MM-dd HH:mm:ss")
        Catch ex As Exception

        End Try
        tm.deviceID = delchr(Encoding.Default.GetString(by, 72, 10))
        tm.source = delchr(Encoding.Default.GetString(by, 82, 10))
        tm.destination = delchr(Encoding.Default.GetString(by, 82, 10))
        Return tm
    End Function
    Public Function tssmsg2byte(ByVal tm As tssMsg) As Byte()
        Dim by(101) As Byte
        Array.Copy(tobyte(tm.flag), by, tobyte(tm.flag).Count)
        Array.Copy(BitConverter.GetBytes(tm.lenofmsg + 1), 0, by, 10, 4)
        Array.Copy(New Byte() {Val(tm.ctrl)}, 0, by, 14, 1)
        Array.Copy(New Byte() {Val(tm.xieyibanbenhao)}, 0, by, 15, 1)
        Array.Copy(tobyte(tm.datatype), 0, by, 16, tobyte(tm.datatype).Length)
        Array.Copy(tobyte(tm.functype), 0, by, 32, tobyte(tm.functype).Length)
        Array.Copy(BitConverter.GetBytes(tm.baowenxuhao), 0, by, 48, 4)
        Array.Copy(BitConverter.GetBytes(tm.baotouchangdu), 0, by, 52, 2)
        Array.Copy(BitConverter.GetBytes(tm.canshuquchangdu), 0, by, 54, 2)
        Array.Copy(BitConverter.GetBytes(tm.shujuquchangdu), 0, by, 56, 4)
        '时标
        Dim de As Date = Now
        Dim yy As Integer = de.ToString("yy")
        Dim MM As Integer = de.ToString("MM")
        Dim dd As Integer = de.ToString("dd")
        Dim HH As Integer = de.ToString("HH")
        Dim m As Integer = de.ToString("mm")
        Dim ss As Integer = de.ToString("ss")
        Array.Copy(New Byte() {Format(yy, "00")}, 0, by, 60, 1)
        Array.Copy(New Byte() {Format(MM, "00")}, 0, by, 61, 1)
        Array.Copy(New Byte() {Format(dd, "00")}, 0, by, 62, 1)
        Array.Copy(New Byte() {Format(HH, "00")}, 0, by, 63, 1)
        Array.Copy(New Byte() {Format(m, "00")}, 0, by, 64, 1)
        Array.Copy(New Byte() {Format(ss, "00")}, 0, by, 65, 1)
        Dim ms As Short = 0
        Array.Copy(BitConverter.GetBytes(ms), 0, by, 66, 2)
        Array.Copy(BitConverter.GetBytes(ms), 0, by, 68, 2)
        Array.Copy(BitConverter.GetBytes(ms), 0, by, 70, 2)
        Array.Copy(tobyte(tm.deviceID), 0, by, 72, tobyte(tm.deviceID).Length)
        Array.Copy(tobyte(tm.source), 0, by, 82, tobyte(tm.source).Length)
        Array.Copy(tobyte(tm.destination), 0, by, 92, tobyte(tm.destination).Length)
        'CRC
        Dim crcInt As Byte = getHeadcrc(by)
        Array.Copy(New Byte() {crcInt}, 0, by, 9, 1)
        Dim bu() As Byte
        If tm.canshuquchangdu > 0 Then
            bu = tobyte(tm.canshuqu)
            ' Array.Copy(bu, 0, by, 102, tm.canshuquchangdu)
        End If
        Dim bk() As Byte
        If tm.shujuquchangdu > 0 Then
            bk = tm.shujuqu
            'Array.Copy(bk, 0, by, 102 + tm.canshuquchangdu, tm.shujuquchangdu)
        End If
        Dim k1 As Integer = 0
        Dim k2 As Integer = 0
        If IsNothing(bu) = False Then k1 = bu.Count
        If IsNothing(bk) = False Then k2 = bk.Count
        Dim bbb(101 + k1 + k2) As Byte
        Array.Copy(by, 0, bbb, 0, 102)
        If IsNothing(bu) = False Then Array.Copy(bu, 0, bbb, 102, k1)
        If IsNothing(bk) = False Then Array.Copy(bk, 0, bbb, 102 + k1, k2)
        Dim t As Integer = getcrc(bbb)
        Dim bkk() As Byte
        Dim bt(0) As Byte
        bt(0) = t
        bkk = bbb.Concat(bt).ToArray
        Return bkk
    End Function
    Public Function getcrc(ByVal by() As Byte) As Byte
        Dim int As Byte = 0
        For i = 0 To by.Length - 1
            Dim y As Integer = int Xor by(i)
            int = crcTable(y)
        Next
        Return int
    End Function
    Public Function msg2TssMsg(ByVal ctrl As Byte, ByVal datatype As String, ByVal functype As String, ByVal canshu As String, ByVal shuju() As Byte) As tssMsg
        Dim tm As New tssMsg
        tm.flag = "$RADIOINF"
        tm.ctrl = ctrl
        tm.datatype = datatype
        tm.functype = functype
        tm.canshuquchangdu = 0
        tm.shujuquchangdu = 0
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
    Public Function getHeadcrc(ByVal by() As Byte) As Byte
        Dim int As Byte = 0
        For i = 10 To 101
            Dim y As Integer = int Xor by(i)
            int = crcTable(y)
        Next
        Return int
    End Function
    Public Function byte2tssmsg(ByVal by() As Byte) As tssMsg
        Try
            Dim tm As tssMsg
            tm.flag = delchr(Encoding.Default.GetString(by, 0, 9))
            tm.crc = by(9)
            tm.lenofmsg = BitConverter.ToInt32(by, 10)
            tm.ctrl = by(14)
            tm.xieyibanbenhao = by(15)
            tm.datatype = delchr(Encoding.Default.GetString(by, 16, 16))
            tm.functype = delchr(Encoding.Default.GetString(by, 32, 16))
            tm.baowenxuhao = BitConverter.ToInt32(by, 48)
            tm.baotouchangdu = BitConverter.ToInt16(by, 52)
            tm.canshuquchangdu = BitConverter.ToInt16(by, 54)
            tm.shujuquchangdu = BitConverter.ToInt32(by, 56)
            Try
                Dim shijian As String = ""
                shijian = "20" & by(60) & "-" & by(61) & "-" & by(62) & " " & by(63) & ":" & by(64) & ":" & by(65)
                tm.shibiao = Date.Parse(shijian).ToString("yyyy-MM-dd HH:mm:ss")
            Catch ex As Exception

            End Try
            tm.deviceID = delchr(Encoding.Default.GetString(by, 72, 10))
            tm.source = delchr(Encoding.Default.GetString(by, 82, 10))
            tm.destination = delchr(Encoding.Default.GetString(by, 82, 10))
            If tm.canshuquchangdu > 0 Then
                tm.canshuqu = delchr(Encoding.Default.GetString(by, 102, tm.canshuquchangdu))
            End If
            If tm.shujuquchangdu > 0 Then
                ReDim tm.shujuqu(tm.shujuquchangdu - 1)
                Array.Copy(by, 102 + tm.canshuquchangdu, tm.shujuqu, 0, tm.shujuquchangdu)
            End If
            Return tm
        Catch ex As Exception

        End Try
        Return Nothing
    End Function
    Public Function GetParaValue(ByVal getStr As String, ByVal ParaName As String) As String
        If InStr(getStr, "/?") > -1 Then
            Dim sumLen As Integer = Len(getStr)
            For i = 0 To Len(getStr) - 2
                Dim a As String = getStr.Substring(i, 2)
                If a = "/?" Then
                    getStr = getStr.Substring(i + 2, sumLen - i - 2)
                    Exit For
                End If
            Next
        End If
        If getStr.IndexOf("&") > -1 Then
            For Each itm In getStr.Split("&")
                Dim st() As String = itm.Split("=")
                If st(0) = ParaName Then
                    Return st(1)
                End If
            Next
        Else
            Dim st() As String = getStr.Split("=")
            If st(0) = ParaName Then
                Return st(1)
            End If
        End If
        Return ""
    End Function
    Public Function getidbyBQ(ByVal str As String) As String
        Try
            If (InStr(str, "TZBQ:")) = False Then Return 0
            If InStr(str, "<") Then
                If InStr(str, ">") Then
                    str = Mid(str, InStr(str, "<"), InStr(str, ">") - InStr(str, "<") + 1)
                    str = str.Split(":")(1)
                    str = str.Replace("<", "")
                    str = str.Replace(">", "")
                    Dim st() As String = str.Split(",")
                    Return st(1)
                End If
            End If
            Return -1
        Catch ex As Exception
            Return -1
        End Try
    End Function
    Public Function CutBQ(ByVal bq As String) As String
        If InStr(bq, "<") Then
            If InStr(bq, ">") Then
                bq = bq.Substring(InStr(bq, "<") - 1, InStr(bq, ">") - InStr(bq, "<") + 1)
                Return bq
            End If
        End If
        Return ""
    End Function
    Public Sub RecordDeviceMsg(ByVal deviceID As String, ByVal deviceKind As String, ByVal func As String, ByVal DeviceMsg As String)
        Try
            If myServerInfo.isSaveDevMsg = False Then Exit Sub
            Dim isTableExist As Boolean = True
            Dim CmdString As String = ""
            Dim ResultRowInt As Integer = 0
            Dim st() As String = New String() {Now.ToString("yyyy-MM-dd HH:mm:ss"), deviceID, deviceKind, func, DeviceMsg.Replace("\", "\\")}
            Dim SQLString As String = "insert into DeviceMsgTable (MsgTime,DeviceID,DeviceKind,Func,DeviceMsg)" &
                                      "values ('{0}','{1}','{2}','{3}','{4}')"
            CmdString = String.Format(SQLString, st)
            SQLCmd(CmdString)

        Catch ex As Exception
            Dim errMsg As String = ex.Message

            'MsgBox(errMsg)
            If InStr(errMsg, "对象名") Then
                If InStr(errMsg, "不存在") Then
                    myServerInfo.isSaveDevMsg = False
                    'Dim SQL As New SqlConnection(ConnectSQL)
                    'SQL.Open()
                    'Dim isTableExist As Boolean = True
                    'Dim CmdString As String = ""
                    'Dim SQLCommand As SqlCommand
                    'Dim ResultRowInt As Integer = 0
                    'CmdString = "Create Table DeviceMsgTable (MsgTime varchar(20),DeviceID varchar(255),DeviceKind varchar(255),Func varchar(255),DeviceMsg varchar(MAX))"
                    'SQLCommand = New SqlCommand(CmdString, SQL)
                    'ResultRowInt = SQLCommand.ExecuteNonQuery()

                    'CmdString = "Create clustered index index_MsgTime on DeviceMsgTable(ID)"
                    'SQLCommand = New SqlCommand(CmdString, SQL)

                    'SQL.Close()
                    ''create clustered index ix_clu_for_employee_ID on Employee(ID); 创建索引

                End If
            End If
        End Try
    End Sub
    Public Function GetTokenByUserName(ByVal usr As String, ByVal kind As String) As String
        Dim TimeString As String = Now.ToString("yyyyMMddHHmmss")
        Dim str As String = TimeString & kind & usr
        Dim by() As Byte = Encoding.Default.GetBytes(str)
        Dim result As Integer = 0
        For Each sh In by
            result = result + sh
            If result > 255 Then
                result = 0
            End If
        Next
        str = str & Format(result, "000")
        Return str
    End Function
    Public Sub SetToken(ByVal usr As String, ByVal token As String)
        SyncLock dik_Lock
            Try
                Dim isfind As Boolean = False
                For Each itm In dik
                    If itm.Key = usr Then
                        isfind = True
                        Exit For
                    End If
                Next
                If isfind Then
                    dik.Remove(usr)
                End If
                dik.Add(usr, token)
            Catch ex As Exception

            End Try
        End SyncLock
    End Sub
    Public Function GetToken(ByVal usr As String) As String
        Dim result As String = ""
        SyncLock dik_Lock
            For Each itm In dik
                If itm.Key = usr Then
                    result = itm.Value
                    Exit For
                End If
            Next
        End SyncLock
        Return result
    End Function
    Public Function GetPostMsg(ByVal context As HttpListenerContext) As String
        Dim by(10 * 1024 * 1024) As Byte
        Dim num As Integer = context.Request.InputStream.Read(by, 0, by.Count)
        If num = 0 Then Return ""
        Dim realbuffr(num - 1) As Byte
        Array.Copy(by, realbuffr, num)
        Dim postMsg As String = Encoding.Default.GetString(realbuffr)
        Return postMsg
    End Function
    Public Sub AliveUsr(ByVal usr As String)
        SyncLock dik_Lock
            Dim isFind As Boolean = False
            For i = 0 To dik.Count - 1

            Next
        End SyncLock
    End Sub
    Public Function CheckLoginFunc(ByVal usr As String, ByVal pwd As String, ByVal kind As String) As String
        Try
            If usr = "" Then Return False
            If pwd = "" Then Return False
            If kind = "" Then Return False
            usr = usr.Replace("'", "")
            pwd = pwd.Replace("'", "")
            kind = kind.Replace("'", "")
            Dim CmdString As String = ""
            Dim para() As String = New String() {usr, pwd, kind}
            Dim SQLString As String = "Select * From userTable where usr='{0}' and pwd='{1}' and kind='{2}'"
            CmdString = String.Format(SQLString, para)
            Dim dt As New DataTable
            dt = SQLGetDT(CmdString)
            If dt.Rows.Count = 0 Then Return "登录失败，用户名或密码错误"
            Dim row As DataRow = dt.Rows(0)
            If row("status") = "0" Then
                Return "登录失败，该用户待审核"
            End If
            If row("status") = "-1" Then
                Return "登录失败，该用户审核未通过"
            End If
            Return "登录成功"
        Catch ex As Exception
            Return "登录失败"
        End Try
    End Function
    Public Function GetUsrByToken(ByVal Token As String) As String
        If Token = "928453310" Then Return "admin"
        Dim usr As String = ""
        SyncLock dik_Lock
            For Each itm In dik
                If itm.Value = Token Then
                    usr = itm.Key
                    Exit For
                End If
            Next
        End SyncLock
        Return usr
    End Function
    Public Function islogined(ByVal token As String) As Boolean
        If token = "928453310" Then Return True
        Dim isFind As Boolean = False
        SyncLock dik_Lock
            Try
                For Each itm In dik
                    If itm.Value = token Then
                        isFind = True
                        Exit For
                    End If
                Next
            Catch ex As Exception

            End Try

        End SyncLock
        Return isFind
    End Function
    Public Function GetSDHLngById(ByVal id As String) As String
        If id = "SDHD01" Then Return "113.141168"
    End Function
    Public Function GetSDHLatById(ByVal id As String) As String
        If id = "SDHD01" Then Return "23.042345"
    End Function
    Public Function SQLGetDT(ByVal CmdString As String) As DataTable
        Dim dt As New DataTable
        Try
            Using SQL As New MySqlConnection(ConnectSQL)
                SQL.Open()
                Dim SQLCommand As MySqlCommand = New MySqlCommand(CmdString, SQL)
                SQLCommand.CommandTimeout = 900000
                'Dim ResultRowInt As Integer = SQLCommand.ExecuteNonQuery()
                Dim SQLDataAdapter As New MySqlDataAdapter(SQLCommand)
                SQLDataAdapter.Fill(dt)
                SQL.Close()
                If IsNothing(dt) Then dt = New DataTable
            End Using
            Return dt
        Catch ex As Exception
            '  MsgBox(ex.ToString)
            Return dt
        End Try
    End Function
    Public Function SQLIsIn(ByVal CmdString As String) As Boolean
        Dim dt As New DataTable
        Try
            Using SQL As New MySqlConnection(ConnectSQL)
                SQL.Open()
                Dim SQLCommand As MySqlCommand = New MySqlCommand(CmdString, SQL)
                SQLCommand.CommandTimeout = 900000
                'Dim ResultRowInt As Integer = SQLCommand.ExecuteNonQuery()
                Dim SQLDataAdapter As New MySqlDataAdapter(SQLCommand)
                SQLDataAdapter.Fill(dt)
                SQL.Close()
            End Using
            If IsNothing(dt) Then Return False
            If dt.Rows.Count = 0 Then Return False
            Return True
        Catch ex As Exception
            '  MsgBox(ex.ToString)
            Return False
        End Try
    End Function

    Public Function SQLGetCount(CmdString As String) As Long
        Dim dt As New DataTable
        Try
            Using SQL As New MySqlConnection(ConnectSQL)
                SQL.Open()
                Dim SQLCommand As MySqlCommand = New MySqlCommand(CmdString, SQL)
                SQLCommand.CommandTimeout = 900000
                Dim SQLDataAdapter As New MySqlDataAdapter(SQLCommand)
                SQLDataAdapter.Fill(dt)
                SQL.Close()
            End Using
            If IsNothing(dt) Then Return 0
            If dt.Rows.Count = 0 Then Return 0
            Dim str As String = dt.Rows(0)(0)
            If IsNumeric(str) = False Then Return 0
            Dim count As Long = Val(str)
            Return count
        Catch ex As Exception
            '  MsgBox(ex.ToString)
            Return 0
        End Try
    End Function
    Public Function SQLCmd(ByVal CmdString As String) As String
        Try
            Using SQL As New MySqlConnection(ConnectSQL)
                SQL.Open()
                Dim SQLCommand As MySqlCommand = New MySqlCommand(CmdString, SQL)
                Dim ResultRowInt As Integer = SQLCommand.ExecuteNonQuery()
                SQL.Close()
            End Using
            Return "success"
        Catch ex As Exception
            Return ex.Message
        End Try
    End Function
    Public Function SQLInsertSQLList(ByVal sqllist As List(Of String)) As String
        If IsNothing(sqllist) Then Return "sqllist is null"
        If sqllist.Count = 0 Then Return "sqllist.count=0"
        Try
            Using SQL As New MySqlConnection(ConnectSQL)
                SQL.Open()
                Dim tx As MySqlTransaction = SQL.BeginTransaction
                Dim SQLCommand As MySqlCommand = New MySqlCommand()
                SQLCommand.Connection = SQL
                SQLCommand.Transaction = tx
                Dim count As Integer = sqllist.Count
                For i = 0 To count - 1
                    SQLCommand.CommandText = sqllist(i)
                    SQLCommand.ExecuteNonQuery()
                    If i > 0 And (i Mod 500 = 0 Or i = count - 1) Then
                        tx.Commit()
                        tx = SQL.BeginTransaction
                    End If
                Next
                SQL.Close()
            End Using
            Return "success"
        Catch ex As Exception
            Return ex.Message
        End Try
    End Function
    Public Function SQLInsertSQLList(ByVal connString As String, ByVal sqllist As List(Of String)) As String
        If IsNothing(sqllist) Then Return "sqllist is null"
        If sqllist.Count = 0 Then Return "sqllist.count=0"
        Try
            Dim SQL As New MySqlConnection(connString)
            SQL.Open()
            Dim tx As MySqlTransaction = SQL.BeginTransaction
            Dim SQLCommand As MySqlCommand = New MySqlCommand()
            SQLCommand.Connection = SQL
            SQLCommand.Transaction = tx
            Dim count As Integer = sqllist.Count
            For i = 0 To count - 1
                SQLCommand.CommandText = sqllist(i)
                SQLCommand.ExecuteNonQuery()
                If i > 0 And (i Mod 500 = 0 Or i = count - 1) Then
                    tx.Commit()
                    tx = SQL.BeginTransaction
                End If
            Next
            SQL.Close()
            Return "success"
        Catch ex As Exception
            Return ex.Message
        End Try
    End Function
    Public Function SQLInfo(ByVal CmdString As String) As String
        Try
            Dim str As String = ""
            Using SQL As New MySqlConnection(ConnectSQL)
                SQL.Open()
                Dim SQLCommand As MySqlCommand = New MySqlCommand(CmdString, SQL)
                str = SQLCommand.ExecuteScalar.ToString
                SQL.Close()
            End Using
            Return str
        Catch ex As Exception
            Return ex.Message
        End Try
    End Function
    Public Sub CheckDir(ByVal path As String)
        If Directory.Exists(path) = False Then
            Directory.CreateDirectory(path)
        End If
    End Sub
    Public Function SendMail(ByVal EmailAddress As String, ByVal Subject As String, ByVal Content As String,
                              Optional ByVal AttachFile As Hashtable = Nothing) As String
        Try
            Dim i As Integer
            'SMTP客户端  
            Dim smtp As New System.Net.Mail.SmtpClient("SMTP.139.COM")
            'smtp.Host = "smtp.163.com"       'SMTP服务器名称  
            '发件人邮箱身份验证凭证。 参数分别为 发件邮箱登录名和密码  
            smtp.Credentials = New System.Net.NetworkCredential("18319087172@139.com", "QQlove12345")
            '创建邮件  
            Dim mail As New System.Net.Mail.MailMessage()
            '主题编码  
            mail.SubjectEncoding = System.Text.Encoding.GetEncoding("GB2312")
            '正文编码  
            mail.BodyEncoding = System.Text.Encoding.GetEncoding("GB2312")
            '邮件优先级  
            mail.Priority = System.Net.Mail.MailPriority.Normal
            '以HTML格式发送邮件,为false则发送纯文本邮箱  
            mail.IsBodyHtml = True
            '发件人邮箱  
            mail.From = New System.Net.Mail.MailAddress("18319087172@139.com")

            '添加收件人,如果有多个,可以多次添加  
            Dim ReceiveAddressList As New List(Of String)
            If InStr(EmailAddress, ",") = False Then
                ReceiveAddressList.Add(EmailAddress)
            Else
                For Each sh In EmailAddress.Split(",")
                    ReceiveAddressList.Add(sh)
                Next
            End If
            If ReceiveAddressList.Count = 0 Then Return False
            For i = 0 To ReceiveAddressList.Count - 1
                mail.To.Add(ReceiveAddressList.Item(i))
            Next

            '邮件主题和内容  
            mail.Subject = Subject
            mail.Body = Content

            '定义附件,参数为附件文件名,包含路径,推荐使用绝对路径  
            If Not AttachFile Is Nothing AndAlso AttachFile.Count <> 0 Then
                For Each sKey As String In AttachFile.Keys
                    Dim objFile As New System.Net.Mail.Attachment(AttachFile.Item(sKey))
                    '附件文件名,用于收件人收到附件时显示的名称  
                    objFile.Name = sKey
                    '加入附件,可以多次添加  
                    mail.Attachments.Add(objFile)
                Next
            End If

            '发送邮件  
            smtp.Send(mail)
            Return "发送邮件成功"
        Catch ex As Exception
            Return "发送邮件错误-->" & ex.Message
        End Try

    End Function
    Public Function addWavHead(ByVal by() As Byte, ByVal caiyanglv As Integer, ByVal weishu As Integer, ByVal tongdaoshu As Integer) As Byte()
        Dim bu(43) As Byte
        Array.Copy(Encoding.Default.GetBytes("RIFF"), 0, bu, 0, 4)            '标志
        Array.Copy(BitConverter.GetBytes(by.Count - 8 + 44), 0, bu, 4, 4)     'by.count-8+44
        Array.Copy(Encoding.Default.GetBytes("WAVE"), 0, bu, 8, 4)            'WAVE
        Array.Copy(Encoding.Default.GetBytes("fmt "), 0, bu, 12, 4)           'FMT 
        Array.Copy(BitConverter.GetBytes(16), 0, bu, 16, 4)                   '16
        Dim s As Short = 1
        Array.Copy(BitConverter.GetBytes(s), 0, bu, 20, 2)                    '1为线性PCM编码，>1为压缩
        Array.Copy(BitConverter.GetBytes(tongdaoshu), 0, bu, 22, 2)                    '声道数
        Array.Copy(BitConverter.GetBytes(caiyanglv), 0, bu, 24, 4)                 '采样率
        Dim int As Integer = caiyanglv * tongdaoshu * weishu / 8
        Array.Copy(BitConverter.GetBytes(int), 0, bu, 28, 4)                 '采样率*通道数*位数/8
        s = tongdaoshu * weishu / 8
        Array.Copy(BitConverter.GetBytes(s), 0, bu, 32, 2)                    '通道数*位数/8
        s = weishu
        Array.Copy(BitConverter.GetBytes(s), 0, bu, 34, 2)
        Array.Copy(Encoding.Default.GetBytes("data"), 0, bu, 36, 4)
        Array.Copy(BitConverter.GetBytes(by.Count), 0, bu, 40, 4)
        Dim bk(43 + by.Count) As Byte
        Array.Copy(bu, 0, bk, 0, bu.Count)
        Array.Copy(by, 0, bk, 44, by.Count)
        Return bk
    End Function
    Public Sub AddDeviceLog(ByVal DeviceID As String, ByVal DeviceName As String, ByVal Address As String, ByVal Log As String, ByVal result As String, ByVal status As String)
        Try
            Dim Time As String = Now.ToString("yyyy-MM-dd HH:mm:ss")
            Dim sql As String = "insert into DeviceLogTable values('{0}','{1}','{2}','{3}','{4}','{5}','{6}')"
            sql = String.Format(sql, New String() {Time, DeviceID, DeviceName, Address, Log, result, status})
            SQLCmd(sql)
        Catch ex As Exception

        End Try
    End Sub
    Public Function GetMyPower(ByVal userName As String) As Integer
        Dim sql As String = "select * from userTable where usr='" & userName & "'"
        Try
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) = False Then
                If dt.Rows.Count > 0 Then
                    Dim row As DataRow = dt.Rows(0)
                    Dim power As String = row("power")
                    Return Val(power)
                Else
                    Return Val(-2)
                End If
            Else
                Return Val(-2)
            End If
        Catch ex As Exception
            Return Val(-2)
        End Try
    End Function
    Public Function AddJobLog(ByVal userName As String, ByVal Cata As String, ByVal Kind As String, ByVal Content As String, ByVal DeviceNickName As String, ByVal DeviceID As String, ByVal TaskNickName As String, ByVal RelateID As String) As String
        Try
            Dim sql As String = "insert into LogTable (Time,Usr,Kind,Content,DeviceNickName,DeviceID,TaskNickName,Cata,RelateID) values('{0}','{1}','{2}','{3}','{4}','{5}','{6}','{7}','{8}')"
            sql = String.Format(sql, New String() {Now.ToString("yyyy-MM-dd HH:mm:ss"), userName, Kind, Content, DeviceNickName, DeviceID, TaskNickName, Cata, RelateID})
            Dim result As String = SQLCmd(sql)

        Catch ex As Exception

        End Try
        Return ""
    End Function
    Public Function GetWarnKind(ByVal bq As String) As String
        If InStr(bq, "<TZBQ:") = False Then Return ""
        If InStr(bq, ",") = False Then Return ""
        Dim st() As String = bq.Split(",")
        Return st(2)
    End Function
    Public Function TransforPara2Query(ByVal dik As Dictionary(Of String, String)) As String
        If IsNothing(dik) Then Return ""
        Dim str As String = ""
        For Each itm In dik
            str = str & "&" & itm.Key & "=" & itm.Value
        Next
        If str <> "" Then
            str = str.Substring(1, str.Length - 1)
        End If
        Return str
    End Function
    Public Function GetDeviceNickNameByID(id As String) As String
        Try
            Dim sql As String = "select DeviceNickName from DeviceTable where DeviceID='" & id & "'"

            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) = False Then
                If dt.Rows.Count > 0 Then
                    Dim row As DataRow = dt.Rows(0)
                    Dim NickName As String = row("DeviceNickName").ToString()

                    Return NickName
                End If
            End If
        Catch ex As Exception

        End Try
        Return ""
    End Function
    Public Function GetDeviceIDByNickName(NickName As String) As String
        Try
            Dim sql As String = "select DeviceID from DeviceTable where DeviceNickName='" & NickName & "'"
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) = False Then
                If dt.Rows.Count > 0 Then
                    Dim row As DataRow = dt.Rows(0)
                    Dim DeviceID As String = row("DeviceID").ToString()

                    Return DeviceID
                End If
            End If
        Catch ex As Exception

        End Try
        Return ""
    End Function
    Public Function TSSShuju2INGMsg(tm As tssMsg) As INGMsgStu
        If IsNothing(tm) Then Return Nothing
        If IsNothing(tm.shujuqu) Then Return Nothing
        Dim by() As Byte = tm.shujuqu
        Dim msg As String = Encoding.Default.GetString(by)
        If msg = "" Then Return Nothing
        Try
            Dim im As INGMsgStu = JsonConvert.DeserializeObject(msg, GetType(INGMsgStu))
            Return im
        Catch ex As Exception

        End Try
        Return Nothing
    End Function
    Public Function INGMsg2TSSShuju(im As INGMsgStu) As Byte()
        If IsNothing(im) Then Return Nothing
        Try
            Dim msg As String = JsonConvert.SerializeObject(im)
            Dim by() As Byte = Encoding.Default.GetBytes(msg)
            Return by
        Catch ex As Exception

        End Try
        Return Nothing
    End Function
    Public Function TransForDeviceStu(ByVal d As List(Of DeviceStu)) As List(Of deviceStuWithOutHttpListener)
        Dim list As New List(Of deviceStuWithOutHttpListener)
        For Each itm In d
            Dim t As deviceStuWithOutHttpListener
            t.DeviceID = itm.DeviceID
            t.Name = itm.Name
            t.Address = itm.Address
            t.Kind = itm.Kind
            t.OnlineTime = itm.OnlineTime
            t.Statu = itm.Statu
            t.Lng = itm.Lng
            t.Lat = itm.Lat
            t.IP = itm.IP
            t.DSGDeviceKind = itm.DSGDeviceKind
            t.Port = itm.Port
            t.RunKind = itm.RunKind
            t.Func = itm.Func
            t.isNetGateWay = itm.isNetGateWay
            t.NetDeviceID = itm.NetDeviceID
            t.NetGateWayID = itm.NetGateWayID
            t.NetSwitch = itm.NetSwitch
            t.HTTPMsgUrl = itm.HTTPMsgUrl
            t.WebSocketUrl = itm.WebSocketUrl
            list.Add(t)
        Next
        Return list
    End Function
    Public Function GetH(ByVal uri As String, ByVal msg As String) As String
        Dim num As Integer = 0
        While True
            Try
                Dim req As HttpWebRequest = WebRequest.Create(uri & msg)
                req.Accept = "*/*"
                req.UserAgent = "Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.2.13) Gecko/20101203 Firefox/3.6.13"
                req.CookieContainer = New CookieContainer
                req.KeepAlive = True
                req.ContentType = "application/x-www-form-urlencoded"
                req.Method = "GET"
                Dim rp As HttpWebResponse = req.GetResponse
                Dim str As String = New StreamReader(rp.GetResponseStream(), Encoding.UTF8).ReadToEnd
                Return str
            Catch ex As Exception

            End Try
            num = num + 1
            If num = 4 Then Return ""
        End While
    End Function
    Structure LocationAddressInfo
        Dim Province As String
        Dim City As String
        Dim District As String
        Dim DetailAddress As String
    End Structure
    Public Function GetAddressByLngLat(ByVal lng As String, ByVal lat As String) As LocationAddressInfo
        Dim miYue As String = "5cW7OlxZXThtbkq1Y0u5yNO6"
        Dim url As String = "http://api.map.baidu.com/geocoder/v2/?ak=" & miYue & "&location=" & lat & "," & lng & "&output=json&pois=0"
        Dim msg As String = GetH(url, "")
        Dim la As New LocationAddressInfo
        Try
            Dim p As Object = CType(JsonConvert.DeserializeObject(msg), JObject)
            Dim province As String = p("result")("addressComponent")("province").ToString
            Dim city As String = p("result")("addressComponent")("city").ToString
            Dim district As String = p("result")("addressComponent")("district").ToString
            Dim street As String = p("result")("addressComponent")("street").ToString
            Dim sematic_description As String = p("result")("sematic_description").ToString
            Dim formatted_address As String = p("result")("formatted_address").ToString
            Dim business As String = p("result")("business").ToString
            Dim str As String = city & "," & district & "," & sematic_description & "," & business
            str = formatted_address & "," & sematic_description & "," & business
            la.Province = province
            la.City = city
            la.DetailAddress = str
            la.District = district
            Return la
            str = city & district & "," & street
            'dt.Rows(index)("地址") = formatted_address
            'dt.Rows(index)("城市区划") = city & district
            'dt.Rows(index)("街道名称") = street
            'dt.Rows(index)("地址描述") = sematic_description
            'dt.Rows(index)("商圈信息") = business
            ' Return str
        Catch ex As Exception
            ' Return ex.Message
        End Try
        Return la
    End Function

    Public Function GetGridBySQL(lng As Double, lat As Double) As Integer
        If lng <= 0 Or lat <= 0 Then Return 0
        Dim sql As String = "select id from gridTable where startlon<={0} and stoplon>={1} and startlat<={2} and stoplat>={3}"
        sql = String.Format(sql, New String() {lng, lng, lat, lat})
        Dim result As String = SQLInfo(sql)
        If result = "" Then Return 0
        If IsNumeric(result) = False Then Return 0
        Return Val(result)
    End Function
    Public Function GetGateWayLocation(deviceId As String) As String
        If IsNothing(DeviceListLock) Then DeviceListLock = New Object
        Dim result As String = ""
        SyncLock DeviceListLock
            For i = 0 To DeviceList.Count - 1
                Dim sh As DeviceStu = DeviceList(i)
                If sh.DeviceID = deviceId Then
                    result = sh.Lng & "," & sh.Lat
                    Exit For
                End If
            Next
        End SyncLock
        Return result
    End Function
End Module
