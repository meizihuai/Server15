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
Imports System.Math
Public Class TaskTZBQ
    Public myDeviceId As String
    Dim taskNickName As String
    Dim myTask As NormalTaskStu
    Dim isPOATaskMyMain As Boolean
    Sub New(_myDeviceId As String, row As DataRow, _isPOATaskMyMain As Boolean)
        myDeviceId = _myDeviceId

        isPOATaskMyMain = _isPOATaskMyMain
        Dim task As New NormalTaskStu
        task.StartTime = row("StartTime")
        task.EndTime = row("EndTime")
        task.TimeStep = row("TimeStep")
        task.TaskName = row("TaskName")
        task.TaskNickName = row("TaskNickName")
        taskNickName = row("TaskNickName")
        task.TaskCode = row("TaskCode")
        task.PushWeChartToUserName = row("PushWeChatUserName")
        task.PushEmailToUserName = row("PushEmailUserName")
        task.UserName = row("UserName")
        task.DeviceID = row("DeviceID")
        myDeviceId = task.DeviceID
        If task.TaskName = "POA定位" Then
            Try
                Dim list As List(Of String) = JsonConvert.DeserializeObject(myDeviceId, GetType(List(Of String)))
                If IsNothing(list) = False Then
                    If list.Count > 0 Then myDeviceId = list(0) : isPOATaskMyMain = True
                End If
            Catch ex As Exception

            End Try
        End If
        task.DeviceName = row("DeviceName")
        myTask = task
    End Sub
    Sub New(_myDeviceId As String, _task As NormalTaskStu, _isPOATaskMyMain As Boolean)
        myDeviceId = _myDeviceId
        myTask = _task
        isPOATaskMyMain = _isPOATaskMyMain
    End Sub
    Public Sub MakeTaskReport()
        Dim th As New Thread(AddressOf SubMakeTaskTeport)
        th.Start()
    End Sub

    Private Sub SubMakeTaskTeport()
        Try
            Dim task As NormalTaskStu
            If IsNothing(myTask) Then Return
            task = myTask

            If task.TaskName = "POA定位" Then
                If isPOATaskMyMain = False Then
                    Return
                End If
            End If
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
            sb.Append("ErrMsg='{1}',")
            sb.Append("isClosed=1 ")
            sb.Append("Where TaskNickName='{2}'")
            log(task.DeviceID & "更新任务数据库……")
            Dim ReportUrl As String = "http://123.207.31.37:8082/Task/Reports/" & myDeviceId & "/" & task.TaskNickName & "_" &
                                      Date.Parse(task.StartTime).ToString("yyyyMMddHHmmss") & "_" &
                                      Date.Parse(task.EndTime).ToString("yyyyMMddHHmmss") & ".docx"
            Dim errMsg As String = ""
            If path = "" Then
                ReportUrl = ""
                If task.TaskName = "事件预警" Or task.TaskName = "状态预警" Then
                    errMsg = "本次任务属于预警类任务，暂不生成报告"
                Else
                    errMsg = "报告生成失败"
                End If
            Else
                If File.Exists(path) = False Then
                    ReportUrl = ""
                    If task.TaskName = "事件预警" Or task.TaskName = "状态预警" Then
                        errMsg = "本次任务属于预警类任务，暂不生成报告"
                    Else
                        errMsg = "报告生成失败"
                    End If
                End If
            End If
            cmdString = String.Format(sb.ToString, New String() {ReportUrl, errMsg, task.TaskNickName})
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
            If task.TaskName = "台站监督" Or task.TaskName = "可用评估" Or task.TaskName = "违章台站" Then
                Return HandleTaskKeyongpinggu(task)
            End If
            If task.TaskName = "POA定位" Then

                Return HandleTaskPOA(task)
            End If
        Catch ex As Exception
            ' MsgBox(ex.ToString)
            log("报告制作ERR>>" & ex.Message)
        End Try
    End Function
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
        CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceId)
        Dim path As String = Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceId & "\" & Task.TaskNickName & "_" &
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
    Private Function HandleTaskKeyongpinggu(ByVal Task As NormalTaskStu) As String
        log("开始制作报告……")
        Dim menXian As Double = -95
        Dim sb As New StringBuilder
        sb.Append("select * from DeviceMsgTable ")
        sb.Append("where DeviceID='{0}' ")
        sb.Append("and Func='{1}' ")
        sb.Append("and MsgTime between '{2}' and '{3}' order by MsgTime")
        Dim para() As String = New String() {myDeviceId, "SSSJ", Task.StartTime, Task.EndTime}
        Dim dt As DataTable = SQLGetDT(String.Format(sb.ToString, para))
        CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceId)
        Dim path As String = Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceId & "\" & Task.TaskNickName & "_" &
                                  Date.Parse(Task.StartTime).ToString("yyyyMMddHHmmss") & "_" &
                                  Date.Parse(Task.EndTime).ToString("yyyyMMddHHmmss") & ".docx"
        Dim pdList As New List(Of String)
        Dim code As String = Task.TaskCode
        code = code.Replace("[", "")
        code = code.Replace("]", "")
        Dim PDStr As String = "["
        If InStr(code, ",") Then
            Dim pds() As String = code.Split(",")
            For Each itm In pds
                If itm = "" Then Continue For
                If IsNumeric(itm) Then
                    pdList.Add(Val(itm))
                    PDStr = PDStr & itm & "MHz,"
                End If
            Next
        End If
        PDStr = PDStr.Substring(0, PDStr.Length - 1) & "]"
        Dim doc As DocX = DocX.Create(path)
        doc.InsertParagraph.AppendLine(Task.TaskNickName & "监测报告").Font(New FontFamily("黑体")).FontSize(25)
        doc.InsertParagraph.AppendLine("1.概述").Font(New FontFamily("宋体")).FontSize(20)
        doc.InsertParagraph.AppendLine("     本次监测任务主要监督" & PDStr & ",共计" & pdList.Count & "个频点的占用度、时序图等信息").Font(New FontFamily("宋体")).FontSize(15)
        If IsNothing(dt) Then
            log("查询历史设备消息失败或者没有历史设备消息")
            doc.Save()
            Return ""
        End If
        Dim jcpdList As New List(Of JCPD)
        'Dim tList As New List(Of reportTabledataStu)
        For Each r As DataRow In dt.Rows
            Dim time As String = r("MsgTime")
            Dim msg As String = r("DeviceMsg")
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
                    jcpdList.Add(New JCPD(time, PD, CQ))
                Next
            End If
        Next
        doc.InsertParagraph.AppendLine("2.占用度分析总览").Font(New FontFamily("宋体")).FontSize(20)
        Dim tab As Table = doc.AddTable(pdList.Count + 1, 7)
        '序号  频率  占用度  是否可用  最大值  最小值  平均值
        tab.Design = TableDesign.TableGrid
        tab.Alignment = Alignment.center
        For i = 0 To 6
            tab.Rows(0).Cells(i).FillColor = Color.FromArgb(226, 226, 226)
        Next
        tab.Rows(0).Cells(0).Paragraphs(0).Append("序号").Bold().FontSize(12)
        tab.Rows(0).Cells(1).Paragraphs(0).Append("频率(MHz)").Bold().FontSize(12)
        tab.Rows(0).Cells(2).Paragraphs(0).Append("占用度").Bold().FontSize(12)
        tab.Rows(0).Cells(3).Paragraphs(0).Append("是否可用").Bold().FontSize(12)
        tab.Rows(0).Cells(4).Paragraphs(0).Append("最大值").Bold().FontSize(12)
        tab.Rows(0).Cells(5).Paragraphs(0).Append("最小值").Bold().FontSize(12)
        tab.Rows(0).Cells(6).Paragraphs(0).Append("平均值").Bold().FontSize(12)
        Dim xx_zyd As New List(Of Single)
        Dim yy_zyd As New List(Of Single)
        For m = 0 To pdList.Count - 1
            Dim pd As Double = Val(pdList(m))
            xx_zyd.Add(pd)
            Dim TimeInt As Integer = Val(Task.TimeStep)
            If TimeInt < 5 Then TimeInt = 5
            Dim num As Integer = jcpdList.Count
            Dim oldTime As String = ""
            Dim zyd As String
            Dim zyNum As Integer = 0
            Dim max As Double = -200
            Dim min As Double = 0
            Dim sum As Double = 0
            Dim avg As Double = 0
            Dim sunNum As Integer = 0
            For i = 0 To num - 1
                Dim itm As JCPD = jcpdList(i)
                If itm.PD <> pd Then Continue For
                Dim tStr As String = itm.msgTime
                Dim time As Date = Date.Parse(tStr)
                Dim v As Double = itm.CQ
                If oldTime = "" Then
                    oldTime = time
                    If max < v Then max = v
                    If min > v Then min = v
                    sum = sum + v
                    sunNum = sunNum + 1
                    If v > menXian Then
                        zyNum = zyNum + 1
                    End If
                Else
                    Dim oTime As Date = Date.Parse(oldTime)
                    Dim t As TimeSpan = time - oTime
                    If t.Seconds >= TimeInt Then
                        If max < v Then max = v
                        If min > v Then min = v
                        sum = sum + v
                        sunNum = sunNum + 1
                        If v > menXian Then
                            zyNum = zyNum + 1
                        End If
                        oldTime = time
                    End If
                End If
            Next
            Dim zy As Double = zyNum / sunNum
            zyd = Format(zy * 100, "0.00") & " %"
            yy_zyd.Add(zy * 100)
            avg = sum / sunNum
            tab.Rows(m + 1).Cells(0).Paragraphs(0).Append(m + 1).FontSize(12)
            tab.Rows(m + 1).Cells(1).Paragraphs(0).Append(pd.ToString("0.0000")).FontSize(12)
            tab.Rows(m + 1).Cells(2).Paragraphs(0).Append(Format(zy * 100, "0.00") & " %").FontSize(12)
            If zy > 0.4 Then
                tab.Rows(m + 1).Cells(3).Paragraphs(0).Append("不可用").FontSize(12).Color(Color.Red)
            Else
                tab.Rows(m + 1).Cells(3).Paragraphs(0).Append("可用").FontSize(12).Color(Color.Green)
            End If
            tab.Rows(m + 1).Cells(4).Paragraphs(0).Append(max.ToString("0.00")).FontSize(12)
            tab.Rows(m + 1).Cells(5).Paragraphs(0).Append(min.ToString("0.00")).FontSize(12)
            tab.Rows(m + 1).Cells(6).Paragraphs(0).Append(avg.ToString("0.00")).FontSize(12)
        Next

        doc.InsertParagraph.InsertTableAfterSelf(tab)
        Dim imgPath_zyd As String = ShiXu2Img_zydFile(xx_zyd.ToArray, yy_zyd.ToArray)
        Dim nImg_zyd As Novacode.Image = doc.AddImage(imgPath_zyd)
        Dim pic_zyd As Novacode.Picture = nImg_zyd.CreatePicture
        pic_zyd.Width = 600
        pic_zyd.Height = 200
        doc.InsertParagraph.AppendPicture(pic_zyd)
        doc.InsertParagraph.AppendLine("3.时序图").Font(New FontFamily("宋体")).FontSize(20)
        For m = 0 To pdList.Count - 1
            Dim xx As New List(Of String)
            Dim yy As New List(Of Double)
            Dim pd As Double = Val(pdList(m))
            Dim TimeInt As Integer = Val(Task.TimeStep)
            If TimeInt < 5 Then TimeInt = 5
            Dim num As Integer = jcpdList.Count
            Dim oldTime As String = ""
            Dim zyd As String
            Dim zyNum As Integer = 0
            For i = 0 To num - 1
                Dim itm As JCPD = jcpdList(i)
                If itm.PD <> pd Then Continue For
                Dim tStr As String = itm.msgTime
                Dim time As Date = Date.Parse(tStr)
                If oldTime = "" Then
                    oldTime = time
                    xx.Add(itm.msgTime)
                    yy.Add(itm.CQ)
                Else
                    Dim oTime As Date = Date.Parse(oldTime)
                    Dim t As TimeSpan = time - oTime
                    If t.Seconds >= TimeInt Then
                        xx.Add(itm.msgTime)
                        yy.Add(itm.CQ)
                        If itm.CQ > menXian Then
                            zyNum = zyNum + 1
                        End If
                        oldTime = time
                    End If
                End If
            Next
            zyd = Format((zyNum / yy.Count) * 100, "0.00") & " %"
            Dim labelStr As String = pd & " MHz," & "," & Task.StartTime & " 到 " & Task.EndTime & " 信号占用时序图 占用度 " & zyd
            doc.InsertParagraph.AppendLine(" ● " & labelStr).Font(New FontFamily("宋体")).FontSize(12)
            Dim imgPath As String = ShiXu2ImgFile(xx.ToArray, yy.ToArray, pd.ToString("0.0000"), menXian, labelStr)
            Dim nImg As Novacode.Image = doc.AddImage(imgPath)
            Dim pic As Novacode.Picture = nImg.CreatePicture
            pic.Width = 600
            pic.Height = 200
            doc.InsertParagraph.AppendPicture(pic)
        Next

        doc.Save()
        doc.Dispose()
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
        Dim no As String = Guid.NewGuid.ToString("N")
        Dim timeFileName As String = no & int
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
    Private Function ShiXu2ImgFile(ByVal xx() As String, ByVal yy() As Double, ByVal pd As String, ByVal menxian As Double, ByVal labelStr As String) As String

        Dim chart As New System.Windows.Forms.DataVisualization.Charting.Chart
        chart.Width = 1000
        chart.Height = 300
        chart.BackColor = Color.White
        chart.ChartAreas.Add("时序")
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
        ser.Color = Color.Blue
        ser.IsVisibleInLegend = False
        chart.Series.Clear()
        chart.Series.Add(ser)
        Dim ser2 As New System.Windows.Forms.DataVisualization.Charting.Series("门限")
        ser2.ChartType = Charting.SeriesChartType.FastLine
        ser2("PointWidth") = 0.5
        For i = 0 To yy.Count - 1
            ser2.Points.AddXY(xx(i), menxian)
        Next
        ser2.Color = Color.FromArgb(255, 0, 0)
        ser2.IsVisibleInLegend = False
        chart.Series.Add(ser2)


        Dim NO As String = Guid.NewGuid.ToString("N")
        Dim timeFileName As String = NO & "SSSJ" & pd
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
        'Dim sr As New StreamWriter( Directory.GetCurrentDirectory()  & "\Task\TaskLog\" & mydeviceid &
    End Sub
End Class
