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

Public Class TaskTSS
    Private legalSigNal As List(Of Double)
    Public myDeviceId As String
    Dim taskNickName As String
    Dim myTask As NormalTaskStu
    Private minWarnNum As Integer = 5
    Sub New(_myDeviceId As String, row As DataRow)
        myDeviceId = _myDeviceId
        taskNickName = row("TaskNickName")

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
        myDeviceId = task.DeviceID
        task.DeviceName = row("DeviceName")
        myTask = task
    End Sub
    Sub New(_myDeviceId As String, _task As NormalTaskStu)
        myDeviceId = _myDeviceId
        myTask = _task
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
            If task.TaskName = "频谱监测" Then
                Return HandleFreqMaxTaskReport(task)
            End If
            If task.TaskName = "频谱取样" Then
                Return HandleSaveFreqStepTaskReport(task)
            End If
            If task.TaskName = "占用统计" Then
                Return HandleZhanYongTongJiTaskReport(task)
            End If
            If task.TaskName = "台站监督" Then
                Return HandleTaiZhanJianDuTaskReport(task)
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
    Private Function HandleTaiZhanJianDuTaskReport(ByVal Task As NormalTaskStu) As String '台站监督生成报告
        log("开始制作报告……")
        Dim menXian As Double = -95
        Dim sb As New StringBuilder
        sb.Append("select * from DeviceMsgTable ")
        sb.Append("where DeviceID='{0}' ")
        sb.Append("and Func='{1}' ")
        sb.Append("and MsgTime between '{2}' and '{3}' order by MsgTime")
        Dim para() As String = New String() {myDeviceId, "bscan", Task.StartTime, Task.EndTime}
        Dim dt As DataTable = SQLGetDT(String.Format(sb.ToString, para))
        CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceId)
        Dim path As String = Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceId & "\" & Task.TaskNickName & "_" &
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
                doc.InsertParagraph.AppendLine("     本次监测任务主要监督[" & freqStart.ToString("0.0000") & "MHz," & freqEnd.ToString("0.0000") & "MHz]频段,在" & Task.StartTime & "到" & Task.EndTime & "期间的台站信息").Font(New FontFamily("宋体")).FontSize(15)
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
                    For i = 0 To BscanList.Count - 1
                        Dim itm As json_PPSJ = BscanList(i)
                        itm = DSGDecompare(itm)
                        BscanList(i) = itm
                    Next
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
                    Dim watchPoints As List(Of Double) = TssBscanOrder.watchPoint
                    Dim flagHaveWatchPoints As Boolean = Not IsNothing(watchPoints)

                    Dim watchSigNalList As List(Of sigNalInfo)
                    If flagHaveWatchPoints Then
                        watchSigNalList = New List(Of sigNalInfo)
                        For Each itm In watchPoints
                            Dim s As sigNalInfo
                            s.freq = itm
                            s.max = 0
                            s.min = 0
                            s.data = New List(Of Double)
                            watchSigNalList.Add(s)
                        Next
                        If IsNothing(watchSigNalList) Then
                            flagHaveWatchPoints = False
                        End If
                    End If
                    '  log("1")
                    Dim overThreshol As Double = TssBscanOrder.Threshol
                    If overThreshol >= 0 Then
                        overThreshol = -60
                    End If
                    For Each itm In BscanList
                        Dim myXX() As Double = GetXXByFreqInfo(itm.freqStart, itm.freqStep, itm.dataCount)
                        Dim myYY() As Double = itm.value
                        If IsNothing(myXX) Then Continue For
                        If IsNothing(myYY) Then Continue For

                        If myXX.Count = myYY.Count Then
                            Dim du As Integer = 5
                            Dim fucha As Integer = 5
                            Dim sigNal(,) As Double = XinHaoFenLi(myXX, myYY, du, fucha)
                            If flagHaveWatchPoints Then
                                For i = 0 To watchSigNalList.Count - 1
                                    Dim wi As sigNalInfo = watchSigNalList(i)
                                    Dim freq As Double = wi.freq
                                    Dim index As Integer = GetWatchPointIndex(itm, freq)
                                    If index < myYY.Length Then
                                        Dim value As Double = myYY(index)
                                        If wi.max = 0 Then
                                            wi.max = value
                                        Else
                                            If wi.max < value Then
                                                wi.max = value
                                            End If
                                        End If
                                        If wi.min = 0 Then
                                            wi.min = value
                                        Else
                                            If wi.min > value Then
                                                wi.min = value
                                            End If
                                        End If
                                        If value > overThreshol Then wi.OverCount = wi.OverCount + 1
                                        wi.SumCount = wi.SumCount + 1
                                        wi.data.Add(value)
                                        watchSigNalList(i) = wi
                                    End If
                                Next
                            End If
                            If IsNothing(sigNal) = False Then
                                For i = 0 To sigNal.GetLength(0) - 1
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
                                            s.data.Add(changqiang)
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
                                        s.data = New List(Of Double)
                                        sigNalList.Add(s)
                                    End If
                                Next
                            End If
                        End If
                    Next

                    Dim halfCount As Integer = BscanList.Count / 2
                    Dim xx_zyd As New List(Of Single)
                    Dim yy_zyd As New List(Of Single)
                    For j = sigNalList.Count - 1 To 0 Step -1
                        Dim s As sigNalInfo = sigNalList(j)
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
                    Dim watchxx_zyd As New List(Of Single)
                    Dim watchyy_zyd As New List(Of Single)
                    If flagHaveWatchPoints Then
                        For i = 0 To watchSigNalList.Count - 1
                            Dim itm As sigNalInfo = watchSigNalList(i)
                            watchxx_zyd.Add(itm.freq.ToString("0.000"))
                            xx_zyd.Add(itm.freq.ToString("0.000"))
                            Dim zy As Double = itm.OverCount / itm.SumCount
                            watchyy_zyd.Add(zy * 100)
                            yy_zyd.Add(zy * 100)
                        Next
                    End If

                    doc.InsertParagraph.AppendLine("3.信号直方图").Font(New FontFamily("宋体")).FontSize(20)
                    Dim imgPath_zyd As String = ShiXu2Img_zydFile(xx_zyd.ToArray, yy_zyd.ToArray)
                    Dim nImg_zyd As Novacode.Image = doc.AddImage(imgPath_zyd)
                    Dim pic_zyd As Novacode.Picture = nImg_zyd.CreatePicture
                    pic_zyd.Width = 600
                    pic_zyd.Height = 200
                    doc.InsertParagraph.AppendPicture(pic_zyd)

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
                    If flagHaveWatchPoints Then
                        For i = 0 To watchSigNalList.Count - 1
                            sigNalList.Add(watchSigNalList(i))
                        Next
                    End If
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
                    doc.InsertParagraph.AppendLine("5.信号时序图").Font(New FontFamily("宋体")).FontSize(20)
                    For i = 0 To sigNalList.Count - 1
                        Dim s As sigNalInfo = sigNalList(i)
                        Dim pd As Double = s.freq
                        Dim yy() As Double = s.data.ToArray
                        Dim xxTmp As New List(Of String)
                        For j = 1 To yy.Length
                            xxTmp.Add(j)
                        Next
                        Dim overCount As Integer = s.OverCount
                        Dim sum As Integer = s.SumCount
                        Dim zy As Double = overCount / sum
                        Dim zyd As String = Format(zy * 100, "0.00") & " %"
                        Dim labelStrTmp As String = pd & " MHz," & "," & Task.StartTime & " 到 " & Task.EndTime & " 信号占用时序图 占用度 " & zyd
                        doc.InsertParagraph.AppendLine(" ● " & labelStrTmp).Font(New FontFamily("宋体")).FontSize(12)
                        Dim imgPathTmp As String = ShiXu2ImgFile(xxTmp.ToArray, yy.ToArray, pd.ToString("0.0000"), overThreshol, labelStrTmp)
                        Dim nImg2 As Novacode.Image = doc.AddImage(imgPathTmp)
                        Dim pic2 As Novacode.Picture = nImg2.CreatePicture
                        pic2.Width = 600
                        pic2.Height = 200
                        doc.InsertParagraph.AppendPicture(pic2)
                    Next
                End If
            End If
        Catch ex As Exception
            log("报告制作完成出错：" & ex.ToString)
        End Try
        doc.Save()
        doc.Dispose()
        log("报告制作完成")
        Return path
    End Function
    Private Function GetWatchPointIndex(itm As json_PPSJ, watchPoint As Double) As Integer
        Dim myXX() As Double = GetXXByFreqInfo(itm.freqStart, itm.freqStep, itm.dataCount)
        If IsNothing(myXX) Then Return 0
        Dim freqEnd As Double = myXX(myXX.Count - 1)
        If watchPoint < itm.freqStart Then Return 0
        If watchPoint > freqEnd Then Return 0
        For i = 0 To myXX.Length - 2
            If myXX(i) = watchPoint Then Return i
            If myXX(i) < watchPoint And myXX(i + 1) >= watchPoint Then
                Dim a As Double = Math.Abs(myXX(i) - watchPoint)
                Dim b As Double = Math.Abs(myXX(i + 1) - watchPoint)
                If a < b Then
                    Return i
                Else
                    Return i + 1
                End If
            End If
        Next
        If myXX(myXX.Count - 1) = watchPoint Then Return myXX.Count - 1
    End Function
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
        Dim timeFileName As String = NO & "Bscan"
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
    Private Function HandleHGBBuHuoTaskReport(ByVal Task As NormalTaskStu) As String '黑广播/违章捕获生成报告
        log("开始制作报告……")
        Dim menXian As Double = -95
        Dim sb As New StringBuilder
        sb.Append("select * from DeviceMsgTable ")
        sb.Append("where DeviceID='{0}' ")
        sb.Append("and Func='{1}' ")
        sb.Append("and MsgTime between '{2}' and '{3}' order by MsgTime")
        Dim para() As String = New String() {myDeviceId, "bscan", Task.StartTime, Task.EndTime}
        Dim dt As DataTable = SQLGetDT(String.Format(sb.ToString, para))
        CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceId)
        Dim path As String = Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceId & "\" & Task.TaskNickName & "_" &
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
                'Dim saveFreqStep As Integer = TssBscanOrder.saveFreqStep
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
                    For i = 0 To BscanList.Count - 1
                        Dim itm As json_PPSJ = BscanList(i)
                        itm = DSGDecompare(itm)
                        BscanList(i) = itm
                    Next
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
        Dim para() As String = New String() {myDeviceId, "bscan", Task.StartTime, Task.EndTime}
        Dim dt As DataTable = SQLGetDT(String.Format(sb.ToString, para))
        CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceId)
        Dim path As String = Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceId & "\" & Task.TaskNickName & "_" &
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
                    For i = 0 To BscanList.Count - 1
                        Dim itm As json_PPSJ = BscanList(i)
                        itm = DSGDecompare(itm)
                        BscanList(i) = itm
                    Next
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
            ser.Points.AddXY(Math.Round(xx(i), 3), yy(i))
        Next
        ser.Color = Color.Blue
        ser.IsVisibleInLegend = False
        chart.Series.Clear()
        chart.Series.Add(ser)
        Dim rand As New Random
        ' Dim int As Integer = rand.Next(100, 999)
        Dim NO As String = Guid.NewGuid.ToString("N")
        Dim timeFileName As String = NO & "Bscan"
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
        Dim data As List(Of Double)
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
        Dim para() As String = New String() {myDeviceId, "bscan", Task.StartTime, Task.EndTime}
        Dim dt As DataTable = SQLGetDT(String.Format(sb.ToString, para))
        CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceId)
        Dim path As String = Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceId & "\" & Task.TaskNickName & "_" &
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
                    For i = 0 To BscanList.Count - 1
                        Dim itm As json_PPSJ = BscanList(i)
                        itm = DSGDecompare(itm)
                        BscanList(i) = itm
                    Next
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
    Private Function DSGDecompare(kk As json_PPSJ) As json_PPSJ
        If kk.isDSGFreq Then
            kk.value = DSGBase2PPSJValues(kk.DSGFreqBase64)
        End If
        Return kk
    End Function
    Private Function HandleFreqMaxTaskReport(ByVal Task As NormalTaskStu) As String '频谱监测生成报告
        log("开始制作报告……")
        Dim menXian As Double = -95
        Dim sb As New StringBuilder
        sb.Append("select * from DeviceMsgTable ")
        sb.Append("where DeviceID='{0}' ")
        sb.Append("and Func='{1}' ")
        sb.Append("and MsgTime between '{2}' and '{3}' order by MsgTime")
        Dim para() As String = New String() {myDeviceId, "bscan", Task.StartTime, Task.EndTime}
        Dim dt As DataTable = SQLGetDT(String.Format(sb.ToString, para))
        CheckDir(Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceId)
        Dim path As String = Directory.GetCurrentDirectory() & "\Task\Reports\" & myDeviceId & "\" & Task.TaskNickName & "_" &
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
                Dim watchPoints As List(Of Double) = TssBscanOrder.watchPoint
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
                    For i = 0 To BscanList.Count - 1
                        Dim itm As json_PPSJ = BscanList(i)
                        itm = DSGDecompare(itm)
                        BscanList(i) = itm
                    Next
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
                    If IsNothing(watchPoints) = False Then
                        If watchPoints.Count <> 0 Then
                            'Dim sigNalList As New List(Of sigNalInfo)
                            'For Each wp In watchPoints

                            'Next
                            'For i = 0 To sigNalList.Count - 1
                            '    Dim s As sigNalInfo = sigNalList(i)
                            '    Dim pd As Double = s.freq
                            '    Dim yy() As Double = s.data.ToArray
                            '    Dim xxTmp As New List(Of String)
                            '    For j = 1 To yy.Length
                            '        xxTmp.Add(j)
                            '    Next
                            '    Dim overCount As Integer = s.OverCount
                            '    Dim sum As Integer = s.SumCount
                            '    Dim zy As Double = overCount / sum
                            '    Dim zyd As String = Format(zy * 100, "0.00") & " %"
                            '    Dim labelStrTmp As String = pd & " MHz," & "," & Task.StartTime & " 到 " & Task.EndTime & " 信号占用时序图 占用度 " & zyd
                            '    doc.InsertParagraph.AppendLine(" ● " & labelStr).Font(New FontFamily("宋体")).FontSize(12)
                            '    Dim imgPathTmp As String = ShiXu2ImgFile(xxTmp.ToArray, yy.ToArray, pd.ToString("0.0000"), overThreshol, labelStrTmp)
                            '    Dim nImg2 As Novacode.Image = doc.AddImage(imgPathTmp)
                            '    Dim pic2 As Novacode.Picture = nImg2.CreatePicture
                            '    pic2.Width = 600
                            '    pic2.Height = 200
                            '    doc.InsertParagraph.AppendPicture(pic2)
                            'Next
                        End If
                    End If
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

        Dim NO As String = Guid.NewGuid.ToString("N")
        Dim timeFileName As String = NO & "Bscan"
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
End Class
