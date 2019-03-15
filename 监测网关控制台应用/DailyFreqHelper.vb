Imports System
Imports System.IO
Imports System.Text
Imports System.Net
Imports System.Net.HttpListener
Imports System.Threading.Thread
Imports System.Threading
Imports Newtonsoft
Imports Newtonsoft.Json
Imports Novacode
Public Class DailyFreqHelper
    Public Shared dailyFreqStart As Double = 80 'MHz
    Public Shared dailyFreqEnd As Double = 1000  'MHz
    Public Shared dailyFreqStep As Double = 0.025 'MHz
    Private Shared workThread As Thread
    Private Shared sleepSecond As Integer = 10 '秒
    Private Shared dailyFreqLock As New Object

    Public Shared Sub StartWork()
        log("每日一谱工作器开启")
        workThread = New Thread(AddressOf WorkLoop)
        workThread.Start()
    End Sub
    Private Shared Sub WorkLoop()
        Dim sleepTime As Integer = sleepSecond * 1000
        While True
            Try
                Work()
            Catch ex As Exception

            End Try
            Sleep(sleepTime)
        End While
    End Sub
    Private Shared Sub Work()
        Dim time As Date = Now
        Dim hour As Integer = time.Hour
        Dim minute As Integer = time.Minute
        Dim Second As Integer = time.Second
        '工作时间为 19:00 07:00
        If (hour >= 19 And hour <= 24) Or (hour >= 0 And hour <= 7) Then
        Else
            Return
        End If
        '工作时间为小时的开始5分钟
        If minute > 5 Then Return
        SyncLock DeviceListLock
            For Each itm In DeviceList
                If itm.Kind.ToLower = "tss" Then
                    Dim cls As DeviceTSS = itm.cls
                    If IsNothing(cls) = False Then
                        If cls.isWorking = False Then
                            Dim th As New Thread(Sub()
                                                     ' log("dailyF work deviceName=" & cls.myDeviceInfo.Name)

                                                     cls.SendOrderFreqToDevice(dailyFreqStart, dailyFreqEnd, dailyFreqStep * 1000, 8, "2to1")
                                                 End Sub)
                            th.Start()
                        End If
                    End If
                End If
            Next
        End SyncLock
    End Sub
    Public Shared Sub OnDeviceFreq(ByVal freq As json_PPSJ, ByVal deviceInfo As DeviceStu)
        DoDeviceFreq(freq, deviceInfo)
        'Dim th As New Thread(Sub()

        '                     End Sub)
        'th.Start()
    End Sub
    Private Shared Sub DoDeviceFreq(ByVal freq As json_PPSJ, deviceInfo As DeviceStu)

        freq.isDSGFreq = True
        freq.DSGFreqBase64 = PPSJValues2DSGBase(freq.value)
        freq.value = Nothing
        '  SyncLock dailyFreqLock
        Try
            Dim nowDate As Date = Now
            Dim deviceId As String = deviceInfo.DeviceID
            Dim runLocation As RunLocation = freq.runLocation
            Dim lng As Double = deviceInfo.Lng
            Dim lat As Double = deviceInfo.Lat
            If IsNothing(runLocation) = False Then
                lng = runLocation.lng
                lat = runLocation.lat
            End If
            Dim dateTime As String = nowDate.ToString("yyyy-MM-dd HH:mm:ss")
            Dim day As String = nowDate.ToString("yyyy-MM-dd")
            Dim freqStart As Double = freq.freqStart
            Dim freqEnd As Double = freq.freqEnd
            Dim freqStep As Double = freq.freqStep
            'If deviceInfo.Name = "长安上沙站" Then
            '    log("step=3 freqStart=" & freq.freqStart)
            '    log("step=3 freqStep=" & freq.freqStep)
            '    log("freqStart=" & freqStart)
            '    log("freqEnd=" & freqEnd)
            '    log("freqStep=" & freqStep)

            'End If

            If freqStart <> dailyFreqStart Or freqEnd <> dailyFreqEnd Or freqStep.ToString <> dailyFreqStep.ToString Then
                Return
            End If

            Dim sql As String = "select * from dailyFreqTable where day='{0}' and deviceId='{1}'"
            sql = String.Format(sql, New String() {day, deviceId})
            Dim dt As DataTable = SQLGetDT(sql)
            Dim isAdd As Boolean = True
            If IsNothing(dt) = False Then
                If dt.Rows.Count > 0 Then
                    isAdd = False
                    Dim row As DataRow = dt.Rows(0)
                    Dim id As String = row("ID").ToString
                    Dim FreqCount As String = row("FreqCount").ToString
                    Dim dbMaxFreq As String = row("MaxFreq").ToString
                    Dim dbMinFreq As String = row("MinFreq").ToString
                    Dim count As Integer = 0
                    If IsNothing(FreqCount) = False Then
                        If IsDBNull(FreqCount) = False Then
                            count = Val(FreqCount) + 1
                        End If
                    End If
                    sql = "update dailyFreqTable set DateTime='{0}',MaxFreq='{1}',MinFreq='{2}',LastestFreq='{3}',FreqCount='{4}'  where id=" & id
                    Dim maxFreq, minFreq As json_PPSJ
                    If IsNothing(dbMaxFreq) = False Then
                        If IsDBNull(dbMaxFreq) = False Then
                            If dbMaxFreq <> "" Then
                                maxFreq = JsonConvert.DeserializeObject(Of json_PPSJ)(dbMaxFreq)
                            End If
                        End If
                    End If
                    If IsNothing(dbMinFreq) = False Then
                        If IsDBNull(dbMinFreq) = False Then
                            If dbMinFreq <> "" Then
                                minFreq = JsonConvert.DeserializeObject(Of json_PPSJ)(dbMinFreq)
                            End If
                        End If
                    End If
                    maxFreq = GetMaxFreq(freq, maxFreq)
                    minFreq = GetMinFreq(freq, minFreq)
                    Dim strMax As String = JsonConvert.SerializeObject(maxFreq)
                    Dim strMin As String = JsonConvert.SerializeObject(minFreq)
                    Dim strTimely As String = JsonConvert.SerializeObject(freq)
                    sql = String.Format(sql, New String() {dateTime, strMax, strMin, strTimely, count})
                    SQLCmd(sql)
                End If
            End If
            If isAdd Then
                sql = "insert into dailyFreqTable(DateTime,Day,DeviceId,Lng,Lat,
                                                    Province,City,District,FreqStart,FreqEnd,FreqStep,MaxFreq,MinFreq,LastestFreq,FreqCount) values
                                                    ({0})"
                Dim list As New List(Of String)
                list.Add(dateTime)
                list.Add(day)
                list.Add(deviceId)
                list.Add(lng)
                list.Add(lat)
                list.Add("")
                list.Add("")
                list.Add("")
                list.Add(freqStart)
                list.Add(freqEnd)
                list.Add(freqStep.ToString)
                Dim strTmp As String = JsonConvert.SerializeObject(freq)
                list.Add(strTmp)
                list.Add(strTmp)
                list.Add(strTmp)
                list.Add("1")
                Dim sb As New StringBuilder
                For Each itm In list
                    sb.Append("'" & itm & "',")
                Next
                Dim tmpStr As String = sb.ToString()
                sql = String.Format(sql, tmpStr.Substring(0, tmpStr.Length - 1))
                ' File.WriteAllText("d:\jdhhuq.txt", sql)
                Dim result As String = SQLCmd(sql)
                '  log(result)
            End If
        Catch ex As Exception
            log(ex.ToString)
        End Try
        ' End SyncLock
    End Sub
    Private Shared Function GetMaxFreq(newFreq As json_PPSJ, maxFreq As json_PPSJ) As json_PPSJ
        If IsNothing(maxFreq) Then Return newFreq
        Dim nFreq As New json_PPSJ
        nFreq.deviceID = newFreq.deviceID
        nFreq.freqStart = newFreq.freqStart
        nFreq.freqStep = newFreq.freqStep
        nFreq.freqEnd = newFreq.freqEnd
        nFreq.dataCount = newFreq.dataCount
        nFreq.runLocation = newFreq.runLocation
        nFreq.value = newFreq.value
        nFreq.isDSGFreq = newFreq.isDSGFreq
        nFreq.DSGFreqBase64 = newFreq.DSGFreqBase64
        If (nFreq.isDSGFreq) Then
            nFreq.value = DSGBase2PPSJValues(nFreq.DSGFreqBase64)
        End If
        If (maxFreq.isDSGFreq) Then
            maxFreq.value = DSGBase2PPSJValues(maxFreq.DSGFreqBase64)
        End If
        If IsNothing(nFreq.value) Then Return maxFreq
        If IsNothing(maxFreq.value) Then Return nFreq
        If nFreq.value.Count <> maxFreq.value.Count Then Return maxFreq
        For i = 0 To nFreq.value.Count - 1
            If nFreq.value(i) > maxFreq.value(i) Then
                maxFreq.value(i) = nFreq.value(i)
            End If
        Next
        If maxFreq.isDSGFreq Then
            maxFreq.DSGFreqBase64 = PPSJValues2DSGBase(maxFreq.value)
            maxFreq.value = Nothing
        End If
        Return maxFreq
    End Function
    Private Shared Function GetMinFreq(newFreq As json_PPSJ, MinFreq As json_PPSJ) As json_PPSJ
        If IsNothing(MinFreq) Then Return newFreq
        Dim nFreq As New json_PPSJ
        nFreq.deviceID = newFreq.deviceID
        nFreq.freqStart = newFreq.freqStart
        nFreq.freqStep = newFreq.freqStep
        nFreq.freqEnd = newFreq.freqEnd
        nFreq.dataCount = newFreq.dataCount
        nFreq.runLocation = newFreq.runLocation
        nFreq.value = newFreq.value
        nFreq.isDSGFreq = newFreq.isDSGFreq
        nFreq.DSGFreqBase64 = newFreq.DSGFreqBase64
        If (nFreq.isDSGFreq) Then
            nFreq.value = DSGBase2PPSJValues(nFreq.DSGFreqBase64)
        End If
        If (MinFreq.isDSGFreq) Then
            MinFreq.value = DSGBase2PPSJValues(MinFreq.DSGFreqBase64)
        End If
        If IsNothing(nFreq.value) Then Return MinFreq
        If IsNothing(MinFreq.value) Then Return nFreq
        If nFreq.value.Count <> MinFreq.value.Count Then Return MinFreq
        For i = 0 To nFreq.value.Count - 1
            If nFreq.value(i) < MinFreq.value(i) Then
                MinFreq.value(i) = nFreq.value(i)
            End If
        Next
        If MinFreq.isDSGFreq Then
            MinFreq.DSGFreqBase64 = PPSJValues2DSGBase(MinFreq.value)
            MinFreq.value = Nothing
        End If
        Return MinFreq

    End Function
End Class
