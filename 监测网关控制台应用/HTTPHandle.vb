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
Imports 监测网关控制台应用.MainHttpListener
Imports OfficeOpenXml
Public Class HTTPHandle
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
    End Structure
    Public Function Handle_GetHisFreqGisTimeList(context As HttpListenerContext) As NormalResponse
        Dim lineId As String = context.Request.QueryString("lineId")
        Dim deviceId As String = context.Request.QueryString("deviceId")
        Dim msgId As String = context.Request.QueryString("msgId")
        Dim startTime As String = context.Request.QueryString("startTime")
        Dim endTime As String = context.Request.QueryString("endTime")
        Dim lng As String = context.Request.QueryString("lng")
        Dim lat As String = context.Request.QueryString("lat")
        If lineId = "" And deviceId = "" Then
            Dim np As New NormalResponse(False, "参数不完整")
            Return np
        End If
        If startTime = "" Or endTime = "" Or lng = "" Or lat = "" Then
            Dim np As New NormalResponse(False, "参数不完整")
            Return np
        End If
        Dim startDate As Date
        Dim endDate As Date
        Try
            startDate = Date.Parse(startTime)
            startTime = startDate.ToString("yyyy-MM-dd HH:mm:ss")
        Catch ex As Exception
            Dim np As New NormalResponse(False, "起始时间非法格式")
            Return np
        End Try
        Try
            endDate = Date.Parse(endTime)
            endTime = endDate.ToString("yyyy-MM-dd HH:mm:ss")
        Catch ex As Exception
            Dim np As New NormalResponse(False, "结束时间非法格式")
            Return np
        End Try
        If IsNumeric(lng) = False Or IsNumeric(lat) = False Then
            Dim np As New NormalResponse(False, "经纬度非法格式")
            Return np
        End If
        Dim sql As String
        Dim grid As String
        Dim isUseGrid As Boolean = False
        If msgId = "" Then
            sql = "select grid from freqGisTable where msgid=" & msgId
            grid = SQLInfo(sql)
            If IsNothing(grid) = False Then
                If IsNumeric(grid) Then
                    If grid > 0 Then isUseGrid = True
                End If
            End If
        End If
        If Not isUseGrid Then
            sql = "select grid from freqGisTable where lineId={0} and lng={1} and lat={2} and time>='{3}' and time<='{4}'"
            sql = String.Format(sql, New String() {lineId, lng, lat, startTime, endTime})
            Dim tmpDt As DataTable = SQLGetDT(sql)
            If IsNothing(tmpDt) = False Then
                If tmpDt.Rows.Count > 0 Then
                    For Each row In tmpDt.Rows
                        Dim tmpGrid As String = row(0)
                        If IsNothing(tmpGrid) = False Then
                            If IsNumeric(tmpGrid) Then
                                If tmpGrid > 0 Then
                                    isUseGrid = True
                                    grid = tmpGrid
                                    Exit For
                                End If
                            End If
                        End If
                    Next
                End If
            End If
        End If

        If isUseGrid Then
            sql = "select msgid,time,freqJsonLen,grid from freqGisTable where lineId={0} and grid={1} and time>='{2}' and time<='{3}'"
            sql = String.Format(sql, New String() {lineId, grid, startTime, endTime})
        Else
            sql = "select msgid,time,freqJsonLen,grid from freqGisTable where lineId={0} and lng={1} and lat={2} and time>='{3}' and time<='{4}'"
            sql = String.Format(sql, New String() {lineId, lng, lat, startTime, endTime})
        End If

        Dim dt As DataTable = SQLGetDT(sql)
        If IsNothing(dt) Then
            Dim np As New NormalResponse(False, "本线路该时间区间该经纬度上没有数据")
            Return np
        End If
        If dt.Rows.Count = 0 Then
            Dim np As New NormalResponse(False, "本线路该时间区间该经纬度上没有数据")
            Return np
        End If
        Dim np2 As New NormalResponse(True, "", "", JsonConvert.SerializeObject(dt))
        Return np2
    End Function
    Public Function Handle_GetHisFreqGis(context As HttpListenerContext) As NormalResponse
        Dim msgid As String = context.Request.QueryString("msgid")
        Dim freqStart As String = context.Request.QueryString("freqStart")
        Dim freqEnd As String = context.Request.QueryString("freqEnd")
        If msgid = "" Or freqStart = "" Or freqEnd = "" Then
            Dim np As New NormalResponse(False, "参数不完整")

            Return np
        End If
        If IsNumeric(msgid) = False Or IsNumeric(freqStart) = False Or IsNumeric(freqEnd) = False Then
            Dim np As New NormalResponse(False, "参数非数字")

            Return np
        End If
        Dim sql As String = "select freqJson from freqGisTable where msgid='" & msgid & "'"
        Dim dt As DataTable = SQLGetDT(sql)
        If IsNothing(dt) Then
            Dim np As New NormalResponse(False, "没有数据")

            Return np
        End If
        If dt.Rows.Count = 0 Then
            Dim np As New NormalResponse(False, "没有数据")

            Return np
        End If
        Dim freqJson As String = dt.Rows(0)("freqJson")
        Dim p As json_PPSJ = JsonConvert.DeserializeObject(freqJson, GetType(json_PPSJ))
        If IsNothing(p) Then
            Dim np As New NormalResponse(False, "数据异常")

            Return np
        End If
        Dim dFreqStart As Double = Val(freqStart)
        Dim dFreqEnd As Double = Val(freqEnd)
        If dFreqEnd <= dFreqStart Then
            Dim np As New NormalResponse(False, "'结束频率' 必须大于 '起始频率'")

            Return np
        End If
        Dim yy() As Double = p.value
        If p.isDSGFreq Then
            yy = DSGBase2PPSJValues(p.DSGFreqBase64)
        End If
        If IsNothing(yy) Then
            Dim np As New NormalResponse(False, "数据异常")

            Return np
        End If
        Dim DtfreqStart As Double = p.freqStart
        Dim DtfreqStep As Double = p.freqStep
        Dim DtFreqEnd As Double = DtfreqStart + (p.dataCount - 1) * DtfreqStep
        Dim min As Double = dFreqStart
        Dim max As Double = dFreqEnd
        If min < DtfreqStart Then
            min = DtfreqStart
        End If
        If max > DtFreqEnd Then
            max = DtFreqEnd
        End If
        Dim newYY((max - min) / DtfreqStep) As Double
        Dim startIndex As Integer = Math.Floor((min - DtfreqStart) / DtfreqStep)
        Dim endIndex As Integer = Math.Floor((max - DtfreqStart) / DtfreqStep)
        While (endIndex - startIndex + 1) > newYY.Count
            endIndex = endIndex - 1
        End While
        For i = 0 To newYY.Count - 1
            Dim value As Double = yy(startIndex + i)
            Dim xx As Double = DtfreqStart + (startIndex + i) * DtfreqStep
            newYY(i) = value
        Next
        Dim realFreqStart As Double = DtfreqStart + startIndex * DtfreqStep
        Dim realFreqStep As Double = DtfreqStep
        Dim realCount As Long = newYY.Count
        Dim realFreqEnd As Double = realFreqStart + (realCount - 1) * realFreqStep
        Dim realValue() As Double = newYY
        Dim jsonPP As json_PPSJ
        jsonPP.freqStart = realFreqStart
        jsonPP.freqStep = realFreqStep
        jsonPP.freqEnd = realFreqEnd
        jsonPP.dataCount = realCount
        jsonPP.deviceID = ""
        jsonPP.value = realValue
        jsonPP.runLocation = Nothing
        If UseDSGFreq Then
            jsonPP.isDSGFreq = True
            jsonPP.DSGFreqBase64 = PPSJValues2DSGBase(jsonPP.value)
            jsonPP.value = Nothing
        End If
        Dim bfgp As BusFreqGisPPSJJsonInfo
        bfgp.freqStart = dFreqStart
        bfgp.freqEnd = dFreqEnd
        bfgp.freqStep = realFreqStep
        bfgp.realPPSJ = jsonPP
        Dim json As String = JsonConvert.SerializeObject(bfgp)
        Dim np2 As New NormalResponse(True, "", "", json)
        Return np2
    End Function
    Structure BusFreqGisPPSJJsonInfo
        Dim freqStart As Double
        Dim freqEnd As Double
        Dim freqStep As Double
        Dim realPPSJ As json_PPSJ
    End Structure
    Structure TaskPanInfo
        Dim unOverCount As Long
        Dim overCount As Long
        Dim onlineDeivceCount As Long
    End Structure
    Public Function Handle_GetTaskPanInfo(context As HttpListenerContext) As NormalResponse
        Dim token As String = context.Request.QueryString("token")
        Dim sql As String = "select count(1) from userTaskTable where OverPercent='100%'"
        Dim overCount As Long = SQLGetCount(sql)
        sql = "select count(1) from userTaskTable"
        Dim sumCount As Long = SQLGetCount(sql)
        Dim unOverCount As Long = sumCount - overCount
        If unOverCount < 0 Then unOverCount = 0
        Dim taskPanInfo As TaskPanInfo
        taskPanInfo.unOverCount = unOverCount
        taskPanInfo.overCount = overCount
        taskPanInfo.onlineDeivceCount = 0
        If IsNothing(DeviceListLock) Then DeviceListLock = New Object
        Dim onlineDeivceCount As Long = 0
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
                        onlineDeivceCount = d.Count
                    Else
                        Dim list As New List(Of String)
                        list = myDevicestring.Split(",").ToList()
                        If IsNothing(list) Then
                            onlineDeivceCount = 0
                        Else
                            For j = d.Count - 1 To 0 Step -1
                                Dim itm As deviceStuWithOutHttpListener = d(j)
                                If list.Contains(itm.DeviceID) = False Then
                                    d.RemoveAt(j)
                                End If
                            Next
                            onlineDeivceCount = d.Count
                        End If
                    End If
                End If
            End If
        End SyncLock
        taskPanInfo.onlineDeivceCount = onlineDeivceCount
        Dim np2 As New NormalResponse(True, "", "", taskPanInfo)
        Return np2
    End Function
    Structure APPUpdateRegionInfo
        Dim Index As Integer
        Dim Name As String
        Dim Version As String
    End Structure
    Public Function Handle_GetRegion(usr As String, msg As String) As NormalResponse
        Dim np As NormalResponse
        Try
            Dim usrid As String = usr
            If usrid = "" Then
                np = New NormalResponse(False, "usrid不可为空")
                Return np
            End If
            Dim sql As String = "select * from appUpdateUsrTable where usr='" & usrid & "'"
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) Then
                np = New NormalResponse(False, "usrid不存在")
                Return np
            End If
            If dt.Rows.Count = 0 Then
                np = New NormalResponse(False, "usrid不存在")
                Return np
            End If
            Dim rootPath As String = dt.Rows(0)("rootPath")
            If rootPath = "" Then
                np = New NormalResponse(False, "rootPath为空")
                Return np
            End If
            If Directory.Exists(rootPath) = False Then
                np = New NormalResponse(False, "rootPath不存在")
                Return np
            End If
            Dim d As New DirectoryInfo(rootPath)
            Dim list As New List(Of APPUpdateRegionInfo)
            For Each itm As DirectoryInfo In d.GetDirectories
                Dim filePath As String = itm.FullName & "\version.txt"
                If File.Exists(filePath) Then
                    Dim apinfo As New APPUpdateRegionInfo
                    apinfo.Name = itm.Name
                    apinfo.Index = list.Count + 1
                    apinfo.Version = File.ReadAllText(filePath).Trim
                    list.Add(apinfo)
                End If
            Next
            np = New NormalResponse(True, "", "", list)
            Return np
        Catch ex As Exception
            np = New NormalResponse(False, ex.Message)
            Return np
        End Try
        np = New NormalResponse(False, "未经处理的错误,errcode=503")
        Return np
    End Function
    Public Function Handle_GetAPPVersion(usr As String, msg As String) As NormalResponse
        Dim np As NormalResponse
        Try
            Dim dirPath As String = msg
            If dirPath = "" Then
                np = New NormalResponse(False, "dirPath不能为空")
                Return np
            End If
            Dim sql As String = "select * from appUpdateUsrTable where usr='" & usr & "'"
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) Then
                np = New NormalResponse(False, "usrid不存在")
                Return np
            End If
            If dt.Rows.Count = 0 Then
                np = New NormalResponse(False, "usrid不存在")
                Return np
            End If
            Dim rootPath As String = dt.Rows(0)("rootPath")
            Dim filePath As String = rootPath & "\" & dirPath & "\version.txt"
            If File.Exists(filePath) = False Then
                np = New NormalResponse(False, "服务器找不到该项目的版本文件")
                Return np
            End If
            Dim str As String = File.ReadAllText(filePath, Encoding.Default)
            np = New NormalResponse(True, "", "", str)
            Return np
        Catch ex As Exception
            np = New NormalResponse(False, ex.Message)
            Return np
        End Try
        np = New NormalResponse(False, "未经处理的错误,errcode=503")
        Return np
    End Function
    Public Function Handle_GetCanUpdate(usr As String, msg As String) As NormalResponse
        Dim np As NormalResponse
        Try
            Dim obj As JObject = JObject.Parse(msg)
            Dim dirPath As String = obj("dirPath")
            Dim myVersion As String = obj("myVersion")
            If dirPath = "" Then
                np = New NormalResponse(False, "dirPath不能为空")
                Return np
            End If
            Dim sql As String = "select * from appUpdateUsrTable where usr='" & usr & "'"
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) Then
                np = New NormalResponse(False, "usrid不存在")
                Return np
            End If
            If dt.Rows.Count = 0 Then
                np = New NormalResponse(False, "usrid不存在")
                Return np
            End If
            Dim rootPath As String = dt.Rows(0)("rootPath")
            Dim filePath As String = rootPath & "\" & dirPath & "\version.txt"
            If File.Exists(filePath) = False Then
                np = New NormalResponse(False, "服务器找不到该项目的版本文件")
                Return np
            End If
            Dim str As String = File.ReadAllText(filePath, Encoding.Default)
            Dim serverVersion As String = str
            Try
                Dim oldVersion As Version = Version.Parse(myVersion)
                Dim newVersion As Version = Version.Parse(serverVersion)
                Dim flag As Boolean = CanUpdate(myVersion, serverVersion)
                Dim nb As New JObject
                nb.Add("canUpdate", flag)
                nb.Add("serverVersion", serverVersion)
                nb.Add("url", "http://123.207.31.37:8082/" & rootPath & "/" & dirPath & "/")
                np = New NormalResponse(True, "", "", nb)
                Return np
            Catch ex As Exception
                np = New NormalResponse(False, "version版本格式非法，无法比较")
                Return np
            End Try
        Catch ex As Exception
            np = New NormalResponse(False, ex.Message)
            Return np
        End Try
        np = New NormalResponse(False, "未经处理的错误,errcode=503")
        Return np
    End Function

    Public Function Handle_UploadAPP(usr As String, msg As String) As NormalResponse
        Dim np As NormalResponse
        Try
            Dim obj As JObject = JObject.Parse(msg)
            Dim dirPath As String = obj("dirPath")
            Dim newVersion As String = obj("newVersion")
            Dim base64 As String = obj("base64")
            Dim type As Integer = obj("type")
            Dim fileName As String = obj("fileName")
            If dirPath = "" Then
                np = New NormalResponse(False, "dirPath不能为空")
                Return np
            End If
            If newVersion = "" Then
                np = New NormalResponse(False, "newVersion不能为空")
                Return np
            End If
            Dim sql As String = "select * from appUpdateUsrTable where usr='" & usr & "'"
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) Then
                np = New NormalResponse(False, "usrid不存在")
                Return np
            End If
            If dt.Rows.Count = 0 Then
                np = New NormalResponse(False, "usrid不存在")
                Return np
            End If
            Dim rootPath As String = dt.Rows(0)("rootPath")
            Dim filePath As String = rootPath & "\" & dirPath & "\version.txt"
            If File.Exists(filePath) = False Then
                np = New NormalResponse(False, "服务器找不到该项目的版本文件")
                Return np
            End If
            Try
                Try
                    Dim v As Version = Version.Parse(newVersion)
                Catch ex As Exception
                    np = New NormalResponse(False, "newVersion格式非法")
                    Return np
                End Try
                Dim sw As New StreamWriter(filePath, False, Encoding.Default)
                sw.Write(newVersion)
                sw.Close()
                If type = 2 Then
                    np = New NormalResponse(True, "设置版本号成功")
                    Return np
                End If
            Catch ex As Exception
                np = New NormalResponse(False, ex.Message)
                Return np
            End Try
            If type = 1 Then
                If base64 = "" Then
                    np = New NormalResponse(True, "base64不可为空")
                    Return np
                End If
                Try
                    Dim by() As Byte = Convert.FromBase64String(base64)
                    If IsNothing(by) Then
                        np = New NormalResponse(True, "base64格式非法，解码字节为0")
                        Return np
                    End If
                    Dim appPath As String = rootPath & "\" & dirPath & "\" & fileName
                    If File.Exists(appPath) Then File.Delete(appPath)
                    File.WriteAllBytes(appPath, by)
                    np = New NormalResponse(True, "上传成功")
                    Return np
                Catch ex As Exception
                    np = New NormalResponse(True, ex.Message)
                    Return np
                End Try
            End If
        Catch ex As Exception
            np = New NormalResponse(False, ex.Message)
            Return np
        End Try
        np = New NormalResponse(False, "未经处理的错误,errcode=503")
        Return np
    End Function
    Public Function Handle_GetDeviceOnlineDetailExcel(context As HttpListenerContext) As NormalResponse
        Dim day As String = context.Request.QueryString("day")
        If IsNothing(day) Then
            Return New NormalResponse(False, "日期不能为空")
        End If
        If day = "" Then
            Return New NormalResponse(False, "日期不能为空")
        End If
        Dim dTmp As Date
        Try
            dTmp = Date.Parse(day)
            day = dTmp.ToString("yyyy-MM-dd 00:00:00")
            Dim nday As String = Now.ToString("yyyy-MM-dd 00:00:00")
            If day <> nday Then
                ''这里应该查历史记录
                Return New NormalResponse(False, "暂没有数据")
            End If
        Catch ex As Exception
            Return New NormalResponse(False, "日期非法")
        End Try
        Dim sql As String = "select * from deviceTable order by onlineTime desc"
        Dim dt As DataTable = SQLGetDT(sql)
        If IsNothing(dt) Then
            Return New NormalResponse(False, "没有查询到结果")
        End If
        If dt.Rows.Count = 0 Then
            Return New NormalResponse(False, "没有查询到结果")
        End If
        Dim path As String = SaveDeviceDetailInfoDt(dt, dTmp)
        If File.Exists(path) Then
            Return New NormalResponse(True, "", "", myServerInfo.IISFileUrl & path)
        Else
            Return New NormalResponse(False, "excel文件生成失败"）
        End If
    End Function
    Private Function SaveDeviceDetailInfoDt(dt As DataTable, day As Date) As String
        Dim nd As New DataTable
        nd.Columns.Add("序号")
        nd.Columns.Add("设备ID")
        nd.Columns.Add("设备名称")
        nd.Columns.Add("设备类型")
        nd.Columns.Add("最新上线时间")
        nd.Columns.Add("设备经度")
        nd.Columns.Add("设备纬度")
        nd.Columns.Add("设备IP位置")
        nd.Columns.Add("设备IP")
        nd.Columns.Add("设备端口")
        nd.Columns.Add("运行类型")
        nd.Columns.Add("省")
        nd.Columns.Add("市")
        nd.Columns.Add("区")
        nd.Columns.Add("位置详情")
        nd.Columns.Add("今日是否上线")
        For Each row As DataRow In dt.Rows
            Dim nr As DataRow = nd.NewRow
            nr("序号") = nd.Rows.Count + 1
            For i = 0 To dt.Columns.Count - 1
                nr(i + 1) = row(i)
            Next
            Dim onlineTime As String = row("OnlineTime").ToString
            Dim dtmp As Date = "2018-01-01 00:00:00"
            If onlineTime <> "" Then
                Date.TryParse(onlineTime, dtmp)
            End If
            If dtmp >= day Then
                nr("今日是否上线") = "是"
            Else
                nr("今日是否上线") = "否"
            End If
            nd.Rows.Add(nr)
        Next
        Dim dirPath As String = "DeviceDetailExcel"
        If Directory.Exists(dirPath) = False Then Directory.CreateDirectory(dirPath)
        dirPath = dirPath & "/"
        Dim fileName As String = dirPath & day.ToString("yyyy_MM_dd") & "设备在线一览表.xlsx"
        If File.Exists(fileName) Then File.Delete(fileName)
        Dim excel As ExcelPackage = Dt2Excel(nd)
        Dim exsheet As ExcelWorksheet = excel.Workbook.Worksheets(1)
        For i = 2 To exsheet.Dimension.End.Row
            exsheet.Cells(i, 1).Value = i - 1
            ' exsheet.Cells(i, 1).Style.Numberformat.Format = "#,##0"
            Dim isNowOnline As String = exsheet.Cells(i, 16).Text
            Dim flag As Boolean = False
            If isNowOnline = "是" Then flag = True
            If Not flag Then
                exsheet.Row(i).Style.Font.Color.SetColor(System.Drawing.Color.Gray)
            End If
        Next

        excel.SaveAs(New FileInfo(fileName))
        Return fileName
    End Function
    Private Function Dt2Excel(dt As DataTable) As ExcelPackage
        Dim excel As New ExcelPackage()
        Dim exsheet As ExcelWorksheet = excel.Workbook.Worksheets.Add("sheet1")
        For i = 0 To dt.Columns.Count - 1
            exsheet.Cells(1, i + 1).Value = dt.Columns(i).ColumnName
        Next
        For i = 0 To dt.Rows.Count - 1
            For j = 0 To dt.Columns.Count - 1
                Dim str As String = dt.Rows(i)(j).ToString
                exsheet.Cells(i + 2, j + 1).Value = str
            Next
        Next
        Return excel
    End Function
    Public Function Handle_GetCarDeviceList(context As HttpListenerContext) As NormalResponse
        Try
            Dim sql As String = "select * from deviceTable where runKind='car'"
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) Then Return New NormalResponse(False, "没有任何数据")
            If dt.Rows.Count = 0 Then Return New NormalResponse(False, "没有任何数据")
            Return New NormalResponse(True, "", "", dt)
        Catch ex As Exception
            Return New NormalResponse(False, ex.ToString)
        End Try
    End Function
    Public Function Handle_GetBusHisFreqList(context As HttpListenerContext) As NormalResponse
        Dim startTime As String = context.Request.QueryString("startTime")
        Dim endTime As String = context.Request.QueryString("endTime")
        Dim lineId As String = context.Request.QueryString("lineId")
        Dim deviceId As String = context.Request.QueryString("deviceId")
        Dim countstr As String = context.Request.QueryString("count")
        Try
            Dim dStartTime As Date = Date.Parse(startTime)
            startTime = dStartTime.ToString("yyyy-MM-dd HH:mm:ss")
        Catch ex As Exception
            Return New NormalResponse(False, "起始时间格式不正确")
        End Try
        Try
            Dim dEndTime As Date = Date.Parse(endTime)
            endTime = dEndTime.ToString("yyyy-MM-dd HH:mm:ss")
        Catch ex As Exception
            Return New NormalResponse(False, "结束时间格式不正确")
        End Try
        Dim count As Long = 0
        If IsNothing(countstr) = False AndAlso countstr <> "" AndAlso IsNumeric(countstr) Then
            count = Val(countstr)
        End If
        Try
            If IsNothing(lineId) And IsNothing(deviceId) Then Return New NormalResponse(False, "lineId和deviceId不可都为空")
            If lineId = "" And deviceId = "" Then Return New NormalResponse(False, "lineId和deviceId不可都为空")
            Dim sql As String = "select * from BusLineTable where lineid='" & lineId & "'"
            'If deviceId <> "" Then sql = "select * from BusLineTable where deviceId='" & deviceId & "'"
            'If SQLIsIn(sql) = False Then
            '    Return New NormalResponse(False, "您选择的线路不存在")
            'End If
            sql = "select msgid,time,freqStart,freqEnd,freqStep,pointCount,lng,lat,freqJsonLen from freqGisTable where lineId=" & lineId & " and time>='" & startTime & "' and time<='" & endTime & "' order by time asc"
            If deviceId <> "" Then
                sql = "select msgid,time,freqStart,freqEnd,freqStep,pointCount,lng,lat,freqJsonLen from freqGisTable where deviceId='" & deviceId & "' and time>='" & startTime & "' and time<='" & endTime & "'  order by time asc"
            End If
            If count > 0 Then
                sql = sql & " limit " & count
            End If
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) Then Return New NormalResponse(False, "该时段该线路无任何数据", sql, "")
            If dt.Rows.Count = 0 Then Return New NormalResponse(False, "该时段该线路无任何数据", sql, "")
            Return New NormalResponse(True, "", "", dt)
        Catch ex As Exception
            Return New NormalResponse(False, ex.Message)
        End Try

    End Function
    Public Function Handle_GetBusHisFreqData(context As HttpListenerContext) As NormalResponse
        Dim msgid As String = context.Request.QueryString("msgid")
        If IsNothing(msgid) Then Return New NormalResponse(False, "msgid不可为空")
        If msgid = "" Then Return New NormalResponse(False, "msgid不可为空")
        If IsNumeric(msgid) = False Then Return New NormalResponse(False, "msgid格式非法")
        Dim sql As String = "select freqJson from freqGisTable where msgid=" & msgid
        Dim dt As DataTable = SQLGetDT(sql)
        If IsNothing(dt) Then Return New NormalResponse(False, "该msgid不存在")
        If dt.Rows.Count = 0 Then Return New NormalResponse(False, "该msgid不存在")
        Dim str As String = dt.Rows(0)(0)
        str = CompressPPSJ(str)
        Return New NormalResponse(True, "", "", str)
    End Function
    Private Function CompressPPSJ(str As String) As String
        Try
            Dim jsonPP As json_PPSJ = JsonConvert.DeserializeObject(str, GetType(json_PPSJ))
            Dim freqStart As Double = jsonPP.freqStart
            Dim freqStep As Double = jsonPP.freqStep
            Dim yy() As Double = jsonPP.value
            If jsonPP.isDSGFreq Then
                yy = DSGBase2PPSJValues(jsonPP.DSGFreqBase64)
            End If
            If IsNothing(yy) Then
                Return str
            End If
            Dim dataCount As Integer = yy.Count
            Dim maxCount As Integer = 5000
            If dataCount > maxCount Then
                Dim realValue() As Double = yy
                Dim xlist As New List(Of Double)
                Dim ylist As New List(Of Double)
                Dim st As Integer = Math.Ceiling(dataCount / maxCount)
                jsonPP.freqStep = freqStep * st
                For i = 0 To dataCount - 1 Step st
                    xlist.Add(freqStart + i * freqStep)
                    ylist.Add(realValue(i))
                Next
                If xlist(xlist.Count - 1) <> freqStart + (dataCount - 1) * freqStep Then
                    xlist.Add(freqStart + (dataCount - 1) * freqStep)
                    ylist.Add(yy(yy.Length - 1))
                End If
                yy = ylist.ToArray
                jsonPP.dataCount = yy.Count
                jsonPP.value = yy
                If UseDSGFreq Then
                    jsonPP.isDSGFreq = True
                    jsonPP.DSGFreqBase64 = PPSJValues2DSGBase(jsonPP.value)
                    jsonPP.value = Nothing
                End If
            End If
            Dim tmpMsg As String = JsonConvert.SerializeObject(jsonPP)
            Return tmpMsg
        Catch ex As Exception

        End Try
        Return str
    End Function
    Public Function Handle_GetGateWayLocation(context As HttpListenerContext) As NormalResponse
        Try
            Dim deviceId As String = context.Request.QueryString("deviceId")
            Dim result As String = GetGateWayLocation(deviceId)
            If result = "" Then Return New NormalResponse(False, "")
            If result.Contains(",") = False Then Return New NormalResponse(False, "")
            Dim st() As String = result.Split(",")
            If st.Length <> 2 Then Return New NormalResponse(False, "")
            Dim lng As String = st(0)
            Dim lat As String = st(1)
            If IsNumeric(lng) = False Then Return New NormalResponse(False, "")
            If IsNumeric(lat) = False Then Return New NormalResponse(False, "")
            If lng = 0 Or lat = 0 Then Return New NormalResponse(False, "")
            Return New NormalResponse(True, "", "", lng & "," & lat)
        Catch ex As Exception
            Return New NormalResponse(False, ex.Message)
        End Try
    End Function
    Public Function Handle_GetDisOnlineDevice(context As HttpListenerContext) As NormalResponse
        Try
            Dim onlineDeviceList As New List(Of String)
            If IsNothing(DeviceListLock) Then DeviceListLock = New Object
            SyncLock DeviceListLock
                If IsNothing(DeviceList) = False Then
                    Dim d As List(Of deviceStuWithOutHttpListener) = TransForDeviceStu(DeviceList)
                    For j = d.Count - 1 To 0 Step -1
                        Dim itm As deviceStuWithOutHttpListener = d(j)
                        onlineDeviceList.Add(itm.Name)
                    Next
                End If
            End SyncLock
            Dim sql As String = "select DeviceID,DeviceNickName,Kind  from deviceTable where kind='TSS'"
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) Then Return New NormalResponse(False, "数据表不存在")
            If dt.Rows.Count = 0 Then Return New NormalResponse(False, "数据表不存在")
            Dim disOnlineList As New List(Of String)
            Dim allDeviceNameList As New List(Of String)
            For Each row In dt.Rows
                allDeviceNameList.Add(row("DeviceNickName"))
            Next
            For Each itm In allDeviceNameList
                If onlineDeviceList.Contains(itm) = False Then
                    disOnlineList.Add(itm)
                End If
            Next
            If IsNothing(disOnlineList) Then Return New NormalResponse(False, "没有离线设备")
            If disOnlineList.Count = 0 Then Return New NormalResponse(False, "没有离线设备")
            Return New NormalResponse(True, "", "", disOnlineList)
            Return New NormalResponse(True, "", JsonConvert.SerializeObject(onlineDeviceList), disOnlineList)
        Catch ex As Exception
            Return New NormalResponse(False, ex.Message)
        End Try
    End Function
    Public Function Handle_GetDutyLogForWeChat(context As HttpListenerContext) As NormalResponse
        Try
            Dim count As Integer = context.Request.QueryString("count")
            Dim sql As String = "select * from logTable order by Time desc limit " & count
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) Then Return New NormalResponse(False, "没有数据")
            If dt.Rows.Count = 0 Then Return New NormalResponse(False, "没有数据")
            Dim sb As New StringBuilder
            For i = 0 To dt.Rows.Count - 1
                Dim row As DataRow = dt.Rows(i)
                Dim kind As String = row("Kind")
                Dim time As String = row("Time")
                Dim content As String = row("Content")
                If i > 0 Then
                    sb.AppendLine("")
                End If
                sb.AppendLine(kind)
                sb.AppendLine("时间:" & time)
                sb.AppendLine("事件:" & content)
            Next
            Return New NormalResponse(True, "", "", sb.ToString)
        Catch ex As Exception
            Return New NormalResponse(False, ex.Message)
        End Try
    End Function
    Public Function Handle_GetDeviceLogForWeChat(context As HttpListenerContext) As NormalResponse
        Try
            Dim count As Integer = context.Request.QueryString("count")
            Dim sql As String = "select * from devicelogTable order by Time desc limit " & count
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) Then Return New NormalResponse(False, "没有数据")
            If dt.Rows.Count = 0 Then Return New NormalResponse(False, "没有数据")
            Dim sb As New StringBuilder
            For i = 0 To dt.Rows.Count - 1
                Dim row As DataRow = dt.Rows(i)
                Dim log As String = row("Log")
                Dim time As String = row("Time")
                Dim deviceName As String = row("DeviceNickName")
                If i > 0 Then
                    sb.AppendLine("")
                End If
                sb.AppendLine("设备:" & deviceName)
                sb.AppendLine("时间:" & time)
                sb.AppendLine("事件:" & log)
            Next
            Return New NormalResponse(True, "", "", sb.ToString)
        Catch ex As Exception
            Return New NormalResponse(False, ex.Message)
        End Try
    End Function
    Private Function GetWarnKindName(ByVal kind As String) As String
        If kind = "GR" Then Return "干扰"
        If kind = "ZC" Then Return "空闲"
        If kind = "WZ" Then Return "违章"
        If kind = "GZ" Then Return "故障"
        If kind = "HGB" Then Return "黑广播"
    End Function
    Private Function WARN2Msg(ByVal strtmp As String, ByVal address As String, deviceName As String, msgTime As String) As String
        If InStr(strtmp, "WARN") Then
            strtmp = strtmp.Replace("<", "")
            strtmp = strtmp.Replace(">", "")
            strtmp = strtmp.Replace(Chr(13), "")
            strtmp = strtmp.Replace(Chr(10), "")
            Dim st() As String = strtmp.Split(",")
            Dim str As String
            Dim lx As String = ""
            If st(2) = "GR" Then lx = "干扰"
            If st(2) = "ZC" Then lx = "正常"
            If st(2) = "WZ" Then lx = "违章"
            If st(2) = "GZ" Then lx = "故障"
            If st(2) = "KX" Then lx = "空闲"
            If st(2) = "HGB" Then lx = "黑广播"
            Dim sb As New StringBuilder
            sb.AppendLine(deviceName)
            sb.AppendLine(lx & "预警")
            sb.AppendLine("频率: " & st(7) & "MHz")
            sb.AppendLine("场强: " & st(8) & "MHz")
            If st(2) = "GR" Then sb.AppendLine("干扰: " & st(10) & "MHz")
            sb.AppendLine("时间: " & msgTime)
            Return sb.ToString
        End If
    End Function
    Public Function Handle_GetAlarmForWeChat(context As HttpListenerContext) As NormalResponse
        Try
            Dim count As Integer = context.Request.QueryString("count")
            Dim sql As String = "select * from warnTable order by msgTime desc limit " & count
            Dim dt As DataTable = SQLGetDT(sql)
            If IsNothing(dt) Then Return New NormalResponse(False, "没有数据")
            If dt.Rows.Count = 0 Then Return New NormalResponse(False, "没有数据")
            Dim sb As New StringBuilder
            Dim isFirstRow As Boolean = True
            For Each row As DataRow In dt.Rows
                If isFirstRow Then
                    isFirstRow = True
                Else
                    sb.AppendLine("")
                End If
                Dim msg As String = row("DeviceMsg")
                Dim warnKind As String = row("WarnKind")
                Dim deviceName As String = GetDeviceNickNameByID(row("DeviceID"))
                sb.AppendLine(WARN2Msg(msg, "", deviceName, row("MsgTime")))
            Next
            Return New NormalResponse(True, "", "", sb.ToString)
        Catch ex As Exception
            Return New NormalResponse(False, ex.Message)
        End Try
    End Function

    Public Function Handle_GetTekBusAllInfo(context As HttpListenerContext) As NormalResponse
        Try
            Dim json As String = ""
            'SyncLock TekBusDevicesLock
            '    If IsNothing(TekBusDevices) = False Then
            '        json = JsonConvert.SerializeObject(TekBusDevices)
            '    End If
            'End SyncLock
            Return New NormalResponse(True, "", "", json)
        Catch ex As Exception
            Return New NormalResponse(False, ex.ToString)
        End Try
    End Function
End Class
