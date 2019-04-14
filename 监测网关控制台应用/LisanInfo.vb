Public Class LisanInfo
    Public freqStart As Double
    Public freqEnd As Double
    Public pointlist As List(Of LisanPointInfo)
    Public startTime As Date
    Public watchTime As Integer
End Class
Public Class LisanPointInfo
    Public freq As Double
    Public sigNalInfo As String  '信号属性
    Public sigNalStatus As String '状态属性
    Public isFree As Boolean '是否可用
    Public dbm As Double '信号电平
    Public dbm_max As Double
    Public dbm_min As Double
    Public dbm_avg As Double
    Public overCount As Integer  '信号出现次数
    Public overPercent As Integer '占用度

End Class
