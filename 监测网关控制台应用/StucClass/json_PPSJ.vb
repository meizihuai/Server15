﻿Public Class json_PPSJ
    Public freqStart As Double
    Public freqStep As Double
    Public freqEnd As Double
    Public deviceID As String
    Public dataCount As Integer
    Public runLocation As runLocation
    Public value() As Double
    Public isDSGFreq As Boolean
    Public DSGFreqBase64 As String
    Public Function Copy() As json_PPSJ
        If IsNothing(Me) Then Return Nothing
        Dim tmp As json_PPSJ = Me.MemberwiseClone
        If IsNothing(runLocation) = False Then tmp.runLocation = runLocation.Copy()
        Return tmp
    End Function
End Class
