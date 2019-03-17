Public Class RunLocation
    Public lng As String
    Public lat As String
    Public time As String
    Sub New()

    End Sub
    Sub New(ByVal _lng As String, ByVal _lat As String, ByVal _time As String)
        lng = _lng
        lat = _lat
        time = _time
    End Sub
    Public Function Copy() As RunLocation
        If IsNothing(Me) Then Return Nothing
        Return Me.MemberwiseClone()
    End Function
End Class
