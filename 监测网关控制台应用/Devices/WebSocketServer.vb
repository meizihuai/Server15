Imports System
Imports System.IO
Imports System.Text
Imports System.Threading
Imports System.Threading.Thread
Imports System.Net
Imports System.Net.HttpListener
Imports System.Net.WebSockets
Public Class WebSocketServer
    Private url As String
    Private listener As HttpListener
    Private clientList As List(Of WebSocket)
    Private clientListLock As Object
    Sub New(_url As String)
        url = _url
        clientListLock = New Object
        clientList = New List(Of WebSocket)
    End Sub

    Sub start()
        Try
            log("开启WebSocket服务,url=" & url)
            listener = New HttpListener()
            listener.Prefixes.Add(url)
            listener.Start()
            listener.BeginGetContext(New AsyncCallback(AddressOf GetContextCallBackAsync), listener)
            log("WebSocket服务开启成功")
        Catch ex As Exception
            log("WebSocket服务开启失败，" & ex.Message)
        End Try
    End Sub
    Private Async Function GetContextCallBackAsync(ByVal ar As IAsyncResult) As Tasks.Task
        Try
            listener = ar.AsyncState
            Dim context As HttpListenerContext = listener.EndGetContext(ar)
            listener.BeginGetContext(New AsyncCallback(AddressOf GetContextCallBackAsync), listener)
            If IsNothing(context) = False Then
                Dim ws As WebSockets.HttpListenerWebSocketContext = Await context.AcceptWebSocketAsync(Nothing)
                Dim socket As WebSocket = ws.WebSocket
                SyncLock clientListLock
                    clientList.Add(socket)
                End SyncLock
                'For i = 0 To 10
                '    Dim buffer() As Byte = Encoding.Default.GetBytes(i & "_Hello" & vbCrLf)
                '    Dim abuf As New ArraySegment(Of Byte)(buffer)
                '    Await socket.SendAsync(abuf, WebSocketMessageType.Text, True, CancellationToken.None)
                'Next
                'Dim abuf As ArraySegment(Of Byte)
                'Dim result As WebSockets.WebSocketReceiveResult = Await ws.WebSocket.ReceiveAsync(abuf, CancellationToken.None)
                'log(result.MessageType)
                'Dim bRec(wsdata.Count - 1) As Byte
                'Array.Copy(buf, bRec, wsdata.Count)
                'log(Encoding.Default.GetString(bRec))
            End If
        Catch ex As Exception

        End Try
    End Function
    Public Sub Close()
        Try
            listener.Close()
            listener.Abort()
            listener = Nothing
        Catch ex As Exception

        End Try
    End Sub
    Public Sub SendMsgToAllAsync(msg As String)
        SyncLock clientListLock
            For i = clientList.Count - 1 To 0 Step -1
                Dim itm As WebSocket = clientList(i)
                If IsNothing(itm) = False Then
                    If itm.State = WebSocketState.Open Then
                        Dim buffer() As Byte = Encoding.Default.GetBytes(msg)
                        Dim abuf As New ArraySegment(Of Byte)(buffer)
                        itm.SendAsync(abuf, WebSocketMessageType.Text, True, CancellationToken.None)
                    Else
                        clientList.RemoveAt(i)
                    End If
                Else
                    clientList.RemoveAt(i)
                End If
            Next
        End SyncLock
    End Sub
End Class
