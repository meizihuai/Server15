﻿Imports System.Net
Imports System.Net.Sockets
Imports System.Threading
Imports System.Text
Public Class TSS_GateWayServer
    Public _LocationListenSocket As Socket '本地侦听服务   
    Public Event RefrushDeviceList()
    Public Event RaiseLog(ByVal mainMsg As String, ByVal UserName As String, ByVal IP As String)
    Public Event RaiseHttplog(ByVal str As String)
    Private myPort As Integer
    Public Sub Httplog(ByVal str As String)
        RaiseEvent RaiseHttplog(str)
    End Sub
    Sub New(ByVal Port As String)
        Try
            log("初始化TSS_GateWay服务器,端口:" & Port)
            myPort = Port
            Dim strHostName As String = Dns.GetHostName()
            Dim strServerHost As New IPEndPoint(IPAddress.Any, Int32.Parse(Port))
            _LocationListenSocket = New Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
            _LocationListenSocket.Bind(strServerHost)
            _LocationListenSocket.Listen(1024)
            _LocationListenSocket.SetSocketOption(SocketOptionLevel.Tcp, SocketOptionName.AcceptConnection, 1)
        Catch ex As Exception
            log("初始化TSS_GateWay服务器失败，" & ex.Message)
        End Try
    End Sub
    Public Sub StartServer()
        Try
            log("TSS网关服务器已开启,端口:" & myPort)
            Dim th As New Thread(AddressOf ListenClient)
            th.Start()
        Catch ex As Exception
            log("TSS_GateWay服务器开启失败，" & ex.Message)
            If (Not _LocationListenSocket Is Nothing) Then
                If _LocationListenSocket.Connected Then
                    _LocationListenSocket.Close()
                End If
            End If
        End Try
    End Sub
    Private Sub ListenClient()
        While True
            Try
                If Not _LocationListenSocket Is Nothing Then
                    Dim clientSocket As System.Net.Sockets.Socket
                    clientSocket = New Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
                    clientSocket = _LocationListenSocket.Accept()
                    If Not clientSocket Is Nothing Then
                        log("TSS网关服务器>>新连接")
                        Dim TSSDevice As New TSS_GateWay(clientSocket)
                        AddHandler TSSDevice.RefrushDeviceList, AddressOf sub_RefrushDeviceList
                        AddHandler TSSDevice.RaiseLog, AddressOf sub_log
                        AddHandler TSSDevice.RaiseHttplog, AddressOf sub_Httplog
                        TSSDevice.Start()
                    End If
                End If
            Catch ex As Exception

            End Try
        End While
    End Sub
    Private Sub log(ByVal MainMsg As String)
        'Form1.log(MainMsg, "TSS_Server", "+:" & _Port)

        RaiseEvent RaiseLog(MainMsg, "TSS_GateWay服务器" & "", "")
    End Sub
    Private Sub sub_RefrushDeviceList()
        RaiseEvent RefrushDeviceList()
    End Sub
    Private Sub sub_log(ByVal mainMsg As String, ByVal UserName As String, ByVal IP As String)
        RaiseEvent RaiseLog(mainMsg, UserName, IP)
    End Sub
    Private Sub sub_Httplog(ByVal str As String)
        RaiseEvent RaiseHttplog(str)
    End Sub
End Class


