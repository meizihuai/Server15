Module Module3
    Public crcTable() As Byte = New Byte() {&H21, &H7F, &H9D, &HC3, &H40, &H1E, &HFC, &HA2, &HE3, &HBD, &H5F, &H1, &H82, &HDC, &H3E, &H60,
&HBC, &HE2, &H0, &H5E, &HDD, &H83, &H61, &H3F, &H7E, &H20, &HC2, &H9C, &H1F, &H41, &HA3, &HFD,
&H2, &H5C, &HBE, &HE0, &H63, &H3D, &HDF, &H81, &HC0, &H9E, &H7C, &H22, &HA1, &HFF, &H1D, &H43,
&H9F, &HC1, &H23, &H7D, &HFE, &HA0, &H42, &H1C, &H5D, &H3, &HE1, &HBF, &H3C, &H62, &H80, &HDE,
&H67, &H39, &HDB, &H85, &H6, &H58, &HBA, &HE4, &HA5, &HFB, &H19, &H47, &HC4, &H9A, &H78, &H26,
&HFA, &HA4, &H46, &H18, &H9B, &HC5, &H27, &H79, &H38, &H66, &H84, &HDA, &H59, &H7, &HE5, &HBB,
&H44, &H1A, &HF8, &HA6, &H25, &H7B, &H99, &HC7, &H86, &HD8, &H3A, &H64, &HE7, &HB9, &H5B, &H5,
&HD9, &H87, &H65, &H3B, &HB8, &HE6, &H4, &H5A, &H1B, &H45, &HA7, &HF9, &H7A, &H24, &HC6, &H98,
&HAD, &HF3, &H11, &H4F, &HCC, &H92, &H70, &H2E, &H6F, &H31, &HD3, &H8D, &HE, &H50, &HB2, &HEC,
&H30, &H6E, &H8C, &HD2, &H51, &HF, &HED, &HB3, &HF2, &HAC, &H4E, &H10, &H93, &HCD, &H2F, &H71,
&H8E, &HD0, &H32, &H6C, &HEF, &HB1, &H53, &HD, &H4C, &H12, &HF0, &HAE, &H2D, &H73, &H91, &HCF,
&H13, &H4D, &HAF, &HF1, &H72, &H2C, &HCE, &H90, &HD1, &H8F, &H6D, &H33, &HB0, &HEE, &HC, &H52,
&HEB, &HB5, &H57, &H9, &H8A, &HD4, &H36, &H68, &H29, &H77, &H95, &HCB, &H48, &H16, &HF4, &HAA,
&H76, &H28, &HCA, &H94, &H17, &H49, &HAB, &HF5, &HB4, &HEA, &H8, &H56, &HD5, &H8B, &H69, &H37,
&HC8, &H96, &H74, &H2A, &HA9, &HF7, &H15, &H4B, &HA, &H54, &HB6, &HE8, &H6B, &H35, &HD7, &H89,
&H55, &HB, &HE9, &HB7, &H34, &H6A, &H88, &HD6, &H97, &HC9, &H2B, &H75, &HF6, &HA8, &H4A, &H14}
    Public Function getValueByCanShuQu(ByVal str As String, ByVal quarName As String) As String
        If InStr(str, ";") = False Then Return ""
        Dim st() As String = str.Split(";")
        For Each sh In st
            If InStr(sh, "=") Then
                Dim sk() As String = sh.Split("=")
                If sk.Length <> 2 Then Return ""
                If sk(0) = quarName Then
                    Return sk(1)
                End If
            End If
        Next
    End Function
End Module
