<div align="center">

## Read Barcode from Image


</div>

### Description

Read two width barcodes like 2 of 5 or Code 39 from an image.

How to use it:

Dim bmp As Bitmap

Dim scanner As New gmseNWScanner(New gmseBarcodeDef2of5interleaved)

Dim result As gmseNWResult

bmp = New Bitmap("d:\sample2of5.bmp")

scanner.Scan(bmp)

For Each result In scanner.Result

Debug.writeline(result.Text)

Next
 
### More Info
 
.NET Bitmap class

The algorithm is designed to be extended to read all two width symbologies.

2 of 5 interleaved barcodes in image.


<span>             |<span>
---                |---
**Submitted On**   |
**By**             |[mackenb](https://github.com/Planet-Source-Code/PSCIndex/blob/master/ByAuthor/mackenb.md)
**Level**          |Intermediate
**User Rating**    |4.5 (27 globes from 6 users)
**Compatibility**  |VB\.NET
**Category**       |[Algorithims](https://github.com/Planet-Source-Code/PSCIndex/blob/master/ByCategory/algorithims__10-29.md)
**World**          |[\.Net \(C\#, VB\.net\)](https://github.com/Planet-Source-Code/PSCIndex/blob/master/ByWorld/net-c-vb-net.md)
**Archive File**   |[](https://github.com/Planet-Source-Code/mackenb-read-barcode-from-image__10-4486/archive/master.zip)





### Source Code

```
' Barcode Reader for INTERLEAVED 2 OF 5
' (c) GMSE GmbH 2006
' http://imaging.gmse.net
Imports System.Text
Imports System.Drawing
Imports System.Drawing.Imaging
Public Class gmseNWResult
  Public Text As String
  Public Count As Integer
End Class
Public Class gmseNWResultCollection
  Implements IEnumerable
  Dim hash As New Hashtable(CaseInsensitiveHashCodeProvider.Default, CaseInsensitiveComparer.Default)
  ReadOnly Property Count() As Integer
    Get
      Return hash.Count
    End Get
  End Property
  ReadOnly Property Item(ByVal Index As Integer) As gmseNWResult
    Get
      Return hash(Index)
    End Get
  End Property
  Public Sub Add(ByVal Text As String)
    Dim r As gmseNWResult
    r = hash(Text)
    If r Is Nothing Then
      r = New gmseNWResult
      r.Text = Text
      hash.Add(r.Text, r)
    End If
    r.Count += 1
  End Sub
  Public Function GetEnumerator() As System.Collections.IEnumerator Implements System.Collections.IEnumerable.GetEnumerator
    Return hash.Values.GetEnumerator
  End Function
End Class
' Definition of one Symbol
Public Class gmseNWBarcodeSymbol
  Public Value As Integer
  Public Text As String
  ' Encoding of Symbol in NW notation
  Public Encoding As String
  Public Sub New(ByVal Value As Integer, ByVal Text As String, ByVal Encoding As String)
    Me.Value = Value
    Me.Text = Text
    Me.Encoding = Encoding
  End Sub
End Class
' Abstract parameters für barcode decoding for further extensions (2 of 5 standard,...)
Public MustInherit Class gmseNWBarcodeDef
  ' Anzahl Bars, die ein Barcode mindestens haben muß.
  MustOverride ReadOnly Property MinBarCount() As Integer
  ' Decodieren des Barcodes aus NW nach Text.
  Public MustOverride Function Decode(ByVal symbol As String) As String
  ' NWidth * Fkt <WWidth sein, sonst kein gültiger BC
  Overridable ReadOnly Property MaxFktNW() As Integer
    Get
      Return 10
    End Get
  End Property
  ' 30% Tolerance to classify N and W barcodes.
  Overridable ReadOnly Property PctNWTolerance() As Double
    Get
      Return 0.47
    End Get
  End Property
  ' Min length of white space before first bar and after last bar in percent of wide bar.
  Overridable ReadOnly Property PctMinWhiteSpace() As Double
    Get
      Return 2
    End Get
  End Property
End Class
' Definition 2 of 5 interleaved
Public Class gmseBarcodeDef2of5interleaved
  Inherits gmseNWBarcodeDef
  Dim StartSymbol As gmseNWBarcodeSymbol
  Dim StopSymbol As gmseNWBarcodeSymbol
  Dim hash As New Hashtable(CaseInsensitiveHashCodeProvider.Default, CaseInsensitiveComparer.Default)
  ' We only accept barcodes with at least 23 bars.
  Public Overrides ReadOnly Property MinBarCount() As Integer
    Get
      Return 23
    End Get
  End Property
  Private Sub CreateSymbolTable()
    Dim BaseSymbol(9) As gmseNWBarcodeSymbol
    Dim sym As gmseNWBarcodeSymbol
    Dim x As Integer
    Dim y As Integer
    Dim Text As String
    Dim i As Integer
    ' Basic symbols
    BaseSymbol(0) = New gmseNWBarcodeSymbol(0, "0", "NNWWN")
    BaseSymbol(1) = New gmseNWBarcodeSymbol(1, "1", "WNNNW")
    BaseSymbol(2) = New gmseNWBarcodeSymbol(2, "2", "NWNNW")
    BaseSymbol(3) = New gmseNWBarcodeSymbol(3, "3", "WWNNN")
    BaseSymbol(4) = New gmseNWBarcodeSymbol(4, "4", "NNWNW")
    BaseSymbol(5) = New gmseNWBarcodeSymbol(5, "5", "WNWNN")
    BaseSymbol(6) = New gmseNWBarcodeSymbol(6, "6", "NWWNN")
    BaseSymbol(7) = New gmseNWBarcodeSymbol(7, "7", "NNNWW")
    BaseSymbol(8) = New gmseNWBarcodeSymbol(8, "8", "WNNWN")
    BaseSymbol(9) = New gmseNWBarcodeSymbol(9, "9", "NWNWN")
    ' Buildung the hash for decoding:
    ' 2 of 5 is an double density barcode. 2 digits are codes together.
    ' The first with the black bars, the second with white bars.
    For x = 0 To 9
      For y = 0 To 9
        Text = ""
        For i = 0 To 4
          Text &= BaseSymbol(x).Encoding.Chars(i)
          Text &= BaseSymbol(y).Encoding.Chars(i)
        Next
        sym = New gmseNWBarcodeSymbol(x * 10 + y, Format(x * 10 + y, "00"), Text)
        hash.Add(sym.Encoding, sym)
      Next
    Next
    StartSymbol = New gmseNWBarcodeSymbol(-1, "Start", "NNNN")
    StopSymbol = New gmseNWBarcodeSymbol(-1, "Start", "WNN")
    hash(StopSymbol.Encoding) = StopSymbol
  End Sub
  Public Sub New()
    CreateSymbolTable()
  End Sub
  Public Overrides Function Decode(ByVal symbol As String) As String
    Return Decode1(symbol, 0)
  End Function
  ' Get human readable text from symvol code.
  Public Function Decode1(ByVal symbol As String, ByVal offset As Integer) As String
    Dim sym As String
    Dim sb As New StringBuilder
    Dim Pos As Integer = offset
    Dim bi As gmseNWBarcodeSymbol
    ' The number of bars must be x*10+3+4
    If (symbol.Length - 7) Mod 10 <> 0 Then
      Return ""
    End If
    ' The first symbol shoul be the start symbol.
    sym = symbol.Substring(Pos, StartSymbol.Encoding.Length)
    If String.Compare(sym, StartSymbol.Encoding, True) <> 0 Then
      Return ""
    End If
    Pos += StartSymbol.Encoding.Length
    ' Data
    While Pos < symbol.Length
      ' Symbols with data have 10 bars:
      If symbol.Length - Pos > 10 Then
        sym = symbol.Substring(Pos, 10)
        bi = hash(sym)
        If bi Is Nothing Then
          Return ""
        End If
        sb.Append(bi.Text)
        Pos += 10
      ElseIf symbol.Length - Pos >= 3 Then ' Stop has 3 bars:
        sym = symbol.Substring(Pos, 3)
        bi = hash(sym)
        If bi Is Nothing Then
          Return ""
        Else
          Return sb.ToString
        End If
      Else
        Return ""
      End If
    End While
    Return ""
  End Function
End Class
' Scanning algorithm:
Public Class gmseNWScanner
  Dim cBitMask(7) As Byte
  Dim cScanRow() As Byte
  Dim nBar As Integer
  Dim cBarWidth() As Integer
  Dim cBarBlack() As Boolean
  Dim cOffset As Integer
  Dim cLength As Integer
  Dim cNWidth As Integer
  Dim cWWidth As Integer
  Dim cBCDef As gmseNWBarcodeDef
  Dim cResult As gmseNWResultCollection
  ReadOnly Property Result() As gmseNWResultCollection
    Get
      Return cResult
    End Get
  End Property
  ' Find all 2 of 5 interleaved barcodes in Bitmap
  Public Sub Scan(ByVal bmp As Bitmap)
    Dim data As BitmapData
    Dim ScanRow() As Byte
    Dim y As Integer
    Dim x As Integer
    data = bmp.LockBits(New Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.ReadOnly, PixelFormat.Format1bppIndexed)
    Try
      cResult = New gmseNWResultCollection
      ReDim ScanRow(data.Stride - 1)
      ' Scan all rows of bitmap.
      For y = 0 To bmp.Height - 1
        For x = 0 To data.Stride - 1
          ScanRow(x) = System.Runtime.InteropServices.Marshal.ReadByte(data.Scan0, data.Stride * y + x)
        Next
        Scan(ScanRow, 0, bmp.Width)
      Next
    Finally
      bmp.UnlockBits(data)
    End Try
  End Sub
  ' ScanRow must be 1bppp format.
  Private Sub Scan(ByVal ScanRow() As Byte, ByVal offset As Integer, ByVal Length As Integer)
    cScanRow = ScanRow
    cLength = Length
    cOffset = offset
    CalcBars()
    Scan()
  End Sub
  ' Calculate bars
  Private Sub CalcBars()
    Dim x As Integer
    Dim bl As Boolean
    nBar = 0
    ReDim cBarWidth(10)
    ReDim cBarBlack(10)
    cBarBlack(0) = IsBlack(cOffset)
    For x = 0 To cLength - 1
      If nBar >= cBarWidth.Length - 1 Then
        ReDim Preserve cBarWidth(nBar * 2)
        ReDim Preserve cBarBlack(nBar * 2)
      End If
      bl = IsBlack(cOffset + x)
      If cBarBlack(nBar) = bl Then
        cBarWidth(nBar) += 1
      Else
        nBar += 1
        cBarBlack(nBar) = bl
        cBarWidth(nBar) = 1
      End If
    Next
    nBar += 1
  End Sub
  ' The scan
  Private Sub Scan()
    Dim Pos As Integer
    Dim ValidNW As Boolean
    Dim BCStart As Integer
    Dim BCEnde As Integer
    Dim Text As String
    Dim sb As StringBuilder
    Dim nwt As Integer
    Dim flag As Boolean
    ' We want a leading white space, so we start at position 1
    Pos = 1
    While Pos < nBar - cBCDef.MinBarCount
      ' Wir start with a black bar
      If cBarBlack(Pos) Then
        ' Try to validate with a sample set, if we can classify the bars in N and W bars within a tolerance of 25%.
        ValidNW = CalcNWWidth(Pos, cBCDef.MinBarCount)
        If ValidNW Then ' n und w are OK.
          sb = New StringBuilder
          BCStart = Pos
          BCEnde = Pos
          ' Start to decode, until no more valid bars are found.
          While Pos < nBar
            nwt = NWBarType(cBarWidth(Pos))
            If nwt = -1 Then
              Exit While
            ElseIf nwt = 0 Then
              sb.Append("n")
            Else
              sb.Append("w")
            End If
            BCEnde += 1
            Pos += 1
          End While
          BCEnde -= 1
          ' Check white space
          flag = cBarWidth(BCStart - 1) >= cBCDef.PctMinWhiteSpace * cWWidth
          If flag Then
            flag = BCEnde < nBar - 1
          End If
          If flag Then
            flag = cBarWidth(BCEnde + 1) >= cBCDef.PctMinWhiteSpace * cWWidth
          End If
          If flag Then
            ' Get the text representation
            Text = cBCDef.Decode(sb.ToString)
            If Text <> "" Then
              cResult.Add(Text)
              Debug.WriteLine(Text)
            End If
          End If
        Else
          Pos += 1
        End If
      Else
        Pos += 1
      End If
    End While
  End Sub
  ' Classify width of a bar in -1: invalid, 0 N(arrow), 1 W(ide).
  Private Function NWBarType(ByVal Width As Integer) As Integer
    Dim MaxAbweichung As Double
    Dim Abweichung As Integer
    Abweichung = Math.Min(Math.Abs(Width - cNWidth), Math.Abs(Width - cWWidth))
    MaxAbweichung = (cWWidth - cNWidth) * cBCDef.PctNWTolerance
    If Abweichung > MaxAbweichung Then
      Return -1
    ElseIf Math.Abs(Width - cNWidth) < Math.Abs(Width - cWWidth) Then
      Return 0
    Else
      Return 1
    End If
  End Function
  ' Calculate Narrow and Wide bar width.
  Private Function CalcNWWidth(ByVal Pos As Integer, ByVal Length As Integer) As Boolean
    Dim NWidth As Integer
    Dim WWidth As Integer
    Dim tmp As Integer
    Dim Abweichung As Double
    Dim MaxAbweichung As Double
    Dim i As Integer
    ' Calc Min and Max.
    NWidth = cBarWidth(Pos)
    WWidth = cBarWidth(Pos)
    For i = 0 To Length - 1
      tmp = cBarWidth(Pos + i)
      If tmp > WWidth Then
        WWidth = tmp
      End If
      If tmp < NWidth Then
        NWidth = tmp
      End If
    Next
    ' As wide we accept as maximum NWidth * cBCDef.MaxFktNW
    If NWidth * cBCDef.MaxFktNW < WWidth Then
      cWWidth = 0
      cNWidth = 0
      Return False
    End If
    Abweichung = 0
    ' Calculate Tolerance
    For i = 0 To Length - 1
      tmp = Math.Min(Math.Abs(cBarWidth(Pos + i) - NWidth), Math.Abs(cBarWidth(Pos + i) - WWidth))
      If tmp > Abweichung Then
        Abweichung = tmp
      End If
    Next
    ' Wir haben: ----N--]----[--W-----
    ' We accept PctNWTolerance% percent maximum.
    MaxAbweichung = (WWidth - NWidth) * cBCDef.PctNWTolerance
    If Abweichung > MaxAbweichung Then
      cWWidth = 0
      cNWidth = 0
      Return False
    Else
      cWWidth = WWidth
      cNWidth = NWidth
      Return True
    End If
  End Function
  ' Is the pixel black?
  Private ReadOnly Property IsBlack(ByVal x As Integer) As Boolean
    Get
      Return (cScanRow(x \ 8) And cBitMask(x Mod 8)) = 0
    End Get
  End Property
  ' Hepler for calculating pixel color.
  Private Sub CreateBitMask()
    Dim b As Byte = 1
    Dim i As Integer
    For i = 0 To 7
      cBitMask(7 - i) = b
      If i < 7 Then
        b = b * 2
      End If
    Next
  End Sub
  Public Sub New(ByVal bcd As gmseNWBarcodeDef)
    cBCDef = bcd
    CreateBitMask()
  End Sub
End Class
```

