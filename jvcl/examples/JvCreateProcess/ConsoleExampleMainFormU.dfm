object ConsoleExampleMainForm: TConsoleExampleMainForm
  Left = 207
  Top = 158
  Width = 701
  Height = 483
  Caption = 'JVCL TJvCreateProcess console redirect demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100001001000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000BBBB0000000000BB000BB000000000BB0000B000000000B
    BB000BB00000000BBB000BB00000000000000BB00000000000000BB000000000
    00000BB00000000000000BB00000000000000BB00000000000000BB000000000
    00000BB0000000000000BBBB00000000000BBBBBB0000000000000000000FFFF
    0000F87F0000E73F0000E7BF0000E39F0000E39F0000FF9F0000FF9F0000FF9F
    0000FF9F0000FF9F0000FF9F0000FF9F0000FF0F0000FE070000FFFF0000}
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lsbConsole: TListBox
    Left = 0
    Top = 0
    Width = 693
    Height = 390
    TabStop = False
    Align = alClient
    BevelInner = bvNone
    Color = clBlack
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 16
    ParentCtl3D = False
    ParentFont = False
    PopupMenu = PopupMenu1
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 390
    Width = 693
    Height = 59
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 198
      Height = 13
      Caption = 'Enter '#39'Exit'#39' to end the command processor'
    end
    object edtCommand: TEdit
      Left = 8
      Top = 28
      Width = 585
      Height = 21
      TabOrder = 0
      OnKeyDown = edtCommandKeyDown
      OnKeyPress = edtCommandKeyPress
    end
    object btnExecute: TButton
      Left = 600
      Top = 28
      Width = 75
      Height = 21
      Caption = 'Execute'
      TabOrder = 1
      OnClick = btnExecuteClick
    end
  end
  object JvCreateProcess1: TJvCreateProcess
    OnTerminate = JvCreateProcess1Terminate
    OnRead = JvCreateProcess1Read
    Left = 336
    Top = 224
  end
  object PopupMenu1: TPopupMenu
    Left = 216
    Top = 104
    object mnuFont: TMenuItem
      Caption = 'Font...'
      OnClick = mnuFontClick
    end
    object mnuBackgroundColor: TMenuItem
      Caption = 'Background color...'
      OnClick = mnuBackgroundColorClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuClear: TMenuItem
      Caption = 'Clear'
      OnClick = mnuClearClick
    end
  end
end
