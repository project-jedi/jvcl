object OpenDatabaseDlg: TOpenDatabaseDlg
  Left = 208
  Top = 181
  ActiveControl = rxDBLookupCombo1
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Open Database'
  ClientHeight = 151
  ClientWidth = 302
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 285
    Height = 105
    Shape = bsFrame
    IsControl = True
  end
  object Label1: TLabel
    Left = 20
    Top = 16
    Width = 45
    Height = 13
    Caption = '&Aliases:   '
    FocusControl = rxDBLookupCombo1
  end
  object Label2: TLabel
    Left = 20
    Top = 60
    Width = 54
    Height = 13
    Caption = '&Directory:   '
    FocusControl = DirectoryEdit1
  end
  object rxDBLookupCombo1: TJvDBLookupCombo
    Left = 20
    Top = 32
    Width = 261
    Height = 21
    DropDownCount = 8
    DisplayEmpty = '<none>'
    EmptyItemColor = 8421631
    ItemHeight = 14
    LookupField = 'NAME'
    LookupDisplay = 'NAME'
    LookupSource = DataSource1
    TabOrder = 0
    OnChange = rxDBLookupCombo1Change
    OnGetImage = DBLookupComboGetImage
  end
  object DirectoryEdit1: TJvDirectoryEdit
    Left = 20
    Top = 76
    Width = 261
    Height = 21
    ButtonHint = 'Select directory|'
    ButtonFlat = False
    CharCase = ecLowerCase
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnChange = DirectoryEdit1Change
  end
  object OkBtn: TButton
    Left = 134
    Top = 121
    Width = 77
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 216
    Top = 121
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object DataSource1: TDataSource
    Left = 176
    Top = 28
  end
  object FormStorage1: TJvFormStorage
    Options = []
    StoredProps.Strings = (
      'DirectoryEdit1.InitialDir')
    StoredValues = <>
    Left = 8
    Top = 116
  end
  object JvPicclip: TJvPicClip
    Cols = 2
    Picture.Data = {
      07544269746D617036010000424D360100000000000076000000280000002000
      00000C0000000100040000000000C00000000000000000000000100000000000
      000000000000000080000080000000808000800000008000800080800000C0C0
      C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF00DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD80000008DDDDDDDDDDDDDDDD
      DDDDDDD8877778808DDDDDD0000000000DDDDDD0F77888880DDDDDD0FFF0FFFF
      0DDDDDD8800000088DDDDDD0F4F0F44F0DDDDDD8877778808DDDDDD0FFF0FFFF
      0DDDDDD0F77888880DDDDDD0F4F0F44F0DDDDDD8800000088DDDDDD0FFF0FFFF
      0DDDDDD8877777708DDDDDD0000000000DDDDDD0F77777770DDDDDD0EEEEEEEE
      0DDDDDD87FFFFF788DDDDDD0000000000DDDDDDD88000088DDDDDDDDDDDDDDDD
      DDDD}
    Left = 204
    Top = 28
  end
end
