object MainFormDlg: TMainFormDlg
  Left = 321
  Top = 227
  BorderStyle = bsDialog
  Caption = 'AppStorage Example #2'
  ClientHeight = 324
  ClientWidth = 382
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 305
    Width = 382
    Height = 19
    Panels = <>
  end
  object Panel1: TPanel
    Left = 0
    Top = 52
    Width = 382
    Height = 253
    Align = alClient
    TabOrder = 1
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 380
      Height = 137
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 12
        Width = 55
        Height = 13
        Caption = 'Stored Edit:'
      end
      object RadioButton1: TRadioButton
        Left = 64
        Top = 34
        Width = 113
        Height = 17
        Caption = 'RadioButton1'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object RadioButton2: TRadioButton
        Left = 64
        Top = 50
        Width = 113
        Height = 17
        Caption = 'RadioButton2'
        TabOrder = 1
      end
      object RadioButton3: TRadioButton
        Left = 64
        Top = 66
        Width = 113
        Height = 17
        Caption = 'RadioButton3'
        TabOrder = 2
      end
      object CheckBox1: TCheckBox
        Left = 64
        Top = 88
        Width = 97
        Height = 17
        Caption = 'CheckBox1'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object Edit1: TEdit
        Left = 64
        Top = 8
        Width = 121
        Height = 21
        TabOrder = 4
        Text = 'Some Value'
      end
      object CheckBox2: TCheckBox
        Left = 64
        Top = 104
        Width = 97
        Height = 17
        Caption = 'CheckBox2'
        TabOrder = 5
      end
      object TrackBar1: TTrackBar
        Left = 240
        Top = 32
        Width = 121
        Height = 24
        TabOrder = 6
        ThumbLength = 15
      end
      object DateTimePicker1: TDateTimePicker
        Left = 248
        Top = 64
        Width = 116
        Height = 21
        Date = 38114.956064340280000000
        Time = 38114.956064340280000000
        TabOrder = 7
      end
      object Button1: TButton
        Left = 240
        Top = 96
        Width = 129
        Height = 25
        Caption = 'Open Dialog'
        TabOrder = 8
      end
    end
    object Panel3: TPanel
      Left = 1
      Top = 138
      Width = 380
      Height = 114
      Align = alClient
      BevelOuter = bvSpace
      Caption = 'Panel3'
      TabOrder = 1
      object Label2: TLabel
        Left = 1
        Top = 1
        Width = 378
        Height = 13
        Align = alTop
        Caption = 'Stored Memo:'
      end
      object Memo2: TMemo
        Left = 1
        Top = 14
        Width = 378
        Height = 99
        Align = alClient
        Lines.Strings = (
          ' This demo of the appstorage components shows how to '
          '  store and restore property values at runtime')
        TabOrder = 0
      end
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 382
    Height = 52
    AutoSize = True
    BorderWidth = 2
    ButtonHeight = 21
    ButtonWidth = 148
    Caption = 'ToolBar1'
    EdgeBorders = []
    EdgeInner = esNone
    EdgeOuter = esNone
    ShowCaptions = True
    TabOrder = 2
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Caption = 'Save Form Values'
      ImageIndex = 0
      OnClick = ToolButton1Click
    end
    object ToolButton2: TToolButton
      Left = 148
      Top = 2
      Caption = 'Load Form Values'
      ImageIndex = 1
      Wrap = True
      OnClick = ToolButton2Click
    end
    object ToolButton3: TToolButton
      Left = 0
      Top = 23
      Caption = 'Select Values From List'
      ImageIndex = 2
      OnClick = Button1Click
    end
    object ToolButton4: TToolButton
      Left = 148
      Top = 23
      Caption = 'Save Values By Name To List'
      ImageIndex = 3
      OnClick = Button2Click
    end
  end
  object JvAppIniFileStorage1: TJvAppIniFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    FileName = 'FilenameRequired.ini'
    DefaultSection = 'General'
    SubStorages = <
      item
      end
      item
      end>
    Left = 200
    Top = 120
  end
  object JvFormStorage1: TJvFormStorage
    AppStorage = JvAppIniFileStorage1
    StoredProps.Strings = (
      'Option1.Checked'
      'YetAnotherOption1.Checked'
      'AnotherOptiopn1.Checked'
      'RadioButton1.Checked'
      'RadioButton2.Checked'
      'RadioButton3.Checked'
      'CheckBox1.Checked'
      'CheckBox2.Checked'
      'DateTimePicker1.Date'
      'Edit1.Text'
      'OpenDialog1.InitialDir'
      'OpenDialog1.FileName'
      'TrackBar1.Position'
      'Memo2.Lines')
    StoredValues = <>
    Left = 208
    Top = 88
  end
  object PopupMenu1: TPopupMenu
    Left = 161
    Top = 119
    object Option1: TMenuItem
      Caption = 'Option'
      OnClick = YetAnotherOption1Click
    end
    object AnotherOptiopn1: TMenuItem
      Caption = 'Another Option'
      Checked = True
      OnClick = YetAnotherOption1Click
    end
    object YetAnotherOption1: TMenuItem
      Caption = 'Yet Another Option'
      OnClick = YetAnotherOption1Click
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 200
    Top = 48
  end
  object JvFormStorageSelectList1: TJvFormStorageSelectList
    AppStorage = JvAppIniFileStorage1
    SelectPath = 'something'
    FormStorage = JvFormStorage1
    Left = 169
    Top = 82
  end
end
