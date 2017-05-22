object JvAppStorageBaseMainFrm: TJvAppStorageBaseMainFrm
  Left = 321
  Top = 227
  Caption = 'App Storage Demo'
  ClientHeight = 455
  ClientWidth = 652
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
    Top = 436
    Width = 652
    Height = 19
    Panels = <>
    ExplicitTop = 332
    ExplicitWidth = 366
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 652
    Height = 436
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 366
    ExplicitHeight = 332
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 650
      Height = 137
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 364
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
        Date = 38114.956064340300000000
        Time = 38114.956064340300000000
        TabOrder = 7
      end
      object Button1: TButton
        Left = 240
        Top = 96
        Width = 129
        Height = 25
        Caption = 'Open Dialog'
        TabOrder = 8
        OnClick = Button1Click
      end
    end
    object Panel3: TPanel
      Left = 1
      Top = 138
      Width = 650
      Height = 297
      Align = alClient
      BevelOuter = bvSpace
      Caption = 'Panel3'
      TabOrder = 1
      ExplicitWidth = 364
      ExplicitHeight = 193
      object Label2: TLabel
        Left = 1
        Top = 1
        Width = 648
        Height = 13
        Align = alTop
        Caption = 'Stored Memo:'
        ExplicitWidth = 66
      end
      object Memo2: TMemo
        Left = 1
        Top = 14
        Width = 648
        Height = 282
        Align = alClient
        Color = clInfoBk
        Lines.Strings = (
          ' The goal of appstorage components is to provide quick and'
          ' easy storage of published properties of objects. In a word:'
          ' Persistence. AppStorage components can be use a number'
          ' of storage vehicles such as INIFiles, the Registry, or even'
          ' XML.'
          ''
          ' Of particular use, and the focus of this demo is the'
          ' descendent TjvFormStorage which when double clicked on'
          ' reveals the properties of the components on the form.'
          ' This makes it an ideal choice for quick and painless'
          ' persistence of values on a form. Great for configuration'
          ' dialogs and per-user settings between sessions. (such'
          ' as storing the directory of the last opened file) ')
        TabOrder = 0
        ExplicitWidth = 362
        ExplicitHeight = 178
      end
    end
  end
  object JvAppIniFileStorage1: TJvAppIniFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    FileName = 'FilenameRequired%SCREENSIZe%.ini'
    DefaultSection = 'General'
    SubStorages = <
      item
      end
      item
      end>
    Left = 200
    Top = 87
  end
  object JvFormStorage1: TJvFormStorage
    AppStorage = JvAppIniFileStorage1
    AppStoragePath = 'Test\'
    StoredProps.Strings = (
      'Option1.Checked'
      'YetAnotherOption1.Checked'
      'AnotherOptiopn1.Checked'
      'RadioButton1.Checked'
      'RadioButton2.Checked'
      'RadioButton3.Checked'
      'DateTimePicker1.Date'
      'Edit1.Text'
      'OpenDialog1.InitialDir'
      'OpenDialog1.FileName'
      'TrackBar1.Position'
      'Memo2.Lines')
    StoredValues = <
      item
        Name = 'Name'
      end
      item
        Name = 'TestBoolean'
        Value = False
      end>
    Left = 200
    Top = 40
  end
  object PopupMenu1: TPopupMenu
    Left = 138
    Top = 163
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
    Top = 8
  end
  object JvAppRegistryStorage1: TJvAppRegistryStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    StorageOptions.BooleanAsString = False
    Root = 'Software\JVCL\Examples\Appstorage Sample1'
    SubStorages = <>
    Left = 206
    Top = 131
  end
end
