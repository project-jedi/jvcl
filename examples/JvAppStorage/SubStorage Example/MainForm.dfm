object MainFormDlg: TMainFormDlg
  Left = 321
  Top = 227
  BorderStyle = bsDialog
  Caption = 'AppStorage Example #3'
  ClientHeight = 397
  ClientWidth = 394
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
    Top = 378
    Width = 394
    Height = 19
    Panels = <>
  end
  object Panel1: TPanel
    Left = 0
    Top = 73
    Width = 394
    Height = 305
    Align = alClient
    TabOrder = 1
    object Panel3: TPanel
      Left = 1
      Top = 138
      Width = 392
      Height = 166
      Align = alClient
      BevelOuter = bvSpace
      Caption = 'Panel3'
      TabOrder = 0
      object Label2: TLabel
        Left = 1
        Top = 1
        Width = 390
        Height = 13
        Align = alTop
        Caption = 'Stored Memo:'
      end
      object Memo2: TMemo
        Left = 1
        Top = 14
        Width = 390
        Height = 151
        Align = alClient
        Lines.Strings = (
          
            'In addition to storing just component properties for the user, a' +
            ' programmer can '
          
            'treat the TjvAppStorage like a filesystem or even a database. Va' +
            'lues can be '
          'associated with a path and retrieved or written as desired. '
          ''
          
            'This can be useful for configuration data, stateful data which m' +
            'ay not be '
          
            'published such as selection information, or even just simple dat' +
            'asets. '
          ''
          
            'This example also demonstrates how the appstorage component can ' +
            'be linked '
          
            'to the registry, an XML file, or an INI file. Using only the pat' +
            'h information the '
          'correct storage facility is used.'
          '')
        TabOrder = 0
      end
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 392
      Height = 137
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 8
        Top = 12
        Width = 55
        Height = 13
        Caption = 'Stored Edit:'
      end
      object Label3: TLabel
        Left = 40
        Top = 48
        Width = 61
        Height = 13
        Caption = 'Storage Kind'
      end
      object rbXML: TRadioButton
        Left = 38
        Top = 63
        Width = 113
        Height = 17
        Caption = 'XML File'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = jvStorageKindClick
      end
      object rbReg: TRadioButton
        Left = 38
        Top = 79
        Width = 113
        Height = 17
        Caption = 'Registry'
        TabOrder = 1
        OnClick = jvStorageKindClick
      end
      object rbINI: TRadioButton
        Left = 38
        Top = 95
        Width = 113
        Height = 17
        Caption = 'INI File'
        TabOrder = 2
        OnClick = jvStorageKindClick
      end
      object Edit1: TEdit
        Left = 64
        Top = 8
        Width = 121
        Height = 21
        TabOrder = 3
        Text = 'Some Value'
      end
      object Button1: TButton
        Left = 168
        Top = 96
        Width = 73
        Height = 25
        Caption = 'Open Dialog'
        TabOrder = 4
      end
      object ListBox1: TListBox
        Left = 248
        Top = 16
        Width = 121
        Height = 105
        ItemHeight = 13
        Items.Strings = (
          'Item 1'
          'Item 2'
          'Item 3'
          'Item 4'
          'Item 5'
          'Item 6')
        TabOrder = 5
      end
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 394
    Height = 73
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
    object ToolButton4: TToolButton
      Left = 0
      Top = 23
      Caption = 'Save Values By Name To List'
      ImageIndex = 3
      OnClick = Button2Click
    end
    object ToolButton3: TToolButton
      Left = 148
      Top = 23
      Caption = 'Select Values From List'
      ImageIndex = 2
      Wrap = True
      OnClick = Button1Click
    end
    object ToolButton5: TToolButton
      Left = 0
      Top = 44
      Caption = 'Save ListBox Select Value'
      ImageIndex = 4
      OnClick = ToolButton5Click
    end
    object ToolButton6: TToolButton
      Left = 148
      Top = 44
      Caption = 'Load ListBox Select Value'
      ImageIndex = 5
      OnClick = ToolButton6Click
    end
  end
  object JvAppIniFileStorage1: TJvAppIniFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    FileName = 'AppStorageExample3.xml'
    DefaultSection = 'General'
    SubStorages = <>
    Left = 304
    Top = 190
  end
  object JvFormStorage1: TJvFormStorage
    AppStorage = JvAppStorage1
    OnRestorePlacement = JvFormStorage1RestorePlacement
    StoredProps.Strings = (
      'Option1.Checked'
      'YetAnotherOption1.Checked'
      'AnotherOptiopn1.Checked'
      'OpenDialog1.InitialDir'
      'OpenDialog1.FileName'
      'Memo2.Lines')
    StoredValues = <>
    Left = 203
    Top = 83
  end
  object PopupMenu1: TPopupMenu
    Left = 169
    Top = 135
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
    Left = 8
    Top = 120
  end
  object JvFormStorageSelectList1: TJvFormStorageSelectList
    AppStorage = JvAppStorage1
    SelectPath = 'something'
    FormStorage = JvFormStorage1
    Left = 110
    Top = 101
  end
  object JvAppXMLFileStorage1: TJvAppXMLFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    FileName = 'AppStorageExample3.xml'
    RootNodeName = 'Configuration'
    SubStorages = <>
    Left = 304
    Top = 143
  end
  object JvAppRegistryStorage1: TJvAppRegistryStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    Root = 'Software\JVCL\JvAppStorageDemo3'
    SubStorages = <>
    Left = 304
    Top = 96
  end
  object JvAppStorage1: TJvAppStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    SubStorages = <
      item
        RootPath = 'XML'
        AppStorage = JvAppXMLFileStorage1
      end
      item
        RootPath = 'REG'
        AppStorage = JvAppRegistryStorage1
      end
      item
        RootPath = 'INI'
        AppStorage = JvAppIniFileStorage1
      end>
    Left = 200
    Top = 136
  end
end
