object Form1: TForm1
  Left = 286
  Top = 168
  Width = 870
  Height = 640
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 20
    Top = 95
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 25
    Top = 155
    Width = 156
    Height = 25
    Caption = 'Test &1 Default Engine'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 25
    Top = 190
    Width = 156
    Height = 25
    Caption = 'Test &2 Base Engine'
    Default = True
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 25
    Top = 225
    Width = 156
    Height = 25
    Caption = 'Test &3 Jvcl Engine'
    Default = True
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 25
    Top = 260
    Width = 156
    Height = 25
    Caption = 'Test &4 DevExp Cx Engine'
    Default = True
    TabOrder = 4
    OnClick = Button4Click
  end
  object GroupBox1: TGroupBox
    Left = 190
    Top = 155
    Width = 331
    Height = 214
    Caption = 'Parameterlist Settings'
    TabOrder = 5
    object Label1: TLabel
      Left = 138
      Top = 20
      Width = 51
      Height = 13
      Caption = 'Max Width'
    end
    object Label2: TLabel
      Left = 218
      Top = 20
      Width = 54
      Height = 13
      Caption = 'Max Height'
    end
    object Label3: TLabel
      Left = 138
      Top = 60
      Width = 28
      Height = 13
      Caption = 'Width'
    end
    object Label4: TLabel
      Left = 218
      Top = 60
      Width = 31
      Height = 13
      Caption = 'Height'
    end
    object AutoWidthCheckBox: TCheckBox
      Left = 10
      Top = 20
      Width = 97
      Height = 17
      Caption = 'Auto Width'
      TabOrder = 0
    end
    object AutoHeightCheckBox: TCheckBox
      Left = 10
      Top = 40
      Width = 97
      Height = 17
      Caption = 'Auto Height'
      TabOrder = 1
    end
    object LoadFromCheckBox: TCheckBox
      Left = 10
      Top = 60
      Width = 119
      Height = 17
      Caption = 'Load From AppStore'
      TabOrder = 2
    end
    object StoreToCheckBox: TCheckBox
      Left = 10
      Top = 80
      Width = 111
      Height = 17
      Caption = 'Store To Appstore'
      TabOrder = 3
    end
    object MaxWidthEdit: TMaskEdit
      Left = 138
      Top = 35
      Width = 59
      Height = 21
      EditMask = '0999;0; '
      MaxLength = 4
      TabOrder = 4
      Text = '600'
    end
    object MaxHeightEdit: TMaskEdit
      Left = 218
      Top = 35
      Width = 59
      Height = 21
      EditMask = '0999;0; '
      MaxLength = 4
      TabOrder = 5
      Text = '400'
    end
    object WidthEdit: TMaskEdit
      Left = 138
      Top = 75
      Width = 56
      Height = 21
      EditMask = '0999;0; '
      MaxLength = 4
      TabOrder = 6
      Text = '0'
    end
    object HeightEdit: TMaskEdit
      Left = 218
      Top = 75
      Width = 58
      Height = 21
      EditMask = '0999;0; '
      MaxLength = 4
      TabOrder = 7
      Text = '0'
    end
    object DevExpCxLookAndFeelRadioGroup: TRadioGroup
      Left = 8
      Top = 134
      Width = 137
      Height = 73
      Caption = 'DevExp Cx LookAndFeel'
      ItemIndex = 0
      Items.Strings = (
        'Standard'
        'Flat'
        'UltraFlat')
      TabOrder = 8
      OnClick = DevExpCxLookAndFeelRadioGroupClick
    end
    object HistoryEnabledCheckBox: TCheckBox
      Left = 10
      Top = 100
      Width = 111
      Height = 17
      Caption = 'History Enabled'
      TabOrder = 9
    end
  end
  object BitBtn1: TBitBtn
    Left = 195
    Top = 75
    Width = 75
    Height = 25
    Caption = '&Load'
    TabOrder = 6
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 195
    Top = 105
    Width = 75
    Height = 25
    Caption = '&Store'
    TabOrder = 7
    OnClick = BitBtn2Click
  end
  object JvAppRegistryStore: TJvAppRegistryStore
    StoreOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StoreOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    StoreOptions.SetAsString = True
    StoreOptions.DefaultIfReadConvertError = True
    Root = 'Software\JVCL\JvParameterListDemo'
    RegRoot = hkCurrentUser
    SubStores = <>
    Left = 144
    Top = 8
  end
  object JvFormStorage1: TJvFormStorage
    AppStorage = JvAppRegistryStore
    AppStoragePath = 'Form\'
    StoredProps.Strings = (
      'AutoHeightCheckBox.Checked'
      'AutoWidthCheckBox.Checked'
      'StoreToCheckBox.Checked'
      'LoadFromCheckBox.Checked'
      'WidthEdit.Text'
      'MaxWidthEdit.Text'
      'MaxHeightEdit.Text'
      'HeightEdit.Text'
      'DevExpCxLookAndFeelRadioGroup.ItemIndex'
      'HistoryEnabledCheckBox.Checked')
    StoredValues = <>
    Left = 184
    Top = 8
  end
  object JvFormStorage2: TJvFormStorage
    AppStorage = JvAppRegistryStore
    AppStoragePath = 'Form\'
    StoredProps.Strings = (
      'AutoHeightCheckBox.Checked'
      'AutoWidthCheckBox.Checked'
      'StoreToCheckBox.Checked'
      'LoadFromCheckBox.Checked'
      'WidthEdit.Text'
      'MaxWidthEdit.Text'
      'MaxHeightEdit.Text'
      'HeightEdit.Text'
      'DevExpCxLookAndFeelRadioGroup.ItemIndex'
      'HistoryEnabledCheckBox.Checked')
    StoredValues = <>
    Left = 379
    Top = 58
  end
end
