object frmMain: TfrmMain
  Left = 192
  Top = 107
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JvBrowseFolder test'
  ClientHeight = 289
  ClientWidth = 439
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 4
    Top = 2
    Width = 433
    Height = 139
    Caption = '[ Options ]'
    TabOrder = 0
    object Label4: TLabel
      Left = 62
      Top = 20
      Width = 23
      Height = 13
      Caption = 'Title:'
    end
    object Label5: TLabel
      Left = 14
      Top = 46
      Width = 71
      Height = 13
      Caption = 'Root Directory:'
    end
    object Edit4: TEdit
      Left = 92
      Top = 16
      Width = 327
      Height = 21
      TabOrder = 0
      Text = 'JVCL - TJvBrowseForFolderDialog Test'
    end
    object CheckBox1: TCheckBox
      Left = 10
      Top = 72
      Width = 107
      Height = 17
      Caption = 'New Dialog Style'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBox2: TCheckBox
      Left = 10
      Top = 92
      Width = 119
      Height = 17
      Caption = 'Browse For Computer'
      TabOrder = 2
    end
    object CheckBox3: TCheckBox
      Left = 10
      Top = 112
      Width = 97
      Height = 17
      Caption = 'Only Directory'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBox4: TCheckBox
      Left = 158
      Top = 72
      Width = 97
      Height = 17
      Caption = 'Only Printers'
      TabOrder = 4
    end
    object CheckBox5: TCheckBox
      Left = 158
      Top = 92
      Width = 97
      Height = 17
      Caption = 'Include Files'
      TabOrder = 5
    end
    object CheckBox6: TCheckBox
      Left = 158
      Top = 112
      Width = 97
      Height = 17
      Caption = 'Include Urls'
      TabOrder = 6
    end
    object CheckBox7: TCheckBox
      Left = 278
      Top = 72
      Width = 97
      Height = 17
      Caption = 'Show Edit Box'
      TabOrder = 7
    end
    object CheckBox8: TCheckBox
      Left = 278
      Top = 92
      Width = 97
      Height = 17
      Caption = 'Shareable'
      TabOrder = 8
    end
    object ComboBox1: TComboBox
      Left = 92
      Top = 42
      Width = 327
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 9
      Items.Strings = (
        'Root Folder'
        'Control Panel'
        'Recycle Bin'
        'Desktop'
        'Desktop Directory'
        'My Computer'
        'Fonts'
        'NetHood'
        'Network'
        'Personal'
        'Printers'
        'Programs'
        'Recent Files'
        'SendTo'
        'StartMenu'
        'Startup'
        'Templates')
    end
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 182
    Width = 433
    Height = 103
    Caption = '[ Results ]'
    TabOrder = 1
    object Label1: TLabel
      Left = 34
      Top = 20
      Width = 45
      Height = 13
      Caption = 'Directory:'
    end
    object Label2: TLabel
      Left = 10
      Top = 74
      Width = 69
      Height = 13
      Caption = 'Current Folder:'
    end
    object Label3: TLabel
      Left = 11
      Top = 46
      Width = 68
      Height = 13
      Caption = 'Display Name:'
    end
    object Edit1: TEdit
      Left = 92
      Top = 16
      Width = 327
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object Edit2: TEdit
      Left = 92
      Top = 42
      Width = 327
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
    object Edit3: TEdit
      Left = 92
      Top = 70
      Width = 327
      Height = 21
      ReadOnly = True
      TabOrder = 2
    end
  end
  object Button1: TButton
    Left = 178
    Top = 152
    Width = 75
    Height = 25
    Caption = '&Test'
    TabOrder = 2
    OnClick = Button1Click
  end
  object JvBrowseFolder1: TJvBrowseForFolderDialog
    Position = fpScreenCenter
    RootDirectory = fdRootFolder
    Title = 'Titre de mes deux'
    OnChange = JvBrowseFolder1Change
    Left = 326
    Top = 134
  end
end
