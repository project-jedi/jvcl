object BdePropertyDlg: TBdePropertyDlg
  Left = 199
  Top = 107
  ActiveControl = SysInformation
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'BDE Properties'
  ClientHeight = 277
  ClientWidth = 336
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PixelsPerInch = 96
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 9
    Top = 132
    Width = 317
    Height = 105
    Caption = ' File Specifications '
    TabOrder = 1
    object Bevel3: TBevel
      Left = 166
      Top = 69
      Width = 141
      Height = 21
    end
    object Bevel2: TBevel
      Left = 166
      Top = 45
      Width = 141
      Height = 21
    end
    object Bevel1: TBevel
      Left = 166
      Top = 21
      Width = 141
      Height = 21
    end
    object FS: TLabel
      Left = 172
      Top = 25
      Width = 48
      Height = 13
      Caption = 'File Size:  '
    end
    object FD: TLabel
      Left = 172
      Top = 49
      Width = 51
      Height = 13
      Caption = 'File Date:  '
    end
    object FT: TLabel
      Left = 172
      Top = 73
      Width = 51
      Height = 13
      Caption = 'File Time:  '
    end
    object FSL: TLabel
      Left = 236
      Top = 25
      Width = 61
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
    end
    object FDL: TLabel
      Left = 236
      Top = 49
      Width = 61
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
    end
    object FTL: TLabel
      Left = 236
      Top = 73
      Width = 61
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
    end
    object BDEDLLList: TJvTextListBox 
      Left = 8
      Top = 16
      Width = 149
      Height = 82
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnClick = DLLListClick
    end
  end
  object Button1: TButton
    Left = 131
    Top = 245
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 2
    TabOrder = 2
  end
  object GroupBox2: TGroupBox
    Left = 9
    Top = 8
    Width = 317
    Height = 117
    Caption = ' BDE System Information '
    TabOrder = 0
    object SysInformation: TJvTextListBox 
      Left = 8
      Top = 16
      Width = 300
      Height = 93
      ItemHeight = 13
      TabOrder = 0
      TabWidth = 38
    end
  end
end
