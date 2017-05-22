object BdePropertyDlg: TBdePropertyDlg
  Left = 199
  Top = 107
  Width = 344
  Height = 307
  ActiveControl = SysInformation
  BorderIcons = []
  Caption = 'BDE Properties'
  Color = clBtnFace
  Constraints.MinHeight = 307
  Constraints.MinWidth = 344
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 9
    Top = 132
    Width = 317
    Height = 105
    Anchors = [akLeft, akRight, akBottom]
    Caption = ' File Specifications '
    TabOrder = 1
    object Bevel3: TBevel
      Left = 166
      Top = 69
      Width = 141
      Height = 21
      Anchors = [akTop, akRight]
    end
    object Bevel2: TBevel
      Left = 166
      Top = 45
      Width = 141
      Height = 21
      Anchors = [akTop, akRight]
    end
    object Bevel1: TBevel
      Left = 166
      Top = 21
      Width = 141
      Height = 21
      Anchors = [akTop, akRight]
    end
    object FS: TLabel
      Left = 172
      Top = 25
      Width = 48
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'File Size:  '
    end
    object FD: TLabel
      Left = 172
      Top = 49
      Width = 51
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'File Date:  '
    end
    object FT: TLabel
      Left = 172
      Top = 73
      Width = 51
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'File Time:  '
    end
    object FSL: TLabel
      Left = 236
      Top = 25
      Width = 61
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
    end
    object FDL: TLabel
      Left = 236
      Top = 49
      Width = 61
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
    end
    object FTL: TLabel
      Left = 236
      Top = 73
      Width = 61
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
    end
    object BDEDLLList: TJvTextListBox
      Left = 8
      Top = 16
      Width = 149
      Height = 82
      Anchors = [akLeft, akTop, akRight, akBottom]
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
    Anchors = [akLeft, akBottom]
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
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' BDE System Information '
    TabOrder = 0
    object SysInformation: TJvTextListBox
      Left = 8
      Top = 16
      Width = 300
      Height = 93
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
      TabWidth = 38
    end
  end
end
