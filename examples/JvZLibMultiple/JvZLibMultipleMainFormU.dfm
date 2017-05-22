object JvZLibMultipleMainForm: TJvZLibMultipleMainForm
  Left = 306
  Top = 160
  ActiveControl = edSrcFolder
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JvZlibMultiple  Demo'
  ClientHeight = 231
  ClientWidth = 368
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 68
    Height = 13
    Caption = 'Source folder:'
  end
  object Label2: TLabel
    Left = 8
    Top = 112
    Width = 106
    Height = 13
    Caption = 'Decompress to folder:'
  end
  object Label3: TLabel
    Left = 8
    Top = 56
    Width = 75
    Height = 13
    Caption = 'Destination file:'
  end
  object lblFilename: TLabel
    Left = 8
    Top = 196
    Width = 31
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Ready'
  end
  object btnCompress: TButton
    Left = 191
    Top = 182
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Compress'
    Enabled = False
    TabOrder = 6
    OnClick = btnCompressClick
  end
  object btnUnCompress: TButton
    Left = 281
    Top = 182
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Decompress'
    Enabled = False
    TabOrder = 7
    OnClick = btnUnCompressClick
  end
  object edSrcFolder: TEdit
    Left = 8
    Top = 24
    Width = 320
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'C:\temp'
    OnChange = edSrcFolderChange
  end
  object edDestFolder: TEdit
    Left = 8
    Top = 128
    Width = 320
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    Text = 'C:\temp\dc'
    OnChange = edDestFolderChange
  end
  object edFilename: TEdit
    Left = 8
    Top = 72
    Width = 320
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = 'C:\temp\temp.z'
    OnChange = edSrcFolderChange
  end
  object pbProgress: TProgressBar
    Left = 0
    Top = 215
    Width = 368
    Height = 16
    Align = alBottom
    Min = 0
    Max = 100
    TabOrder = 8
  end
  object btnSrc: TButton
    Left = 333
    Top = 24
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = btnSrcClick
  end
  object btnDestFile: TButton
    Left = 333
    Top = 72
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = btnDestFileClick
  end
  object btnDestFolder: TButton
    Left = 333
    Top = 128
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 5
    OnClick = btnDestFolderClick
  end
end
