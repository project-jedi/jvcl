object FormMain: TFormMain
  Left = 192
  Top = 114
  Width = 307
  Height = 477
  Caption = 'VisualCLX Unit Generator'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    299
    450)
  PixelsPerInch = 96
  TextHeight = 13
  object LblText: TLabel
    Left = 8
    Top = 8
    Width = 78
    Height = 13
    Caption = 'Click on process'
  end
  object Label1: TLabel
    Left = 13
    Top = 52
    Width = 12
    Height = 13
    Caption = 'In:'
  end
  object Label2: TLabel
    Left = 11
    Top = 84
    Width = 20
    Height = 13
    Caption = 'Out:'
  end
  object Label3: TLabel
    Left = 8
    Top = 360
    Width = 280
    Height = 26
    Anchors = [akLeft, akRight, akBottom]
    Caption = 
      'Before pressing Process please make sure that you have a backup ' +
      'of the destination directory'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object BtnProcess: TButton
    Left = 112
    Top = 416
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = 'Process'
    TabOrder = 0
    OnClick = BtnProcessClick
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 24
    Width = 281
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object EditInDir: TEdit
    Left = 32
    Top = 48
    Width = 257
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = 'C:\Tests\JVCLX\run'
  end
  object EditOutDir: TEdit
    Left = 32
    Top = 80
    Width = 257
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'C:\Tests\JVCLX\qrun'
  end
  object ProcessedFiles: TListBox
    Left = 8
    Top = 112
    Width = 281
    Height = 233
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExtendedSelect = False
    ItemHeight = 13
    TabOrder = 4
  end
end
