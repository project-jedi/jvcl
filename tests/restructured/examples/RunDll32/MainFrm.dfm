object Form2: TForm2
  Left = 404
  Top = 243
  BorderStyle = bsDialog
  Caption = 'RunDLL32 demo'
  ClientHeight = 219
  ClientWidth = 508
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'LMS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 104
    Top = 192
    Width = 222
    Height = 13
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'Get more info about rundll32 from the Internet...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label1Click
  end
  object edModule: TLabeledEdit
    Left = 8
    Top = 32
    Width = 458
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 67
    EditLabel.Height = 14
    EditLabel.Caption = '&Module Name:'
    LabelPosition = lpAbove
    LabelSpacing = 3
    TabOrder = 0
    Text = 'shell32.dll'
    OnChange = edModuleChange
  end
  object edFunc: TLabeledEdit
    Left = 8
    Top = 72
    Width = 489
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 74
    EditLabel.Height = 14
    EditLabel.Caption = '&Function Name:'
    LabelPosition = lpAbove
    LabelSpacing = 3
    TabOrder = 2
    Text = 'Control_RunDLL'
  end
  object edCmdLine: TLabeledEdit
    Left = 8
    Top = 112
    Width = 489
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 73
    EditLabel.Height = 14
    EditLabel.Caption = '&Command Line:'
    LabelPosition = lpAbove
    LabelSpacing = 3
    TabOrder = 3
    Text = 'desk.cpl,,3'
  end
  object chkWait: TCheckBox
    Left = 24
    Top = 144
    Width = 137
    Height = 17
    Caption = '&Wait for completion'
    TabOrder = 4
  end
  object btnBrowse: TButton
    Left = 473
    Top = 32
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object btnRun: TButton
    Left = 423
    Top = 144
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Run'
    Default = True
    TabOrder = 6
    OnClick = btnRunClick
  end
  object btnInfo: TButton
    Left = 8
    Top = 184
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Module &Info...'
    TabOrder = 5
    OnClick = btnInfoClick
  end
  object btnInternal: TButton
    Left = 423
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Internal'
    Default = True
    TabOrder = 7
    OnClick = btnInternalClick
  end
end
