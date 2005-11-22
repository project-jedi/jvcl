object RunDll32MainForm: TRunDll32MainForm
  Left = 137
  Top = 177
  Width = 428
  Height = 245
  Caption = 'RunDLL32 demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
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
  object lblModule: TLabel
    Left = 8
    Top = 14
    Width = 69
    Height = 13
    Caption = '&Module Name:'
  end
  object lblFunc: TLabel
    Left = 8
    Top = 55
    Width = 75
    Height = 13
    Caption = '&Function Name:'
  end
  object lblCmdLine: TLabel
    Left = 8
    Top = 96
    Width = 73
    Height = 13
    Caption = '&Command Line:'
  end
  object edModule: TEdit
    Left = 8
    Top = 32
    Width = 372
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'shell32.dll'
    OnChange = edModuleChange
  end
  object edFunc: TEdit
    Left = 8
    Top = 72
    Width = 403
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'Control_RunDLL'
  end
  object edCmdLine: TEdit
    Left = 8
    Top = 112
    Width = 403
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = 'desk.cpl,,3'
  end
  object chkWait: TCheckBox
    Left = 8
    Top = 144
    Width = 137
    Height = 17
    Caption = '&Wait for completion'
    TabOrder = 3
  end
  object btnBrowse: TButton
    Left = 387
    Top = 32
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 4
    OnClick = btnBrowseClick
  end
  object btnRun: TButton
    Left = 337
    Top = 144
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Run'
    Default = True
    TabOrder = 5
    OnClick = btnRunClick
  end
  object btnInfo: TButton
    Left = 8
    Top = 184
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Module &Info...'
    TabOrder = 6
    OnClick = btnInfoClick
  end
  object btnInternal: TButton
    Left = 337
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
