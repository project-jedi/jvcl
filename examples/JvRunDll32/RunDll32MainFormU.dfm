object RunDll32MainForm: TRunDll32MainForm
  Left = 361
  Top = 191
  BorderStyle = bsDialog
  Caption = 'RunDLL32 demo'
  ClientHeight = 219
  ClientWidth = 422
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 430
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'LMS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
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
  object lblModule: TLabel
    Left = 8
    Top = 14
    Width = 67
    Height = 14
    Caption = '&Module Name:'
  end
  object lblFunc: TLabel
    Left = 8
    Top = 55
    Width = 74
    Height = 14
    Caption = '&Function Name:'
  end
  object lblCmdLine: TLabel
    Left = 8
    Top = 96
    Width = 73
    Height = 14
    Caption = '&Command Line:'
  end
  object edModule: TEdit
    Left = 8
    Top = 32
    Width = 372
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'shell32.dll'
    OnChange = edModuleChange
  end
  object edFunc: TEdit
    Left = 8
    Top = 72
    Width = 403
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = 'Control_RunDLL'
  end
  object edCmdLine: TEdit
    Left = 8
    Top = 112
    Width = 403
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'desk.cpl,,3'
  end
  object chkWait: TCheckBox
    Left = 8
    Top = 144
    Width = 137
    Height = 17
    Caption = '&Wait for completion'
    TabOrder = 4
  end
  object btnBrowse: TButton
    Left = 387
    Top = 32
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
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
