object MainForm: TMainForm
  Left = 196
  Top = 113
  Width = 536
  Height = 393
  Caption = 'TJvAppDdeCmd Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 528
    Height = 325
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 325
    Width = 528
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 128
      Top = 16
      Width = 32
      Height = 13
      Caption = 'Label1'
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Show Modal Dialog'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object JvAppDdeCmd1: TJvAppDdeCmd
    CorrectParams = True
    OnBusyChanged = PvAppDdeCmd1BusyChanged
    OnExecParsedCmd = PvAppDdeCmd1ExecParsedCmd
    Left = 8
    Top = 272
  end
end
