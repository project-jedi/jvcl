object Form1: TForm1
  Left = 353
  Top = 149
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JvThread Demo'
  ClientHeight = 99
  ClientWidth = 243
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 132
    Top = 24
    Width = 50
    Height = 13
    Caption = 'Job1 Stats'
  end
  object Label2: TLabel
    Left = 200
    Top = 24
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 132
    Top = 58
    Width = 50
    Height = 13
    Caption = 'Job2 Stats'
  end
  object Label4: TLabel
    Left = 200
    Top = 58
    Width = 3
    Height = 13
  end
  object Button1: TButton
    Left = 36
    Top = 18
    Width = 75
    Height = 25
    Caption = 'Start Job 1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 36
    Top = 54
    Width = 75
    Height = 25
    Caption = 'Start Job 2'
    TabOrder = 1
    OnClick = Button2Click
  end
  object JvThread1: TJvThread
    Exclusive = True
    RunOnCreate = True
    FreeOnTerminate = False
    OnExecute = JvThread1Execute
    Left = 6
    Top = 6
  end
  object JvThread2: TJvThread
    Exclusive = True
    RunOnCreate = True
    FreeOnTerminate = False
    OnExecute = JvThread2Execute
    Left = 6
    Top = 44
  end
end
