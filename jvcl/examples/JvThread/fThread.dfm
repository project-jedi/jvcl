object Form1: TForm1
  Left = 397
  Top = 361
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JvThread Demo'
  ClientHeight = 452
  ClientWidth = 719
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
    Left = 67
    Top = 49
    Width = 50
    Height = 13
    Caption = 'Job1 Stats'
  end
  object Label2: TLabel
    Left = 135
    Top = 49
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 67
    Top = 103
    Width = 50
    Height = 13
    Caption = 'Job2 Stats'
  end
  object Label4: TLabel
    Left = 135
    Top = 103
    Width = 3
    Height = 13
  end
  object Button1: TButton
    Left = 36
    Top = 18
    Width = 75
    Height = 25
    Caption = 'Start Job &1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 36
    Top = 69
    Width = 75
    Height = 25
    Caption = 'Start Job &2'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 121
    Top = 18
    Width = 100
    Height = 25
    Caption = 'Start Job 1 &Dialog'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 121
    Top = 69
    Width = 100
    Height = 25
    Caption = 'Start Job 2 D&ialog'
    TabOrder = 3
    OnClick = Button4Click
  end
  object JvThread1: TJvThread
    Exclusive = True
    RunOnCreate = True
    FreeOnTerminate = False
    OnExecute = JvThread1Execute
    ThreadDialog = JvThreadSimpleDialog1
    Left = 6
    Top = 6
  end
  object JvThread2: TJvThread
    Exclusive = True
    RunOnCreate = True
    FreeOnTerminate = False
    OnExecute = JvThread2Execute
    ThreadDialog = JvThreadAnimateDialog1
    Left = 6
    Top = 44
  end
  object JvThreadSimpleDialog1: TJvThreadSimpleDialog
    DialogOptions.ShowDialog = True
    DialogOptions.InfoText = 'Infotext'
    DialogOptions.Caption = '23423423423432'
    DialogOptions.CancelButtonCaption = 'Cancel'
    Left = 260
    Top = 20
  end
  object JvThreadAnimateDialog1: TJvThreadAnimateDialog
    DialogOptions.ShowDialog = True
    DialogOptions.InfoText = 'Info'
    DialogOptions.Caption = 'Execute Thread'
    DialogOptions.CancelButtonCaption = 'Cancel'
    DialogOptions.CommonAVI = aviCopyFiles
    Left = 255
    Top = 70
  end
end
