object Form1: TForm1
  Left = 887
  Top = 407
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JvThread Demo'
  ClientHeight = 143
  ClientWidth = 364
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
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
    ThreadDialog = JvThreadSimpleDialog1
    OnExecute = JvThread1Execute
    Left = 6
    Top = 6
  end
  object JvThread2: TJvThread
    Exclusive = True
    RunOnCreate = True
    FreeOnTerminate = False
    ThreadDialog = JvThreadAnimateDialog1
    OnExecute = JvThread2Execute
    Left = 6
    Top = 44
  end
  object JvThreadSimpleDialog1: TJvThreadSimpleDialog
    DialogOptions.FormStyle = fsNormal
    DialogOptions.ShowDialog = True
    DialogOptions.ShowModal = False
    DialogOptions.CancelButtonCaption = 'Cancel'
    DialogOptions.Caption = '23423423423432'
    DialogOptions.InfoText = 'Infotext'#13#10'Multiline'#13#10'Third Line'
    DialogOptions.InfoTextAlignment = taCenter
    Left = 260
    Top = 20
  end
  object JvThreadAnimateDialog1: TJvThreadAnimateDialog
    DialogOptions.FormStyle = fsNormal
    DialogOptions.ShowDialog = True
    DialogOptions.CancelButtonCaption = 'Cancel'
    DialogOptions.Caption = 'Execute Thread'
    DialogOptions.InfoText = 'Infotext'#13#10'<yx<yx'#13#10'<y'#13#10'x'
    DialogOptions.CommonAVI = aviCopyFiles
    Left = 260
    Top = 80
  end
end
