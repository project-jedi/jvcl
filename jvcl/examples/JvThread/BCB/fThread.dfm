object Form1: TForm1
  Left = 229
  Top = 185
  Width = 726
  Height = 478
  Caption = 'JvThread Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
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
    DialogOptions.ShowDialog = True
    DialogOptions.ShowModal = False
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
