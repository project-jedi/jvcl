object frmMain: TfrmMain
  Left = 192
  Top = 107
  Width = 643
  Height = 357
  Caption = 'JvAppInstances Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RunningInstances: TLabel
    Left = 224
    Top = 296
    Width = 3
    Height = 13
    Caption = '-'
  end
  object MaxInstances: TLabel
    Left = 352
    Top = 296
    Width = 3
    Height = 13
    Caption = '-'
  end
  object Label2: TLabel
    Left = 352
    Top = 272
    Width = 74
    Height = 13
    Caption = 'Max. instances:'
  end
  object Label1: TLabel
    Left = 224
    Top = 272
    Width = 91
    Height = 13
    Caption = 'Running instances:'
  end
  object MemoLog: TMemo
    Left = 344
    Top = 8
    Width = 281
    Height = 249
    TabOrder = 0
  end
  object MemoCmdLine: TMemo
    Left = 8
    Top = 8
    Width = 329
    Height = 249
    TabOrder = 1
  end
  object CheckBoxActive: TCheckBox
    Left = 8
    Top = 266
    Width = 73
    Height = 17
    Caption = 'Active'
    TabOrder = 2
    OnClick = CheckBoxActiveClick
  end
  object Button1: TButton
    Left = 8
    Top = 296
    Width = 185
    Height = 25
    Caption = 'Create another process instance'
    TabOrder = 3
    OnClick = Button1Click
  end
  object BtnQuit: TButton
    Left = 552
    Top = 296
    Width = 75
    Height = 25
    Caption = '&Quit'
    TabOrder = 4
    OnClick = BtnQuitClick
  end
  object JvAppInstances: TJvAppInstances
    MaxInstances = 5
    OnInstanceCreated = JvAppInstancesInstanceCreated
    OnInstanceDestroyed = JvAppInstancesInstanceDestroyed
    OnUserNotify = JvAppInstancesUserNotify
    OnCmdLineReceived = JvAppInstancesCmdLineReceived
    OnRejected = JvAppInstancesRejected
    Left = 56
    Top = 16
  end
end
