object FormMain: TFormMain
  Left = 192
  Top = 114
  BorderStyle = bsSingle
  Caption = 'JvAppInstances Test'
  ClientHeight = 330
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MemoCmdLine: TMemo
    Left = 8
    Top = 8
    Width = 329
    Height = 249
    TabOrder = 0
  end
  object MemoLog: TMemo
    Left = 344
    Top = 8
    Width = 281
    Height = 249
    TabOrder = 1
  end
  object BtnQuit: TButton
    Left = 552
    Top = 296
    Width = 75
    Height = 25
    Caption = '&Quit'
    TabOrder = 3
    OnClick = BtnQuitClick
  end
  object Button1: TButton
    Left = 8
    Top = 296
    Width = 185
    Height = 25
    Caption = 'Create another process instance'
    TabOrder = 2
    OnClick = Button1Click
  end
  object CheckBoxActive: TCheckBox
    Left = 8
    Top = 266
    Width = 73
    Height = 17
    Caption = 'Active'
    TabOrder = 4
    OnClick = CheckBoxActiveClick
  end
  object BtnInstanceCount: TButton
    Left = 208
    Top = 296
    Width = 113
    Height = 25
    Caption = 'Instance Count'
    TabOrder = 5
    OnClick = BtnInstanceCountClick
  end
  object JvAppInstances: TJvAppInstances
    MaxInstances = 2
    OnInstanceCreated = EvInstanceCreated
    OnInstanceDestroyed = EvInstanceDestroyed
    OnUserNotify = JvAppInstancesUserNotify
    OnCmdLineReceived = EvCmdLineReceived
    OnRejected = JvAppInstancesRejected
    Left = 16
    Top = 16
  end
end
