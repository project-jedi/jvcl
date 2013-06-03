object Form1: TForm1
  Left = 192
  Top = 179
  Width = 422
  Height = 133
  Caption = 'Form1'
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
    Left = 244
    Top = 32
    Width = 50
    Height = 13
    Caption = 'Job1 Stats'
  end
  object Label2: TLabel
    Left = 312
    Top = 32
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 244
    Top = 58
    Width = 50
    Height = 13
    Caption = 'Job2 Stats'
  end
  object Label4: TLabel
    Left = 312
    Top = 58
    Width = 3
    Height = 13
  end
  object Button1: TButton
    Left = 52
    Top = 18
    Width = 75
    Height = 25
    Caption = 'Start Job 1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 52
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
    FreeOnTerminate = True
    OnExecute = JvThread1Execute
    Left = 6
    Top = 6
  end
  object JvThread2: TJvThread
    Exclusive = True
    RunOnCreate = True
    FreeOnTerminate = True
    OnExecute = JvThread2Execute
    Left = 6
    Top = 44
  end
end
