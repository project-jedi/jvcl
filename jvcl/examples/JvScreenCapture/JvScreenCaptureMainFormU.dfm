object JvScreenCaptureMainForm: TJvScreenCaptureMainForm
  Left = 374
  Top = 78
  Width = 584
  Height = 499
  Caption = 'Screen Capture using JclGraphics unit'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 16
    Top = 66
    Width = 537
    Height = 391
    Stretch = True
  end
  object Button1: TButton
    Left = 28
    Top = 26
    Width = 75
    Height = 25
    Caption = 'Capture'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 168
    Top = 26
    Width = 139
    Height = 25
    Caption = 'Place on clipboard'
    TabOrder = 1
    OnClick = Button2Click
  end
  object TaskBar: TCheckBox
    Left = 328
    Top = 32
    Width = 97
    Height = 17
    Caption = 'include taskbar'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
end
