object JvZoomForm: TJvZoomForm
  Left = 363
  Top = 249
  Width = 659
  Height = 476
  Caption = 'JvZoomForm'
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
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 10
    Top = 13
    Width = 503
    Height = 316
    Buttons = [capClose, capHelp]
    Caption = 'Demo for the JvZoom component'
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'Arial'
    CaptionFont.Style = [fsBold]
    OutlookLook = False
    TabOrder = 0
    object Label1: TLabel
      Left = 336
      Top = 120
      Width = 103
      Height = 13
      Caption = 'Adjust the zoom level:'
    end
    object JvZoom1: TJvZoom
      Left = 66
      Top = 66
      Width = 233
      Height = 189
      Active = False
    end
    object CheckBox1: TCheckBox
      Left = 334
      Top = 68
      Width = 97
      Height = 17
      Caption = 'Active'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object Button1: TButton
      Left = 370
      Top = 146
      Width = 75
      Height = 25
      Caption = '+'
      TabOrder = 2
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 369
      Top = 178
      Width = 75
      Height = 25
      Caption = '-'
      TabOrder = 3
      OnClick = Button2Click
    end
  end
end
