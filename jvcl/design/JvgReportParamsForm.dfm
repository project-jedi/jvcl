object JvgReportParamsForm: TJvgReportParamsForm
  Left = 343
  Top = 219
  BorderStyle = bsDialog
  Caption = 'Globus  Report Parameters Editor'
  ClientHeight = 80
  ClientWidth = 222
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object SB: TScrollBox
    Left = 0
    Top = 0
    Width = 222
    Height = 80
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 0
    object Panel1: TPanel
      Left = 0
      Top = 50
      Width = 222
      Height = 30
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object Panel2: TPanel
        Left = 54
        Top = 0
        Width = 168
        Height = 30
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object BitBtn2: TBitBtn
          Left = 89
          Top = 2
          Width = 75
          Height = 25
          Cancel = True
          Caption = 'Cancel'
          ModalResult = 2
          TabOrder = 0
        end
        object BitBtn1: TBitBtn
          Left = 8
          Top = 2
          Width = 75
          Height = 25
          Caption = 'OK'
          ModalResult = 1
          TabOrder = 1
        end
      end
    end
  end
end
