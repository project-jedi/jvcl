object JvgCompEditorTemplate: TJvgCompEditorTemplate
  Left = 346
  Top = 119
  Width = 512
  Height = 438
  Caption = 'JEDI VCL Component Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnMain: TPanel
    Left = 0
    Top = 0
    Width = 504
    Height = 358
    Align = alClient
    TabOrder = 0
    object pgMain: TPageControl
      Left = 1
      Top = 1
      Width = 502
      Height = 356
      ActivePage = tabMain
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      TabStop = False
      object tabMain: TTabSheet
        Caption = 'Text'
      end
    end
  end
  object pnBottom: TPanel
    Left = 0
    Top = 358
    Width = 504
    Height = 53
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnCancel1: TButton
      Left = 418
      Top = 14
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = btnCancel1Click
    end
    object btnOK1: TButton
      Left = 333
      Top = 14
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      TabOrder = 1
      OnClick = btnOK1Click
    end
  end
end
