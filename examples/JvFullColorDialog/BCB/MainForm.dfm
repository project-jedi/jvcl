object frmMain: TfrmMain
  Left = 192
  Top = 107
  Width = 580
  Height = 325
  Caption = 'JvFullColorDialog demo'
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
  object JvFullColorLabel: TJvFullColorLabel
    Left = 16
    Top = 32
    Width = 257
    Height = 17
    LabelColor = 67108864
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Brush.Color = clBlack
    Caption = 'JvFullColorLabel'
    OnDblClick = JvFullColorLabelDblClick
  end
  object LabelInfo: TLabel
    Left = 16
    Top = 8
    Width = 170
    Height = 13
    Caption = 'Double-click on a label to customize'
  end
  object JvFullColorDialog: TJvFullColorDialog
    FullColor = 83886079
    HelpContext = 0
    OnApply = JvFullColorDialogApply
    Left = 232
    Top = 128
  end
end
