object frmMain: TfrmMain
  Left = 192
  Top = 107
  Width = 351
  Height = 421
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
  object Bevel: TBevel
    Left = 16
    Top = 48
    Width = 108
    Height = 108
    Style = bsRaised
  end
  object Image: TImage
    Left = 20
    Top = 52
    Width = 100
    Height = 100
  end
  object LabelImage: TLabel
    Left = 24
    Top = 16
    Width = 35
    Height = 13
    Caption = 'Image :'
    Layout = tlCenter
  end
  object Memo: TMemo
    Left = 8
    Top = 160
    Width = 129
    Height = 65
    Alignment = taCenter
    BorderStyle = bsNone
    Lines.Strings = (
      'Original image')
    ParentColor = True
    TabOrder = 0
    OnKeyDown = MemoKeyDown
    OnKeyPress = MemoKeyPress
  end
  object ComboBoxFileName: TComboBox
    Left = 72
    Top = 16
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnClick = ComboBoxFileNameClick
  end
  object JvFullColorCircleDialog: TJvFullColorCircleDialog
    HelpContext = 0
    OnApply = JvFullColorCircleDialogApply
    Left = 184
    Top = 80
  end
end
