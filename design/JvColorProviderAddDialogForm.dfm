object frmAddColor: TfrmAddColor
  Left = 534
  Top = 422
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Add color...'
  ClientHeight = 95
  ClientWidth = 275
  Color = clBtnFace
  Constraints.MaxHeight = 125
  Constraints.MinHeight = 122
  Constraints.MinWidth = 283
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnColor: TSpeedButton
    Left = 115
    Top = 37
    Width = 156
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    OnClick = btnColorClick
  end
  object rbProvider: TRadioButton
    Left = 10
    Top = 10
    Width = 96
    Height = 17
    Caption = '&Existing color:'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = rbProviderClick
  end
  object rbDialog: TRadioButton
    Left = 10
    Top = 40
    Width = 96
    Height = 17
    Caption = '&New color:'
    TabOrder = 1
    OnClick = rbProviderClick
  end
  object cbColor: TComboBox
    Left = 115
    Top = 8
    Width = 155
    Height = 22
    Style = csOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    TabOrder = 2
    OnChange = cbColorChange
    OnDrawItem = cbColorDrawItem
  end
  object btnOK: TButton
    Left = 115
    Top = 65
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 195
    Top = 65
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
