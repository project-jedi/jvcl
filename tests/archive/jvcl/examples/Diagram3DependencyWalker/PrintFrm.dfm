object frmPrint: TfrmPrint
  Left = 311
  Top = 187
  ActiveControl = cbFormat
  BorderStyle = bsDialog
  Caption = 'Print'
  ClientHeight = 113
  ClientWidth = 294
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 73
    Height = 13
    Caption = '&Output format:'
    FocusControl = cbFormat
  end
  object cbFormat: TComboBox
    Left = 8
    Top = 32
    Width = 279
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      'Text'
      'HTML'
      'XML')
  end
  object btnOK: TButton
    Left = 118
    Top = 80
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 198
    Top = 80
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
